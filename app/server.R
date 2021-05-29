library(tidyverse)
library(shiny)
library(plotly)
library(fst)
library(data.table)
library(leaflet)
library(lubridate)
library(scales)
library(shinyjs)

# all_agencies <- stops_df[order(agency), list(agency)] %>%
#     unique() %>%
#     mutate(agency = str_extract(agency, "(?<=: ).*"),
#            agency = trimws(agency)) %>%
#     unique()
# 
# write_rds(all_agencies, "data/all_agencies.RDS")

# stops_df[, officer] %>% unique()

# Define server logic required to draw a histogram
function(input, output, session) {
        
    # Create a Progress object
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Loading data", value = 0.1, 
                 detail = "Loading traffic stops.")
        
    # Load the cleaned data
    stops_df <- read_fst("data/stops.fst", as.data.table=T)
    progress$inc(.6, detail = "Loading offenses.")
    offenses_df <- read_fst("data/offenses.fst", as.data.table=T)
    
    data_mass_race <- read_rds("data/mass_race.RDS") %>%
        rename(var = race) %>%
        arrange(var)
    
    ma_towns <- read_rds("data/ma_towns.rds")
    town_race_pop <- readRDS("data/towns_race.rds")
    
    colors <- c("White" = "#3c3532", 
                "Black" = "#681b40", 
                "Hispanic/Latinx" = "#ef404d",
                "Asian" = "#fabeaf", 
                "Middle Eastern" =  "#fbb416", 
                "Native American" = "#a7d7b5", 
                "Unknown" = "white",
                "Other" = "white")
    
    # # Delay tab load ------------------------------------------------------------------
    # 
    # plotData <- reactiveVal()
    # 
    # observeEvent(input$panels, {
    #     if (input$panels == "Stops by Race"){
    #         req(is.null(plotData(s)))
    #         print("Stops code is run")
    #         plotData(render_officer_demog_plot())
    #     }
    # })
    
    # Download data subset ------------------------------------------------------------
    
    # Link to this page from landing page
    observeEvent(input$link_to_download, {
        updateTabsetPanel(session, "panels", "Download the Data")
    })
    
    # Update the list of officers based on the selected agency
    observeEvent(input$download_agency, {
        
        if (input$download_agency == 'All agencies') {
            updateSelectizeInput(session, "download_officer",
                                 choices = c("Please select agency"=""), server=T)
        } else {
            
            selected_officers <- stops_df[agency == input$download_agency, 
                                          list(officer)][order(officer), officer] %>%
                unique()
    
            updateSelectizeInput(session, "download_officer",
                                 choices = c("All officers", selected_officers), server=T)
        }
    })
    
    download_values <- reactiveValues(agency = NULL)
    
    observeEvent(input$download_filters, {
        download_values$officer <-      input$download_officer
        download_values$town <-         input$download_town
        download_values$agency <-       input$download_agency
        download_values$start_date <-   input$download_start_date
        download_values$end_date <-     input$download_end_date
    
    })
        
    output$download_size <- renderText({
        validate(
            need(download_values$agency, 'Please select filters and press "Go" to view estimated download size.')
        )
        
        if (download_values$town == "All cities and towns" & 
            download_values$agency == "All agencies"  &
            (download_values$officer != "All officers" |
             download_values$officer != "") &
            download_values$end_date - download_values$start_date > years(2)) {
        
            cat("no!!!\n")
            disable("download_button")
            download_values$filtered <- F
            
                
            validate(need(download_values$filtered, "For a manageable download, please either select a single town, agency, or officer ID; or restrict the date range to less than two years."))
                    
        } else {
                
            download_values$filtered <- T
            cat("applying data filters\n")
            
            download_values$data <- stops_df[date >= download_values$start_date &
                                                 date <= download_values$end_date &
                                                 (if(download_values$town != "All cities and towns")
                                                     loc == download_values$town else T==T) &
                                                 (if(download_values$agency != "All agencies")
                                                     agency == download_values$agency else T==T) &
                                                 (if (download_values$officer != "All officers" &
                                                      download_values$officer != "")
                                                     officer == download_values$officer else T==T)] %>%
                merge(offenses_df, by="citation", all.x=T, all.y=F)
            
            enable("download_button")
        }
        
        
        cat("printing object size\n")
        
        object.size(download_values$data) %>% 
            format(units="auto", standard="SI") %>%
            paste("The dataset with the applied filters is estimated to be", .)
    })
    
    output$download_button <- downloadHandler(
        filename = "MassDOT_stops.csv",
        content = function(file) {
            cat("Download commencing!!\n")
            withProgress(message = 'Downloading...', value = 1, {
                write_csv(download_values$data, file)
            })
        }
    )
    
    # Stops by town -------------------------------------------------------------------
    
    stops_per_county <- reactiveValues(data=NULL)
    
    observeEvent(input$map_stops_button, {
        cat("calculating stops per county in time frame\n")
        cat("radio button: ", input$towns_radio, "\n")
        
        stops_sf <- stops_df[date >= input$town_start_date &
                                 date <= input$town_end_date, 
                             .N, loc] %>%
            filter(!loc %in% c("New Hampshire", "Not Assigned", "Other")) %>%
            mutate(loc = case_when(
                loc == "Manchester" ~ "Manchester-By-The-Sea",
                loc %in% c("Allston", "Brighton", "Charlestown", "Dorchester", 
                           "E Boston", "Forest Hills", "Hyde Park", "Jamaica Plain", 
                           "Mattapan", "Roslindale", "Roxbury", "S Boston", "W Roxbury") ~ "Boston",
                T ~ loc
            )) %>%
            group_by(loc) %>%
            summarize(N = sum(N)) %>%
            merge(ma_towns, by.y = "town", by.x = "loc", all=T) %>% 
            mutate(N= replace_na(N, 0)) %>% # Fill in 0s for towns with no stops
            select(-TOWN, town=loc) %>%
            st_as_sf() %>%
            st_transform('+proj=longlat +datum=WGS84') 
        
        if (input$towns_radio == "Stops per capita") {
            stops_sf <- stops_sf %>%
                mutate(N = N / pop)
        }
        
        stops_per_county$data <- stops_sf
        stops_per_county$log <- input$town_log
        stops_per_county$percap <- input$towns_radio
    })
    
    # Replace legend labels with custom format
    stopsLabelFormat = function(..., log){ 
        if (log) {
            function(type = "numeric", cuts){ 
                10**as.numeric(cuts) %>%
                    scales::number(big.mark=",")
            } 
        } else {
            labelFormat(...)
        }
    }
    
    output$stops_by_town <- renderLeaflet({
        validate(
            need(stops_per_county$data, 'Please select date range and press "Go."')
        )
        
        palette_domain <- if (stops_per_county$log) log10(stops_per_county$data$N) 
        else stops_per_county$data$N
        
        if (stops_per_county$percap == "Total stops") {
            legend_title <- "<a style='font-family:GT America; color: dimgrey'>Total <br>traffic stops</a>"
            label_accuracy <- 1
            label_suffix <- ""
        } else {
            legend_title <- "<a style='font-family:GT America; color: dimgrey'>Traffic stops<br>per capita</a>"
            label_accuracy <- 0.001
            label_suffix <- "per capita"
        }
        
        pal_total_stops <- colorNumeric(
            palette = "inferno",
            domain = palette_domain,
            na.color = "grey" #scales::viridis_pal()(10) %>% tail(1)
        )
        
        leaflet(options = leafletOptions(attributionControl = T)) %>%
            addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
            addPolygons(data = stops_per_county$data,
                        fillOpacity = 0.8, 
                        weight = 1, 
                        fillColor = if(stops_per_county$log) ~pal_total_stops(log10(N)) else  ~pal_total_stops(N),
                        stroke=F,
                        smoothFactor=.5,
                        label = ~lapply(paste0("<b>", town, "</b></br>", 
                                               scales::number(N, big.mark=",", accuracy=label_accuracy), " traffic stops ", label_suffix), 
                                        htmltools::HTML),
                        color="none",
                        group="poly")  %>%
            addLegend(pal = pal_total_stops,
                      values = palette_domain,
                      labFormat = stopsLabelFormat(log=stops_per_county$log),
                      position = "topright",
                      title = legend_title
            )  %>%
            addEasyButton(easyButton(
                icon="fa-home", title="Reset",
                onClick=JS("function(btn, map){ 
                   var groupLayer = map.layerManager.getLayerGroup('poly');
                   map.fitBounds(groupLayer.getBounds());
               }"))) %>%
            addControl("<img src='Logo_White_CMYK_Massachusetts.png' style='max-width:100px;'>", 
                       "bottomleft", className="logo-control")
    })
    
    # Stops over time -----------------------------------------------------------------
    
    observeEvent(input$time_agency, {
        cat("filtering out agency's officers\n")
        if (input$time_agency != "All agencies") {
            selected_officers <- stops_df[agency == input$time_agency, 
                                          list(officer)][order(officer), officer] %>%
                unique()
            
            updateSelectizeInput(session, "time_officer",
                                 choices = c("All officers", selected_officers), server=T)
        } else {
            updateSelectizeInput(session, "time_officer",
                                 choices = c("Please select agency" = ""))
        }
    })
    
    time_values <- reactiveValues(town = NULL)
    
    observeEvent(input$time_button, {
        time_values$town <- input$time_town
        time_values$agency <- input$time_agency
        time_values$officer <- input$time_officer
        
        cat(time_values$town, time_values$agency, time_values$officer, "\n")
        cat("recalculating data...\n")
        time_values$data <- stops_df[
            (if(time_values$town != "All cities and towns") 
                loc == time_values$town else T==T) &
            (if(time_values$agency != "All agencies") 
                agency == time_values$agency else T==T) &
            (if (time_values$officer != "All officers" & 
                time_values$officer != "")
                officer == time_values$officer else T==T),
            .(date, year = year(date), 
              month = floor_date(date, "month"))]
        
        # View(time_values$data)
    })
    
    output$stops_v_time <- renderPlotly({
        validate(
            need(time_values$town, 'Please select filters and press "Go."')
        )
        
        data <- time_values$data
        
        if (input$time_type == "Year") {
            link <- "in"
            date_format <- "%Y"
            data <- data[year < 2021, .N, .(x=year)]
        } else if (input$time_type == "Month") {
            link <- "in"
            date_format <- "%b %Y"
            data <- data[month < ymd("20210201"), .N, .(x=month)]
        } else if (input$time_type == "Day") {
            link <- "on"
            date_format <- "%b %d, %Y"
            data <- data[, .N, .(x=date)]
        }
        
        if (nrow(data) > 0) {
            
            data %>%
                plot_ly(textfont = list(family = "GT America"),
                        hovertemplate = paste0('%{y:,} stops ', link, 
                                               ' %{x|', date_format, "}<extra></extra>"),
                        line = list(color = '#3c3532')) %>% 
                add_lines(x=~x, y=~N)%>%
                layout(yaxis = list(title = "Number of traffic stops", zeroline = F),
                       xaxis = list(title = "", zeroline = F),
                       font=list(family = "GT America"),
                       hoverlabel=list(font = list(family = "GT America")),
                       annotations = list(list(
                           showarrow = F, opacity = 0.7,
                           x = .5, xref="paper", xanchor = "center",
                           y = 1, yref="paper",
                           text = "<i>Click and drag to zoom in on a specific date range</i>"
                       )))
        } else {
            plot_ly() %>%
                layout(yaxis = list(zeroline = F, showticklabels = F),
                       xaxis = list(zeroline = F, showticklabels = F),
                       font=list(family = "GT America"),
                       hoverlabel=list(font = list(family = "GT America")),
                       annotations = list(list(
                           showarrow = F,
                           x = .5, xref="paper", xanchor = "center",
                           y = .5, yref="paper", yanchor = "center",
                           text = "<i>No stops for selected filters.</i>",
                           opacity = 0.7
                       ))
                )
        }
        
    })
    
    # Agency stats ------------------------------------------------------------
    # Top towns
    # Top officers
    # Stops made per officer
    
    agency_values <- reactiveValues(total_stops = NULL)
    
    observeEvent(input$agency_button, {
        # Direct input
        agency_values$agency <- input$agency_agency
        agency_values$start_date <- input$agency_start_date
        agency_values$end_date <- input$agency_end_date
        
        # Filter the data
        # data <- stops_df[agency == input$agency_agency & 
        #                      date >= input$agency_start_date &
        #                      date <= input$agency_end_date]
        
        # Calculate total stops
        agency_values$total_stops <- data[, .N]
        
        # Top towns
        output$top_towns <- renderTable({ 
            stops_df[agency == agency_values$agency & 
                         date >= agency_values$start_date &
                         date <= agency_values$end_date, .N, loc] %>%
                slice_max(N, n=10) %>%
                mutate(N = number(N, big.mark=",", accuracy=1)) %>%
                rename(`Town/City` = loc,
                       `Number of Stops` = N) %>%
                head(10)
            })  
        
        # Top officers
        output$top_officers <- renderTable({ 
            
            stops_df[agency == agency_values$agency & 
                         date >= agency_values$start_date &
                         date <= agency_values$end_date, .N, officer] %>%
                filter(!is.na(officer)) %>%
                slice_max(N, n=10) %>%
                mutate(N = number(N, big.mark=",", accuracy=1)) %>%
                rename(`Officer ID` = officer,
                       `Number of Stops` = N) %>%
                head(10)
            })  
        
        # Top reasons
        output$top_offenses <- renderTable({ 
        
            offenses_df[which(offenses_df$citation %in% 
                                  stops_df[agency == agency_values$agency & 
                                               date >= agency_values$start_date &
                                               date <= agency_values$end_date, citation]), 
                        .N, offense] %>%
                slice_max(N, n=10) %>%
                separate(offense, into=c(NA, "desc"), sep=": ") %>%
                separate(desc, into = c("Offense", "Statute"), sep= " \\* | (?=c)") %>%
                mutate(N = number(N, big.mark=",", accuracy=1)) %>%
                rename(`Number of Stops` = N)
        })
        
    })
    
    
    
    output$agency_dashboard <- renderUI({
        validate(
            need(agency_values$total_stops, 'Please select filters and press "Go."')
        )
        
        tagList(
            h2(number(agency_values$total_stops, big.mark=","), style="text-align: center;"),
            p(em("Stops made by", agency_values$agency, br(), "between", 
              format(agency_values$start_date, "%b %d, %Y"), "and", 
              format(agency_values$end_date, "%b %d, %Y")), 
              style="text-align: center;"),
            splitLayout(
                div(h4("Top Cities & Towns"),
                    tableOutput("top_towns")),
                div(h4("Most Active Officers"),
                    tableOutput("top_officers"))
            ),
            h4("Most Common Traffic Violations"),
            tableOutput("top_offenses")
        )
    })
    
    # Town overview -------------------------------------------------------------------
    
    townover_values <- reactiveValues(done = NULL)
    
    observeEvent(input$townover_button, {
        # Direct input
        townover_values$town <- input$townover_town
        townover_values$start_date <- input$townover_start_date
        townover_values$end_date <- input$townover_end_date
        
        # Calculate total stops
        townover_values$total_stops <- stops_df[loc == townover_values$town & 
                                                    date >= townover_values$start_date &
                                                    date <= townover_values$end_date, .N]
        
        # Top towns
        output$town_top_agencies <- renderTable({ 
            stops_df[loc == townover_values$town & 
                         date >= townover_values$start_date &
                         date <= townover_values$end_date &
                         !is.na(agency), .N, agency] %>%
                slice_max(N, n=10) %>%
                mutate(N = number(N, big.mark=",", accuracy=1)) %>%
                rename(`Agency` = agency,
                       `Number of Stops` = N) %>%
                head(10)
        })  
        
        # Top officers
        output$townover_top_officers <- renderTable({ 
            
            stops_df[loc == townover_values$town & 
                         date >= townover_values$start_date &
                         date <= townover_values$end_date, .N, .(agency, officer)] %>%
                filter(!is.na(officer)) %>%
                slice_max(N, n=10) %>%
                mutate(N = number(N, big.mark=",", accuracy=1)) %>%
                rename(Agency = agency,
                       `Officer ID` = officer,
                       `Number of Stops` = N) %>%
                head(10)
        })  
        
        townover_values$done <- T
        
    })
    
    output$townover_dashboard <- renderUI({
        validate(
            need(townover_values$done, 'Please select filters and press "Go."')
        )
        
        # Top reasons (this calculation lives in here to prevent early load)
        townover_top_offenses <- offenses_df[which(offenses_df$citation %in% 
                                  stops_df[loc == townover_values$town & 
                                               date >= townover_values$start_date &
                                               date <= townover_values$end_date, citation]), 
                        .N, offense] %>%
                slice_max(N, n=10) %>%
                separate(offense, into=c(NA, "desc"), sep=": ") %>%
                separate(desc, into = c("Offense", "Statute"), sep= " \\* | (?=c)") %>%
                mutate(N = number(N, big.mark=",", accuracy=1)) %>%
                rename(`Number of Stops` = N)
    
        output$townover_top_offenses <- renderTable({ townover_top_offenses })
        
        tagList(
            h2(number(townover_values$total_stops, big.mark=","), 
               style="text-align: center;"),
            p(em("Stops made in", townover_values$town, br(), "between", 
                 format(townover_values$start_date, "%b %d, %Y"), "and", 
                 format(townover_values$end_date, "%b %d, %Y")), 
              style="text-align: center;"),
            splitLayout(
                div(h4("Top Agencies"),
                    tableOutput("town_top_agencies")),
                div(h4("Most Active Officers"),
                    tableOutput("townover_top_officers"))
            ),
            h4("Most Common Traffic Violations"),
            tableOutput("townover_top_offenses")
        )
    })

    stops_df %>% select(loc) %>% distinct()
    
    # Town's stops by race ------------------------------------------------------------
    
    observeEvent(input$town_town, {
        cat("filtering out town's agencies\n")
        selected_agencies <- stops_df[loc == input$town_town, 
                                      list(agency)] %>%
            unique() %>% arrange(agency)
        
        updateSelectizeInput(session, "town_agency",
                             choices = c("All agencies", selected_agencies), server=T)
    })
    
    town_values <- reactiveValues()
    
    observeEvent(input$town_button, {
        town_values$town <- input$town_town
        town_values$agency <- input$town_agency
        town_values$start_date <- input$town_race_start_date
        town_values$end_date <- input$town_race_end_date
    })
    
    output$town_demog <- renderPlotly({
        validate(
            need(town_values$town, 'Please select a city or town.')
        )
        
        cat("calculating town dataset\n")
        
        if (town_values$agency != "All agencies") {
            cat("Filtering town for agency\n")
            data_town <- stops_df[agency == town_values$agency]
            title_suffix <- paste("by", town_values$agency)
        } else {
            data_town <- stops_df
            title_suffix <- ""
        }
        
        data_town <- data_town[loc == town_values$town & 
                                   date >= town_values$start_date &
                                   date <= town_values$end_date,
                               .(n=.N), 
                               .(var=race)] %>%
            arrange(var)
        
        data_town_pop <- town_race_pop %>%
            filter(town == town_values$town) %>%
            select(n=pop, var = race) %>%
            arrange(var)
        
        town_stop_colors <- colors[data_town$var]
        town_pop_colors <- colors[data_town_pop$var]
        
        plot_ly(sort=F,
                direction = "clockwise",
                textfont = list(family = "GT America"),
                hovertemplate = '<i>Race</i>: %{label}<br><i>Number stopped</i>: %{value} (%{percent})',
                marker = list(line = list(color = 'lightgrey', width = 1)),
                labels = ~var, values = ~n ) %>%
            add_pie(data = data_town, 
                    name = paste("Stops in", town_values$town, "\n", title_suffix), 
                    domain = list(x = c(0, .5), y = c(0, 1)),
                    marker = list(colors = town_stop_colors)) %>% 
            add_annotations(
                x= 0.25, y= -.03, xanchor="center", yanchor="top", xref = "paper", yref = "paper",
                text = paste("Stops in", town_values$town, "\n", title_suffix), 
                showarrow = F,
                font = list(size=17)
            ) %>%
            add_pie(data = data_town_pop, name = paste(town_values$town, "Population\n(2018 estimate)"),
                    domain = list(x = c(.5, 1), y = c(.2, .8)),
                    marker = list(colors = town_pop_colors)) %>%
            add_annotations(
                x= 0.75, y= .15, xanchor="center",yanchor="top", xref = "paper", yref = "paper",
                text = paste(town_values$town, "Population\n(2018 estimate)"), showarrow = F
            ) %>%
            layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   showlegend = FALSE, 
                   font=list(family = "GT America"),
                   hoverlabel=list(font = list(family = "GT America")))
        
    })
    
    # Officer's stops by race ---------------------------------------------------------
    
    observeEvent(input$officer_agency, {
        cat("filtering out agency's officers\n")
        selected_officers <- stops_df[agency == input$officer_agency, 
                                      list(officer)][order(officer), officer] %>%
            unique()

        updateSelectizeInput(session, "officer_officer",
                             choices = selected_officers, server=T)
    })
    
    officer_values <- reactiveValues()
    
    observeEvent(input$officer_button, {
        officer_values$officer <- input$officer_officer
        officer_values$agency <- input$officer_agency
        officer_values$start_date <- input$officer_race_start_date
        officer_values$end_date <- input$officer_race_end_date
    })
    
    output$officer_demog <- renderPlotly({
        validate(
            need(officer_values$officer, 'Please select an officer ID.')
        )
        
        cat("calculating officer dataset\n")
        data_officer <- stops_df[officer == officer_values$officer & 
                                     agency == officer_values$agency  & 
                                     date >= officer_values$start_date &
                                     date <= officer_values$end_date,
                                 .(n=.N), 
                                 .(var=race)] %>%
            arrange(var)
        
        cat("calculating agency dataset\n")
        data_agency <- stops_df[agency == officer_values$agency, 
                                .(n=.N), 
                                .(var=race)] %>%
            arrange(var)
        
        officer_colors <- colors[data_officer$var]
        agency_colors <- colors[data_agency$var]
        
        data_officer$var
        
        cat("generating plot\n")
        plot_ly(sort=F,
                direction = "clockwise",
                textfont = list(family = "GT America"),
                hovertemplate = '<i>Race</i>: %{label}<br><i>Number stopped</i>: %{value} (%{percent})',
                marker = list(line = list(color = 'lightgrey', width = 1)),
                labels = ~var, values = ~n ) %>%
            add_pie(data = data_officer, 
                    name = paste("Officer", officer_values$officer), 
                    domain = list(x = c(.25, 0.75), y = c(0.3, 1)),
                    marker = list(colors = officer_colors)) %>% 
            add_annotations(
                x= 0.5, y= .2, xanchor="center", xref = "paper", yref = "paper",
                text = paste("Officer", officer_values$officer), 
                showarrow = F,
                font = list(size=17)
            ) %>%
            add_pie(data = data_agency, name = paste(officer_values$agency, "Stops"),
                    domain = list(x = c(0, .3), y = c(0, .4)),
                    marker = list(colors = agency_colors)) %>%
            add_annotations(
                x= 0.15, y= -.07, xanchor="center", xref = "paper", yref = "paper",
                text = paste(officer_values$agency, "Stops"), showarrow = F
            ) %>%
            add_pie(data = data_mass_race, name = "All Massachusetts Stops", 
                    domain = list(x = c(.7, 1), y = c(0, 0.4)),
                    marker = list(colors = colors)) %>%
            add_annotations(
                x= 0.85, y= -.07, xanchor="center", xref = "paper", yref = "paper",
                text = "All Massachusetts Stops", showarrow = F
            ) %>%
            layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   showlegend = FALSE, 
                   font=list(family = "GT America"),
                   hoverlabel=list(font = list(family = "GT America")))
    
        })


}
