library(tidyverse)
library(shiny)
library(plotly)
library(fst)
library(data.table)
library(leaflet)
library(lubridate)
library(scales)
library(shinyjs)
library(sf)
library(DBI)
library(RSQLite)

function(input, output, session) {
    
    # Connect to sql database
    sqldb <- dbConnect(SQLite(), 
                       dbname="data/statewide_2002_21.sqlite")
    
    # Load other datasets
    officers_per_agency <- read_rds("data/sep/officers_per_agency.rds")
    mapping_df <- read_rds("data/sep/mapping.rds")
    ma_towns <- read_rds("data/ma_towns.rds")
    all_loc_agency_v_time <- fread("data/sep/all_loc_agency_stops_v_time.csv")  %>%
        mutate_at(vars(date, month), as_date)
    all_offenses <- fread("data/sep/all_offenses_by_date.csv")  %>%
        mutate(date = as_date(date))
    data_mass_race <- read_rds("data/mass_race.RDS") %>%
        rename(var = race) %>%
        arrange(var)
    town_race_pop <- readRDS("data/towns_race.rds")
    named_colors <- readRDS("data/offense_colors.rds")
    violations <- read_csv("data/violations.csv",
                           col_types = cols(
                               offense = col_character(),
                               group = col_character()
                           ))
    
    colors <- c("White" = "#3c3532", 
                "Black" = "#681b40", 
                "Hispanic/Latinx" = "#ef404d",
                "Asian" = "#fabeaf", 
                "Middle Eastern" =  "#fbb416", 
                "Native American" = "#a7d7b5", 
                "Unknown" = "white",
                "Other" = "white")
    
    empty_plotly <- function(label) {
        plot_ly() %>%
            layout(yaxis = list(zeroline = F, showticklabels = F),
                   xaxis = list(zeroline = F, showticklabels = F),
                   font=list(family = "GT America"),
                   hoverlabel=list(font = list(family = "GT America")),
                   annotations = list(list(
                       showarrow = F,
                       x = .5, xref="paper", xanchor = "center",
                       y = .5, yref="paper", yanchor = "center",
                       text = paste("<i>No", label, "for selected filters.</i>"),
                       opacity = 0.7
                   ))
            )
    }
    
    build_query <- function(start_date="2002-01-01", 
                            end_date="2021-02-04", 
                            town="All cities and towns", 
                            agency="All agencies", 
                            officer="All officers", 
                            outcome="All outcomes", col="*", group="") {
        
        query <- paste("SELECT", col, "FROM `statewide_2002_21` WHERE")
        and_needed <- F
        
        if (start_date != "2002-01-01" | end_date != "2021-02-04") {
            query <- paste0(query, " `date` BETWEEN ", as.numeric(as_date(start_date)), 
                                " AND ", as.numeric(as_date(end_date)))
            and_needed <- T
        }
        
        if (town != "All cities and towns") {
            
            query <- paste0(query, ifelse(and_needed, " AND", ""), " `loc` = '", town, "'")
            and_needed <- T
        }
        
        if (agency != "All agencies") {
            query <- paste0(query, ifelse(and_needed, " AND", ""), 
                           " `agency` = '", agency, "'")
            and_needed <- T
            
            if (officer != "All officers" & officer != "") {
                query <- paste0(query, " AND `officer` = '", officer, "'")
            }
        }
        
        if (outcome != "All outcomes") {
            
            query <- paste0(query, ifelse(and_needed, " AND", ""), " `type` = '", 
                            outcome, "'")
        }
        
        if (str_ends(query, "WHERE")) {
            cat("querying entire database?? wuh-oh\n")
        }
        
        query <- paste(query, 
                       ifelse(group=="", "", paste("GROUP BY", group)))
        
        cat(query, "\n")
        
        return(query)
        
    }
    
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
            
            selected_officers <- officers_per_agency[agency == input$download_agency, 
                                                     list(officer)]
    
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
            
            q <- build_query(start_date = download_values$start_date, 
                             end_date = download_values$end_date, 
                             town = download_values$town, 
                             agency = download_values$agency, 
                             officer = download_values$officer)
            
            download_values$data <- 
                dbGetQuery(sqldb, q) %>%
                mutate(date = as_date(date))
            
            validate(
                need(nrow(download_values$data) > 0, 
                     "No data for selected filters. Please try a different selection.")
            )
            
            enable("download_button")
        }
        
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
    
    # Mapping stops -------------------------------------------------------------------
    
    stops_per_county <- reactiveValues(data=NULL)
    
    observeEvent(input$map_stops_button, {
        
        stops_sf <- mapping_df[date >= input$town_start_date & 
                                   date <= input$town_end_date, 
                               .(N=sum(N)), loc] %>%
            merge(ma_towns, by.y = "town", by.x = "loc", all=T) %>% 
            mutate(N= replace_na(N, 0)) %>% # Fill in 0s for towns with no stops
            select(-TOWN, town=loc) %>%
            st_as_sf() %>%
            st_transform('+proj=longlat +datum=WGS84') 
        
        if (input$towns_radio == "Stops per capita") {
            stops_sf <- stops_sf %>%
                mutate(N = (N / pop) * 1e3)
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
        
        palette_domain <- if (stops_per_county$log) log10(stops_per_county$data$N) else stops_per_county$data$N
        palette_domain <- replace(palette_domain, 
                                  palette_domain == -Inf, NA)
        
        if (stops_per_county$percap == "Total stops") {
            legend_title <- "<a style='font-family:GT America; color: dimgrey'>Total <br>traffic stops</a>"
            label_accuracy <- 1
            label_suffix <- ""
        } else {
            legend_title <- "<a style='font-family:GT America; color: dimgrey'>Traffic stops<br>per 1,000</a>"
            label_accuracy <- 1
            label_suffix <- "per 1,000 population"
        }
        
        pal_total_stops <- colorNumeric(
            palette = "inferno",
            domain = palette_domain,
            na.color = viridis_pal(option="inferno")(10) %>% head(1)
        )
        
        pal_total_stops_noNA <- colorNumeric(
            palette = "inferno",
            domain = palette_domain,
            na.color = NA
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
            addLegend(pal = pal_total_stops_noNA,
                      values = palette_domain,
                      labFormat = stopsLabelFormat(log=stops_per_county$log),
                      position = "topright",
                      title = legend_title,
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
        if (input$time_agency != "All agencies") {
            selected_officers <- officers_per_agency[agency == input$time_agency, 
                                                     list(officer)]
            
            updateSelectizeInput(session, "time_officer",
                                 choices = c("All officers", selected_officers), server=T)
        } else {
            updateSelectizeInput(session, "time_officer",
                                 choices = c("Please select agency" = ""))
        }
    })
    
    observeEvent(input$time_agency2, {
        if (!input$time_agency2 %in% c("All agencies", "--")) {
            selected_officers <- officers_per_agency[agency == input$time_agency2, 
                                                     list(officer)]
            
            updateSelectizeInput(session, "time_officer2",
                                 choices = c("All officers", selected_officers), server=T)
        } else {
            updateSelectizeInput(session, "time_officer2",
                                 choices = c("Please select agency" = ""))
        }
    })
    
    time_values <- reactiveValues(agency = NULL)
    
    get_unit <- function(type) {
        case_when(
            type == "All outcomes" ~ "Stops", 
            type == "Warn" ~ "Warnings",
            type == "Arrest" ~ "Arrests",
            type == "Crim" ~ "Criminal Citations",
            type == "Civil" ~ "Civil Citations",
            T ~ type
        )
    }
    
    get_legend_name <- function(loc, agency, officer, type, pie_label=F) {
        
        unit <- get_unit(type)
        
        if (officer != "All officers" & 
            officer != "") {
            if (loc == "All cities and towns") {
                name <- paste(unit, "by Officer", officer, "of the", agency) 
            } else {
                name <- paste(unit, "by Officer", officer, "of the", agency, "in", loc) 
            }
        } else if (agency != "All agencies") {
            if (loc != "All cities and towns") {
                name <- paste(unit, "by the", agency, "in", loc)
            } else {
                name <- paste(unit, "by the", agency)
            }
        } else if (loc != "All cities and towns") {
            name <- paste(unit, "in", loc)
        } else {
            name <- unit
        }
        
        if (pie_label) {
            name <- name %>%
                str_replace(" of the", "\nof the") %>%
                str_replace(" by the", "\nby the") %>%
                paste("<span style='font-size: 1.5rem; margin-bottom:3rem;'>", ., "\n </span>")
        }
        
        return(name)
    }
    
    observeEvent(input$time_button, {
        cat("time type:", input$time_outcome, "\n")
        
        time_values$town <- input$time_town
        time_values$agency <- input$time_agency
        time_values$officer <- input$time_officer
        time_values$outcome <- input$time_outcome
        
        time_values$compare <- input$compare_time
        
        time_values$town2 <- input$time_town2
        time_values$agency2 <- input$time_agency2
        time_values$officer2 <- input$time_officer2
        time_values$outcome2 <- input$time_outcome2
        
        cat(time_values$town, time_values$agency, time_values$officer, "\n")
        cat("recalculating data...\n")
        
        if (time_values$town == "All cities and towns" & 
            time_values$agency == "All agencies") {
            
            cat("Calculating for all cities, towns, and agencies\n")
            
            time_values$data <- all_loc_agency_v_time %>%
                filter(if(time_values$outcome != "All outcomes") 
                        type == time_values$outcome else T) %>%
                uncount(N) %>%
                select(date, month, year)
            
            cat("Done calculating all!\n")
            
        } else {
            
            q <- build_query(town = time_values$town, 
                             agency = time_values$agency, 
                             officer = time_values$officer, 
                             outcome = time_values$outcome, 
                             col = "date")
            
            time_values$data <-
                dbGetQuery(sqldb, q) %>%
                mutate(date = lubridate::as_date(date),
                       year = lubridate::year(date),
                       month = lubridate::floor_date(date, "month")) %>%
                as.data.table()
        }
        
        if (time_values$compare) {
            
            if (time_values$town2 == "All cities and towns" & 
                time_values$agency2 == "All agencies") {
                
                time_values$data2 <- all_loc_agency_v_time %>%
                    filter(if(time_values$outcome2 != "All outcomes") 
                        type == time_values$outcome2 else T) %>%
                    uncount(N) %>%
                    select(date, month, year)
                
            } else {
                
                q <- build_query(town = time_values$town2, 
                                 agency = time_values$agency2, 
                                 officer = time_values$officer2, 
                                 outcome = time_values$outcome2, 
                                 col = "date")
                
                time_values$data2 <- dbGetQuery(sqldb, q) %>%
                    mutate(date = lubridate::as_date(date),
                           year = lubridate::year(date),
                           month = lubridate::floor_date(date, "month")) %>%
                    as.data.table()
            }
            
        } else {
            time_values$data2 <- NULL
        }
    })
    
    output$stops_v_time <- renderPlotly({
        
        validate(
            need(time_values$agency, 'Please select filters and press "Go."')
        )
        
        data <- time_values$data
        data2 <- if (time_values$compare) time_values$data2 else NULL
        
        cat("data2?", !is.null(data2))
        
        if (input$time_type == "Year") {
            link <- "in"
            date_format <- "%Y"
            data <- data[year < 2021, .N, .(x=year)]
            data2 <- if (time_values$compare) data2[year < 2021, .N, .(x=year)] else NULL
            
        } else if (input$time_type == "Month") {
            link <- "in"
            date_format <- "%b %Y"
            data <- data[month < ymd("20210201"), .N, .(x=month)]
            data2 <- if (time_values$compare) data2[month < ymd("20210201"), .N, .(x=month)] else NULL
            
        } else if (input$time_type == "Day") {
            link <- "on"
            date_format <- "%b %d, %Y"
            data <- data[, .N, .(x=date)]
            data2 <- if (time_values$compare) data2[, .N, .(x=date)] else NULL
        }
            
        if (time_values$compare == F & nrow(data) > 0) {
            
            unit <- get_unit(time_values$outcome)
        
            data %>%
                plot_ly(hovertemplate = paste0('%{y:,} ', unit, " ", link, 
                                               ' %{x|', date_format, "}<extra></extra>"),
                        line = list(color = '#3c3532')) %>% 
                add_lines(x=~x, y=~N)%>%
                layout(yaxis = list(title = paste("Number of", unit), zeroline = F),
                       xaxis = list(title = "", zeroline = F),
                       font=list(family = "GT America"),
                       hoverlabel=list(font = list(family = "GT America")),
                       annotations = list(list(
                           showarrow = F, opacity = 0.7,
                           x = .5, xref="paper", xanchor = "center",
                           y = 1, yref="paper",
                           text = "<i>Click and drag to zoom in on a specific date range</i>"
                       )))
        } else if (time_values$compare == T) {
            if (nrow(data) > 0 | nrow(data2) > 0) {
            
                name1 <- get_legend_name(time_values$town, time_values$agency, 
                                         time_values$officer, time_values$outcome)
                name2 <- get_legend_name(time_values$town2, time_values$agency2, 
                                         time_values$officer2, time_values$outcome2)
                
                unit1 <- unit <- get_unit(time_values$outcome)
                unit2 <- unit <- get_unit(time_values$outcome2)
                
                plot_ly() %>% 
                    add_lines(data=data, x=~x, y=~N, name=name1, opacity=.7,
                              line = list(color = '#3c3532'),
                              hovertemplate = paste0('%{y:,} ', unit1, " ", link, 
                                                     ' %{x|', date_format, "}<extra></extra>"))%>%
                    add_lines(data=data2, x=~x, y=~N,name=name2, opacity=.7,
                              line = list(color = "#ef404d"),
                              hovertemplate = paste0('%{y:,} ', unit2, " ", link, 
                                                     ' %{x|', date_format, "}<extra></extra>")) %>%
                    add_annotations(showarrow = F, opacity = 0.7,
                                    x = .5, xref="paper", xanchor = "center",
                                    y = 1, yref="paper",
                                    text = "<i>Click and drag to zoom in on a specific date range</i>") %>%
                    layout(yaxis = list(title = "Number of traffic stops", zeroline = F),
                           xaxis = list(title = "", zeroline = F),
                           font=list(family = "GT America"),
                           hoverlabel=list(font = list(family = "GT America")),
                           legend = list(x = 0.5, y=-.4,
                                         xanchor="center",
                                         bgcolor = alpha('lightgray', 0.4)))
            }
        } else {
            # If there are no stops for the filter
            empty_plotly("stops")
        }
        
    })
    
    # Stops by offense ----------------------------------------------------------------
    
    # Update officer list when agency is selected
    observeEvent(input$offense_agency, {
        
        if (input$offense_agency != "All agencies") {
            selected_officers <- officers_per_agency[agency == input$offense_agency, 
                                                     list(officer)]
            
            updateSelectizeInput(session, "offense_officer",
                                 choices = c("All officers", selected_officers), server=T)
        } else {
            updateSelectizeInput(session, "offense_officer",
                                 choices = c("Please select agency" = ""))
        }
    })
    
    offense_values <- reactiveValues(town = NULL)
    
    observeEvent(input$offense_button, {
        offense_values$town <- input$offense_town
        offense_values$agency <- input$offense_agency
        offense_values$officer <- input$offense_officer
        offense_values$start_date <- input$offense_start_date
        offense_values$end_date <- input$offense_end_date
    })
    
    output$offenses <- renderPlotly({
        validate(
            need(offense_values$town, 'Please select filters and press "Go."')
        )
        
        if (offense_values$town == "All cities and towns" &
            offense_values$agency == "All agencies") {
            
            data <- all_offenses[date >= offense_values$start_date &
                                     date <= offense_values$end_date] %>%
                group_by(group) %>%
                summarize(n = sum(n)) %>%
                arrange(group == "Other", desc(n))
            
        } else {
            
            q <- build_query(start_date = offense_values$start_date,
                             end_date = offense_values$end_date,
                             town = offense_values$town, 
                             agency = offense_values$agency, 
                             officer = offense_values$officer, 
                             col = "offense")
            
            data <- dbGetQuery(sqldb, q) %>%
                merge(violations, by="offense", all.x=T) %>%
                count(group) %>%
                arrange(group == "Other", desc(n))
        }
        
        offense_colors <- named_colors[data$group]
        
        if (nrow(data) > 0) {
            
            annotation <- get_legend_name(offense_values$town, offense_values$agency, 
                                          offense_values$officer, "Offenses from stops",
                                          pie_label=T)
        
            data %>%
                plot_ly(sort=F,direction = "clockwise",
                        hovertemplate = '<i>Offense</i>: %{label}<br><i>Number stopped</i>: %{value} (%{percent})<extra></extra>',
                        marker = list(line = list(color = 'lightgrey', width = 1),
                                      colors = offense_colors)) %>%
                add_pie(labels=~group, values=~n,
                        textposition = "inside",
                        title = annotation) %>%
                layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       font=list(family = "GT America"),
                       hoverlabel=list(font = list(family = "GT America")),
                       legend = list(itemclick=F, itemdoubleclick=F))
            
        } else  {
            empty_plotly("stops")
        }
        
    })
    
    # Agency stats ------------------------------------------------------------

    agency_values <- reactiveValues(agency = NULL)

    observeEvent(input$agency_button, {
        agency_values$agency <- input$agency_agency
        agency_values$start_date <- input$agency_start_date
        agency_values$end_date <- input$agency_end_date
    })

    output$top_towns <- renderTable({agency_values$top_towns})

    output$agency_dashboard <- renderUI({
        validate(
            need(agency_values$agency, 'Please select filters and press "Go."')
        )
        
        q <- build_query(start_date = agency_values$start_date,
                         end_date = agency_values$end_date,
                         agency = agency_values$agency, 
                         col = "loc, officer, offense")
        
        agency_data <- dbGetQuery(sqldb, q)
        
        # Calculate total stops
        agency_values$total_stops <- agency_data %>%
            count(name="N") %>%
            pull(1)
        
        # Top towns
        agency_values$top_towns <- agency_data %>%
            count(loc, name="N") %>%
            slice_max(N, n=10) %>%
            mutate(Rank = min_rank(-N),
                   N = number(N, big.mark=",", accuracy=1)) %>%
            head(10) %>%
            select(Rank, `Town/City`=loc, `Number of Stops`=N)
        
        # Top officers
        output$top_officers <- renderTable({
            
            agency_data %>%
                filter(!is.na(officer)) %>%
                count(officer, name="N") %>%
                slice_max(N, n=10) %>%
                mutate(Rank = min_rank(-N),
                       N = number(N, big.mark=",", accuracy=1)) %>%
                head(10) %>%
                select(Rank, `Officer ID` = officer, `Number of Stops`=N)
        })
        
        # Top offenses
        output$top_offenses <- renderTable({
            
            agency_data %>%
                count(offense, name="N") %>%
                slice_max(N, n=10) %>%
                separate(offense, into = c("Offense", "Statute"), sep= " \\* | (?=c)") %>%
                mutate(N = number(N, big.mark=",", accuracy=1)) %>%
                rename(`Number of Stops` = N)

        })

        tagList(
            h2(number(agency_values$total_stops, big.mark=","), style="text-align: center;"),
            p(em("Stops made by", agency_values$agency, br(), "between",
              format(agency_values$start_date, "%b %d, %Y"), "and",
              format(agency_values$end_date, "%b %d, %Y")),
              style="text-align: center;"),
            splitLayout(id="dashboard_split",
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
        townover_values$town <- input$townover_town
        townover_values$start_date <- input$townover_start_date
        townover_values$end_date <- input$townover_end_date
    })

    output$townover_dashboard <- renderUI({
        validate(
            need(townover_values$town, 'Please select filters and press "Go."')
        )
        
        q <- build_query(start_date = townover_values$start_date,
                         end_date = townover_values$end_date,
                         town = townover_values$town, 
                         col = "agency, officer, offense")
        
        town_data <- dbGetQuery(sqldb, q)
        
        # Calculate total stops
        townover_values$total_stops <- town_data %>%
            count(name="N") %>%
            pull(1)
        
        # Top agencies
        output$town_top_agencies <- renderTable({
            
            town_data %>%
                filter(!is.na(agency)) %>%
                count(agency, name="N") %>%
                slice_max(N, n=10) %>%
                mutate(Rank = min_rank(-N),
                       N = number(N, big.mark=",", accuracy=1)) %>%
                head(10) %>%
                select(Rank, `Agency` = agency, `Number of Stops`=N)
        })
    
        # Top officers
        output$townover_top_officers <- renderTable({
            
            town_data %>%
                filter(!is.na(officer)) %>%
                count(agency, officer, name="N") %>%
                slice_max(N, n=10) %>%
                mutate(Rank = min_rank(-N),
                       N = number(N, big.mark=",", accuracy=1)) %>%
                head(10) %>%
                select(Rank, `Officer ID` = officer,
                       `Agency` = agency, `Number of Stops`=N)
        })

        # Top offenses
        output$townover_top_offenses <- renderTable({ 
            
            town_data %>%
                count(offense, name="N") %>%
                slice_max(N, n=10) %>%
                separate(offense, into = c("Offense", "Statute"), sep= " \\* | (?=c)") %>%
                mutate(N = number(N, big.mark=",", accuracy=1)) %>%
                rename(`Number of Stops` = N)

        })

        tagList(
            h2(number(townover_values$total_stops, big.mark=","),
               style="text-align: center;"),
            p(em("Stops made in", townover_values$town, br(), "between",
                 format(townover_values$start_date, "%b %d, %Y"), "and",
                 format(townover_values$end_date, "%b %d, %Y")),
              style="text-align: center;"),
            splitLayout(id="dashboard_split",
                div(h4("Top Agencies"),
                    tableOutput("town_top_agencies")),
                div(h4("Most Active Officers"),
                    tableOutput("townover_top_officers"))
            ),
            h4("Most Common Traffic Violations"),
            tableOutput("townover_top_offenses")
        )
        
    })

    # Town's stops by race ------------------------------------------------------------

    town_values <- reactiveValues()

    observeEvent(input$town_button, {
        town_values$town <- input$town_town
        town_values$agency <- input$town_agency
        town_values$start_date <- input$town_race_start_date
        town_values$end_date <- input$town_race_end_date
    })

    output$town_demog <- renderPlotly({
        shinyjs::hide("town_race_legend")

        validate(
            need(town_values$town, 'Please select a city or town.')
        )
        
        q <- build_query(start_date = town_values$start_date,
                         end_date = town_values$end_date,
                         town = town_values$town, 
                         agency = town_values$agency,
                         col="race")
        
        data_town <- dbGetQuery(sqldb, q) %>%
            count(race) %>%
            mutate(var = factor(race, 
                                 levels = c("White", "Black", "Hispanic/Latinx", 
                                            "Asian", "Middle Eastern", "Native American", 
                                            "Unknown"))) %>%
            arrange(var)
        
        if (nrow(data_town) > 0) {
            
            shinyjs::show("town_race_legend")
            
            title_suffix <- if (town_values$agency != "All agencies") 
                paste("by", town_values$agency) else ""

            data_town_pop <- town_race_pop %>%
                filter(town == town_values$town) %>%
                select(n=pop, var = race) %>%
                arrange(var)
    
            town_stop_colors <- colors[data_town$var]
            town_pop_colors <- colors[data_town_pop$var]
            
            annotation <- get_legend_name(town_values$town, town_values$agency, 
                                          "All officers", "Race of stops",
                                          pie_label=T) %>%
                str_replace(" in", "\nin")
    
            plot_ly(sort=F,
                    direction = "clockwise",
                    marker = list(line = list(color = 'lightgrey', width = 1)),
                    labels = ~var, values = ~n,
                    textposition = "inside"
                    ) %>%
                add_pie(data = data_town,
                        title = annotation,
                        domain = list(x = c(0, .5), y = c(0, 1)),
                        marker = list(colors = town_stop_colors),
                        hovertemplate = '<i>Race</i>: %{label}<br><i>Number stopped</i>: %{value} (%{percent})<extra></extra>') %>%
                add_pie(data = data_town_pop, 
                        title = paste("<span style='font-size:1.2rem'>", 
                                      town_values$town, "Population\n(2018 estimate)\n </span>"),
                        domain = list(x = c(.5, 1), y = c(.2, .8)),
                        marker = list(colors = town_pop_colors),
                        hovertemplate = '<i>Race</i>: %{label}<br><i>Population (2018 estimate)</i>: %{value} (%{percent})<extra></extra>') %>%
                layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       showlegend = FALSE,
                       font=list(family = "GT America"),
                       hoverlabel=list(font = list(family = "GT America")))
        } else {
            empty_plotly("stops")
        }

    })

    # Officer's stops by race ---------------------------------------------------------

    observeEvent(input$officer_agency, {
        selected_officers <- officers_per_agency[agency == input$officer_agency,
                                                 list(officer)]
        
        updateSelectizeInput(session, "officer_officer",
                             choices = c(selected_officers), server=T)
    })

    officer_values <- reactiveValues()

    observeEvent(input$officer_button, {
        officer_values$officer <- input$officer_officer
        officer_values$agency <- input$officer_agency
        officer_values$start_date <- input$officer_race_start_date
        officer_values$end_date <- input$officer_race_end_date
    })

    output$officer_demog <- renderPlotly({
        shinyjs::hide("officer_race_legend")

        validate(
            need(officer_values$officer, 'Please select an officer ID.')
        )
        
        q <- build_query(start_date = officer_values$start_date,
                         end_date = officer_values$end_date, 
                         agency = officer_values$agency,
                         officer = officer_values$officer,
                         col = "race")
        
        data_officer <- dbGetQuery(sqldb, q) %>%
            count(race) %>%
            mutate(var = factor(race, 
                                 levels = c("White", "Black", "Hispanic/Latinx", 
                                            "Asian", "Middle Eastern", "Native American", 
                                            "Unknown"))) %>%
            arrange(var)
        
        if (nrow(data_officer) > 0) {
            
            shinyjs::show("officer_race_legend")
            
            q <- build_query(start_date = officer_values$start_date,
                             end_date = officer_values$end_date, 
                             agency = officer_values$agency,
                             col = "race")
        
            data_agency <- dbGetQuery(sqldb, q) %>%
                count(race) %>%
                mutate(var = factor(race, 
                                    levels = c("White", "Black", "Hispanic/Latinx", 
                                               "Asian", "Middle Eastern", "Native American", 
                                               "Unknown"))) %>%
                arrange(var)
    
            officer_colors <- colors[data_officer$var]
            agency_colors <- colors[data_agency$var]
            
            officer_annotation <- get_legend_name("All cities and towns", officer_values$agency, 
                                          officer_values$officer, "Stops",
                                          pie_label=T) %>%
                str_replace(" in", "\nin")
            
            outline_css <- "text-shadow: -1px -1px 0 #fff, 0 -1px 0 #fff, 1px -1px 0 #fff, 1px 0 0 #fff, 1px 1px 0 #fff, 0 1px 0 #fff, -1px 1px 0 #fff, -1px 0 0 #fff;"
    
            plot_ly(sort=F,
                    direction = "clockwise",
                    hovertemplate = '<i>Race</i>: %{label}<br><i>Number stopped</i>: %{value} (%{percent})<extra></extra>',
                    marker = list(line = list(color = 'lightgrey', width = 1)),
                    labels = ~var, values = ~n,
                    textposition = "inside") %>%
                add_pie(data = data_officer,
                        title = officer_annotation,
                        domain = list(x = c(.25, 0.75), y = c(0.3, 1)),
                        marker = list(colors = officer_colors)) %>%
                add_pie(data = data_agency, 
                        title = paste0("<span style='font-size:1.2rem; ", outline_css,
                                       "'>Stops by the\n",
                                       officer_values$agency,
                                       "</span>"),
                        domain = list(x = c(0, .4), y = c(0, 0.5)),
                        marker = list(colors = agency_colors)) %>%
                add_pie(data = data_mass_race, 
                        title = paste0("<span style='font-size:1.2rem; ", outline_css, 
                                       "'>All Massachusetts Stops\n </span>"),
                        domain = list(x = c(.6, 1), y = c(0, 0.5)),
                        marker = list(colors = colors)) %>%
                layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       showlegend = FALSE,
                       font=list(family = "GT America"),
                       hoverlabel=list(font = list(family = "GT America")))
            
        } else {
            empty_plotly("stops")
        }

        })
}
