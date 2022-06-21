library(ggplot2)
library(shiny)
library(shinycssloaders)
library(showtext)
library(leaflet)
library(plotly)
library(shinyjs)

all_agencies <- readRDS("data/all_agencies.RDS")
all_towns <- readRDS("data/all_towns.rds")

all_outcomes <- c("All outcomes"="All outcomes", 
                  "Warning"="Warn", 
                  "Civil Citation"="Civil",
                  "Criminal Citation"="Crim", 
                  "Arrest"="Arrest")

log_tooltip_html <- "
<div id='log-tooltip' width=20px>
    <b>What is a logarithmic scale?</b>
    <p>Logarithmic scales are an alternative way of presenting numerical data that can more helpfully represent relative difference between wide-ranging values.</p>
    
    <p>Instead of spacing values along a scale linearly (e.g., 1, 2, 3, 4), a logarithmic scale spaces out values logarithmically (e.g., 1, 10, 100, 1,000).</p>
    <img width=200px src='log_example.png' />
</div>
"

officer_tooltip_html <- "
<div id='officer-id-tooltip' width=20px>
    The format of officer identifiers varies widely between law enforcement agencies - some agencies just use numbers, some include letters, etc. The options presented here reflect the IDs exactly as reported by MassDOT. We anticipate some may be typos.
</div>
"

# Initialization --------------------------------------------------------------

# Set ggplot settings
theme_set(theme_minimal())

theme_update(text = element_text(family="GT America"),
             plot.title = element_text(family="GT America", face="bold"))

# UI --------------------------------------------------------------------------

fluidPage(
  useShinyjs(),  # Set up shinyjs
  theme = "massdot_app.css",
  
  # Add favicon          
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "512x512", href = "favicon.png")
  ),
  
  # Add javascript for log tooltip
  tags$script(HTML('
               $( document ).on("shiny:sessioninitialized", function(event) {
                    $(\'a[data-toggle="tooltip"]\').tooltip({
                        animated: "fade",
                        placement: "bottom",
                        html: true
                    });      
               });'
  )),
  
  # App title
  div(id="title",
      div(id = "title_and_icon",
          img(src="copcar_icon.png", id="car_icon"),
          titlePanel("Traffic Stops in Massachusetts")
      )
  ),
  
  div(
    navlistPanel(widths = c(3, 9), id="panels",
                 
                 # About ----------------------------------------------------
                 tabPanel("About", 
                          
                          # h3("Explore Traffic Stops by Massachusetts Police"),
                          br(),
                          p("Explore the complete record of police traffic stops in Massachusetts over the past two decades - almost 12 million stops - as documented in data obtained from the Massachusetts Department of Transportation (MassDOT)."),
                          
                          div(id="dev-wait",
                              wellPanel(
                                icon('exclamation-triangle'),
                                h4("Disclaimer"),
                                em("Based on discussions with MassDOT, ACLUM understands that MassDOT’s historical record of traffic stops can change due to updates in reporting requirements, corrections of records, delays by municipalities in reporting their warnings and citations, and other factors.", #. See", 
                                   #a(href="X", "[SHOULD COVER IN BLOG POST]."), 
                                   "The record presented here of traffic stops occurring between 2002-2020 reflects only the MassDOT database as it was on February 4, 2021. The record of 2021 traffic stops reflects the MassDOT database as of March 7, 2022.",)
                              )
                          ),
                          
                          h3("About the Data"),
                          
                          h4("Where did the data come from?"),
                          "The data displayed here are the result of public records requests (", 
                          a(href="https://data.aclum.org/wp-content/uploads/2021/05/2021_01_21-ACLUM-Public-Records-Request-MassDOT-MUC-Data.pdf",
                            "No. 1"),  
                          " and ",
                          a(href="https://data.aclum.org/wp-content/uploads/2022/06/2022_02_11-ACLUM-Public-Records-Request-MassDOT-MUC-Data.pdf", "No. 2"),
                          ") ",
                          "with MassDOT filed by the",
                          a(href="https://www.aclum.org",
                            target="_blank",
                            "ACLU of Massachusetts."), 
                          
                          br(),br(),
                          h4("What's included?"),
                          "All stops by police - including the Massachusetts State Police and local town and city police departments - between January 1, 2002 and December 31, 2021. The dataset includes the date and time of the stop, the town where the stop occured, the ID of the police officer, the agency of the police officer, the nature of the traffic violation, and the race, gender, and age of the driver.",
                          
                          br(),br(),
                          h4("What's not included?"),
                          "The dataset does not denote whether or not a search was performed or whether any contraband (drugs or firearms) were found, nor does it include the badge number of the police officer. (Note that the officer ID and badge number are not the same.)",
                          
                          # br(),br(),
                          # h4("Why does it matter?"),
                          # 
                          # "Do we want a D4J blog to link to here? LINK BLOG",
                          # "Learn more about traffic policing on the ACLU of Massachusetts website:",
                          #   a(href="#",
                          #     target="_blank",
                          #     "INSERT LINK HERE."),
                          
                          br(),br(),
                          h4("Where can I get it?"),
                          
                          "The data sourced for all visualizations on this site are available for download",
                          actionLink("link_to_download", "here."),
                          
                          
                          h3("Source Code"),
                          p("Interested programmers can view the source code for this app, written in R, on",
                            a("GitHub.", href="https://github.com/ACLUmass/massdot-traffic-stops"))
                 ),
                 
                 # Download data --------------------------------------------
                 tabPanel("Download the Data", 
                          wellPanel(id="internal_well",
                                    p("This page allows you to select a subset of the MassDOT data to download. If you required the entire 3.2 GB dataset, you can download a compressed version from Google Drive", 
                                      a("here.", href="https://drive.google.com/file/d/10UuoizSId6sMMBetAa5XTaqGnWdVBgT-/view?usp=sharing", target="_blank"), 
                                      style="font-style: italic; text-align: center; font-weight: 100;"),
                                    fluidRow(
                                      column(4, selectizeInput("download_town", "Town/City", c("All cities and towns", all_towns))),
                                      column(4, selectizeInput("download_agency", 
                                                               label="Agency/Department", c("All agencies", all_agencies))),
                                      column(4, div(id="custom_label_div",
                                                    tags$b("Officer ID"),
                                                    a(icon("info-circle"), id="officer_tooltip",
                                                      `data-toggle`="tooltip", title=officer_tooltip_html),
                                                    selectizeInput("download_officer", 
                                                                   label=NULL, 
                                                                   c("Loading, please wait..." = "")))
                                      )),
                                    splitLayout(
                                      dateInput("download_start_date", "Start Date",
                                                value = "2002-01-01", min="2002-01-01", max="2021-12-31"),
                                      dateInput("download_end_date", "End Date",
                                                value = "2021-12-31", min="2002-01-01", max="2021-12-31")),
                                    actionButton("download_filters", "Apply Filters")
                          ),
                          
                          div(id="download_div",
                              withSpinner(textOutput("download_size", inline=T), type=4, color="#b5b5b5", size=0.5),
                              br(),
                              disabled(downloadButton("download_button", "Download"))
                          )
                 ),
                 
                 "Explore the Data:",
                 # Mapping stops --------------------------------------------
                 tabPanel("Mapping Stops", 
                          wellPanel(id="internal_well",
                                    splitLayout(
                                      dateInput("town_start_date", "Start Date",
                                                value = "2002-01-01", min="2002-01-01", max="2021-12-31"),
                                      dateInput("town_end_date", "End Date",
                                                value = "2021-12-31", min="2002-01-01", max="2021-12-31")),
                                    splitLayout(
                                      radioButtons("towns_radio", "Value Type", 
                                                   choiceValues=c("Total stops", 
                                                                  "Stops per capita"),
                                                   choiceNames=c("Total stops", 
                                                                 "Stops per 1,000 population"),
                                                   selected="Total stops", inline=F),
                                      div(id="town_log_span",
                                          div(tags$b("Numeric Scale")),
                                          checkboxInput("town_log", 
                                                        span("Plot logarithmic scale", 
                                                             a(icon("info-circle"), 
                                                               id="log_tooltip",
                                                               `data-toggle`="tooltip", 
                                                               title=log_tooltip_html)), 
                                                        value=T)
                                      )),
                                    actionButton("map_stops_button", "Go")),
                          withSpinner(leafletOutput("stops_by_town"), type=4, color="#b5b5b5", size=0.5)
                          
                 ),
                 
                 # Stops over time ------------------------------------------
                 tabPanel("Compare stops over time", 
                          wellPanel(id="internal_well",
                                    em("Select a town, agency, or officer to show all stops over time:"),
                                    fluidRow(
                                      column(6, splitLayout(selectizeInput("time_town", "Town/City", c("All cities and towns", all_towns)),
                                                            selectizeInput("time_agency", 
                                                                           label="Agency / Department", c("All agencies", all_agencies)))),
                                      
                                      column(6, splitLayout(div(id="custom_label_div",
                                                                tags$b("Officer ID"),
                                                                a(icon("info-circle"), id="officer_tooltip",
                                                                  `data-toggle`="tooltip", title=officer_tooltip_html),
                                                                selectizeInput("time_officer", 
                                                                               label=NULL, 
                                                                               c("Loading, please wait..." = ""))),
                                                            
                                                            selectizeInput("time_outcome", 
                                                                           label="Outcome", choices= all_outcomes)))),
                                    checkboxInput("compare_time", label="Select a second town, agency, or officer to compare?", value=F),
                                    conditionalPanel(
                                      condition = "input.compare_time == true",
                                      fluidRow(
                                        column(6, splitLayout(selectizeInput("time_town2", "Town/City", c("All cities and towns", all_towns)),
                                                              selectizeInput("time_agency2", 
                                                                             label="Agency / Department", c("All agencies", all_agencies)))),
                                        
                                        column(6, splitLayout(div(id="custom_label_div",
                                                                  tags$b("Officer ID"),
                                                                  a(icon("info-circle"), id="officer_tooltip",
                                                                    `data-toggle`="tooltip", title=officer_tooltip_html),
                                                                  selectizeInput("time_officer2", 
                                                                                 label=NULL, 
                                                                                 c("Loading, please wait..." = ""))),
                                                              
                                                              selectizeInput("time_outcome2", 
                                                                             label="Outcome", choices= all_outcomes))))),
                                    actionButton("time_button", "Go")),
                          radioButtons("time_type", "Plot by", choices=c("Year", "Month", "Day"), selected="Month", inline=T),#style="text-align: center;"),
                          withSpinner(plotlyOutput("stops_v_time"), type=4, color="#b5b5b5", size=0.5)
                 ),
                 
                 # Stops by offense ------------------------------------------
                 tabPanel("Stops by offense", 
                          wellPanel(id="internal_well",
                                    fluidRow(
                                      column(4, selectizeInput("offense_town", "Town/City", c("All cities and towns", all_towns))),
                                      column(4, selectizeInput("offense_agency", 
                                                               label="Agency/Department", c("All agencies", all_agencies))),
                                      column(4, div(id="custom_label_div",
                                                    tags$b("Officer ID"),
                                                    a(icon("info-circle"), id="officer_tooltip",
                                                      `data-toggle`="tooltip", title=officer_tooltip_html),
                                                    selectizeInput("offense_officer", 
                                                                   label=NULL, 
                                                                   c("Loading, please wait..." = ""))))
                                    ),
                                    
                                    splitLayout(
                                      dateInput("offense_start_date", "Start Date",
                                                value = "2002-01-01", min="2002-01-01", max="2021-12-31"),
                                      dateInput("offense_end_date", "End Date",
                                                value = "2021-12-31", min="2002-01-01", max="2021-12-31")),
                                    actionButton("offense_button", "Go")),#style="text-align: center;"),
                          withSpinner(plotlyOutput("offenses"), type=4, color="#b5b5b5", size=0.5)
                 ),
                 
                 # Agencies ------------------------------------------
                 # "Agency Lookup",
                 tabPanel("Agency Lookup", 
                          wellPanel(id="internal_well",
                                    selectizeInput("agency_agency", 
                                                   label="Agency/Department", 
                                                   c(all_agencies)),
                                    splitLayout(
                                      dateInput("agency_start_date", "Start Date",
                                                value = "2002-01-01", min="2002-01-01", max="2021-12-31"),
                                      dateInput("agency_end_date", "End Date",
                                                value = "2021-12-31", min="2002-01-01", max="2021-12-31")),
                                    actionButton("agency_button", "Go")),
                          withSpinner(uiOutput("agency_dashboard"), type=4, color="#b5b5b5", size=0.5)
                 ),
                 
                 # TOWN LOOKUP ----------------------------------------------
                 # "Town Lookup",
                 
                 # Town overview ----------------------------------------------
                 tabPanel("Town Lookup", 
                          wellPanel(id="internal_well",
                                    selectizeInput("townover_town", "Town/City", all_towns),
                                    splitLayout(
                                      dateInput("townover_start_date", "Start Date",
                                                value = "2002-01-01", min="2002-01-01", max="2021-12-31"),
                                      dateInput("townover_end_date", "End Date",
                                                value = "2021-12-31", min="2002-01-01", max="2021-12-31")),
                                    actionButton("townover_button", "Go")),
                          withSpinner(uiOutput("townover_dashboard"), type=4, color="#b5b5b5", size=0.5)
                 ),
                 
                 # Town stops by race ---------------------------------------
                 tabPanel("Race of Stops by Town", 
                          wellPanel(id="internal_well",
                                    splitLayout(
                                      selectizeInput("town_town", "Town/City", all_towns),
                                      selectizeInput("town_agency", 
                                                     label="Agency/Department",
                                                     choices=c("All agencies", all_agencies))
                                    ),
                                    splitLayout(
                                      dateInput("town_race_start_date", "Start Date",
                                                value = "2002-01-01", min="2002-01-01", max="2021-12-31"),
                                      dateInput("town_race_end_date", "End Date",
                                                value = "2021-12-31", min="2002-01-01", max="2021-12-31")),
                                    actionButton("town_button", "Go")
                          ),
                          hidden(img(src="race_legend.png", id="town_race_legend")),
                          withSpinner(plotlyOutput("town_demog"), type=4, color="#b5b5b5", size=0.5)),
                 
                 # "Officer ID Lookup",
                 # Officer stops by race ------------------------------------
                 tabPanel("Race of Stops by Officer", 
                          wellPanel(id="internal_well",
                                    splitLayout(
                                      selectizeInput("officer_agency", 
                                                     label="Agency / Department", all_agencies),
                                      div(id="custom_label_div",
                                          tags$b("Officer ID"),
                                          a(icon("info-circle"), id="officer_tooltip",
                                            `data-toggle`="tooltip", title=officer_tooltip_html),
                                          selectizeInput("officer_officer", 
                                                         label=NULL, 
                                                         choices=c("Loading, please wait..." = "")))
                                    ),
                                    splitLayout(
                                      dateInput("officer_race_start_date", "Start Date",
                                                value = "2002-01-01", min="2002-01-01", max="2021-12-31"),
                                      dateInput("officer_race_end_date", "End Date",
                                                value = "2021-12-31", min="2002-01-01", max="2021-12-31")),
                                    actionButton("officer_button", "Go")
                          ),
                          hidden(img(src="race_legend.png", id="officer_race_legend")),
                          withSpinner(plotlyOutput("officer_demog"), type=4, color="#b5b5b5", size=0.5)
                 )
                 
                 
    )
  ),
  
  div(id="footer",
      hr(),
      div(align="center",
          a(href="https://www.aclum.org/", target="_blank",
            img(src="Logo_CMYK_Massachusetts_Massachusetts.png", height="50px", 
                style="display: inline; margin: 10px;")),
          a(href="https://www.data.aclum.org/",  target="_blank",
            img(src="D4J-logo.png", height="50px", 
                style="display: inline; margin: 10px;"))),
      p("Please contact data4justice@aclum.org with questions.", align="center", style="opacity: 0.6;"),
      p("Icons by FontAwesome and", a("Flaticon", href="https://www.flaticon.com/free-icon/police_811976"), align="center", style="opacity: 0.6; margin-top: -10px; font-size: 1rem;")
  )
)