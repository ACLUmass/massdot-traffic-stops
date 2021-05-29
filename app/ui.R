library(ggplot2)
library(shiny)
library(shinycssloaders)
library(showtext)
library(leaflet)
library(plotly)
library(shinyjs)

all_agencies <- readRDS("data/all_agencies.RDS")
all_towns <- readRDS("data/all_towns.rds")

# Initialization --------------------------------------------------------------

# Set ggplot settings
theme_set(theme_minimal())

# Load ggplot-friendly font using show_text
sysfonts::font_add("gtam", "GT-America-Standard-Regular.ttf",
                   bold = "GT-America-Standard-Bold.ttf")
sysfonts::font_add("gtam-cond", "GT-America-Condensed-Regular.ttf",
                   bold = "GT-America-Condensed-Bold.ttf")
showtext_auto()

theme_update(text = element_text(family="gtam"),
             plot.title = element_text(family="gtam", face="bold"))

# UI --------------------------------------------------------------------------

fluidPage(
  useShinyjs(),  # Set up shinyjs
  theme = "massdot_app.css",
          
          # Add favicon          
          tags$head(
              tags$link(rel = "shortcut icon", href = "favicon.ico"),
              tags$link(rel = "icon", type = "image/png", sizes = "512x512", href = "favicon.png")
          ),
          
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
                                 icon('hourglass-half'),
                                 h4("Please be patient"),
                                 em("This dashboard's underlying dataset consists of over 200 million cells across 11.8 million traffic stops. Please have patience as certain pages may take a few seconds to load.")
                               )
                           ),
                                    
                                    h3("About the Data"),
                                    h4("Where did the data come from?"),
                                    "The data displayed here are the result of a", 
                                    a(href="https://data.aclum.org/wp-content/uploads/2021/05/2021_01_21-ACLUM-Public-Records-Request-MassDOT-MUC-Data.pdf",
                                      "public records request"),
                                    "with MassDOT filed by the",
                                      a(href="https://www.aclum.org",
                                        target="_blank",
                                        "ACLU of Massachusetts."),
                                    
                                    br(),br(),
                                    h4("What's included?"),
                                    "All stops by police - including the Massachusetts State Police and local town and city police departments - between January 1, 2002 and February 4, 2021. The dataset includes the date and time of the stop, the town where the stop occured, the ID of the police officer, the agency of the police officer, the nature of the traffic violation, and the race, gender, and age of the driver.",
                                    
                                    br(),br(),
                                    h4("What's not included?"),
                                    "The dataset does not denote whether or not a search was performed or whether any contraband (drugs or firearms) were found, nor does it include the badge number of the police officer. (Note that the officer ID and badge number are not the same.)",
                                    
                                    br(),br(),
                                    h4("Why does it matter?"),
                                    
                                    "Do we want a D4J blog to link to here? LINK BLOG",
                                    "Learn more about traffic policing on the ACLU of Massachusetts website:",
                                      a(href="#",
                                        target="_blank",
                                        "INSERT LINK HERE."),
                                    
                                    br(),br(),
                                    h4("Where can I get it?"),
                                    
                                    "The data sourced for all visualizations on this site are available for download",
                                      actionLink("link_to_download", "here."),
                                    
                                    h3("Source Code"),
                                    p("Interested programmers can view the source code for this app, written in R, on",
                                      a("GitHub - UPDATE URL.", href="#"))
                           ),
                           
                           # Download data --------------------------------------------
                           tabPanel("Download the Data", 
                                    wellPanel(id="internal_well",
                                              p("This page allows you to select a subset of the MassDOT data to download. If you required the entire 2.8 GB dataset, you can download it from GitHub", 
                                                a("here - ADD LINK.", href="#"), 
                                                style="font-style: italic; text-align: center; font-weight: 100;"),
                                      splitLayout(
                                        selectizeInput("download_town", "Town/City", c("All cities and towns", all_towns)),
                                        selectizeInput("download_agency", 
                                                       label="Agency/Department", c("All agencies", all_agencies)),
                                        selectizeInput("download_officer", 
                                                       label="Officer ID", 
                                                       c("Loading, please wait..." = ""))),
                                      splitLayout(
                                        dateInput("download_start_date", "Start Date",
                                                  value = "2002-01-01", min="2002-01-01", max="2021-02-04"),
                                        dateInput("download_end_date", "End Date",
                                                  value = "2021-02-04", min="2002-01-01", max="2021-02-04")),
                                      actionButton("download_filters", "Apply Filters")
                                    ),
                                    
                                    div(id="download_div",
                                        withSpinner(textOutput("download_size", inline=T), type=4, color="#b5b5b5", size=0.5),
                                      br(),
                                      disabled(downloadButton("download_button", "Download"))
                                      )
                                    ),
                           
                           "Aggregate Statistics",
                           # Stops by town --------------------------------------------
                           tabPanel("Stops by town", 
                                    wellPanel(id="internal_well",
                                      splitLayout(
                                        dateInput("town_start_date", "Start Date",
                                                  value = "2002-01-01", min="2002-01-01", max="2021-02-04"),
                                        dateInput("town_end_date", "Start Date",
                                                  value = "2021-02-04", min="2002-01-01", max="2021-02-04")),
                                      splitLayout(
                                        radioButtons("towns_radio", "Value Type", 
                                                     choices=c("Total stops", 
                                                               "Stops per capita"),
                                                     selected="Total stops", inline=T),
                                        checkboxInput("town_log", "Plot logarithmic scale", T)),
                                      actionButton("map_stops_button", "Go")),
                                    withSpinner(leafletOutput("stops_by_town"), type=4, color="#b5b5b5", size=0.5)
                                    
                           ),
                           
                           # Stops over time ------------------------------------------
                           tabPanel("Stops over time", 
                                    wellPanel(id="internal_well",
                                      splitLayout(
                                        selectizeInput("time_town", "Town/City", c("All cities and towns", all_towns)),
                                        selectizeInput("time_agency", 
                                                       label="Agency/Department", c("All agencies", all_agencies)),
                                        selectizeInput("time_officer", 
                                                       label="Officer ID", 
                                                       c("Loading, please wait..." = ""))),
                                    actionButton("time_button", "Go")),
                                    radioButtons("time_type", "Plot by", choices=c("Year", "Month", "Day"), selected="Month", inline=T),#style="text-align: center;"),
                                    withSpinner(plotlyOutput("stops_v_time"), type=4, color="#b5b5b5", size=0.5)
                                    ),
                           
                           # Stops by offense ------------------------------------------
                           tabPanel("Stops by offense", 
                                    wellPanel(id="internal_well",
                                              splitLayout(
                                                selectizeInput("offense_town", "Town/City", c("All cities and towns", all_towns)),
                                                selectizeInput("offense_agency", 
                                                               label="Agency/Department", c("All agencies", all_agencies)),
                                                selectizeInput("offense_officer", 
                                                               label="Officer ID", 
                                                               c("Loading, please wait..." = ""))),
                                              
                                              splitLayout(
                                                dateInput("offense_start_date", "Start Date",
                                                          value = "2002-01-01", min="2002-01-01", max="2021-02-04"),
                                                dateInput("offense_end_date", "Start Date",
                                                          value = "2021-02-04", min="2002-01-01", max="2021-02-04")),
                                              actionButton("offense_button", "Go")),#style="text-align: center;"),
                                    withSpinner(plotlyOutput("offenses"), type=4, color="#b5b5b5", size=0.5)
                           ),
                                    
                           # Agencies ------------------------------------------
                           "Agency Lookup",
                           tabPanel("Agency overview", 
                                    wellPanel(id="internal_well",
                                      selectizeInput("agency_agency", 
                                                     label="Agency/Department", 
                                                     c(all_agencies)),
                                      splitLayout(
                                        dateInput("agency_start_date", "Start Date",
                                                  value = "2002-01-01", min="2002-01-01", max="2021-02-04"),
                                        dateInput("agency_end_date", "Start Date",
                                                  value = "2021-02-04", min="2002-01-01", max="2021-02-04")),
                                      actionButton("agency_button", "Go")),
                                    withSpinner(uiOutput("agency_dashboard"), type=4, color="#b5b5b5", size=0.5)
                           ),

                           # TOWN LOOKUP ----------------------------------------------
                           "Town Lookup",
                           
                           # Town overview ----------------------------------------------
                           tabPanel("Town overview", 
                                    wellPanel(id="internal_well",
                                              selectizeInput("townover_town", "Town/City", all_towns),
                                              splitLayout(
                                                dateInput("townover_start_date", "Start Date",
                                                          value = "2002-01-01", min="2002-01-01", max="2021-02-04"),
                                                dateInput("townover_end_date", "Start Date",
                                                          value = "2021-02-04", min="2002-01-01", max="2021-02-04")),
                                              actionButton("townover_button", "Go")),
                                    withSpinner(uiOutput("townover_dashboard"), type=4, color="#b5b5b5", size=0.5)
                                    ),
                           
                           # Town stops by race ---------------------------------------
                           tabPanel("Stops by Race", 
                                    wellPanel(id="internal_well",
                                      splitLayout(
                                        selectizeInput("town_town", "Town/City", all_towns),
                                        selectizeInput("town_agency", 
                                                       label="Agency/Department",
                                                       choices=c("Loading, please wait..." = ""))
                                      ),
                                      splitLayout(
                                        dateInput("town_race_start_date", "Start Date",
                                                  value = "2002-01-01", min="2002-01-01", max="2021-02-04"),
                                        dateInput("town_race_end_date", "Start Date",
                                                  value = "2021-02-04", min="2002-01-01", max="2021-02-04")),
                                      actionButton("town_button", "Go")
                                    ),
                                    withSpinner(plotlyOutput("town_demog"), type=4, color="#b5b5b5", size=0.5)),

                           "Officer ID Lookup",
                           # Officer stops by race ------------------------------------
                           tabPanel("Stops by Race", 
                                    wellPanel(id="internal_well",
                                      splitLayout(
                                        selectizeInput("officer_agency", 
                                                       label="Agency/Department", all_agencies),
                                        selectizeInput("officer_officer", 
                                                       "Officer ID", 
                                                       choices=c("Loading, please wait..." = ""))
                                        ),
                                      splitLayout(
                                        dateInput("officer_race_start_date", "Start Date",
                                                  value = "2002-01-01", min="2002-01-01", max="2021-02-04"),
                                        dateInput("officer_race_end_date", "Start Date",
                                                  value = "2021-02-04", min="2002-01-01", max="2021-02-04")),
                                      actionButton("officer_button", "Go")
                                    ),
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
              p("Icons by Those Icons from", a("flaticon.com", href="https://www.flaticon.com/free-icon/police_811976"), align="center", style="opacity: 0.6;")
          )
)