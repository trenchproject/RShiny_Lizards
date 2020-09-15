# Load necessary packages
pkgs <- c('shiny', 'ggplot2','ggridges','magrittr','plotly','shinyBS', 'data.table', 'leaflet', 'shinyjs', 'shinysky', 'shinythemes', 'shinyWidgets', 'maps', 'shinycssloaders')
lapply(pkgs, library, character.only = TRUE)

month <- c(1:12)
names(month) <- c("January","February","March","April","May","June","July","August","September","October","November","December")
scenarios <- c("Normal","+1.5 °C","+2 °C")
hours <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")

org_done <- c("Anolis carolinensis", "Anolis oculatus", "Anolis semilineatus", "Coleonyx brevis", "Elgaria multicarinata", "Phrynosoma cornutum", "Phrynosoma douglasii", "Phrynosoma platyrhinos",  
              "Sceloporus graciosus", "Sceloporus grammicus", "Sceloporus magister", "Sceloporus merriami", "Sceloporus occidentalis", "Sceloporus woodi", 
              "Uta stansburiana")
# org <- c( "Ctenotus regius", "Ctenotus taeniolatus", "Ctenotus uber", "Dipsosaurus dorsalis", "Eulamprus kosciuskoi", 
#           "Eulamprus tympanum", "Hemiergis decresiensis", "Lepidophyma flavimaculatum", "Phrynosoma coronatum", "Platysaurus intermedius", "Podarcis muralis", "Psammodromus algirus", 
#           "Psammodromus hispanicus", "Pseudemoia entrecasteauxii", "Sceloporus variabilis", 
#           "Sphaerodactylus macrolepis", "Sphaerodactylus nicholsi", "Takydromus septentrionalis", "Takydromus sexlineatus", "Xantusia vigilis")

variables <- c("Month", "Hour", "Scenario", "Shade")

shinyUI <- 
   fluidPage (theme = shinytheme("united"),
              setBackgroundColor(color = "#F5F5F5"),
              useShinyjs(),
              
              title = "Climate Change and Lizards",
              titlePanel(
                div(tags$img(src="lizard_pic.png", height = 50), 
                    "Climate Change and Lizards")
              ),
              
              hr(),
              
              includeHTML("./intro.html"),
              
              hr(),
              
              strong("Select a species and explore their distribution, current status and the risk they may face from increasing temperature."),
              
              fluidRow(
                column(6, selectInput("species", label = "", choices = org_done))
              ),
              
              htmlOutput("species_info") %>% withSpinner(type = 7),
              
              tabsetPanel(id = "tabset", type = "tabs",
                          
                          tabPanel("Distribution Map",
                                   sidebarLayout(
                                     sidebarPanel(
                                       h3(icon("map-marked-alt"), " Distribution map"),
                                       p("Set the variables and hit \"Run\" to take a look at the thermal safety margins of the selected species within their distribution."),
                                       p("Click on the map to get more accurate data on TSM of that location."),
                                                                                      
                                       hr(),
                                       fluidRow(
                                         column(6,radioButtons("rows", label = "Horizontal facets", choices = variables)),
                                         column(6,radioButtons("columns", label = "Vertical facets", choices = variables, selected = "Hour"))
                                       ),
                                       
                                       select2Input("month", label = "Month", choices = names(month), multiple = TRUE, selected = "January"),
                                       select2Input("hour", label = "Hour", choices = hours, multiple = TRUE, selected = "01 PM"),
                                       
                                       fluidRow(
                                         column(6, checkboxGroupInput("scenario", label = "Scenario", choices = scenarios, selected = "Normal")),
                                         column(6, checkboxGroupInput("shade", label = "Shade", choices = c("Exposed", "Covered", "Thermoregulating"), selected = "Exposed"))
                                       ),  
                                       
                                       fluidRow(
                                         column(8, materialSwitch("map_onoff", "World map", status = "danger")),
                                         column(2, actionButton("run", "Run", styleclass = "primary"))
                                       ),
                                     ),
                                     
                                     mainPanel(
                                       br(), 
                                       switchInput(inputId = "scale", label = "Scale", onLabel = "Discrete", offLabel = "Continuous", inline = TRUE, value = TRUE, size = "small"),
                                       br(),
                                       fluidRow(column(12, plotOutput("plot1", click = "plot_click") %>% withSpinner(type = 7))),
                                       br(), 
                                       strong("Operative temperature and TSM of the clicked location"),
                                       verbatimTextOutput("info"),
                                       strong("Distribution of TSM"),
                                       fluidRow(column(12, plotOutput("density") %>% withSpinner(type = 7))),
                                       br(),
                                       column(8, offset = 2, align="center", leafletOutput("mymap")),
                                       br(),
                                     )
                                   )
                          ),
                          
                          
                          tabPanel("Plot", 
                                   sidebarLayout(
                                     sidebarPanel(
                                       h3(icon("chart-bar"), " Plot"),
                                       p("Explore the change in thermal safety margins of the selected species throughout the day in different months."),
                                       p("The shapes on the plot represent the frequency of TSM within the species."),
                                       
                                       radioButtons("facet", label = "Facet", choices = c("Shade", "Scenario"), inline = TRUE),
                                       
                                       selectInput("month_2", label = "Month", choices = month)
                                     ),
                                     
                                     mainPanel(
                                       column(12, align = "center", plotOutput("plot2", width = "100%") %>% withSpinner(type = 7))
                                     )
                                   )
                          ),
                          
                          tabPanel("Thermal Performance Curve",
                                   sidebarLayout(
                                     sidebarPanel(
                                       h3(icon("chart-line"), " Thermal performance curve"),
                                       p("Take a look at the selected species' thermal performance curve and how varying environmental conditions affect their performance."),
                                       
                                       selectInput("month_tpc", label = "Month", choices = month, selected = 1),
                                       selectInput("hour_tpc", label = "Hour", choices = hours, selected = "01 PM"),
                                       
                                       fluidRow(
                                         column(6, radioButtons("scenario_tpc", label = "Scenario", choices = scenarios, selected = "Normal")),
                                         column(6, radioButtons("shade_tpc", label = "Shade", choices = c("Exposed", "Covered"), selected = "Exposed")),
                                       )
                                     ),
                                     
                                     mainPanel(
                                        plotOutput("TPC") %>% withSpinner(type = 7)),
                                     ),
                                   
                                     br(),
                          )
                          
                          # tabPanel("Raw Data", 
                          #          sidebarLayout(
                          #            sidebarPanel(
                          #              h3("Raw Data"),
                          #              p("Get the numric data on thermal safety margins here."),
                          #              p("Select \"Annual\" and set TSM to 100 to see all our data."),
                          #              
                          #              hr(),
                          #              
                          #              h5("Show results for"),
                          #              
                          #              selectInput("month_data", label = "Month", choices = c(month, "Annual")),
                          #              
                          #              numericInput("value", label = "TSM Less than", value = 0),
                          #              
                          #              fluidRow(
                          #                column(6, radioButtons("scenario_data", label = "Scenario", choices = scenarios, selected = "Normal")),
                          #                column(6, radioButtons("shade_data", label = "Shade", choices = c("Exposed", "Covered"), selected = "Exposed")),
                          #              )
                          #            ),
                          #            
                          #            mainPanel(
                          #              br(),
                          #              h4("Mean thermal safety margins across the distibution"),
                          #              htmlOutput("text_data") %>% withSpinner(type = 7)
                          #            )
                          #          )
                          # )
              ),
              
              bsTooltip("shade", "\"Thermoregulating\" assumes lizards actively moving between shade and sun to approach their body temperature to Topt"),
              bsTooltip("scale", "Colors by categories or continuous gradient"),
              bsTooltip("map_onoff", "Show/hide world map")
   )