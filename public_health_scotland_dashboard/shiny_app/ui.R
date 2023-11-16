ui <- fluidPage(
  
  # Title
  titlePanel("Public Health Scotland Dashboard"),
  
  tabsetPanel(
    # Tab 1 - Main Page
    tabPanel("Main", sidebarLayout(
      
      # First row of plots
      sidebarPanel(
        checkboxGroupInput(inputId = "hb_code_input",
                           label = "Health Board",
                           choices = hb_codes_with_names,
                           selected = hb_codes,
                           inline = FALSE), width = 3),
      mainPanel(width = 9,

      # First row of plots  
        fluidRow(
        column(6, plotOutput("waiting_times_plot")),
        
        column(6, plotOutput("stays_by_sex"))
      ),
      # Second row of plots
      fluidRow(
        column(6, plotOutput("bed_occupancy_percent")),
        
        column(6, plotOutput("stays_by_simd"))
      )
      )
    )),
    
    
    
    # Tab 2 - Demographics - James & Lesley
    tabPanel("Demographics", sidebarLayout(
      
      # Sidebar
      sidebarPanel(
        radioButtons(inputId = "demog_input",
                     label = "Demographic Comparison",
                     choices = c("Sex", "Age", "SIMD"), 
                     inline = TRUE),
        
        checkboxGroupInput(inputId = "hb_code_input_demog",
                           label = "Health Board",
                           choices = hb_codes_with_names,
                           selected = hb_codes,
                           inline = FALSE),
        
        radioButtons(inputId = "admission_type_input",
                     label = "Admission Type",
                     choices = c("Day Cases" = "All Day cases",
                                 "Elective Inpatients",
                                 "Emergency Inpatients"),
                     selected = "Emergency Inpatients",
                     inline = FALSE),
        width = 3),
      
      mainPanel(width = 9,
        # First row, analysis
        fluidRow(
          column(12, 
                tags$h2(textOutput("deprivation_title")),
                p('"Stays" are the total number of days patients spend in
                  hospital during a continuous inpatient stay.'),
                p('Across Scotland, there are significantly fewer stays in hospital after the
                  COVID-19 pandemic. On average, stays in hospital are longer post-COVID.'),
                p(textOutput("deprivation_text")))
        ),
        # Second row, plots
        fluidRow(
          column(6, plotOutput("stays_plot")),
          
          column(6, plotOutput("stay_length_plot"))
        ))
    )),
    
    
    # Tab 3 - Waiting Times - Calum
    
    tabPanel("Waiting Times", sidebarLayout(
      
      # Sidebar
      sidebarPanel(width = 3,
        checkboxGroupInput("health_board", "Health Board",
                           choices = unique(wait$health_board),
                           selected = hb_names),
        selectInput("wait_duration", "Select wait duration", 
                    choices = c(
                      "Within 4 hours" = "number_within4hours_episode",
                      "Over 4 hours" = "number_over4hours_episode",
                      "Over 8 hours" = "number_over8hours_episode",
                      "Over 12 hours" = "number_over12hours_episode"),
                    selected = "Within 4 hours"),
        selectInput("selected_year", "Map year selection",
                    choices = unique(wait$year),
                    selected = unique(wait$year)[6])),
      
      
      mainPanel(width = 9,
                titlePanel("Hospital Waiting Times"),
                column(width = 6,
                       plotOutput("wait_plot")),
                column(width = 6, 
                       plotOutput("wait_map"))
      )
    )
    ),
      
# Tab 4 - Beds - Fergus
tabPanel("Bed Occupancy", sidebarLayout(
  sidebarPanel(
    checkboxGroupInput(inputId = "hb_code_input_beds",
                       label = "Health Board",
                       choices = hb_codes_with_names,
                       selected = hb_codes,
                       inline = FALSE),
    selectInput(inputId = "specialty_name",
                label = "Specialty",
                choices = distinct(bed_occupancy, specialty_name)),
    width = 3
  ),
  mainPanel(width = 9,
            plotOutput("bed_occupancy_total")
  )
)),

# Tab 5 - About
tabPanel("About", fluidPage(
  
  titlePanel("About the Dashboard"),
  mainPanel("Since the start of the COVID-19 pandemic, NHS Scotland has had to
            adjust and adapt to unforeseen and changing circumstances on a
            continual basis to meet the ever changing demand and challenges
            facing it. While the pandemic has been an unforeseen pressure,
            one predictable pressure is the winter period and effect it has on
            unscheduled care and subsequent capacity and demand on hospital
            services in the NHS.",
            br(),br(),
            "In this dashboard, ",
            tags$a("publicly available data",
                   href = "https://www.opendata.nhs.scot/dataset/inpatient-and-daycase-activity"),
            " has been analysed to answer two queries
            relating to pressures on the NHS:",  
            br(),
            "1. To what extent are the ‘winter crises’ reported by the media real?",
            br(),
            "2. How has the Covid-19 pandemic affected provision of acute care in Scotland?",
            br(),br(),
            "Data used in the project is used under the ",
            tags$a("Open Government Licence",
                   href = "https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/"),
            ".",
            br(),br(),
            "Authors: Fergus Cherry, Lesley Duff, James Pritchard and Calum Young"))
),
  )
)