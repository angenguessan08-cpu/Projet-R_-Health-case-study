# =========================================================
# PUBLIC HEALTH ANALYTICS 
# =========================================================

# =========================================================
#  INSTALL PACKAGES
# =========================================================
required_packages <- c(
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "tidyverse",
  "readr",
  "lubridate",
  "plotly",
  "DT",
  "scales"
)

new_packages <- required_packages[
  !required_packages %in% installed.packages()[, "Package"]
]

if(length(new_packages)){
  install.packages(new_packages)
}


# =========================================================
# 1. LIBRARIES
# =========================================================
library(shiny)
library(shinydashboard)
library(shinyWidgets)

library(tidyverse)
library(readr)
library(lubridate)

library(plotly)
library(DT)

library(scales)

# =========================================================
# 2. LOAD DATA
# =========================================================
health_case_study <- read_csv(
  "outputs/health_case_study_clean_final.csv"
)

# =========================================================
# 3. DATA PREPARATION
# =========================================================
health_case_study <- health_case_study %>%
  mutate(
    consultation_date = as.Date(consultation_date),
    
    month = floor_date(
      consultation_date,
      unit = "month"
    )
  )

# =========================================================
# 4. PREMIUM COLORS
# =========================================================
premium_palette <- c(
  "#7C3AED",
  "#A855F7",
  "#C084FC",
  "#8B5CF6",
  "#6366F1",
  "#EC4899",
  "#F472B6",
  "#818CF8",
  "#6D28D9",
  "#9333EA"
)

# =========================================================
# 5. UI
# =========================================================
ui <- dashboardPage(
  
  skin = "purple",
  
  # =======================================================
  # HEADER
  # =======================================================
  dashboardHeader(
    title = span(
      "HealthPulse",
      style = "
        font-weight:700;
        font-size:22px;
        color:white;
      "
    )
  ),
  
  # =======================================================
  # SIDEBAR
  # =======================================================
  dashboardSidebar(
    
    width = 260,
    
    sidebarMenu(
      
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("chart-line")
      ),
      
      menuItem(
        "Data Table",
        tabName = "table",
        icon = icon("table")
      )
    ),
    
    br(),
    
    pickerInput(
      inputId = "region",
      label = "Select Region",
      choices = c(
        "All",
        sort(unique(health_case_study$region))
      ),
      selected = "All",
      multiple = FALSE,
      options = list(
        `live-search` = TRUE
      )
    ),
    
    pickerInput(
      inputId = "diagnosis",
      label = "Select Diagnosis",
      choices = c(
        "All",
        sort(unique(health_case_study$diagnosis))
      ),
      selected = "All",
      multiple = FALSE,
      options = list(
        `live-search` = TRUE
      )
    )
  ),
  
  # =======================================================
  # BODY
  # =======================================================
  dashboardBody(
    
    tags$head(
      
      tags$style(HTML("

      /* =====================================================
      GOOGLE FONT
      ===================================================== */
      @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700&display=swap');

      /* =====================================================
      GLOBAL
      ===================================================== */
      body,
      .content-wrapper,
      .right-side {
        font-family: 'Poppins', sans-serif !important;
        background-color: #F5F7FB !important;
      }

      /* =====================================================
      NAVBAR
      ===================================================== */
      .skin-purple .main-header .navbar {
        background: linear-gradient(
          135deg,
          #7C3AED,
          #A855F7
        ) !important;
      }

      .skin-purple .main-header .logo {
        background: linear-gradient(
          135deg,
          #6D28D9,
          #9333EA
        ) !important;

        color: white !important;
        border-bottom: 0px !important;
      }

      /* =====================================================
      SIDEBAR
      ===================================================== */
      .skin-purple .main-sidebar {
        background: #111827 !important;
      }

      .skin-purple .sidebar-menu > li.active > a {
        background: linear-gradient(
          135deg,
          #7C3AED,
          #A855F7
        ) !important;

        border-left-color: #C084FC !important;
      }

      .skin-purple .sidebar-menu > li:hover > a {
        background: #1F2937 !important;
      }

      /* =====================================================
      KPI BOXES
      ===================================================== */
 .small-box {
    border-radius: 15px !important;
    overflow: hidden !important;
    border: none !important;
    box-shadow:
      0 4px 20px rgba(0,0,0,0.05) !important;
}

.kpi-1 {
    background: linear-gradient(
        135deg,
        #7C3AED,
        #A855F7
    ) !important;
}

.kpi-2 {
    background: linear-gradient(
        135deg,
        #8B5CF6,
        #C084FC
    ) !important;
}

.kpi-3 {
    background: linear-gradient(
        135deg,
        #6366F1,
        #818CF8
    ) !important;
}

.kpi-4 {
    background: linear-gradient(
        135deg,
        #EC4899,
        #F472B6
    ) !important;
}

.small-box  {
    font-size: 45px !important; 
    font-weight: 500 !important;
}






.small-box p {
    font-size: 20px !important;
    font-weight: 500 !important;
}

.small-box .icon {
    top: 15px !important;
    font-size: 65px !important;
    opacity: 0.15 !important;
}
      /* =====================================================
      BOXES
      ===================================================== */
      .box {
        border-radius: 18px !important;
        border-top: 0px !important;

        box-shadow:
          0 4px 18px rgba(0,0,0,0.05);
      }

      .box-header {
        border-top-left-radius: 18px !important;
        border-top-right-radius: 18px !important;
      }

      .box-title {
        font-weight: 600 !important;
        font-size: 25px !important;
        color: white !important;
      }

      /* =====================================================
      TITLES
      ===================================================== */
      .dashboard-title {
        font-size: 30px;
        font-weight: 700;
        color: #111827;
        margin-bottom: 5px;
      }

      .dashboard-subtitle {
        color: #6B7280;
        margin-bottom: 25px;
        font-size: 15px;
      }

      /* =====================================================
      FILTERS
      ===================================================== */
      .form-control,
      .selectize-input {
        border-radius: 10px !important;
      }

      /* =====================================================
      DATATABLE
      ===================================================== */
      table.dataTable thead th {
        background: #7C3AED !important;
        color: white !important;
        border: none !important;
      }

      "))
      
    ),
    
    tabItems(
      
      # ===================================================
      # DASHBOARD TAB
      # ===================================================
      tabItem(
        
        tabName = "dashboard",
        
        div(
          class = "dashboard-title",
          "Public Health Analytics Dashboard"
        ),
        
        div(
          class = "dashboard-subtitle",
          "Interactive monitoring of healthcare consultations and epidemiological trends"
        ),
        
        # =================================================
        # KPI ROW
        # =================================================
        fluidRow(
          
          valueBoxOutput(
            "total_cases_box",
            width = 3
          ),
          
          valueBoxOutput(
            "regions_box",
            width = 3
          ),
          
          valueBoxOutput(
            "diagnosis_box",
            width = 3
          ),
          
          valueBoxOutput(
            "latest_month_box",
            width = 3
          )
        ),
        
        
        # =================================================
        # FIRST ROW
        # =================================================
        fluidRow(
          
          box(
            title = "Consultations by Diagnosis",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "diagnosis_plot",
              height = "320px"
            )
          ),
          
          box(
            title = "Monthly Consultation Trend",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "monthly_plot",
              height = "320px"
            )
          )
        ),
        
        # =================================================
        # SECOND ROW
        # =================================================
        fluidRow(
          
          box(
            title = "Regional Distribution",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "region_plot",
              height = "380px"
            )
          ),
          
          box(
            title = "Diagnosis Summary Table",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            
            DTOutput("summary_table")
          )
        )
      ),
      
      # ===================================================
      # DATA TABLE TAB
      # ===================================================
      tabItem(
        
        tabName = "table",
        
        fluidRow(
          
          box(
            title = "Complete Dataset",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            DTOutput("full_table")
          )
        )
      )
    )
  )
)

# =========================================================
# 6. SERVER
# =========================================================
server <- function(input, output) {
  
  # =======================================================
  # FILTERED DATA
  # =======================================================
  filtered_data <- reactive({
    
    data <- health_case_study
    
    if(input$region != "All"){
      
      data <- data %>%
        filter(region == input$region)
    }
    
    if(input$diagnosis != "All"){
      
      data <- data %>%
        filter(diagnosis == input$diagnosis)
    }
    
    data
  })
  
  # =======================================================
  # KPI 1
  # =======================================================
  output$total_cases_box <- renderValueBox({
    
    valueBox(
      value = comma(nrow(filtered_data())),
      subtitle = "Total Consultations",
      icon = icon("hospital"),
      color = "purple"
    )
  })
  
  # =======================================================
  # KPI 2
  # =======================================================
  output$regions_box <- renderValueBox({
    
    valueBox(
      value = n_distinct(filtered_data()$region),
      subtitle = "Regions",
      icon = icon("map"),
      color = "fuchsia"
    )
  })
  
  # =======================================================
  # KPI 3
  # =======================================================
  output$diagnosis_box <- renderValueBox({
    
    valueBox(
      value = n_distinct(filtered_data()$diagnosis),
      subtitle = "Diagnosis Categories",
      icon = icon("notes-medical"),
      color = "teal"
    )
  })
  
  # =======================================================
  # KPI 4
  # =======================================================
  output$latest_month_box <- renderValueBox({
    
    latest_month <- max(
      filtered_data()$consultation_date,
      na.rm = TRUE
    )
    
    valueBox(
      value = format(latest_month, "%b %Y"),
      subtitle = "Latest Record",
      icon = icon("calendar"),
      color = "pink"
    )
  })
  
  # =======================================================
  # DIAGNOSIS PLOT
  # =======================================================
  output$diagnosis_plot <- renderPlotly({
    
    plot_data <- filtered_data() %>%
      group_by(diagnosis) %>%
      summarise(
        total = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(total))
    
    p <- ggplot(
      plot_data,
      aes(
        x = reorder(diagnosis, total),
        y = total,
        fill = diagnosis
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        x = "Diagnosis",
        y = "Consultations"
      ) +
      
      scale_fill_manual(
        values = premium_palette
      ) +
      
      theme_minimal() +
      
      theme(
        
        legend.position = "none",
        
        axis.text = element_text(
          size = 11,
          color = "#374151"
        ),
        
        axis.title = element_text(
          size = 12,
          face = "bold"
        )
      )
    
    ggplotly(p)
  })
  
  # =======================================================
  # MONTHLY TREND
  # =======================================================
  output$monthly_plot <- renderPlotly({
    
    monthly_data <- filtered_data() %>%
      group_by(month) %>%
      summarise(
        total = n(),
        .groups = "drop"
      )
    
    p <- ggplot(
      monthly_data,
      aes(
        x = month,
        y = total
      )
    ) +
      
      geom_line(
        color = "#7C3AED",
        linewidth = 1.5
      ) +
      
      geom_point(
        color = "#A855F7",
        size = 3
      ) +
      
      labs(
        x = "Month",
        y = "Consultations"
      ) +
      
      theme_minimal() +
      
      theme(
        
        axis.text = element_text(
          size = 11,
          color = "#374151"
        ),
        
        axis.title = element_text(
          size = 12,
          face = "bold"
        )
      )
    
    ggplotly(p)
  })
  
  # =======================================================
  # REGION PLOT
  # =======================================================
  output$region_plot <- renderPlotly({
    
    region_data <- filtered_data() %>%
      group_by(region) %>%
      summarise(
        total = n(),
        .groups = "drop"
      )
    
    p <- ggplot(
      region_data,
      aes(
        x = region,
        y = total,
        fill = region
      )
    ) +
      
      geom_col() +
      
      labs(
        x = "Region",
        y = "Consultations"
      ) +
      
      scale_fill_manual(
        values = premium_palette
      ) +
      
      theme_minimal() +
      
      theme(
        
        legend.position = "none",
        
        axis.text = element_text(
          size = 10,
          color = "#374151"
        ),
        
        axis.title = element_text(
          size = 12,
          face = "bold"
        )
      )
    
    ggplotly(p)
  })
  
  # =======================================================
  # SUMMARY TABLE
  # =======================================================
  output$summary_table <- renderDT({
    
    filtered_data() %>%
      group_by(diagnosis) %>%
      summarise(
        consultations = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(consultations))
    
  },
  
  options = list(
    pageLength = 8,
    scrollX = TRUE
  ),
  
  rownames = FALSE,
  
  class = "compact stripe hover"
  )
  
  # =======================================================
  # FULL TABLE
  # =======================================================
  output$full_table <- renderDT({
    
    filtered_data()
    
  },
  
  options = list(
    pageLength = 10,
    scrollX = TRUE
  ),
  
  rownames = FALSE,
  
  class = "compact stripe hover"
  )
}

# =========================================================
# 7. RUN APPLICATION
# =========================================================
shinyApp(
  ui = ui,
  server = server
)