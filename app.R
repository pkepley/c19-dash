library(shiny)
library(scales)
library(tidyverse)
library(ggthemes)
library(shinyWidgets)
library(DT)
library(lubridate)

# State data
state_data <- data.frame(state.abb, state.name, state.division, state.region)
state_data <- as_tibble(state_data) %>%
    mutate(state.name.lower = tolower(state.name))

# Regions / division levels
regions <- levels(state.region)
divisions <- levels(state.division)
divisions_by_region <- state_data %>% 
    group_by(state.region, state.division) %>% 
    summarise() %>%
    ungroup()

# Start in the midwest!
starting_region <- "North Central" 
starting_region_states <- state_data %>%
    filter(state.region == starting_region) %>%
    select(state.name)
starting_state_options <- lapply(as.list(starting_region_states$state.name), as.character)
starting_state_options <- unlist(starting_state_options)

# Pull data
source("config.R") # read in app_data_dir
us_df     <- read_csv(paste(app_data_dir, "us_cases.csv",  sep="/"))

# Latest data
df_us_latest <- us_df %>% 
    filter(report_date == max(report_date))

# Define UI for application that draws a histogram
ui <- function(request){
fluidPage(
    ## Application title
    titlePanel("COVID-19 Cases by Reporting Date"),

    fluidRow(
        column(2,
            selectInput("region", "Select Region:", regions, selected="North Central"),
            pickerInput(
                inputId = "states",
                label = "Select State:",
                choices  = starting_state_options,
                selected = starting_state_options,
                options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format`="count",
                    `count-selected-text` = "Showing {0} of {1} States"
                ),
                multiple = TRUE                
            ),
            sliderInput(
               inputId = "start_date",
               label = "Select First Display Date:",
               min = as_date("2020-03-15"),
               max = as_date(today()),
               step = 14,
               ticks = FALSE,
               value = as_date("2020-03-15"),
               timeFormat = "%Y-%m-%d"
            ),
            
            radioButtons("cumu_or_new", label = "Cumulative or Newly Reported:", choices = c("Cumulative", "Newly Reported"), selected = "Cumulative"),
            radioButtons("raw_or_percap", label = "Raw or Per Capita:", choices = c("Raw", "Per Capita"), selected="Raw"),
            radioButtons("transform", label = "Scaling:", choices = c("None", "Log"), selected="None"),
	    tags$b("Toggles:"),
            checkboxInput("show_smoother", "Show 7 Day Moving Average", value = FALSE, width = NULL),
            checkboxInput("hide_original", "Hide original", value = FALSE, width = NULL),
            bookmarkButton(label = "Share Current Plots",
                           title = "Bookmark the current plots and get a URL for sharing.")
	    ),
        column(7,
            plotOutput("Confirmed"),
            plotOutput("Deaths"),            
            plotOutput("ConditionedDeathRate"),
        ),
        column(3,
            dataTableOutput("Table")
        )
    ),

    hr(),
    fluidRow(
      column(12,
	p("Data sourced from the JHU CSSE COVID-19 Data Repository:",
          br(),
	  icon("github"),
	  a("https://github.com/CSSEGISandData/COVID-19", href = "https://github.com/CSSEGISandData/COVID-19")
	),
	p("Repository for this app:",
          br(),
  	  icon("github"),
	  a("https://github.com/pkepley/c19-dash", href = "https://github.com/pkepley/c19-dash")
	)
      )
    )
)
}

# Define server logic
server <- function(input, output, session) {
    
    # Update the selection list for region division
    observeEvent(input$region, {              
        region_states <- state_data %>%
            filter(state.region == input$region) %>%
            select(state.name)
        
        state_options <- lapply(as.list(region_states$state.name), as.character)
        
        updatePickerInput(
            session,
             "states",
            choices = state_options,
            selected = state_options
        )
    })

    # Plot everything
    observe({    
        # Incremental or cumualative
        cumu_or_new = input$cumu_or_new
        if (cumu_or_new == 'Newly Reported'){
            cumu_or_new = 'New'
        }
        
        # Show a smoother or not        
        if (input$show_smoother){
            smoothing_geom <- geom_line
        }
        else{
            smoothing_geom = geom_blank
        }
        
        # Show line for original data or not
        if (input$hide_original){
            line_geom = geom_blank
        }
        else{
            line_geom = geom_line
        }
        

        # Display raw or Per Capita
        if (input$raw_or_percap == "Raw"){
            death_raw_or_percap_string <- paste("Number of", cumu_or_new, "Confirmed Deaths", sep=" ")
            count_raw_or_percap_string <- paste("Number of", cumu_or_new, "Confirmed Cases", sep=" ")
            death_raw_or_percap_var <- paste(cumu_or_new, "Deaths", sep="_")
            count_raw_or_percap_var <- paste(cumu_or_new, "Confirmed", sep="_")
            death_raw_or_percap_var_MA <- paste(cumu_or_new, "Deaths_MA", sep="_")
            count_raw_or_percap_var_MA <- paste(cumu_or_new, "Confirmed_MA", sep="_")            
            raw_or_percap_label_type = comma
        }
        else{
            death_raw_or_percap_string <- paste(cumu_or_new, "Confirmed Deaths Per Capita", sep=" ")
            count_raw_or_percap_string <- paste(cumu_or_new, "Confirmed Cases Per Capita", sep=" ")
            death_raw_or_percap_var <- paste(cumu_or_new, "Deaths_Per_Capita", sep="_")
            count_raw_or_percap_var <- paste(cumu_or_new, "Confirmed_Per_Capita", sep="_")
            death_raw_or_percap_var_MA <- paste(cumu_or_new, "Deaths_MA_Per_Capita", sep="_")
            count_raw_or_percap_var_MA <- paste(cumu_or_new, "Confirmed_MA_Per_Capita", sep="_")
            
            raw_or_percap_label_type = percent
        }
        
        # Handle plotting scale
        if (input$transform == "None"){
            transform_type_string <- ""
            y_axis_scaling <- scale_y_continuous(labels = raw_or_percap_label_type)
        }
        else{
            y_axis_scaling <- scale_y_continuous(trans="log10", labels = raw_or_percap_label_type)
        }
        
        # Dataset to Plot
        us_df_to_plot <- us_df %>% 
            filter(report_date >= input$start_date, 
                   Province_State %in% input$states)
        print(us_df_to_plot %>% head())
        
        # Table of most recent data
        region_table <- df_us_latest %>% 
            filter(Province_State %in% input$states) %>%
            select(c("Province_State", "Cumulative_Confirmed", "Cumulative_Deaths")) %>%
            rename(State = Province_State, 
                   'Cumulative Confirmed' = Cumulative_Confirmed,
                   'Cumulative Deaths' = Cumulative_Deaths) %>%
            datatable(options = list(paging = FALSE, searching = FALSE, dom = 'f'),
                      rownames = FALSE) %>%
            formatCurrency('Cumulative Confirmed', currency = "", interval = 3, mark = ",", digits = 0) %>%
            formatCurrency('Cumulative Deaths', currency = "", interval = 3, mark = ",", digits = 0)                     
            
        
        # Render the table for the region
        output$Table <- renderDataTable(region_table)

        
        # Confirmed Cases Plot
        output$Confirmed <- renderPlot({            
            ggplot(data = us_df_to_plot) + 
                line_geom(mapping = aes_string(x = "report_date", y = count_raw_or_percap_var, color = "Province_State")) + 
                smoothing_geom(mapping = aes_string(x = "report_date", y = count_raw_or_percap_var_MA, color = "Province_State"), lwd=1) +
                xlab("Reporting Date") +
                ylab(count_raw_or_percap_string) +
                ggtitle(paste(count_raw_or_percap_string, " by Reporting Date", sep="")) +
                labs(color = "State") + 
                theme_classic() +
                theme(text = element_text(size=14),
                      legend.title = element_text(size=18),
                      axis.text = element_text(size=14),
                      axis.title = element_text(size=16)) +         
                scale_colour_tableau('Tableau 20') +
                y_axis_scaling
        })

        # Confirmed Deaths Plot
        output$Deaths <- renderPlot({
            ggplot(data = us_df_to_plot) + 
                line_geom(mapping = aes_string(x = "report_date", y = death_raw_or_percap_var, color = "Province_State")) + 
                smoothing_geom(mapping = aes_string(x = "report_date", y = death_raw_or_percap_var_MA, color = "Province_State"), lwd=1) +
                xlab("Reporting Date") +
                ylab(death_raw_or_percap_string) +
                ggtitle(paste(death_raw_or_percap_string, " by Reporting Date", sep="")) +
            labs(color = "State") + 
                theme_classic() +
                theme(text = element_text(size=14),
                      legend.title = element_text(size=18),
                      axis.text = element_text(size=14),
                      axis.title = element_text(size=16)) +         
                scale_colour_tableau('Tableau 20') + 
            y_axis_scaling            
        })

        # Case Fatality Rate Plot        
        output$ConditionedDeathRate <- renderPlot({
            ggplot(data = us_df_to_plot) + 
                line_geom(mapping = aes(x = report_date, y = Death_Rate, color = Province_State)) +
                smoothing_geom(mapping = aes(x = report_date, y = Death_Rate_MA, color = Province_State), lwd=1) + 
                xlab("Reporting Date") +
                ylab("Case Fatality Rate") +
                ggtitle("Case Fatality Rate (Deaths / Confirmed)") +
                labs(color = "State") + 
                theme_classic() +
                theme(text = element_text(size=14),
                      legend.title = element_text(size=18),
                      axis.text = element_text(size=14),
                      axis.title = element_text(size=16)) +         
                scale_colour_tableau('Tableau 20') + 
                scale_y_continuous(labels = percent)
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
#shinyApp(ui = ui, server = server)

