# Necessary libraries
library(shiny)
library(shinythemes)
library(readxl)
library(tidyverse)
library(plotly)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(pander)
library(shinyjs)

  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
      body {
        font-family: 'Arial', sans-serif;
        font-size: 16px;"))),
    theme = shinytheme("paper"),
    navbarPage(
      # Title tab
      "Multiplex Analysis",
      # Home tab
      tabPanel(
        "Home",
        HTML("Welcome to the Multiplex PCR Analysis App! The app's goal is to automate the process of graphing and analyzing data from a PCR and to predict targets for unknown smaples."),
        HTML("To begin, make sure your data looks similar as the following:"),
        tags$h4("The fluorescence data file should be structured as follows:"),
        img(src = "flu_image.png", width = "900px", height = "300px"),
        tags$br(),
        tags$br(),
        HTML("The \"Cycle\" column should list all cycle numbers, while each sample (e.g., A1) should have its own column populated with the corresponding RFU value for each cycle. Currently, the app only supports samples labeled A-Z, not extended labels like AB1. Ensure that the sample letters are capitalized, but the \"Cycle\" column is case-insensitive and can be labeled as \"cycle\"."),
        tags$br(),
        tags$br(),
        tags$h4("The plate layout data file should be structured as follows:"),
        img(src = "plate_image.png", width = "500px", height = "500px"),
        tags$br(),
        tags$br(),
        HTML("Column names do not need to be capitalized. Each row should represent the data for a single sample. If the target is unknown, label it as 'Unknown_Sample_X', where X is a number corresponding to the identity of the unknown sample. For example, if you have 3 unknown targets, and each target has 6 samples, label the samples as 'Unknown_Sample_1' for the first set of 6 samples, 'Unknown_Sample_2' for the second set of 6 samples, and 'Unknown_Sample_3' for the third set of 6 samples."),
        tags$br(),
        tags$br(),
        tags$a(href = "https://github.com/DerekDoelling/multiplex_analysis.git", 
               tags$img(src = "https://img.shields.io/badge/GitHub-Repo-blue?logo=github", 
                        height = "30px"), target = "_blank")
      ),
      tags$br(),
      tags$br(),
      # Import Data tab
      tabPanel(
        "Import Data",
        tags$h2("Upload your data files to begin", style = "text-align: center;"),
        tags$hr(style = "border-top: 3px solid lightblue; margin: 20px 0;"),
        fileInput("fluorescence_file", "Import Fluorescence File (.csv/.xlsx)", accept = c(".csv", ".xlsx", ".xls")),
        uiOutput("fluorescence_status"),
        tableOutput("fluorescence_table"),
        fileInput("plate_file", "Import Plate File (.csv/.xlsx)", accept = c("csv", ".xlsx", ".xls")),
        uiOutput("plate_status"),
        tableOutput("plate_table"),
        tags$br()
      ),
      # Graphing tab
      tabPanel(
        "Graphing",
        useShinyjs(),
        actionButton("toggle_sidebar", "Toggle Filter Options", icon = icon("bars")),  # Add a toggle button
        sidebarLayout(
          div(id = "sidebar",
            sidebarPanel(
              tags$h4("Filter Options", style = "text-align: center;"),
              pickerInput("filter_targets", "Filter Targets", 
                          choices = NULL, multiple = TRUE, 
                          options = list(`actions-box` = TRUE, 
                                         `selected-text-format` = "count > 1",
                                         `live-search` = TRUE)),
              
              pickerInput("filter_samples", "Filter Samples", 
                          choices = NULL, multiple = TRUE, 
                          options = list(`actions-box` = TRUE, 
                                         `selected-text-format` = "count > 1",
                                         `live-search` = TRUE)),
              
              pickerInput("filter_groups", "Filter Groups", 
                          choices = NULL, multiple = TRUE, 
                          options = list(`actions-box` = TRUE, 
                                         `selected-text-format` = "count > 1",
                                         `live-search` = TRUE)),
              
              pickerInput("filter_target_concentration", "Filter Target Concentration", 
                          choices = NULL, multiple = TRUE, 
                          options = list(`actions-box` = TRUE, 
                                         `selected-text-format` = "count > 1",
                                         `live-search` = TRUE)),
              
              pickerInput("filter_probe_concentration", "Filter Probe Concentration", 
                          choices = NULL, multiple = TRUE, 
                          options = list(`actions-box` = TRUE, 
                                         `selected-text-format` = "count > 1",
                                         `live-search` = TRUE)),
              
              sliderInput("filter_cycles", "Filter Cycles", 
                          min = 1, max = 100,  # Default values; will be updated
                          value = c(1, 100)),
              actionButton("reset_filters", "Reset Filters", icon = icon("refresh")),
              width = 3
            )
          ),
          mainPanel(
            tags$h3("Generate Graphs"),
            tags$hr(style = "border-top: 3px solid lightblue; margin: 20px 0;"),
            actionButton("showbase_graph", "Show Base Graph"),
            actionButton("showtarget_graph", "Show Target Graph"),
            actionButton("showdistribution_graph", "Show Distribution Graph"),
            plotlyOutput("chosen_graph", width="110%", height="110%"),
            tags$br(),
            tags$br(),
            textOutput("plot_text"),
            #tags$br(),
            #downloadButton("download_filtered_plot", "Download Selected Plot"),
            tags$br()
          )
        )
      ),
        # Summary Table tab
        tabPanel(
          "Summary Table",
          tags$h2("Difference Summaries", style = "text-align: center;"),
          tags$hr(style = "border-top: 3px solid lightblue; margin: 20px 0;"), 
          tags$br(),
          tags$h3("Base Level is cycles 5-10, End Level is cycles 50-end", style = "text-align: center;"),
          tags$br(),
          tags$br(),
          DT::dataTableOutput("summary_table"),
          tags$br(),
          downloadButton("download_summary_table", "Download Summary Table"),
          tags$br(),
          tags$br()
        ),
      # Prediction tab
      tabPanel(
        "Predictions",
        tags$h3("Pattern of the original targets without the unknown samples", style = "text-align: center;"),
        tags$hr(style = "border-top: 3px solid lightblue; margin: 20px 0;"),
        plotlyOutput("real_target_graph"),
        tags$h3("The true bounds of all possible targets", style = "text-align: center;"),
        tags$hr(style = "border-top: 3px solid lightblue; margin: 20px 0;"),
        DT::dataTableOutput("real_target_table"),
        HTML("The true bounds were found by taking the last 20 cycles and finding the corresponding range of the RFU."),
        tags$h3("The predicted targets for each unknown sample", style = "text-align: center;"),
        tags$hr(style = "border-top: 3px solid lightblue; margin: 20px 0;"),
        DT::dataTableOutput("predictions_data_table"),
        HTML("The average RFU for each sample was found by taking the last 20 cycles and averaging the RFU."),
        tags$h3("Outliers present within the bounds of the assigned target", style = "text-align: center;"),
        tags$hr(style = "border-top: 3px solid lightblue; margin: 20px 0;"),
        DT::dataTableOutput("outliers_data_table"),
        HTML("The following samples were ignored during the prediction process."), 
        HTML("If the table is empty, then there are no outliers found."), 
        tags$br(),
        tags$br()
      ),
      )
    )
  
  server <- function(input, output, session) {
    # Store uploaded files as reactive values
    uploaded_files <- reactiveValues(fluorescence = NULL, plate = NULL)
    
    observeEvent(input$fluorescence_file, {
      req(input$fluorescence_file)
      
      read_file <- function(file_path, file_name) {
        # Get file extension from the original filename
        file_extension <- tools::file_ext(file_name)
        
        # Read the file based on its extension
        if (file_extension == "csv") {
          data <- read_csv(file_path)  # Reads CSV file
        } else if (file_extension %in% c("xls", "xlsx")) {
          data <- read_excel(file_path, sheet = 1)  # Reads first sheet by default
        } else {
          stop("Unsupported file format")  # Stops execution if format is not supported
        }
        
        return(data)
      }
      
      # Read file and store in reactiveValues
      uploaded_files$fluorescence <- read_file(input$fluorescence_file$datapath, 
                                               input$fluorescence_file$name)
    })
    
    observeEvent(input$plate_file, {
      req(input$plate_file)
      read_file <- function(file_path, file_name) {
        # Get file extension from the original filename
        file_extension <- tools::file_ext(file_name)
        
        # Read the file based on its extension
        if (file_extension == "csv") {
          data <- read_csv(file_path)
        } else if (file_extension %in% c("xls", "xlsx")) {
          data <- read_excel(file_path, sheet = "Data")  # Reads Data sheet
        } else {
          stop("Unsupported file format")  # Stops execution if format is not supported
        }
        
        return(data)
      }
      
      # Read file and store in reactiveValues
      uploaded_files$plate <- read_file(input$plate_file$datapath, 
                                               input$plate_file$name)

    })
    
    # Status bar
    output$fluorescence_status <- renderUI({
      if (!is.null(uploaded_files$fluorescence)) {
        div(style = "background-color: #ADD8E6; color: white; padding: 5px; text-align: center;",
            "Upload complete")
      }
    })
    
    output$plate_status <- renderUI({
      if (!is.null(uploaded_files$plate)) {
        div(style = "background-color: #ADD8E6; color: white; padding: 5px; text-align: center;",
            "Upload complete")
      }
    })
    
    # Render tables for uploaded files
    output$fluorescence_table <- renderTable({
      req(uploaded_files$fluorescence)
      head(uploaded_files$fluorescence, 5)
    })
    
    output$plate_table <- renderTable({
      req(uploaded_files$plate)
      head(uploaded_files$plate, 5)
    })
    
    # Create the main dataset
    pcr_data <- reactive({
      req(uploaded_files$fluorescence, uploaded_files$plate)
      
      colnames(uploaded_files$plate) <- tolower(colnames(uploaded_files$plate))
      flu_long <- uploaded_files$fluorescence %>%
        pivot_longer(cols = matches("\\d"), names_to = "sample", values_to = "rfu")
      
      colnames(flu_long) <- tolower(colnames(flu_long))
      
      pcr <- merge(flu_long, uploaded_files$plate, by = "sample")
      
      colnames(pcr) <- gsub("\\s+", "_", colnames(pcr))
      
      pcr$group <- gsub("[^A-Za-z]", "", pcr$sample)
      
      min_cycle <- min(pcr$cycle)
      max_cycle <- max(pcr$cycle)
      
      list(pcr = pcr, min_cycle = min_cycle, max_cycle = max_cycle)
    })
    
    # Toggle sidebar visibility
    observeEvent(input$toggle_sidebar, {
      toggle(id = "sidebar")
    })
    
    # Update filters
    observe({
      req(pcr_data())
      
      # Get the cycle range directly from pcr_data
      min_cycle <- pcr_data()$min_cycle
      max_cycle <- pcr_data()$max_cycle
      
      # Update selectInput choices
      updatePickerInput(session, "filter_targets", choices = unique(pcr_data()$pcr$target))
      updatePickerInput(session, "filter_samples", choices = unique(pcr_data()$pcr$sample))
      updatePickerInput(session, "filter_groups", choices = unique(pcr_data()$pcr$group))
      updatePickerInput(session, "filter_target_concentration", choices = unique(pcr_data()$pcr$target_concentration))
      updatePickerInput(session, "filter_probe_concentration", choices = unique(pcr_data()$pcr$probe_concentration))
      
      # Update the slider for cycles
      updateSliderInput(session, "filter_cycles", min = min_cycle, max = max_cycle, value = c(min_cycle, max_cycle))
    })
    
    # Create the filtered dataset based on user selections
    filtered_data <- reactive({
      pcr_data()$pcr %>%
        filter(
          (target %in% input$filter_targets | length(input$filter_targets) == 0),
          (sample %in% input$filter_samples | length(input$filter_samples) == 0),
          (group %in% input$filter_groups | length(input$filter_groups) == 0),
          (target_concentration %in% input$filter_target_concentration | length(input$filter_target_concentration) == 0),
          (probe_concentration %in% input$filter_probe_concentration | length(input$filter_probe_concentration) == 0),
          cycle >= input$filter_cycles[1] & cycle <= input$filter_cycles[2]
        )
    })
    
    # Sets the text as a reactive object
    text_output <- reactiveVal("")
    
    # Show text when distribution graph is clicked
    observeEvent(input$showdistribution_graph, {
      text_output("The red lines are error bars, which represent the standard deviation of RFU values. They are calculated by subtracting the standard deviation from the mean RFU for the lower bound, and adding the standard deviation to the mean RFU for the upper bound. These error bars show the range of variability around the mean RFU based on the filtered cycles.")
    })
    
    # Update text when base graph button is clicked
    observeEvent(input$showbase_graph, {
      text_output("")
    })
    
    # Update text when target graph button is clicked
    observeEvent(input$showtarget_graph, {
      text_output("")
    })
    
    # Render text based on graph selection
    output$plot_text <- renderText({
      text_output()
    })
    
    # Base graph
    observeEvent(input$showbase_graph, {
      output$chosen_graph  <- renderPlotly({
        req(filtered_data())
        
        plot_ly(filtered_data(), x = ~cycle, y = ~rfu, color = ~sample,
                text = ~paste("Sample: ", sample, "<br>",
                              "Group: ", group, "<br>",
                              "RFU: ", rfu, "<br>",
                              "Cycle: ", cycle, "<br>",
                              "Target: ", target),
                type = 'scatter', mode = 'lines', opacity = 0.1,
                hoverinfo = 'text') %>%
          layout(
            xaxis = list(title = "Cycle"),
            yaxis = list(title = "RFU"),
            legend = list(title = list(text = "Sample")),
            showlegend = TRUE
          )
        
        #b <- ggplot(filtered_data(), aes(x = cycle, y = rfu, color = sample,
         #                                text = paste("Sample: ", sample, "<br>",
          #                                            "Group: ", group, "<br>",
           #                                           "RFU: ", rfu, "<br>",
            #                                          "Cycle: ", cycle, "<br>",
             #                                         "Target: ", target))) +
        #  geom_line() +
         # labs(x="Cycle", y="RFU", color = "Sample") +
          #theme_minimal()
        #ggplotly(b, tooltip = "text")
      })
    })
    
    # Target graph
    observeEvent(input$showtarget_graph, {
      output$chosen_graph  <- renderPlotly({
        req(filtered_data())
        t <- ggplot(filtered_data(), aes(x = cycle, y = rfu, color = target,
                                         text = paste("Target: ", target, "<br>",
                                                      "Sample: ", sample, "<br>",
                                                      "Group: ", group, "<br>",
                                                      "RFU: ", rfu, "<br>",
                                                      "Cycle: ", cycle))) +
          geom_point() +
          labs(x="Cycle", y="RFU", color = "Target") +
          theme_minimal()
        ggplotly(t, tooltip = "text")
      })
    })
    
    # Distribution graph
    observeEvent(input$showdistribution_graph, {
      output$chosen_graph  <- renderPlotly({
        req(filtered_data())
        
        t <- ggplot(filtered_data(), aes(x = target, y = rfu)) +
          geom_boxplot() +
          stat_summary(fun.data = function(x) {
            data.frame(
              ymin = mean(x) - sd(x), 
              ymax = mean(x) + sd(x)
            )
          }, geom = "errorbar", width = 0.2, color = "red") +
          labs(x="Target", y="RFU") +
          theme_minimal()
        ggplotly(t, tooltip = "text")
      })
    })
    
    # Reset filters
    observeEvent(input$reset_filters, {
      req(pcr_data())
      
      min_cycle <- pcr_data()$min_cycle
      max_cycle <- pcr_data()$max_cycle
      
      updatePickerInput(session, "filter_targets", selected = character(0))
      updatePickerInput(session, "filter_samples", selected = character(0))
      updatePickerInput(session, "filter_groups", selected = character(0))
      updatePickerInput(session, "filter_target_concentration", selected = character(0))
      updatePickerInput(session, "filter_probe_concentration", selected = character(0))
      
      # Dynamically reset the slider range
      updateSliderInput(session, "filter_cycles", value = c(min_cycle, max_cycle))
    })
    
  #  output$download_filtered_plot <- downloadHandler(
   #   filename = function() {
    #    paste("custom_plot_", Sys.Date(), ".png", sep = "")
     # },
     # content = function(file) {
      #  # Export plotly graph to PNG
      #  plotly::save_image(output$chosen_graph, file = file, format="png")
    #  }
  #  )
    
    # Generate the dataframe for the summary table
    summary_data <- reactive({
      req(pcr_data())
      pcr <- pcr_data()$pcr
      
      summary_base <- pcr %>% 
        filter(cycle >= 5 & cycle <= 10) %>% 
        group_by(sample) %>% 
        mutate(
          base_level_rfu = mean(rfu, na.rm = TRUE))
      
      summary_end <- pcr %>% 
        filter(cycle >= max(pcr$cycle) - 10 & cycle <= max(pcr$cycle)) %>% 
        group_by(sample) %>% 
        mutate(
          end_level_rfu = mean(rfu, na.rm=TRUE))
      
      summary_data <- summary_base %>% 
        inner_join(summary_end, by = "sample") %>% 
        rename(target = target.x) %>% 
        mutate(
          difference = round(end_level_rfu - base_level_rfu, 3),
          base_level_rfu = round(base_level_rfu, 3),
          end_level_rfu = round(end_level_rfu, 3)) %>% 
        select(sample, target, base_level_rfu, end_level_rfu, difference) %>% 
        rename(Sample = sample, 
               Target = target,
               Base_Level_RFU = base_level_rfu,
               End_Level_RFU = end_level_rfu,
               Difference = difference) %>% 
        distinct() %>% 
        arrange(Sample)
      
      return(summary_data)
    })
    
    # Create the summary table
    output$summary_table <- DT::renderDataTable({
      datatable(
        summary_data(),
        filter = "top",  
        options = list(
          pageLength = 10,  
          lengthMenu = c(5, 10, 25, 50, 100),
          scrollX = TRUE 
        )
      )
    })
    
    # Download summary table
    output$download_summary_table <- downloadHandler(
      filename = function() {
        paste("summary_table_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(summary_data(), file, row.names = FALSE)
        
        showNotification("Download completed: Summary Table Saved!", type = "message", duration = 3)
      }
    )
    
    # Data wrangling to retrieve predictions
    prediction_data <- reactive({
      req(pcr_data())
      pcr <- pcr_data()$pcr
      
      real <- pcr %>%
       filter(!grepl("Unknown", target))
      
      
      real_rfu <- real %>% 
        group_by(target) %>% 
        filter(cycle >= max(pcr$cycle) - 20 & cycle <= max(pcr$cycle)) %>% 
        mutate(Q1 = quantile(rfu, 0.25),
               Q3 = quantile(rfu, 0.75),
               IQR = Q3 - Q1,
               lower_bound = Q1 - 1.5 * IQR,
               upper_bound = Q3 + 1.5 * IQR) %>% 
        distinct(target, lower_bound, upper_bound)
      
      unknown <- pcr %>% 
        filter(grepl("Unknown", target), 
               cycle >= max(pcr$cycle) - 20 & cycle <= max(pcr$cycle)) %>% 
        group_by(target, sample) %>% 
        summarise(mean_rfu = mean(rfu))
      
      # Detect outliers within the unknown targets
      unknown_outliers <- unknown %>% 
        group_by(target) %>% 
        summarise(
          mean_value = mean(mean_rfu),
          sd_value = sd(mean_rfu),
          iqr_value = IQR(mean_rfu),
          q1 = quantile(mean_rfu, 0.25),
          q3 = quantile(mean_rfu, 0.75)
        )
      
      unknown_outliers_determined <- unknown %>% 
        left_join(unknown_outliers, by="target") %>% 
        mutate(
          lower_bound = q1 - 1.5 * iqr_value,
          upper_bound = q3 + 1.5 * iqr_value,
          is_outlier = (mean_rfu < lower_bound) | (mean_rfu > upper_bound)
        )
      
      outliers <- unknown_outliers_determined %>% 
        dplyr::filter(is_outlier == TRUE)
      
      unknown_without_outliers <- unknown %>% 
        filter(!(sample %in% outliers$sample)) %>% 
        group_by(target) %>% 
        summarise(
          mean_rfu_target = mean(mean_rfu))
      
      outliers_table <- unknown_outliers_determined %>% 
        dplyr::filter(is_outlier == TRUE) %>% 
        rename(
          Avg_RFU_sample = mean_rfu, 
          Avg_RFU_target = mean_value, 
          Lower_Bound_target = lower_bound, 
          Upper_Bound_target = upper_bound) %>% 
        select(sample, target, Avg_RFU_sample, Avg_RFU_target, Lower_Bound_target, Upper_Bound_target)
      
      
      
      sum_all_pairs <- function(df) {
        new_rows <- list()
        for (i in 1:(nrow(df) - 1)) {
          for (j in (i + 1):nrow(df)) {
            lower_value <- df$lower_bound[i] + df$lower_bound[j]
            upper_value <- df$upper_bound[i] + df$upper_bound[j]
            
            new_row <- data.frame(
              target = paste(df$target[i], df$target[j], sep = "/"),
              lower_bound = lower_value,
              upper_bound = upper_value
            )
            new_rows[[length(new_rows) + 1]] <- new_row
          }
        }
        new_rows_df <- do.call(rbind, new_rows)
        df <- rbind(df, new_rows_df)
        return(df)
      } 
      
      new_targets_pairs <- sum_all_pairs(real_rfu)
      
      sum_all_triples <- function(df) {
        new_rows <- list()
        for (i in 1:(nrow(df) - 2)) {
          for (j in (i + 1):(nrow(df) - 1)) {
            for (k in (j + 1):nrow(df)) {
              lower_value <- df$lower_bound[i] + df$lower_bound[j] + df$lower_bound[k]
              upper_value <- df$upper_bound[i] + df$upper_bound[j] + df$upper_bound[k]
              
              new_row <- data.frame(
                target = paste(df$target[i], df$target[j], df$target[k], sep = "/"),
                lower_bound = lower_value,
                upper_bound = upper_value
              )
              new_rows[[length(new_rows) + 1]] <- new_row
            }
          }
        }
        new_rows_df <- do.call(rbind, new_rows)
        df <- rbind(df, new_rows_df)
        return(df)
      }
      
      new_targets_triples <- sum_all_triples(real_rfu)
      
      prediction_raw <- rbind(new_targets_pairs, new_targets_triples)
      new_targets <- unique(prediction_raw)
      
      new_targets <- new_targets %>% 
        filter(!grepl("Blank/", target))
      
      new_targets$rank <- rank(new_targets$lower_bound)
      new_targets$standard <- new_targets$rank - 1
      new_targets <- subset(new_targets, select = -rank)
      
      classify_sample <- function(rfu_value, new_targets) {
        for (i in 1:nrow(new_targets)) {
          if (rfu_value >= new_targets$lower_bound[i] && rfu_value <= new_targets$upper_bound[i]) {
            return(new_targets$target[i])
          }
        }
        return("Uncertain")
      }
      
      unknown_without_outliers$classification <- sapply(unknown_without_outliers$mean_rfu_target, classify_sample, new_targets = new_targets)
      
      predictions_data <- unknown_without_outliers %>% 
        mutate(
          predicted_standard = new_targets$standard[match(classification, new_targets$target)],
          mean_rfu = round(mean_rfu_target, 3)
        ) %>% 
        arrange(predicted_standard) %>%
        select(-mean_rfu_target) %>% 
        rename(
          Target = target,
          Avg.RFU_Target = mean_rfu,
          Predicted_Target = classification,
          Predicted_Standard = predicted_standard
        )
      
      target_data <- new_targets %>% 
        mutate(
          lower_bound = round(lower_bound, 3),
          upper_bound = round(upper_bound, 3)) %>% 
        arrange(standard) %>% 
        rename(
          Target = target,
          Lower_Bound = lower_bound,
          Upper_Bound = upper_bound,
          Standard = standard)
      
      return(list(predictions_data = predictions_data, target_data = target_data, real = real, outliers_table = outliers_table))
    })
    
    output$real_target_graph  <- renderPlotly({
      req(prediction_data())
      real <- prediction_data()$real
      
      plot_ly(real, x = ~cycle, y = ~rfu, color = ~target,
              text = ~paste("Sample: ", sample, "<br>",
                            "Group: ", group, "<br>",
                            "RFU: ", rfu, "<br>",
                            "Cycle: ", cycle, "<br>",
                            "Target: ", target),
              type = 'scatter', mode = 'lines', opacity = 0.4,
              hoverinfo = 'text') %>%
        layout(
          xaxis = list(title = "Cycle"),
          yaxis = list(title = "RFU"),
          legend = list(title = list(text = "Target")),
          showlegend = TRUE
        )
      
      #r <- ggplot(real, aes(x = cycle, y = rfu, color = target,
       #                     text = paste("Sample: ", sample, "<br>",
        #                                 "Group: ", group, "<br>",
         #                                "RFU: ", rfu, "<br>",
          #                               "Cycle: ", cycle, "<br>",
           #                              "Target: ", target))) +
      #  geom_line() +
       # labs(x="Cycle", y="RFU", color = "Target") +
        #theme_minimal()
      
    #  ggplotly(r, tooltip = "text")
    })
    
    # Show the target table
    output$real_target_table <- DT::renderDataTable({
      req(prediction_data())
      target_data <- prediction_data()$target_data
      datatable(
        target_data,
        filter = "top",  
        options = list(
          pageLength = 10,  
          lengthMenu = c(5, 10, 25, 50, 100),
          scrollX = TRUE 
        )
      )
    })
    
    # Show the predictions table
    output$predictions_data_table <- DT::renderDataTable({
      req(prediction_data())
      predictions_data <- prediction_data()$predictions_data
      datatable(
        predictions_data,
        filter = "top",  
        options = list(
          pageLength = 10,  
          lengthMenu = c(5, 10, 25, 50, 100),
          scrollX = TRUE 
        )
      )
    })
      
      # Show the outliers table
      output$outliers_data_table <- DT::renderDataTable({
        req(prediction_data())
        outliers_data <- prediction_data()$outliers_table
        datatable(
          outliers_data,
          filter = "top",  
          options = list(
            pageLength = 10,  
            lengthMenu = c(5, 10, 25, 50, 100),
            scrollX = TRUE 
          )
        )
    })
    
    
    
  }

  shinyApp(ui = ui, server = server)
