
library(shiny)
library(shinydashboard)
library(DT)
library(jsonlite)
library(mongolite)
library(dplyr)
library(ggplot2)
library(lubridate)
library(purrr)


# MongoDB connection

mongo_url <- "mongodb://localhost:27017"
db_name <- "genomics"
collection_name <- "provenance"

con <- mongo(
  collection = collection_name,
  db = db_name,
  url = mongo_url
)


# JSON import function

parse_json_file <- function(file) {
  raw <- fromJSON(file, simplifyVector = FALSE)
  
  generated <- raw$generated
  associated <- raw$wasAssociatedWith
  
  sha_value <- ""
  seqfu_value <- ""
  total_size <- NA
  file_count <- NA
  
  for (item in generated) {
    if (!is.null(item$label) && grepl("SHA256", item$label, ignore.case = TRUE)) {
      sha_value <- item$value
    }
    
    if (!is.null(item$label) && grepl("Seqfu", item$label, ignore.case = TRUE)) {
      seqfu_value <- item$value
    }
    
    if (!is.null(item$totalSizeBytes)) {
      total_size <- as.numeric(item$totalSizeBytes)
    }
    
    if (!is.null(item$fileCount)) {
      file_count <- as.numeric(item$fileCount)
    }
  }
  
  software <- paste(
    map_chr(associated, ~ ifelse(!is.null(.x$label), .x$label, NA_character_)),
    collapse = ", "
  )
  
  start_time <- ymd_hms(raw$startTime)
  end_time <- ymd_hms(raw$endTime)
  
  list(
    id = raw$`@id`,
    label = raw$label,
    startTime = raw$startTime,
    endTime = raw$endTime,
    executionNode = raw$executionNode,
    sourceDirectory = raw$sourceDirectory,
    destinationDirectory = raw$destinationDirectory,
    software = software,
    sha256_result = sha_value,
    seqfu_result = seqfu_value,
    sha256_ok = grepl("coincide|match|ok", sha_value, ignore.case = TRUE),
    seqfu_ok = grepl("OK", seqfu_value, ignore.case = TRUE),
    totalSizeBytes = total_size,
    totalSizeGB = total_size / 1e9,
    fileCount = file_count,
    durationSeconds = as.numeric(difftime(end_time, start_time, units = "secs")),
    raw_json = raw
  )
}


# Import JSON files 

import_json_folder <- function(folder_path) {
  files <- list.files(folder_path, pattern = "\\.json$", full.names = TRUE)
  
  if (length(files) == 0) {
    stop("No JSON files found in the selected folder.")
  }
  
  records <- map(files, parse_json_file)
  
  df <- bind_rows(records)
  
  con$drop()
  con$insert(df)
  
  length(records)
}


# UI

ui <- dashboardPage(
  dashboardHeader(title = "Genomic Provenance Monitor"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Logs", tabName = "logs", icon = icon("table")),
      menuItem("Full Provenance", tabName = "details", icon = icon("code"))
    ),
    
    hr(),
    
    textInput(
      "json_folder",
      "JSON folder path",
      value = "data/json_logs"
    ),
    
    actionButton("import_btn", "Import JSON to MongoDB"),
    
    hr(),
    
    selectInput("node_filter", "Execution node", choices = "All"),
    
    checkboxInput("failed_only", "Show failed checks only", FALSE)
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        
        fluidRow(
          valueBoxOutput("total_runs"),
          valueBoxOutput("failed_runs"),
          valueBoxOutput("total_gb")
        ),
        
        fluidRow(
          box(
            title = "System Health",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("health_plot")
          ),
          
          box(
            title = "Slowest Execution Nodes",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("slow_nodes_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Throughput Over Time",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("throughput_plot")
          )
        )
      ),
      
      tabItem(
        tabName = "logs",
        
        box(
          title = "Provenance Activity Logs",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          DTOutput("logs_table")
        )
      ),
      
      tabItem(
        tabName = "details",
        
        box(
          title = "Selected Full Provenance Record",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          verbatimTextOutput("json_details")
        )
      )
    )
  )
)


# Server

server <- function(input, output, session) {
  
  observeEvent(input$import_btn, {
    tryCatch({
      n <- import_json_folder(input$json_folder)
      
      showNotification(
        paste(n, "JSON files imported into MongoDB."),
        type = "message"
      )
      
    }, error = function(e) {
      showNotification(
        paste("Import failed:", e$message),
        type = "error"
      )
    })
  })
  
  data <- reactive({
    df <- con$find('{}')
    
    if (nrow(df) == 0) {
      return(data.frame())
    }
    
    df
  })
  
  observe({
    df <- data()
    
    if (nrow(df) > 0 && "executionNode" %in% names(df)) {
      nodes <- sort(unique(df$executionNode))
      updateSelectInput(
        session,
        "node_filter",
        choices = c("All", nodes)
      )
    }
  })
  
  filtered_data <- reactive({
    df <- data()
    
    if (nrow(df) == 0) {
      return(df)
    }
    
    if (input$node_filter != "All") {
      df <- df %>% filter(executionNode == input$node_filter)
    }
    
    if (input$failed_only) {
      df <- df %>% filter(!sha256_ok | !seqfu_ok)
    }
    
    df
  })
  
  output$total_runs <- renderValueBox({
    df <- filtered_data()
    
    valueBox(
      nrow(df),
      "Total Runs",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$failed_runs <- renderValueBox({
    df <- filtered_data()
    
    failed <- if (nrow(df) == 0) {
      0
    } else {
      sum(!df$sha256_ok | !df$seqfu_ok, na.rm = TRUE)
    }
    
    valueBox(
      failed,
      "Failed Integrity Checks",
      icon = icon("triangle-exclamation"),
      color = ifelse(failed > 0, "red", "green")
    )
  })
  
  output$total_gb <- renderValueBox({
    df <- filtered_data()
    
    total <- if (nrow(df) == 0) {
      0
    } else {
      round(sum(df$totalSizeGB, na.rm = TRUE), 2)
    }
    
    valueBox(
      paste(total, "GB"),
      "Total Data Moved",
      icon = icon("hard-drive"),
      color = "purple"
    )
  })
  
  output$health_plot <- renderPlot({
    df <- filtered_data()
    
    req(nrow(df) > 0)
    
    df %>%
      mutate(
        status = ifelse(sha256_ok & seqfu_ok, "OK", "FAIL")
      ) %>%
      count(status) %>%
      ggplot(aes(x = status, y = n, fill = status)) +
      geom_col() +
      labs(x = "Status", y = "Number of Runs") +
      theme_minimal()
  })
  
  output$slow_nodes_plot <- renderPlot({
    df <- filtered_data()
    
    req(nrow(df) > 0)
    
    df %>%
      group_by(executionNode) %>%
      summarise(
        avg_duration = mean(durationSeconds, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_duration)) %>%
      ggplot(aes(x = reorder(executionNode, avg_duration), y = avg_duration)) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Execution Node",
        y = "Average Duration Seconds"
      ) +
      theme_minimal()
  })
  
  output$throughput_plot <- renderPlot({
    df <- filtered_data()
    
    req(nrow(df) > 0)
    
    df %>%
      mutate(date = as.Date(startTime)) %>%
      group_by(date) %>%
      summarise(total_gb = sum(totalSizeGB, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = date, y = total_gb)) +
      geom_line() +
      geom_point() +
      labs(
        x = "Date",
        y = "GB Moved"
      ) +
      theme_minimal()
  })
  
  output$logs_table <- renderDT({
    df <- filtered_data()
    
    req(nrow(df) > 0)
    
    df %>%
      select(
        label,
        executionNode,
        startTime,
        endTime,
        durationSeconds,
        sha256_ok,
        seqfu_ok,
        totalSizeGB,
        fileCount,
        software
      ) %>%
      datatable(
        selection = "single",
        options = list(pageLength = 10, scrollX = TRUE)
      )
  })
  
  output$json_details <- renderPrint({
    df <- filtered_data()
    selected <- input$logs_table_rows_selected
    
    if (is.null(selected) || length(selected) == 0) {
      cat("Click a row in the Logs table to view the full provenance record.")
      return()
    }
    
    record <- df[selected, ]
    
    cat(toJSON(record, pretty = TRUE, auto_unbox = TRUE))
  })
}


shinyApp(ui, server)