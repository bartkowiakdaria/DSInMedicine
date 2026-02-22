library(shiny)
library(ggplot2)

titanic_df <- as.data.frame(Titanic)

ui <- fluidPage(
  titlePanel("Titanic Survival"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("class", "Class:", choices = levels(titanic_df$Class), selected = levels(titanic_df$Class)),
      checkboxGroupInput("sex", "Sex:", choices = levels(titanic_df$Sex), selected = levels(titanic_df$Sex)),
      checkboxGroupInput("age", "Age group:", choices = levels(titanic_df$Age), selected = levels(titanic_df$Age)),
      
      radioButtons("view", "View mode:", choices = c("Counts" = "count", "Survival rate" = "rate"), selected = "rate"),
      
      selectInput("group_by", "Group results by:", choices = c("Class", "Sex", "Age"), selected = "Class")
    ),
    
    mainPanel(plotOutput("plot", height = 380))
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$class, input$sex, input$age)
    titanic_df[titanic_df$Class %in% input$class &
        titanic_df$Sex %in% input$sex &
        titanic_df$Age %in% input$age, ]
  })
  
  plot_data <- reactive({
    d <- filtered_data()
    x <- input$group_by
    agg <- aggregate(Freq ~ d[[x]] + Survived, data = d, sum)
    names(agg)[1] <- "X"
    
    if (input$view == "count") {
      agg$value <- agg$Freq
      return(agg)
    }
    
    pos <- agg[agg$Survived == "Yes", c("X", "Freq")]
    neg <- agg[agg$Survived == "No",  c("X", "Freq")]
    names(pos)[2] <- "pos"
    names(neg)[2] <- "neg"
    m <- merge(pos, neg, by = "X", all = TRUE)
    m[is.na(m)] <- 0
    m$value <- m$pos / (m$pos + m$neg)
    m
  })
  
  output$plot <- renderPlot({
    pd <- plot_data()
    if (input$view == "count") {
      ggplot(pd, aes(x = X, y = value, fill = Survived)) +
        geom_col(position = "dodge") +
        labs(title = paste("Counts by", input$group_by, "and survival"), x = input$group_by, y = "Count") +
        theme_minimal()
    } else {
      ggplot(pd, aes(x = X, y = value)) +
        geom_col(fill = "steelblue") +
        scale_y_continuous(limits = c(0, 1)) +
        labs(title = paste("Survival rate by", input$group_by), x = input$group_by, y = "Survival rate") +
        theme_minimal()
    }
  })
}

shinyApp(ui, server)