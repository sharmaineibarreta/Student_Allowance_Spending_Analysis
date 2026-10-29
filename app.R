
# Project 2: Student Allowance and Spending Habits
# All student records are simulated for educational use.

# 1. Install packages if needed
packages <- c("shiny", "dplyr", "ggplot2", "DT")

for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# 2. Load the dataset
data <- read.csv("student_allowance_spending.csv")

# 3. Define the dashboard interface
ui <- fluidPage(
  
  titlePanel("Student Allowance and Spending Habits"),
  
  tags$div(
    style = paste(
      "background-color: #fff3cd;",
      "padding: 12px;",
      "border-radius: 8px;",
      "margin-bottom: 15px;"
    ),
    strong("Important: "),
    "This dashboard uses 50 simulated student records.",
    " Results are for educational demonstration only."
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Dashboard Filters"),
      
      sliderInput(
        "allowance",
        "Minimum Weekly Allowance (PHP)",
        min = min(data$Weekly_Allowance_PHP),
        max = max(data$Weekly_Allowance_PHP),
        value = min(data$Weekly_Allowance_PHP),
        step = 50
      ),
      
      sliderInput(
        "savings",
        "Minimum Weekly Savings (PHP)",
        min = min(data$Weekly_Savings_PHP),
        max = max(data$Weekly_Savings_PHP),
        value = min(data$Weekly_Savings_PHP),
        step = 25
      ),
      
      selectInput(
        "budgeting",
        "Budgeting Habit",
        choices = c(
          "All" = "All",
          "Always" = "Always",
          "Sometimes" = "Sometimes",
          "Never" = "Never"
        ),
        selected = "All"
      ),
      
      checkboxInput(
        "show_trend",
        "Show trend line on allowance graph",
        value = TRUE
      )
    ),
    
    mainPanel(
      
      h3("Summary Statistics"),
      
      fluidRow(
        column(
          4,
          wellPanel(
            h4("Students"),
            textOutput("student_count")
          )
        ),
        
        column(
          4,
          wellPanel(
            h4("Average Allowance"),
            textOutput("average_allowance")
          )
        ),
        
        column(
          4,
          wellPanel(
            h4("Average Savings"),
            textOutput("average_savings")
          )
        )
      ),
      
      tabsetPanel(
        
        tabPanel(
          "Student Data",
          br(),
          DTOutput("student_table")
        ),
        
        tabPanel(
          "Spending Categories",
          br(),
          plotOutput("spending_plot")
        ),
        
        tabPanel(
          "Allowance vs Spending",
          br(),
          plotOutput("allowance_plot")
        ),
        
        tabPanel(
          "Savings by Budgeting",
          br(),
          plotOutput("savings_plot")
        )
      )
    )
  )
)

# 4. Define dashboard calculations and outputs
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
    result <- data %>%
      filter(
        Weekly_Allowance_PHP >= input$allowance,
        Weekly_Savings_PHP >= input$savings
      )
    
    if (input$budgeting != "All") {
      result <- result %>%
        filter(Budgeting_Habit == input$budgeting)
    }
    
    result
  })
  
  # Summary cards
  output$student_count <- renderText({
    nrow(filtered_data())
  })
  
  output$average_allowance <- renderText({
    result <- filtered_data()
    
    if (nrow(result) == 0) {
      return("No data")
    }
    
    paste0(
      "PHP ",
      format(
        round(mean(result$Weekly_Allowance_PHP), 2),
        nsmall = 2,
        big.mark = ","
      )
    )
  })
  
  output$average_savings <- renderText({
    result <- filtered_data()
    
    if (nrow(result) == 0) {
      return("No data")
    }
    
    paste0(
      "PHP ",
      format(
        round(mean(result$Weekly_Savings_PHP), 2),
        nsmall = 2,
        big.mark = ","
      )
    )
  })
  
  # Student data table
  output$student_table <- renderDT({
    datatable(
      filtered_data(),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  # Average spending by category
  output$spending_plot <- renderPlot({
    
    result <- filtered_data()
    
    validate(
      need(nrow(result) > 0, "No students match these filters.")
    )
    
    spending_summary <- data.frame(
      Category = c(
        "Food",
        "Transportation",
        "School Supplies",
        "Leisure",
        "Other"
      ),
      Average_Spending = c(
        mean(result$Food_Spending_PHP),
        mean(result$Transportation_PHP),
        mean(result$School_Supplies_PHP),
        mean(result$Leisure_Spending_PHP),
        mean(result$Other_Spending_PHP)
      )
    )
    
    ggplot(
      spending_summary,
      aes(
        x = reorder(Category, Average_Spending),
        y = Average_Spending,
        fill = Category
      )
    ) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(
        title = "Average Weekly Spending by Category",
        x = "Category",
        y = "Average Amount (PHP)"
      ) +
      theme_minimal(base_size = 13)
  })
  
  # Allowance versus spending
  output$allowance_plot <- renderPlot({
    
    result <- filtered_data()
    
    validate(
      need(nrow(result) > 0, "No students match these filters.")
    )
    
    p <- ggplot(
      result,
      aes(
        x = Weekly_Allowance_PHP,
        y = Total_Spending_PHP
      )
    ) +
      geom_point(size = 3, color = "#168AAD") +
      labs(
        title = "Weekly Allowance vs Total Spending",
        x = "Weekly Allowance (PHP)",
        y = "Total Weekly Spending (PHP)"
      ) +
      theme_minimal(base_size = 13)
    
    if (input$show_trend && nrow(result) >= 2) {
      p <- p +
        geom_smooth(
          method = "lm",
          se = FALSE,
          color = "#F26430"
        )
    }
    
    p
  })
  
  # Savings rate by budgeting habit
  output$savings_plot <- renderPlot({
    
    result <- filtered_data()
    
    validate(
      need(nrow(result) > 0, "No students match these filters.")
    )
    
    ggplot(
      result,
      aes(
        x = Budgeting_Habit,
        y = Savings_Rate_Percent,
        fill = Budgeting_Habit
      )
    ) +
      geom_boxplot(show.legend = FALSE) +
      labs(
        title = "Savings Rate by Budgeting Habit",
        x = "Budgeting Habit",
        y = "Savings Rate (%)"
      ) +
      theme_minimal(base_size = 13)
  })
}

# 5. Run the Shiny application
shinyApp(ui = ui, server = server)
