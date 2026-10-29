
# Project 2: Student Allowance and Spending Habits
# IMPORTANT: This dataset contains simulated student records.

# 1. Load required packages
packages <- c("dplyr", "ggplot2")

for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

library(dplyr)
library(ggplot2)

# 2. Load the dataset
data <- read.csv("student_allowance_spending.csv")

# 3. Check the dataset
print(dim(data))
print(head(data))
str(data)

# 4. Descriptive statistics
cat("\n--- DESCRIPTIVE STATISTICS ---\n")

summary_stats <- data %>%
  summarise(
    Number_of_Students = n(),
    Average_Weekly_Allowance =
      mean(Weekly_Allowance_PHP),
    Average_Weekly_Spending =
      mean(Total_Spending_PHP),
    Average_Weekly_Savings =
      mean(Weekly_Savings_PHP),
    Average_Savings_Rate =
      mean(Savings_Rate_Percent)
  )

print(summary_stats)

# 5. Average spending by category
spending_summary <- data.frame(
  Category = c(
    "Food",
    "Transportation",
    "School Supplies",
    "Leisure",
    "Other"
  ),
  Average_Spending = c(
    mean(data$Food_Spending_PHP),
    mean(data$Transportation_PHP),
    mean(data$School_Supplies_PHP),
    mean(data$Leisure_Spending_PHP),
    mean(data$Other_Spending_PHP)
  )
)

print(spending_summary)

# 6. Correlation between allowance and total spending
allowance_correlation <- cor(
  data$Weekly_Allowance_PHP,
  data$Total_Spending_PHP
)

cat("\nAllowance and spending correlation:\n")
print(allowance_correlation)

# 7. Average savings rate by budgeting habit
budget_summary <- data %>%
  group_by(Budgeting_Habit) %>%
  summarise(
    Students = n(),
    Average_Savings_Rate =
      mean(Savings_Rate_Percent),
    .groups = "drop"
  )

print(budget_summary)

# 8. Graph 1: Average spending by category
graph1 <- ggplot(
  spending_summary,
  aes(x = reorder(Category, Average_Spending),
      y = Average_Spending,
      fill = Category)
) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Average Weekly Spending by Category",
    x = "Spending Category",
    y = "Average Amount (PHP)"
  ) +
  theme_minimal()

print(graph1)

# 9. Graph 2: Weekly allowance vs. weekly spending
graph2 <- ggplot(
  data,
  aes(x = Weekly_Allowance_PHP,
      y = Total_Spending_PHP)
) +
  geom_point(size = 3, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE,
              color = "darkorange") +
  labs(
    title = "Weekly Allowance vs. Total Spending",
    x = "Weekly Allowance (PHP)",
    y = "Total Weekly Spending (PHP)"
  ) +
  theme_minimal()

print(graph2)

# 10. Graph 3: Savings rate by budgeting habit
graph3 <- ggplot(
  data,
  aes(x = Budgeting_Habit,
      y = Savings_Rate_Percent,
      fill = Budgeting_Habit)
) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    title = "Savings Rate by Budgeting Habit",
    x = "Budgeting Habit",
    y = "Savings Rate (%)"
  ) +
  theme_minimal()

print(graph3)

# 11. Save summary results
write.csv(
  summary_stats,
  "summary_statistics.csv",
  row.names = FALSE
)

write.csv(
  spending_summary,
  "spending_by_category.csv",
  row.names = FALSE
)

write.csv(
  budget_summary,
  "savings_by_budgeting_habit.csv",
  row.names = FALSE
)

cat("\nAnalysis complete. Remember: all records are simulated.\n")

