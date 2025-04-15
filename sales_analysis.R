# Install required libraries if you haven't already
install.packages(c("dplyr", "lubridate", "ggplot2"))

# Load libraries
library(dplyr)
library(lubridate)
library(ggplot2)

# Set random seed for reproducibility
set.seed(123)

# Create a vector of brands and regions
brands <- c("LV", "Gucci", "Dior", "Prada", "Fendi")
regions <- c("North America", "Europe", "Asia", "Middle East")

# Create a date sequence from 2018 to 2023
dates <- seq(from = as.Date("2018-01-01"), to = as.Date("2023-12-31"), by = "day")

# Simulate the data
sales_data <- data.frame(
  Date = rep(dates, each = length(brands) * length(regions)),
  Brand = rep(brands, times = length(dates) * length(regions)),
  Region = rep(regions, each = length(dates) * length(brands)),
  Sales_Amount = rnorm(length(dates) * length(brands) * length(regions), mean = 1000, sd = 500),
  Units_Sold = rnorm(length(dates) * length(brands) * length(regions), mean = 5, sd = 2)
)

# Assign "COVID_Period" column (before or after COVID)
sales_data$COVID_Period <- ifelse(sales_data$Date < as.Date("2020-03-01"), "Before COVID", "After COVID")

# Save the sales data as a CSV file
write.csv(sales_data, "sales_data.csv", row.names = FALSE)

# Check for missing values and handle them
sales_data$Sales_Amount <- ifelse(is.na(sales_data$Sales_Amount), 
                                   mean(sales_data$Sales_Amount, na.rm = TRUE), 
                                   sales_data$Sales_Amount)

sales_data$Units_Sold <- ifelse(is.na(sales_data$Units_Sold), 
                                mean(sales_data$Units_Sold, na.rm = TRUE), 
                                sales_data$Units_Sold)

# Ensure the 'Date' column is in Date format
sales_data$Date <- as.Date(sales_data$Date)

# Exploratory Data Analysis (EDA)

# 1. Total sales before and after COVID for each brand
sales_summary <- sales_data %>%
  group_by(COVID_Period, Brand) %>%
  summarize(Total_Sales = sum(Sales_Amount), Total_Units_Sold = sum(Units_Sold))

# View the summary
print(sales_summary)

# 2. Sales trend over time (Before vs After COVID)
ggplot(sales_data, aes(x = Date, y = Sales_Amount, color = COVID_Period)) +
  geom_line() +
  labs(title = "Sales Trend Before and After COVID-19", 
       x = "Date", y = "Sales Amount") +
  facet_wrap(~ Brand)

# Save the plot as a PNG
ggsave("sales_trend.png", plot = last_plot(), width = 8, height = 6)

# 3. Brand comparison of sales before and after COVID
ggplot(sales_summary, aes(x = Brand, y = Total_Sales, fill = COVID_Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Sales Comparison Before and After COVID", 
       x = "Brand", y = "Total Sales") +
  theme_minimal()

# Save the plot as a PNG
ggsave("brand_comparison.png", plot = last_plot(), width = 8, height = 6)
