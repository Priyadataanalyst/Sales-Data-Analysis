# Title:Sales Data Analysis.
#1. Load and Clean the Data
getwd()
setwd("C:/Users/user/Downloads")
sales_data <- read.csv("sales_data.csv")
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# Display the first few rows of the dataset
head(sales_data)
# Check the structure of the dataset
str(sales_data)
#check the missing value.
anyNA(sales_data)
#comment: TRUE indicate there is missing value present in the dataset.

# Display the first few rows to the date format
head(sales_data$Order.Date, 10)
head(sales_data$Ship.Date, 10)
#2 Correct Date Conversion
# Convert date columns to Date type using the correct format
sales_data$Order.Date <- as.Date(sales_data$Order.Date, format = "%m/%d/%Y")
sales_data$Ship.Date <- as.Date(sales_data$Ship.Date, format = "%m/%d/%Y")

# Verify conversion
summary(sales_data$Order.Date)
summary(sales_data$Ship.Date)


#3 Clean the Data
# Remove unnecessary columns
cleaned_sales_data <- sales_data %>%
  select(-starts_with("X"))

# Handle missing values
cleaned_sales_data <- cleaned_sales_data %>% drop_na()

# Verify cleaned data
summary(cleaned_sales_data)
str(cleaned_sales_data)
colnames(cleaned_sales_data)

#4 Compute Total Sales
# Calculate total sales
total_sales <- sum(cleaned_sales_data$Sales)
# Display total sales
print(total_sales)
#comment: The total sales amount to $2,297,201. This figure represents the sum of all sales transactions in the dataset, providing a clear measure of the overall revenue generated.


#5 Analyze Sales Trends Over Time
# Aggregate sales by month
monthly_sales <- cleaned_sales_data %>%
  mutate(Month = floor_date(Order.Date, "month")) %>%
  group_by(Month) %>%
  summarize(Total_Sales = sum(Sales, na.rm = TRUE))

monthly_sales
#comment: Total sales by month wise.
# Month      Total_Sales
#<date>           <dbl>
# 1 2011-01-01      13946.
#2 2011-02-01       4811.
#3 2011-03-01      55691.
#4 2011-04-01      28295.
#5 2011-05-01      23648.
#6 2011-06-01      34595.
#7 2011-07-01      33946.
#8 2011-08-01      27909.
#9 2011-09-01      81777.
#10 2011-10-01      31453. and so on...

# Plot monthly sales trend
ggplot(monthly_sales, aes(x = Month, y = Total_Sales)) +
  geom_line() +
  labs(title = "Monthly Sales Trend", x = "Month", y = "Total Sales")
#comment: The  graph shows a possible increasing trend in sales over a five year period, from 2011 to 2015.
#The total sales appear to be much higher in 2015 than in 2011. This could be due to a number of factors, such as a growing customer base, an increase in marketing efforts, or the introduction of new products or services.

#6 Determine Best-Selling Products
# Calculate total sales per product
product_sales <- cleaned_sales_data %>%
  group_by(Product.Name) %>%
  summarize(Total_Sales = sum(Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales))
product_sales
#comment: The product of Canon imageCLASS 2200 Advanced Copier is total sales is 61600 & so on...

# Display top 10 best-selling products
top_products <- head(product_sales, 10)
top_products
#comment: total sale of Canon imageCLASS 2200 Advanced Copier is 6160. and so on.

# Plot top 10 best-selling products
ggplot(top_products, aes(x = reorder(Product.Name, -Total_Sales), y = Total_Sales)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Best-Selling Products", x = "Product", y = "Total Sales")
#comment: this plot give the top 10 product selling by the graph.

#7 Build Visualizations
# Plot total sales over time
ggplot(sales_data, aes(x = Order.Date, y = Sales)) +
  geom_line() +
  labs(title = "Total Sales Over Time", x = "Date", y = "Sales")
#comment: The graph is a line graph showing total sales over time. The x-axis represents the date, ranging from 2011 to 2015. The y-axis represents the total sales amount.

# Plot sales distribution by product category 
ggplot(sales_data, aes(x = Category, y = Sales)) +
  geom_boxplot() +
  labs(title = "Sales Distribution by Product Category", x = "Category", y = "Sales")
#comment: The graph shows the sales distribution of furniture, office supplies, and technology. 
#Furniture: This category has the highest sales, at around 20,000 units.
#Technology: This category has the second highest sales, at around 10,000 units.
#Office Supplies: This category has the lowest sales, at around 5,000 units.

#8 Additional Analysis and Visualizations
# Calculate total sales for each region
region_sales <- cleaned_sales_data %>%
  group_by(Region) %>%
  summarise(Total_Sales = sum(Sales)) %>%
  arrange(desc(Total_Sales))

region_sales
#comment: sales by region wise  
#1West        725458.
#2 East        678781.
#3 Central     501240.
#4 South       391722.
