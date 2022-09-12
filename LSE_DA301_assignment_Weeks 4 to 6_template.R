## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################
 

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
install.packages('tidyverse')
# Install and import Tidyverse.
library(tidyverse)

# Import the data set.
turtle_sales <- read.csv('C:/Users/carsza/Desktop/LSE Files/LSE_DA301_assignment_files/turtle_sales.csv')

# Print the data frame.
head(turtle_sales)
view(turtle_sales)

# Explore the data set 
dim(turtle_sales)
glimpse(turtle_sales)
summary(turtle_sales)
as_tibble(turtle_sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
colnames(turtle_sales)
turtle_sales2 <- select(df,
              -Ranking,
              -Year,
              -Genre,
              -Publisher)

# Check for blanks in new dataframe
sum(is.na(turtle_sales2))

# View the data frame.
head(turtle_sales2)
View(turtle_sales2)

# View the descriptive statistics.
summary(turtle_sales2)
as_tibble(turtle_sales2)

################################################################################

# 2. Review plots to determine insights into the data set.
plot(turtle_sales2)
## 2a) Scatterplots
# Create scatterplots.
# Global Sales
qplot(data = turtle_sales2, Global_Sales, Product)
qplot(data = turtle_sales2,Global_Sales, Platform)
qplot(data = turtle_sales2, Global_Sales,Platform, colour=Product)

######### EU Sales
qplot(data = turtle_sales2, EU_Sales, Product)
qplot(data = turtle_sales2, EU_Sales, Platform)
qplot(data = turtle_sales2, EU_Sales, Platform, colour=Product)

######### North American Sales
qplot(data = turtle_sales2, NA_Sales, Product)
qplot(data = turtle_sales2, NA_Sales, Platform)
qplot(data = turtle_sales2, NA_Sales, Platform, colour=Product)

## Global vs NA vs EU Sales
qplot(data = turtle_sales2, NA_Sales, Global_Sales)
qplot(data = turtle_sales2, EU_Sales, Global_Sales)
qplot(data = turtle_sales2, EU_Sales, NA_Sales, colour=Global_Sales)
qplot(data = turtle_sales2, EU_Sales, NA_Sales, colour=Platform)

## 2b) Histograms
## Create histograms.
## Global Sales
qplot(data = turtle_sales2, Global_Sales, bins=20)

## EU Sales
qplot(data = turtle_sales2, EU_Sales, bins=20)

## NA Sales
qplot(data = turtle_sales2, NA_Sales, bins=20)


## 2c) Boxplots
# Create boxplots.
qplot(data = turtle_sales2, 
      Platform,
      Global_Sales,
      geom = 'boxplot',
      ylab = 'Global Sales'
      )

## EU Sales
qplot(data = turtle_sales2, 
      Platform,
      EU_Sales,
      geom = 'boxplot',
      ylab = 'EU Sales'
      )

##NA Sales
qplot(data = turtle_sales2, 
      Platform,
      NA_Sales,
      geom = 'boxplot',
      ylab = 'NA Sales'
      )

###############################################################################

# 3. Observations and insights

### Data is not normally distributed, heavy right skew with most of sales
### being concentrated on the left hand side of the graph (lower priced)
### As it is sales data this seems logical
### The correlation between the NA and global appears strongest
### EU and NA seems to have the most dispersion



###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

head(turtle_sales2)
view(turtle_sales2)

# View data frame created in Week 4.
View(turtle_sales2)

# Check output: Determine the min, max, and mean values.
mean(turtle_sales2$Global_Sales)
mean(turtle_sales2$EU_Sales)
mean(turtle_sales2$NA_Sales)

max(turtle_sales2$Global_Sales)
max(turtle_sales2$EU_Sales)
max(turtle_sales2$NA_Sales)

min(turtle_sales2$Global_Sales)
min(turtle_sales2$EU_Sales)
min(turtle_sales2$NA_Sales)

# View the descriptive statistics.

summary(turtle_sales2)
as_tibble(turtle_sales2)


###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sales_by_product <- turtle_sales2 %>% group_by(Product) %>%
  summarise(NA_sales_sum=sum(NA_Sales),
            EU_sales_sum=sum(EU_Sales),
            Global_sales_sum=sum(Global_Sales), 
            .groups = 'drop')

# View the data frame.
view(sales_by_product)
head(sales_by_product)


# Explore the data frame.
summary(sales_by_product)
as_tibble(sales_by_product)


## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
ggplot(sales_by_product, aes(x= NA_sales_sum, y= EU_sales_sum)) + geom_point()
ggplot(sales_by_product, aes(x= EU_sales_sum, y= Global_sales_sum)) + geom_point()
ggplot(sales_by_product, aes(x= Global_sales_sum, y= NA_sales_sum)) + geom_point()

# Create histograms.
ggplot(sales_by_product, aes(x= NA_sales_sum)) + geom_histogram()
ggplot(sales_by_product, aes(x= EU_sales_sum)) + geom_histogram()
ggplot(sales_by_product, aes(x= Global_sales_sum)) + geom_histogram()

# Create boxplots.
ggplot(sales_by_product, aes(x= NA_sales_sum)) + geom_boxplot()
ggplot(sales_by_product, aes(x= EU_sales_sum)) + geom_boxplot()
ggplot(sales_by_product, aes(x= Global_sales_sum)) + geom_boxplot()


###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.

##North American Sales
qqnorm(sales_by_product$NA_sales_sum)
qqline(sales_by_product$NA_sales_sum)

##EU Sales
qqnorm(sales_by_product$EU_sales_sum)
qqline(sales_by_product$EU_sales_sum)

##Global Sales
qqnorm(sales_by_product$Global_sales_sum)
qqline(sales_by_product$Global_sales_sum)

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages('moments')
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test((sales_by_product$NA_sales_sum))
shapiro.test((sales_by_product$EU_sales_sum))
shapiro.test((sales_by_product$Global_sales_sum))
### p-values are all very low, therefore we can reject
### the null hypothesis that the data is normally distributed

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

### North America
skewness(sales_by_product$NA_sales_sum)
kurtosis(sales_by_product$NA_sales_sum)

### EU
skewness(sales_by_product$EU_sales_sum)
kurtosis(sales_by_product$EU_sales_sum)

### Global
skewness(sales_by_product$Global_sales_sum)
kurtosis(sales_by_product$Global_sales_sum)

## 3d) Determine correlation
# Determine correlation.
cor(sales_by_product$NA_sales_sum, sales_by_product$EU_sales_sum)
cor(sales_by_product$NA_sales_sum, sales_by_product$Global_sales_sum)
cor(sales_by_product$Global_sales_sum, sales_by_product$EU_sales_sum)

### Suggests strong positive correlation between sales in different regions
### And also to Global Sales

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Chose to use plots that explore the correlation between
# Sales in different regions a little more
# Correlation between NA and Global sales is especially high (0.92)
# Correlation between EU and Global is also high (0.84)

### NA vs Global
ggplot(data=sales_by_product, 
       mapping = aes(x=NA_sales_sum, y=Global_sales_sum)) +
  geom_point(colour = 'blue',
             alpha = 1,
             size = 3) +
  labs(title = 'Global Sales vs North American Sales', 
       x = 'North American Sales',
       y = 'Global Sales') +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm")

### Eu vs Global
ggplot(data=sales_by_product, 
       mapping = aes(x=EU_sales_sum, y=Global_sales_sum)) +
  geom_point(colour = 'blue',
             alpha = 1,
             size = 3) +
  labs(title = 'Global Sales vs Europe Sales', 
       x = 'Europe Sales',
       y = 'Global Sales') +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm")


###############################################################################

# 5. Observations and insights
# Correlations between EU and Global, and NA and Global sales is high
# This is confirmed with our additional tests performed this week
# Shapiro test confirms non-normal distribution
# Further correlation analysis shows that EU, NA and Global sales
# Are all farily highly correlated


###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
View(sales_by_product)
as_tibble(sales_by_product)
head(sales_by_product)

# Determine a summary of the data frame.
summary(sales_by_product)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
cor(sales_by_product)


## 2b) Create a plot (simple linear regression)
# Basic visualisation.
model_eu_glo <- lm(Global_sales_sum~EU_sales_sum, data=sales_by_product)

# View the new model
View(model_eu_glo)

# plot to explore the model
plot(model_eu_glo$residuals)
plot(sales_by_product$EU_sales_sum, sales_by_product$Global_sales_sum)
abline(coefficients(model_eu_glo))

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
turtle_sales_3 <- select(turtle_sales2, -Product, -Platform)
View(turtle_sales_3)

# Multiple linear regression model.
sales_mult_model <- lm(Global_Sales~EU_Sales + NA_Sales, data = turtle_sales_3)
summary(sales_mult_model) 
# High adj r^2 of 0.97

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.
##### Using the first 5 observations from head(sales_by_product)
# Example 1
Product <- 107
NA_Sales <- c(34.02)
EU_Sales <- c(23.80)
predict_data <- data.frame(Product, NA_Sales, EU_Sales)

## Predict
predict(sales_mult_model, newdata = predict_data)
# Prediction of 71 vs actual of 68

# Example 2 
Product <- 123
NA_Sales <- c(23.85)
EU_Sales <- c(2.94)
predict_data <- data.frame(Product, NA_Sales, EU_Sales)

## Predict 2
predict(sales_mult_model, newdata = predict_data)
## Prediction of 31.7 vs actual of 33 

# Example 3 
Product <- 195
NA_Sales <- c(13)
EU_Sales <- c(10.6)
predict_data <- data.frame(Product, NA_Sales, EU_Sales)

## Predict 3
predict(sales_mult_model, newdata = predict_data)
## Prediction of 29.4 vs actual of 29.4 

# Example 4 
Product <- 231
NA_Sales <- c(12.9)
EU_Sales <- c(9.03)
predict_data <- data.frame(Product, NA_Sales, EU_Sales)

## Predict 4
predict(sales_mult_model, newdata = predict_data)
## Prediction of 27.2 vs actual of 27.1 

# Example 5 
Product <- 249
NA_Sales <- c(9.24)
EU_Sales <- c(7.29)
predict_data <- data.frame(Product, NA_Sales, EU_Sales)

## Predict 5
predict(sales_mult_model, newdata = predict_data)
## Prediction of 20.7 vs actual of 25.7 



###############################################################################

# 5. Observations and insights
# Your observations and insights here...
# High R2 for both the single variate and multi-variate models
# Using the model to predict values seems to product acceptable results
# It is likely that the team at Turtle Games could use the model for 
# Sales forecasting their existing product sales into the future
# A next step might be to a time variable to this anlaysis to understand
# How revenue growth for the business has evolved over time


###############################################################################
###############################################################################




