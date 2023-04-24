## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

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

# Install and import Tidyverse.
install.packages('tidyverse')
library('tidyverse')

# Import the data set.
getwd()
setwd(dir='C:/Users/jencheng/Documents/LSE/LSE_DA301_assignment_files')
imported <- read.csv('turtle_sales.csv', header = TRUE)

# Print the data frame.
print(imported)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
games <- imported[,-3]

# View the data frame.
# Return the structure of the data frame.
str(games)


# Check the type of the data frame.
typeof(games)


# Check the class of the data frame.
class(games)


# Check the dimensions of the data frame
dim(games)

# View the descriptive statistics.
# View summary of games
summary(games)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
plot(x = games$Global_Sales, y = games$Ranking,
     xlab = "Global Sales",
     ylab = "Ranking",
     main = "Ranking vs Global Sales"
    )

## 2b) Histograms
# Create histograms.
plot(hist(games$Global_Sales),
     xlab = "Global Sales",
     main = "Histogram of Global Sales"
     )

## 2c) Boxplots
qplot(games$Global_Sales, games$Genre, data=games, 
      xlab = "Global Sales",
      ylab = "Genre", 
      main = "Genre vs Global Sales",
      geom='boxplot')

###############################################################################

# 3. Observations and insights

## Your observations and insights here ......




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

# View data frame created in Week 4.
print(games)

# Check output: Determine the min, max, and mean values.
# View the data set in a new window in RStudio as an Excel-style sheet.
View(games)


# View the dimensions of the data set i.e. the number of rows and columns.
dim(games)


# View the titles or names of the columns in the data set.
# There are two functions to return column names.
colnames(games)
names(games)


# Determine the structure of the data set.
str(games)

glimpse(games)

as_tibble(games)

# View the descriptive statistics.
# View summary of games
summary(games)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
df_Product = games %>% group_by(Product)  %>%
  summarise(Num_product = sum(Product))

# View the data frame.
View(df_Product)

# Explore the data frame.
# Check output: Determine the min, max, and mean values.
# View the dimensions of the data set i.e. the number of rows and columns.
dim(df_Product)


# View the titles or names of the columns in the data set.
# There are two functions to return column names.
colnames(df_Product)
names(df_Product)


# Determine the structure of the data set.
str(df_Product)

glimpse(df_Product)

as_tibble(df_Product)


## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
plot(x = games$Global_Sales, y = games$Product,
     xlab = "Global Sales",
     ylab = "Product",
     main = "Product vs Global Sales"
)

# Create histograms.
plot(hist(games$Product),
     xlab = "Product",
     main = "Histogram of Product"
)

# Create boxplots.
qplot(Global_Sales, Product, data=games, 
      xlab = "Global Sales",
      ylab = "Product", 
      main = "Product vs Global Sales",
      geom='boxplot')


###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(games$Global_Sales,
       col='blue',
       xlab="z Value",
       ylab='Global Sales')

qqline(games$Global_Sales,
       col='red',
       lwd=2) 


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages('moments')
library (moments)

# Perform Shapiro-Wilk test.
shapiro.test(games$Global_Sales)


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
# Specify the skewness and kurtosis functions.
skewness(games$Global_Sales) 
kurtosis(games$Global_Sales)


## 3d) Determine correlation
# Determine correlation.
t.test(games$Global_Sales,
       conf.level=0.68,
       mu=100)

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.
# Histogram:
hist(games$NA_Sales)

# Boxplot:
qplot(NA_Sales, Product, data=games, 
      xlab = "North America Sales",
      ylab = "Product", 
      main = "Product vs North America Sales",
      geom='boxplot')

# Histogram:
hist(games$EU_Sales)

# Boxplot:
qplot(EU_Sales, Product, data=games, 
      xlab = "Eurpoe Sales",
      ylab = "Product", 
      main = "Product vs Eurpoe Sales",
      geom='boxplot')

?ggplot()
?aes()

ggplot (data = games, 
        # Add mapping elements.
        # Insert a + to add the geom.
        mapping=aes(x = Global_Sales, y = Ranking)) +
  
  # Add a geom as point for scatterplot.
  # Set the colour to red.
  geom_point(color = 'red',
             # Set the alpha transparency to 0.5.
             alpha = 0.5,  
             # Set the point size to 1.5.
             size = 1.5) 
###############################################################################

# 5. Observations and insights
# Your observations and insights here...



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

# 1. Load and explore the data
# View data frame created in Week 5.
sales <- games[,-1:-5]

print(sales)

# Check output: Determine the min, max, and mean values.
# View the data set in a new window in RStudio as an Excel-style sheet.
View(sales)


# View the dimensions of the data set i.e. the number of rows and columns.
dim(sales)


# View the titles or names of the columns in the data set.
# There are two functions to return column names.
colnames(sales)
names(sales)


# Determine the structure of the data set.
str(sales)

glimpse(sales)

as_tibble(sales)


# Determine a summary of the data frame.
# View summary of games
summary(sales)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
# Identify relationships between the two variables - year and index.

# Find a correlation.
cor(sales)

# Plot the relationship with base R graphics.
plot(games$Year, games$Index)


## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(games$Year, games$Index)

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.


# Multiple linear regression model.
# Create a model with only one x variable.
model1 <- lm(Ranking~Year,
             data=games)


# View the model.
model1


# View more outputs for the model - the full regression table.
summary(model1)

# View residuals on a plot.
plot(model1$residuals)


# Plot the relationship with base R graphics.
plot(games$Year, games$Ranking)
coefficients(model1)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.
#Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.


###############################################################################

# 4b. Creat K-mean clustering

# Install the factoextra package for k-means clustering. 
install.packages('factoextra')

# Import the necessary libraries.
# For k-means clustering and visualisation.
library(factoextra) 

# Create a scatterplot to view the data set.
# Specify x as sepal_length, y as sepal_width, and color as fruit_type.
ggplot(sales, aes(x=EU_Sales,
                  y=NA_Sales,
                  color=Global_Sales)) +
  geom_point()

###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################




