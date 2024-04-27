# Load libraries
library(readxl)
library(dplyr)
library(caret)
library(ggplot2)
library(leaps)
library(ggcorrplot)
library(car)
library(gridExtra)
library(performance)
library(magrittr)
library(corrplot)
library(dummy)


# DATA --------------------------------------------------------------------

# Load Dataset
car_data <- read_excel("C:/Users/girli/OneDrive/Desktop/R_Exercises/R_Kunskapskontroll/Volvo_V60_Data_2018_2023/car_data.xlsx")
rmarkdown::paged_table(car_data)

glimpse(car_data)
summary(car_data)

# Encode categorical variables
car_data$Fuel <- as.factor(car_data$Fuel)
car_data$Gearbox <- as.factor(car_data$Gearbox)

dim(car_data)
str(car_data)
summary(car_data)

# EDA - Exploratory Data Analysis -----------------------------------------

# Check the distribution of the target variable: "Price"
hist(car_data$Price, main = "Distribution of Target Variable (PRICE)", xlab = "Target Variable (PRICE)", col = "chartreuse")

# Scatterplot matrix
pairs(Price ~ Model_Year + Mileage + Horsepower + Fuel + Gearbox, data = car_data)

# Create a scatter plot between "Horsepower" and "Price" by "Fuel" and "Gearbox"
ggplot(car_data, aes(x = Horsepower, y = Price, color = Fuel)) +
  geom_point(color = "darkred") +
  labs(title = "Scatter Plot: Car Price vs. Horsepower by Fuel Type", x = "Horsepower", y = "Car Price") +
  theme_minimal() +
  geom_smooth(method = lm)

ggplot(car_data, aes(x = Horsepower, y = Price, color = Gearbox)) +
  geom_point(color = "darkviolet") +
  labs(title = "Scatter Plot: Car Price vs. Horsepower by Gearbox Type", x = "Horsepower", y = "Car Price") +
  theme_minimal() +
  geom_smooth(method = lm)

# Create a scatter plot between "Mileage" and "Price" by "Fuel" and "Gearbox"
ggplot(car_data, aes(x = Mileage, y = Price, color = Fuel)) +
  geom_point(color = "darkmagenta") +
  labs(title = "Scatter Plot: Car Price vs. Mileage by Fuel Type", x = "Horsepower", y = "Car Price") +
  theme_minimal() +
  geom_smooth(method = lm)

ggplot(car_data, aes(x = Mileage, y = Price, color = Gearbox)) +
  geom_point(color = "darkgreen") +
  labs(title = "Scatter Plot: Car Price vs. Mileage by Gearbox Type", x = "Horsepower", y = "Car Price") +
  theme_minimal() +
  geom_smooth(method = lm)

# Create dummy variables for categorical columns
car_data_dummy <- car_data %>%
  mutate(
    FuelHybrid = as.numeric(Fuel == "Hybrid"),
    FuelGasoline = as.numeric(Fuel == "Gasoline"),
    FuelDiesel = as.numeric(Fuel == "Diesel"),
    GearboxAutomatic = as.numeric(Gearbox == "Automatic"),
    GearboxManual = as.numeric(Gearbox == "Manual")
  )

# Split data into training, validation, and test sets
set.seed(123)
train_index <- createDataPartition(car_data$Price, p = 0.7, list = FALSE)
validation_index <- createDataPartition(car_data[-train_index, ]$Price, p = 0.5, list = FALSE)

car_data_train <- car_data[train_index, ]
car_data_validation <- car_data[-train_index, ][validation_index, ]
car_data_test <- car_data[-train_index, ][-validation_index, ]

dim(car_data_train)
dim(car_data_validation)
dim(car_data_test)

#Check the distribution of target variable from Train Dataset
par(mfrow = c(1, 3))
hist(car_data_train$Price, main = "Distribution of Target Variable (PRICE) from Train Dataset", xlab = "Target Variable (PRICE) from Train Dataset", col = "darkred")
# Check the distribution of target variable from Validation Dataset
hist(car_data_validation$Price, main = "Distribution of Target Variable (PRICE) from Validation Dataset", xlab = "Target Variable (PRICE) from Validation Dataset", col = "darkblue")
# Check the distribution of target variable from Test Dataset
hist(car_data_test$Price, main = "Distribution of Target Variable (PRICE) from Test Dataset", xlab = "Target Variable (PRICE) from Test Dataset", col = "darkmagenta")

# Select only numeric and dummy variable columns
numeric_dummy_data <- select(car_data_dummy, -Fuel, -Gearbox)

# Create a correlation plot
ggcorrplot::ggcorrplot(cor(numeric_dummy_data), hc.order = TRUE, lab = TRUE)
 corr <- cor((numeric_dummy_data))
 corr

# MODELING: Train, Validate, Test -----------------------------------------

# Train: Multiple Linear Regression
# Model 1:
ml_model_0 <- lm(Price ~ 1, car_data_train) # Intercept, represents the mean value of mpg for all cars represented by the ones included in this data set.
summary(ml_model_0)

# Model 2:
ml_model_all <- lm(Price ~ ., data = car_data_train)
summary(ml_model_all)

# Feature Selection
# Backward Elimination
model_back <- step(ml_model_all,direction = "backward", trace = 0)
summary(model_back)

# Forward Selection
model_forward <- step(
  ml_model_0, direction = "forward", scope = list(lower = ml_model_0,
  upper = ml_model_all),trace = 0
)
summary(model_forward)

# Both models
model_both <- step(
  ml_model_0, direction = "both", scope = list(lower = ml_model_0, upper = ml_model_all),
  trace = 0
)
summary(model_both)

# Model Comparison (Problem01: Issue about compare_performance function)
compare_performance(ml_model_all,model_back,model_forward,model_both)


# Subset Selection & Diagnostic Analysis: Linear Regression Assumption --------
check_model(ml_model_all)

# Model Using Scaled Data
# Scaling data: Use the pipe operator
num_data <- car_data_train %>% 
  select(is.numeric) %>% 
  sapply(scale)

fac_data <- car_data_train %>% select(where(is.factor))

# Find the smaller number of rows between num_data and fac_data
min_rows <- min(nrow(num_data), nrow(fac_data))

# Combine the matrices
car_scale <- cbind(num_data, fac_data)
model_scale <- lm(Price ~ ., car_scale)
summary(model_scale)
check_model(model_scale)


# Model Validation
# Predictions on validation set
ml_pred_val <- predict(ml_model_all, newdata = car_data_validation)

# Calculate Mean Squared Error (MSE)
ml_rmse_val <- sqrt(mean((ml_pred_val - car_data_validation$Price)^2))
cat("Multiple Linear Regression RMSE on Validation Set:", ml_rmse_val, "\n")

#Visualize the chosen model
summary(ml_model_all)
avPlots(ml_model_all)


# Subset Selection and Diagnostic Analysis --------------------------------

# Perform subset selection using regsubsets
regfit <- regsubsets(Price ~ ., data = car_data_train, nvmax = 6)  # Choose the maximum number of predictors
# Summary of subset selection results
summary(regfit)

# Plot with heatmap
par(mfrow = c(2, 2))
plot(regfit, scale = "adjr2", main = "Subset Selection Heatmap")
# Residual plot to check for homoscedasticity
plot(ml_model_all, which = 1)
# Normal Q-Q plot to check for normality of residuals
plot(ml_model_all, which = 2)
# Cook's distance plot to check for influential points
cooksd <- cooks.distance(ml_model_all)
plot(cooksd, pch = 20, main = "Cook's Distance Plot")
abline(h = 4/length(ml_model_all$coefficients), col = "red")  # Adjust based on the model

# summary(cooksd)

# Other diagnostic plots
par(mfrow = c(2, 2))
plot(ml_model_all, which = 3)
plot(ml_model_all, which = 4)
plot(ml_model_all, which = 5)
plot(ml_model_all, which = 6)
acf(resid(ml_model_all))

threshold <- 0.05
high_cooks_indices <- which(cooksd > threshold)

# Check if there are any observations with high Cook's distances
if (length(high_cooks_indices) > 0) {
  # Subset data for observations with high Cook's distances
  high_cooks_data <- data.frame(Index = high_cooks_indices, cooksd = cooksd[high_cooks_indices])
  
  # Plot the relationship between index and Cook's distances
  ggplot(high_cooks_data, aes(x = Index, y = cooksd)) +
    geom_point() +
    labs(title = "Relationship between Index and Cook's Distances", x = "Index", y = "Cook's Distance")
} else {
  print("No observations with high Cook's distances found.")
}

# Calculate leverage values
leverage <- hatvalues(ml_model_all)

# Plot leverage values
plot(leverage, main = "Leverage Values", xlab = "Observation Index", ylab = "Leverage")

# Identify observations with high leverage
high_leverage_indices <- which(leverage > (2 * (ncol(car_data) - 1) / nrow(high_cooks_data)))

# Check if there are any observations with high leverage
if (length(high_leverage_indices) > 0) {
  print("Observations with high leverage:")
  print(high_leverage_indices)
} else {
  print("No observations with high leverage found.")
}

# Check for duplicates and sort
high_leverage_indices <- unique(sort(high_leverage_indices))

# Display high leverage observations
print(high_leverage_indices)

# Further investigate these observations (e.g., review data, calculate influence measures)
# For example, calculate Cook's distance
cooksd <- cooks.distance(ml_model_all)
cooksd_high_leverage <- cooksd[high_leverage_indices]

# Assess influence of high leverage points
influential_threshold <- 4 / nrow(car_data)
influential_indices <- which(cooksd_high_leverage > influential_threshold)

# Display influential observations
print(influential_indices)


# MODEL INFERENCE ---------------------------------------------------------

# Extract coefficients and perform hypothesis testing
coef_summary <- summary(ml_model_all)
cat("Coefficient Summary:\n")
print(coef_summary)

# Predict on the test dataset
test_predictions <- predict(ml_model_all, newdata = car_data_test)

# test_predictions
summary(test_predictions)


# Test the Chosen Model -----------------------------------------------------------------

# Evaluate test predictions
test_rmse <- sqrt(mean((test_predictions - car_data_test$Price)^2))
cat("Test RMSE for the chosen model:", test_rmse, "\n")

# Calculate confidence interval
conf_interval <- predict(ml_model_all, newdata = car_data_test, interval = "confidence", level = 0.95)
summary(conf_interval)

# Calculate prediction interval
pred_interval <- predict(ml_model_all, newdata = car_data_test, interval = "prediction", level = 0.95)
summary(pred_interval)

# Create data frames for intervals
conf_data <- data.frame(car_data_test, lower_bound = conf_interval[, "lwr"], upper_bound = conf_interval[, "upr"])
pred_data <- data.frame(car_data_test, lower_bound = pred_interval[, "lwr"], upper_bound = pred_interval[, "upr"])

summary(conf_data)
summary(pred_data)

# Plot the test data with confidence and prediction intervals
par(mfrow = c(1, 1))
plot(car_data_test$Price, type = "l", ylim = c(min(pred_interval), max(pred_interval)), xlab = "Observations", ylab = "Price")
lines(conf_data$lower_bound, col = "blue", lty = 2)
lines(conf_data$upper_bound, col = "blue", lty = 2)
lines(pred_data$lower_bound, col = "red", lty = 2)
lines(pred_data$upper_bound, col = "red", lty = 2)
legend("topleft", legend = c("Confidence Interval", "Prediction Interval"), col = c("blue", "red"), lty = 2)







# API: www.statistikdatabasen.scb.se  -------------------------------------

library(tidyverse)
library(httr)
library(dplyr)
library(caret)
library(ggplot2)
library(leaps)
library(ggcorrplot)
library(car)
library(gridExtra)
library(performance)
library(magrittr)
library(corrplot)
library(dummy)



# Define the API endpoint URL
api_url <- "https://www.statistikdatabasen.scb.se/sq/148752"

# Make a GET request to the API endpoint
response <- httr::GET(url = api_url)

# Check if the request was successful
if (response$status_code == 200) {
  # Parse the JSON data from the API response
  api_data <- httr::content(response, "text", encoding = "UTF-8")
  api_data <- jsonlite::fromJSON(api_data)
  
  # Extract relevant information from the parsed data
  keys <- names(api_data$dimension$Tid$category$label)
  year <- substr(keys, 1, 4)
  month <- substr(keys, 6, 7)
  value <- api_data$value
  fuel_type <- api_data$dimension$Drivmedel$category$label
  
  # Convert the character vector to a factor
  fuel_type <- factor(unlist(fuel_type))
  
  # Create a data frame
  data_df <- data.frame(year = as.integer(year),
                        month = as.integer(month),
                        fuel_type = fuel_type,
                        value = value)
  
  # Convert month to factor with appropriate labels
  month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  data_df$month <- factor(data_df$month, levels = 1:12, labels = month_labels)
  
  # Plot the data with a grouped bar plot
  ggplot(data_df, aes(x = month, y = value, fill = fuel_type)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    labs(x = "Month", y = "Total Value", fill = "Fuel Type", title = "Total Value of New Car Registrations by Fuel Type, 2023-2024") +
    scale_fill_manual(values = c("bensin" = "chartreuse", "diesel" = "#ff7f0e", "elhybrid" = "darkblue", "laddhybrid" = "darkred")) +
    facet_wrap(~year, scales = "free", ncol = 1) +
    theme_minimal() +
    theme(legend.position = "top")  # Move legend to the top
} else {
  # Handle error
  stop("Error: API request failed.")
}

summary(data_df)
str(data_df)
print(data_df)
coef_summary_api <- summary(data_df)
cat("Coefficient Summary:\n")
print(coef_summary_api)

