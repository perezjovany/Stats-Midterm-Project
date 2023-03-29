# ---- TRAINING DATA ----

# Read and Clean Training Data
train_df = read.csv("Housing_train.csv")
head(train_df)

# Check for missing data
sum(is.na(train_df))

# Checking data types
str(train_df)

# Binary values conversion
train_df$mainroad = as.numeric(train_df$mainroad == "yes")
train_df$guestroom = as.numeric(train_df$guestroom == "yes")
train_df$basement = as.numeric(train_df$basement == "yes")
train_df$hotwaterheating = as.numeric(train_df$hotwaterheating == "yes")
train_df$airconditioning = as.numeric(train_df$airconditioning == "yes")
train_df$prefarea = as.numeric(train_df$prefarea == "yes")

# Dummy encoding
train_df$furnishingstatus = factor(train_df$furnishingstatus,
                                   levels=c("unfurnished", "semi-furnished", "furnished"))
contr.treatment(levels(train_df$furnishingstatus))
str(train_df)

# Drop index column
train_df = train_df[, !names(train_df) %in% "X"]
str(train_df)

# Descriptive Analysis
summary(train_df)

# ---- TESTING DATA ----

# Clean Testing Data
test_df = read.csv("Housing_test.csv")

# Check for missing data
sum(is.na(test_df))

# Binary values conversion
test_df$mainroad = as.numeric(test_df$mainroad == "yes")
test_df$guestroom = as.numeric(test_df$guestroom == "yes")
test_df$basement = as.numeric(test_df$basement == "yes")
test_df$hotwaterheating = as.numeric(test_df$hotwaterheating == "yes")
test_df$airconditioning = as.numeric(test_df$airconditioning == "yes")
test_df$prefarea = as.numeric(test_df$prefarea == "yes")

# Dummy encoding
test_df$furnishingstatus = factor(test_df$furnishingstatus,
                                  levels=c("unfurnished", "semi-furnished", "furnished"))

# Drop index column
test_df = test_df[, !names(test_df) %in% "X"]

str(test_df)

# ---- Univariate Analysis ----

# Summary statistics for numeric variables
summary(train_df[c("price", "area", "bathrooms", "stories", "parking")])

# Frequency table for categorical variables
table(train_df$mainroad)
table(train_df$guestroom)
table(train_df$basement)
table(train_df$hotwaterheating)
table(train_df$airconditioning)
table(train_df$prefarea)

# Bar and box plots for categorical variables
barplot(table(train_df$mainroad), main="Main Road (Yes/No)", xlab="Main Road")
boxplot(train_df$price ~ train_df$mainroad, main = "Price differences of Main Road",
        xlab = "Main Road", ylab = "Price")
barplot(table(train_df$guestroom), main="Guest Room (Yes/No)", xlab="Guest Room")
boxplot(train_df$price ~ train_df$guestroom, main = "Price differences of Guest Room",
        xlab = "Guest Room", ylab = "Price")
barplot(table(train_df$basement), main="Basement (Yes/No)", xlab="Basement")
boxplot(train_df$price ~ train_df$basement, main = "Price differences of Basement",
        xlab = "Basement", ylab = "Price")
barplot(table(train_df$hotwaterheating), main="Hot Water Heating (Yes/No)", xlab="Hot Water Heating")
boxplot(train_df$price ~ train_df$hotwaterheating, main = "Price differences of Hot Water Heating",
        xlab = "Hot Water Heating", ylab = "Price")
barplot(table(train_df$airconditioning), main="Air Conditioning (Yes/No)", xlab="Air Conditioning")
boxplot(train_df$price ~ train_df$airconditioning, main = "Price differences of Air Conditioning",
        xlab = "Air conditioning", ylab = "Price")
barplot(table(train_df$prefarea), main="Preferred Area (Yes/No)", xlab="Preferred Area")
boxplot(train_df$price ~ train_df$prefarea, main = "Price differences of Preferred Area",
        xlab = "Preferred Area", ylab = "Price")

# Bar plot for ordinal variable
barplot(table(train_df$stories), main="Number of Stories", xlab="Stories")

# Box plot for numeric variables
boxplot(train_df$price, main="Price")
boxplot(train_df$area, main="Area")
boxplot(train_df$bathrooms, main="Number of Bathrooms")
boxplot(train_df$parking, main="Number of Parking Spaces")


# ---- Bivariate Analysis ----

# Load required libraries
#library(ggplot2)
#library(reshape2)

# Select only numeric variables
numeric_vars <- train_df[, sapply(train_df, is.numeric)]

# Create correlation matrix
corr_matrix <- cor(numeric_vars)

# Convert correlation matrix to long format
corr_df <- reshape2::melt(corr_matrix)

# Create heatmap with numbers
ggplot(corr_df, aes(x=Var1, y=Var2, fill=value, label=round(value, 2))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(size=3, color="black") +
  theme_minimal()

# Now create Scatter Plots for the pairs of features having significant correlations.


# Select significant correlations
sig_corrs <- subset(reshape2::melt(corr_matrix), abs(value) > 0.38 & Var1 != Var2)
sig_corrs

#Scatterplot to visualize relationship between Area and Price
ggplot(train_df, aes(x = area, y = price)) +
  geom_point() +
  labs(x = "Area", y = "Price") +
  theme_minimal()

ggplot(train_df, aes(x = bathrooms, y = price)) +
  geom_point() +
  labs(x = "Bathrooms", y = "Price") +
  theme_minimal()


ggplot(train_df, aes(x = stories, y = price)) +
  geom_point() +
  labs(x = "Stories", y = "Price") +
  theme_minimal()

ggplot(train_df, aes(x = airconditioning, y = price)) +
  geom_point() +
  labs(x = "Air Conditioning", y = "Price") +
  theme_minimal()

ggplot(train_df, aes(x = parking, y = price)) +
  geom_point() +
  labs(x = "Parking", y = "Price") +
  theme_minimal()

ggplot(train_df, aes(x = bathrooms, y = bedrooms)) +
  geom_point() +
  labs(x = "Bathrooms", y = "Bedrooms") +
  theme_minimal()

ggplot(train_df, aes(x = stories, y = bedrooms)) +
  geom_point() +
  labs(x = "Stories", y = "Bedrooms") +
  theme_minimal()

ggplot(train_df, aes(x = price, y = airconditioning)) +
  geom_point() +
  labs(x = "price", y = "air conditioning") +
  theme_minimal()



# ---- Model Building ----

# OLS model
model <- lm(price ~ ., data = train_df)
summary(model)

# Iteratively remove insignificant parameters
max_p_value <- 0.005

is_insignificant <- function(model) {
  p_values <- summary(model)$coefficients[,"Pr(>|t|)"]
  any(p_values[-1] > max_p_value)
}

while (is_insignificant(model)) {
  coef_summary <- summary(model)$coefficients
  p_values <- coef_summary[,"Pr(>|t|)"]
  max_p_index <- which.max(p_values[-1]) + 1
  insignificant_var <- rownames(coef_summary)[max_p_index]
  
  if (length(insignificant_var) > 0) {
    message("Removing ", insignificant_var)
    model <- update(model, as.formula(paste("~ . -", insignificant_var)))
  } else {
    break
  }
}

# Final model summary
summary(model)

# R2 and Adjusted R2 values
r_squared <- summary(model)$r.squared
adjusted_r_squared <- summary(model)$adj.r.squared

cat("R2: ", r_squared, "\n")
cat("Adjusted R2: ", adjusted_r_squared, "\n")

#checking assumtpions of multiple linear regresssion

#checking scatter plot of each independent varaible against price.
scatter_plots <- function(df) {
  y_var <- colnames(df)[1] # Get the name of the first column as the y variable
  
  # Loop through each remaining column and plot a scatter plot with the y variable
  for (x_var in colnames(df)[-1]) {
    plot(df[[x_var]], df[[y_var]], main = paste("Scatter Plot of", y_var, "vs.", x_var), xlab = x_var, ylab = y_var)
  }
}
scatter_plots(train_df)
#only bedrooms appears to have a non-linear relationship, but this variable was ommitted rom the final model.

#multicollinearity
#from the earlier heatmap, thid condition appears to be met.

#Independence and Homoscedasticity
my_resid <- resid(model)
plot(fitted(model), my_resid, xlab = "Predicted Values", ylab = "Residuals", main = "Residual Plot")
abline(h = 0)
#potential heteroscedacity

#Performance
#use our model with the test_df

predictions <- predict(model, test_df)
predictions
test_price = test_df$price

plot(predictions, test_price, xlab = "Predicted Values", ylab = "Actual Values", main = "Actual vs. Predicted Values",
     xlim = c(0, max(predictions, test_price)), ylim = c(0, max(predictions, test_price)))
abline(0, 1, col = "red")

#we can calculate the r^2 value of the model using the predicted vs actual price
test_r = cor(predictions, test_price)
test_r # r = .8292326

test_r_squared = test_r * test_r
test_r_squared # r^2 = .6876

MSE = mean((test_price - predictions)^2)
RMSE = sqrt(MSE)
RMSE # rmse = 1022871
test_average_price = mean(test_price)
test_average_price # 4608023

# Calculate adjusted R-squared
n <- nrow(test_df)
p <- length(model$coefficients) - 1 # subtract 1 for the intercept term
adj_test_r_squared <- 1 - ((1 - test_r_squared) * (n - 1) / (n - p - 1))
adj_test_r_squared #adj R^2 = .6558

#removing the 3 outliers and recalculating
new_test_df = test_df[-c(1,3,4),] #outliers are rows 1,3,4

#use our model with the new_test_df

new_predictions <- predict(model, new_test_df)
new_predictions
new_test_price = new_test_df$price

plot(new_predictions, new_test_price, xlab = "Predicted Values", ylab = "Actual Values", main = "Actual vs. Predicted Values",
     xlim = c(0, max(predictions, test_price)), ylim = c(0, max(predictions, test_price)))
abline(0, 1, col = "red")

#we can calculate the r^2 value of the model using the predicted vs actual price
new_test_r = cor(new_predictions, new_test_price)
new_test_r # r = .813

new_test_r_squared = new_test_r * new_test_r
new_test_r_squared # r^2 = .6609

MSE = mean((new_test_price - new_predictions)^2)
RMSE = sqrt(MSE)
RMSE # rmse = 957023.4
new_test_average_price = mean(new_test_price)
new_test_average_price # 4450514

# Calculate adjusted R-squared
n <- nrow(new_test_df)
p <- length(model$coefficients) - 1 # subtract 1 for the intercept term
adj_new_test_r_squared <- 1 - ((1 - new_test_r_squared) * (n - 1) / (n - p - 1))
adj_new_test_r_squared #adj R^2 = .6252
