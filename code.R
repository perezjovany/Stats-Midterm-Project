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

# Bar plot for categorical variables
barplot(table(train_df$mainroad), main="Main Road (Yes/No)", xlab="Main Road")
barplot(table(train_df$guestroom), main="Guest Room (Yes/No)", xlab="Guest Room")
barplot(table(train_df$basement), main="Basement (Yes/No)", xlab="Basement")
barplot(table(train_df$hotwaterheating), main="Hot Water Heating (Yes/No)", xlab="Hot Water Heating")
barplot(table(train_df$airconditioning), main="Air Conditioning (Yes/No)", xlab="Air Conditioning")
barplot(table(train_df$prefarea), main="Preferred Area (Yes/No)", xlab="Preferred Area")

# Bar plot for ordinal variable
barplot(table(train_df$stories), main="Number of Stories", xlab="Stories")

# Box plot for numeric variables
boxplot(train_df$price, main="Price")
boxplot(train_df$area, main="Area")
boxplot(train_df$bathrooms, main="Number of Bathrooms")
boxplot(train_df$parking, main="Number of Parking Spaces")


# ---- Bivariate Analysis ----

# Load required libraries
library(ggplot2)
library(reshape2)

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
max_p_value <- 0.05

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
