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