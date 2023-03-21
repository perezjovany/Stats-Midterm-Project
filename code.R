# Read and Clean Data
test_df = read.csv("Housing_test.csv")
head(test_df)

# Check for missing data
sum(is.na(test_df))

# Checking data types
str(test_df)

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
contr.treatment(levels(test_df$furnishingstatus))
str(test_df)

# Descriptive Analysis
summary(test_df)

# Univariate Analysis