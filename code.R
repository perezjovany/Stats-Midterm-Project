# Read and Clean Data
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

# Univariate Analysis