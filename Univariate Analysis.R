# Univariate Analysis

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
