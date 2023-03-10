# Read and Clean Data
test_df = read.csv("Housing_test.csv")
head(test_df)

sum(is.na(test_df))

# Descriptive Analysis
dim(test_df)

str(test_df)

summary(test_df)

colnames(test_df)
