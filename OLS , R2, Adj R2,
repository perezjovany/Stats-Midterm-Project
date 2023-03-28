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

