library(readr)

# Load the dataset
happiness_information <- read_csv("2016.csv")

#assign the all value
names(happiness_information)[4] <- "happiness_score"
names(happiness_information)[7] <- "gdp_per_capita"
names(happiness_information)[8] <- "family"
names(happiness_information)[10] <- "freedom"

# Filter data to exclude rows with missing happiness_score and gdp_per_capita
df4 <- subset(happiness_information, !is.na(happiness_score) & !is.na(gdp_per_capita))

# Ensure 'happiness_score' is numeric
happiness_information$happiness_score <- as.numeric(happiness_information$happiness_score)

# Define the threshold for categorization (mean of happiness_score)
threshold <- mean(happiness_information$happiness_score, na.rm = TRUE)

# Categorize data into High and Low Happiness based on the threshold
happiness_information$happiness_category <- ifelse(happiness_information$happiness_score > threshold,
                                                   "High Happiness",
                                                   "Low Happiness")

# Split data into two groups
high_happiness <- happiness_information[happiness_information$happiness_category == "High Happiness", "happiness_score"]
low_happiness <- happiness_information[happiness_information$happiness_category == "Low Happiness", "happiness_score"]

# Perform a t-test
t_test_result <- t.test(high_happiness, low_happiness, var.equal = FALSE)

# Print the t-test results
print("T-Test Results:")
print(t_test_result)

# Interpretation
if (t_test_result$p.value < 0.05) {
  print("Reject the null hypothesis")
} else {
  print("Fail to reject the null hypothesis")
}

# Histogram for Happiness Score with a normal curve overlay
hist(df4$happiness_score, 
     main = "Histogram of Happiness Score with Normal Curve", 
     xlab = "Happiness Score", 
     col = "lightgreen", 
     border = "black", 
     freq = FALSE,
     ylim = c(0, 0.35)
     )  # Use density for y-axis to overlay curve

# Normal distribution curve
curve(dnorm(x, mean = mean(df4$happiness_score), sd = sd(df4$happiness_score)), 
      col = "red", 
      lwd = 2, 
      add = TRUE)

# Correlation test using Pearson
correlation_result <- cor.test(df4$happiness_score, df4$gdp_per_capita, method = "pearson")
# Correlation test result
print(correlation_result)

# Scatterplot with linear trendline
plot(df4$gdp_per_capita, df4$happiness_score,
     xlab = "Economy (GDP Per Capita)", 
     ylab = "Happiness Score", 
     main = "Scatterplot of Economy (GDP Per Capita) vs Happiness Score",
     col = "blue", 
     pch = 16,
     xlim = c(min(df4$gdp_per_capita), max(df4$gdp_per_capita)))

# Linear trend line to the scatterplot
abline(lm(happiness_score ~ gdp_per_capita, data = df4), col = "red", lwd = 2)


