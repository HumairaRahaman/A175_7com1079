library(readr)

happiness_data <- read_csv("2016.csv")

names(happiness_data)[4] <- "happiness_score"
names(happiness_data)[7] <- "gdp_per_capita"

df2 <- subset(happiness_data, !is.na(happiness_score) & !is.na(gdp_per_capita))
# Histogram for Happiness Score with a normal curve overlay
hist(df2$happiness_score, 
     main = "Histogram of Happiness Score with Normal Curve", 
     xlab = "Happiness Score", 
     col = "lightgreen", 
     border = "black", 
     freq = FALSE)  # Use density for y-axis to overlay curve

# Normal distribution curve
curve(dnorm(x, mean = mean(df2$happiness_score), sd = sd(df2$happiness_score)), 
      col = "red", 
      lwd = 2, 
      add = TRUE)

# Correlation test using Pearson
correlation_result <- cor.test(df2$happiness_score, df2$gdp_per_capita, method = "pearson")
# Correlation test result
print("Correlation test =")
print(correlation_result)

# Scatterplot with linear trendline
plot(df2$gdp_per_capita, df2$happiness_score,
     xlab = "GDP Per Capita", 
     ylab = "Happiness Score", 
     main = "Scatterplot of GDP Per Capita vs Happiness Score",
     col = "blue", 
     pch = 16)
# Linear trend line to the scatterplot
abline(lm(happiness_score ~ gdp_per_capita, data = df2), col = "red", lwd = 2)


