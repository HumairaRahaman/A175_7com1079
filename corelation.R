library(ggplot2)
library(readr)  

# dataset

data <- read.csv("2016.csv")
head(data)

# Histogram for Happiness Score with Normal Curve Overlay
ggplot(data, aes(x = Happiness.Score)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.2, fill = "seagreen", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(data$Happiness.Score, na.rm = TRUE), 
                                         sd = sd(data$Happiness.Score, na.rm = TRUE)), 
                color = "red", linewidth = 1) +
  ggtitle("Histogram of Happiness Score with Normal Curve") +
  xlab("Happiness Score") +
  ylab("Density")

# Plotting a Normal Distribution Curve
x <- seq(min(data$Happiness.Score, na.rm = TRUE), max(data$Happiness.Score, na.rm = TRUE), length.out = 100)
y <- dnorm(x, mean = mean(data$Happiness.Score, na.rm = TRUE), sd = sd(data$Happiness.Score, na.rm = TRUE))
plot(x, y, type = "l", col = "blue", lwd = 2, main = "Normal Distribution Curve", 
     xlab = "Happiness Score", ylab = "Density")

# Scatterplot with Linear Trendline 
ggplot(data, aes(x = Economy..GDP.per.Capita., y = Happiness.Score)) +
  geom_point(color = "purple", size = 2, alpha = 0.7) +                     
  geom_smooth(method = "lm", color = "red", se = FALSE) +               
  ggtitle("Scatterplot of Happiness Score vs Economy (GDP per Capita)") +
  xlab("Economy (GDP per Capita)") +
  ylab("Happiness Score")

