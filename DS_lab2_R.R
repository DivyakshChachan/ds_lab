# Name - Divyaksh Chachan
# Roll No. - U23AI124
# Data Science Lab-II (R Version)

# -----------------------------
# 1. Load libraries and data
# -----------------------------
library(tidyverse)
library(lubridate)
library(ggplot2)
library(moments)
library(reshape2)
library(gridExtra)
library(RColorBrewer)
library(scales)
library(stats)
if(!require(corrplot)) install.packages("corrplot"); library(corrplot)

setwd("/Users/divyakshchachan/Documents/ds_lab")
df <- read.csv("yellow_tripdata_sample.csv")

# -----------------------------
# Save console output to file
# -----------------------------
output_file <- "output.txt"
sink(output_file, append = FALSE, split = TRUE)  # Console output saved to file

# -----------------------------
# 2. Data cleaning and datetime conversion
# -----------------------------
df$tpep_pickup_datetime <- ymd_hms(df$tpep_pickup_datetime)
df$tpep_dropoff_datetime <- ymd_hms(df$tpep_dropoff_datetime)

# -----------------------------
# 3. Descriptive statistics
# -----------------------------
numeric_cols <- c('passenger_count', 'trip_distance', 'fare_amount', 'total_amount', 'tip_amount', 'extra')

cat("----- Descriptive Statistics -----\n")
describe_stats <- df %>%
  summarise(across(all_of(numeric_cols), list(
    mean = mean,
    sd = sd,
    var = var,
    skewness = skewness,
    kurtosis = kurtosis
  ), na.rm = TRUE))
print(describe_stats)

cat("\n----- Missing Values -----\n")
missing_stats <- df %>%
  summarise(across(all_of(numeric_cols), ~sum(is.na(.))))
print(missing_stats)

# -----------------------------
# 4. Outlier removal (IQR method)
# -----------------------------
remove_outliers_iqr <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  data[data[[column]] >= lower & data[[column]] <= upper, ]
}

for (col in c('trip_distance', 'fare_amount', 'tip_amount')) {
  df <- remove_outliers_iqr(df, col)
}

# -----------------------------
# 5. Open PDF device for all plots
# -----------------------------
pdf("Rplots.pdf", width = 7, height = 5)  # All plots will go here

# Histograms & Density Plots
for (col in c('trip_distance', 'fare_amount', 'tip_amount')) {
  p <- ggplot(df, aes_string(x = col)) +
    geom_histogram(bins = 30, fill = 'blue', alpha = 0.6) +
    geom_density(color = 'red') +
    ggtitle(paste('Histogram & Density Plot of', col))
  print(p)
}

# Boxplots
for (col in c('trip_distance', 'fare_amount', 'tip_amount')) {
  p <- ggplot(df, aes_string(x = col)) +
    geom_boxplot(fill = 'orange', alpha = 0.6) +
    ggtitle(paste('Box Plot of', col))
  print(p)
}

# Bar chart for payment_type
p <- ggplot(df, aes(x = factor(payment_type))) +
  geom_bar(fill = 'purple') +
  ggtitle('Payment Type Distribution')
print(p)

# Pie chart for VendorID
vendor_counts <- df %>% count(VendorID)
p <- ggplot(vendor_counts, aes(x = '', y = n, fill = factor(VendorID))) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar('y') +
  ggtitle('VendorID Proportions')
print(p)

# -----------------------------
# 6. Inferential statistics
# -----------------------------
cat("\n----- Confidence Intervals (95%) -----\n")
for (col in c('trip_distance', 'fare_amount', 'tip_amount')) {
  vals <- df[[col]][!is.na(df[[col]])]
  mean_val <- mean(vals)
  sem <- sd(vals) / sqrt(length(vals))
  ci <- mean_val + c(-1, 1) * qt(0.975, df = length(vals) - 1) * sem
  cat(paste('95% CI for mean', col, ':', round(ci[1],2), 'to', round(ci[2],2)), "\n")
}

cat("\n----- One-sample t-test (tip_amount vs $2) -----\n")
test_tip <- t.test(df$tip_amount, mu = 2)
print(test_tip)

cat("\n----- Two-sample t-test (fare by payment_type) -----\n")
fare_credit <- df$fare_amount[df$payment_type == 1]
fare_cash <- df$fare_amount[df$payment_type == 2]
test_fare <- t.test(fare_credit, fare_cash)
print(test_fare)

cat("\n----- Chi-square test (payment_type vs RatecodeID) -----\n")
tab <- table(df$payment_type, df$RatecodeID)
chi_test <- chisq.test(tab)
print(chi_test)

# -----------------------------
# 7. Correlation analysis and heatmap
# -----------------------------
cat("\n----- Correlation Matrix -----\n")
corr_matrix <- cor(df[,c('trip_distance', 'fare_amount', 'tip_amount', 'total_amount')], use = 'complete.obs')
print(corr_matrix)

corrplot(corr_matrix, method = 'color', addCoef.col = 'black', tl.col = 'black', number.cex = 0.7)

# -----------------------------
# 8. Bonus: Time series plots
# -----------------------------
df$pickup_hour <- hour(df$tpep_pickup_datetime)
df$pickup_day <- as.Date(df$tpep_pickup_datetime)

# Trip counts per day
trip_counts <- df %>% group_by(pickup_day) %>% summarise(count = n())
p <- ggplot(trip_counts, aes(x = pickup_day, y = count)) +
  geom_line(color = 'blue') +
  ggtitle('Trips Per Day')
print(p)

# Average fare by hour
avg_fare_by_hour <- df %>% group_by(pickup_hour) %>% summarise(avg_fare = mean(fare_amount, na.rm = TRUE))
p <- ggplot(avg_fare_by_hour, aes(x = pickup_hour, y = avg_fare)) +
  geom_bar(stat = 'identity', fill = 'green') +
  ggtitle('Average Fare by Hour of Day')
print(p)

# Close PDF device
dev.off()

# -----------------------------
# End sink
# -----------------------------
sink()  # Stop diverting output
cat("All statistics saved to", output_file, "\n")
cat("All plots saved to Rplots.pdf\n")
