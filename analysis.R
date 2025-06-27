
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(GGally)         # for correlation plots
library(ggthemes)       # for improved plot themes
library(forecast)       # for time series decomposition
library(broom)          # for tidy regression output


air_quality <- read_csv("data/air_quality.csv")
resp_cases <- read_csv("data/respiratory_cases.csv")

# Convert date column
air_quality$Date <- as.Date(air_quality$Date, format = "%Y-%m-%d")
resp_cases$Date <- as.Date(resp_cases$Date, format = "%Y-%m-%d")

# Merge datasets
df <- merge(air_quality, resp_cases, by = c("Date", "City"))

# Ensure no missing data in important columns
df <- df %>% drop_na(PM2.5, Temperature, Humidity, Cases)

summary(df)

# Grouped summary by city
city_stats <- df %>%
  group_by(City) %>%
  summarise(
    avg_pm25 = mean(PM2.5),
    avg_cases = mean(Cases),
    sd_cases = sd(Cases),
    n = n()
  )

print(city_stats)



# 1. Time series plot of pollution & cases
ggplot(df, aes(x = Date)) +
  geom_line(aes(y = PM2.5, color = "PM2.5")) +
  geom_line(aes(y = scale(Cases), color = "Respiratory Cases (scaled)")) +
  facet_wrap(~City) +
  labs(title = "PM2.5 and Respiratory Cases Over Time",
       y = "Value (scaled where needed)",
       x = "Date") +
  theme_minimal() +
  scale_color_manual(values = c("PM2.5" = "blue", "Respiratory Cases (scaled)" = "darkred"))

ggsave("plots/timeseries_pm25_cases.png", width = 10, height = 6)

# 2. Correlation matrix
cor_data <- df %>% 
  select(PM2.5, Temperature, Humidity, Cases) %>%
  rename(CASES = Cases)

ggpairs(cor_data)
ggsave("plots/correlation_matrix.png", width = 8, height = 6)

# 3. Boxplot of cases by city
ggplot(df, aes(x = City, y = Cases, fill = City)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of Respiratory Cases by City")

ggsave("plots/cases_by_city_boxplot.png", width = 8, height = 5)



# Let's choose one city (e.g., Chicago)
chicago_data <- df %>%
  filter(City == "Chicago") %>%
  arrange(Date) %>%
  group_by(Date) %>%
  summarise(cases = mean(Cases))

# Convert to ts object (monthly frequency assumed)
ts_cases <- ts(chicago_data$cases, start = c(2015, 1), frequency = 12)

decomp <- stl(ts_cases, s.window = "periodic")
plot(decomp)
ggsave("plots/chicago_decomposition.png", width = 10, height = 6)


model <- lm(Cases ~ PM2.5 + Temperature + Humidity, data = df)

# Save model summary
sink("output/regression_summary.txt")
print(summary(model))
sink()

# Model diagnostics plots
png("plots/regression_diagnostics.png", width = 1000, height = 1000)
par(mfrow = c(2, 2))
plot(model)
dev.off()


model_summary <- tidy(model)
write_csv(model_summary, "output/model_coefficients.csv")

glance_summary <- glance(model)
write_csv(as.data.frame(glance_summary), "output/model_fit_stats.csv")
