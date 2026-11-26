#Install packages
#install.packages(c("tidyverse", "lubridate", "corrplot")) 

#Run libraries
library(tidyverse)
library(lubridate)
library(corrplot)

#Import data
steel_industry_data <- "Steel_industry_energy_consumption_data.csv" 
df_raw <- read_csv(steel_industry_data, col_types = cols(date = col_character()))
df_raw

# Checking data
dim(df_raw)        # 35040 rows, 11 columns
glimpse(df_raw)
head(df_raw)
summary(df_raw)



#Cleaning Data

#Rename columns to shorter names
stopifnot(ncol(df_raw) == 11) # number of columns safety check before name changes
df <- df_raw
names(df) <- c("date", "usage_kwh", "lag_reactive", "lead_reactive", "co2",
               "lag_pf", "lead_pf", "nsm", "week_status", "day_of_week",
               "load_type")

#Parse date day/month/year  hour:minutes
df$date <- parse_date_time(df$date,
                           orders = c("dmY HM", "dmY HMS", "Ymd HMS", "Ymd HM"),
                           tz = "UTC")

sum(is.na(df$date)) # check missing values on date column caused by wrong date format
range(df$date) # first and last timestamps

#Missing values
colSums(is.na(df))

#Duplicates
sum(duplicated(df))

# check labels of the categorical data columns
table(df$week_status)
table(df$day_of_week)
table(df$load_type)

#convert the text columns (week_status, day_of_week and load_type) into factors

df <- df %>% mutate(week_status = factor(week_status),
                    day_of_week = factor(day_of_week,
                    levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                    "Friday", "Saturday", "Sunday")),
                    load_type   = factor(load_type, # Ordered, because Light < Medium < Maximum
                                         levels = c("Light_Load", "Medium_Load", 
                                                    "Maximum_Load"),ordered = TRUE)
  )
    
# Helper time features
df <- df %>%
  mutate(
    hour    = hour(date),
    minute  = minute(date),
    month   = month(date, label = TRUE),
    weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                "Saturday", "Sunday")[wday(date, week_start = 1)]
  )


# Check Outliers following IQR rule without blindly deleting it
# In this case, in energy data, high values are often real production peaks.

iqr_outliers <- function(x) {
  q <- quantile(x, c(0.25, 0.75)) 
  fence <- 1.5 * (q[2] - q[1])
  sum(x < q[1] - fence | x > q[2] + fence)
}

df %>% summarise(across(c(usage_kwh, lag_reactive, lead_reactive, co2, lag_pf, 
                          lead_pf), iqr_outliers))                    

# EDA

#Variation(usage_kWh)

#How is energy usage distributed ?

ggplot(df, aes(usage_kwh)) +
  geom_histogram(bins = 60, fill = "steelblue", colour = "white") +
  geom_vline(xintercept = median(df$usage_kwh), colour = "red", linewidth = 1) +
  geom_vline(xintercept = mean(df$usage_kwh), colour = "darkgreen",
             linewidth = 1, linetype = "dashed") +
  labs(title = "Distribution of energy usage",
       subtitle = "Red line = median, green dashed line = mean",
       x = "Usage (kWh per 15 min)", y = "Number of readings")
summary(df$usage_kwh)



# Covariation

# Q1. How is energy usage distributed in each load type ?

# The histogram shows odd low values on all load types
ggplot(df, aes(usage_kwh, fill = load_type)) +
  geom_histogram(bins = 40, colour = "white", show.legend = FALSE) +
  facet_wrap(~ load_type, ncol = 1, scales = "free_y") +
  labs(title = "Energy usage by load type",
       x = "Usage (kWh per 15 min)", y = "Number of readings")
summary(df$usage_kwh)




# Q2 At what time of the day are low-usage readings labelled Medium/Maximum Load ?
low_energy_cutoff <- 6  

low_odd   <- df %>% filter(usage_kwh < low_energy_cutoff, load_type != "Light_Load")

nrow(low_odd) #how many rows
nrow(low_odd) / nrow(df)  *100    # what % of the dataset


# odd kWh usage most in the evening, and between 12pm and 13pm(possibly lunch time)
low_odd %>%                                  # when do they happen?
  count(hour) %>%
  ggplot(aes(hour, n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Low-usage readings labelled Medium/Maximum Load, by hour of the day",
       x = "Hour of day", y = "Number of readings")



# filter odd load_type readings

df_filtered <- df %>%
  filter(!(usage_kwh < low_energy_cutoff & load_type != "Light_Load"))

nrow(df)
nrow(df_filtered)   # difference should be 3,906

# Redraw the energy distribution histogram without the odd low values on medium and maximum load types
ggplot(df_filtered, aes(usage_kwh, fill = load_type)) +
  geom_histogram(bins = 40, colour = "white", show.legend = FALSE) +
  facet_wrap(~ load_type, ncol = 1, scales = "free_y") +
  labs(title = "Energy usage by load type (odd readings removed)",
       x = "Usage (kWh per 15 min)", y = "Number of readings")
summary(df_filtered$usage_kwh)



#Task 3

# Enconding Categorical columns

#Stop if the cleaning data steps were not done
stopifnot(exists("df"),
          all(c("usage_kwh", "week_status", "day_of_week", "load_type", "hour") %in% names(df)))


# check categories
levels(df$week_status)    # 2 categories  -> binary encoding
levels(df$day_of_week)    # 7 categories, no order -> one-hot encoding
levels(df$load_type)      # 3 categories, ordered  -> ordinal (label) encoding

#Binary encoding week_status, Weekday = 0 and Weekend = 1
df_enc <- df %>% mutate(weekend_bin = ifelse(week_status == "Weekend", 1, 0))

#check the mapping
table(df$week_status, df_enc$weekend_bin)       

#ordinal(label) encoding load_type, Light = 1, medium = 2 and maximum = 3
df_enc <- df_enc %>% mutate(load_type_ord = as.integer(load_type))

# check the mapping
table(df$load_type, df_enc$load_type_ord)       

#One-hot encoding days of week, one 0/1 column per day
day_onehot <- model.matrix(~ day_of_week - 1, data = df)

colnames(day_onehot) <- sub("day_of_week", "is_", colnames(day_onehot))

# all rows have one 1
table(rowSums(day_onehot))     
df_enc <- bind_cols(df_enc, as_tibble(day_onehot))

# Categorical columns after binary, label and one-hot encoding
df_enc %>% select(week_status, weekend_bin, load_type, load_type_ord, 
                  day_of_week, starts_with("is_")) %>% head(10)


#Prepare data for PCA

num_vars <- c("usage_kwh", "lag_reactive", "lead_reactive", "co2", "lag_pf", 
              "lead_pf", "nsm")

pca_data <- df_enc %>% select(all_of(num_vars), weekend_bin, 
                              starts_with("is_"), -is_Monday)

dim(pca_data)                        # expect 35040 rows, 14 columns
anyNA(pca_data)                      # false
sapply(pca_data, sd)                 # no zero values (PCA cant use constant columns)
stopifnot(!anyNA(pca_data), all(sapply(pca_data, sd) > 0))


#PCA
# scale true so all variables are on the same scale
# and columns with big numbers dont dominate
pca <- prcomp(pca_data, center = TRUE, scale. = TRUE)
summary(pca)

#Variance 
pc_names <- paste0("PC", seq_along(pca$sdev))

var_tbl <- tibble(PC = factor(pc_names, levels = pc_names), 
                  eigenvalue = pca$sdev^2, 
                  prop = eigenvalue / sum(eigenvalue), cum = cumsum(prop))

print(var_tbl, n = Inf)

# Plot to show variance of each principal component
ggplot(var_tbl, aes(PC, prop)) +
  geom_col(fill = "steelblue") +
  geom_line(aes(y = cum, group = 1), colour = "red") +
  geom_point(aes(y = cum), colour = "red") +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Variance explained by each principal component",
       subtitle = "Red line = cumulative variance, dashed line = 80%",
       x = "Variance of each component", y = "Proportion of variance")

#Keep components which eigenvalue above 1
ggplot(var_tbl, aes(PC, eigenvalue, group = 1)) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
  labs(title = "Eigenvalues, keep those above 1",
       x = "Components", y = "Eigenvalue")

# How many components reach 80% of the variance?
k <- which(var_tbl$cum >= 0.80)[1]
k

#Columns and each components values
round(pca$rotation[, 1:3], 2)

as.data.frame(pca$rotation[, 1:3]) %>%
  rownames_to_column("variable") %>%
  pivot_longer(starts_with("PC"), names_to = "component", values_to = "loading") %>%
  ggplot(aes(variable, loading, fill = loading > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ component, ncol = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Loadings of the first three components",
       x = "", y = "Loading")

scores <- as_tibble(pca$x) %>%
  mutate(load_type   = df$load_type,
         week_status = df$week_status,
         hour        = df$hour)
stopifnot(nrow(scores) == nrow(df))

# PC1 vs PC2, coloured by load type
ggplot(scores, aes(PC1, PC2, colour = load_type)) +
  geom_point(alpha = 0.3, size = 0.8) +
  labs(title = "PC1 vs PC2 by load type", colour = "Load type")

# PC1 vs PC2, coloured by hour of day
ggplot(scores, aes(PC1, PC2, colour = hour)) +
  geom_point(alpha = 0.3, size = 0.8) +
  scale_colour_viridis_c() +
  labs(title = "PC1 vs PC2 by hour of day", colour = "Hour")


#PC1 plant activity
round(pca$rotation[, 1:2], 2)

# Weekdays and weekend days
cor(scores$PC1, df$usage_kwh)

bind_cols(scores["PC1"], df[c("usage_kwh", "week_status")]) %>%
  group_by(week_status) %>%
  summarise(mean_PC1   = mean(PC1),
            mean_usage = mean(usage_kwh))

cor(scores$PC2, df$nsm)

bind_cols(scores["PC2"],
          df[c("hour", "usage_kwh", "lag_pf", "lead_reactive")]) %>%
  mutate(stage = cut(hour, breaks = c(-1, 5, 11, 17, 23),
                     labels = c("Night (0-5)", "Morning (6-11)",
                                "Afternoon (12-17)", "Evening (18-23)"))) %>%
  group_by(stage) %>%
  summarise(mean_PC2           = mean(PC2),
            mean_usage         = mean(usage_kwh),
            mean_lag_pf        = mean(lag_pf),
            mean_lead_reactive = mean(lead_reactive))
