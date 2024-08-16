## Visual Story Telling: Part 1
rm(list = ls())
library("tidyverse")
library("dplyr")
library("ggplot2")


df <- read.csv("/Users/alexparson/Documents/UT MSBA 2025/STA 380/Unsupervised Learning/greenbuildings.csv")
glimpse(df)

column_names <- names(df)

summary(df)

# probably want to trim off lower leasing rates and super high rents

# Plot of size by green rating
n_bins <- 7
quantiles <- quantile(df$size, probs = seq(0, 1, length.out = n_bins + 1))
df$bins_size <- cut(df$size, breaks = quantiles, include.lowest = TRUE, labels = paste("Q", 1:n_bins, sep = ""))
crosstab <- table(df$green_rating, df$bins_size)

# Plot by Class and green rating

crosstab <- table(df$class_a, df$green_rating)
crosstab

# Plot by leasing_rate and green_rating
n_bins <- 4
quantiles <- quantile(df$leasing_rate, probs = seq(0, 1, length.out = n_bins + 1))
df$bins_lr <- cut(df$leasing_rate, breaks = quantiles, include.lowest = TRUE, labels = paste("Q", 1:n_bins, sep = ""))
crosstab <- table(df$green_rating, df$bins_lr)
crosstab
# look into the rent with net variable. Want to make sure we are looking at true rents
crosstab <- table(df$green_rating, df$net)

df <- df %>% filter(leasing_rate > .1)
# Original Analysis
result <- df %>%
  group_by(green_rating) %>%
  summarise( average_occupancy = median(leasing_rate),
             average_size = median(size),
             Med_Rent = median(Rent),
             med_cluster = median(cluster_rent))
result

# First, we want to make sure that we are comparing rents equally. This means we should compare rents that have utilities included, and rents that don't have them included. 
# When we do this, we notice a significant change 
# Looking at average rent by net, rent
result <- df %>%
  group_by(net, green_rating) %>%
  summarise( average_occupancy = median(leasing_rate),
             average_size = median(size),
             Med_Rent = median(Rent),
             med_cluster = median(cluster_rent))

result

ggplot(result, aes(x = factor(net), y = Med_Rent, fill = factor(green_rating))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Med_Rent), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  scale_fill_manual(values = c("blue", "orange")) +
  labs(title = "Median Rent by Net and Green Rating",
       x = "Net",
       y = "Median Rent",
       fill = "Green Rating") +
  theme_minimal()

# We see that Green Buildings have a Rent increase of 3$ over normal buildings when utilities are included in the rent and a .3$ increase when they are not. 
# However, this is not an accurate comparison, as we still aren't comparing similar buildings. 
# We see that normal buildings are significantly smaller in size when comparing to green buildings. Additionally, we will want to 
# make sure we are comparing similar classes of buildings, so we will look at class_a and class_b

result <- df %>%
  filter(class_a == 1,
         size >= 200000 & size <= 300000) %>% 
  group_by(net, green_rating) %>%
  summarise( average_occupancy = median(leasing_rate),
             average_size = median(size),
             Med_Rent = median(Rent),
             med_cluster = median(cluster_rent)
            )
result

ggplot(result, aes(x = factor(net), y = Med_Rent, fill = factor(green_rating))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Med_Rent), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  scale_fill_manual(values = c("blue", "orange")) +
  labs(title = "Class_a Median Rent by Net and Green Rating",
       x = "Net",
       y = "Median Rent",
       fill = "Green Rating") +
  theme_minimal()

## In this instance, we see that normal buildings have a slightly lower rent price when including utilities and a significantly lower rent price otherwise. This suggests
# it is more profitable to build a normal building. 

# we see that when class_b == 1 we have lower occupancy and lower average_size, but a higher median rent
result <- df %>%
  filter(class_b == 1,
         size >= 200000 & size <= 300000) %>% 
  group_by(net, green_rating) %>%
  summarise( average_occupancy = median(leasing_rate),
             average_size = median(size),
             Med_Rent = median(Rent),
             med_cluster = median(cluster_rent)
  )
result

ggplot(result, aes(x = factor(net), y = Med_Rent, fill = factor(green_rating))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Med_Rent), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  scale_fill_manual(values = c("blue", "orange")) +
  labs(title = "Class_b Median Rent by Net and Green Rating",
       x = "Net",
       y = "Median Rent",
       fill = "Green Rating") +
  theme_minimal()

# For class_b buildings, we see a large decrease in median Rent and a slightly higher occupancy rate. It is less profitable to build a green class_b building relative to a normal
# building

## Visual Story Telling: Part 2
df <- read.csv("/Users/alexparson/Documents/UT MSBA 2025/STA 380/Unsupervised Learning/capmetro_UT.csv")
glimpse(df)

column_names <- names(df)

summary(df)

df %>% group_by(month) %>%
       summarise(avg_temp = mean(temperature))

result <- df %>%
  mutate(day = as.Date(timestamp)) %>% 
  group_by(month, day) %>%
  summarise( avg_temp = mean(temperature),
             total_boarding = sum(boarding),
             total_alighting = sum(alighting)) %>% 
  mutate(change = total_boarding - total_alighting,
         avg_daily_error = change/total_boarding) %>% 
  group_by(month) %>% 
  summarise(avg_error = mean(avg_daily_error))
result
## Unless people are sleeping on the bus, we should assume that our optical scanner has some error

# looking how average temperature changes ridership
result <- df %>%
  mutate(day = as.Date(timestamp)) %>% 
  filter(hour_of_day >= 10,
         hour_of_day <= 16) %>%
  mutate(day = as.Date(timestamp)) %>% 
  group_by(month, day) %>%
  summarise( avg_temp = mean(temperature),
             total_boarding = sum(boarding),
             total_alighting = sum(alighting)) %>%
  group_by(avg_temp) %>% 
    summarise(avg_boarding = mean(total_boarding),
              avg_alighting = mean(total_alighting))
  
print(result, n=100)
