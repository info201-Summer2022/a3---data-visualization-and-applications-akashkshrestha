# Akash Shrestha
# INFO 201
# Professor Deeb-Swihart
# TA: Alyson Yu
# 8/14/22
# Assignment 3: Incarceration Analysis
library("stringr")

# Load dataset in
incarceration_data <- read.csv("incarceration_trends.csv")

# Summary Values

# Which state had the highest total jail population in the dataset?
# Store the state in the variable 'highest_jail_state'.

jail_highest_state <- incarceration_data %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = T)) %>%
  pull(state)

# What year had the highest total male jail population from 1976 to 2018?
# Store the year in the variable 'jail_highest_year'.

jail_highest_year <- incarceration_data %>%
  filter(male_adult_jail_pop == max(male_adult_jail_pop, na.rm = T)) %>%
  pull(year)

# What state has the highest total AAPI jail population in 2018? 
# Store the state and number in the variable `high_state_aapi`

# What state had the highest total AAPI jail population in 2018?
# Store

high_state_aapi <- incarceration_data %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(aapi_jail_pop == max(aapi_jail_pop, na.rm = T)) %>%
  pull(state, aapi_jail_pop)

# What is the total AAPI jail population in the state of WA?
# Combine the amount from each year and store in the variable `wa_aapi`

wa_aapi_jail <- incarceration_data %>%
  group_by(year) %>%
  filter(state == "WA") %>%
  filter(aapi_jail_pop == aapi_jail_pop) %>%
  summarize(amount = sum(aapi_jail_pop))

wa_aapi <- sum(wa_aapi_jail[, "amount"])

# Change in percentage of aapi jail incarceration from the past 10 years (2008-2018)
aapi_jail_change_year <- incarceration_data %>%
select(year, state, aapi_jail_pop) %>%
  group_by(year) %>%
  summarise(aapi_jail_pop = sum(aapi_jail_pop, na.rm = TRUE)) %>% 
  summarise(
    change = abs((aapi_jail_pop[year == 2018] - aapi_jail_pop[year == 2008])/aapi_jail_pop[year == 2008]))%>%
  mutate(change_decimal = round(change, 2)) %>% 
  pull(change_decimal)

# Trends Over Time Chart (Histogram/Line Chart)

library(ggplot2)

aapi_histogram <- ggplot(incarceration_data, aes(x=year, y=aapi_jail_pop)) +
  geom_line(aes(color = "AAPI")) +
  labs(x = "Year", y = "AAPI Jail Pop") +
  ggtitle("AAPI Jailed Population across the United States")

# Comparison Chart (Line Chart)

# Recieved help from this YouTube video 
# [Make Beautiful Graphs in R: 5 Quick Ways to Improve ggplot2 Graphs](https://www.youtube.com/watch?v=qnw1xDnt_Ec)
# Libraries
library(ggplot2)
install.packages("ggthemes")
library(ggthemes)

aapi_linechart <- ggplot(incarceration_data, aes(x=aapi_jail_pop, y=year)) +
  geom_line() +
  labs(title = "AAPI Jailed Population across the United States",
       subtitle = "How do AAPI jail rates fluxuate over time?",
       x = "AAPI Jail Pop",
       y = "Year",
       color = "Region")
