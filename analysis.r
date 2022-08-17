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
# Combine the amount from each year and store in the variable `ca_latinx_`

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

# Trends Over Time Chart (Line Chart)

library(ggplot2)

aapi_linechart <- ggplot(incarceration_data, aes(x=year, y=aapi_jail_pop)) +
  geom_line(aes(color = "AAPI")) +
  labs(x = "Year", y = "AAPI Jail Pop") +
  ggtitle("AAPI Jailed Population across the United States")

# Comparison Chart (Scatterplot)

states <- incarceration_data %>%
  group_by(year) %>%
  filter(state == "WA") %>%
  filter(aapi_jail_pop == aapi_jail_pop) %>%
  summarize(jail = sum(aapi_jail_pop))

aapi_scatterplot <- ggplot(states, aes(x = year, y = jail)) +
  geom_point(aes(color = jail)) +
  scale_color_continuous("AAPI Jail Population") +
  labs(x = "Year", y = "AAPI Jail Population in Washington") +
  ggtitle("AAPI Jail Population in Washington")

# US Map

library(usmap)
library(ggplot2)

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

aapi_state <- incarceration_data %>%
  group_by(state) %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(aapi_jail_pop == aapi_jail_pop) %>%
  filter(total_pop == total_pop) %>%
  mutate(total_pop / aapi_jail_pop) %>%
  summarize(
    pop = sum(aapi_jail_pop), total = max(total_pop),
    mutate = sum(total_pop / aapi_jail_pop)
  )

map <- plot_usmap(
  data = aapi_state, values = "pop", color = "black",
  name = "AAPI Jail Population"
) +
  coord_fixed(1) +
  blank_theme +
  scale_fill_gradientn(
    colours = c("white", "blue"),
    breaks = c(0, 10, 100, 1000),
    trans = "log10", name = "AAPI Jail Population"
  ) +
  labs(title = "The United States", subtitle = "AAPI Jail Population in
       2018", name = "AAPI Jail Population") +
  theme(legend.position = "left")
