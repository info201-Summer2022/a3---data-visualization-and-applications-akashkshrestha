# Akash Shrestha
# INFO 201
# Professor Deeb-Swihart
# TA: Alyson Yu
# 8/14/22
# Assignment 3: Incarceration Analysis

# Load dataset in
incarceration_data <- read.csv("incarceration_trends.csv")
# Load libraries
library("stringr")
library("dplyr")
library("ggplot2")

# 5 chosen summary values

# Which state had the highest total metropolitan jail population in the dataset?
# Stored the state in the variable 'highest_jail_state'

jail_highest_state <- incarceration_data %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = T)) %>%
  filter(metro_area == metro_area) %>%
  filter(year == max(year, na.rm = T)) %>%
  pull(state)

# What year had the highest total metropolitan male jail population from 1976 to 2018?
# Stored the year in the variable 'jail_highest_year'

jail_highest_year <- incarceration_data %>%
  filter(male_adult_jail_pop == max(male_adult_jail_pop, na.rm = T)) %>%
  filter(metro_area == metro_area) %>%
  pull(year)

# What state had the highest total metropolitan AAPI jail population in 2018?
# Stored the state in the variable 'aapi_highest_state

aapi_highest_state <- incarceration_data %>%
  filter(aapi_jail_pop == max(aapi_jail_pop, na.rm = T)) %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(metro_area == metro_area) %>%
  pull(state, aapi_jail_pop)

# What was the total metropolitan AAPI jail population in Washington State for all past years across the country?
# Stored the value in the variable 'wa_aapi_total_jail'

wa_aapi_jail <- incarceration_data %>%
  group_by(year) %>%
  filter(state == "WA") %>%
  filter(aapi_jail_pop == aapi_jail_pop) %>%
  summarize(sum(aapi_jail_pop))

wa_appi_total_jail <- sum(wa_aapi_jail)

# Find the total change in percent of metropolitan aapi jail incarceration from the past 30 years (1988-2018)?
# Stored the number in the variable 'aapi_percent_change'
aapi_percent_change <- incarceration_data %>%
  select(metro_area, aapi_jail_pop, state, year) %>%
  group_by(year) %>%
  filter(metro_area == metro_area) %>%
  summarise(aapi_jail_pop = sum(aapi_jail_pop, na.rm = TRUE)) %>%
  summarise(percent_changed = abs((aapi_jail_pop[year == 2018] - aapi_jail_pop[year == 1988])/aapi_jail_pop[year == 1988]))%>%
  mutate(answer = round(percent_changed, 4)) %>% 
  pull(answer) # run this specific line to see answer


# Trends Over Time Chart (Histogram/Line Chart)

# Load ggplot
library(ggplot2)

# Creates AAPI line chart
aapi_linechart <- ggplot(incarceration_data, aes(x=aapi_jail_pop, y=year)) +
  geom_line() +
  labs(title = "AAPI Jailed Population across the United States",
       subtitle = "How do AAPI jail rates fluxuate over time?",
       x = "AAPI Jail Pop",
       y = "Year",
       color = "Region")


# Comparison Chart (Line Chart)

# Recieved help from this YouTube video 
# [Make Beautiful Graphs in R: 5 Quick Ways to Improve ggplot2 Graphs](https://www.youtube.com/watch?v=qnw1xDnt_Ec)

# Set colors
myColors = c("#A6611A", "#DFC27D", "#6e6c6b", "#80CDC1", "#018571")

# Creates comparison chart
aapi_comp_chart <- incarceration_data %>%
  ggplot(aes(x = state, y = aapi_jail_pop, color = region)) +
  geom_line()
labs(
  color = "Region")

# US Map

# Load libraries
library(usmap)
library(dplyr)
library(ggplot2)

# Borrowed from the textbook [Programming Skills for Data Science](https://learning.oreilly.com/library/view/programming-skills-for/9780135159071/ch16.xhtml#sec16_4)
# Define a minimalist theme for maps
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

# Filter dataset to AAPI rates per state in 2018
incar_pop_2018 <- filter(incarceration_data, year == "2018")
aapi_pop_state_2018 <- select(incar_pop_2018, state, aapi_jail_pop)

# Creates US map
aapi_map <- plot_usmap(
  regions = c("states"),
  data = (aapi_pop_state_2018),
  values = "aapi_jail_pop",
  labels = FALSE,
  label_color = "black",
) +
  scale_fill_continuous( name = "AAPI Jail Pop") + 
  blank_theme

