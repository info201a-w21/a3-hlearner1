library(dplyr)
library(knitr)
library(tidyverse)
library(maps)
library(ggplot2)
library(mapproj)
library(patchwork)

# Load CSV File
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# DPLYR Exploration 1
black_county <- incarceration %>%
  filter(year == max(year)) %>%
  filter(total_pop == max(total_pop)) %>%
  pull(state)

# DPLYR Exploration 2
max_county <- incarceration %>%
  filter(year == max(year)) %>%
  filter(state == "CA") %>%
  filter(total_pop == max(total_pop)) %>%
  pull(county_name)

# DPLYR Exploration 3
max_black_male_prison_year <- incarceration %>%
  filter(black_male_prison_pop == max(black_male_prison_pop, na.rm = TRUE)) %>%
  pull(year)

# DPLYR Exploration 4
max_black_female_prison_year <- incarceration %>%
  filter(black_female_prison_pop ==
    max(black_female_prison_pop, na.rm = TRUE)) %>%
  pull(year)

# DPLYR Exploration 5
max_latinx_pop <- incarceration %>%
  filter(latinx_prison_pop == max(latinx_prison_pop, na.rm = TRUE)) %>%
  pull(year)

# Gender Filter
gender_data <- incarceration %>%
  filter(county_name == "Clayton County") %>%
  select(year, black_female_prison_pop, black_male_prison_pop)

# Chart Combination
combined_gender <-
  pivot_longer(
    data = gender_data,
    cols = c(
      "black_female_prison_pop", "black_male_prison_pop"
    ),
    names_to = "gender",
    values_to = "value"
  )

# Make Trend over Time Chart
trend_over_time_chart <- ggplot(combined_gender) +
  geom_point(aes(x = year, y = value, group = gender, color = gender)) +
  labs(
    x = "Year",
    y = "Gender Prison Population",
    title = "Black Gender Prison Disparity Over Time"
  )

# Filter for Clayton County
clayton_county <- incarceration %>%
  filter(county_name == "Clayton County")


# Make Variable Comparison Chart
comparison_chart <- ggplot(clayton_county) +
  geom_point(aes(x = black_prison_adm_rate, y = total_prison_adm_rate)) +
  labs(
    title = "Total Prison Admission Rate vs Black Prison Admission Rate",
    x = "Black Prison Admission Rate",
    y = "Total Prison Admission Rate"
  )

# Get most recent data
black_male_pop <- incarceration %>%
  filter(year == max(year))

# Merge incarceration data and map data
county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

# Merge map data and filtered black_male_pop data
map_data <- county_shapes %>%
  left_join(black_male_pop, by = "fips") %>%
  filter(state == "GA", county_name != "Unknown")

# Create Blank Theme
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

# Create Map
black_jail_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color = "gray", size = .3
  ) +
  coord_map() +
  scale_fill_continuous(
    limits = c(0, max(map_data$black_jail_pop)),
    na.value = "white", low = "yellow", high = "red"
  ) +
  blank_theme +
  ggtitle("Black Male Jail Population in Georgia")
