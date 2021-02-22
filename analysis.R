library(dplyr)
library(knitr)
library(tidyverse)

#Load CSV File
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#Gender Filter
gender_data <- incarceration %>%
  filter(county_name == "Clayton County")%>%
  select(year, black_female_prison_pop, black_male_prison_pop)

#Chart Supplement
combined_gender <- 
  pivot_longer(
  data = gender_data,
  cols = c(
    "black_female_prison_pop", "black_male_prison_pop"
  ),
  names_to = "gender",
  values_to = "value"
  )

#Make Chart
chart <- ggplot(combined_gender) + 
    geom_point(aes(x = year, y = value, group = gender, color = gender)) +
    labs(x= "Year", 
         y = "Gender Prison Population", 
         title = "Black Gender Prison Disparity Over Time")