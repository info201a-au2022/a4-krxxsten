# Uploading the necessary packages and dataset 
library(tidyverse)
library(ggplot2)
library(dplyr)
install.packages("maps")
library(maps)
install.packages("ggpubr")
library(ggpubr)

incarceration_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
View(incarceration_df)

## Section 2  ---- 

### Finding the highest and lowest jail population between states of the variables 
# 'black_jail_pop' and 'white_jail_pop'

# Highest Black jail population between all states within the most recent year
highest_black_pop <- incarceration_df %>%
  group_by(state) %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  pull(state)

print(highest_black_pop)

# Lowest Black jail population between all states within the most recent year 
lowest_black_pop <- incarceration_df %>%
  group_by(state) %>%
  filter(black_jail_pop == min(black_jail_pop, na.rm = TRUE)) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  pull(state)
print(lowest_black_pop)

# Highest White jail population between all states within the most recent year
highest_white_pop <- incarceration_df %>%
  group_by(state) %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = TRUE))%>%
  filter(year == max(year, na.rm = TRUE)) %>%
  pull(state)
print(highest_white_pop)

# Lowest White jail population between all states within the most recent year 
lowest_white_pop <- incarceration_df %>%
  group_by(state) %>%
  filter(white_jail_pop == min(white_jail_pop, na.rm= TRUE)) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  pull(state)

print(lowest_white_pop)


### Average jail population across all countries by 2018 

tester <- incarceration_df %>%
  mean(black_jail_pop)

average_black_pop <- incarceration_df %>%
  filter(year == 2018) %>%
  summarise(average_black = mean(black_jail_pop, na.rm=TRUE)) %>%
  pull(average_black)

print(average_black_pop)

average_white_pop <- incarceration_df %>%
  filter(year == 2018) %>%
  summarise(average_white = mean(white_jail_pop, na.rm= TRUE)) %>%
  pull(average_white)

print(average_white_pop)

diff_jail_pop <- average_white_pop - average_black_pop
print(diff_jail_pop)

### Highest jail population in California

# Average Black jail population in CA
black_pop_in_ca <- incarceration_df %>%
  filter(state == "CA") %>%
  summarise(black_jail_pop = max(black_jail_pop, na.rm = TRUE)) %>%
  pull(black_jail_pop)

print(black_pop_in_ca) 
 
# Average White jail population in WA
white_pop_in_ca <- incarceration_df %>%
  filter(state == "CA") %>%
  summarise(white_jail_pop = max(white_jail_pop, na.rm = TRUE)) %>%
  pull(white_jail_pop)

print(white_pop_in_ca)
  
## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ...
#----------------------------------------------------------------------------#
# This function creates the dataframe including information needed for the barchart
library(ggplot2)

get_year_jail_pop <- function() {
  jail_df <- incarceration_df %>%
    select(year, total_jail_pop) %>%
    drop_na(total_jail_pop)
return(jail_df)   
}
View(get_year_jail_pop())

# This function returns the dataframe, which creates the chart 
plot_jail_pop_for_us <- function()  {
  jail_plot <- ggplot(get_year_jail_pop()) +
    geom_col(
      mapping = aes(x = year, y= total_jail_pop) 
    ) +
    labs(title = "Increase of Jail Population in U.S (1970-2018)", 
         x= "Year",
         y= "Total Jail Population")
  return(jail_plot)   
} 
print(plot_jail_pop_for_us())

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here 

# Creating vector of states for dataframe 
states_jail_pop <- c("WA", "OR", "CA")

# Data frame for vector 
get_jail_pop_by_states <- function(states) {
    state_pop <- incarceration_df %>%
      group_by(state, year) %>%
      filter(state %in% states_jail_pop) %>%
      summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
      select(year, state, total_jail_pop)
    return(state_pop)
}

View(get_jail_pop_by_states(states))

# Creating chart with data frame 

plot_jail_pop_by_states <- function(states) {
    state_plot <- ggplot(get_jail_pop_by_states(states)) +
    geom_line(
      mapping = aes(x = year, y= total_jail_pop) 
    ) +
    labs(title = "Growth of the U.S. prison population from 1970 to 2018 (By State)",
         x= "Year", 
         y= "Total Jail Population")
  return(state_plot)
}
print(plot_jail_pop_by_states(c("WA", "OR", "CA")))


#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#

# Plot that shows female jail rates over the years
female_plot <- incarceration_df %>%
  drop_na(female_jail_pop_rate) %>%
  ggplot(aes(x= year, y= female_jail_pop_rate)) +
  geom_col(fill = "lightpink")+
  labs(title = "Female Jail Rates Over the Years",
       x= "year", 
       y= "Female Jail Pop. Rate")
print(female_plot)

# Plot that shows male jail rates over the years 
male_plot <- incarceration_df %>%
  drop_na(male_jail_pop_rate) %>%
  ggplot(aes(x= year, y= male_jail_pop_rate)) +
  geom_col(fill = "lightblue")+
  labs(title= "Male Jail Rates over the Years",
       x= "Year", 
       y= "Male Jail Pop. Rate")
print(male_plot)

# Combining the charts together 
variable_comparison <- ggarrange(female_plot, male_plot, ncol= 1, nrow= 2)
print(variable_comparison)

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
#----------------------------------------------------------------------------#

## Load data frame ---- 

# Theme for the map 

theme <- theme_bw() +
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

# Creating my dataframes for my map 
gender_ratio <- incarceration_df %>%
  drop_na(female_jail_pop_rate, male_jail_pop_rate) %>%
  filter(state == "WA") %>%
  select(county_name, year, state, fips, 
         female_jail_pop_rate, male_jail_pop_rate) %>%
  mutate(
    female_male_jail_ratio = (female_jail_pop_rate/male_jail_pop_rate)
)

View(jail_ratio)

# Creating the data needed for my base map 
county <- map_data("county") %>%
  filter(region == "washington")  %>%
  unite(polyname, 
        region, 
        subregion,
        sep = ","
        ) %>%
  left_join(county.fips, by= "polyname")
View(county)

map_data <- county %>%
  left_join(gender_ratio, by= "fips") 

# My map 
map_chart <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(long, lat, group = group,
      fill = female_male_jail_ratio
    )
  ) +
  coord_map() +
  ggtitle("Female to Male Jail Ratio") +
  theme()

print(map_chart)




  

