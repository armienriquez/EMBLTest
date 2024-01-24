library(tidyverse)
surveys <- read_csv("data_raw/portal_data_joined.csv")
str(surveys)

#Select only specific columns
select(surveys,plot_id, species_id, weight)

#Exclude specific columns
select(surveys,-record_id, -species_id)

#Filter specific data
filter(surveys, year == 1995, sex == "M")

surveys2 <- filter(surveys, weight < 5)
surveys_sml <- select(surveys2, species_id, sex, weight)

surveys_sml2 <- select(filter(surveys, weight <5), species_id, sex, weight)

surveys %>% 
  filter(weight<5) %>% 
  select (species_id, sex, weight)

surveys %>% 
  filter (year == 1995) %>% 
  select(year, sex, weight)

#Create a new column with weight in kg
surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg = weight/1000, weight_lb=weight_kg*2.2) %>% 
  view()

#Split - apply - combine

surveys %>% 
  filter(!is.na(sex)) %>% 
  group_by(sex) %>% 
  summarise(mean_weight = mean(weight, na.rm = T))
# the filter function filters for NA in the sex column. 
# Whereas the na.rm  = T removes NA of the weight column

surveys %>% 
  filter(!is.na(weight), !is.na(sex)) %>% 
  group_by(sex,species_id) %>% 
  summarise(mean_weight = mean(weight, na.rm = T)) %>% 
  view() %>% 
  print(n=15)

surveys %>% 
  filter(!is.na(weight), !is.na(sex)) %>% 
  group_by(sex,species_id) %>% 
  summarise(mean_weight = mean(weight, na.rm = T),min_weight = min(weight)) %>% 
  view()  

surveys %>% 
  filter(!is.na(weight), !is.na(sex)) %>% 
  group_by(sex,species_id) %>% 
  summarise(mean_weight = mean(weight, na.rm = T),min_weight = min(weight)) %>% 
  arrange(desc(min_weight)) %>% 
  view()  

# Count the number of observations
surveys_new <- surveys %>% 
  count(sex, species) %>% 
  arrange(species, desc(n))

#How many animals were caught in each plot_type surveyed
surveys %>% 
  count(plot_type) %>% 
  arrange(desc(n))

# Use group_by and summarise to find mean, min, 
# and max hindfoot length for each species
# Also add the number of observations
surveys %>% 
  filter(!is.na(hindfoot_length)) %>% 
  group_by(species_id) %>% 
  summarise(mean_hindfoot = mean(hindfoot_length), 
            min_hindfoot = min(hindfoot_length), 
            max_hindfoot = max(hindfoot_length), 
            n = n()) %>% 
  arrange(desc(mean_hindfoot)) %>% 
  view()

# What was the heaviest animal measured in each year? 
# Return the columns year, genus, species_id, and weight
surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(year) %>% 
  filter(weight == max(weight)) %>% 
  select (year, genus, species_id, weight) %>% 
  arrange (year) %>% 
  unique()
# The last line makes sure that only one instance is recorded
# for multiple observations with the maximum weight

#########
# Long to wide format

# This part extracts data in long format
surveys_gw <- surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(plot_id, genus) %>% 
  summarise(mean_weight = mean(weight))
view(surveys_gw)

str(surveys_gw)
# This part converts from long to wide format
surveys_wide <- surveys_gw %>% 
  pivot_wider(names_from = genus, values_from = mean_weight, values_fill = 0)

# Wide to long format
surveys_wide %>% 
  pivot_longer(names_to = "genus", values_to = "mean_weight", cols = -plot_id) %>% 
  view()

# Use pivot_longer() to create a dataset where we have a key column called
# measurement and a value column that takes on the value of either hindfoot_length
# or weight

# With this new data set (surveys_long), 
# calculate the mean of each measurement in each year
# for each different plot type
# then separate out the mean values into separate columns
# for hindfoot_length and weight using pivot_wider()

surveys_long <- surveys %>% 
  pivot_longer(names_to = "measurement", 
               values_to = "value",
               cols = c(hindfoot_length, weight)) %>% 
  view()

surveys_long %>% 
  group_by(year, measurement, plot_id) %>% 
  summarise(mean_value = mean(value, na.rm = T)) %>% 
  pivot_wider(names_from = measurement, values_from = mean_value) %>% 
  view()

# Write a new csv file
surveys_complete <- surveys %>%
  filter(!is.na(weight),
         !is.na(hindfoot_length),
         !is.na(sex))
write_csv(surveys_complete, file = "data/surveys_complete.csv")
  






