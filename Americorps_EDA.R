amco = read.csv("C:\\Users\\a_car\\OneDrive\\Documents\\GitHub\\AmeriCorps-Volunteers\\AmeriCorps_Participant_Demographics_Data.csv")
str(amco)


library(dplyr)
library(ggplot2)
library(tidyr)

dem_cat_subset <- 'Race'
amco_race <- amco %>%
  filter(dem_cat == dem_cat_subset)
str(amco_race)

summary(amco_race)

time_period_subset <- '2022'
amco_race_2022 <- amco_race %>%
  filter(time_period == time_period_subset)

amco_race_2022$dem_group <- as.factor(amco_race_2022$dem_group)
str(amco_race_2022)
summary(amco_race_2022)
table(amco_race_2022$dem_group)
print(amco_race_2022)
levels(amco_race_2022$dem_group)

amco_race_2022$program_office <- as.factor(amco_race_2022$program_office)
str(amco_race_2022)
levels(amco_race_2022$program_office)


race_breakdown <- amco_race_2022 %>%
  group_by(program_office, dem_group) %>%
  summarise(count = n()) %>%
  arrange(program_office, dem_group)


###
amco_r2022_noALL <- amco_race_2022 %>%
  filter(!program_office %in% c("All AmeriCorps Members", "AmeriCorps Member Programs (no NCCC)", "AmeriCorps Seniors (All)"))
print(amco_r2022_noALL)

amco_r2022_noNW <- amco_r2022_noALL %>%
  filter(!dem_group %in% c("Racially Diverse (Non-White)"))
print(amco_r2022_noNW)

race_breakdown <- amco_r2022_noNW %>%
  group_by(program_office, dem_group) %>%
  summarise(count = n()) %>%
  arrange(program_office, dem_group)
print(race_breakdown)

program_MSY <- amco_r2022_noNW %>%
  group_by(program_office) %>%
  summarise (total_dem_group = sum(count_dem_group))
print(program_MSY)

race_program <- aggregate(count_dem_group ~ program_office + dem_group, data = amco_r2022_noNW, sum)
print(race_program)

group_MSY <- aggregate(count_dem_group ~ dem_group, data = amco_r2022_noNW, sum)
total_MSY <- sum(group_MSY$count_dem_group)
group_MSY$percentage <- (group_MSY$count_dem_group / total_MSY) * 100
print(group_MSY)

#data set has Hispanic/Latino as ethinicity so not included here
#per 2020 census, White is ~61% of population. Black ~12%

ggplot(group_MSY, aes(x = dem_group, y = count_dem_group, fill = dem_group)) +
  geom_bar(stat = "identity") +
  labs(title = "Count of Demographic Groups", x = "Demographic Group", y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") + 
  geom_text(aes(label = count_dem_group), vjust = -0.5, size = 5)

ggplot(group_MSY, aes(x = dem_group, y = percentage, fill = dem_group)) +
  geom_bar(stat = "identity") +
  labs(title = "Overall AmeriCorps Participation by Percentage of Demographic Groups", x = "Demographic Group", y = "Percent") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") + 
  geom_text(aes(label = round(percentage, 1)), vjust = -0.5, size = 5) +
  theme(axis.text.x = element_blank())

ggplot(group_MSY, aes(x = "", y = percentage, fill = dem_group)) +
  geom_bar(stat = "identity", width = 1) +  # Use actual percentages
  coord_polar(theta = "y") +  # Convert bar chart to pie chart
  labs(title = "Percentage of Demographic Groups", fill = "Demographic Group") +
  theme_void() +  # Removes background and axes
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 5)


# Program with highest proportion of Diversity
perc_diversity <- amco_r2022_noALL %>%
  group_by(program_office, dem_group) %>%
  summarise(total_dem_group = sum(count_dem_group)) %>%
  mutate(perc_dem_group = total_dem_group / sum(total_dem_group) * 100) %>%
  filter(dem_group == "Racially Diverse (Non-White)")
perc_diversity <- perc_diversity %>%
  arrange(desc(perc_dem_group))
print(perc_diversity)

# Plot of Highest Proportion of Diversity
plot_program_diversity <- ggplot(perc_diversity, 
               aes(x = reorder(program_office, perc_dem_group), 
                   y = perc_dem_group, fill = program_office)) +
  geom_bar(stat = "identity") +
  labs(title = "Programs with Highest Proportion of Diversity", 
       x = "Program Office", y = "Percentage") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = round(perc_dem_group, 1)), vjust = -0.5, size = 5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

print(plot_program_diversity)

# Program with highest amount of diversity per state
perc_diversity_state_dem <- amco_r2022_noALL %>%
  group_by(state, dem_group) %>%
  summarise(total_dem_group = sum(count_dem_group)) %>%
  mutate(perc_dem_group = total_dem_group / sum(total_dem_group) * 100) %>%
  filter(dem_group == "Racially Diverse (Non-White)")
perc_diversity_state_dem <- perc_diversity_state_dem %>%
  arrange(desc(perc_dem_group))
group_perc_diversity_state_dem <- perc_diversity_state_dem 
group_by(state, program_office) 
summarise(total_dem_group = sum(total_dem_group)) 
arrange(desc(total_dem_group))
print(group_perc_diversity_state_dem[1:15, ])

# Plot of Highest Proportion of Diversity per State
plot_state_diversity <- group_perc_diversity_state_dem %>%
  arrange(desc(perc_dem_group)) %>%
  head(8) %>%
  ggplot(aes(x = reorder(state, perc_dem_group),
             y = perc_dem_group, fill = state)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 8 States with Highest Proportion of Diversity",
       x = "State", y = "Percentage") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = round(perc_dem_group, 1)), vjust = -1, size = 5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

print(plot_state_diversity)
