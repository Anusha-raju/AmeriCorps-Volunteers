amco = read.csv("/Users/rachelthomas/Desktop/DATS6101/Americorps Project/AmeriCorps-Volunteers/AmeriCorps_Participant_Demographics_Data.csv")
str(amco)


library(dplyr)
library(ggplot2)
#takes only the Race dem_cat
dem_cat_subset <- 'Race'
amco_race <- amco %>%
  filter(dem_cat == dem_cat_subset)
str(amco_race)

unique(amco_race$dem_cat)
unique(amco_race$geography)

summary(amco_race)
#takes only 2022
time_period_subset <- '2022'
amco_race_2022 <- amco_race %>%
  filter(time_period == time_period_subset)

#This removes all the summary program_office lines from the data set
amco_r2022_noALL <- amco_race_2022 %>%
  filter(!program_office %in% c("All AmeriCorps Members", "AmeriCorps Member Programs (no NCCC)", "AmeriCorps Seniors (All)"))
print(amco_r2022_noALL)

#This removes the Non-white summary results
amco_r2022_noNW <- amco_r2022_noALL %>%
  filter(!dem_group %in% c("Racially Diverse (Non-White)"))
print(amco_r2022_noNW)

amco_race_2022$dem_group <- as.factor(amco_race_2022$dem_group)
str(amco_race_2022)
summary(amco_race_2022)
table(amco_race_2022$dem_group)
print(amco_race_2022)
levels(amco_race_2022$dem_group)

amco_race_2022$program_office <- as.factor(amco_race_2022$program_office)
str(amco_race_2022)
levels(amco_race_2022$program_office)




#filter by state
state <- amco_r2022_noNW %>%
  filter(geography == "State")
print(state)

unique(state$program_office)
unique(state$dem_group)
unique(state$geography)

program_MSY_state <- state %>%
  group_by(program_office) %>%
  summarise (total_dem_group = sum(count_dem_group))
print(program_MSY_state)

race_program_state <- aggregate(count_dem_group ~ program_office + dem_group, data = state, sum)
print(race_program_state)

group_MSY_state <- aggregate(count_dem_group ~ dem_group, data = state, sum)
total_MSY_state <- sum(group_MSY_state$count_dem_group)
group_MSY_state$percentage <- (group_MSY_state$count_dem_group / total_MSY_state) * 100
print(group_MSY_state)

state_total <- state %>%
  group_by(state) %>%
  summarise (total_dem_group = sum(count_dem_group))
print(state_total)

race_state <- state %>%
  group_by(state, dem_group) %>%
  summarise (total_dem_group = sum(count_dem_group))
print(race_state)


#stacked bar chart
ggplot(race_state, aes(x = state, y = total_dem_group, fill = dem_group)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Demographic Groups by State", x = "State", y = "Total MSY") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



#filter by region
region <- amco_r2022_noNW %>%
  filter(geography == "Region")
print(region)

program_MSY_region <- region %>%
  group_by(program_office) %>%
  summarise (total_dem_group = sum(count_dem_group))
print(program_MSY_region)

race_program_region <- aggregate(count_dem_group ~ program_office + dem_group, data = region, sum)
print(race_program_region)

group_MSY_region <- aggregate(count_dem_group ~ dem_group, data = region, sum)
total_MSY_region <- sum(group_MSY_region$count_dem_group)
group_MSY_region$percentage <- (group_MSY_region$count_dem_group / total_MSY_region) * 100
print(group_MSY_region)

race_region <- region %>%
  group_by(service_region, dem_group) %>%
  summarise (total_dem_group = sum(count_dem_group))
print(race_region)

program_region <- region %>%
  group_by(service_region, program_office) %>%
  summarise (total_dem_group = sum(count_dem_group))
print(program_region)

ggplot(program_region, aes(x = service_region, y = total_dem_group, fill = program_office)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Demographic Groups by Program Office and Service Region", 
       x = "Service Region", 
       y = "Total MSY") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#filter by national
national <- amco_r2022_noNW %>%
  filter(geography == "National")
print(national)

program_MSY_national <- national %>%
  group_by(program_office) %>%
  summarise (total_dem_group = sum(count_dem_group))
print(program_MSY_national)

race_program <- national %>%
  group_by()

race_program_national <- aggregate(count_dem_group ~ program_office + dem_group, data = national, sum)
print(race_program_national)

group_MSY_national <- aggregate(count_dem_group ~ dem_group, data = national, sum)
total_MSY_national <- sum(group_MSY_national$count_dem_group)
group_MSY_national$percentage <- (group_MSY_national$count_dem_group / total_MSY_national) * 100
print(group_MSY_national)

#bar chart total MSY
ggplot(group_MSY_national, aes(x = dem_group, y = count_dem_group, fill = dem_group)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "AmeriCorps Participation of Race by Member Service Years (MSY)", x = "Race", y = "MSY") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") + 
  geom_text(aes(label = count_dem_group), vjust = 0, size = 3.5) +
  theme(axis.text.x = element_blank()) +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_blank(),
        axix.text.y = element_text(size = 10))

#bar chart w %MSY
ggplot(group_MSY_national, aes(x = dem_group, y = percentage, fill = dem_group)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "Race Distribution by Percentage of Member Service Years (MSY)", x = "Race", y = "MSY(%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") + 
  geom_text(aes(label = round(percentage, 1)), vjust = 0, size = 3) +
  theme(axis.text.x = element_blank()) +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, face = "bold", hjust = 0),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 6, angle = 90, vjust = -3),
        axix.text.y = element_text(size = 10))+
  scale_y_continuous(breaks = seq(0, 100, by = 15))


program_race_national <- national %>%
  group_by(program_office, dem_group) %>%
  summarise (total_dem_group = sum(count_dem_group))
print(program_race_national)

ggplot(program_race_national, aes(x = program_office, y = total_dem_group, fill = dem_group)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Demographic Groups by Program Office", 
       x = "Program Office", 
       y = "Total MSY") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#MSYs by race for each program office
#the Seniors offices do not report "other" race

nccc_race <- race_program_national %>%
  filter(program_office == "AmeriCorps NCCC")
print(nccc_race)

fgp_race <- race_program_national %>%
  filter(program_office == "AmeriCorps Seniors FGP")
print(fgp_race)

rsvp_race <- race_program_national %>%
  filter(program_office == "AmeriCorps Seniors RSVP")
print(rsvp_race)

scp_race <- race_program_national %>%
  filter(program_office == "AmeriCorps Seniors SCP")
print(scp_race)

sdp_race <- race_program_national %>%
  filter(program_office == "AmeriCorps Seniors SDP")
print(sdp_race)

san_race <- race_program_national %>%
  filter(program_office == "AmeriCorps State and National")
print(san_race)

vista_race <- race_program_national %>%
  filter(program_office == "AmeriCorps VISTA")
print(vista_race)

ph_race <- race_program_national %>%
  filter(program_office == "Public Health AmeriCorps")
print(ph_race)


#sum of all MSY by program office
program_MSY <- amco_r2022_noNW %>%
  group_by(program_office) %>%
  summarise (total_dem_group = sum(count_dem_group))
print(program_MSY)

#MSY by race for each program office
race_program <- aggregate(count_dem_group ~ program_office + dem_group, data = amco_r2022_noNW, sum)
print(race_program)

#sum MSY by race across all program offices
group_MSY <- aggregate(count_dem_group ~ dem_group, data = amco_r2022_noNW, sum)
total_MSY <- sum(group_MSY$count_dem_group)
group_MSY$percentage <- (group_MSY$count_dem_group / total_MSY) * 100
print(group_MSY)

#data set has Hispanic/Latino as ethnicity so not included here
#For member data, member service years [MSYs] (i.e., the number of days or 
#hours members are required to serve over the course of their term to fulfill 
#their service requirements) are summed and used in place of observed member 
#counts to account for potential differences in service time allotments 
#assigned to different demographic groups.(from AmeriCorps Demographic Analysis Procedure, 2023)

#bar chart with MSYs
ggplot(group_MSY, aes(x = dem_group, y = count_dem_group, fill = dem_group)) +
  geom_bar(stat = "identity") +
  labs(title = "AmeriCorps Participation of Race by Percent of Member Service Years (MSY)", x = "Race", y = "MSY") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") + 
  geom_text(aes(label = count_dem_group), vjust = -0.5, size = 5)

#bar chart w %MSY
ggplot(group_MSY, aes(x = dem_group, y = percentage, fill = dem_group)) +
  geom_bar(stat = "identity") +
  labs(title = "AmeriCorps Participation of Race by Percent of Member Service Years (MSY)", x = "Race", y = "Percent MSY") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") + 
  geom_text(aes(label = round(percentage, 1)), vjust = -0.5, size = 5) +
  theme(axis.text.x = element_blank())

#pie chart
ggplot(group_MSY, aes(x = "", y = percentage, fill = dem_group)) +
  geom_bar(stat = "identity", width = 1) +  # Use actual percentages
  coord_polar(theta = "y") +  # Convert bar chart to pie chart
  labs(title = "AmeriCorps Participation of Race by Percent of Member Service Years (MSY)", fill = "Race") +
  theme_void() +  # Removes background and axes
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 2)

#not particularly usefull
race_breakdown <- amco_race_2022 %>%
  group_by(program_office, dem_group) %>%
  summarise(count = n()) %>%
  arrange(program_office, dem_group)