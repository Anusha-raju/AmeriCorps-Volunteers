amco = read.csv("/Users/rachelthomas/Desktop/DATS6101/Americorps Project/AmeriCorps-Volunteers/AmeriCorps_Participant_Demographics_Data.csv")
str(amco)


library(dplyr)
library(ggplot2)
#takes only the Race dem_cat
dem_cat_subset <- 'Race'
amco_race <- amco %>%
  filter(dem_cat == dem_cat_subset)
str(amco_race)

summary(amco_race)
#takes only 2022
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

#not particularly usefull
race_breakdown <- amco_race_2022 %>%
  group_by(program_office, dem_group) %>%
  summarise(count = n()) %>%
  arrange(program_office, dem_group)

#This removes all the summary program_office lines from the data set
amco_r2022_noALL <- amco_race_2022 %>%
  filter(!program_office %in% c("All AmeriCorps Members", "AmeriCorps Member Programs (no NCCC)", "AmeriCorps Seniors (All)"))
print(amco_r2022_noALL)

#This removes the Non-white summary results
amco_r2022_noNW <- amco_r2022_noALL %>%
  filter(!dem_group %in% c("Racially Diverse (Non-White)"))
print(amco_r2022_noNW)

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

#MSYs by race for each program office
#the Seniors offices do not report "other" race

nccc_race <- race_program %>%
  filter(program_office == "AmeriCorps NCCC")
print(nccc_race)

fgp_race <- race_program %>%
  filter(program_office == "AmeriCorps Seniors FGP")
print(fgp_race)

rsvp_race <- race_program %>%
  filter(program_office == "AmeriCorps Seniors RSVP")
print(rsvp_race)

scp_race <- race_program %>%
  filter(program_office == "AmeriCorps Seniors SCP")
print(scp_race)

sdp_race <- race_program %>%
  filter(program_office == "AmeriCorps Seniors SDP")
print(sdp_race)

san_race <- race_program %>%
  filter(program_office == "AmeriCorps State and National")
print(san_race)

vista_race <- race_program %>%
  filter(program_office == "AmeriCorps VISTA")
print(vista_race)

ph_race <- race_program %>%
  filter(program_office == "Public Health AmeriCorps")
print(ph_race)