
library(ezids)
americorps <- read.csv("/home/anusha/workspace/Data science/AmeriCorps-Volunteers/AmeriCorps_Participant_Demographics_Data.csv", header =TRUE)

names(americorps)


summary(americorps)


americorps$geography<-as.factor(americorps$geography)
americorps$dem_cat<-as.factor(americorps$dem_cat)
americorps$dem_group<-as.factor(americorps$dem_group)
americorps$program_office<-as.factor(americorps$program_office)
americorps$service_region<-as.factor(americorps$service_region)
americorps$state<-as.factor(americorps$state)
americorps$time_period<-as.factor(americorps$time_period)

library(dplyr)


diversity_summary <- americorps %>% group_by(program_office) %>%
  summarise(unique_participants = n_distinct(dem_group)) %>% 
  arrange(desc(unique_participants))


library(ggplot2)

ggplot(diversity_summary, aes(x = reorder(program_office, unique_participants), y = unique_participants)) +
  geom_bar(stat = "identity") +
  coord_flip() + # Flip coordinates for better readability
  labs(title = "Diversity of Participants by Program Office",
       x = "Program Office",
       y = "Number of Unique Participants") 
  theme_minimal()


#notes: the "Number of Unique Participants" would count how many different demographic groups are represented under each program_office

total_population <- sum(americorps$census_pop_total)
americorps <- americorps %>%
    mutate(proportion_pop = census_pop_total / total_population,
           proportion_count = count_total / sum(count_total))
  
americorps <- americorps %>%
    select(geography, dem_cat, proportion_pop, proportion_count)
  
unique_geographies <- unique(americorps$geography)
colors <- RColorBrewer::brewer.pal(n = length(unique_geographies), name = "Set1")


ggplot(americorps, aes(x = dem_cat, fill = geography)) +
  geom_bar(aes(y = proportion_pop), position = "dodge", stat = "identity", alpha = 0.5) +
  geom_bar(aes(y = proportion_count), position = "dodge", stat = "identity") +
  labs(title = "Comparison of Demographic Group Representation",
       y = "Proportion",
       x = "Demographic Group") +
  scale_fill_manual(values = colors) +
  theme_minimal()
  
