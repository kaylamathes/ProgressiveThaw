library(tidyverse)
library(ggplot2)
#####Upland vs lowland determination 

# 1. List all CSV files in your folder
landform <- list.files(path = "TransientThaw/Data/Counterfactual_landform_classification/", 
                    pattern = "*.csv", 
                    full.names = TRUE)

# 2. Read and combine them
AllFires <- landform %>% 
  map_df(~read_csv(.))


AllFire_summary <- AllFires%>%
  group_by(ID, landform) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(ID)%>%
  mutate(percent = (count / sum(count)))

AllFire_summary_sorted <- AllFire_summary %>%
  group_by(ID) %>%
  # Create a helper column that pulls the percent where landform is "0"
  mutate(sort_key = percent[landform == "0"]) %>%
  ungroup() %>%
  # Sort by that helper column in descending order
  arrange(desc(sort_key)) %>%
  # (Optional) Remove the helper column when done
  select(-sort_key)

AllFire_summary_sorted <- AllFire_summary_sorted%>%
  mutate(Landform = case_when(landform == "0" ~ "Upland", 
                              landform == "1" ~ "Lowland"))

AllFire_summary_sorted$Landform <- as.factor(AllFire_summary_sorted$Landform)


##Individual Fires 
ggplot(AllFire_summary_sorted, aes(y = reorder(ID, -percent * (Landform == "Upland")), 
                      x = percent, 
                      fill = Landform)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Upland" = "#fee090", 
                               "Lowland" = "#4575b4"
  )) + ylab("FireName") +theme_classic() +xlab("proportion")

ggsave("ProgressiveThaw/Output/Landform_individualfires.png")


##Total Boxplot 
ggplot(AllFire_summary_sorted, aes(x = Landform, y = percent, fill = Landform))+
  geom_boxplot() +
  scale_fill_manual(values = c("Upland" = "#fee090", 
                               "Lowland" = "#4575b4")) + 
  theme_classic() + ylab("portional of counterfactual area")

ggsave("ProgressiveThaw/Output/Landform_Boxplot.png")

