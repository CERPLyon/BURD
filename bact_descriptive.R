data_sejours$IPP_sejour %>% n_distinct

data_bact_cleaned$IPP_sejour %>% n_distinct
82656/153039
data_bact_classified %>% 
  filter(!is.na(classification)) %>% 
  pull(IPP_sejour) %>% n_distinct

33516/82656


data_bact_classified %>% 
  mutate(Year = year(date_preleve)) %>% 
  group_by(Year, sample_type1) %>% 
  summarise(n = n_distinct(IPP_sejour)) %>% 
  ggplot(aes(x = Year, fill = sample_type1, y = n)) + geom_bar(stat = "identity", position = "dodge") + 
  theme_bw() + 
  labs(fill = "Type of sample", y = "Patient stays")

ggsave("output/plots/bact_descriptive_staysSample.png", width = 10, height = 6, units = "cm")
library(tidyverse)


data_bact_classified %>% 
  group_by(classification) %>% 
  mutate(appearances = n_distinct(IPP_sejour)) %>% 
  arrange(-appearances) %>% 
  filter(appearances > 1000) %>% 
  ungroup %>% 
  mutate(classification = fct_infreq(classification) %>% fct_rev) %>% 
  group_by(sample_type1, appearances, classification) %>% 
  summarise(n = n_distinct(IPP_sejour)) %>% 
  ungroup %>% 
  complete(sample_type1, nesting(appearances, classification), fill = list(n = 0)) %>% 
  filter(!is.na(classification), classification != "BACTERIA OTHER", !grepl("GENUS", classification)) %>% 
  ggplot(aes(x = n, y = classification, fill = sample_type1)) + geom_bar(position = "dodge", stat = "identity") + 
  theme_bw() + 
  labs(fill = "Type of sample", x = "Patient stays")

ggsave("output/plots/bact_descriptive_staysClass.png", width = 16, height = 10, units = "cm")


data_bact_classified %>% 
  filter(sample_type1 == "BPO") %>% 
  group_by(classification) %>% 
  mutate(appearances = n_distinct(IPP_sejour)) %>% 
  arrange(-appearances) %>% 
  filter(appearances > 2000) %>% 
  filter(!is.na(classification), classification != "BACTERIA OTHER", !grepl("GENUS", classification)) %>% 
  mutate(month_preleve = floor_date(date_preleve, unit = "month") + 14) %>% 
  group_by(classification, age_group, month_preleve) %>% 
  summarise(n = n_distinct(IPP_sejour)) %>% 
  ungroup %>% 
  complete(classification, age_group, month_preleve, fill = list(n = 0)) %>% 
  ggplot(aes(x = month_preleve, y = n, colour = classification)) + geom_line(linewidth = 1) + 
  facet_grid(age_group~., scales = "free_y") + 
  theme_bw() + 
  labs(colour = "Species", x = "Month", y = "Patient stays") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  
ggsave("output/plots/bact_descriptive_timeline.png", width = 30, height = 15, units = "cm")

