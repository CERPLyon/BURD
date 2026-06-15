
patient_outcomes_inc_master %>% 
  filter(!(seasonAdmitted %in% c("20/21", "21/22"))) %>% 
  group_by(Virus, age_group, Adjustment, type_sej, burden, seasonAdmitted) %>% 
  summarise(n = sum(n)) %>% 
  left_join(catchmentPop, by = c("seasonAdmitted", "age_group", "type_sej")) %>% 
  mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
  group_by(Virus, age_group, Adjustment, type_sej, burden) %>% 
  summarise(Inc1e5 = mean(Inc1e5, na.rm = T)) %>% 
  filter(Adjustment %in% c("knownCommunity", "afterCorrection")) %>% 
  mutate(Adjustment = fct_recode(Adjustment, Unadjusted = "knownCommunity", Adjusted = "afterCorrection")) %>% 
  
  filter((burden %in% c("Emergency_only", "Hospital_admission", "Hospital_ICU")) | #this applies the 30 day death correction only to the adjusted numbers
           (burden == "Hospital_deathRaw" & Adjustment == "Unadjusted") |
           (burden == "Hospital_deathPlus30" & Adjustment == "Adjusted")) %>% 
  mutate(burden = fct_collapse(burden, Hospital_death = c("Hospital_deathRaw", "Hospital_deathPlus30"))) %>% 
  
  mutate(burden = fct_relabel(burden, .fun = ~gsub("_", "\n", .x))) %>% 
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  # filter(type_sej == "HOSPITALISATION") %>%
  ggplot(aes(x = age_group, y = Inc1e5, fill = Adjustment)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(burden~Virus
             , scales = "free_y"
  ) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Age group", y = "Incidence per 100 000 catchment population", fill = "")

ggsave("output/plots/admission_incidence_age.png", width = 30, height = 16, units = "cm", dpi = 600)



patient_outcomes_inc_master %>% 
  # filter(!(seasonAdmitted %in% c("20/21", "21/22"))) %>%
  group_by(Virus, Adjustment, type_sej, burden, seasonAdmitted) %>% 
  summarise(n = sum(n)) %>% 
  left_join(catchmentPop %>% group_by(seasonAdmitted, type_sej) %>% summarise(catchmentHosp = sum(catchmentHosp)), by = c("seasonAdmitted", "type_sej")) %>% 
  mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
  ungroup %>% 
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  filter(Adjustment %in% c("knownCommunity", "afterCorrection")) %>% 
  mutate(Adjustment = fct_recode(Adjustment, Unadjusted = "knownCommunity", Adjusted = "afterCorrection")) %>% 
  
  filter((burden %in% c("Emergency_only", "Hospital_admission", "Hospital_ICU")) | #this applies the 30 day death correction only to the adjusted numbers
           (burden == "Hospital_deathRaw" & Adjustment == "Unadjusted") |
           (burden == "Hospital_deathPlus30" & Adjustment == "Adjusted")) %>% 
  mutate(burden = fct_collapse(burden, Hospital_death = c("Hospital_deathRaw", "Hospital_deathPlus30"))) %>% 
  
  mutate(burden = fct_relabel(burden, .fun = ~gsub("_", "\n", .x))) %>% 
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  ggplot(aes(x = seasonAdmitted, y = Inc1e5, colour = Virus, linetype = Adjustment, group = Adjustment)) + 
  # geom_bar(stat = "identity", position = "dodge") + 
  geom_line()+ 
  facet_grid(burden~Virus
             , scales = "free_y"
  ) + 
  scale_colour_manual(values = virus_col) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Season admitted", y = "Incidence per 100 000 catchment population")

ggsave("output/plots/admission_incidence_season.png", width = 40, height = 13, units = "cm")





patient_outcomes_inc_master %>% 
  group_by(Virus, age_group, seasonAdmitted, monthAdmitted, Adjustment, type_sej, burden) %>% 
  summarise(n = sum(n)) %>% 
  left_join(catchmentPop, by = c("seasonAdmitted", "age_group", "type_sej")) %>% 
  mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  filter(burden == "Hospital_admission") %>%
  group_by(Virus, age_group, seasonAdmitted, monthAdmitted, Adjustment) %>% 
  summarise(Inc1e5 = sum(Inc1e5)) %>% 
  # ungroup %>% 
  # filter(burden == "Hospital_admission") %>% 
  # mutate(burden = fct_relabel(burden, .fun = ~gsub("_", "\n", .x))) %>% 
  
  
  filter(Adjustment %in% c("knownCommunity", "afterCorrection")) %>% 
  mutate(Adjustment = fct_recode(Adjustment, Unadjusted = "knownCommunity", Adjusted = "afterCorrection")) %>% 
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  ggplot(aes(x = monthAdmitted, y = Inc1e5, colour = Virus, linetype = Adjustment)) + 
  geom_line() + 
  # geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(age_group~.
             , scales = "free_y"
  ) + 
  scale_colour_manual(values = virus_col) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  labs(x = "Month admitted", y = "Incidence per 100 000 catchment population")

ggsave("output/plots/admission_incidence_month.png", width = 30, height = 15, units = "cm")


admission_incidence_age_CI_plot %>% 
  filter(Virus %in% virus_vec[1:4]) %>% 
  ggplot(aes(x = age_group, y = Inc1e5, colour = Adjustment, fill = Adjustment)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5) + 
  geom_errorbar(aes(ymin = Lo, ymax = Hi, colour = Adjustment), width = 0.2, position = position_nudge(x = 0.25)) +
  facet_grid(burden~Virus
             , scales = "free_y"
  ) + 
  scale_colour_manual(values = adjustment_col) +
  scale_fill_manual(values = adjustment_col) +
  theme_bw() + 
  guides(colour = F) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Age group", y = "Incidence per 100 000 catchment population", fill = "")

ggsave("output/plots/admission_incidence_age_CI_big3.png", width = 24, height = 16, units = "cm")

admission_incidence_age_CI_plot %>% 
  filter(Virus %in% virus_vec[5:9]) %>% 
  ggplot(aes(x = age_group, y = Inc1e5, colour = Adjustment, fill = Adjustment)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5) + 
  geom_errorbar(aes(ymin = Lo, ymax = Hi, colour = Adjustment), width = 0.2, position = position_nudge(x = 0.25)) +
  facet_grid(burden~Virus
             , scales = "free_y"
  ) + 
  scale_colour_manual(values = adjustment_col) +
  scale_fill_manual(values = adjustment_col) +
  theme_bw() + 
  guides(colour = F) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Age group", y = "Incidence per 100 000 catchment population", fill = "")

ggsave("output/plots/admission_incidence_age_CI_others.png", width = 30, height = 16, units = "cm")


patient_outcomes_inc_master %>% 
  group_by(Virus, age_group, seasonAdmitted, monthAdmitted, Adjustment, type_sej, burden) %>% 
  summarise(n = sum(n)) %>% 
  left_join(catchmentPop, by = c("seasonAdmitted", "age_group", "type_sej")) %>% 
  mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  filter(burden == "Hospital_admission") %>%
  group_by(Virus, age_group, seasonAdmitted, monthAdmitted, Adjustment) %>% 
  summarise(Inc1e5 = sum(Inc1e5)) %>% 
  # mutate(burden = fct_relabel(burden, .fun = ~gsub("_", "\n", .x))) %>% 
  
  filter(Adjustment %in% c("knownCommunity", "afterCorrection")) %>% 
  mutate(Adjustment = fct_recode(Adjustment, Unadjusted = "knownCommunity", Adjusted = "afterCorrection")) %>% 
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  ggplot(aes(x = monthAdmitted, y = Inc1e5, colour = age_group, linetype = Adjustment)) + 
  geom_line() + 
  # geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(Virus~.
             , scales = "free_y"
  ) + 
  # scale_colour_manual(values = virus_col) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  labs(x = "Month admitted", y = "Incidence per 100 000 catchment population")

ggsave("output/plots/admission_incidence_month_ageColour.png", width = 30, height = 15, units = "cm")


# Comparison with Sentinel data
# https://www.sentiweb.fr/france/en/?page=table&maladie=25
# https://www.sentiweb.fr/france/en/?page=maladies&mal=25 
#Definition: Visit (or teleconsultation) of a patient with an acute respiratory infection syndrome defined by an acute onset of fever (or fever feeling) with respiratory symptoms

# sentiweb.fr > Acute Respiratory Infection (ARI) > Download

senti <- read_csv("data/SentinelARI/inc-25-RDD-ds2.csv", skip = 1) %>% 
  filter(geo_insee == 84) %>% 
  mutate(YearWeek = gsub("([0-9]{4})([0-9]{2})", "\\1-W\\2-1", week)) %>% 
  mutate(week_beginning = ISOweek::ISOweek2date(YearWeek), 
         week_mid = week_beginning) %>% 
  transmute(week_mid, ARI1e5=inc100, ARIlo=inc100_low, ARIhi=inc100_up)



patient_outcomes_inc_master %>% 
  group_by(Virus, Adjustment, type_sej, burden, seasonAdmitted, monthAdmitted) %>% 
  summarise(n = sum(n)) %>% 
  left_join(catchmentPop %>% group_by(seasonAdmitted, type_sej) %>% summarise(catchmentHosp = sum(catchmentHosp)), by = c("seasonAdmitted", "type_sej")) %>% 
  mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
  ungroup %>% 
  filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  group_by(Virus, Adjustment, seasonAdmitted, monthAdmitted) %>% 
  summarise(Inc1e5 = sum(Inc1e5)) %>% 
  # mutate(burden = fct_relabel(burden, .fun = ~gsub("_", "\n", .x))) %>% 
  filter(Adjustment %in% c("knownCommunity", "afterCorrection")) %>% 
  mutate(Adjustment = fct_recode(Adjustment, Unadjusted = "knownCommunity", Adjusted = "afterCorrection")) %>% 
  group_by(monthAdmitted, Adjustment) %>% 
  mutate(totalInc1e5 = sum(Inc1e5, na.rm = T)) %>% 
  left_join(senti %>% 
              mutate(month = floor_date(week_mid, unit = "month") + 15) %>% 
              group_by(month) %>% 
              summarise(across(starts_with("ARI"), mean)) %>% 
              rename(monthAdmitted = month)) %>% 
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  ggplot(aes(x = monthAdmitted)) + 
  geom_area(aes(y = Inc1e5, fill = Virus)) + 
  # geom_line(aes(y = totalInc1e5, linetype = "Total Inc")) + 
  geom_line(aes(y = ARI1e5, linetype = "ARI in primary care")) + 
  geom_line(aes(y = ARIlo, linetype = "ARI 95% CI")) + 
  geom_line(aes(y = ARIhi, linetype = "ARI 95% CI")) + 
  # geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(Adjustment~.
             # , scales = "free_y"
  ) + 
  scale_fill_manual(values = virus_col) +
  scale_linetype_manual(values = c(`ARI in primary care` = "solid", `ARI 95% CI` = "dotted")) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  labs(x = "Month admitted", y = "Incidence per 100 000 catchment population", linetype = "")

ggsave("output/plots/area_vs_sentiweb.png", width = 25, height = 10, units = "cm")
