virus_col = viridisLite::turbo(length(virus_vec)) %>%
  setNames(virus_vec)

library(ISOweek)


# big bar plot for age
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

ggsave("output/plots/admission_incidence_age.png", width = 30, height = 16, units = "cm")


# big bar plot for season

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


# big bar plot for months

patient_outcomes_inc_master %>% 
  group_by(Virus, age_group, seasonAdmitted, monthAdmitted, Adjustment, type_sej, burden) %>% 
  summarise(n = sum(n)) %>% 
  left_join(catchmentPop, by = c("seasonAdmitted", "age_group", "type_sej")) %>% 
  mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
  filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  # filter(burden == "Hospital_admission") %>% 
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


patient_outcomes_inc_master %>% 
  group_by(Virus, age_group, seasonAdmitted, monthAdmitted, Adjustment, type_sej, burden) %>% 
  summarise(n = sum(n)) %>% 
  left_join(catchmentPop, by = c("seasonAdmitted", "age_group", "type_sej")) %>% 
  mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
  filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  # filter(burden == "Hospital_admission") %>% 
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

# proportion plot



patient_outcomes_inc_master %>% 
  filter(!(seasonAdmitted %in% c("20/21", "21/22"))) %>%
  group_by(Virus, Adjustment, type_sej, burden, seasonAdmitted) %>% 
  summarise(n = sum(n)) %>% 
  left_join(catchmentPop %>% group_by(seasonAdmitted, type_sej) %>% summarise(catchmentHosp = sum(catchmentHosp)), by = c("seasonAdmitted", "type_sej")) %>% 
  mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
  ungroup %>% 
  group_by(Virus, Adjustment, type_sej, burden) %>% 
  summarise(Inc1e5 = mean(Inc1e5, na.rm = T)) %>% 
  filter(Adjustment %in% c("knownCommunity", "afterCorrection")) %>% 
  mutate(Adjustment = ifelse(Adjustment == "knownCommunity", "Unadjusted", "Adjusted") %>% factor(levels = c("Unadjusted", "Adjusted"))) %>% 
  
  filter((burden %in% c("Emergency_only", "Hospital_admission", "Hospital_ICU")) | #this applies the 30 day death correction only to the adjusted numbers
           (burden == "Hospital_deathRaw" & Adjustment == "Unadjusted") |
           (burden == "Hospital_deathPlus30" & Adjustment == "Adjusted")) %>% 
  mutate(burden = fct_collapse(burden, Hospital_death = c("Hospital_deathRaw", "Hospital_deathPlus30"))) %>% 
  
  mutate(burden = fct_relabel(burden, .fun = ~gsub("_", "\n", .x))) %>% 
  ggplot(aes(x = Adjustment, y = Inc1e5, fill = Virus)) + geom_bar(stat = "identity", position = "fill") + 
  facet_grid(.~burden) + 
  scale_fill_manual(values = virus_col) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "Proportion contribution", x = "") + 
  theme_bw()


ggsave("output/plots/proportion_incidence.png", width = 25, height = 10, units = "cm")



pct_contribution_age <- patient_outcomes_inc_master %>% 
  # mutate(Virus = fct_rev(Virus)) %>% 
  group_by(Virus, age_group, Adjustment, type_sej, burden, seasonAdmitted) %>% 
  summarise(n = sum(n)) %>% 
  left_join(catchmentPop, by = c("seasonAdmitted", "age_group", "type_sej")) %>% 
  mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
  ungroup %>% 
  filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  filter(Adjustment %in% c("knownCommunity", "afterCorrection")) %>% 
  mutate(Adjustment = ifelse(Adjustment == "knownCommunity", "Unadjusted", "Adjusted") %>% factor(levels = c("Unadjusted", "Adjusted"))) %>% 
  # mutate(Adjustment = fct_rev(Adjustment)) %>% 
  group_by(Virus, age_group, Adjustment, seasonAdmitted) %>% #sum over emergency and hospital admissions
  summarise(Inc1e5 = sum(Inc1e5)) %>% 
  filter(!(seasonAdmitted %in% c("20/21", "21/22"))) %>% 
  group_by(Virus, age_group, Adjustment) %>% 
  summarise(Inc1e5 = mean(Inc1e5, na.rm = T)) %>% 
  group_by(age_group, Adjustment) %>% 
  mutate(p_inc = Inc1e5/sum(Inc1e5))


pct_contribution <- patient_outcomes_inc_master %>% 
  # mutate(Virus = fct_rev(Virus)) %>% 
  group_by(Virus, Adjustment, type_sej, burden, seasonAdmitted) %>% 
  summarise(n = sum(n)) %>% 
  left_join(catchmentPop %>% group_by(seasonAdmitted, type_sej) %>% summarise(catchmentHosp = sum(catchmentHosp)), by = c("seasonAdmitted", "type_sej")) %>% 
  mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
  ungroup %>% 
  filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  filter(Adjustment %in% c("knownCommunity", "afterCorrection")) %>% 
  mutate(Adjustment = ifelse(Adjustment == "knownCommunity", "Unadjusted", "Adjusted") %>% factor(levels = c("Unadjusted", "Adjusted"))) %>% 
  # mutate(Adjustment = fct_rev(Adjustment)) %>% 
  group_by(Virus, Adjustment, seasonAdmitted) %>% #sum over emergency and hospital admissions
  summarise(Inc1e5 = sum(Inc1e5)) %>% 
  filter(!(seasonAdmitted %in% c("20/21", "21/22"))) %>% 
  group_by(Virus, Adjustment) %>% 
  summarise(Inc1e5 = mean(Inc1e5, na.rm = T)) %>% 
  group_by(Adjustment) %>% 
  mutate(p_inc = Inc1e5/sum(Inc1e5)) %>% 
  mutate(age_group = "All ages")




pct_contrib1 <- ggplot() + 
  geom_bar(data = pct_contribution, aes(y = p_inc, x = Virus, fill = Adjustment), stat = "identity", position = "dodge") + 
  # geom_text(aes(label = paste0(round(p_inc*100, 1), "%")), nudge_x = 0.05) + 
  geom_text(data = pct_contribution %>% filter(Adjustment == "Unadjusted"), 
            aes(y = p_inc, x = Virus, colour = "Unadjusted", label = paste0(round(p_inc*100, 1), "%")), nudge_y = 0.02, nudge_x = -0.2, size = 1.5) + 
  geom_text(data = pct_contribution %>% filter(Adjustment == "Adjusted"), 
            aes(y = p_inc, x = Virus, colour = "Adjusted", label = paste0(round(p_inc*100, 1), "%")), nudge_y = 0.02, nudge_x = 0.2, size = 1.5) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values = c(Unadjusted = "#F8766D", Adjusted = "#00BFC4")) + 
  scale_colour_manual(values = c(Unadjusted = "#F8766D", Adjusted = "#00BFC4")) + 
  guides(colour = F, fill = F) + 
  facet_wrap(age_group~.) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "", y = "")

pct_contrib1

pct_contrib2 <- ggplot() + 
  geom_bar(data = pct_contribution_age, aes(y = p_inc, x = Virus, fill = Adjustment), stat = "identity", position = "dodge") + 
  # geom_text(aes(label = paste0(round(p_inc*100, 1), "%")), nudge_x = 0.05) + 
  geom_text(data = pct_contribution_age %>% filter(Adjustment == "Unadjusted"), 
            aes(y = p_inc, x = Virus, colour = "Unadjusted", label = paste0(round(p_inc*100, 1), "%")), nudge_y = 0.1, nudge_x = -0.2, size = 1.3) + 
  geom_text(data = pct_contribution_age %>% filter(Adjustment == "Adjusted"), 
            aes(y = p_inc, x = Virus, colour = "Adjusted", label = paste0(round(p_inc*100, 1), "%")), nudge_y = 0.1, nudge_x = 0.2, size = 1.3) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values = c(Unadjusted = "#F8766D", Adjusted = "#00BFC4")) + 
  scale_colour_manual(values = c(Unadjusted = "#F8766D", Adjusted = "#00BFC4")) + 
  guides(colour = F) + 
  facet_wrap(age_group~., ncol = 2) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "", y = "")

pct_contrib2


ggpubr::ggarrange(pct_contrib1, pct_contrib2, ncol = 2, widths = c(2, 4))

ggsave(paste0("output/plots/pct_contribution_plot.png"), width = 30, height = 20, units = "cm")




# citation(package = "xgboost")
# Comparison with Sentinel data
# https://www.sentiweb.fr/france/en/?page=table&maladie=25
# sentiweb.fr > Acute Respiratory Infection (ARI) > Download

senti <- read_csv("data/SentinelARI/inc-25-RDD-ds2.csv", skip = 1) %>% 
  filter(geo_insee == 84) %>% 
  mutate(YearWeek = gsub("([0-9]{4})([0-9]{2})", "\\1-W\\2-1", week)) %>% 
  mutate(week_beginning = ISOweek2date(YearWeek), 
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





# make a patient_outcomes_inc master table
# which then aggregates to age, season, month
# and the catchment pop is added after this aggregation

# ONE BY SEASON 
# ONE BY MONTHS

# COMPARISON WITH Senti

THEN ONTO DEATH

# Supplement
# LATENT PARAMETERS
# DECAY PLOTS
# CLASSIFICATION PLOTS
# IMPORTANCE
# Add in COMPARISON of PRAUC and BASELINE

SOMETHING ABOUT DEATH(Mfh, Afh etc)

# COMPARISON WITH Li 

 







patient_outcomes_virus_correction %>% 
  # filter(type_sej == "HOSPITALISATION") %>%
  mutate(Virus = case_when(Virus == "Rhinovirus" ~ "Enterovirus/Rhinovirus", 
                           Virus == "Enterovirus" ~ "Enterovirus/Rhinovirus", 
                           # Virus == "Rhinovirus" ~ "Enterovirus/Rhinovirus", 
                           T ~ Virus)) %>% 
  group_by(age_group, type_sej, Virus) %>% 
  summarise(n_knownCommunity = sum(knownCommunity), 
            n_probableCommunity = sum(probableCommunity), 
            # n_probableCommunity = sum(probableCommunity), 
            n_withLate = sum(correctedCommunity_late),
            n_withLateUntested = sum(correctedCommunity_lateUntested),
            n_afterCorrection = sum(correctedCommunity), 
  ) %>% 
  # mutate(p_Corrected = n_corrected/n_knownCommunity - 1) %>% 
  transmute(Virus, 
            age_group, 
            n_knownCommunity, 
            # n_probableCommunity,
            n_withLate, 
            n_withLateUntested, 
            # n_corrected = n_afterCorrection - n_knownCommunity, 
            n_afterCorrection) %>% 
  pivot_longer(-c("age_group", "type_sej", "Virus"), names_sep = "_", names_to = c(".value", "Adjustment")) %>% 
  mutate(Virus = factor(Virus, virus_vec_slash), 
         Adjustment = factor(Adjustment, levels = c("knownCommunity", "withLate", "withLateUntested", "afterCorrection"))) %>% 
  ggplot(aes(x = age_group, y = n, fill = Adjustment)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(type_sej~Virus) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









senstestadjusted_inc_typesej <- 
  senstestadjusted_burden %>% 
  mutate(Virus = factor(Virus, virus_order)) %>% 
  filter(Virus != c("InfluenzaNT")) %>% 
  mutate(Year = paste0("20", gsub(".*/", "", season_admitted)) %>% as.numeric) %>% 
  mutate(Emergency = type_sej == "EMERGENCY") %>% 
  group_by(Year, season_admitted, Virus, age_group) %>% 
  summarise(visit_Unadjusted = sum(knownCommunity), 
            admHosp_Unadjusted = sum(knownCommunity*(!Emergency)), 
            Emer_Unadjusted = sum(knownCommunity*(Emergency)), 
            icu_Unadjusted = sum(ICU*knownCommunity),
            mor_Unadjusted = sum(Death*knownCommunity),
            # adm_late = sum(infected_sensAdjusted), 
            visit_Adjusted = sum(infected_sensTestHospAnyconfAdjusted), 
            admHosp_Adjusted = sum(infected_sensTestHospAnyconfAdjusted*(!Emergency)), 
            Emer_Adjusted = sum(infected_sensTestHospAnyconfAdjusted*(Emergency)), 
            icu_Adjusted = sum(infected_sensTestHospAnyconfAdjusted*ICU),
            mor_Adjusted = sum(infected_sensTestHospAnyconfAdjusted*Death)
            # adm_AdjustedLate = sum(infected_sensAdjusted), 
            # adm_AdjustedLate = sum(infected_sensAdjusted), 
            # icu_adj = sum(infected_sensTestAdjusted*ICU), 
            # mor_adj = sum(infected_sensTestAdjusted*Death)
  ) %>% 
  left_join(catchment_rhone_interp %>% filter(sex == "MF"), by = c("age_group", "Year")) %>% 
  pivot_longer(-c("Year", "season_admitted", "age_group", "Virus", "sex", "Pop"), names_sep = "_", names_to = c("Outcome", "Adjustment"), values_to = "cases") %>%
  mutate(inc = cases/Pop*1e5) %>% 
  mutate(Adjustment = factor(Adjustment, levels = c("Unadjusted", "Adjusted")))


senstestadjusted_inc_typesej %>% 
  filter(Adjustment %in% c("Unadjusted", "Adjusted")) %>% 
  # filter(Outcome == "visit") %>% 
  mutate(age_group = factor(age_group, levels = age_vec)) %>% 
  mutate(Virus = fct_recode(Virus, `Enterovirus/\nRhinovirus` = "Enterovirus/Rhinovirus")) %>% 
  group_by(Virus, age_group, Adjustment, Outcome) %>% 
  summarise(inc = mean(inc)) %>% 
  filter(Outcome == "visit") %>% 
  ggplot(aes(x = age_group, fill = Adjustment, y = inc)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(.~Virus) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1.1)#, 
        # panel.grid = element_blank()
  ) + 
  labs(x = "Age group", 
       y = paste0("Hospital or emergency incidence\nper season per 100 000")
  )

ggsave(paste0("BURD/Inc_", "adm", "_age.png"), width = 25, height = 9, units = "cm")


senstestadjusted_inc_typesej %>% 
  filter(Adjustment %in% c("Unadjusted", "Adjusted")) %>% 
  # filter(Outcome == "visit") %>% 
  mutate(age_group = factor(age_group, levels = age_vec)) %>% 
  mutate(Virus = fct_recode(Virus, `Enterovirus/\nRhinovirus` = "Enterovirus/Rhinovirus")) %>% 
  group_by(Virus, age_group, Adjustment, Outcome) %>% 
  summarise(inc = mean(inc)) %>% 
  filter(Outcome != "visit") %>%
  mutate(Outcome = fct_recode(Outcome, Emergency = "Emer", 
                              `Hospital admission`= "admHosp", 
                              ICU = "icu", 
                              Mortality = "mor") %>% fct_relevel("Emergency", after = 0)) %>% 
  ggplot(aes(x = age_group, fill = Adjustment, y = inc)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(Outcome~Virus, scales = "free_y") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1.1)#, 
        # panel.grid = element_blank()
  ) + 
  labs(x = "Age group", 
       y = paste0("Incidence per season per 100 000")
  )

ggsave(paste0("BURD/Inc_", "adm", "_age_outcome.png"), width = 30, height = 12, units = "cm")




senstestadjusted_inc_typesej %>% 
  filter(Adjustment %in% c("Unadjusted", "Adjusted")) %>% 
  mutate(age_group = factor(age_group, levels = age_vec)) %>% 
  mutate(Virus = fct_recode(Virus, `Enterovirus/\nRhinovirus` = "Enterovirus/Rhinovirus")) %>% 
  filter(Outcome == "visit") %>% 
  ggplot(aes(x = season_admitted, fill = Adjustment, y = inc)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(age_group~Virus, scales = "free_y") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1.1)#, 
        # panel.grid = element_blank()
  ) + 
  labs(x = "Season (begins September)", 
       y = paste0("Hospital or emergency incidence\nper 100 000")
  )

ggsave(paste0("BURD/Inc_", "adm", "_age_season.png"), width = 30, height = 12, units = "cm")




