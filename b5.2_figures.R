

# big bar plot

patient_outcomes_ageinc %>% 
  filter(!(seasonAdmitted %in% c("20/21", "21/22"))) %>% 
  group_by(Virus, age_group, Adjustment, burden) %>% 
  summarise(Inc1e5 = mean(Inc1e5, na.rm = T)) %>% 
  ggplot(aes(x = age_group, y = Inc1e5, fill = Adjustment)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(burden~Virus) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# two smaller barplots

patient_outcomes_ageinc %>% 
  filter(!(seasonAdmitted %in% c("20/21", "21/22"))) %>% 
  filter(burden %in% c("Emergency_only", "Hospital_admission")) %>% 
  group_by(Virus, age_group, Adjustment, burden) %>% 
  summarise(Inc1e5 = mean(Inc1e5, na.rm = T)) %>% 
  ggplot(aes(x = age_group, y = Inc1e5, fill = Adjustment)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(burden~Virus) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

patient_outcomes_ageinc %>% 
  filter(!(seasonAdmitted %in% c("20/21", "21/22"))) %>% 
  # filter(!(burden %in% c("Emergency_only"))) %>% 
  filter(!(burden %in% c("Emergency_only", "Hospital_admission"))) %>% 
  group_by(Virus, age_group, Adjustment, burden) %>% 
  summarise(Inc1e5 = mean(Inc1e5, na.rm = T)) %>% 
  ggplot(aes(x = age_group, y = Inc1e5, fill = Adjustment)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(burden~Virus) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

patient_outcomes_inc %>% 
  # filter(!(seasonAdmitted %in% c("20/21", "21/22"))) %>% 
  filter(burden %in% c("Emergency_only", "Hospital_admission")) %>% 
  group_by(Virus, age_group, Adjustment, burden) %>% 
  summarise(Inc1e5 = mean(Inc1e5, na.rm = T)) %>% 
  ggplot(aes(x = age_group, y = Inc1e5, fill = Adjustment)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(burden~Virus) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# proportion plot

virus_col = viridisLite::turbo(length(virus_vec_slash)) %>%
  setNames(virus_vec_slash)

patient_outcomes_inc %>% 
  filter(!(seasonAdmitted %in% c("20/21", "21/22"))) %>% 
  group_by(Virus, age_group, Adjustment, burden) %>% 
  summarise(Inc1e5 = mean(Inc1e5, na.rm = T)) %>% 
  ungroup %>% 
  filter(Adjustment %in% c("knownCommunity", "afterCorrection")) %>% 
  mutate(Adjustment = ifelse(Adjustment == "knownCommunity", "Unadjusted", "Adjusted") %>% factor(levels = c("Unadjusted", "Adjusted"))) %>% 
  ggplot(aes(x = Adjustment, y = Inc1e5, fill = Virus)) + geom_bar(stat = "identity", position = "fill") + 
  facet_wrap(.~burden) + 
  scale_fill_manual(values = virus_col) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "Proportion contribution", x = "") + 
  theme_bw()
  




make a patient_outcomes_inc master table
which then aggregates to age, season, month
and the catchment pop is added after this aggregation

ONE BY SEASON 
ONE BY MONTHS

THEN ONTO DEATH

Supplement
LATENT PARAMETERS
DECAY PLOTS
CLASSIFICATION PLOTS
Add in COMPARISON of PRAUC and BASELINE
IMPORTANCE
 
SOMETHING ABOUT DEATH(Mfh, Afh etc)

COMPARISON WITH Li 

 







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




