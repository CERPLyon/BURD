
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




