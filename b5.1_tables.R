# b5.1_tables


patient_outcomes_virus_correction %>% 
  # filter(type_sej == "HOSPITALISATION") %>% 
  # mutate(Virus = case_when(Virus == "Rhinovirus" ~ "Enterovirus/Rhinovirus", 
  #                          Virus == "Enterovirus" ~ "Enterovirus/Rhinovirus", 
  #                          # Virus == "Rhinovirus" ~ "Enterovirus/Rhinovirus", 
  #                          T ~ Virus)) %>% 
  group_by(Virus) %>% 
  summarise(n_knownCommunity = sum(knownCommunity), 
            n_probableCommunity = sum(probableCommunity), 
            # n_probableCommunity = sum(probableCommunity), 
            n_withLate = sum(correctedCommunity_late),
            n_withLateUntested = sum(correctedCommunity_lateUntested),
            n_afterCorrection = sum(correctedCommunity), 
  ) %>% 
  # WHY IS LATE CORRECTED BIGGER THAN PROBABLE COMMUNITY
  # mutate(p_Corrected = n_corrected/n_knownCommunity - 1) %>% 
  transmute(Virus, 
            n_knownCommunity, 
            n_probableCommunity,
            addedRaw_fromLate = n_withLate - n_knownCommunity, 
            addedRaw_fromUntested = n_withLateUntested - n_withLate, 
            n_addedRawTotal = n_withLateUntested, 
            # n_corrected = n_afterCorrection - n_knownCommunity, 
            n_afterCorrection) %>% 
  mutate(Virus = factor(Virus, virus_vec)) %>% 
  arrange(Virus) %>% 
  write_csv2(file = "output/tables/totals_corrected.csv")


patient_outcomes_virus_correction %>% 
  filter(knownCommunity == 1) %>% 
  group_by(Virus) %>% 
  summarise(Death = sum(Death), Death_plus30 = sum(Death_plus30)) %>% 
  write_csv2(file = "output/tables/deaths_corrected.csv")

deathRate_age_SNDS %>% 
  left_join(dp_deaths_FIRESP) %>% 
  mutate(deathRate_FIRESP = deaths_FIRESP/nDP) %>% 
  filter(nDP >= 40) %>% 
  transmute(DP = gsub("_", "", DP), age_group, deathRate_FIRESP, deathRate_within30) %>% 
  arrange(-deathRate_FIRESP) %>% 
  write_csv2(file = "output/tables/deathRates.csv")


