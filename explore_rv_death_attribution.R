

patient_outcomes_virus_correction %>% 
  filter(Virus == "RVEV") %>% 
  filter(Death == 1) %>% 
  filter(knownCommunity == 1) %>%
  # filter(knownCommunity == 0, correctedCommunity > 0.8) %>%
  select(IPP_sejour, age, season_admitted, correctedCommunity) %>% 
  # left_join(diag_sejour_long %>% group_by(IPP_sejour) %>% summarise(ICD10 = code %>% unique %>% sort %>% paste(collapse = "_")))
  left_join(diag_sejour_long %>% select(IPP_sejour, NDA_pseudo, code) %>% unique %>% arrange(code)) %>% 
  left_join(cim10_ref) %>% 
  group_by(code, cim10_description) %>% 
  summarise(n = n_distinct(IPP_sejour)) %>% 
  arrange(-n)
  
