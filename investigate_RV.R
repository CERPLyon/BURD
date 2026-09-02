library(tidyverse)
tab = data_PCR %>% 
  group_by(IPP_sejour, testday, Virus) %>% 
  unique %>% 
  group_by(IPP_sejour, testday) %>% 
  summarise(Viruses = paste(sort(unique(Virus)), collapse = ".")) %>% 
  group_by(IPP_sejour) %>% 
  mutate(tV = paste0(testday, Viruses)) %>% 
  summarise(tVs = paste(tV, collapse = "_")) %>% 
  group_by(tVs) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

View(tab)


# look at diagnostic codes among those who die with high predicted rhinovirus, but no actually rhinovirus test 

death_rhino_unconfirmed <- patient_outcomes_virus_correction %>% 
  filter(Virus == "RVEV") %>% 
  filter(knownCommunity == 0) %>% 
  filter(Death == 1) %>% 
  filter(knownCommunityViruses == 0) %>% 
  arrange(-correctedCommunity)

death_rhino_unconfirmed$correctedCommunity %>% sort %>% plot
death_rhino_unconfirmed %>% 
  filter(correctedCommunity > 0.8)

death_rhino_unconfirmed %>% 
  select(IPP_sejour, age_group, correctedCommunity) %>% 
  left_join(diag_sejour_long %>% 
              select(IPP_sejour, position, code) %>% unique) %>% 
  filter(correctedCommunity > 0.8) %>% 
  left_join(cim10_ref) %>% 
  select(IPP_sejour, age_group, correctedCommunity, code, cim10_description) %>% 
  unique
  
.Last.value %>% View

