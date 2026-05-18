ICU_babies_untestedRSV <- patient_outcomes_virus_correction %>% 
  filter(age_group == "0-1", ICU == 1, Virus == "RSV") %>% 
  select(IPP_sejour, season_admitted, month_name, Virus:correctedCommunity) %>% 
  filter(test02 + test36 == 0) %>% 
  pull(IPP_sejour)


this_ipp = ICU_babies_untestedRSV[1]
this_ipp = ICU_babies_untestedRSV[2]
this_ipp = ICU_babies_untestedRSV[3]
this_ipp = ICU_babies_untestedRSV[4]
this_ipp = ICU_babies_untestedRSV[5]
this_ipp = ICU_babies_untestedRSV[6]

data_sejours %>% 
  filter(IPP_sejour == this_ipp) %>% 
  select(Date_nais, date_ent_sejour, Date_ent_reaSI, duree_reasi, date_sor_sejour, DP, DR, mode_sor)
  
data_PCR %>% 
  filter(IPP_sejour == this_ipp)

data_bact %>% 
  filter(IPP_sejour == this_ipp)

