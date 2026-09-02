library(tidyverse)

data_bact_classified
patient_outcomes_inc_master

patient_knownStatus
patient_outcomes_virus_correction

full_join(
  data_PCR_sejour_collapseSubtypes %>% 
    ungroup %>% 
    filter(resultatbis == "POSITIVE") %>% 
    transmute(IPP_alea, IPP_sejour, season_admitted, age_group, date_ent_sejour, Virus, posday_virus = testday) %>% 
    arrange(IPP_sejour, posday_virus) %>% 
    group_by(IPP_sejour) %>% 
    slice(1)
  ,
  data_bact_classified %>% 
    left_join(data_sejours %>% select(IPP_sejour, date_ent_sejour) %>% unique) %>% 
    filter(sample_type1 == "BPO", !is.na(classification)) %>% 
    transmute(IPP_alea, IPP_sejour, date_ent_sejour, sample_type1, classification, posday_bacteria = as.numeric(date_preleve - date_ent_sejour)) %>% 
    transmute(IPP_sejour, classification, posday_bacteria) %>% 
    arrange(IPP_sejour, posday_bacteria) %>% 
    group_by(IPP_sejour) %>% 
    slice(1)
) %>% 
  filter(!(season_admitted %in% c("20/21", "21/22"))) %>% 
  ungroup %>% 
  filter(!is.na(posday_virus), !is.na(posday_bacteria)) %>% 
  mutate(sequence = case_when(posday_virus == posday_bacteria ~ "Same", 
                              posday_virus < posday_bacteria - 14 ~ "Virus first (not coinfection)",
                              posday_virus > posday_bacteria + 14 ~ "Bacteria first (not coinfection)",
                              posday_virus < posday_bacteria ~ "Virus first", 
                              posday_virus > posday_bacteria ~ "Bacteria first", 
                              )) %>% 
  count(sequence)
  group_by(posday_virus, posday_bacteria) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = posday_virus, y = posday_bacteria, colour = n)) + geom_point()
