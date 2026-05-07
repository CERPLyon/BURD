library(tidyverse)
source("b0.R")



acute_day_limit = 3
suspected_day_limit = 7


patient_outcomes <- data_sejours %>% 
  group_by(IPP_sejour, age, age_group, 
           # date_ent_sejour, 
           # week_admitted, year_admitted, 
           month_admitted, 
           season_admitted, pandemic_status, type_sej) %>% 
  summarise(
  # across(c("age", "age_group" 
                     # "low_birth_weight", "premature", 
                     # "cardio", "pulmo", "cancer", "tabac", "immudef", "down"
                     # ), ~.x[1]), 
            Admission = 1, 
            across(c("oxy", "VMI", "VMNI", "ECMO"), ~any(.x %in% 1, na.rm = T) %>% as.numeric), 
            ICU = any(rea_SI %in% 1, na.rm = T) %>% as.numeric, 
            Death = any(mode_sor %in% "DEATH", na.rm = T) %>% as.numeric
            # , LoS = duree_sejour[1]
            ) %>% 
  ungroup %>% 
  left_join(data_sejours %>% 
              filter(rea_SI == 1) %>% 
              group_by(IPP_sejour) %>% 
              summarise(ICUday = min(as.numeric(Date_ent_reaSI - date_ent_sejour), na.rm = T))) %>% 
  mutate(month_name = lubridate::month(month_admitted, label = T))

#### known status ####

patient_knownStatus <- data_sejours %>% 
  select(IPP_sejour) %>% unique %>% 
  left_join(data_PCR_sejour_collapseSubtypes) %>% 
  select(IPP_sejour, Virus, resultatbis, testday) %>% 
  mutate(Virus = factor(Virus, levels = virus_order)) %>% 
  # filter(IPP_sejour == "10000_1") %>%
  group_by(IPP_sejour, Virus) %>% 
  summarise(test02 = as.numeric(any(testday < acute_day_limit & resultatbis %in% c("POSITIVE", "NEGATIVE"))), 
            test36 = as.numeric(any(testday >= acute_day_limit & testday < suspected_day_limit & resultatbis %in% c("POSITIVE", "NEGATIVE"))), 
            knownCommunity = as.numeric(any(testday < acute_day_limit & resultatbis %in% "POSITIVE")), 
            probableCommunity = as.numeric((knownCommunity == 0) & any(testday >= acute_day_limit & testday < suspected_day_limit & resultatbis %in% "POSITIVE")),
            nosocomial = as.numeric((knownCommunity == 0) & (probableCommunity == 0) & any(testday >= suspected_day_limit & resultatbis %in% "POSITIVE"))
  ) %>% 
  ungroup %>% 
  complete(IPP_sejour, Virus, fill = list(test02 = 0, test36 = 0, knownCommunity = 0, probableCommunity = 0, nosocomial = 0)) %>% 
  filter(!is.na(Virus))

# patient_knownStatus$IPP_sejour %>% table %>% table
# patient_knownStatus$Virus %>% table

#unadjusted burden

patient_outcomes %>% 
  # filter(type_sej == "HOSPITALISATION") %>% 
  left_join(patient_knownStatus %>% select(IPP_sejour, Virus, knownCommunity, probableCommunity), relationship = "one-to-many") %>% 
  group_by(Virus
           # , type_sej, ICU, Death
           ) %>% 
  summarise(n_knownCommunity = sum(knownCommunity), n_probableCommunity = sum(probableCommunity))

#### inferred from late testing ####

# sensitivity_fits <- read_csv(file = paste0("~/FIRESP/FIRESP_analysis/BURD/sensitivity_fits.csv"))
estStatus <- read_csv(file = paste0("output/estStatus.csv"))


lateCorrection_table <- patient_outcomes %>% 
  left_join(patient_knownStatus %>% select(IPP_sejour, Virus, test02, test36, knownCommunity, probableCommunity), relationship = "one-to-many") %>% 
  left_join(estStatus %>% transmute(IPP_sejour = patient_id, Virus, late_infected = p_infected), by = c("IPP_sejour", "Virus"), relationship = "one-to-one") %>% 
  mutate(Virus = factor(Virus, levels = virus_order)) %>% 
  mutate(correctedCommunity = case_when(knownCommunity == 1 ~ 1, #ruled in
                                        test02 == 1 & (knownCommunity == 0)  ~ 0, #ruled out
                                        (test36 == 1) & (!is.na(late_infected)) ~ late_infected, #inferred
                                        # (!is.na(p_infected)) ~ p_infected, 
                                        # (probableCommunity == 1) & (!is.na(p_infected)) ~ p_infected, 
                                        T ~ 0)) %>% 
  group_by(Virus
           # , type_sej, ICU, Death
  ) %>% 
  summarise(n_knownCommunity = sum(knownCommunity), n_probableCommunity = sum(probableCommunity), n_lateCorrect = sum(correctedCommunity)) %>% 
  mutate(lateCorrected = n_lateCorrect - n_knownCommunity) %>% 
  select(Virus, n_knownCommunity, n_probableCommunity, lateCorrected)


# inferred from un-testing
NDA_virus_prediction_sejour <- read_csv("output/NDA_virus_prediction_sejour.csv") %>% 
  mutate(Virus = ifelse(Virus == "EnterovirusRhinovirus", "Enterovirus/Rhinovirus", Virus))

mlPrediction_sejour <- NDA_virus_prediction_sejour %>% 
  filter(NDA_startDay < suspected_day_limit) %>% 
  group_by(IPP_sejour, Virus) %>% 
  # summarise(infected_rfPredicted = median(prediction))
  arrange(.by_group = T, NDA_startDay) %>% 
  dplyr::slice(1) %>% 
  transmute(IPP_sejour, Virus, infected_mlPredicted = prediction) %>% 
  ungroup

patient_outcomes_virus_correction <- patient_outcomes %>% 
  left_join(patient_knownStatus %>% select(IPP_sejour, Virus, test02, test36, knownCommunity, probableCommunity), relationship = "one-to-many") %>% 
  left_join(estStatus %>% transmute(IPP_sejour = patient_id, Virus, late_infected = p_infected), by = c("IPP_sejour", "Virus"), relationship = "one-to-one") %>% 
  left_join(mlPrediction_sejour %>% transmute(IPP_sejour, Virus, untested_infected = infected_mlPredicted), by = c("IPP_sejour", "Virus"), relationship = "one-to-one") %>% 
  mutate(Virus = factor(Virus, levels = virus_order)) %>% 
  filter(Virus != "InfluenzaNT") %>% 
  mutate(correctedCommunity_late = case_when(knownCommunity == 1 ~ 1, #if there's a positive test on days 0-2, it's a defined community case
                                        (test02 == 1) & (knownCommunity == 0) ~ 0, #if there's a negative test, it's a non-case
                                        (test36 == 1) ~ late_infected, #if there's a test on days 3-6, use the late testing analysis
                                        T ~ 0)) %>% 
  mutate(correctedCommunity_lateUntested = case_when(knownCommunity == 1 ~ 1, #if there's a positive test on days 0-2, it's a defined community case
                                        (test02 == 1) & (knownCommunity == 0) ~ 0, #if there's a negative test, it's a non-case
                                        (Virus == "SARS-CoV-2") & (season_admitted %in% season_vec[1:3]) ~ 0, #no SARS-CoV-2 before 2019/20
                                        (Virus == "hCoV") & (season_admitted %in% season_vec[4:9]) ~ 0, #no hCoV during or after 2019/20
                                        (test36 == 1) ~ late_infected, #if there's a test on days 3-6, use the late testing analysis
                                        T ~ untested_infected)) %>% 
  group_by(IPP_sejour) %>% 
  mutate(knownCommunityViruses = sum(knownCommunity), 
         sumCorrected = sum(correctedCommunity_lateUntested, na.rm = T), # sum across all corrected infection probabilities (can be >1)
         totalProbabilityInfected = 1 - prod(1 - correctedCommunity_lateUntested, na.rm = T)) %>% # total probability of being infected, across all viruses
  ungroup %>% 
  mutate(correctedCommunity = case_when(knownCommunityViruses > 0 ~ knownCommunity/knownCommunityViruses, #if any virus was confirmed in the first three days, then this case is attributed fractionally to each of the known viruses
                                        # knownCommunityViruses > 0 ~ knownCommunity, #alternatively, let that be a whole contribution to that virus
                                        sumCorrected == 0 ~ 0, # to avoid dividing by zero
                                        T ~ correctedCommunity_lateUntested*totalProbabilityInfected/sumCorrected # adjusts correction probabilities so that their sum is equal to the total probability of being infected
                                        )) 

# total overall probability of being infected, by any virus
patient_outcomes_virus_correction %>% 
  group_by(IPP_sejour) %>% 
  summarise(totalProbabilityInfected = 1 - prod(1 - correctedCommunity_lateUntested, na.rm = T))


patient_outcomes_virus_correction %>% 
  # filter(type_sej == "HOSPITALISATION") %>% 
  mutate(Virus = case_when(Virus == "Rhinovirus" ~ "Enterovirus/Rhinovirus", 
                           Virus == "Enterovirus" ~ "Enterovirus/Rhinovirus", 
                           # Virus == "Rhinovirus" ~ "Enterovirus/Rhinovirus", 
                           T ~ Virus)) %>% 
  group_by(Virus) %>% 
  summarise(n_knownCommunity = sum(knownCommunity), 
            # n_probableCommunity = sum(probableCommunity), 
            n_lateCorrect = sum(correctedCommunity_late),
            n_lateUntestedCorrect = sum(correctedCommunity_lateUntested),
            n_Correct = sum(correctedCommunity), 
            ) %>% 
  mutate(p_Corrected = n_Correct/n_knownCommunity - 1) %>% 
  transmute(Virus, n_knownCommunity, 
         lateCorrected = n_lateCorrect - n_knownCommunity, 
         untestedCorrected = n_Correct - n_lateCorrect) %>% 
  mutate(Virus = factor(Virus, virus_order)) %>% 
  arrange(Virus) %>% 
  View


patient_outcomes_virus_correction %>% 
  group_by(Virus
           , type_sej
           
           # , ICU
           # ,
           , Death
  ) %>% 
  # filter(is.na(correctedCommunity))
  summarise(n_knownCommunity = sum(knownCommunity), 
            # n_probableCommunity = sum(probableCommunity), 
            n_lateCorrect = sum(correctedCommunity_late),
            n_lateUntestedCorrect = sum(correctedCommunity_lateUntested),
            n_Correct = sum(correctedCommunity, na.rm = T)) %>% 
  mutate(p_Corrected = n_Correct/n_knownCommunity - 1) %>% 
  print(n = 100)



patient_outcomes_virus_correction %>% 
  # filter(type_sej == "HOSPITALISATION") %>% 
  mutate(Virus = case_when(Virus == "Rhinovirus" ~ "Enterovirus/Rhinovirus", 
                           Virus == "Enterovirus" ~ "Enterovirus/Rhinovirus", 
                           # Virus == "Rhinovirus" ~ "Enterovirus/Rhinovirus", 
                           T ~ Virus) %>% 
           factor(levels = virus_order)) %>% 
  group_by(age_group, Virus) %>% 
  summarise(n_knownCommunity = sum(knownCommunity), 
            # n_probableCommunity = sum(probableCommunity), 
            n_lateCorrect = sum(correctedCommunity_late),
            n_lateUntestedCorrect = sum(correctedCommunity_lateUntested),
            n_Correct = sum(correctedCommunity)) %>% 
  ungroup %>% 
  rename(n_Unadjusted = n_knownCommunity, n_lateAdjusted = n_lateCorrect, n_Adjusted = n_Correct) %>% 
  select(-n_lateUntestedCorrect) %>% 
  pivot_longer(-c("age_group", "Virus"), names_sep = "_", names_to = c(".value", "Adjustment")) %>% 
  ggplot(aes(x = age_group, y = n, fill = Adjustment)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(.~Virus) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


