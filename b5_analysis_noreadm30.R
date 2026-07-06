library(tidyverse)
source("b0.R")


deathRate_age_SNDS <- read_csv2(file = paste0("output/deathRate_age_SNDS.csv"))

load("~/ZoneCap/output/HCLcatchment_ageyear.RData", verbose = T)
load("~/ZoneCap/output/HCLcatchment_ageyear_Urg_ARA.RData", verbose = T)

catchmentPop <- HCLcatchment_ageyear_Urg_ARA %>% 
  rename(catchmentHosp = catchment_Urg_ARA) %>% 
  mutate(type_sej = "EMERGENCY") %>% 
  {rbind(filter(., Year == 2019) %>% mutate(Year = 2017), 
         filter(., Year == 2019) %>% mutate(Year = 2018), 
         filter(., Year == 2024) %>% mutate(Year = 2025), 
         .)} %>% 
  rbind(HCLcatchment_ageyear %>% mutate(type_sej = "HOSPITALISATION")) %>% 
  mutate(seasonAdmitted = as.numeric(substr(Year, 3, 4)) %>% {paste0(.-1, "/", .)})


acute_day_limit = 3
suspected_day_limit = 7

later_outcomes <-
  data_sejours %>% 
  group_by(IPP_alea) %>% 
  mutate(latest_discharge = ifelse(date_sor == max(date_sor), 1, 0)) %>% #is this the final discharge in our dataset?
  group_by(IPP_sejour) %>% 
  mutate(latest_discharge = max(latest_discharge)) %>% #this IPP_sejour is the final stay in our data
  ungroup %>% 
  left_join(deathRate_age_SNDS %>% transmute(age_group = factor(age_group, levels = age_vec), DP, deathRate_within30_minusExcess), by = c("age_group", "DP")) %>% 
  mutate(deathRate_within30_minusExcess = replace_na(deathRate_within30_minusExcess, 0)) %>% #REMOVE THIS IF WE HAVE ALL THE DPs represented!
  group_by(IPP_alea, IPP_sejour_noreadmin30, IPP_sejour, latest_discharge) %>% 
  summarise(ICU = any(rea_SI %in% 1, na.rm = T) %>% as.numeric, 
            Death = any(mode_sor %in% "DEATH", na.rm = T) %>% as.numeric, 
            deathRate_within30_minusExcess = max(deathRate_within30_minusExcess, na.rm = T)
  ) %>% 
  mutate(Death_plus30 = pmax(Death, latest_discharge*deathRate_within30_minusExcess)) %>% 
  group_by(IPP_sejour_noreadmin30) %>% 
  mutate(ICUlater = rev(cummax(rev(ICU))), 
         Deathlater = rev(cummax(rev(Death))), 
         Death_plus30later = rev(cummax(rev(Death_plus30)))
  ) %>% 
  select(IPP_alea, IPP_sejour_noreadmin30, IPP_sejour, latest_discharge, ICU, ICUlater, Death, Deathlater, Death_plus30, Death_plus30later)




patient_outcomes_noreadm <- data_sejours %>% 
  left_join(later_outcomes %>% select(IPP_sejour, Death_plus30) %>% unique) %>%
  group_by(IPP_alea, IPP_sejour_noreadmin30
           # age, age_group, 
           # month_admitted, season_admitted, pandemic_status, 
           # type_sej
           ) %>% 
  summarise(
    IPP_sejour = IPP_sejour[1], #keep only the first stay, for linking to virological results
    IPP_sejour_last = last(IPP_sejour), 
    age = age[1], 
    age_group = age_group[1], 
    # date_ent_sejour, 
    month_admitted = month_admitted[1], 
    season_admitted = season_admitted[1], 
    pandemic_status = pandemic_status[1], 
    
    type_sej = type_sej %>% unique %>% sort %>% last, 
      
    
    Admission = 1, 
    across(c("oxy", "VMI", "VMNI", "ECMO"), ~any(.x %in% 1, na.rm = T) %>% as.numeric), 
    # , 
    ICU = any(rea_SI %in% 1, na.rm = T) %>% as.numeric,
    Death = any(mode_sor %in% "DEATH", na.rm = T) %>% as.numeric, 
    Death_plus30 = max(Death_plus30)
    # ,
    # deathRate_within30_minusExcess = max(deathRate_within30_minusExcess*latest_discharge, na.rm = T)
    # , LoS = duree_sejour[1]
  ) %>% 
  left_join(later_outcomes %>% transmute(IPP_sejour_last = IPP_sejour, Death_plus30)) %>%
  
  # left_join(later_outcomes %>% select(IPP_sejour, latest_discharge, ICU, ICUlater, Death, Deathlater, Death_plus30, Death_plus30later)) %>% 
  # left_join(data_sejours %>% 
  #             filter(rea_SI == 1) %>% 
  #             group_by(IPP_sejour) %>% 
  #             summarise(ICUday = min(as.numeric(Date_ent_reaSI - date_ent_sejour), na.rm = T))) %>% 
  mutate(month_name = lubridate::month(month_admitted, label = T))



# patient_outcomes <- data_sejours %>% 
#   group_by(IPP_alea) %>% 
#   mutate(latest_discharge = ifelse(date_sor == max(date_sor), 1, 0)) %>% #is this the final discharge in our dataset?
#   group_by(IPP_sejour) %>% 
#   mutate(latest_discharge = max(latest_discharge)) %>% #this IPP_sejour is the final stay in our data
#   ungroup %>% 
#   left_join(deathRate_age_SNDS %>% transmute(age_group = factor(age_group, levels = age_vec), DP, deathRate_within30_minusExcess), by = c("age_group", "DP")) %>% 
#   group_by(IPP_sejour, age, age_group, 
#            # date_ent_sejour, 
#            # week_admitted, year_admitted, 
#            month_admitted, 
#            season_admitted, pandemic_status, type_sej) %>% 
#   summarise(
#     # across(c("age", "age_group" 
#     # "low_birth_weight", "premature", 
#     # "cardio", "pulmo", "cancer", "tabac", "immudef", "down"
#     # ), ~.x[1]), 
#     Admission = 1, 
#     across(c("oxy", "VMI", "VMNI", "ECMO"), ~any(.x %in% 1, na.rm = T) %>% as.numeric), 
#     ICU = any(rea_SI %in% 1, na.rm = T) %>% as.numeric, 
#     Death = any(mode_sor %in% "DEATH", na.rm = T) %>% as.numeric, 
#     deathRate_within30_minusExcess = max(deathRate_within30_minusExcess, na.rm = T)
#     # , LoS = duree_sejour[1]
#   ) %>% 
#   left_join(later_outcomes %>% select(IPP_sejour, ICUlater, Deathlater)) %>%
#   mutate(Death_plus30 = case_when(Death == 1 ~ 1, 
#                                   latest_discharge == 1 & is.finite(deathRate_within30_minusExcess) ~ deathRate_within30_minusExcess, 
#                                   T ~ 0)) %>% 
#   left_join(data_sejours %>% 
#               filter(rea_SI == 1) %>% 
#               group_by(IPP_sejour) %>% 
#               summarise(ICUday = min(as.numeric(Date_ent_reaSI - date_ent_sejour), na.rm = T))) %>% 
#   mutate(month_name = lubridate::month(month_admitted, label = T))


#### known status ####

patient_knownStatus <- data_sejours %>% #this stays the same as in the readmission analysis, because the table patient_outcomes_noreadm only includes first IPP_sejours
  select(IPP_sejour) %>% unique %>% 
  left_join(data_PCR_sejour_collapseSubtypes) %>% 
  select(IPP_sejour, Virus, resultatbis, testday) %>% 
  mutate(Virus = factor(Virus, levels = virus_vec)) %>% 
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
estStatus <- read_csv2(file = paste0("output/estStatus_age.csv"))

# inferred from un-testing
NDA_virus_prediction_sejour <- read_csv("output/NDA_virus_prediction_sejour.csv")

mlPrediction_sejour <- NDA_virus_prediction_sejour %>% 
  filter(NDA_startDay < suspected_day_limit) %>% 
  group_by(IPP_sejour, Virus) %>% 
  # summarise(infected_rfPredicted = median(prediction))
  arrange(.by_group = T, NDA_startDay) %>% 
  dplyr::slice(1) %>% 
  transmute(IPP_sejour, Virus, infected_mlPredicted = prediction, infected_mlPredictedLo = predictionLo, infected_mlPredictedHi = predictionHi) %>% 
  ungroup


patient_outcomes_noreadm_virus_correction <- patient_outcomes_noreadm %>% 
  left_join(patient_knownStatus %>% select(IPP_sejour, Virus, test02, test36, knownCommunity, probableCommunity), relationship = "one-to-many") %>% 
  left_join(estStatus %>% transmute(IPP_sejour = patient_id, Virus, 
                                    late_infected = p_infected, late_infected_lo = p_infected_lo, late_infected_hi = p_infected_hi), by = c("IPP_sejour", "Virus"), relationship = "one-to-one") %>% 
  left_join(mlPrediction_sejour %>% transmute(IPP_sejour, Virus, 
                                              untested_infected = infected_mlPredicted, untested_infected_lo = infected_mlPredictedLo, untested_infected_hi = infected_mlPredictedHi), by = c("IPP_sejour", "Virus"), relationship = "one-to-one") %>% 
  mutate(Virus = factor(Virus, levels = virus_vec)) %>% 
  filter(Virus != "InfluenzaNT") %>% 
  mutate(correctedCommunity_late = case_when(knownCommunity == 1 ~ 1, #if there's a positive test on days 0-2, it's a defined community case
                                             (test02 == 1) & (knownCommunity == 0) ~ 0, #if there's a negative test, it's a non-case
                                             (test36 == 1) ~ late_infected, #if there's a test on days 3-6, use the late testing analysis
                                             T ~ 0)) %>% 
  mutate(correctedCommunity_late_lo = case_when(knownCommunity == 1 ~ 1, #if there's a positive test on days 0-2, it's a defined community case
                                             (test02 == 1) & (knownCommunity == 0) ~ 0, #if there's a negative test, it's a non-case
                                             (test36 == 1) ~ late_infected_lo, #if there's a test on days 3-6, use the late testing analysis
                                             T ~ 0)) %>% 
  mutate(correctedCommunity_late_hi = case_when(knownCommunity == 1 ~ 1, #if there's a positive test on days 0-2, it's a defined community case
                                                (test02 == 1) & (knownCommunity == 0) ~ 0, #if there's a negative test, it's a non-case
                                                (test36 == 1) ~ late_infected_hi, #if there's a test on days 3-6, use the late testing analysis
                                                T ~ 0)) %>% 
  mutate(correctedCommunity_lateUntested = case_when(knownCommunity == 1 ~ 1, #if there's a positive test on days 0-2, it's a defined community case
                                                     (test02 == 1) & (knownCommunity == 0) ~ 0, #if there's a negative test, it's a non-case
                                                     (Virus == "SARS-CoV-2") & (season_admitted %in% season_vec[1:3]) ~ 0, #no SARS-CoV-2 before 2019/20
                                                     (test36 == 1) ~ late_infected, #if there's a test on days 3-6, use the late testing analysis
                                                     (Virus == "hCoV") & (season_admitted %in% season_vec[4:9]) ~ 0, #no hCoV during or after 2019/20
                                                     T ~ untested_infected)) %>% 
  mutate(correctedCommunity_lateUntested_lo = case_when(knownCommunity == 1 ~ 1, #if there's a positive test on days 0-2, it's a defined community case
                                                     (test02 == 1) & (knownCommunity == 0) ~ 0, #if there's a negative test, it's a non-case
                                                     (Virus == "SARS-CoV-2") & (season_admitted %in% season_vec[1:3]) ~ 0, #no SARS-CoV-2 before 2019/20
                                                     (test36 == 1) ~ late_infected_lo, #if there's a test on days 3-6, use the late testing analysis
                                                     (Virus == "hCoV") & (season_admitted %in% season_vec[4:9]) ~ 0, #no hCoV during or after 2019/20
                                                     T ~ untested_infected_lo)) %>% 
  mutate(correctedCommunity_lateUntested_hi = case_when(knownCommunity == 1 ~ 1, #if there's a positive test on days 0-2, it's a defined community case
                                                     (test02 == 1) & (knownCommunity == 0) ~ 0, #if there's a negative test, it's a non-case
                                                     (Virus == "SARS-CoV-2") & (season_admitted %in% season_vec[1:3]) ~ 0, #no SARS-CoV-2 before 2019/20
                                                     (test36 == 1) ~ late_infected_hi, #if there's a test on days 3-6, use the late testing analysis
                                                     (Virus == "hCoV") & (season_admitted %in% season_vec[4:9]) ~ 0, #no hCoV during or after 2019/20
                                                     T ~ untested_infected_hi)) %>% 
  
  group_by(IPP_sejour_noreadmin30) %>% 
  mutate(knownCommunityViruses = sum(knownCommunity), 
         sumCorrected = sum(correctedCommunity_lateUntested, na.rm = T), # sum across all corrected infection probabilities (can be >1)
         totalProbabilityInfected = 1 - prod(1 - correctedCommunity_lateUntested, na.rm = T)) %>% # total probability of being infected, across all viruses
  ungroup %>% 
  mutate(correctedCommunity = case_when(knownCommunityViruses > 0 ~ knownCommunity/knownCommunityViruses, #if any virus was confirmed in the first three days, then this case is attributed fractionally to each of the known viruses
                                        # knownCommunityViruses > 0 ~ knownCommunity, #alternatively, let that be a whole contribution to that virus
                                        sumCorrected == 0 ~ 0, # to avoid dividing by zero
                                        T ~ correctedCommunity_lateUntested/sumCorrected*totalProbabilityInfected)) %>% # adjusts correction probabilities so that their sum is equal to the total probability of being infected
  mutate(correctedCommunity_lo = case_when(knownCommunityViruses > 0 ~ knownCommunity/knownCommunityViruses, #if any virus was confirmed in the first three days, then this case is attributed fractionally to each of the known viruses
                                        # knownCommunityViruses > 0 ~ knownCommunity, #alternatively, let that be a whole contribution to that virus
                                        sumCorrected == 0 ~ 0, # to avoid dividing by zero
                                        T ~ correctedCommunity_lateUntested_lo/sumCorrected*totalProbabilityInfected)) %>% # adjusts correction probabilities so that their sum is equal to the total probability of being infected
  mutate(correctedCommunity_hi = case_when(knownCommunityViruses > 0 ~ knownCommunity/knownCommunityViruses, #if any virus was confirmed in the first three days, then this case is attributed fractionally to each of the known viruses
                                        # knownCommunityViruses > 0 ~ knownCommunity, #alternatively, let that be a whole contribution to that virus
                                        sumCorrected == 0 ~ 0, # to avoid dividing by zero
                                        T ~ correctedCommunity_lateUntested_hi/sumCorrected*totalProbabilityInfected)) %>% # adjusts correction probabilities so that their sum is equal to the total probability of being infected
  mutate(attribKnownCommunity = ifelse(knownCommunityViruses > 1, knownCommunity/knownCommunityViruses, knownCommunity)) 


# now work into incidences

patient_outcomes_noreadm_inc_master <- patient_outcomes_noreadm_virus_correction %>% 
  # mutate(month_name = month(month_admitted, label = T, abbr = T, locale = "EN")) %>% 
  # filter(type_sej == "HOSPITALISATION") %>%
  rename(seasonAdmitted = season_admitted, monthAdmitted = month_admitted) %>% 
  group_by(age_group, seasonAdmitted, monthAdmitted, Virus, type_sej) %>% 
  {rbind(summarise(., 
                   n_knownCommunityRaw = sum(knownCommunity), 
                   n_knownCommunity = sum(attribKnownCommunity), 
                   n_probableCommunity = sum(probableCommunity), 
                   n_withLate = sum(correctedCommunity_late),
                   n_withLateLo = sum(correctedCommunity_late_lo),
                   n_withLateHi = sum(correctedCommunity_late_hi),
                   n_withLateUntested = sum(correctedCommunity_lateUntested),
                   n_withLateUntestedLo = sum(correctedCommunity_lateUntested_lo),
                   n_withLateUntestedHi = sum(correctedCommunity_lateUntested_hi),
                   n_afterCorrection = sum(correctedCommunity),
                   n_afterCorrectionLo = sum(correctedCommunity_lo),
                   n_afterCorrectionHi = sum(correctedCommunity_hi)) %>% 
           mutate(burden = ifelse(type_sej == "HOSPITALISATION", "Hospital_admission", "Emergency_only")), 
         filter(., type_sej == "HOSPITALISATION", ICU == 1) %>% 
           summarise(n_knownCommunityRaw = sum(knownCommunity), 
                     n_knownCommunity = sum(attribKnownCommunity), 
                     n_probableCommunity = sum(probableCommunity), 
                     n_withLate = sum(correctedCommunity_late),
                     n_withLateLo = sum(correctedCommunity_late_lo),
                     n_withLateHi = sum(correctedCommunity_late_hi),
                     n_withLateUntested = sum(correctedCommunity_lateUntested),
                     n_withLateUntestedLo = sum(correctedCommunity_lateUntested_lo),
                     n_withLateUntestedHi = sum(correctedCommunity_lateUntested_hi),
                     n_afterCorrection = sum(correctedCommunity),
                     n_afterCorrectionLo = sum(correctedCommunity_lo),
                     n_afterCorrectionHi = sum(correctedCommunity_hi)) %>% 
           mutate(burden = "Hospital_ICU"), 
         filter(., type_sej == "HOSPITALISATION", Death == 1) %>% 
           summarise(n_knownCommunityRaw = sum(knownCommunity), 
                     n_knownCommunity = sum(attribKnownCommunity), 
                     n_probableCommunity = sum(probableCommunity), 
                     n_withLate = sum(correctedCommunity_late),
                     n_withLateLo = sum(correctedCommunity_late_lo),
                     n_withLateHi = sum(correctedCommunity_late_hi),
                     n_withLateUntested = sum(correctedCommunity_lateUntested),
                     n_withLateUntestedLo = sum(correctedCommunity_lateUntested_lo),
                     n_withLateUntestedHi = sum(correctedCommunity_lateUntested_hi),
                     n_afterCorrection = sum(correctedCommunity),
                     n_afterCorrectionLo = sum(correctedCommunity_lo),
                     n_afterCorrectionHi = sum(correctedCommunity_hi)) %>% 
           mutate(burden = "Hospital_deathRaw"), 
         # filter(., type_sej == "HOSPITALISATION") %>% 
         #   summarise(n_knownCommunity = sum(knownCommunity*Death), 
         #             n_probableCommunity = sum(probableCommunity*Death), 
         #             n_withLate = sum(correctedCommunity_late*Death),
         #             n_withLateUntested = sum(correctedCommunity_lateUntested*Death),
         #             n_afterCorrection = sum(correctedCommunity*Death)) %>% 
         #   mutate(burden = "Hospital_deathControl"), 
         filter(., type_sej == "HOSPITALISATION") %>% 
           summarise(n_knownCommunityRaw = sum(knownCommunity*Death_plus30), 
                     n_knownCommunity = sum(attribKnownCommunity*Death_plus30), 
                     n_probableCommunity = sum(probableCommunity*Death_plus30), 
                     n_withLate = sum(correctedCommunity_late*Death_plus30),
                     n_withLateLo = sum(correctedCommunity_late_lo*Death_plus30),
                     n_withLateHi = sum(correctedCommunity_late_hi*Death_plus30),
                     n_withLateUntested = sum(correctedCommunity_lateUntested*Death_plus30),
                     n_withLateUntestedLo = sum(correctedCommunity_lateUntested_lo*Death_plus30),
                     n_withLateUntestedHi = sum(correctedCommunity_lateUntested_hi*Death_plus30),
                     n_afterCorrection = sum(correctedCommunity*Death_plus30),
                     n_afterCorrectionLo = sum(correctedCommunity_lo*Death_plus30),
                     n_afterCorrectionHi = sum(correctedCommunity_hi*Death_plus30)) %>% 
           mutate(burden = "Hospital_deathPlus30")
  )} %>% 
  # mutate(p_Corrected = n_corrected/n_knownCommunity - 1) %>% 
  transmute(age_group, 
            seasonAdmitted, 
            monthAdmitted,
            type_sej,
            burden, 
            Virus, 
            n_knownCommunityRaw, 
            n_knownCommunity, 
            n_probableCommunity,
            n_withLate = n_withLate, 
            n_withLateLo = n_withLateLo,  
            n_withLateHi = n_withLateHi,  
            n_withLateUntested = n_withLateUntested,   
            n_withLateUntestedLo = n_withLateUntestedLo,   
            n_withLateUntestedHi = n_withLateUntestedHi,   
            n_afterCorrection = n_afterCorrection,   
            n_afterCorrectionLo = n_afterCorrectionLo,    
            n_afterCorrectionHi = n_afterCorrectionHi
            
            ) %>% 
  
  pivot_longer(-c("age_group", "seasonAdmitted", "monthAdmitted", "type_sej", "burden", "Virus"), names_sep = "_", names_to = c(".value", "Adjustment")) %>% 
  ungroup %>% 
  complete(age_group, nesting(seasonAdmitted, monthAdmitted), nesting(type_sej, burden), Virus, Adjustment, fill = list(n = 0)) %>% 
  mutate(n = ifelse((Virus == "SARS-CoV-2") & (seasonAdmitted %in% season_vec[1:3]), NA, n)) %>% 
  # mutate(n = ifelse((Virus == "hCoV") & !(seasonAdmitted %in% season_vec[1:3]), NA, n)) %>% 
  mutate(Virus = factor(Virus, virus_vec), 
         # month_name = factor(month_name, month.abb[c(9:12, 1:8)]),
         Adjustment = factor(Adjustment, levels = c("knownCommunityRaw", "knownCommunity", "probableCommunity", "withLate", "withLateLo", "withLateHi", "withLateUntested", "withLateUntestedLo", "withLateUntestedHi", "afterCorrection", "afterCorrectionLo", "afterCorrectionHi")), 
         burden = factor(burden, levels = c("Emergency_only", "Hospital_admission", "Hospital_ICU", "Hospital_deathRaw", "Hospital_deathControl", "Hospital_deathPlus30")))




# patient_outcomes_inc <- patient_outcomes_virus_correction %>% 
#   # filter(type_sej == "HOSPITALISATION") %>%
#   mutate(Virus = case_when(Virus == "Rhinovirus" ~ "Enterovirus/Rhinovirus", 
#                            Virus == "Enterovirus" ~ "Enterovirus/Rhinovirus", 
#                            # Virus == "Rhinovirus" ~ "Enterovirus/Rhinovirus", 
#                            T ~ Virus)) %>% 
#   rename(seasonAdmitted = season_admitted) %>% 
#   group_by(age_group, seasonAdmitted, type_sej, Virus) %>% 
#   {rbind(summarise(., n_knownCommunity = sum(knownCommunity), 
#                    n_probableCommunity = sum(probableCommunity), 
#                    # n_probableCommunity = sum(probableCommunity), 
#                    n_withLate = sum(correctedCommunity_late),
#                    n_withLateUntested = sum(correctedCommunity_lateUntested),
#                    n_afterCorrection = sum(correctedCommunity)) %>% 
#            mutate(burden = ifelse(type_sej == "HOSPITALISATION", "Hospital_admission", "Emergency_only")), 
#          filter(., type_sej == "HOSPITALISATION", ICU == 1) %>% 
#            summarise(n_knownCommunity = sum(knownCommunity), 
#                      n_probableCommunity = sum(probableCommunity), 
#                      # n_probableCommunity = sum(probableCommunity), 
#                      n_withLate = sum(correctedCommunity_late),
#                      n_withLateUntested = sum(correctedCommunity_lateUntested),
#                      n_afterCorrection = sum(correctedCommunity)) %>% 
#            mutate(burden = "Hospital_ICU"), 
#          filter(., type_sej == "HOSPITALISATION", Death == 1) %>% 
#            summarise(n_knownCommunity = sum(knownCommunity), 
#                      n_probableCommunity = sum(probableCommunity), 
#                      # n_probableCommunity = sum(probableCommunity), 
#                      n_withLate = sum(correctedCommunity_late),
#                      n_withLateUntested = sum(correctedCommunity_lateUntested),
#                      n_afterCorrection = sum(correctedCommunity)) %>% 
#            mutate(burden = "Hospital_death")
#   )} %>% 
#   # mutate(p_Corrected = n_corrected/n_knownCommunity - 1) %>% 
#   transmute(Virus, 
#             age_group, 
#             seasonAdmitted, 
#             type_sej, 
#             burden, 
#             n_knownCommunity, 
#             # n_probableCommunity,
#             n_withLate, 
#             n_withLateUntested, 
#             # n_corrected = n_afterCorrection - n_knownCommunity, 
#             n_afterCorrection) %>% 
#   pivot_longer(-c("age_group", "type_sej", "burden", "seasonAdmitted", "Virus"), names_sep = "_", names_to = c(".value", "Adjustment")) %>% 
#   ungroup %>% 
#   complete(Virus, age_group, seasonAdmitted, nesting(type_sej, burden), Adjustment, fill = list(n = 0)) %>% 
#   mutate(n = ifelse((Virus == "SARS-CoV-2") & (seasonAdmitted %in% season_vec[1:3]), NA, n)) %>% 
#   mutate(Virus = factor(Virus, virus_vec_slash), 
#          Adjustment = factor(Adjustment, levels = c("knownCommunity", "withLate", "withLateUntested", "afterCorrection")), 
#          burden = factor(burden, levels = c("Emergency_only", "Hospital_admission", "Hospital_ICU", "Hospital_death"))) %>% 
#   left_join(catchmentPop) %>% 
#   mutate(Inc1e5 = n/catchmentHosp*1e5)
  


# # total overall probability of being infected, by any virus
# patient_outcomes_virus_correction %>% 
#   group_by(IPP_sejour) %>% 
#   summarise(totalProbabilityInfected = 1 - prod(1 - correctedCommunity_lateUntested, na.rm = T))
# 
# 
# patient_outcomes_virus_correction %>% 
#   # filter(type_sej == "HOSPITALISATION") %>% 
#   mutate(Virus = case_when(Virus == "Rhinovirus" ~ "Enterovirus/Rhinovirus", 
#                            Virus == "Enterovirus" ~ "Enterovirus/Rhinovirus", 
#                            # Virus == "Rhinovirus" ~ "Enterovirus/Rhinovirus", 
#                            T ~ Virus)) %>% 
#   group_by(Virus) %>% 
#   summarise(n_knownCommunity = sum(knownCommunity), 
#             # n_probableCommunity = sum(probableCommunity), 
#             n_lateCorrect = sum(correctedCommunity_late),
#             n_lateUntestedCorrect = sum(correctedCommunity_lateUntested),
#             n_Correct = sum(correctedCommunity), 
#             ) %>% 
#   mutate(p_Corrected = n_Correct/n_knownCommunity - 1) %>% 
#   transmute(Virus, n_knownCommunity, 
#          lateCorrected = n_lateCorrect - n_knownCommunity, 
#          untestedCorrected = n_Correct - n_lateCorrect) %>% 
#   mutate(Virus = factor(Virus, virus_vec)) %>% 
#   arrange(Virus) %>% 
#   View
# 
# 
# patient_outcomes_virus_correction %>% 
#   group_by(Virus
#            , type_sej
#            
#            # , ICU
#            # ,
#            , Death
#   ) %>% 
#   # filter(is.na(correctedCommunity))
#   summarise(n_knownCommunity = sum(knownCommunity), 
#             # n_probableCommunity = sum(probableCommunity), 
#             n_lateCorrect = sum(correctedCommunity_late),
#             n_lateUntestedCorrect = sum(correctedCommunity_lateUntested),
#             n_Correct = sum(correctedCommunity, na.rm = T)) %>% 
#   mutate(p_Corrected = n_Correct/n_knownCommunity - 1) %>% 
#   print(n = 100)





