

# sensitivity_fits <- read_csv(file = paste0("~/FIRESP/FIRESP_analysis/BURD/sensitivity_fits.csv"))
estStatus <- read_csv2(file = paste0("output/estStatus_age.csv"))

# inferred from un-testing
NDA_virus_prediction_sejour <- read_csv("output/NDA_virus_prediction_sejour.csv")
NDA_virus_prediction_noSpecific_sejour <- read_csv("output/NDA_virus_prediction_sejour_noSpecific.csv")
NDA_virus_prediction_strictNDA_sejour <- read_csv("output/NDA_virus_prediction_sejour_strictNDA.csv")

make_mlPrediction <- function(tib){
  tib %>% 
    filter(NDA_startDay < suspected_day_limit) %>% 
    group_by(IPP_sejour, Virus) %>% 
    # summarise(infected_rfPredicted = median(prediction))
    arrange(.by_group = T, NDA_startDay) %>% 
    dplyr::slice(1) %>% 
    transmute(IPP_sejour, Virus, infected_mlPredicted = prediction, infected_mlPredictedLo = predictionLo, infected_mlPredictedHi = predictionHi) %>% 
    ungroup
}

mlPrediction_sejour <- NDA_virus_prediction_sejour %>% make_mlPrediction
mlPrediction_noSpecific_sejour <- NDA_virus_prediction_noSpecific_sejour %>% make_mlPrediction
mlPrediction_strictNDA_sejour <- NDA_virus_prediction_strictNDA_sejour %>% make_mlPrediction %>% 
  filter(!is.na(Virus))



make_pat_out_vir_cor <- function(tib, this_mlPrediction){
  tib %>% 
    left_join(patient_knownStatus %>% select(IPP_sejour, Virus, test02, test36, knownCommunity, probableCommunity), relationship = "one-to-many") %>%
    left_join(estStatus %>% transmute(IPP_sejour = patient_id, Virus,
                                      late_infected = p_infected, late_infected_lo = p_infected_lo, late_infected_hi = p_infected_hi), by = c("IPP_sejour", "Virus"), relationship = "one-to-one") %>%
    left_join(this_mlPrediction %>% transmute(IPP_sejour, Virus,
                                              untested_infected = infected_mlPredicted, untested_infected_lo = infected_mlPredictedLo, untested_infected_hi = infected_mlPredictedHi), by = c("IPP_sejour", "Virus"), relationship = "one-to-one") %>%
    mutate(Virus = factor(Virus, levels = virus_vec)) %>% 
    filter(Virus != "InfluenzaNT") %>% 
    filter(Virus != "hCoV") %>% 
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
    
    group_by(IPP_sejour) %>% 
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
}

patient_outcomes_virus_correction <- make_pat_out_vir_cor(patient_outcomes, mlPrediction_sejour)
patient_outcomes_noSpecific_virus_correction <- make_pat_out_vir_cor(patient_outcomes, mlPrediction_noSpecific_sejour)
patient_outcomes_strictNDA_virus_correction <- make_pat_out_vir_cor(patient_outcomes, mlPrediction_strictNDA_sejour)

make_pat_out_inc_master <- function(tib){
  tib %>% 
    filter(knownCommunityViruses <= 1) %>% 
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
}

patient_outcomes_inc_master <- make_pat_out_inc_master(patient_outcomes_virus_correction)
patient_outcomes_noSpecific_inc_master <- make_pat_out_inc_master(patient_outcomes_noSpecific_virus_correction)
patient_outcomes_strictNDA_inc_master <- make_pat_out_inc_master(patient_outcomes_strictNDA_virus_correction)

# patient_outcomes_inc_master %>% 
# patient_outcomes_noSpecific_inc_master %>% 
patient_outcomes_strictNDA_inc_master %>% 
  filter(Virus == "InfluenzaA", Adjustment == "knownCommunity", burden %in% c("Emergency_only", "Hospital_admission")) %>% pull(n) %>% sum


patient_outcomes_virus_correction %>% filter(Virus == "InfluenzaA") %>% pull(knownCommunity) %>% sum
patient_outcomes_noSpecific_virus_correction %>% filter(Virus == "InfluenzaA") %>% pull(knownCommunity) %>% sum
patient_outcomes_strictNDA_virus_correction %>% filter(Virus == "InfluenzaA") %>% pull(knownCommunity) %>% sum
