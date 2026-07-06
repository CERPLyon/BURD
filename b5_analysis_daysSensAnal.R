#### inferred from late testing ####

patient_knownStatus_days <- data_sejours %>% 
  select(IPP_sejour) %>% unique %>% 
  left_join(data_PCR_sejour_collapseSubtypes) %>% 
  select(IPP_sejour, Virus, resultatbis, testday) %>% 
  mutate(Virus = factor(Virus, levels = virus_vec)) %>% 
  cross_join(expand.grid(list(acute_day_limit = 1:5, suspected_day_limit = c(7, 10)))) %>% 
  # filter(IPP_sejour == "10000_1") %>%
  group_by(IPP_sejour, Virus, acute_day_limit, suspected_day_limit) %>% 
  summarise(testADM = as.numeric(any(testday < acute_day_limit & resultatbis %in% c("POSITIVE", "NEGATIVE"))), 
            testSUSP = as.numeric(any(testday >= acute_day_limit & testday < suspected_day_limit & resultatbis %in% c("POSITIVE", "NEGATIVE"))), 
            knownCommunity = as.numeric(any(testday < acute_day_limit & resultatbis %in% "POSITIVE")), 
            probableCommunity = as.numeric((knownCommunity == 0) & any(testday >= acute_day_limit & testday < suspected_day_limit & resultatbis %in% "POSITIVE")),
            nosocomial = as.numeric((knownCommunity == 0) & (probableCommunity == 0) & any(testday >= suspected_day_limit & resultatbis %in% "POSITIVE"))
  ) %>% 
  ungroup %>% 
  complete(IPP_sejour, Virus, acute_day_limit, suspected_day_limit, fill = list(testADM = 0, testSUSP = 0, knownCommunity = 0, probableCommunity = 0, nosocomial = 0)) %>% 
  filter(!is.na(Virus))

#unadjusted burden

patient_outcomes %>% 
  # filter(type_sej == "HOSPITALISATION") %>% 
  left_join(patient_knownStatus_days %>% select(IPP_sejour, Virus, acute_day_limit, suspected_day_limit, knownCommunity, probableCommunity), relationship = "one-to-many") %>% 
  group_by(Virus, acute_day_limit, suspected_day_limit
           # , type_sej, ICU, Death
  ) %>% 
  summarise(n_knownCommunity = sum(knownCommunity), n_probableCommunity = sum(probableCommunity))


# inferred from un-testing
NDA_virus_prediction_sejour <- read_csv("output/NDA_virus_prediction_sejour.csv")

mlPrediction_sejour_days <- NDA_virus_prediction_sejour %>% 
  cross_join(tibble(suspected_day_limit = c(7, 10))) %>% 
  filter(NDA_startDay < suspected_day_limit) %>% 
  group_by(IPP_sejour, Virus, suspected_day_limit) %>% 
  # summarise(infected_rfPredicted = median(prediction))
  arrange(.by_group = T, NDA_startDay) %>% 
  dplyr::slice(1) %>% 
  transmute(IPP_sejour, Virus, suspected_day_limit, infected_mlPredicted = prediction, infected_mlPredictedLo = predictionLo, infected_mlPredictedHi = predictionHi) %>% 
  ungroup


# sensitivity_fits <- read_csv(file = paste0("~/FIRESP/FIRESP_analysis/BURD/sensitivity_fits.csv"))
estStatus_days <- read_csv2(file = paste0("output/estStatus_age_sensAnal.csv"))
# estStatus_days <- estStatus_days %>% 
#   rows_upsert(estStatus %>% 
#                 mutate(acute_day_limit = 3, suspected_day_limit = 7), by = c("Virus", "patient_id", "acute_day_limit", "suspected_day_limit"))





patient_outcomes_virus_correction_days <- patient_outcomes %>% 
  left_join(patient_knownStatus_days %>% select(IPP_sejour, Virus, acute_day_limit, suspected_day_limit, testADM, testSUSP, knownCommunity, probableCommunity), relationship = "one-to-many") %>% 
  left_join(estStatus_days %>% transmute(IPP_sejour = patient_id, Virus, 
                                         acute_day_limit, suspected_day_limit, 
                                    late_infected = p_infected, late_infected_lo = p_infected_lo, late_infected_hi = p_infected_hi), by = c("IPP_sejour", "Virus", "acute_day_limit", "suspected_day_limit"), relationship = "one-to-one") %>% 
  left_join(mlPrediction_sejour_days %>% transmute(IPP_sejour, Virus, 
                                                   suspected_day_limit, 
                                                   untested_infected = infected_mlPredicted, untested_infected_lo = infected_mlPredictedLo, untested_infected_hi = infected_mlPredictedHi), by = c("IPP_sejour", "Virus", "suspected_day_limit"), relationship = "many-to-one") %>% 
  mutate(Virus = factor(Virus, levels = virus_vec)) %>% 
  filter(Virus != "InfluenzaNT") %>% 
  mutate(correctedCommunity_late = case_when(knownCommunity == 1 ~ 1, #if there's a positive test on days 0-2, it's a defined community case
                                             (testADM == 1) & (knownCommunity == 0) ~ 0, #if there's a negative test, it's a non-case
                                             (testSUSP == 1) ~ late_infected, #if there's a test on days 3-6, use the late testing analysis
                                             T ~ 0)) %>% 
  mutate(correctedCommunity_late_lo = case_when(knownCommunity == 1 ~ 1, #if there's a positive test on days 0-2, it's a defined community case
                                                (testADM == 1) & (knownCommunity == 0) ~ 0, #if there's a negative test, it's a non-case
                                                (testSUSP == 1) ~ late_infected_lo, #if there's a test on days 3-6, use the late testing analysis
                                                T ~ 0)) %>% 
  mutate(correctedCommunity_late_hi = case_when(knownCommunity == 1 ~ 1, #if there's a positive test on days 0-2, it's a defined community case
                                                (testADM == 1) & (knownCommunity == 0) ~ 0, #if there's a negative test, it's a non-case
                                                (testSUSP == 1) ~ late_infected_hi, #if there's a test on days 3-6, use the late testing analysis
                                                T ~ 0)) %>% 
  mutate(correctedCommunity_lateUntested = case_when(knownCommunity == 1 ~ 1, #if there's a positive test on days 0-2, it's a defined community case
                                                     (testADM == 1) & (knownCommunity == 0) ~ 0, #if there's a negative test, it's a non-case
                                                     (Virus == "SARS-CoV-2") & (season_admitted %in% season_vec[1:3]) ~ 0, #no SARS-CoV-2 before 2019/20
                                                     (testSUSP == 1) ~ late_infected, #if there's a test on days 3-6, use the late testing analysis
                                                     (Virus == "hCoV") & (season_admitted %in% season_vec[4:9]) ~ 0, #no hCoV during or after 2019/20
                                                     T ~ untested_infected)) %>% 
  mutate(correctedCommunity_lateUntested_lo = case_when(knownCommunity == 1 ~ 1, #if there's a positive test on days 0-2, it's a defined community case
                                                        (testADM == 1) & (knownCommunity == 0) ~ 0, #if there's a negative test, it's a non-case
                                                        (Virus == "SARS-CoV-2") & (season_admitted %in% season_vec[1:3]) ~ 0, #no SARS-CoV-2 before 2019/20
                                                        (testSUSP == 1) ~ late_infected_lo, #if there's a test on days 3-6, use the late testing analysis
                                                        (Virus == "hCoV") & (season_admitted %in% season_vec[4:9]) ~ 0, #no hCoV during or after 2019/20
                                                        T ~ untested_infected_lo)) %>% 
  mutate(correctedCommunity_lateUntested_hi = case_when(knownCommunity == 1 ~ 1, #if there's a positive test on days 0-2, it's a defined community case
                                                        (testADM == 1) & (knownCommunity == 0) ~ 0, #if there's a negative test, it's a non-case
                                                        (Virus == "SARS-CoV-2") & (season_admitted %in% season_vec[1:3]) ~ 0, #no SARS-CoV-2 before 2019/20
                                                        (testSUSP == 1) ~ late_infected_hi, #if there's a test on days 3-6, use the late testing analysis
                                                        (Virus == "hCoV") & (season_admitted %in% season_vec[4:9]) ~ 0, #no hCoV during or after 2019/20
                                                        T ~ untested_infected_hi)) %>% 
  
  group_by(IPP_sejour, acute_day_limit, suspected_day_limit) %>% 
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




patient_outcomes_inc_master_days <- patient_outcomes_virus_correction_days %>% 
  # mutate(month_name = month(month_admitted, label = T, abbr = T, locale = "EN")) %>% 
  # filter(type_sej == "HOSPITALISATION") %>%
  rename(seasonAdmitted = season_admitted, monthAdmitted = month_admitted) %>% 
  group_by(acute_day_limit, suspected_day_limit, age_group, seasonAdmitted, monthAdmitted, type_sej, Virus) %>% 
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
  transmute(acute_day_limit, 
            suspected_day_limit, 
            
            age_group, 
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
  
  pivot_longer(-c("acute_day_limit", "suspected_day_limit", "age_group", "seasonAdmitted", "monthAdmitted", "type_sej", "burden", "Virus"), names_sep = "_", names_to = c(".value", "Adjustment")) %>% 
  ungroup %>% 
  complete(acute_day_limit, suspected_day_limit, age_group, nesting(seasonAdmitted, monthAdmitted), nesting(type_sej, burden), Virus, Adjustment, fill = list(n = 0)) %>% 
  mutate(n = ifelse((Virus == "SARS-CoV-2") & (seasonAdmitted %in% season_vec[1:3]), NA, n)) %>% 
  # mutate(n = ifelse((Virus == "hCoV") & !(seasonAdmitted %in% season_vec[1:3]), NA, n)) %>% 
  mutate(Virus = factor(Virus, virus_vec), 
         # month_name = factor(month_name, month.abb[c(9:12, 1:8)]),
         Adjustment = factor(Adjustment, levels = c("knownCommunityRaw", "knownCommunity", "probableCommunity", "withLate", "withLateLo", "withLateHi", "withLateUntested", "withLateUntestedLo", "withLateUntestedHi", "afterCorrection", "afterCorrectionLo", "afterCorrectionHi")), 
         burden = factor(burden, levels = c("Emergency_only", "Hospital_admission", "Hospital_ICU", "Hospital_deathRaw", "Hospital_deathControl", "Hospital_deathPlus30")))


patient_outcomes_inc_master_days %>% 
  filter(!is.na(acute_day_limit)) %>% 
  filter(burden == "Hospital_admission") %>%
  filter(!seasonAdmitted %in% c("20/21", "21/22")) %>% 
  group_by(acute_day_limit, suspected_day_limit, Virus, Adjustment) %>% 
  summarise(n = sum(n, na.rm = T)) %>% 
  pivot_wider(id_cols = c("acute_day_limit", "suspected_day_limit", "Virus"), names_from = "Adjustment", values_from = "n") %>% 
  transmute(acute_day_limit, suspected_day_limit, 
            Virus, 
            # Confirmed = knownCommunityRaw, 
            Confirmed = knownCommunity, 
            Suspected = probableCommunity,
            `Inferred (late)`  = paste0(round(withLate - knownCommunity, 1), " (", round(withLateLo - knownCommunity, 1), ", ", round(withLateHi - knownCommunity, 1), ")"), 
            # `Inferred (untested)`  = paste0(round(withLateUntested - knownCommunity, 1), " (", round(withLateUntestedLo - knownCommunity, 1), ",", round(withLateUntestedHi - knownCommunity, 1), ")"), 
            `Inferred (untested)`  = paste0(round(withLateUntested - withLate, 1), " (", round(withLateUntestedLo - withLateLo, 1), ", ", round(withLateUntestedHi - withLateHi, 1), ")"), 
            `Inferred (total)`  = paste0(round(withLateUntested, 1), " (", round(withLateUntestedLo, 1), ", ", round(withLateUntestedHi, 1), ")"), 
            # n_corrected = n_afterCorrection - n_knownCommunity, 
            `Total (after adjustment)`  = paste0(round(afterCorrection, 1), " (", round(afterCorrectionLo, 1), ", ", round(afterCorrectionHi, 1), ")"), 
            
            Ratio = paste0(round((afterCorrection)/knownCommunity, 2), 
                           " (", round((afterCorrectionLo)/knownCommunity, 2), 
                           ", ", round((afterCorrectionHi)/knownCommunity, 2), ")")
  ) %>% 
  mutate(Virus = factor(Virus, virus_vec)) %>% 
  arrange(Virus, suspected_day_limit, acute_day_limit) %>%
  write_csv2(file = "output/tables/totals_corrected_days.csv")

