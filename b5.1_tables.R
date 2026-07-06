# b5.1_tables

# patient_outcomes_virus_correction %>% 
#   # filter(type_sej == "HOSPITALISATION") %>% 
#   # mutate(Virus = case_when(Virus == "Rhinovirus" ~ "Enterovirus/Rhinovirus", 
#   #                          Virus == "Enterovirus" ~ "Enterovirus/Rhinovirus", 
#   #                          # Virus == "Rhinovirus" ~ "Enterovirus/Rhinovirus", 
#   #                          T ~ Virus)) %>% 
#   group_by(Virus) %>% 
#   summarise(n_knownCommunity = sum(knownCommunity), 
#             n_probableCommunity = sum(probableCommunity), 
#             # n_probableCommunity = sum(probableCommunity), 
#             n_withLate = sum(correctedCommunity_late),
#             n_withLateUntested = sum(correctedCommunity_lateUntested),
#             n_afterCorrection = sum(correctedCommunity), 
#   ) %>% 
#   # WHY IS LATE CORRECTED BIGGER THAN PROBABLE COMMUNITY
#   # mutate(p_Corrected = n_corrected/n_knownCommunity - 1) %>% 
#   transmute(Virus, 
#             n_knownCommunity, 
#             n_probableCommunity,
#             addedRaw_fromLate = n_withLate - n_knownCommunity, 
#             addedRaw_fromUntested = n_withLateUntested - n_withLate, 
#             n_addedRawTotal = n_withLateUntested, 
#             # n_corrected = n_afterCorrection - n_knownCommunity, 
#             n_afterCorrection) %>% 
#   mutate(Virus = factor(Virus, virus_vec)) %>% 
#   arrange(Virus) %>% 


patient_outcomes_inc_master %>% 
  filter(burden == "Hospital_admission") %>%
  filter(!seasonAdmitted %in% c("20/21", "21/22")) %>% 
  group_by(Virus, Adjustment) %>% 
  summarise(n = sum(n, na.rm = T)) %>% 
  pivot_wider(id_cols = "Virus", names_from = "Adjustment", values_from = "n") %>% 
  transmute(Virus, 
            # Confirmed = knownCommunityRaw, 
            Confirmed = knownCommunity, 
            Suspected = probableCommunity,
            `Inferred (late)`  = paste0(round(withLate - knownCommunity, 1), "\n(", round(withLateLo - knownCommunity, 1), ", ", round(withLateHi - knownCommunity, 1), ")"), 
            # `Inferred (untested)`  = paste0(round(withLateUntested - knownCommunity, 1), "\n(", round(withLateUntestedLo - knownCommunity, 1), ",", round(withLateUntestedHi - knownCommunity, 1), ")"), 
            `Inferred (untested)`  = paste0(round(withLateUntested - withLate, 1), "\n(", round(withLateUntestedLo - withLateLo, 1), ", ", round(withLateUntestedHi - withLateHi, 1), ")"), 
            `Inferred (total)`  = paste0(round(withLateUntested, 1), "\n(", round(withLateUntestedLo, 1), ", ", round(withLateUntestedHi, 1), ")"), 
            # n_corrected = n_afterCorrection - n_knownCommunity, 
            `Total (after adjustment)`  = paste0(round(afterCorrection, 1), "\n(", round(afterCorrectionLo, 1), ", ", round(afterCorrectionHi, 1), ")"), 
            
            Ratio = paste0(round((afterCorrection)/knownCommunity, 2), 
                           "\n(", round((afterCorrectionLo)/knownCommunity, 2), 
                           ", ", round((afterCorrectionHi)/knownCommunity, 2), ")")
  ) %>% 
  mutate(Virus = factor(Virus, virus_vec)) %>% 
  arrange(Virus) %>%
  write_csv2(file = "output/tables/totals_corrected.csv")

patient_outcomes_noreadm_inc_master %>% 
  filter(burden == "Hospital_admission") %>%
  filter(!seasonAdmitted %in% c("20/21", "21/22")) %>% 
  group_by(Virus, Adjustment) %>% 
  summarise(n = sum(n, na.rm = T)) %>% 
  pivot_wider(id_cols = "Virus", names_from = "Adjustment", values_from = "n") %>% 
  transmute(Virus, 
            # Confirmed = knownCommunityRaw, 
            Confirmed = knownCommunity, 
            Suspected = probableCommunity,
            `Inferred (late)`  = paste0(round(withLate - knownCommunity, 1), "\n(", round(withLateLo - knownCommunity, 1), ", ", round(withLateHi - knownCommunity, 1), ")"), 
            # `Inferred (untested)`  = paste0(round(withLateUntested - knownCommunity, 1), "\n(", round(withLateUntestedLo - knownCommunity, 1), ",", round(withLateUntestedHi - knownCommunity, 1), ")"), 
            `Inferred (untested)`  = paste0(round(withLateUntested - withLate, 1), "\n(", round(withLateUntestedLo - withLateLo, 1), ", ", round(withLateUntestedHi - withLateHi, 1), ")"), 
            `Inferred (total)`  = paste0(round(withLateUntested, 1), "\n(", round(withLateUntestedLo, 1), ", ", round(withLateUntestedHi, 1), ")"), 
            # n_corrected = n_afterCorrection - n_knownCommunity, 
            `Total (after adjustment)`  = paste0(round(afterCorrection, 1), "\n(", round(afterCorrectionLo, 1), ", ", round(afterCorrectionHi, 1), ")"), 
            
            Ratio = paste0(round((afterCorrection)/knownCommunity, 2), 
                           "\n(", round((afterCorrectionLo)/knownCommunity, 2), 
                           ", ", round((afterCorrectionHi)/knownCommunity, 2), ")")
  ) %>% 
  mutate(Virus = factor(Virus, virus_vec)) %>% 
  arrange(Virus)%>%
  write_csv2(file = "output/tables/totals_corrected_noreadm.csv")


rbind(read_csv2("output/tables/totals_corrected.csv") %>% mutate(Readmissions = "Separated"),
read_csv2("output/tables/totals_corrected_noreadm.csv") %>% mutate(Readmissions = "Combined")) %>% 
  mutate(Virus = factor(Virus, levels = virus_vec), 
         Readmissions = factor(Readmissions, c("Separated", "Combined"))) %>% 
  relocate("Readmissions", .after = "Virus") %>% 
  arrange(Virus, Readmissions) %>% 
  write_csv2(file = "output/tables/totals_corrected_sepcomb.csv")


patient_outcomes_inc_master %>% 
  filter(Adjustment == "knownCommunity") %>% 
  filter(burden %in% c("Hospital_deathRaw", "Hospital_deathPlus30")) %>% 
  group_by(Virus, burden) %>% 
  summarise(n = sum(n, na.rm = T)) %>% 
  pivot_wider(id_cols = "Virus", names_from = "burden", values_from = "n") %>% 
  transmute(Virus, 
            `Recorded deaths (attributed)` = Hospital_deathRaw, 
            `Inferred (30 days)` = Hospital_deathPlus30 - Hospital_deathRaw, 
            `Deaths (incl inferred)` = Hospital_deathPlus30) %>% 
  write_csv2(file = "output/tables/deaths_corrected.csv")


deathRate_age_SNDS %>% 
  left_join(dp_deaths_FIRESP) %>% 
  mutate(deathRate_FIRESP = deaths_FIRESP/nDP) %>% 
  filter(nDP >= 100) %>% 
  filter(type_sej == "HOSPITALISATION") %>%
  transmute(DP = gsub("_", "", DP), age_group, deathRate_FIRESP, deathRate_within30) %>% 
  arrange(-deathRate_FIRESP) %>% 
  mutate(rank = 1:n()) %>% 
  # select(rank, everything()) %>% 
  write_csv2(file = "output/tables/deathRates.csv")


