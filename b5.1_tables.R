# b5.1_tables




# patient_outcomes_inc_master %>% 
#   filter(burden == "Hospital_admission") %>%
#   filter(!seasonAdmitted %in% c("20/21", "21/22")) %>% 
#   group_by(Virus, Adjustment) %>% 
#   summarise(n = sum(n, na.rm = T)) %>% 
#   pivot_wider(id_cols = "Virus", names_from = "Adjustment", values_from = "n") %>% 
#   transmute(Virus, 
#             # Confirmed = knownCommunityRaw, 
#             Confirmed = knownCommunity, 
#             Suspected = probableCommunity,
#             `Inferred (late)`  = paste0(round(withLate - knownCommunity, 1), "\n(", round(withLateLo - knownCommunity, 1), ", ", round(withLateHi - knownCommunity, 1), ")"), 
#             # `Inferred (untested)`  = paste0(round(withLateUntested - knownCommunity, 1), "\n(", round(withLateUntestedLo - knownCommunity, 1), ",", round(withLateUntestedHi - knownCommunity, 1), ")"), 
#             `Inferred (untested)`  = paste0(round(withLateUntested - withLate, 1), "\n(", round(withLateUntestedLo - withLateLo, 1), ", ", round(withLateUntestedHi - withLateHi, 1), ")"), 
#             `Inferred (total)`  = paste0(round(withLateUntested, 1), "\n(", round(withLateUntestedLo, 1), ", ", round(withLateUntestedHi, 1), ")"), 
#             # n_corrected = n_afterCorrection - n_knownCommunity, 
#             `Total (after adjustment)`  = paste0(round(afterCorrection, 1), "\n(", round(afterCorrectionLo, 1), ", ", round(afterCorrectionHi, 1), ")"), 
#             
#             Ratio = paste0(round((afterCorrection)/knownCommunity, 2), 
#                            "\n(", round((afterCorrectionLo)/knownCommunity, 2), 
#                            ", ", round((afterCorrectionHi)/knownCommunity, 2), ")")
#   ) %>% 
#   mutate(Virus = factor(Virus, virus_vec)) %>% 
#   arrange(Virus) %>%
#   write_csv2(file = "output/tables/totals_corrected.csv")
# 
# patient_outcomes_noreadm_inc_master %>% 
#   filter(burden == "Hospital_admission") %>%
#   filter(!seasonAdmitted %in% c("20/21", "21/22")) %>% 
#   group_by(Virus, Adjustment) %>% 
#   summarise(n = sum(n, na.rm = T)) %>% 
#   pivot_wider(id_cols = "Virus", names_from = "Adjustment", values_from = "n") %>% 
#   transmute(Virus, 
#             # Confirmed = knownCommunityRaw, 
#             Confirmed = knownCommunity, 
#             Suspected = probableCommunity,
#             `Inferred (late)`  = paste0(round(withLate - knownCommunity, 1), "\n(", round(withLateLo - knownCommunity, 1), ", ", round(withLateHi - knownCommunity, 1), ")"), 
#             # `Inferred (untested)`  = paste0(round(withLateUntested - knownCommunity, 1), "\n(", round(withLateUntestedLo - knownCommunity, 1), ",", round(withLateUntestedHi - knownCommunity, 1), ")"), 
#             `Inferred (untested)`  = paste0(round(withLateUntested - withLate, 1), "\n(", round(withLateUntestedLo - withLateLo, 1), ", ", round(withLateUntestedHi - withLateHi, 1), ")"), 
#             `Inferred (total)`  = paste0(round(withLateUntested, 1), "\n(", round(withLateUntestedLo, 1), ", ", round(withLateUntestedHi, 1), ")"), 
#             # n_corrected = n_afterCorrection - n_knownCommunity, 
#             `Total (after adjustment)`  = paste0(round(afterCorrection, 1), "\n(", round(afterCorrectionLo, 1), ", ", round(afterCorrectionHi, 1), ")"), 
#             
#             Ratio = paste0(round((afterCorrection)/knownCommunity, 2), 
#                            "\n(", round((afterCorrectionLo)/knownCommunity, 2), 
#                            ", ", round((afterCorrectionHi)/knownCommunity, 2), ")")
#   ) %>% 
#   mutate(Virus = factor(Virus, virus_vec)) %>% 
#   arrange(Virus)%>%
#   write_csv2(file = "output/tables/totals_corrected_noreadm.csv")


format_fix <- function(val, dig){
  format(nsmall = dig, round(digits = dig, val))
}

# makes a function of this code which we will run several times
make_totals_corrected <- function(tib){
  
         
  tib %>% 
    filter(burden == "Hospital_admission") %>%
    filter(!seasonAdmitted %in% c("20/21", "21/22")) %>% 
    group_by(Virus, Adjustment) %>% 
    summarise(n = sum(n, na.rm = T)) %>% 
    pivot_wider(id_cols = "Virus", names_from = "Adjustment", values_from = "n") %>% 
    transmute(Virus, 
              # Confirmed = knownCommunityRaw, 
              Confirmed = format_1(knownCommunity, dig = 1), 
              Suspected = probableCommunity,
              `Inferred (late)`  = paste0(format_fix(withLate - knownCommunity, 1), "\n(", format_fix(withLateLo - knownCommunity, 1), ", ", format_fix(withLateHi - knownCommunity, 1), ")"), 
              # `Inferred (untested)`  = paste0(round(withLateUntested - knownCommunity, 1), "\n(", round(withLateUntestedLo - knownCommunity, 1), ",", round(withLateUntestedHi - knownCommunity, 1), ")"), 
              `Inferred (untested)`  = paste0(format_fix(withLateUntested - withLate, 1), "\n(", format_fix(withLateUntestedLo - withLateLo, 1), ", ", format_fix(withLateUntestedHi - withLateHi, 1), ")"), 
              `Inferred (total)`  = paste0(format_fix(withLateUntested, 1), "\n(", format_fix(withLateUntestedLo, 1), ", ", format_fix(withLateUntestedHi, 1), ")"), 
              # n_corrected = n_afterCorrection - n_knownCommunity, 
              `Total (after adjustment)`  = paste0(format_fix(afterCorrection, 1), "\n(", format_fix(afterCorrectionLo, 1), ", ", format_fix(afterCorrectionHi, 1), ")"), 
              
              Ratio = paste0(format_fix((afterCorrection)/knownCommunity, 2), 
                             "\n(", format_fix((afterCorrectionLo)/knownCommunity, 2), 
                             ", ", format_fix((afterCorrectionHi)/knownCommunity, 2), ")")
    ) %>% 
    mutate(Virus = factor(Virus, virus_vec)) %>% 
    arrange(Virus) %>% 
    ungroup
}

#MANUSCRIPT TEXT VALUES #run this block to get values for the text
# patient_outcomes_inc_master %>% 
patient_outcomes_noreadm_inc_master %>%   
  filter(burden == "Hospital_admission") %>%
  filter(!seasonAdmitted %in% c("20/21", "21/22")) %>% 
  group_by(Virus, Adjustment) %>% 
  summarise(n = sum(n, na.rm = T)) %>% 
  pivot_wider(id_cols = "Virus", names_from = "Adjustment", values_from = "n") %>% 
  transmute(knownCommunityRaw, knownCommunity, probableCommunity, withLate, withLateUntested, afterCorrection, 
            late_increase = withLate/knownCommunity - 1, 
            late_absolute = withLate - knownCommunity, 
            untested_increase = (withLateUntested - withLate)/knownCommunity, 
            untested_absolute = withLateUntested - withLate, 
            total_increase = afterCorrection/knownCommunity - 1, 
            total_absolute = afterCorrection - knownCommunity) %>% 
  View

make_totals_corrected(patient_outcomes_inc_master) %>%  
  write_csv2(file = "output/tables/totals_corrected.csv")

make_totals_corrected(patient_outcomes_noreadm_inc_master) %>%  
  write_csv2(file = "output/tables/totals_corrected_noreadm.csv")

make_totals_corrected(patient_outcomes_inc_icd10inclusion_master)

make_totals_corrected(patient_outcomes_strictNDA_inc_master) %>%  
  write_csv2(file = "output/tables/totals_corrected_strictNDA.csv")

make_totals_corrected(patient_outcomes_noSpecific_inc_master) %>%  
  write_csv2(file = "output/tables/totals_corrected_noSpecific.csv")

# need to do something different for the days sensitivity analysis because the day limits are in the master table
daylimit_table <- patient_outcomes_inc_master_days %>% select(acute_day_limit, suspected_day_limit) %>% unique

# x = 1
# y = 7
purrr::map2(.x = daylimit_table$acute_day_limit, .y = daylimit_table$suspected_day_limit, 
            function(x, y) {
              patient_outcomes_inc_master_days %>% 
                filter(acute_day_limit %in% x, suspected_day_limit %in% y) %>% 
                select(-acute_day_limit, -suspected_day_limit) %>% 
                make_totals_corrected %>% 
                ungroup %>% 
                mutate(acute_day_limit = x, suspected_day_limit = y) %>% 
                select(Virus, acute_day_limit, suspected_day_limit, everything())
            }) %>% 
  bind_rows() %>% 
  arrange(Virus, suspected_day_limit, acute_day_limit) %>% 
  write_csv2(file = "output/tables/totals_corrected_days.csv")


rbind(read_delim("output/tables/totals_corrected.csv", delim = ";") %>% mutate(Readmissions = "Separated"),
      read_delim("output/tables/totals_corrected_noreadm.csv", delim = ";") %>% mutate(Readmissions = "Combined")) %>% 
  mutate(Virus = factor(Virus, levels = virus_vec), 
         Readmissions = factor(Readmissions, c("Separated", "Combined"))) %>% 
  relocate("Readmissions", .after = "Virus") %>% 
  arrange(Virus, Readmissions) %>% 
  mutate(Confirmed = format_fix(Confirmed, 1)) %>% 
  write_csv2(file = "output/tables/totals_corrected_sepcomb.csv")
 

rbind(read_delim("output/tables/totals_corrected.csv", delim = ";") %>% mutate(Training = "All"),
      read_delim("output/tables/totals_corrected_noSpecific.csv", delim = ";") %>% mutate(Training = "No virus-specific codes"), 
      read_delim("output/tables/totals_corrected_strictNDA.csv", delim = ";") %>% mutate(Training = "Only admissions data")) %>% 
  mutate(Virus = factor(Virus, levels = virus_vec)) %>% 
  relocate("Training", .after = "Virus") %>% 
  arrange(Virus, Training) %>% 
  mutate(Confirmed = format_fix(Confirmed, 1)) %>% 
  write_csv2(file = "output/tables/totals_corrected_training.csv")






patient_outcomes_inc_master %>% 
  filter(Adjustment == "knownCommunity") %>% 
  filter(burden %in% c("Hospital_deathRaw", "Hospital_deathPlus30")) %>% 
  group_by(Virus, burden) %>% 
  summarise(n = sum(n, na.rm = T)) %>% 
  pivot_wider(id_cols = "Virus", names_from = "burden", values_from = "n") %>% 
  transmute(Virus, 
            `Recorded deaths (attributed)` = format_fix(Hospital_deathRaw, 1), 
            `Inferred (30 days)` = format_fix(Hospital_deathPlus30 - Hospital_deathRaw, 1), 
            `Deaths (incl inferred)` = format_fix(Hospital_deathPlus30, 1)) %>% 
  write_csv2(file = "output/tables/deaths_corrected.csv")


  

deathRate_age_SNDS %>% 
  left_join(dp_deaths_FIRESP %>% 
              group_by(age_group, DP) %>% 
              summarise(across(c("deaths_FIRESP", "nDP"), sum)) %>% 
              ungroup %>% 
              mutate(deathRate_FIRESP = deaths_FIRESP/nDP)) %>% 
  filter(nDP >= 100) %>% 
  # filter(type_sej == "HOSPITALISATION") %>%
  arrange(-deathRate_FIRESP) %>% 
  mutate(DP = gsub("_", "", DP), rank = 1:n()) %>% 
  left_join(cim10_ref %>% transmute(DP = code, Description = cim10_description) %>% 
              rbind(tibble(DP = "C920", Description = "Acute myeloblastic leukaemia"), 
                    tibble(DP = "I5019", Description = "Left ventricular failure: Unspecified"), 
                    tibble(DP = "I5009", Description = "Congestive heart failure, with left ventricular ejection fraction [LVEF]
unspecified"), 
                    tibble(DP = "I5002", Description = "Congestive heart failure, with a left ventricular ejection fraction [LVEF] of less than 40%"), 
                    tibble(DP = "S7210", Description = "Fracture of femur: Trochanteric fracture, unspecified"), 
                    tibble(DP = "S7200", Description = "Fracture of neck of femur: Part unspecified "))) %>% 
  transmute(rank, DP, Description, age_group, 
            deathRate_FIRESP = format_fix(deathRate_FIRESP, 3), 
            deathRate_within30 = format_fix(deathRate_within30, 3)) %>%
  write_csv2(file = "output/tables/deathRates.csv")


# deathRate_age_SNDS %>% 
#   mutate(deathRate_FIRESP = deaths_FIRESP/nDP) %>% 
#   filter(nDP >= 100) %>% 
#   filter(type_sej == "HOSPITALISATION") %>%
#   arrange(-deathRate_FIRESP) %>% 
#   mutate(DP = gsub("_", "", DP), rank = 1:n()) %>% 
#   left_join(cim10_ref %>% transmute(DP = code, Description = cim10_description)) %>% 
#   transmute(rank, DP, Description, age_group, 
#             deathRate_FIRESP = format_fix(deathRate_FIRESP, 3), 
#             deathRate_within30 = format_fix(deathRate_within30, 3)) %>%
#   write_csv2(file = "output/tables/deathRates.csv")

# admission_incidence_age_CI_plot is created in b5.2_figures
admission_incidence_age_CI_plot %>% 
  filter(burden == "Hospital\nadmission") %>% 
  pivot_wider(id_cols = Virus:burden, names_from = Adjustment, values_from = c("Inc1e5", "Lo", "Hi")) %>% 
  select(Virus:burden, Inc1e5_Unadjusted, Inc1e5_Adjusted, Lo_Adjusted, Hi_Adjusted) %>% 
  mutate(across(where(is.numeric), ~format_fix(.x, dig = 2))) %>% 
  write_csv2("output/tables/admission_incidence_age_CI.csv")


