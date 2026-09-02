


codage_data_classBact <- codage_data_cleaned %>% 
  ungroup %>% 
  # slice(1:1000) %>% 
  group_by(type_sej, ICU, season_admitted, month_admitted, week_admitted, age_group, IPP_sejour) %>% 
  summarise(#across(`SARS-CoV-2`:`RV/EV`, ~as.numeric(any(.x %in% 1))),
    across(codage_virus_order, ~case_when(any(.x == 1) ~ 1, 
                                            any(.x == 0) ~ 0, 
                                            T ~ NA_real_)), 
    across(paste0("BPO_", bact_vec), ~case_when(any(.x == 1) ~ 1, 
                                 any(.x == 0) ~ 0, 
                                 T ~ NA_real_)), 
    codes_relevant = CIM10[!is.na(Status)] %>% unique %>% sort %>% paste(collapse = "_"), 
    Influenza_code = case_when(any(Status == "Influenza" & Confirmation %in% "confirmed") ~ 1, 
                               any(Status == "Influenza") ~ 0.5, 
                               T ~ 0), 
    `SARS-CoV-2_code` = case_when(all(year(month_admitted) < 2020) ~ NA_real_,
                                  any(Status == "SARS-CoV-2" & Confirmation %in% "confirmed") ~ 1, 
                                  any(Status == "SARS-CoV-2") ~ 0.5, 
                                  T ~ 0),
    RSV_code = as.numeric(any(Status %in% "RSV")), 
    hCoV_code = as.numeric(any(Status %in% "hCoV")), 
    ADV_code = as.numeric(any(Status %in% "ADV")), 
    PIV_code = as.numeric(any(Status %in% "PIV")), 
    `RV/EV_code` = as.numeric(any(Status %in% "RV/EV")), 
    hMPV_code = as.numeric(any(Status %in% "hMPV")), 
    
    Streppneum_code = as.numeric(any(Status %in% "Streppneum")), 
    Haeminf_code = as.numeric(any(Status %in% "Haeminf")), 
    Klebpneum_code = as.numeric(any(Status %in% "Klebpneum")), 
    Pseudomonas_code = as.numeric(any(Status %in% "Pseudomonas")), 
    Staphylococcus_code = as.numeric(any(Status %in% "Staphylococcus")), 
    StrepB_code = as.numeric(any(Status %in% "StrepB")), 
    Strepother_code = as.numeric(any(Status %in% "Strepother")), 
    Ecoli_code = as.numeric(any(Status %in% "Ecoli")), 
    Gramnegother_code = as.numeric(any(Status %in% "Gramnegother")), 
    Mycopneum_code = as.numeric(any(Status %in% "Mycopneum")), 
    Other_code = as.numeric(any(Status %in% "Other")), 
    Unspec_code = as.numeric(any(Status %in% "Unspec")), 
    anyBact_code = as.numeric(any(Status %in% "anyBact"))
  ) %>% 
  ungroup %>% 
  {mutate(., BPO_anyBact = select(., paste0("BPO_", bact_vec)) %>% 
            # apply(1, function(x) max(x[!is.na(x)])) %>%
            {(rowSums(., na.rm = T) > 0) * ifelse(rowSums(is.na(.)) == ncol(.), NA, 1)} %>% 
            as.numeric)}

codage_data_classBact %>% 
  # group_by(pick(contains("Streppneum"))) %>% 
  # group_by(pick(contains("Haeminf"))) %>%
  group_by(pick(contains("Pseudomonas"))) %>%
  # group_by(pick(contains("Staphylococcus"))) %>% 
  # group_by(pick(contains("Klebpneum"))) %>% 
  # group_by(pick(contains("Ecoli"))) %>% 
  # group_by(pick(contains("Mycopneum"))) %>% 
  # group_by(pick(contains("StrepB"))) %>% 
  # group_by(pick(contains("Strepother"))) %>% 
  # group_by(pick(contains("Gramnegother"))) %>% 
  # group_by(pick(contains("Gramnegother"))) %>% 
  
  summarise(n = n_distinct(IPP_sejour))



codage_data_classBact_summarised <- codage_data_classBact %>% 
  # rename_with(~paste0(.x, "_pcr"), `SARS-CoV-2`:`RV/EV`) %>% 
  rename_all(~gsub("BPO_(.*)", "\\1_BPO", .x)) %>% 
  select(IPP_sejour, type_sej, season_admitted, month_admitted, week_admitted, age_group, ends_with("_BPO"), paste0(bact_vec, "_code")) %>% 
  pivot_longer(cols = -c(IPP_sejour, type_sej, season_admitted, month_admitted, week_admitted, age_group), names_sep = "_", names_to = c("Bact", ".value")) %>% 
  mutate(Virus = factor(Bact, bact_vec), 
         age_group = factor(age_group, age_vec), 
         type_sej = factor(type_sej, levels = c("HOSPITALISATION", "EMERGENCY")), 
         BPO = factor(BPO, levels = c(1, 0, NA)), 
         code = factor(code, levels = c(1, 0, NA))) %>% 
  mutate(code = fct_recode(code, Code = "1", `No code` = "0"), 
         BPO = fct_recode(BPO, Pos = "1", Neg = "0") %>% fct_na_value_to_level(level = "Untested")) %>% 
  group_by(type_sej, season_admitted, month_admitted, week_admitted, age_group, Bact, BPO, code) %>% 
  summarise(n = n_distinct(IPP_sejour)) %>% 
  ungroup %>% 
  left_join(enframe(bact_icd10_vec, name = "Bact", value = "ICD10")) %>% 
  left_join(enframe(bacteria_vec, name = "Bact", value = "Bacteria"))

