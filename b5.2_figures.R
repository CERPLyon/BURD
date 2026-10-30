virus_col = viridisLite::turbo(length(virus_vec)) %>%
  setNames(virus_vec)

adjustment_col = c(Unadjusted = "#1a80bb", Adjusted = "#ea801c")


virus_col[7:9] <- c("#F28E2BFF",
                    "#D64B00FF",
                    "#8E0152FF")


# big bar plot for age

patient_outcomes_inc_master %>% 
  filter(burden == "Hospital_admission") %>% 
  filter(Adjustment %in% c("knownCommunity", "probableCommunity", "withLate")) %>% 
  ggplot(aes(x = monthAdmitted, y = n, colour = Adjustment)) + 
  geom_line() + 
  # geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(Virus ~ age_group, scales = "free_y") + theme_bw()

ggsave("output/plots/nos_results.png", units = "cm", width = 35, height = 20)

patient_outcomes_inc_master %>% 
  filter(!(seasonAdmitted %in% c("20/21", "21/22"))) %>% 
  group_by(Virus, age_group, Adjustment, type_sej, burden, seasonAdmitted) %>% 
  summarise(n = sum(n)) %>% 
  left_join(catchmentPop, by = c("seasonAdmitted", "age_group", "type_sej")) %>% 
  mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
  group_by(Virus, age_group, Adjustment, type_sej, burden) %>% 
  summarise(Inc1e5 = mean(Inc1e5, na.rm = T)) %>% 
  filter(Adjustment %in% c("knownCommunity", "afterCorrection", "afterCorrectionLo", "afterCorrectionHi")) %>% 
  ungroup %>% 
  mutate(CI = gsub("afterCorrection", "", gsub("knownCommunity", "", Adjustment)), 
         Adjustment = gsub("[LoHi]{2}$", "", Adjustment), 
         CI = ifelse(CI == "", "Inc1e5", CI)) %>% 
  pivot_wider(id_cols = c("Virus", "age_group", "type_sej", "burden", "Adjustment"), names_from = "CI", values_from = "Inc1e5") %>% 
  mutate(Adjustment = fct_recode(Adjustment, Unadjusted = "knownCommunity", Adjusted = "afterCorrection")) %>% 
  mutate(Adjustment = fct_rev(Adjustment)) %>% 
  filter((burden %in% c("Emergency_only", "Hospital_admission", "Hospital_ICU")) | #this applies the 30 day death correction only to the adjusted numbers
           (burden == "Hospital_deathRaw" & Adjustment == "Unadjusted") |
           (burden == "Hospital_deathPlus30" & Adjustment == "Adjusted")) %>% 
  mutate(burden = fct_collapse(burden, Hospital_death = c("Hospital_deathRaw", "Hospital_deathPlus30"))) %>% 
  
  mutate(burden = fct_relabel(burden, .fun = ~gsub("_", "\n", .x))) -> admission_incidence_age_CI_plot

admission_incidence_age_CI_plot %>% 
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  # filter(type_sej == "HOSPITALISATION") %>%
  ggplot(aes(x = age_group, y = Inc1e5, colour = Adjustment, fill = Adjustment)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5) + 
  geom_errorbar(aes(ymin = Lo, ymax = Hi, colour = Adjustment), width = 0.2, position = position_nudge(x = 0.25)) +
  facet_grid(burden~Virus
             , scales = "free_y"
  ) + 
  scale_colour_manual(values = adjustment_col) +
  scale_fill_manual(values = adjustment_col) +
  theme_bw() + 
  guides(colour = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Age group", y = "Incidence per 100 000 catchment population per year", fill = "")

ggsave("output/plots/admission_incidence_age_CI.png", width = 30, height = 16, units = "cm", dpi = 600)

admission_incidence_age_CI_plot %>% 
  filter(!(Virus %in% c("ADV", "PIV", "hCoV"))) %>% 
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  # filter(type_sej == "HOSPITALISATION") %>%
  ggplot(aes(x = age_group, y = Inc1e5, colour = Adjustment, fill = Adjustment)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5) + 
  geom_errorbar(aes(ymin = Lo, ymax = Hi, colour = Adjustment), width = 0.2, position = position_nudge(x = 0.25)) +
  facet_grid(burden~Virus
             , scales = "free_y"
  ) + 
  scale_colour_manual(values = adjustment_col) +
  scale_fill_manual(values = adjustment_col) +
  theme_bw() + 
  guides(colour = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Age group", y = "Incidence per 100 000 catchment population per year", fill = "")

ggsave("output/plots/admission_incidence_age_CI_big6.png", width = 25, height = 16, units = "cm", dpi = 600)


admission_incidence_age_CI_plot %>% 
  filter((Virus %in% c("ADV", "PIV", "hCoV"))) %>% 
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  # filter(type_sej == "HOSPITALISATION") %>%
  ggplot(aes(x = age_group, y = Inc1e5, colour = Adjustment, fill = Adjustment)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5) + 
  geom_errorbar(aes(ymin = Lo, ymax = Hi, colour = Adjustment), width = 0.2, position = position_nudge(x = 0.25)) +
  facet_grid(burden~Virus
             , scales = "free_y"
  ) + 
  scale_colour_manual(values = adjustment_col) +
  scale_fill_manual(values = adjustment_col) +
  theme_bw() + 
  guides(colour = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Age group", y = "Incidence per 100 000 catchment population per year", fill = "")

ggsave("output/plots/admission_incidence_age_CI_small3.png", width = 15, height = 16, units = "cm", dpi = 600)



# line plot for season

patient_outcomes_inc_master %>% 
  # filter(!(seasonAdmitted %in% c("20/21", "21/22"))) %>%
  group_by(Virus, Adjustment, type_sej, burden, seasonAdmitted) %>% 
  summarise(n = sum(n)) %>% 
  left_join(catchmentPop %>% group_by(seasonAdmitted, type_sej) %>% summarise(catchmentHosp = sum(catchmentHosp)), by = c("seasonAdmitted", "type_sej")) %>% 
  mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
  ungroup %>% 
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  filter(Adjustment %in% c("knownCommunity", "afterCorrection", "afterCorrectionLo", "afterCorrectionHi")) %>% 
  ungroup %>% 
  
  
  mutate(CI = gsub("afterCorrection", "", gsub("knownCommunity", "", Adjustment)), 
         Adjustment = gsub("[LoHi]{2}$", "", Adjustment), 
         CI = ifelse(CI == "", "Inc1e5", CI)) %>% 
  pivot_wider(id_cols = c("Virus", "seasonAdmitted", "type_sej", "burden", "Adjustment"), names_from = "CI", values_from = "Inc1e5") %>% 
  mutate(Adjustment = fct_recode(Adjustment, Unadjusted = "knownCommunity", Adjusted = "afterCorrection")) %>% 
  mutate(Adjustment = fct_rev(Adjustment)) %>% 
  
  filter((burden %in% c("Emergency_only", "Hospital_admission", "Hospital_ICU")) | #this applies the 30 day death correction only to the adjusted numbers
           (burden == "Hospital_deathRaw" & Adjustment == "Unadjusted") |
           (burden == "Hospital_deathPlus30" & Adjustment == "Adjusted")) %>% 
  mutate(burden = fct_collapse(burden, Hospital_death = c("Hospital_deathRaw", "Hospital_deathPlus30"))) %>% 
  
  mutate(burden = fct_relabel(burden, .fun = ~gsub("_", "\n", .x))) -> admission_incidence_season_CI_plot

admission_incidence_season_CI_plot %>% 
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  ggplot(aes(x = seasonAdmitted, colour = Adjustment, group = Adjustment)) + 
  # geom_bar(stat = "identity", position = "dodge") + 
  geom_ribbon(aes(ymin = Lo, ymax = Hi, fill = "Adjusted"), alpha = 0.3, colour = NA)+ 
  geom_line(aes(y = Inc1e5))+ 
  facet_grid(burden~Virus
             , scales = "free_y"
  ) + 
  scale_colour_manual(values = adjustment_col) +
  scale_fill_manual(values = adjustment_col, guide="none") +
  theme_bw() + 
  guides(alpha = F) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Season admitted", y = "Incidence per 100 000 catchment population per year") 


ggsave("output/plots/admission_incidence_season_CI.png", width = 40, height = 13, units = "cm", dpi = 600)


admission_incidence_season_CI_plot %>% 
  filter(!(Virus %in% c("ADV", "PIV", "hCoV"))) %>% 
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  ggplot(aes(x = seasonAdmitted, colour = Adjustment, group = Adjustment)) + 
  # geom_bar(stat = "identity", position = "dodge") + 
  geom_ribbon(aes(ymin = Lo, ymax = Hi, fill = "Adjusted"), alpha = 0.3, colour = NA)+ 
  geom_line(aes(y = Inc1e5))+ 
  facet_grid(burden~Virus
             , scales = "free_y"
  ) + 
  scale_colour_manual(values = adjustment_col) +
  scale_fill_manual(values = adjustment_col, guide="none") +
  theme_bw() + 
  guides(alpha = F) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Season admitted", y = "Incidence per 100 000 catchment population per year")

ggsave("output/plots/admission_incidence_season_CI_big6.png", width = 30, height = 13, units = "cm", dpi = 600)

admission_incidence_season_CI_plot %>% 
  filter((Virus %in% c("ADV", "PIV", "hCoV"))) %>% 
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  ggplot(aes(x = seasonAdmitted, colour = Adjustment, group = Adjustment)) + 
  # geom_bar(stat = "identity", position = "dodge") + 
  geom_ribbon(aes(ymin = Lo, ymax = Hi, fill = "Adjusted"), alpha = 0.3, colour = NA)+ 
  geom_line(aes(y = Inc1e5))+ 
  facet_grid(burden~Virus
             , scales = "free_y"
  ) + 
  scale_colour_manual(values = adjustment_col) +
  scale_fill_manual(values = adjustment_col, guide="none") +
  theme_bw() + 
  guides(alpha = F) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") + 
  labs(x = "Season admitted", y = "Incidence per 100 000 catchment population per year")

ggsave("output/plots/admission_incidence_season_CI_small3.png", width = 15, height = 16, units = "cm", dpi = 600)


# big bar plot for months

patient_outcomes_inc_master %>% 
  group_by(Virus, age_group, seasonAdmitted, monthAdmitted, Adjustment, type_sej, burden) %>% 
  summarise(n = sum(n)) %>% 
  left_join(catchmentPop, by = c("seasonAdmitted", "age_group", "type_sej")) %>% 
  mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  filter(burden == "Hospital_admission") %>%
  group_by(Virus, age_group, seasonAdmitted, monthAdmitted, Adjustment) %>% 
  summarise(Inc1e5 = sum(Inc1e5)) %>% 
  # ungroup %>% 
  # filter(burden == "Hospital_admission") %>% 
  # mutate(burden = fct_relabel(burden, .fun = ~gsub("_", "\n", .x))) %>% 
  
  filter(Adjustment %in% c("knownCommunity", "afterCorrection", "afterCorrectionLo", "afterCorrectionHi")) %>% 
  ungroup %>% 
  
  mutate(CI = gsub("afterCorrection", "", gsub("knownCommunity", "", Adjustment)), 
         Adjustment = gsub("[LoHi]{2}$", "", Adjustment), 
         CI = ifelse(CI == "", "Inc1e5", CI)) %>% 
  pivot_wider(id_cols = c("Virus", "age_group", "seasonAdmitted", "monthAdmitted", "Adjustment"), names_from = "CI", values_from = "Inc1e5") %>% 
  mutate(Adjustment = fct_recode(Adjustment, Unadjusted = "knownCommunity", Adjusted = "afterCorrection")) %>% 
  mutate(Adjustment = fct_rev(Adjustment)) -> admission_incidence_month_CI_plot

admission_incidence_month_CI_plot %>% 
  ggplot(aes(x = monthAdmitted, colour = Virus, linetype = Adjustment)) + 
  geom_line(aes(y = Inc1e5)) + 
  geom_ribbon(aes(ymin = Lo, ymax = Hi, fill = Virus), alpha = 0.3, colour = NA)+ 
  # geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(age_group~.
             , scales = "free_y"
  ) + 
  scale_colour_manual(values = virus_col) +
  scale_fill_manual(values = virus_col) +
  scale_linetype_manual(values = c(Unadjusted = "dashed", Adjusted = "solid")) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  labs(x = "Month admitted", y = "Incidence per 100 000 catchment population per month")

ggsave("output/plots/admission_incidence_month_CI.png", width = 30, height = 15, units = "cm", dpi = 600)

admission_incidence_month_CI_plot %>% 
  filter(Virus %in% virus_vec[1:4]) %>% 
  ggplot(aes(x = monthAdmitted, colour = Virus, linetype = Adjustment)) + 
  geom_line(aes(y = Inc1e5)) + 
  geom_ribbon(aes(ymin = Lo, ymax = Hi, fill = Virus), alpha = 0.3, colour = NA)+ 
  # geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(age_group~.
             , scales = "free_y"
  ) + 
  scale_colour_manual(values = virus_col) +
  scale_fill_manual(values = virus_col) +
  scale_linetype_manual(values = c(Unadjusted = "dashed", Adjusted = "solid")) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  labs(x = "Month admitted", y = "Incidence per 100 000 catchment population per month")

ggsave("output/plots/admission_incidence_month_CI_big3.png", width = 30, height = 15, units = "cm", dpi = 600)

admission_incidence_month_CI_plot %>% 
  filter(Virus %in% virus_vec[5:9]) %>% 
  ggplot(aes(x = monthAdmitted, colour = Virus, linetype = Adjustment)) + 
  geom_line(aes(y = Inc1e5)) + 
  geom_ribbon(aes(ymin = Lo, ymax = Hi, fill = Virus), alpha = 0.3, colour = NA)+ 
  # geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(age_group~.
             , scales = "free_y"
  ) + 
  scale_colour_manual(values = virus_col) +
  scale_fill_manual(values = virus_col) +
  scale_linetype_manual(values = c(Unadjusted = "dashed", Adjusted = "solid")) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  labs(x = "Month admitted", y = "Incidence per 100 000 catchment population per month")

ggsave("output/plots/admission_incidence_month_CI_others.png", width = 30, height = 15, units = "cm", dpi = 600)


# testing rates plot for supplement

rbind(
  patient_outcomes_virus_correction %>% 
    group_by(Virus, type_sej, season_admitted, age_group, test02) %>% 
    summarise(n = n_distinct(IPP_sejour)) %>% 
    ungroup %>% 
    mutate(burden = ifelse(type_sej == "HOSPITALISATION", "Hospital_admission", "Emergency_only")) %>% 
    select(-type_sej)
  ,
  patient_outcomes_virus_correction %>% 
    filter(ICU == 1, type_sej == "HOSPITALISATION") %>% 
    group_by(Virus, season_admitted, age_group, test02) %>% 
    summarise(n = n_distinct(IPP_sejour)) %>% 
    ungroup %>% 
    mutate(burden = "Hospital_ICU") 
  ,
  patient_outcomes_virus_correction %>% 
    filter(Death == 1, type_sej == "HOSPITALISATION") %>% 
    group_by(Virus, season_admitted, age_group, test02) %>% 
    summarise(n = n_distinct(IPP_sejour)) %>% 
    ungroup %>% 
    mutate(burden = "Hospital_death") 
) %>% 
  complete(Virus, burden, season_admitted, age_group, test02, fill = list(n = 0)) %>% 
  mutate(n = if_else(Virus == "SARS-CoV-2" & season_admitted %in% c("16/17", "17/18", "18/19"), NA, n)) %>% 
  pivot_wider(names_from = "test02", values_from = "n") %>% 
  rename(Untested = "0", Tested = "1") %>% 
  mutate(pTested = Tested/(Untested + Tested)) %>% 
  mutate(burden = factor(burden, c("Emergency_only", "Hospital_admission", "Hospital_ICU", "Hospital_death"))) %>% 
  mutate(burden = fct_relabel(burden, .fun = ~gsub("_", "\n", .x))) %>% 
  # filter(burden == "Hospital\nadmission", Virus == "RSV") %>% View #MANUSCRIPT TEXT VALUES #run this line to get values for the text
  ggplot(aes(x = season_admitted, y = pTested, colour = age_group, group = age_group)) + 
  geom_line(linewidth = 1) + 
  # geom_point() + 
  facet_grid(burden ~ Virus) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Season admitted", y = "Proportion of admissions tested", colour = "Age group") + 
  scale_colour_viridis_d(option = "plasma")

ggsave("output/plots/testingRates_season.png", width = 40, height = 13, units = "cm", dpi = 600)



# ratio plots

ratio_age_CI <- admission_incidence_age_CI_plot %>% 
  pivot_wider(id_cols = c("age_group", "Virus", "type_sej", "burden"), names_from = "Adjustment", values_from = c("Inc1e5", "Lo", "Hi")) %>% 
  select(-Lo_Unadjusted, -Hi_Unadjusted) %>% 
  mutate(Ratio = Inc1e5_Adjusted/Inc1e5_Unadjusted, 
         RatioLo = Lo_Adjusted/Inc1e5_Unadjusted, 
         RatioHi = Hi_Adjusted/Inc1e5_Unadjusted)

ratio_age_CI %>% 
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  # filter(type_sej == "HOSPITALISATION") %>%
  mutate(across(starts_with("Ratio"), ~ifelse(is.finite(.x), .x, NA))) %>% 
  ggplot(aes(x = age_group, y = Ratio)) + 
  geom_point(aes(y = Ratio)) + 
  geom_errorbar(aes(ymin = RatioLo, ymax = RatioHi)) + 
  geom_label(aes(label = round(Ratio, 1), y = 0.5), size = 2) + 
  facet_grid(burden~Virus
             , scales = "free_y"
  ) + 
  theme_bw() + 
  guides(colour = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Age group", y = "Adjustment ratio", fill = "") + 
  coord_cartesian(ylim = c(0, 5))

ggsave("output/plots/ratio_age_CI.png", width = 30, height = 16, units = "cm", dpi = 600)


ratio_season_CI <- admission_incidence_season_CI_plot %>% 
  pivot_wider(id_cols = c("seasonAdmitted", "Virus", "type_sej", "burden"), names_from = "Adjustment", values_from = c("Inc1e5", "Lo", "Hi")) %>% 
  select(-Lo_Unadjusted, -Hi_Unadjusted) %>% 
  mutate(Ratio = Inc1e5_Adjusted/Inc1e5_Unadjusted, 
         RatioLo = Lo_Adjusted/Inc1e5_Unadjusted, 
         RatioHi = Hi_Adjusted/Inc1e5_Unadjusted)

ratio_season_CI %>% 
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  # filter(type_sej == "HOSPITALISATION") %>%
  mutate(across(starts_with("Ratio"), ~ifelse(is.finite(.x), .x, NA))) %>% 
  ggplot(aes(x = seasonAdmitted, y = Ratio)) + 
  geom_point(aes(y = Ratio)) + 
  geom_errorbar(aes(ymin = RatioLo, ymax = RatioHi)) + 
  geom_label(aes(label = round(Ratio, 1), y = 0.5), size = 1.5) + 
  facet_grid(burden~Virus
             , scales = "free_y"
  ) + 
  theme_bw() + 
  guides(colour = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Season", y = "Adjustment ratio", fill = "") + 
  coord_cartesian(ylim = c(0, 5))

ggsave("output/plots/ratio_season_CI.png", width = 30, height = 16, units = "cm", dpi = 600)

ratio_month_CI <- admission_incidence_month_CI_plot %>% 
  select(-seasonAdmitted) %>% 
  pivot_wider(id_cols = c("monthAdmitted", "age_group", "Virus"), names_from = "Adjustment", values_from = c("Inc1e5", "Lo", "Hi")) %>% 
  select(-Lo_Unadjusted, -Hi_Unadjusted) %>% 
  mutate(Ratio = Inc1e5_Adjusted/Inc1e5_Unadjusted, 
         RatioLo = Lo_Adjusted/Inc1e5_Unadjusted, 
         RatioHi = Hi_Adjusted/Inc1e5_Unadjusted)

ratio_month_CI %>% 
  mutate(across(starts_with("Ratio"), ~ifelse(is.finite(.x), .x, NA))) %>% 
  filter(Virus %in% c("InfluenzaA", "InfluenzaB", "RSV", "SARS-CoV-2")) %>% 
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  # filter(type_sej == "HOSPITALISATION") %>%
  ggplot(aes(x = monthAdmitted, colour = Virus, fill = Virus, group = Virus)) + 
  geom_line(aes(y = Ratio)) + 
  geom_point(aes(y = Ratio)) + 
  geom_ribbon(aes(ymin = RatioLo, ymax = RatioHi), alpha = 0.5) + 
  facet_grid(age_group~.
             , scales = "free_y"
  ) + 
  theme_bw() + 
  guides(colour = "none") +
  scale_colour_manual(values = virus_col) + 
  scale_fill_manual(values = virus_col) + 
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Month", y = "Adjustment ratio", fill = "") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  coord_cartesian(ylim = c(0, 5))

ggsave("output/plots/ratio_month_CI.png", width = 30, height = 16, units = "cm", dpi = 600)


# proportion plot

proportion_plot <- patient_outcomes_inc_master %>% 
  # filter(!(seasonAdmitted %in% c("20/21", "21/22"))) %>%
  filter(!(seasonAdmitted %in% c("16/17", "17/18", "18/19", "19/20", "20/21", "21/22"))) %>%
  group_by(Virus, Adjustment, type_sej, burden, seasonAdmitted) %>% 
  summarise(n = sum(n)) %>% 
  left_join(catchmentPop %>% group_by(seasonAdmitted, type_sej) %>% summarise(catchmentHosp = sum(catchmentHosp)), by = c("seasonAdmitted", "type_sej")) %>% 
  mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
  ungroup %>% 
  group_by(Virus, Adjustment, type_sej, burden) %>% 
  summarise(Inc1e5 = mean(Inc1e5, na.rm = T)) %>% 
  filter(Adjustment %in% c("knownCommunity", "afterCorrection")) %>% 
  mutate(Adjustment = ifelse(Adjustment == "knownCommunity", "Unadjusted", "Adjusted") %>% factor(levels = c("Unadjusted", "Adjusted"))) %>% 
  
  filter((burden %in% c("Emergency_only", "Hospital_admission", "Hospital_ICU")) | #this applies the 30 day death correction only to the adjusted numbers
           (burden == "Hospital_deathRaw" & Adjustment == "Unadjusted") |
           (burden == "Hospital_deathPlus30" & Adjustment == "Adjusted")) %>% 
  mutate(burden = fct_collapse(burden, Hospital_death = c("Hospital_deathRaw", "Hospital_deathPlus30")))

proportion_plot %>% 
  group_by(burden, Adjustment) %>% 
  mutate(p = Inc1e5/sum(Inc1e5, na.rm = T)) %>% 
  select(Adjustment, burden, Virus, p) %>% 
  arrange(burden) %>% 
  pivot_wider(id_cols = c("burden", "Virus"), names_from = "Adjustment", values_from = "p") %>% 
  # mutate(z = ifelse(Adjusted > Unadjusted, "BIGGER", "smaller")) %>% View #MANUSCRIPT TEXT VALUES #run this line to get values for the text
  mutate(across(ends_with("djusted"), function(.x) format_fix(.x*100, 1) %>% {ifelse(. == "  NaN", "", .)} %>% paste0("%"))) %>% 
  write_csv2("output/tables/proportion_incidence.csv")
  
proportion_plot %>% 
  mutate(burden = fct_relabel(burden, .fun = ~gsub("_", "\n", .x))) %>% 
  ggplot(aes(x = Adjustment, y = Inc1e5, fill = Virus)) + geom_bar(stat = "identity", position = "fill") + 
  facet_grid(.~burden) + 
  scale_fill_manual(values = virus_col) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "Proportion contribution", x = "") + 
  theme_bw()


ggsave("output/plots/proportion_incidence.png", width = 25, height = 10, units = "cm", dpi = 600)


proportion_plot_season <- patient_outcomes_inc_master %>% 
  # filter(!(seasonAdmitted %in% c("20/21", "21/22"))) %>%
  group_by(Virus, Adjustment, type_sej, burden, seasonAdmitted) %>% 
  summarise(n = sum(n)) %>% 
  left_join(catchmentPop %>% group_by(seasonAdmitted, type_sej) %>% summarise(catchmentHosp = sum(catchmentHosp)), by = c("seasonAdmitted", "type_sej")) %>% 
  mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
  ungroup %>% 
  group_by(Virus, Adjustment, type_sej, seasonAdmitted, burden) %>% 
  summarise(Inc1e5 = mean(Inc1e5, na.rm = T)) %>% 
  filter(Adjustment %in% c("knownCommunity", "afterCorrection")) %>% 
  mutate(Adjustment = ifelse(Adjustment == "knownCommunity", "Unadjusted", "Adjusted") %>% factor(levels = c("Unadjusted", "Adjusted"))) %>% 
  
  filter((burden %in% c("Emergency_only", "Hospital_admission", "Hospital_ICU")) | #this applies the 30 day death correction only to the adjusted numbers
           (burden == "Hospital_deathRaw" & Adjustment == "Unadjusted") |
           (burden == "Hospital_deathPlus30" & Adjustment == "Adjusted")) %>% 
  mutate(burden = fct_collapse(burden, Hospital_death = c("Hospital_deathRaw", "Hospital_deathPlus30")))

proportion_plot_season %>% 
  group_by(burden, seasonAdmitted, Adjustment) %>% 
  mutate(p = Inc1e5/sum(Inc1e5, na.rm = T)) %>% 
  select(Adjustment, burden, seasonAdmitted, Virus, p) %>% 
  arrange(burden, Virus, seasonAdmitted, Adjustment) %>% 
  
  mutate(p = (p*100) %>% round(1) %>% format(nsmall = 1) %>% {ifelse(. == "  NaN", "", .)} %>% paste0("%")) %>% 
  pivot_wider(id_cols = c("burden", "Virus"), names_from = c("seasonAdmitted", "Adjustment"), values_from = "p") %>% 
  arrange(burden) %>% 
  write_csv2("output/tables/proportion_incidence_season.csv")


proportion_plot_season %>% 
  mutate(burden = fct_relabel(burden, .fun = ~gsub("_", "\n", .x))) %>% 
  ggplot(aes(x = Adjustment, y = Inc1e5, fill = Virus)) + geom_bar(stat = "identity", position = "fill") + 
  facet_grid(burden~seasonAdmitted) + 
  scale_fill_manual(values = virus_col) + 
  # scale_alpha_manual(values = c(Unadjusted = 0.7, Adjusted = 1)) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "Proportion contribution", x = "") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/plots/proportion_incidence_season.png", width = 25, height = 25, units = "cm", dpi = 600)


proportion_plot_age <- patient_outcomes_inc_master %>% 
  # filter(!(seasonAdmitted %in% c("20/21", "21/22"))) %>%
  filter(!(seasonAdmitted %in% c("16/17", "17/18", "18/19", "19/20", "20/21", "21/22"))) %>%
  group_by(Virus, Adjustment, type_sej, burden, age_group, seasonAdmitted) %>% 
  summarise(n = sum(n)) %>% 
  left_join(catchmentPop %>% group_by(age_group, seasonAdmitted, type_sej) %>% summarise(catchmentHosp = sum(catchmentHosp)), by = c("age_group", "seasonAdmitted", "type_sej")) %>% 
  mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
  group_by(Virus, Adjustment, type_sej, age_group, burden) %>% 
  summarise(Inc1e5 = mean(Inc1e5, na.rm = T)) %>% 
  filter(Adjustment %in% c("knownCommunity", "afterCorrection")) %>% 
  mutate(Adjustment = ifelse(Adjustment == "knownCommunity", "Unadjusted", "Adjusted") %>% factor(levels = c("Unadjusted", "Adjusted"))) %>% 
  
  filter((burden %in% c("Emergency_only", "Hospital_admission", "Hospital_ICU")) | #this applies the 30 day death correction only to the adjusted numbers
           (burden == "Hospital_deathRaw" & Adjustment == "Unadjusted") |
           (burden == "Hospital_deathPlus30" & Adjustment == "Adjusted")) %>% 
  mutate(burden = fct_collapse(burden, Hospital_death = c("Hospital_deathRaw", "Hospital_deathPlus30")))


proportion_plot_age %>% 
  group_by(burden, age_group, Adjustment) %>% 
  mutate(p = Inc1e5/sum(Inc1e5, na.rm = T)) %>% 
  select(Adjustment, burden, age_group, Virus, p) %>% 
  arrange(burden, Virus, age_group, Adjustment) %>% 
  mutate(p = (p*100) %>% round(1) %>% format(nsmall = 1) %>% {ifelse(. == "  NaN", "", .)} %>% paste0("%")) %>% 
  pivot_wider(id_cols = c("burden", "Virus"), names_from = c("Adjustment", "age_group"), values_from = "p") %>% 
  arrange(burden) %>%
  write_csv2("output/tables/proportion_incidence_age.csv")

proportion_plot_age %>% 
  mutate(burden = fct_relabel(burden, .fun = ~gsub("_", "\n", .x))) %>% 
  ggplot(aes(x = Adjustment, y = Inc1e5, fill = Virus)) + geom_bar(stat = "identity", position = "fill") + 
  facet_grid(burden~age_group) + 
  scale_fill_manual(values = virus_col) + 
  # scale_alpha_manual(values = c(Unadjusted = 0.7, Adjusted = 1)) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "Proportion contribution", x = "") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/plots/proportion_incidence_age.png", width = 25, height = 25, units = "cm", dpi = 600)


# pct_contribution_age <- patient_outcomes_inc_master %>% 
#   # mutate(Virus = fct_rev(Virus)) %>% 
#   group_by(Virus, age_group, Adjustment, type_sej, burden, seasonAdmitted) %>% 
#   summarise(n = sum(n)) %>% 
#   left_join(catchmentPop, by = c("seasonAdmitted", "age_group", "type_sej")) %>% 
#   mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
#   ungroup %>% 
#   filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
#   filter(Adjustment %in% c("knownCommunity", "afterCorrection")) %>% 
#   mutate(Adjustment = ifelse(Adjustment == "knownCommunity", "Unadjusted", "Adjusted") %>% factor(levels = c("Unadjusted", "Adjusted"))) %>% 
#   # mutate(Adjustment = fct_rev(Adjustment)) %>% 
#   group_by(Virus, age_group, Adjustment, seasonAdmitted) %>% #sum over emergency and hospital admissions
#   summarise(Inc1e5 = sum(Inc1e5)) %>% 
#   filter(!(seasonAdmitted %in% c("20/21", "21/22"))) %>% 
#   group_by(Virus, age_group, Adjustment) %>% 
#   summarise(Inc1e5 = mean(Inc1e5, na.rm = T)) %>% 
#   group_by(age_group, Adjustment) %>% 
#   mutate(p_inc = Inc1e5/sum(Inc1e5))
# 
# 
# pct_contribution <- patient_outcomes_inc_master %>% 
#   # mutate(Virus = fct_rev(Virus)) %>% 
#   group_by(Virus, Adjustment, type_sej, burden, seasonAdmitted) %>% 
#   summarise(n = sum(n)) %>% 
#   left_join(catchmentPop %>% group_by(seasonAdmitted, type_sej) %>% summarise(catchmentHosp = sum(catchmentHosp)), by = c("seasonAdmitted", "type_sej")) %>% 
#   mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
#   ungroup %>% 
#   filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
#   filter(Adjustment %in% c("knownCommunity", "afterCorrection")) %>% 
#   mutate(Adjustment = ifelse(Adjustment == "knownCommunity", "Unadjusted", "Adjusted") %>% factor(levels = c("Unadjusted", "Adjusted"))) %>% 
#   # mutate(Adjustment = fct_rev(Adjustment)) %>% 
#   group_by(Virus, Adjustment, seasonAdmitted) %>% #sum over emergency and hospital admissions
#   summarise(Inc1e5 = sum(Inc1e5)) %>% 
#   filter(!(seasonAdmitted %in% c("20/21", "21/22"))) %>% 
#   group_by(Virus, Adjustment) %>% 
#   summarise(Inc1e5 = mean(Inc1e5, na.rm = T)) %>% 
#   group_by(Adjustment) %>% 
#   mutate(p_inc = Inc1e5/sum(Inc1e5)) %>% 
#   mutate(age_group = "All ages")




# pct_contrib1 <- ggplot() + 
#   geom_bar(data = pct_contribution, aes(y = p_inc, x = Virus, fill = Adjustment), stat = "identity", position = "dodge") + 
#   # geom_text(aes(label = paste0(round(p_inc*100, 1), "%")), nudge_x = 0.05) + 
#   geom_text(data = pct_contribution %>% filter(Adjustment == "Unadjusted"), 
#             aes(y = p_inc, x = Virus, colour = "Unadjusted", label = paste0(round(p_inc*100, 1), "%")), nudge_y = 0.02, nudge_x = -0.2, size = 1.5) + 
#   geom_text(data = pct_contribution %>% filter(Adjustment == "Adjusted"), 
#             aes(y = p_inc, x = Virus, colour = "Adjusted", label = paste0(round(p_inc*100, 1), "%")), nudge_y = 0.02, nudge_x = 0.2, size = 1.5) + 
#   scale_y_continuous(labels = scales::percent) + 
#   scale_fill_manual(values = c(Unadjusted = "#F8766D", Adjusted = "#00BFC4")) + 
#   scale_colour_manual(values = c(Unadjusted = "#F8766D", Adjusted = "#00BFC4")) + 
#   guides(colour = F, fill = F) + 
#   facet_wrap(age_group~.) + 
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
#   labs(x = "", y = "")
# 
# pct_contrib1
# 
# pct_contrib2 <- ggplot() + 
#   geom_bar(data = pct_contribution_age, aes(y = p_inc, x = Virus, fill = Adjustment), stat = "identity", position = "dodge") + 
#   # geom_text(aes(label = paste0(round(p_inc*100, 1), "%")), nudge_x = 0.05) + 
#   geom_text(data = pct_contribution_age %>% filter(Adjustment == "Unadjusted"), 
#             aes(y = p_inc, x = Virus, colour = "Unadjusted", label = paste0(round(p_inc*100, 1), "%")), nudge_y = 0.1, nudge_x = -0.2, size = 1.3) + 
#   geom_text(data = pct_contribution_age %>% filter(Adjustment == "Adjusted"), 
#             aes(y = p_inc, x = Virus, colour = "Adjusted", label = paste0(round(p_inc*100, 1), "%")), nudge_y = 0.1, nudge_x = 0.2, size = 1.3) + 
#   scale_y_continuous(labels = scales::percent) + 
#   scale_fill_manual(values = c(Unadjusted = "#F8766D", Adjusted = "#00BFC4")) + 
#   scale_colour_manual(values = c(Unadjusted = "#F8766D", Adjusted = "#00BFC4")) + 
#   guides(colour = F) + 
#   facet_wrap(age_group~., ncol = 2) + 
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
#   labs(x = "", y = "")
# 
# pct_contrib2
# 
# 
# ggpubr::ggarrange(pct_contrib1, pct_contrib2, ncol = 2, widths = c(2, 4))
# 
# ggsave(paste0("output/plots/pct_contribution_plot.png"), width = 30, height = 20, units = "cm")




# citation(package = "xgboost")

patient_outcomes_inc_master %>% 
  group_by(Virus, Adjustment, type_sej, burden, seasonAdmitted, monthAdmitted) %>% 
  summarise(n = sum(n)) %>% 
  left_join(catchmentPop %>% group_by(seasonAdmitted, type_sej) %>% summarise(catchmentHosp = sum(catchmentHosp)), by = c("seasonAdmitted", "type_sej")) %>% 
  mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
  ungroup %>% 
  filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  group_by(Virus, Adjustment, seasonAdmitted, monthAdmitted) %>% 
  summarise(Inc1e5 = sum(Inc1e5)) %>% 
  # mutate(burden = fct_relabel(burden, .fun = ~gsub("_", "\n", .x))) %>% 
  filter(Adjustment %in% c("knownCommunity", "afterCorrection")) %>% 
  mutate(Adjustment = fct_recode(Adjustment, Unadjusted = "knownCommunity", Adjusted = "afterCorrection")) %>% 
  group_by(monthAdmitted, Adjustment) %>% 
  mutate(totalInc1e5 = sum(Inc1e5, na.rm = T)) %>% 
  left_join(data_sejours %>% 
              rename(monthAdmitted = month_admitted, seasonAdmitted = season_admitted) %>% 
              group_by(type_sej, seasonAdmitted, monthAdmitted) %>% 
              summarise(FIRESPtotal = n_distinct(IPP_sejour)) %>% 
              left_join(catchmentPop %>% group_by(type_sej, seasonAdmitted) %>% summarise(catchmentHosp = sum(catchmentHosp))) %>% 
              mutate(FIRESPInc1e5 = FIRESPtotal/catchmentHosp*1e5) %>% 
              group_by(monthAdmitted) %>% 
              summarise(FIRESPInc1e5 = sum(FIRESPInc1e5))) %>% 
  # filter((burden %in% c("Emergency_only", "Hospital_admission"))) %>%
  ggplot(aes(x = monthAdmitted)) + 
  geom_area(aes(y = Inc1e5, fill = Virus)) + 
  # geom_line(aes(y = totalInc1e5, linetype = "Total Inc")) + 
  geom_line(aes(y = FIRESPInc1e5, colour = "Cohort total")) + 
  # geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(Adjustment~.
             # , scales = "free_y"
  ) + 
  scale_fill_manual(values = virus_col) +
  # scale_linetype_manual(values = c(`ARI in primary care` = "solid", `ARI 95% CI` = "dotted")) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  scale_colour_manual(values = c(`FIRESP total` = "black")) + 
  labs(x = "Month admitted", y = "Incidence per 100 000 catchment population per month", colour = "")

ggsave("output/plots/area_vs_firesptotal.png", width = 25, height = 11, units = "cm", dpi = 600)





