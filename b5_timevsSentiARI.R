
library(ISOweek)


adjusted_outcomes_month <- senstestdeathadjusted_burden %>% 
  select(-sex, -infected_rfPredicted) %>% 
  mutate(Year = paste0("20", gsub(".*/", "", season_admitted)) %>% as.numeric) %>% 
  left_join(catchment_rhone_interp %>% filter(sex == "MF"), by = c("age_group", "Year")) %>% 
  mutate(across(starts_with("infected"), list(adm = ~.x/Pop*1e5, 
                                              icu = ~.x/Pop*1e5*ICU, 
                                              mort = ~.x/Pop*1e5*Death, 
                                              Death_mort = ~.x/Pop*1e5*Death_corrected))) %>% 
  select(-infected_unadjusted_Death_mort, -infected_sensAdjusted_Death_mort) %>% 
  # rename(infected_deathAdjusted_mort = infected_unadjusted_Death_mort, 
  #        infected_sensDeathAdjusted_mort = infected_sensAdjusted_Death_mort) %>% 
  rename(infected_sensTestDeathAdjusted_mort = infected_sensTestAdjusted_Death_mort) %>% 
  ungroup %>% 
  rename_all(~gsub("infected_", "inc_", .x)) %>% 
  # filter(year_admitted %in% 2017:2022) %>% 
  group_by(Virus, age_group, month_admitted) %>% 
  summarise(across(starts_with("inc"), sum), Pop = Pop[1]) %>% 
  filter((Virus != "SARS-CoV-2") | (year(month_admitted) >= 2020))

adjusted_outcomes_month_long <- adjusted_outcomes_month %>% 
  pivot_longer(-c(Virus, age_group, month_admitted, Pop), names_sep = "_", names_to = c(".value", "Adjustment", "Outcome")) %>% 
  filter(!is.na(Outcome)) %>% 
  mutate(Virus = factor(Virus, virus_order), 
         age_group = factor(age_group, age_vec), 
         Adjustment = factor(Adjustment, c("unadjusted", "sensAdjusted", "sensTestAdjusted", "sensTestDeathAdjusted")), 
         Adjustment = fct_recode(Adjustment, 
                                 Unadjusted = "unadjusted", 
                                 S = "sensAdjusted", 
                                 `S-T` = "sensTestAdjusted", 
                                 `S-T-D` = "sensTestDeathAdjusted")) %>% 
  arrange(Virus, month_admitted, age_group, Outcome)

adj_col = RColorBrewer::brewer.pal(name = "Set1", n = 4) %>% 
  setNames(levels(adjusted_outcomes_month_long$Adjustment))

chosen_outcome = "adm"


senti <- read_csv("../SentinelARI/inc-25-RDD-ds2.csv", skip = 1) %>% 
  filter(geo_insee == 84) %>% 
  mutate(YearWeek = gsub("([0-9]{4})([0-9]{2})", "\\1-W\\2-1", week)) %>% 
  mutate(week_beginning = ISOweek2date(YearWeek), 
         week_mid = week_beginning) %>% 
  transmute(week_mid, ARI1e5=inc100, ARIlo=inc100_low, ARIhi=inc100_up)





adjusted_outcomes_month_long %>% 
  filter(Virus != "InfluenzaNT") %>% 
  filter(Outcome == chosen_outcome) %>% 
  # filter(Virus == "SARS-CoV-2") %>% 
  filter((Adjustment %in% "Unadjusted") | ((Adjustment %in% "S-T") & Outcome != "mort") | ((Adjustment %in% "S-T-D") & Outcome == "mort")) %>%
  ggplot(aes(x = month_admitted, y = inc)) + 
  geom_line(aes(colour = Adjustment)) + 
  facet_grid(age_group ~ Virus, scales = "free_y") + 
  scale_colour_manual(values = adj_col) +
  theme_bw() + 
  labs(x = "Month", 
       y = paste0(case_match(chosen_outcome, "adm" ~ "Admissions", 
                             "icu" ~ "ICU admissions", 
                             "mort" ~ "Deaths"), " per 100 000")
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")#+ 
  # theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1.1), 
  #       panel.grid = element_blank())

chosen_outcome = "adm"

adjusted_outcomes_month_long %>% 
  group_by(Virus, month_admitted, Adjustment, Outcome) %>% 
  summarise(inc = sum(inc*Pop)/sum(Pop)) %>% 
  filter(Virus != "InfluenzaNT") %>% 
  filter(Outcome == chosen_outcome) %>% 
  # filter(Virus == "SARS-CoV-2") %>% 
  filter((Adjustment %in% "Unadjusted") | ((Adjustment %in% "S-T") & Outcome != "mort") | ((Adjustment %in% "S-T-D") & Outcome == "mort")) %>%
  mutate(Adjustment = fct_recode(Adjustment, "Adjusted" = "S-T")) %>% 
  left_join(senti %>% 
              mutate(month = floor_date(week_mid, unit = "month") + 15) %>% 
              group_by(month) %>% 
              summarise(across(starts_with("ARI"), mean)) %>% 
              rename(month_admitted = month)) %>% 
  ggplot(aes(x = month_admitted)) + 
  geom_area(aes(y = inc, fill = Virus)) + 
  geom_line(aes(y = ARI1e5, linetype = "ARI in primary care")) + 
  geom_line(aes(y = ARIlo, linetype = "ARI 95% CI")) + 
  geom_line(aes(y = ARIhi, linetype = "ARI 95% CI")) + 
  facet_grid(Adjustment ~ ., scales = "free_y") + 
  # scale_colour_manual(values = adj_col) +
  scale_fill_brewer(palette = "Paired") + 
  scale_linetype_manual(values = c(`ARI in primary care` = "solid", `ARI 95% CI` = "dashed")) + 
  theme_bw() + 
  labs(x = "Month", 
       linetype = "",
       y = paste0(case_match(chosen_outcome, "adm" ~ "Admissions", 
                             "icu" ~ "ICU admissions", 
                             "mort" ~ "Deaths"), " per 100 000")
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

ggsave(paste0("BURD/admissions_vs_sentiARI.png"), width = 20, height = 12, units = "cm")

