source("b0.R")

deaths_age_dp_raw <- read_csv2("~/Team CERP/Aby Ba/SNDS/HCL_FIRESP_AGG_extrac_21_04.csv")
deaths_age_dp_raw <- read_csv2("~/Team CERP/Aby Ba/SNDS/HCL_FIRESP_AGG_extrac_12_05.csv")


deaths_age_dp <- deaths_age_dp_raw %>% 
  mutate(AGE_GROUP = case_when(AGE_GROUP == "00-01" ~ "0-1", 
                               AGE_GROUP == "02-04" ~ "2-4", 
                               AGE_GROUP == "05-17" ~ "5-17", 
                               T ~ AGE_GROUP)) %>% 
  mutate(AGE_GROUP = factor(AGE_GROUP, age_vec)) %>% 
  transmute(YearDischarged = SOR_ANN, 
            DP = DGN_PAL, 
            age_group = AGE_GROUP, 
            death_inHCL = nb_deces_hcl, 
            death_within30 = nb_deces_30j, 
            death_after30 = nb_deces_30_af, 
            total_deaths = death_inHCL + death_within30 + death_after30, 
            total_dp = nb_total)


# dp_deaths_FIRESP = data_sejours %>% 
#   transmute(age_group, IPP_alea , type_sej, DP, Death = as.numeric(mode_sor == "DEATH"), YearDischarged = year(date_sor_sejour), date_sor_sejour) %>% 
#   arrange(today() - date_sor_sejour) %>% 
#   group_by(IPP_alea) %>% 
#   slice(1) %>% 
#   ungroup %>% 
#   mutate(dp1 = substr(DP, 1, 1), dp2 = substr(DP, 1, 2), dp3 = substr(DP, 1, 3)) %>% 
#   group_by(YearDischarged
#            , age_group
#            , type_sej
#            , dp1
#            , dp2
#            , dp3
#            , DP
#   ) %>% 
#   summarise(deaths = sum(Death), nDP = n()) %>% 
#   ungroup

dp_deaths_FIRESP <- data_sejours %>% 
  # filter(type_sej == "HOSPITALISATION") %>% 
  group_by(IPP_sejour) %>% 
  mutate(DeathSejour = as.numeric(any(mode_sor == "DEATH")), YearDischarged = year(date_sor_sejour)) %>% 
  transmute(age_group, IPP_alea , IPP_sejour , type_sej, DP, mode_sor, YearDischarged, DeathSejour) %>% 
  mutate(dp1 = substr(DP, 1, 1), dp2 = substr(DP, 1, 2), dp3 = substr(DP, 1, 3)) %>% 
  # filter(IPP_alea == zz) %>%
  group_by(YearDischarged
           , age_group
           , type_sej
           , dp1
           , dp2
           , dp3
           , DP
  ) %>% 
  summarise(deaths = n_distinct(IPP_sejour[DeathSejour %in% 1]), nDP = n_distinct(IPP_sejour))




dp_deaths_SNDS <- deaths_age_dp %>% 
  mutate(dp1 = substr(DP, 1, 1), dp2 = substr(DP, 1, 2), dp3 = substr(DP, 1, 3), DP = paste0(DP, "_")) %>% 
  group_by(YearDischarged, age_group
           , dp1
           , dp2
           , dp3
           , DP
  ) %>% 
  summarise(across(death_inHCL:total_dp, sum)) %>% 
  ungroup


inclusion_string <- paste0(c(
  paste0(gsub("[.]", "", inclusion_code$code), "_"), 
  na.omit(inclusion_code$code_group)), 
  collapse = "|")
full_join(dp_deaths_FIRESP %>% 
            filter(type_sej == "HOSPITALISATION") %>% 
            group_by(YearDischarged, age_group
                     , dp1
                     , dp2
                     , dp3
                     , DP
            ) %>% 
            summarise(deaths = sum(deaths), nDP = sum(nDP)) %>% 
            mutate(deathRate_FIRESP = deaths/nDP) 
          , 
          dp_deaths_SNDS %>% 
            group_by(YearDischarged, age_group
                     , dp1
                     , dp2
                     , dp3
                     , DP
            ) %>% 
            summarise(across(c("death_inHCL", "death_within30", "total_dp"), sum)) %>% 
            mutate(deathRate_inHCL = death_inHCL/total_dp, 
                   deathRate_within30 = death_within30/total_dp)
          
) %>% 
  filter(grepl(pattern = inclusion_string, DP))

dp_deaths_joined %>% 
  filter(deathRate_FIRESP > 0.05, deathRate_inHCL == 0)

dp_deaths_joined %>% 
  filter(YearDischarged %in% c(2017:2024)) %>% 
  # ggplot(aes(x = deaths, y = death_inHCL)) +
  mutate(problem = (deathRate_FIRESP > 0.05) & (deathRate_inHCL == 0)) %>%
  mutate(problem2 = (nDP > 1700) & (total_dp < 500)) %>% 
  filter(problem2) %>% 
  View
  # filter(is.na(problem)) %>% View
  # filter(problem) %>% View
  # ggplot(aes(x = deathRate_FIRESP, y = deathRate_inHCL, colour = problem)) +
  ggplot(aes(x = nDP, y = total_dp)) +
  geom_point(alpha = 0.2) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") + 
  geom_smooth(method = "lm") + 
  facet_wrap(.~age_group) + 
  theme_bw() #+
  # scale_y_log10() + scale_x_log10()
  
  dp_deaths_joined %>% 
    # ggplot(aes(x = deaths, y = death_inHCL)) +
    mutate(problem = (deathRate_FIRESP > 0.05) & (deathRate_inHCL == 0)) %>% 
    # filter(is.na(problem)) %>% 
    filter(problem) %>% 
    View
    ggplot(aes(x = deaths, fill = problem)) + geom_histogram()
