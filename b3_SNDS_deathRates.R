source("b0.R")

deaths_age_dp_raw <- read_csv2("~/Team CERP/Aby Ba/SNDS/HCL_FIRESP_AGG_extrac_21_04.csv")

deaths_age_dp <- deaths_age_dp_raw %>% 
  mutate(AGE_GROUP = case_when(AGE_GROUP == "00-01" ~ "0-1", 
                               AGE_GROUP == "02To04" ~ "2-4", 
                               AGE_GROUP == "05To17" ~ "5-17", 
                               T ~ AGE_GROUP)) %>% 
  mutate(AGE_GROUP = factor(AGE_GROUP, age_vec)) %>% 
  transmute(YearDischarged = SOR_ANN, DP = DGN_PAL, age_group = AGE_GROUP, death_inHCL = nb_deces_hcl, death_within30 = nb_deces_30j, death_after30 = nb_deces_30_af, total_deaths = death_inHCL + death_within30 + death_after30, total_dp = nb_total)


dp_deaths_FIRESP = data_sejours %>% 
  transmute(age_group, IPP_sejour , DP, Death = as.numeric(mode_sor == "DEATH"), YearDischarged = year(date_sor_sejour), date_sor_sejour) %>% 
  arrange(today() - date_sor_sejour) %>% 
  group_by(IPP_sejour) %>% 
  slice(1) %>% 
  ungroup %>% 
  mutate(dp1 = substr(DP, 1, 1), dp2 = substr(DP, 1, 2), dp3 = substr(DP, 1, 3)) %>% 
  group_by(YearDischarged
           , age_group
           , dp1
           , dp2
           , dp3
  ) %>% 
  summarise(deaths = sum(Death), nDP = n()) %>% 
  ungroup





dp_deaths_SNDS <- deaths_age_dp %>% 
  mutate(dp1 = substr(DP, 1, 1), dp2 = substr(DP, 1, 2), dp3 = substr(DP, 1, 3)) %>% 
  group_by(YearDischarged, age_group
           , dp1
           , dp2
           , dp3
  ) %>% 
  summarise(across(death_inHCL:total_dp, sum)) %>% 
  ungroup


dp_deaths_FIRESP %>% 
  filter(age_group == "80+", YearDischarged == 2018, dp3 == "J20")

dp_deaths_SNDS %>% 
  filter(age_group == "80+", YearDischarged == 2018, dp3 == "J20")

deaths_age_dp %>% 
  filter(grepl("J20", DP), YearDischarged == 2018, age_group == "80+")


deaths_age_dp %>% 
  filter(grepl("C72", DP)) %>% 
  print(n = 100)

deaths_age_dp$DP %>% sample(1)
deaths_age_dp %>% 
  group_by(YearDischarged) %>% 
  summarise(across(c("total_deaths", "total_dp"), sum))


full_join(dp_deaths_FIRESP %>% 
            group_by(YearDischarged, age_group
                     , dp1
                     , dp2
                     , dp3
            ) %>% 
            summarise(deaths = sum(deaths), nDP = sum(nDP)) %>% 
            mutate(deathRate_FIRESP = deaths/nDP) 
          , 
          dp_deaths_SNDS %>% 
            group_by(YearDischarged, age_group
                     , dp1
                     , dp2
                     , dp3
            ) %>% 
            summarise(across(c("death_inHCL", "death_within30", "total_dp"), sum)) %>% 
            mutate(deathRate_inHCL = death_inHCL/total_dp, 
                   deathRate_within30 = death_within30/total_dp)
          
) %>% 
  ggplot(aes(x = deathRate_FIRESP, y = deathRate_inHCL)) + 
  geom_point(alpha = 0.2) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") + 
  geom_smooth(method = "lm") + 
  facet_wrap(.~age_group) + 
  theme_bw()
