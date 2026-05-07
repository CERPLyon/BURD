library(tidyverse)

deaths_age_dp <- read_csv2("~/Team CERP/Aby Ba/SNDS/20250710_175103_ABOU-CHAKRA-00045-4210899_00_8572_SYNTHESE_DEATH.csv") %>% 
  mutate(AGE_GROUP = case_when(AGE_GROUP == "00-01" ~ "0-1", 
                               AGE_GROUP == "02-04" ~ "2-4", 
                               AGE_GROUP == "05-14" ~ "5-14", 
                               T ~ AGE_GROUP)) %>% 
  mutate(AGE_GROUP = factor(AGE_GROUP, age_vec)) %>% 
  transmute(year_discharged = DISHARGE_YEAR, age_group = AGE_GROUP, DP, Death_inHCL = DEATH_IN_HCL, Death_30days = DEATG_30DAYS) %>% 
  filter(!is.na(age_group)) %>% 
  full_join(expand.grid(list(year_discharged = 2016:2023, 
                             age_group = age_vec, 
                             DP = DP_fullList$value))) %>% 
  mutate(across(starts_with("Death"), ~replace_na(.x, 0))) %>% 
  arrange(year_discharged, age_group, DP)


# calculate death rates by age and DP (with a frequency measure to troubleshoot stochasticity)
mort <- read_csv("../CatchmentPop/mortality.csv") %>% 
  # rename(year_admitted = Year) %>% 
  separate(col = age_gr, into = c("from", "to"), convert = T) %>% 
  mutate(to = replace_na(to, replace = 0)) %>% 
  mutate(to = ifelse(to == 110, 121, to)) %>% 
  rename(background_mort = mort_rate) %>% 
  {rbind(., # assume mortality in 2023 is the same as in 2022 (it's changeable)
         filter(., Year == 2022) %>% mutate(Year = 2023))}

DP_age_death_rate <- patient_outcomes %>% 
  left_join(data_diag_sejour_NDA %>% #merge with all DP values from the diagnostics table
              filter(name == "DP") %>% 
              transmute(IPP_sejour, DP = value) %>% 
              unique) %>% 
  group_by(age_group, DP) %>% 
  summarise(deaths = sum(Death), death_rate = mean(Death), nDP = n()) %>% 
  ungroup %>% 
  left_join(deaths_age_dp %>%  #merge in the SNDS data
              group_by(DP, age_group) %>% 
              summarise(SNDSdeathsHCL = sum(Death_inHCL),
                        SNDSdeaths30days = sum(Death_30days))) %>% 
  # mutate(death_correction_factor = case_when(SNDSdeathsHCL == 0 & SNDSdeaths30days == 0 ~ 0, 
  #                                            SNDSdeaths30days > SNDSdeathsHCL ~ 1, 
  #                                            T ~ SNDSdeaths30days/SNDSdeathsHCL)) %>% 
  mutate(add_deaths = deaths*SNDSdeaths30days/SNDSdeathsHCL) %>% # correct number of deaths
  mutate(add_deaths2 = pmin(SNDSdeaths30days, add_deaths)) %>% #cap this at the number of 30day deaths
  mutate(add_deaths3 = ifelse(SNDSdeathsHCL == 0, SNDSdeaths30days, add_deaths2)) %>% #and set it to this number if  SNDSdeathsHCL is zero
  mutate(add_deaths4 = ifelse(is.na(DP), 0, add_deaths3)) %>% 
  mutate(add_deaths_per_undead = add_deaths4/(nDP - deaths)) %>% #split this between the people who didn't die in FIRESP
  mutate(add_deaths_per_undead2 = ifelse(nDP == deaths, 0, add_deaths_per_undead)) %>%  #don't do a correction if everyone with this DP died anyway
  mutate(add_deaths_per_undead3 = pmin(add_deaths_per_undead2, 1)) %>%  # this shouldn't go above 1
  # View
  mutate(add_deaths = add_deaths4, add_deaths_per_undead = add_deaths_per_undead3)

DP_age_death_rate <- DP_age_death_rate%>% 
  select(-add_deaths2, -add_deaths3, -add_deaths4, -add_deaths_per_undead2, -add_deaths_per_undead3)


patient_outcomes_DP <- patient_outcomes %>% 
  mutate(year_discharged = year(date_sor_sejour)) %>% 
  left_join(data_diag_sejour_NDA %>% #merge with all DP values from the diagnostics table
              filter(name == "DP") %>% 
              transmute(IPP_sejour, DP = value) %>% 
              unique) %>% 
  left_join(DP_age_death_rate) %>%
  group_by(IPP_sejour) %>% 
  # arrange(IPP_sejour, -SNDSdeathsHCL/nDP) %>% # then select the DP with the greatest number of deaths in HCL per DP in the data
  arrange(IPP_sejour, -deaths/nDP) %>% # then select the DP with the greatest number of deaths in HCL per DP in the data
  group_by(IPP_sejour) %>% 
  dplyr::slice(1) %>% 
  ungroup
  # select(-DPfreq, -DPdeaths) #now take out these two variables


# DP_age_death_rate %>% 
#   filter(is.na(add_deaths_per_undead)) %>% 
#   View
# patient_outcomes_DP %>% 
#   filter(is.na(add_deaths_per_undead)) %>% 
#   View

nDP_threshold = 5
# SNDSdeathsHCL_threshold = 1

patient_outcomes_DP_deathCorrection <- patient_outcomes_DP %>% 
  left_join(mort, by = join_by("year_admitted" == "Year", between(age, from, to))) %>% 
  mutate(background_mort_30day = 1-((1-background_mort)^(1/365))^30
         # , background_mort_30day = background_mort*30/365
  ) %>% 
  mutate(add_deaths_per_undead_minusBackground = pmax(0, add_deaths_per_undead - background_mort_30day)) %>% 
  mutate(Death_corrected = case_when(Death == 1 ~ 1, 
                                     nDP < nDP_threshold ~ 0,
                                     # SNDSdeathsHCL < SNDSdeathsHCL_threshold ~ 0, 
                                     T ~ add_deaths_per_undead_minusBackground)) %>% 
  select(-(deaths:add_deaths_per_undead_minusBackground))


patient_outcomes_DP_deathCorrection %>% 
  group_by(age_group) %>% 
  summarise(across(starts_with("Death", ignore.case = F), sum))

patient_outcomes_DP_deathCorrection %>% 
  select(IPP_sejour, Death, Death_corrected) %>% 
  write_csv("BURD/deathCorrection.csv")


DP_age_death_rate %>% 
  arrange(-deaths) %>% 
  dplyr::slice(1:10) %>% 
  {rbind(., dplyr::slice(.[NA, ], 1))} %>% 
  {rbind(., DP_age_death_rate %>% 
           filter(nDP > 10) %>% 
           arrange(-death_rate) %>% 
           dplyr::slice(1:10), 
         dplyr::slice(.[NA, ], 1))} %>% 
  {rbind(., DP_age_death_rate %>% 
           filter(add_deaths < nDP - deaths) %>% 
           # filter(nDP > 10) %>% 
           arrange(-add_deaths) %>% 
           dplyr::slice(1:10)
  )} %>% 
  transmute(`Age group`=age_group, `Principle diagnostic code`=DP, MFH=deaths, AFH=nDP, `MFH/AFH`=death_rate, 
            MSH=SNDSdeathsHCL, MSD=SNDSdeaths30days, MaxAddDeaths=add_deaths, IndAddDeaths=add_deaths_per_undead) %>% 
  write_csv2("BURD/deathCorrection_top10s.csv")




