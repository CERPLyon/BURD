source("b0.R")

# https://www.insee.fr/en/statistiques/7748162?sommaire=7751503
# INSEE Demographic balance sheet 2023
# Probability of dying - France
mortality_raw <- rbind(readxl::read_xlsx("data/fe_dod_quotients_mortalite_va.xlsx", sheet = 1, skip = 4, n_max = 30) %>% mutate(Sex = "F"), 
                       readxl::read_xlsx("data/fe_dod_quotients_mortalite_va.xlsx", sheet = 2, skip = 4, n_max = 30) %>% mutate(Sex = "M")) %>% 
  rename_all(~gsub(" yea.*", "", .x)) %>% 
  rename(Year = `...1`) %>% 
  mutate(Year = as.integer(gsub(" .*", "", Year))) %>% 
  pivot_longer(-c("Year", "Sex"), names_to = "Age", values_to = "annualMortality1e5", names_transform = as.numeric)

mortality = mortality_raw %>% 
  mutate(annualMortality = annualMortality1e5/1e5) %>% 
  group_by(Year, Age) %>% 
  summarise(annualMortality = mean(annualMortality)) %>% 
  ungroup

mortality_ageGroup = mortality %>% 
  mutate(age_group = case_when(Age <= 1 ~ "0-1", 
                               Age <= 4 ~ "2-4", 
                               Age <= 17 ~ "5-17", 
                               Age <= 64 ~ "18-64", 
                               Age <= 79 ~ "65-79", 
                               T ~ "80+") %>% factor(levels = age_vec)) %>% 
  group_by(Year, age_group) %>% 
  summarise(annualMortality = sum(annualMortality))

background_mortality <- mortality_ageGroup %>% 
  filter(Year > 2016) %>% 
  group_by(age_group) %>% 
  summarise(annualMortality = mean(annualMortality)) %>% 
  transmute(age_group, mortality30day = annualMortality*30/365.25)


# project forward the latest years
# rbind(mortality_ageGroup,
#       mortality_ageGroup %>% filter(Year == "2023") %>% mutate(Year = 2024),
#       mortality_ageGroup %>% filter(Year == "2023") %>% mutate(Year = 2025)) %>% 
#   mutate(mortality30day = annualMortality*30/365.25)



# # calculate death rates by age and DP (with a frequency measure to troubleshoot stochasticity)
# mort <- read_csv("../CatchmentPop/mortality.csv") %>% 
#   # rename(year_admitted = Year) %>% 
#   separate(col = age_gr, into = c("from", "to"), convert = T) %>% 
#   mutate(to = replace_na(to, replace = 0)) %>% 
#   mutate(to = ifelse(to == 110, 121, to)) %>% 
#   rename(background_mort = mort_rate) %>% 
#   {rbind(., # assume mortality in 2023 is the same as in 2022 (it's changeable)
#          filter(., Year == 2022) %>% mutate(Year = 2023))}

# deaths_age_dp_raw <- read_csv2("~/Team CERP/Aby Ba/SNDS/HCL_FIRESP_AGG_extrac_21_04.csv")
# deaths_age_dp_raw <- read_csv2("~/Team CERP/Aby Ba/SNDS/HCL_FIRESP_AGG_extrac_12_05.csv")
# deaths_age_dp_raw <- read_csv2("~/Team CERP/Aby Ba/SNDS/HCL_FIRESP_AGG_extrac_19_05.csv")
deaths_age_dp_raw <- read_csv2("data/SNDS/HCL_FIRESP_AGG_extrac_19_05.csv") OLD
deaths_age_dp_raw <- read_csv2("data/SNDS/HCL_FIRESP_AGG_extrac_09_07.csv") NEW

deaths_age_SNDS <- deaths_age_dp_raw %>% 
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
            total_dp = nb_total) %>% 
  mutate(dp1 = substr(DP, 1, 1), dp2 = substr(DP, 1, 2), dp3 = substr(DP, 1, 3), DP = paste0(DP, "_")) %>% 
  group_by(YearDischarged, age_group
           , dp1
           , dp2
           , dp3
           , DP
  ) %>% 
  summarise(across(death_inHCL:total_dp, sum)) %>% 
  ungroup

deathRate_age_SNDS <- deaths_age_SNDS %>% 
  group_by(age_group, DP) %>% 
  summarise(across(death_inHCL:total_dp, ~sum(.x, na.rm = T))) %>% 
  mutate(deathRate_inHCL = death_inHCL/total_dp, 
         deathRate_within30 = death_within30/total_dp) %>% 
  ungroup %>% 
  left_join(background_mortality) %>% 
  mutate(deathRate_within30_minusExcess = pmax(0, deathRate_within30 - mortality30day))

deathRate_age_SNDS %>% 
  write_csv2(file = "output/deathRate_age_SNDS.csv")


#### investigation, not essential
dp_deaths_FIRESP <- data_sejours %>% 
    # filter(type_sej == "HOSPITALISATION") %>%
  filter(!(season_admitted %in% c("20/21", "21/22"))) %>% 
  group_by(IPP_sejour) %>%
  mutate(DeathSejour = as.numeric(any(mode_sor == "DEATH")), YearDischarged = year(date_sor_sejour)) %>%
  transmute(age_group, IPP_alea , IPP_sejour , type_sej, DP, mode_sor, YearDischarged, DeathSejour) %>%
  # filter(IPP_alea == zz) %>%
  group_by(age_group
           , YearDischarged
           , type_sej
           , DP
  ) %>%
  summarise(deaths_FIRESP = n_distinct(IPP_sejour[DeathSejour %in% 1]), nDP = n_distinct(IPP_sejour))
# %>% 
#   group_by(age_group, YearDischarged, type_sej, DP) %>% 
#   summarise(deaths_FIRESP = mean(deaths), nDP = mean(nDP))




inclusion_string <- paste0(c(
  paste0(gsub("[.]", "", inclusion_code$code), "_"), 
  na.omit(inclusion_code$code_group)), 
  collapse = "|")

dp_deaths_joined <- full_join(dp_deaths_FIRESP %>% 
            filter(type_sej == "HOSPITALISATION") %>% 
            group_by(YearDischarged, age_group
                     # , dp1
                     # , dp2
                     # , dp3
                     , DP
            ) %>% 
            summarise(deaths_FIRESP = sum(deaths_FIRESP), nDP = sum(nDP)) %>% 
            mutate(deathRate_FIRESP = deaths_FIRESP/nDP) 
          , 
          deaths_age_SNDS %>% 
            group_by(YearDischarged, age_group
                     # , dp1
                     # , dp2
                     # , dp3
                     , DP
            ) %>% 
            summarise(across(c("death_inHCL", "death_within30", "total_dp"), sum)) %>% 
            mutate(deathRate_inHCL = death_inHCL/total_dp, 
                   deathRate_within30 = death_within30/total_dp)
          
) %>% 
  filter(grepl(pattern = inclusion_string, DP))


dp_deaths_joined %>% 
  # filter(YearDischarged %in% c(2017:2024)) %>% 
  # filter(problem2) %>% 
  # View
  # filter(is.na(problem)) %>% View
  # filter(problem) %>% View
  # ggplot(aes(x = deaths, y = death_inHCL)) +
  # ggplot(aes(x = deathRate_FIRESP, y = deathRate_inHCL)) +
  # # ggplot(aes(x = nDP, y = total_dp)) +
  # geom_point(alpha = 0.2) + 
  # geom_smooth(method = "lm") +
  ggpubr::ggscatter(x = 'deathRate_FIRESP',
                    y = 'deathRate_inHCL',
                    # facet.by = "type_sej",
                    color = 'grey50',
                    cor.method = "spearman",
                    cor.coef = TRUE,
                    conf.int = TRUE,
                    size = 0.5,
                    add = "reg.line",
                    add.params = list(color = 'red'),
  ) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  facet_wrap(.~age_group) +
  theme_bw() +
  scale_y_log10() + scale_x_log10() + 
  labs(x = "HCL cohort death rate", y = "SNDS HCL death rate")



ggsave("output/plots/inHosp_deathRates.png", width = 20, height = 12, units = "cm")
  

# dp_deaths_joined %>%
#   # filter(YearDischarged %in% c(2017:2024)) %>%
#   # filter(problem2) %>%
#   # View
#   # filter(is.na(problem)) %>% View
#   # filter(problem) %>% View
#   # ggplot(aes(x = deaths, y = death_inHCL)) +
#   # ggplot(aes(x = deathRate_FIRESP, y = deathRate_inHCL)) +
#   # ggplot(aes(x = nDP, y = total_dp)) +
#   ggplot(aes(x = deathRate_inHCL, y = deathRate_within30)) +
#   geom_point(alpha = 0.2) +
#   geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
#   geom_smooth(method = "lm") +
#   facet_wrap(.~age_group) +
#   theme_bw() +
#   scale_y_log10() + scale_x_log10()
# 
# dp_deaths_joined %>%
#   mutate(ICD10_group = DP) %>%
#   group_by(ICD10_group, YearDischarged, age_group) %>%
#   summarise(across(deaths:total_dp, ~sum(.x, na.rm = T))) %>%
#   mutate(deathRate_FIRESP = deaths/nDP,
#          deathRate_inHCL = death_inHCL/total_dp,
#          deathRate_within30 = death_within30/total_dp) %>%
#   ggplot(aes(x = YearDischarged, y = deathRate_inHCL, group = ICD10_group)) +
#   geom_line(aes(y = deathRate_inHCL, colour = "inHCL")) +
#   geom_line(aes(y = deathRate_FIRESP, colour = "FIRESP")) +
#   geom_line(aes(y = deathRate_within30, colour = "within30")) +
#   facet_wrap(.~age_group)
# 
# dp_deaths_joined %>%
#   mutate(ICD10_group = DP) %>%
#   group_by(ICD10_group, YearDischarged, age_group) %>%
#   summarise(across(deaths:total_dp, ~sum(.x, na.rm = T))) %>%
#   group_by(ICD10_group, age_group) %>%
#   mutate(across(deaths:total_dp, ~mean(.x, na.rm = T))) %>%
#   mutate(deathRate_FIRESP = deaths/nDP,
#          deathRate_inHCL = death_inHCL/total_dp,
#          deathRate_within30 = death_within30/total_dp) %>%
#   ggplot(aes(x = age_group, y = deathRate_inHCL, group = ICD10_group)) +
#   geom_line(aes(y = deathRate_inHCL, colour = "inHCL")) +
#   geom_line(aes(y = deathRate_FIRESP, colour = "FIRESP")) +
#   geom_line(aes(y = deathRate_within30, colour = "within30"))
# 
#   scale_y_log10()
