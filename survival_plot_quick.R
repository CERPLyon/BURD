library(tidyverse)
library(survival)
surv_death_data <- data_sejours %>% 
  filter(age_group == "80+", season_admitted == "23/24") %>% 
  group_by(IPP_sejour) %>% 
  transmute(LoS = duree_sejour, 
            mode = ifelse(any(mode_sor %in% "DEATH"), "Death", "Discharge")
            # mode = as.numeric(any(mode_sor %in% "DEATH"))
            ) %>% 
  unique

surv_death_data %>% 
  filter(mode != "Death") %>% 
  {survfit(Surv(time = .$LoS, event = .$mode == "Discharge") ~ 1)} %>% 
  plot(main = "Discharges")

png(filename = "output/plots/surv_80_2425.png", width = 20, height = 12, units = "cm", res = 300)
surv_death_data %>% 
  # filter(mode != "Death") %>% 
  {survfit(Surv(time = .$LoS, event = .$mode == "Death") ~ 1)} %>% 
  plot(main = "Survival in 80+ in 2024/25", xlab = "Days after admission")

dev.off()


