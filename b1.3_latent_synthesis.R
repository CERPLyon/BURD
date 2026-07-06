library(data.table)

fits = NULL
# this_virus = "InfluenzaA"
for(this_virus in virus_vec){
  this_virus_filename = gsub("/", "", this_virus)
  
  print(this_virus_filename)
  
  print("get master dataset")
  this_dataset_inclPandemic <- data_PCR_sejour_collapseSubtypes %>% 
    # filter(!(season_admitted %in% c("20/21", "21/22"))) %>% #take out the pandemic period for everyone
    # filter((this_virus != "SARS-CoV-2") | (season_admitted %in% c("22/23", "23/24", "24/25"))) %>% #and for COVID make sure it's only post-pandemic
    filter(resultatbis %in% c("POSITIVE", "NEGATIVE")) %>% 
    filter(Virus == this_virus) %>% 
    filter(testday < big_day_limit) %>% 
    group_by(IPP_sejour, age_group, season_admitted) %>% 
    mutate(knownCommunity = as.numeric(any((testday < acute_day_limit) & resultatbis == "POSITIVE"))) %>% 
    ungroup %>% 
    transmute(patient_id = IPP_sejour, age_group, season_admitted, testday, y = (0:1)[match(resultatbis, c("NEGATIVE", "POSITIVE"))], knownCommunity) 
  
  
  print("get dataset of community infections")
  this_dataset_inclPandemic_knownCommunity <- this_dataset_inclPandemic %>% 
    filter(knownCommunity == 1)
  
  
  validParams_piece <- read_csv2(file = paste0("output/piecewise/validParams_age_", this_virus_filename, ".csv"), show_col_types = F)
  validParams_decay_piece <- read_csv2(file = paste0("output/piecewise/validParams_decay_", this_virus_filename, ".csv"), show_col_types = F) %>% 
    mutate(Virus = this_virus)
  fits_piece <- read_csv2(file = paste0("output/piecewise/fits_age_", this_virus_filename, ".csv"), show_col_types = F)
  # estStatus_best <- read_csv2(file = paste0("output/piecewise/estStatus_age_", this_virus_filename, ".csv"))
  

  fits_piece[11:16] <- validParams_decay_piece %>% summarise(across(sensAdm:falsPos, list(min = min, max = max)))
  
  fits_piece[2:4] <- validParams_decay_piece %>% slice(1) %>% select(sensAdm:falsPos)
  
  
  params_best = fits_piece %>% 
    select(sensAdm:`prev80+`) %>% 
    unlist
  
  params_forLoCI = fits_piece %>% 
    transmute(sensAdm = sensAdm_max, 
              dailClr = dailClr_min, 
              falsPos = falsPos_max, 
              `prev0-1` = `prev0-1_min`, 
              `prev2-4` = `prev2-4_min`, 
              `prev5-17` = `prev5-17_min`, 
              `prev18-64` = `prev18-64_min`, 
              `prev65-79` = `prev65-79_min`, 
              `prev80+` = `prev80+_min`) %>% 
    unlist
  
  params_forHiCI = fits_piece %>% 
    transmute(sensAdm = sensAdm_min, 
              dailClr = dailClr_max, 
              falsPos = falsPos_min, 
              `prev0-1` = `prev0-1_max`, 
              `prev2-4` = `prev2-4_max`, 
              `prev5-17` = `prev5-17_max`, 
              `prev18-64` = `prev18-64_max`, 
              `prev65-79` = `prev65-79_max`, 
              `prev80+` = `prev80+_max`) %>% 
    unlist
  
  
  # calc_loglik_wrapper_age(fit_pars = validParams_piece %>% select(sensAdm:`prev80+`) %>% 
  #                           # mutate(`prev0-1` = `prev0-1` - 0.001) %>%
  #                           mutate(`prev80+` = `prev80+`) %>%
  #                           slice(1) %>% 
  #                           unlist, 
  #                         unfit_pars = NULL, 
  #                         FUN = calculate_remaining_sensitivity, 
  #                         dt = as.data.table(this_dataset), return_loglik = T)
  
  estStatus_best_piece <- calc_loglik_wrapper_age(fit_pars = params_best, unfit_pars = NULL, 
                                            FUN = calculate_remaining_sensitivity, 
                                            dt = as.data.table(this_dataset_inclPandemic), return_pinfected = T) %>% 
    mutate(Virus = this_virus)
  
  
  estStatus_lo_piece <- calc_loglik_wrapper_age(fit_pars = params_forLoCI, unfit_pars = NULL, 
                                                FUN = calculate_remaining_sensitivity, 
                                                dt = as.data.table(this_dataset_inclPandemic), return_pinfected = T) %>% 
    mutate(Virus = this_virus) %>% 
    rename(p_infected_lo = p_infected)
  
  
  estStatus_hi_piece <- calc_loglik_wrapper_age(fit_pars = params_forHiCI, unfit_pars = NULL, 
                                                FUN = calculate_remaining_sensitivity, 
                                                dt = as.data.table(this_dataset_inclPandemic), return_pinfected = T) %>% 
    mutate(Virus = this_virus) %>% 
    rename(p_infected_hi = p_infected)
  
  estStatus_piece = estStatus_best_piece %>% 
    left_join(estStatus_lo_piece, by = c("patient_id", "Virus")) %>% 
    left_join(estStatus_hi_piece, by = c("patient_id", "Virus"))  %>% 
    mutate(p_infected_lo = ifelse(p_infected < p_infected_lo, p_infected, p_infected_lo), 
           p_infected_hi = ifelse(p_infected > p_infected_hi, p_infected, p_infected_hi))
  
  if(is.null(fits)){
    fits = fits_piece
    estStatus = estStatus_piece
    validParams <- bind_rows(validParams_piece, validParams_decay_piece)
  } else {
    fits <- fits %>% rows_upsert(fits_piece, by = "Virus")
    estStatus <- estStatus %>% rows_upsert(estStatus_piece, by = c("Virus", "patient_id"))
    validParams <- bind_rows(validParams, validParams_piece, validParams_decay_piece)
  }
  
}


fits %>% write_csv2(file = paste0("output/tables/fits_age.csv"))
estStatus %>% write_csv2(file = paste0("output/estStatus_age.csv"))
validParams %>% write_csv2(file = paste0("output/validParams_age.csv"))

fits %>% 
  select(-(confirmed:inferredSuspected)) %>% 
  {rename_with(., .fn = ~paste0(.x, "_best"), .cols = select(., 2:10) %>% names)} %>% 
  pivot_longer(-Virus, names_sep = "_", names_to = c("Parameter", ".value")) %>% 
  mutate(cell = paste0(signif(best, digits = 3), "\n(", signif(min, 3), ",", signif(max, 3), ")")) %>% 
  select(-best, -min, -max) %>% 
  pivot_wider(id_cols = "Virus", names_from = "Parameter", values_from = "cell") %>% 
  rename_all(~gsub("prev", "Prevalence\n", .x)) %>% 
  write_csv2(file = paste0("output/tables/fits_age_CI.csv"))


  
# 
# 
data_PCR_sejour_collapseSubtypes %>%
  filter(Virus != "InfluenzaNT") %>%
  filter(testday < big_day_limit) %>%
  # filter(Virus == this_virus) %>%
  filter(resultatbis %in% c("POSITIVE", "NEGATIVE")) %>%
  transmute(patient_id = IPP_sejour, age_group, Virus, testday, y = (0:1)[match(resultatbis, c("NEGATIVE", "POSITIVE"))]) %>%
  group_by(Virus, patient_id, age_group) %>% 
  mutate(knownCommunity = as.numeric(any((testday < acute_day_limit) & y == 1))) %>% 
  # filter(knownCommunity == 1) %>% 
  group_by(Virus, age_group, testday) %>%
  summarise(p_pos = mean(y), total = n()) %>%
  left_join(fits %>%
              select(Virus:falsPos) %>% 
              cross_join(tibble(testday = 0:big_day_limit)) %>%
              rowwise() %>% 
              transmute(Virus, testday, seij = calculate_remaining_sensitivity(pars = c(sensAdm = sensAdm, dailClr = dailClr), t = testday),
                        # model = prev*(1-(1-seij)*(1-falsPos)) 
                        # model = seij, 
                        falsPos
              )) %>%
  left_join(fits %>% 
              select(Virus, starts_with("prev")) %>% 
              select(-contains("_")) %>% 
              pivot_longer(-Virus, names_to = "age_group", names_pattern = "prev(.*)", values_to = "prev")) %>% 
  mutate(model = seij*prev) %>% 
  mutate(Virus = factor(Virus, virus_vec)) %>%
  ggplot(aes(x = testday)) +
  geom_point(aes(y = p_pos, size = total)) +
  geom_line(aes(y = model, colour = "Community infection"), linetype = "dashed", size = 1) +
  geom_line(aes(y = falsPos, colour = "False positive\nOR nosocomial infection"), linetype = "dashed", size = 1) +
  scale_colour_manual(values = c(`Community infection` = "red", `False positive\nOR nosocomial infection` = "blue")) +
  facet_grid(Virus~age_group, scales = "free") +
  labs(x = "Days after admission", y = "Proportion of tests positive", size = "Number of\nhospital stays", colour = "") +
  theme_bw() + 
  coord_cartesian(ylim = c(0, 1))

# ggsave("plots/data_knownCommunity.png", width = 20, height = 14, units = "cm")
ggsave("output/plots/data_model_wholeSample.png", width = 22, height = 14, units = "cm")



data_PCR_sejour_collapseSubtypes %>%
  filter(Virus != "InfluenzaNT") %>%
  filter(testday < big_day_limit) %>%
  # filter(Virus == this_virus) %>%
  filter(resultatbis %in% c("POSITIVE", "NEGATIVE")) %>%
  transmute(patient_id = IPP_sejour, age_group, Virus, testday, y = (0:1)[match(resultatbis, c("NEGATIVE", "POSITIVE"))]) %>%
  group_by(Virus, patient_id, age_group) %>% 
  mutate(knownCommunity = as.numeric(any((testday < acute_day_limit) & y == 1))) %>% 
  filter(knownCommunity == 1) %>%
  group_by(Virus, age_group, testday) %>%
  summarise(p_pos = mean(y), total = n()) %>%
  left_join(fits %>%
              select(Virus:falsPos) %>% 
              cross_join(tibble(testday = 0:big_day_limit)) %>%
              rowwise() %>% 
              transmute(Virus, testday, seij = calculate_remaining_sensitivity(pars = c(sensAdm = sensAdm, dailClr = dailClr), t = testday),
                        # model = prev*(1-(1-seij)*(1-falsPos)) 
                        # model = seij, 
                        falsPos
              )) %>%
  # left_join(fits %>% 
  #             select(Virus, starts_with("prev")) %>% 
  #             select(-contains("_")) %>% 
  #             pivot_longer(-Virus, names_to = "age_group", names_pattern = "prev(.*)", values_to = "prev")) %>% 
  mutate(model = seij*1) %>% 
  mutate(Virus = factor(Virus, virus_vec)) %>%
  ggplot(aes(x = testday)) +
  geom_point(aes(y = p_pos, size = total)) +
  geom_line(aes(y = model, colour = "Community infection"), linetype = "dashed", size = 1) +
  geom_line(aes(y = falsPos, colour = "False positive\nOR nosocomial infection"), linetype = "dashed", size = 1) +
  scale_colour_manual(values = c(`Community infection` = "red", `False positive\nOR nosocomial infection` = "blue")) +
  facet_grid(Virus~age_group, scales = "free") +
  labs(x = "Days after admission", y = "Proportion of tests positive", size = "Number of\nhospital stays", colour = "") +
  theme_bw() + 
  coord_cartesian(ylim = c(0, 1))

# ggsave("plots/data_knownCommunity.png", width = 20, height = 14, units = "cm")
ggsave("output/plots/data_model_knownCommunity.png", width = 22, height = 14, units = "cm")


validParams %>% 
  mutate(i = 1:n()) %>% 
  # pivot_longer(-c("Virus", "i", "Loglik", "deltaLoglik"), names_to = c("Parameter")) %>% 
  # mutate(age_group = ifelse(grepl("prev", Parameter), gsub("prev", "", Parameter), NA) %>% factor(levels = age_vec)) %>% 
  cross_join(tibble(testday = 0:big_day_limit)) %>%
  rowwise() %>% 
  transmute(Virus, i, testday, seij = calculate_remaining_sensitivity(pars = c(sensAdm = sensAdm, dailClr = dailClr), t = testday),
            # model = prev*(1-(1-seij)*(1-falsPos)) 
            # model = seij, 
            falsPos
  ) %>% 
  group_by(Virus, testday) %>% 
  summarise(seij_lo = min(seij), seij_hi = max(seij), falsPos_lo = min(falsPos), falsPos_hi = max(falsPos)) %>% 
  left_join(fits %>% select(Virus:falsPos)) %>% 
  rowwise() %>% 
  mutate(seij = calculate_remaining_sensitivity(pars = c(sensAdm = sensAdm, dailClr = dailClr), t = testday)) %>% 
  ungroup %>% 
  left_join(data_PCR_sejour_collapseSubtypes %>%
              filter(Virus != "InfluenzaNT") %>%
              filter(testday < big_day_limit) %>%
              # filter(Virus == this_virus) %>%
              filter(resultatbis %in% c("POSITIVE", "NEGATIVE")) %>%
              transmute(patient_id = IPP_sejour, age_group, Virus, testday, y = (0:1)[match(resultatbis, c("NEGATIVE", "POSITIVE"))]) %>%
              group_by(Virus, patient_id) %>% 
              mutate(knownCommunity = as.numeric(any((testday < acute_day_limit) & y == 1))) %>% 
              filter(knownCommunity == 1) %>% 
              group_by(Virus, testday) %>%
              summarise(p_pos = mean(y), total = n())
  ) %>% 
  mutate(Virus = factor(Virus, virus_vec)) %>%
  ggplot(aes(x = testday)) + 
  geom_line(aes(y = seij, colour = "Community infection"), size = 1) +
  geom_line(aes(y = falsPos, colour = "False positive\nOR nosocomial infection"), size = 1) +
  # filter(knownCommunity == 1) %>% 
  geom_ribbon(aes(ymin = seij_lo, ymax = seij_hi, fill = "Community infection"), alpha = 0.7) + 
  geom_ribbon(aes(ymin = falsPos_lo, ymax = falsPos_hi, fill = "False positive\nOR nosocomial infection"), alpha = 0.7) + 
  geom_point(aes(y = p_pos, size = total)) +
  guides(fill = F) + 
  facet_wrap(.~Virus) + 
  labs(x = "Days after admission", y = "Proportion positive", size = "Number of samples", colour = "Model") + 
  theme_bw()

ggsave("output/plots/data_model_knownCommunity_CI.png", width = 30, height = 20, units = "cm", dpi = 600)



data_model_wholeSample_CI <- validParams %>% 
  mutate(i = 1:n()) %>% 
  # pivot_longer(-c("Virus", "i", "Loglik", "deltaLoglik"), names_to = c("Parameter")) %>% 
  # mutate(age_group = ifelse(grepl("prev", Parameter), gsub("prev", "", Parameter), NA) %>% factor(levels = age_vec)) %>% 
  cross_join(tibble(testday = 0:big_day_limit)) %>%
  rowwise() %>% 
  mutate(seij = calculate_remaining_sensitivity(pars = c(sensAdm = sensAdm, dailClr = dailClr), t = testday)) %>% 
  select(Virus, i, testday, seij, falsPos, starts_with("prev"),
         # model = prev*(1-(1-seij)*(1-falsPos)) 
         # model = seij, 
         falsPos
  ) %>% 
  pivot_longer(starts_with("prev"), names_pattern = "(prev)(.*)$", names_to = c(".value", "age_group")) %>% 
  mutate(age_group = factor(age_group, levels = age_vec)) %>% 
  mutate(model = seij*prev) %>% 
  group_by(Virus, age_group, testday) %>% 
  summarise(model_lo = min(model, na.rm = T), model_hi = max(model, na.rm = T)) %>% 
  ungroup %>% 
  
  left_join(fits %>% select(Virus, falsPos, falsPos_min, falsPos_max), by = "Virus") %>% 
  
  left_join(data_PCR_sejour_collapseSubtypes %>%
              filter(Virus != "InfluenzaNT") %>%
              filter(testday < big_day_limit) %>%
              # filter(Virus == this_virus) %>%
              filter(resultatbis %in% c("POSITIVE", "NEGATIVE")) %>%
              transmute(patient_id = IPP_sejour, age_group, Virus, testday, y = (0:1)[match(resultatbis, c("NEGATIVE", "POSITIVE"))]) %>%
              group_by(Virus, patient_id) %>% 
              mutate(knownCommunity = as.numeric(any((testday < acute_day_limit) & y == 1))) %>% 
              # filter(knownCommunity == 1) %>% 
              group_by(Virus, age_group, testday) %>%
              summarise(p_pos = mean(y), total = n())
  ) %>% 
  mutate(Virus = factor(Virus, virus_vec))

data_model_wholeSample_CI %>% 
  ggplot(aes(x = testday)) + 
  # geom_line(aes(y = model, colour = "Community infection"), size = 1) +
  # geom_line(aes(y = falsPos, colour = "False positive\nOR nosocomial infection"), size = 1) +
  # filter(knownCommunity == 1) %>% 
  # geom_line(aes(y = seij, colour = "Community infection"), size = 1) +
  # geom_line(aes(y = falsPos, colour = "False positive\nOR nosocomial infection"), size = 1) +
  
  geom_line(aes(y = falsPos, colour = "False positive\nOR nosocomial infection"), size = 1) +
  geom_ribbon(aes(ymin = falsPos_min, ymax = falsPos_max, fill = "False positive\nOR nosocomial infection"), alpha = 0.7) + 

  geom_line(aes(y = model_lo, colour = "Community infection")) + 
  geom_line(aes(y = model_hi, colour = "Community infection")) + 
  geom_ribbon(aes(ymin = model_lo, ymax = model_hi, fill = "Community infection"), alpha = 0.7) + 
    
    
  # geom_ribbon(aes(ymin = falsPos_lo, ymax = falsPos_hi, fill = "False positive\nOR nosocomial infection"), alpha = 0.7) + 
  geom_point(aes(y = p_pos, size = total)) +
  guides(fill = F) + 
  facet_grid(Virus~age_group, scales = "free_y") + 
  labs(x = "Days after admission", y = "Proportion positive", size = "Number of samples", colour = "Model") + 
  theme_bw()

ggsave("output/plots/data_model_wholeSample_CI.png", width = 22, height = 24, units = "cm", dpi = 600)



# 
# data_PCR_sejour_collapseSubtypes %>%
#   filter(Virus != "InfluenzaNT") %>%
#   filter(testday < big_day_limit) %>%
#   # filter(Virus == this_virus) %>%
#   filter(resultatbis %in% c("POSITIVE", "NEGATIVE")) %>%
#   transmute(patient_id = IPP_sejour, Virus, testday, y = (0:1)[match(resultatbis, c("NEGATIVE", "POSITIVE"))]) %>%
#   group_by(Virus, patient_id) %>% 
#   mutate(knownCommunity = as.numeric(any((testday < acute_day_limit) & y == 1))) %>% 
#   # filter(knownCommunity == 1) %>% 
#   group_by(Virus, testday) %>%
#   summarise(p_pos = mean(y), total = n()) %>%
#   left_join(fits %>%
#               cross_join(tibble(testday = 0:big_day_limit)) %>%
#               transmute(Virus, testday, seij = prev*calculate_remaining_sensitivity(sensAdm, dailClr, testday),
#                         # model = prev*(1-(1-seij)*(1-falsPos)) 
#                         model = seij, falsPos
#               )) %>%
#   mutate(Virus = factor(Virus, virus_vec)) %>%
#   ggplot(aes(x = testday)) +
#   geom_point(aes(y = p_pos, size = total)) +
#   geom_line(aes(y = model, colour = "Community infection"), linetype = "dashed", size = 1) +
#   geom_line(aes(y = falsPos, colour = "False positive\nOR nosocomial infection"), linetype = "dashed", size = 1) +
#   scale_colour_manual(values = c(`Community infection` = "red", `False positive\nOR nosocomial infection` = "blue")) +
#   facet_wrap(Virus~., scales = "free") +
#   labs(x = "Days after admission", y = "Proportion of tests positive", size = "Number of\nhospital stays", colour = "") +
#   theme_bw() #+ 
# # coord_cartesian(ylim = c(0, 0.4))
# 
# ggsave("output/plots/data_model_knownCommunity_wholeSample.png", width = 22, height = 14, units = "cm")


