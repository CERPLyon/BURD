
this_virus = "SARS-CoV-2"

acute_day_limit = 3

data_known <- data_PCR_sejour_collapseSubtypes %>% 
  # filter(season_admitted %in% c("22/23", "23/24", "24/25")) %>% 
  # filter(season_admitted %in% c("20/21", "21/22")) %>% 
  filter(resultatbis %in% c("POSITIVE", "NEGATIVE")) %>% 
  filter(Virus == this_virus) %>% 
  filter(testday < big_day_limit) %>% 
  group_by(IPP_sejour) %>% 
  mutate(knownCommunity = as.numeric(any((testday < acute_day_limit) & resultatbis == "POSITIVE"))) %>% 
  ungroup %>% 
  transmute(age_group, season_admitted, type_sej, patient_id = IPP_sejour, testday, y = (0:1)[match(resultatbis, c("NEGATIVE", "POSITIVE"))], knownCommunity) %>% 
  filter(knownCommunity == 1)


data_this <- data_PCR_sejour_collapseSubtypes %>% 
  # filter(season_admitted %in% c("22/23", "23/24", "24/25")) %>% 
  # filter(season_admitted %in% c("20/21", "21/22")) %>% 
  filter(resultatbis %in% c("POSITIVE", "NEGATIVE")) %>% 
  filter(Virus == this_virus) %>% 
  filter(testday < big_day_limit) %>% 
  group_by(IPP_sejour) %>% 
  mutate(knownCommunity = as.numeric(any((testday < acute_day_limit) & resultatbis == "POSITIVE"))) %>% 
  ungroup %>% 
  transmute(age_group, season_admitted, type_sej, patient_id = IPP_sejour, testday, y = (0:1)[match(resultatbis, c("NEGATIVE", "POSITIVE"))], knownCommunity)



View(data_known)
data_known %>%
  # data_this %>%
  group_by(testday) %>% 
  summarise(p_pos = mean(y), total = n()) %>% 
  # mutate(remaining_sensitivity = calculate_remaining_sensitivity_cubic(testday, beta0 = 5, beta1 = -2, beta2 = 0.1, beta3 = 0)) %>% 
  mutate(remaining_sensitivity = calculate_remaining_sensitivity(mcmc_res$bestpar, testday)) %>%
  mutate(expected_positive = remaining_sensitivity*1) %>% #assume prev in this group is 1
  ungroup %>% 
  ggplot(aes(x = testday)) + 
  geom_point(aes(y = p_pos, size = total)) + 
  geom_line(aes(y = expected_positive), colour = "red", linetype = "dashed") + 
  # facet_wrap(Virus~., scales = "free") + 
  scale_size_continuous(trans = "log10") + 
  # facet_grid(age_group~type_sej) + 
  labs(x = "Days after admission", y = "Proportion of tests positive", title = this_virus)


# DO THIS PLOT BY AGE! EXPLORE THE FITS!
data_known %>% 
  # filter(y == 1 | testday > 2) %>% 
  group_by(patient_id) %>% 
  summarise(last_test = max(testday), 
            last_positive = max(testday[y == 1])) %>% 
  
  pull(last_test) %>% hist
  
data_this %>% 
  group_by(patient_id) %>% 
  arrange(patient_id, testday, -y) %>% 
  filter(any((testday < 3 & y == 1)) & any((testday %in% 4:6 & y == 1)))  %>% 
  group_by(testday) %>% 
  summarise(p_pos = mean(y), total = n()) %>% 
  ggplot(aes(x = testday)) + 
  geom_point(aes(y = p_pos, size = total)) 
# + 
#   facet_grid(.~age_group)
  
data_known %>% 
  # filter(y == 1 | testday > 2) %>% 
  group_by(patient_id) %>% 
  arrange(patient_id, testday, -y) %>% 
  mutate(collapse_y = paste0(y, collapse = ""), 
         collapse_y_reduced = collapse_y %>% 
            {gsub("([0])\\1+", "\\1", .)}, 
         n = n()) %>% 
  mutate(first_positive_day = min(testday[y == 1]), 
         second_testday = min(testday[testday > first_positive_day])) %>% 
  filter(first_positive_day == 0) %>% 
  # filter(second_testday < 7) %>% 
  # filter(collapse_y_reduced != "10"
  #        # , n <= 4
  #        ) %>%

           # {gsub("00", "0", .)} %>% 
           # {gsub("11", "1", .)}) %>% 
  # filter(!any((which(y == 0) > min(which(y == 1))) &
  #              (which(y == 0) < max(which(y == 1)))
  #            )) %>%
  # filter(any((which(y == 1) > min(which(y == 0))) &
  #              (which(y == 1) < max(which(y == 0)))
  #            )) %>%
  # mutate(n = n()) %>% 
  
  # filter(!any(testday %in% c(3, 4))) %>% 
  # filter(any(testday >= 9)) %>%
  # filter(any(testday > 15)) %>%
  # filter(any(y == 0)) %>%
  # filter(any(y == 0 & cumany(y == 1)) & 
  #          any(y == 1 & cumany(y == 0))) %>%
  # View
  group_by(first_positive_day, second_testday, testday) %>% 
  summarise(p_pos = mean(y), total = n()) %>% 
  ggplot(aes(x = testday)) + 
  geom_point(aes(y = p_pos, size = total)) + 
  facet_grid(.~second_testday)

Patients who are retested after 3 days or more likely to be negative than those retested after 5 days
Patients who test positive and then are retested, the probability of a positive test is higher after 10 days, which suggests that clearance is not what is happening
They are being admitted very early in infection, perhaps


  