patient_knownStatus %>% 
  filter(nosocomial == 1) %>% 
  left_join(data_sejours %>% select(IPP_sejour, month_admitted) %>% unique) %>% 
  group_by(Virus, month_admitted) %>% 
  summarise(nos = n_distinct(IPP_sejour)) %>% 
  ggplot(aes(x = month_admitted, y = nos)) + geom_line() + 
  facet_wrap(.~Virus, scales = "free_y")



ambig_patients <- data_PCR_sejour_collapseSubtypes %>% 
  filter(resultatbis != "UNINTERPRETABLE") %>% 
  group_by(Virus, IPP_sejour) %>% 
  filter(!any(testday %in% (0:2))) %>% 
  filter(testday %in% (3:6)) %>% 
  select(IPP_sejour, age_group, type_sej, Virus, testday, resultatbis)
?slice
set.seed(1)



ambig_toplot <- ambig_patients %>% 
  filter(Virus == "InfluenzaA") %>% 
  left_join(estStatus %>% rename(IPP_sejour = patient_id)) %>% 
  group_by(IPP_sejour) %>% 
  mutate(n = n()) %>% 
  filter(n >= 2
         # , n_distinct(resultatbis) == 2
         ) %>% 
  mutate(testcollection = resultatbis %>% unique %>% sort %>% paste0(collapse = "_")) %>% 
  arrange(p_infected) %>% 
  ungroup

set.seed(1)
ambig_sample <- c(
  ambig_toplot %>% 
  filter(testcollection == "NEGATIVE_POSITIVE") %>% 
  pull(IPP_sejour) %>% sample(5),
ambig_toplot %>% 
  filter(testcollection == "POSITIVE") %>% 
  pull(IPP_sejour) %>% sample(5),
ambig_toplot %>% 
  filter(testcollection == "NEGATIVE") %>% 
  pull(IPP_sejour) %>% sample(5)
)


# ambig_sample <- sample(ambig_toplot$IPP_sejour, 10)

ambig_toplot %>% 
  filter(IPP_sejour %in% ambig_sample) %>% 
  arrange(testcollection) %>% 
  mutate(IPP_sejour = factor(IPP_sejour, levels = unique(IPP_sejour))) %>% 
  ggplot(aes(x = testday, colour = resultatbis, y = IPP_sejour)) + geom_jitter(width = 0, height = 0.3) +
  facet_wrap(.~Virus) +
  theme_bw() +
  scale_colour_manual(values = c(POSITIVE = "red", NEGATIVE = "blue")) +
  labs(x = "Days after admission", y = "Patient", colour = "Test result")
ggsave("output/plots/nosocomial_tests.png", height = 10, width = 10, units = "cm")

ambig_toplot %>% 
  filter(IPP_sejour %in% ambig_sample) %>% 
  arrange(testcollection) %>% 
  mutate(IPP_sejour = factor(IPP_sejour, levels = unique(IPP_sejour))) %>% 
  select(Virus, IPP_sejour, p_infected, p_infected_lo, p_infected_hi) %>% unique %>% 
  ggplot(aes(y = IPP_sejour)) + 
  geom_point(aes(x = p_infected)) + 
  geom_errorbarh(aes(xmin = p_infected_lo, xmax = p_infected_hi)) + 
  theme_bw() + 
  facet_wrap(.~Virus) + 
  coord_cartesian(xlim = c(0, 1))

ggsave("output/plots/nosocomial_ests.png", height = 10, width = 10, units = "cm")
  

data_PCR_sejour_collapseSubtypes %>%
  filter(Virus != "InfluenzaNT") %>%
  filter(Virus == "InfluenzaA") %>% 
  filter(testday < big_day_limit) %>%
  # filter(Virus == this_virus) %>%
  filter(resultatbis %in% c("POSITIVE", "NEGATIVE")) %>%
  transmute(patient_id = IPP_sejour, age_group, Virus, testday, y = (0:1)[match(resultatbis, c("NEGATIVE", "POSITIVE"))]) %>%
  group_by(Virus, patient_id, age_group) %>% 
  mutate(knownCommunity = as.numeric(any((testday < acute_day_limit) & y == 1))) %>% 
  filter(knownCommunity == 1) %>%
  group_by(Virus, testday) %>%
  summarise(p_pos = mean(y), total = n()) %>%
  # left_join(fits %>%
  #             select(Virus:falsPos) %>% 
  #             cross_join(tibble(testday = 0:big_day_limit)) %>%
  #             rowwise() %>% 
  #             transmute(Virus, testday, seij = calculate_remaining_sensitivity(pars = c(sensAdm = sensAdm, dailClr = dailClr), t = testday),
  #                       # model = prev*(1-(1-seij)*(1-falsPos)) 
  #                       # model = seij, 
  #                       falsPos
  #             )) %>%
  # # left_join(fits %>% 
  # #             select(Virus, starts_with("prev")) %>% 
  # #             select(-contains("_")) %>% 
  # #             pivot_longer(-Virus, names_to = "age_group", names_pattern = "prev(.*)", values_to = "prev")) %>% 
  # mutate(model = seij*1) %>% 
  mutate(Virus = factor(Virus, virus_vec)) %>%
  ggplot(aes(x = testday)) +
  # geom_point(aes(size = total)) +
  geom_bar(aes(y = p_pos), stat = "identity") + 
  # geom_line(aes(y = model, colour = "Community infection"), linetype = "dashed", size = 1) +
  # geom_line(aes(y = falsPos, colour = "False positive\nOR nosocomial infection"), linetype = "dashed", size = 1) +
  # scale_colour_manual(values = c(`Community infection` = "red", `False positive\nOR nosocomial infection` = "blue")) +
  facet_wrap(Virus~., scales = "free") +
  labs(x = "Days after admission", y = "Proportion of tests positive", size = "Number of\nhospital stays", colour = "") +
  theme_bw() +
  coord_cartesian(ylim = c(0, 1))
ggsave("output/plots/InfluenzaA_data_wholeSample.png", width = 10, height = 10, units = "cm")

# ggsave("plots/data_knownCommunity.png", width = 20, height = 14, units = "cm")
ggsave("output/plots/InfluenzaA_data_knownCommunity.png", width = 10, height = 10, units = "cm")



tibble(testday = 0:28) %>% 
  mutate(remaining_sensitivity = calculate_remaining_sensitivity(pars = c(sensAdm = 1, dailClr = 0.03), testday)) %>% 
  mutate(falsPos = 0.05) %>% 
  ggplot(aes(x = testday)) + 
  geom_line(aes(y = remaining_sensitivity, colour = "Community infection")) + 
  geom_line(aes(y = falsPos, colour = "FP or nosocomial infection")) + 
  theme_bw() + 
  labs(x = "Days since admission", y = "Test positivity", colour = "Model")

ggsave("output/plots/illustrate_models.png", units = "cm", width = 10, height = 10)

tibble(testday = 0:28) %>% 
  mutate(falsPos = 0.05) %>% 
  {rbind(mutate(., dataset = "Known community infect.", 
                remaining_sensitivity = 0.2*calculate_remaining_sensitivity(pars = c(sensAdm = 1, dailClr = 0.03), testday)
  ))} %>% 
  ggplot(aes(x = testday)) + 
  geom_line(aes(y = remaining_sensitivity, colour = "Community infection")) + 
  geom_line(aes(y = falsPos, colour = "FP or nosocomial infection")) + 
  theme_bw() + 
  labs(x = "Days since admission", y = "Test positivity", colour = "Model") + 
  coord_cartesian(ylim = c(0, NA))

ggsave("output/plots/illustrate_models_prev.png", units = "cm", width = 15, height = 8)

