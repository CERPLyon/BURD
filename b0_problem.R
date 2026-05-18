library(tidyverse)
testday_bardata <- data_PCR_sejour_age %>% 
  filter(resultatbis %in% c("POSITIVE", "NEGATIVE")) %>% 
  arrange(testday, resultatbis) %>% 
  group_by(Virus, IPP_sejour) %>% 
  summarise(first_testday = testday[1],
            result = resultatbis[1]) %>% 
  mutate(result = fct_rev(result)) %>% 
  mutate(Virus = plyr::revalue(x = Virus, c("Enterovirus/Rhinovirus" = "EnterovirusRhinovirus"))) %>% 
  right_join(expand.grid(list(IPP_sejour = unique(data_sejours$IPP_sejour), Virus = virus_vec))) %>% 
  mutate(Virus = factor(Virus, virus_vec)) %>% 
  mutate(first_testday = replace_na(first_testday, replace = Inf), 
         result = replace_na(result, "UNINTERPRETABLE")) %>% 
  filter(Virus != "InfluenzaNT") 
  # group_by(Virus, first_testday, result) %>% 
  # summarise(n = n()) %>% 

testday_table <- testday_bardata %>% 
  group_by(Virus, first_testday) %>%
  summarise(n = n()) %>% 
  mutate(TestStatus = case_when(first_testday < 3 ~ "Test before day 3", 
                                first_testday < 7 ~ "Test on day 3-6", 
                                is.finite(first_testday) ~ "Test after day 6", 
                                T ~ "No test performed", 
  )) %>% 
  group_by(Virus, TestStatus) %>% 
  summarise(n = sum(n)) %>% 
  group_by(Virus) %>% 
  mutate(p = n/sum(n)) %>% 
  mutate(Patients = paste0(n, " (", round(p*100, 1), "%)")) %>% 
  select(-n, -p) %>% 
  mutate(TestStatus = factor(TestStatus, c("Test before day 3", "Test on day 3-6", "Test after day 6", "No test performed"))) %>% 
  arrange(Virus, TestStatus)

tbs <- lapply(split(testday_table, testday_table$Virus), "[", -1L)
df <- tibble(x   = rep(Inf, length(tbs)), 
             y   = rep(Inf, length(tbs)), 
             Virus = levels(as.factor(testday_table$Virus)), 
             tbl = tbs) %>% 
  mutate(Virus = factor(Virus, Virus))

testday_bardata %>% 
  ggplot(aes(x = first_testday, fill = result)) + 
  geom_histogram(breaks = c(0:7)
                 ) + 
  ggpp::geom_table(data = df, aes(x = x, y = y, label = tbs),
                   inherit.aes = FALSE,
                   table.colnames = F, table.hjust = 1,
                   table.theme = gridExtra::ttheme_default(
                     core = list(
                       core.just = "right",
                       fg_params = list(cex = 0.5, lineheight = 0.6, col = "black"),  # shrink body rows
                     bg_params = list(fill = "white", col = "darkgrey")
                       )
                   )) +
  theme_bw() + 
  labs(x = "Days after admission", fill = "Test result", y = "Patients") + 
  scale_x_continuous(breaks = 0:6 + 0.5, labels = 0:6) + 
  scale_fill_manual(values = c(NEGATIVE = "skyblue3", POSITIVE = "red")) + 
  facet_wrap(.~Virus, scale = "free_y")

ggsave(filename = "plots/testday_problem.png", width = 24, height = 16, units = "cm")

