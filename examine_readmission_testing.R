library(tidyverse) 

data_sejours %>% 
  select(IPP_alea, IPP_sejour_noreadmin30, IPP_sejour, read_within_30days) %>% 
  unique %>% 
  View

data_PCR %>% 
  filter(resultatbis != "UNINTERPRETABLE") %>% 
  group_by(Virus, IPP_sejour) %>% 
  summarise(testday = min(testday)) %>% 
  left_join(data_sejours %>% select(IPP_sejour, read_within_30days) %>% unique) %>% 
  group_by(Virus, testday, read_within_30days) %>% 
  summarise(n = n()) %>% 
  group_by(Virus, read_within_30days) %>% 
  mutate(p = n/sum(n)) %>% 
  ggplot(aes(x = testday, y = p, fill = as.factor(read_within_30days))) + geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(.~Virus) + 
  coord_cartesian(xlim = c(0, 30))

data_PCR %>% 
  filter(resultatbis != "UNINTERPRETABLE") %>% 
  group_by(Virus, IPP_sejour, resultatbis) %>% 
  summarise(testday = min(testday)) %>% 
  left_join(data_sejours %>% select(IPP_sejour, read_within_30days) %>% unique) %>% 
  group_by(Virus, testday, read_within_30days, resultatbis) %>% 
  summarise(n = n()) %>% 
  group_by(Virus, read_within_30days, resultatbis) %>% 
  mutate(p = n/sum(n)) %>% 
  ggplot(aes(x = testday, y = p, fill = as.factor(read_within_30days))) + geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(.~Virus + resultatbis) + 
  coord_cartesian(xlim = c(0, 30))


set.seed(3)
this_ipp = data_sejours %>% 
  filter(read_within_30days == 1) %>% 
  pull(IPP_sejour_noreadmin30) %>% sample(1)


data_sejours %>% 
  filter(IPP_sejour_noreadmin30 %in% this_ipp) %>% 
  select(IPP_alea, IPP_sejour_noreadmin30, read_within_30days, IPP_sejour, NDA_pseudo, date_ent, date_sor)
  
data_PCR %>% 
  filter(IPP_sejour_noreadmin30 %in% this_ipp) %>% 
  select(IPP_sejour, IPP_sejour_noreadmin30, date_preleve, Virus, testday, resultatbis)
