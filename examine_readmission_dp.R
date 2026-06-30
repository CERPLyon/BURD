readmin_ipps <- data_PCR_sejour_collapseSubtypes %>% 
  left_join(data_sejours %>% select(NDA_pseudo_ordre, read_within_30days)) %>% 
  filter(read_within_30days == 1) %>% 
  pull(IPP_sejour_noreadmin30) %>% 
  unique


data_PCR_sejour_collapseSubtypes %>% 
  filter(IPP_sejour_noreadmin30 %in% readmin_ipps[3]) %>% 
  ungroup %>% 
  left_join(data_sejours %>% select(NDA_pseudo_ordre, read_within_30days, date_sor_sejour, DP, DR)) %>% 
  select(IPP_sejour_noreadmin30, IPP_sejour, read_within_30days, date_ent_sejour, date_sor_sejour, Virus, testday, resultatbis, DP, DR) %>% 
  group_by(IPP_sejour_noreadmin30)

data_sejours %>% 
  filter(IPP_alea == "P000037") %>% 
  select(IPP_alea, IPP_sejour_noreadmin30, IPP_sejour, date_ent, date_sor, read_within_30days)

data_sejours %>% 
  select(IPP_alea, IPP_sejour_noreadmin30, IPP_sejour, NDA_pseudo_ordre, age_group, read_within_30days, date_ent_sejour, date_sor_sejour, DP, DR) %>% 
  left_join(data_PCR %>% ungroup %>% select(NDA_pseudo_ordre, Virus, date_preleve, testday, resultatbis)) %>% 
  group_by(IPP_sejour) %>% 
  mutate(read_within_30days = max(read_within_30days)) %>% 
  mutate(posViruses = paste0(Virus[resultatbis %in% "POSITIVE"] %>% unique %>% sort, collapse = "_")) %>% 
  group_by(IPP_sejour_noreadmin30) %>% 
  filter(any(read_within_30days %in% 1)) %>% 
  select(IPP_sejour_noreadmin30, IPP_sejour, date_ent_sejour, date_sor_sejour, posViruses, DP) %>% 
  unique %>% 
  mutate(iIPP = n_distinct(IPP_sejour), nDP = n_distinct(DP)) %>% 
  filter(iIPP == nDP) %>% 
  left_join(cim10_ref %>% transmute(DP = paste0(code, "_"), cim10_description)) %>% 
  View

  group_by(iIPP, nDP) %>% 
  summarise(freq = n()) %>% 
  print(n = 100)

  
  
cim10_ref
  data_se
  
  select(IPP_sejour_noreadmin30, IPP_sejour, read_within_30days, date_ent_sejour, date_sor_sejour, Virus, testday, resultatbis, DP, DR)

