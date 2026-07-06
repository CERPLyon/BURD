DP_fullList <- #diag_sejour_long %>% 
  diag_sejour_long_beforeExclusions %>%
  filter(position %in% c("DP", "rDP")) %>% 
  mutate(code = gsub("[+].*", "", code)) %>% 
  # filter(!target) %>% 
  select(IPP_sejour, code) %>% 
  unique %>% 
  group_by(code) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  select(code)

DP_fullList %>% unlist %>% unname %>% sort %>% 
  cat(sep = "\",\"", file = "data/SNDS/DP_fullList_Jul2026.txt")

DP_fullList %>% arrange(code) %>% 
  write_csv("data/SNDS/DP_fullList_Dec2025.csv", col_names = F)

DP_fullList_3chr <- DP_fullList %>% mutate(code = substr(code, 1, 3)) %>% unique
DP_fullList_2chr <- DP_fullList %>% mutate(code = substr(code, 1, 2)) %>% unique


