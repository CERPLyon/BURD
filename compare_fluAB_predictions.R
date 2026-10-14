

NDA_virus_prediction_sejour %>% 
  filter(NDA_startDay < 3) %>% 
  filter(Virus %in% c("InfluenzaA", "InfluenzaB", "RSV")) %>% 
  select(IPP_sejour, Virus, prediction) %>% 
  group_by(IPP_sejour, Virus) %>% 
  slice(1) %>% 
  pivot_wider(names_from = Virus, values_from = prediction) %>% 
  # {cor(.$InfluenzaA, .$InfluenzaB)}
  {cor(.$InfluenzaA, .$RSV)}
  ggplot(aes(x = InfluenzaA, y = InfluenzaB)) + 
  geom_point(alpha = 0.1) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
  theme_bw()

