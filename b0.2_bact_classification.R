
data_bact_cleaned <- data_bact %>% 
  mutate(raw_taxon_label = taxon_label) %>% 
  mutate(taxon_label = case_when(taxon_code %in% c("BACTEC", "BACTNI") ~ "Unknown", 
                                 taxon_code %in% c("ABSENCE_DE") ~ NA, 
                                 taxon_code == "KOCRHI" ~ "Kocuria rhizophila", 
                                 taxon_code == "ENTHOROH" ~ "Enterobacter hormaechei", 
                                 taxon_code == "STCALP" ~ "Streptococcus", 
                                 taxon_code == "STCSALAP" ~ "Streptococcus salivarius (apparent)", 
                                 taxon_code == "STCVNP" ~ "Streptococcus Groupe viridans except S.pneumoniae", 
                                 taxon_code == "STCSPPU" ~ "Streptococcus", 
                                 taxon_code == "BGPNEC" ~ "Bacille", 
                                 taxon_code == "CLODIF" ~ "Clostridium difficile", 
                                 taxon_code == "ENTAER" ~ "Klebsielle aerogenes", 
                                 T ~ taxon_label), 
         taxon_label = gsub(" spp.", "", taxon_label), 
         taxon_label = gsub(" spp", "", taxon_label), 
         taxon_label = gsub(" Groupe", "", taxon_label), 
         taxon_label = gsub("(^[A-Za-z]+) ([A-Za-z]+)([\ ./]+).*", "\\1 \\2", taxon_label), 
         taxon_code = case_when(taxon_code == "ENTHOROH" ~ "ENTHOR", 
                                taxon_code %in% c("BACTEC", "BACTNI") ~ "Unknown", 
                                taxon_code %in% c("ABSENCE_DE") ~ NA, 
                                T ~ taxon_code))


# data_bact_cleaned %>% 
  

data_bact_cleaned %>% 
  select(taxon_code, raw_taxon_label, taxon_label) %>% unique %>% 
  arrange(taxon_label) %>% 
  View



species_threshold = 100
genus_threshold = 100

species_list = data_bact_cleaned %>% 
  filter(sample_type1 == "BPO") %>%
  filter(!is.na(taxon_label)) %>% 
  group_by(taxon_code, taxon_label) %>% 
  summarise(n = n_distinct(NDA_pseudo)) %>% 
  filter(n >= species_threshold) %>% 
  transmute(taxon_code, species = taxon_label)

genus_list <- data_bact_cleaned %>% 
  filter(sample_type1 == "BPO") %>%
  filter(!is.na(taxon_label)) %>% 
  filter(!(taxon_label %in% species_list$species)) %>% 
  # slice(74714  ) %>% t
  mutate(genus = gsub("([A-Za-z]+) .*", "\\1", taxon_label)) %>% 
  mutate(genus = ifelse(genus == "Unknown", "UNKNOWN", stringr::str_to_title(genus))) %>% 
  mutate(genus = ifelse(genus == "Bacille", "Bacillus", genus)) %>% 
  group_by(genus) %>% 
  # summarise(n = n()) %>% arrange(-n) %>% print(n = 200)
  mutate(n = n_distinct(NDA_pseudo)) %>% 
  filter(n >= genus_threshold) %>% 
  select(taxon_code, genus) %>% 
  unique

genus_list$genus %>% unique %>% sort
species_list$species %>% unique %>% sort

data_bact_classified <- data_bact_cleaned %>% 
  # filter(sample_type1 == "BPO") %>% 
  left_join(species_list) %>% 
  left_join(genus_list) %>% 
  mutate(classification = case_when(!is.na(species) ~ species, 
                                    !is.na(genus) ~ paste(genus, "GENUS"), 
                                    is.na(taxon_code) ~ NA, 
                                    T ~ "BACTERIA OTHER"))

bact_BPOpresent_NDA <- data_bact_classified %>% 
  filter(sample_type1 == "BPO") %>% 
  {left_join(select(., NDA_pseudo, IPP_sejour) %>% unique, 
             select(., NDA_pseudo, IPP_sejour, classification) %>% filter(!is.na(classification)))} %>% 
  mutate(classification = replace_na(classification, "BACTERIA ALLNEGATIVE"))


bact_BPOpresent_NDA %>% 
  group_by(classification) %>% 
  summarise(n = n_distinct(NDA_pseudo)) %>% 
  arrange(-n) %>% 
  View

bact_BPOpresent_NDA_wide <- bact_BPOpresent_NDA %>% 
  select(NDA_pseudo, IPP_sejour, classification) %>% unique %>% 
  mutate(classification = gsub(" ", ".", classification)) %>% 
  mutate(val = 1) %>% 
  pivot_wider(id_cols = c("NDA_pseudo", "IPP_sejour"), names_from = classification,  values_from = val)

