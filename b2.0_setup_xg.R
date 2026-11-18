library(tidyverse)

source("b0.R")

# diagnostic codes and test results

summ_stats_wide <- read_csv("data/biomarkers_summ_stat.csv")

source("b0.2_bact_classification.R")


#at level of NDA

data_PCR_NDA <- data_PCR_sejour_collapseSubtypes %>% 
  group_by(NDA_pseudo, Virus) %>% 
  summarise(TestPos = case_when(any(resultatbis %in% "POSITIVE") ~ 1, 
                                any(resultatbis %in% "NEGATIVE") ~ 0, 
                                T ~ NA)) %>% 
  ungroup %>% 
  complete(NDA_pseudo, Virus, fill = list(TestPos = NA))

# data_NDA_long <- diag_sejour_long %>%
#   # left_join(data_PCR_NDA, by = "NDA_pseudo", relationship = "many-to-many") %>%
#   transmute(NDA_pseudo, IPP_sejour, CIM10 = code) %>%
#   unique %>%
#   # HERE SHOULD IT BE RIGHT JOIN???
#   full_join(data_sejours_all %>%
#               group_by(NDA_pseudo) %>%
#               summarise(age = age[1],
#                         age_group = age_group[1],
#                         relativeWeek = get_relativeWeek(date_ent_sejour[1]),
#                         monthname = lubridate::month(date_ent_sejour[1], label = T),
#                         monthnum = lubridate::month(date_ent_sejour[1], label = F),
#                         season_admitted = season_admitted[1],
#                         catego_UM = catego_UM[1],
#                         catego_UM_agg = catego_UM_agg[1]
#                         ),
#                       by = "NDA_pseudo", relationship = "many-to-one")


data_NDA_long <- 
  # data_sejours_all %>%
  data_sejours_code %>%
  group_by(IPP_sejour, NDA_pseudo) %>%
  summarise(age = age[1],
            age_group = age_group[1],
            relativeWeek = get_relativeWeek(date_ent_sejour[1]),
            monthname = lubridate::month(date_ent_sejour[1], label = T),
            monthnum = lubridate::month(date_ent_sejour[1], label = F),
            season_admitted = season_admitted[1],
            catego_UM = catego_UM[1],
            catego_UM_agg = catego_UM_agg[1]
  ) %>% 
  ungroup %>% 
  left_join(diag_sejour_long %>% 
              # left_join(data_PCR_NDA, by = "NDA_pseudo", relationship = "many-to-many") %>% 
              transmute(NDA_pseudo, CIM10 = code) %>% 
              unique, 
            by = "NDA_pseudo", relationship = "one-to-many")

  data_NDA_wide_noBiomarkers <- data_NDA_long %>% 
    # filter(Virus == this_virus) %>% 
    # mutate(name = gsub("[0-9]*", "", name)) %>% 
    # mutate(name = gsub("^r", "", name)) %>% 
    # mutate(name = ifelse(name == "diag", "V", name)) %>% 
    # mutate(type = factor(name, levels = c("DP", "DR", "AS", "V"))) %>% 
    mutate(CIM10 = gsub("\\+", "", CIM10)) %>%
    mutate(type = as.integer(1)) %>% 
    # select(NDA_pseudo, IPP_sejour, type, age, age_group, monthnum, monthname, season_admitted, catego_UM, CIM10) %>%
    unique %>% 
    # group_by(NDA_pseudo, age, age_group, monthnum, monthname, season_admitted, CIM10) %>% 
    # unique %>% 
    # summarise(type = levels(type)[min(as.numeric(type))]) %>% 
    # ungroup %>%
    pivot_wider(#id_cols = c("NDA_pseudo", "IPP_sejour", "age", "age_group", "monthnum", "monthname", "season_admitted", "catego_UM"), 
                names_from = CIM10, values_from = type, values_fill = 0) %>% 
    rename_all(~gsub("^0$", "ZERO", .x))


  
  data_NDA_wide_noBacteria <- data_NDA_wide_noBiomarkers %>% 
    left_join(summ_stats_wide 
              # %>% select(NDA_pseudo, IPP_sejour, ends_with("_n"))
              , relationship = "one-to-one")
  
  
  data_NDA_wide = data_NDA_wide_noBacteria %>% 
    left_join(bact_BPOpresent_NDA_wide 
              , relationship = "one-to-one") %>% 
    mutate(across(bact_BPOpresent_NDA_wide %>% 
                    select(-NDA_pseudo, -IPP_sejour) %>% 
                    names, ~replace_na(.x, 0)))
  
  
  col_info <- colSums(data_NDA_wide != 0, na.rm = T)

  
  
  data_NDA_wide[, c("age_group", 
                    "monthname",
                    "monthnum", 
                    "relativeWeek", 
                    "season_admitted",
                    "catego_UM", 
                    col_info[(8:1000)] %>% sort(decreasing = T) %>% names)]
  

data_NDA_reduced <- data_NDA_wide %>% 
  select(names(col_info[col_info > 100]))

data_NDA_reduced200 <- data_NDA_wide %>% 
  select(names(col_info[col_info > 200]))

data_NDA_reduced50 <- data_NDA_wide %>% 
  select(names(col_info[col_info > 50]))

save(data_NDA_reduced, file = "data/data_NDA_reduced.RData")
save(data_PCR_NDA, file = "data/data_PCR_NDA.RData")

save(data_NDA_reduced200, file = "data/data_NDA_reduced200.RData")
save(data_NDA_reduced50, file = "data/data_NDA_reduced50.RData")

remove(bact_BPOpresent_NDA)
remove(bact_BPOpresent_NDA_wide)
remove(data_NDA_wide_noBacteria)
remove(data_NDA_wide_noBiomarkers)
remove(data_NDA_wide)
remove(data_NDA_reduced50)
remove(data_NDA_reduced200)
remove(data_NDA_reduced_noSpecific)
remove(data_NDA_reduced_strictNDA)

# HERE WE CAN DO SOME EXCLUSIONS FOR SENSITIVITY ANALYSIS
# here we can remove those where the NDA starts several days into the visit
if(T){
  acute_day_limit = 3
  
  when_nda <- data_sejours_code %>% 
    group_by(IPP_sejour, date_ent_sejour, NDA_pseudo) %>% 
    summarise(date_ent = min(date_ent), date_sor = max(date_sor)) %>% 
    mutate(nda_start_sinceAdmission = as.numeric(date_ent - date_ent_sejour), 
           nda_end_sinceAdmission = as.numeric(date_sor - date_ent_sejour)) %>% 
    ungroup
  
  data_NDA_reduced_strictNDA <- when_nda %>% 
    filter(nda_start_sinceAdmission <= acute_day_limit) %>% 
    select(IPP_sejour, NDA_pseudo) %>% 
    left_join(data_NDA_reduced)
  
  
  data_PCR_NDA_strictNDA <- when_nda %>% 
    filter(nda_start_sinceAdmission <= acute_day_limit) %>% 
    select(IPP_sejour, NDA_pseudo) %>% 
    left_join(data_PCR_NDA)
  
  
  save(data_NDA_reduced_strictNDA, file = "data/data_NDA_reduced_strictNDA.RData")
  save(data_PCR_NDA_strictNDA, file = "data/data_PCR_NDA_strictNDA.RData")
  
}

# here we can remove columns which specify organisms
if(T){
  organism_specific_icd10 <- read_csv2("data/Organism_ICD10_codes.csv") %>% 
    filter(Kingdom == "Virus") %>% 
    filter(!grepl("Bronch", Category)) %>% 
    pull(cim10)
  
  organism_specific_icd10 %>% cat(sep = ", ")
  
  data_NDA_reduced_noSpecific <- data_NDA_reduced[, !(names(data_NDA_reduced) %in% organism_specific_icd10)]
  
  save(data_NDA_reduced_noSpecific, file = "data/data_NDA_reduced_noSpecific.RData")
  
}


########## training stage #################

# Run b2.1_train_xg.R for each virus
# designed to be run on slurm
# returns a trained model for each virus

###########################################




