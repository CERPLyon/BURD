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

data_NDA_long <- diag_sejour_long %>% 
  # left_join(data_PCR_NDA, by = "NDA_pseudo", relationship = "many-to-many") %>% 
  transmute(NDA_pseudo, IPP_sejour, CIM10 = code) %>% 
  unique %>% 
  full_join(data_sejours %>%
              group_by(NDA_pseudo) %>%
              summarise(age = age[1],
                        age_group = age_group[1],
                        relativeWeek = get_relativeWeek(date_ent_sejour[1]),
                        monthname = lubridate::month(date_ent_sejour[1], label = T),
                        monthnum = lubridate::month(date_ent_sejour[1], label = F),
                        season_admitted = season_admitted[1],
                        catego_UM = catego_UM[1]#,
                        # catego_UM_agg = catego_UM_agg[1]
                        ),
            # left_join(.Last.value
            #             , 
                      by = "NDA_pseudo", relationship = "many-to-one")


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

  data_NDA_wide[, c("age_group", "monthname", "season_admitted", 
                    col_info[(10:1000)] %>% sort(decreasing = T) %>% names)]

    
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

########## training stage #################

# Run b2.3_train_xg.R for each virus
# designed to be run on slurm
# returns a trained model for each virus

###########################################




