## to be accessible anywhere the main analysis or post-process are run


virus_vec = c("InfluenzaA", "InfluenzaB", "SARS-CoV-2", "RSV", "hMPV", 
              "RVEV", 
              # "Rhinovirus", "EnterovirusRhinovirus", "Enterovirus", 
              "ADV","PIV","hCoV")
season_vec = sort(unique(data_NDA_reduced$season_admitted))
UM_vec = sort(unique(data_NDA_reduced$catego_UM))
UM_agg_vec = unique(data_NDA_reduced$catego_UM_agg)

this_virus = virus_vec[virus_index]
this_string = virus_index*nrounds
print(c(virus_index, this_virus))

output_stem = paste(this_virus)

print("make subsets")

data_NDA_thisVirus <- data_NDA_reduced %>% 
  mutate(season_admitted = factor(season_admitted, levels = season_vec)) %>% 
  mutate(catego_UM = factor(catego_UM, levels = UM_vec)) %>% 
  mutate(catego_UM_agg = factor(catego_UM_agg, levels = UM_agg_vec)) %>% 
  left_join(data_PCR_NDA %>% 
              mutate(Virus = gsub("/", "", Virus)) %>% 
              filter(Virus == this_virus) %>% 
              select(NDA_pseudo, TestPos)
            , by = "NDA_pseudo", relationship = "one-to-one") %>% 
  relocate(TestPos, .after = "NDA_pseudo") %>% 
  relocate(season_admitted, .after = "age_group") %>% 
  select(-age, -monthname, -monthnum, -catego_UM)# %>% 
  # mutate(across(matches("^[A-Z][0-9].*"), ~ifelse(is.na(.x), "", .x)))


data_NDA_sparse <- as(data_NDA_thisVirus %>%
                        select(-NDA_pseudo, -IPP_sejour) %>%
                        mutate(age_group = as.numeric(age_group),
                               season_admitted = as.numeric(season_admitted),
                               catego_UM_agg = as.numeric(catego_UM_agg)#,
                               # catego_UM = as.numeric(catego_UM)#,
                               # across(matches("^[A-Z][0-9].*"), ~ifelse(.x == "", 0, 1))
                               ) %>%
                        as.matrix,
                      "dgCMatrix")


print(c("rows data_NDA_sparse", nrow(data_NDA_sparse)))

data_NDA_sparse_predictOnly <- data_NDA_sparse[is.na(data_NDA_sparse[, 1]), ]
data_NDA_sparse_testResult <- data_NDA_sparse[!is.na(data_NDA_sparse[, 1]), ]

print(c("rows data_NDA_sparse_testResult", nrow(data_NDA_sparse_testResult)))

# MAKE TRAINING AND TESTING DATASET
print("create partitions")
set.seed(this_string)

inTrain <- createDataPartition(y = data_NDA_sparse_testResult[,1], 
                               p=p_training, list = FALSE) %>% as.vector


data_NDA_sparse_train <- data_NDA_sparse_testResult[inTrain,]
data_NDA_sparse_test <- data_NDA_sparse_testResult[-inTrain,]

print(c("rows data_NDA_sparse_train", nrow(data_NDA_sparse_train)))
print(c("rows data_NDA_sparse_test", nrow(data_NDA_sparse_test)))

