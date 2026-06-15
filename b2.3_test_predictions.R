# BURD2.4_test_predictions
library(tidyverse)
library(xgboost)
library(caret)
library(Matrix)

# load files
print("loading files")
load("data/data_NDA_reduced.RData")
load("data/data_PCR_NDA.RData")

#### specific virus
# this_virus = "RSV"
# this_virus = "hMPV"
# this_virus = "InfluenzaB"


# virus_vec = gsub("/", "", virus_order[virus_order != "InfluenzaNT"])
# virus_vec

ntr = 100
nrounds = 5000
p_training = 0.7
  

virus_index = 9
ncv = NULL
NDA_virus_prediction = NULL
for(virus_index in 1:9){
  
  gc()  
  # source("BURD2.2_prep_rf.R") #this ensures that the data prep is the same as in the actual training script
  source("b2.2_prep_xg.R") #this ensures that the data prep is the same as in the actual training script
  
  load(file = paste0("ml/cvXG_", output_stem, ".RData"), verbose = T)
  
  prediction_LoHi <- read_csv2(paste0("ml/bootPredictions_", output_stem, ".csv")) 
  if((prediction_LoHi %>% 
    filter(is.na(predictionHi) | is.na(predictionLo)) %>% 
    nrow) > 0) stop()
  # %>% 
  #   pivot_longer(-c("NDA_pseudo", "Virus"), names_to = "boot", values_to = "prediction") %>% 
  #   group_by(NDA_pseudo, Virus) %>% 
  #   summarise(predictionLo = quantile(prediction, 0.025), predictionHi = quantile(prediction, 0.975))
  
  # print(ncv_out$results)
  # print(ncv_out$baseline)
  
  
  ncv_piece <- ncv_out$results %>% 
    summarise(Virus = output_stem, baseline = ncv_out$baseline, mean_prauc = mean(aucpr), sd_prauc = sd(aucpr))
  if(is.null(ncv)){
    ncv = ncv_piece
  } else {
    ncv = rbind(ncv, ncv_piece)
  }
  ncv %>% write_csv2("output/tables/ncv.csv")
  
#   next
# }
  # print("shouldn't print this")
  
  # load rf trained model
  # load(file = paste0("rf/train_", output_stem, ".RData"), verbose = T)
  load(file = paste0("ml/trainXG_", output_stem, ".RData"), verbose = T)
  xgb.save(xg_out, fname = paste0("ml/trainXG_", output_stem, ".ubj"))
  xg_out <- xgb.load(modelfile = paste0("ml/trainXG_", output_stem, ".ubj"))
  
  # predict for all NDAs
  # prediction_allNDA <- predict(rf_out, newdata = data_NDA_thisVirus, type = "prob")[, "1"]
  prediction_allNDA <- predict(xg_out, newdata = data_NDA_sparse[,-1], type = "response")
  
  NDA_virus_prediction_piece = tibble(NDA_pseudo = data_NDA_thisVirus$NDA_pseudo, season_admitted = data_NDA_thisVirus$season_admitted, Virus = this_virus, prediction = prediction_allNDA) %>% 
    mutate(prediction = ifelse((Virus != "SARS-CoV-2") | (season_admitted %in% season_vec[-(1:3)]), prediction, 0)) %>% 
    select(-season_admitted) %>% 
    left_join(prediction_LoHi) %>% 
    mutate(predictionLo = ifelse(prediction < predictionLo, prediction, predictionLo), 
           predictionHi = ifelse(prediction > predictionHi, prediction, predictionHi))
  

  if(is.null(NDA_virus_prediction)){
    NDA_virus_prediction = NDA_virus_prediction_piece
  } else {
    NDA_virus_prediction = rbind(NDA_virus_prediction, NDA_virus_prediction_piece)
  }
  
  NDA_virus_prediction %>% write_csv("output/NDA_virus_prediction.csv")
  
  if(F){
    
    # try and predict the unknowns
    prediction_unknown <- predict(xg_out, newdata = data_NDA_sparse_predictOnly[, -1], type = "response")
    
    # POST-PROCESS
    
    # importance(xg_out) %>% 
    #   {tibble(importance = as.vector(unname(.)), code = rownames(.))} %>% 
    #   arrange(-importance) %>% 
    #   write_csv(file = paste0("rf/importance_", this_virus, ".csv"))
    
    #validating that this method works on training set
    # prediction_train <- predict(rf_out, type = "prob")[, "1"]
    prediction_train <- predict(xg_out, newdata = data_NDA_sparse_train[, -1], type = "response")
    tibble(p = unname(prediction_train), TestPos = data_NDA_sparse_train[, 1]) %>% 
      ggplot(aes(x = p, fill = as.factor(TestPos))) + 
      geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5) + 
      theme_bw() + 
      labs(fill = "Test result", x = "Predicted test result", title = "Training dataset")

    ggsave(paste0("diag/predict_", output_stem, "_trainset.png"))
    
    prediction_test <- predict(xg_out, newdata = data_NDA_sparse_test[, -1], type = "response")
    
    tibble(p = unname(prediction_test), TestPos = data_NDA_sparse_test[, 1]) %>% 
      ggplot(aes(x = p, fill = as.factor(TestPos))) + 
      geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5) + 
      theme_bw()+ 
      labs(fill = "Test result", x = "Predicted test result", title = "Testing dataset")
    
    ggsave(paste0("diag/predict_", output_stem, "_testset.png"))
    
    
    # data_NDA_thisVirus_predictOnly %>% 
    tibble(monthnum = data_NDA_sparse_predictOnly[, 4]) %>% 
      mutate(TestPos = unname(prediction_unknown)) %>% 
      # group_by(age_group, monthnum) %>% 
      group_by(monthnum) %>%
      summarise(TestPos = mean(TestPos)) %>% 
      ggplot(aes(y = TestPos, x = as.factor(monthnum))) + geom_bar(stat = "identity", position = "dodge")
    
    # data_NDA_thisVirus_predictOnly %>%
    tibble(age_group = factor(age_vec[data_NDA_sparse_predictOnly[, 2]], age_vec)) %>% 
      mutate(TestPos = unname(prediction_unknown)) %>% 
      # group_by(age_group, monthnum) %>% 
      # group_by(monthnum) %>%
      group_by(age_group) %>%
      summarise(TestPos = mean(TestPos)) %>% 
      ggplot(aes(y = TestPos, x = as.factor(age_group))) + geom_bar(stat = "identity", position = "dodge")
    
    # data_NDA_thisVirus_predictOnly %>% 
    tibble(TestPos = unname(prediction_unknown)) %>% 
      # group_by(age_group, monthnum) %>% 
      # group_by(monthnum) %>%
      # summarise(TestPos = mean(TestPos)) %>% 
      ggplot(aes(x = TestPos)) + geom_histogram(aes(y = after_stat(density)), fill = "orange") + 
      theme_bw()+ 
      labs(x = "Predicted test result", title = "Untested dataset")
    
    ggsave(paste0("diag/predict_", output_stem, "_unknownset.png"))
    
    
    tibble(train = prediction_train %>% mean(na.rm = T), 
           test = prediction_test %>% mean(na.rm = T), 
           unknown = prediction_unknown %>%  mean(na.rm = T),
           true = data_NDA_thisVirus$TestPos %>% mean(na.rm = T)) %>% 
      write_csv(paste0("diag/predictStats_", output_stem, ".csv"))
    
    
    # data_NDA_thisVirus_predictOnly %>% 
    #   select(NDA_pseudo) %>% 
    #   mutate(TestPos = unname(prediction_unknown)) %>% 
    #   write_csv(paste0("rf/prediction_unknown_", this_virus, ".csv"))
  }
  
  
}

NDA_virus_prediction <- read_csv("output/NDA_virus_prediction.csv")



NDA_virus_prediction_sejour = data_sejours %>% 
  group_by(IPP_sejour, NDA_pseudo) %>% 
  summarise(date_ent = min(date_ent), 
            date_ent_sejour = date_ent_sejour[1]) %>% 
  mutate(NDA_startDay = difftime(date_ent, date_ent_sejour, units = "days") %>% as.numeric) %>% 
  ungroup %>% 
  select(IPP_sejour, NDA_pseudo, NDA_startDay) %>% 
  left_join(NDA_virus_prediction, relationship = "one-to-many")


NDA_virus_prediction_sejour %>% write_csv("output/NDA_virus_prediction_sejour.csv")

## plot for display

untested_IPP_prediction <- NDA_virus_prediction_sejour %>% 
  filter(NDA_startDay < suspected_day_limit) %>% 
  group_by(IPP_sejour, Virus) %>% 
  arrange(NDA_startDay, .by_group = T) %>% 
  dplyr::slice(1) %>% 
  select(IPP_sejour, Virus, NDA_pseudo, NDA_startDay, prediction) %>% 
  left_join(data_PCR_sejour_collapseSubtypes %>% 
              filter(testday < 7) %>% 
              select(Virus, IPP_sejour) %>% 
              unique %>% 
              mutate(early_test = 1)) %>% 
  filter(is.na(early_test))


data_PCR_NDA %>% 
  mutate(Virus = gsub("/", "", Virus)) %>% 
  filter(!is.na(TestPos)) %>% 
  mutate(TestPos = case_match(TestPos, 0 ~ "Negative at 0-2 days", 
                              1 ~ "Positive at 0-2 days")) %>% 
  left_join(NDA_virus_prediction_sejour %>% select(NDA_pseudo, Virus, prediction)) %>%
  {rbind(., untested_IPP_prediction %>% 
           ungroup %>% 
           transmute(NDA_pseudo, Virus, prediction, TestPos = "Unknown"))} %>% 
  mutate(Virus = factor(Virus, levels = virus_vec)) %>% 
  filter(Virus != "InfluenzaNT") %>% 
  ggplot(aes(x = prediction, group = TestPos, fill = TestPos)) + 
  geom_histogram(aes(y = after_stat(density)), alpha = 0.5, position = "identity") + 
  facet_wrap(.~Virus) +
  labs(x = "Predicted probability of positive test", fill = "True test result") + 
  theme_bw()

ggsave("output/plots/xg_prediction.png", width = 20, height = 12, units = "cm", dpi = 600)



#### variable importance

library(data.table)


dt = NULL

# virus_index = 2
for(virus_index in 1:9){
  
  this_virus = virus_vec[virus_index]
  this_string = virus_index*nrounds
  
  output_stem = paste(this_virus)
  print(output_stem)
  # load rf trained model
  # load(file = paste0("rf/train_", output_stem, ".RData"), verbose = T)
  # load(file = paste0("ml/trainXG_", output_stem, ".RData"), verbose = T)
  xg_out <- xgb.load(modelfile = paste0("ml/trainXG_", output_stem, ".ubj"))
  
  dt_piece = xgb.importance(model = xg_out)
  dt_piece[, Virus := this_virus][, rank := 1:nrow(dt_piece)]
  
  if(is.null(dt)){
    dt = dt_piece
  } else {
    dt = rbind(dt, dt_piece)
  }
  
}


dt[, .(ifelse(Virus == shift(Virus, 1, fill = "NA"), "", Virus), 
                   Rank = rank, 
                   Contribution = Gain, 
                   Variable = case_match(Feature, 
                                         "season_admitted" ~ "Season", 
                                         "monthnum" ~ "Month",
                                         "age_group" ~ "Age group", 
                                         .default = Feature
                   ))][Contribution >= 0.015, ] %>% 
  left_join(cim10_ref %>% transmute(Variable = code, Description = cim10_description)) %>%
  mutate(Description = ifelse(is.na(Description), NA, paste("ICD-10:", Description))) %>% 
  mutate(Description = gsub(" w ", " with ", Description), 
         Description = gsub(" oth ", " other ", Description), 
         Description = gsub(" ident ", " identified ", Description), 
         Description = gsub(" resp ", " respiratory ", Description), 
         Description = gsub(" manifest$", " manifestation", Description), 
         Description = gsub("Chr ", "Chronic ", Description), 
         Description = gsub(" pulmon ", " pulmonary ", Description), 
         Description = gsub(" infct", " infection", Description), 
         Description = gsub("classd", "classified", Description), 
         Description = gsub("elswhr", "elsewhere", Description)
         ) %>% 
  mutate(Description = ifelse(Variable == "relativeWeek", "Week relative to December 31st", Description), 
         Description = ifelse(Variable == "Season", "September to August", Description), 
         Description = ifelse(Variable == "Month", "Calendar month", Description), 
         Description = ifelse(Variable == "catego_UM", "Hospital service category", Description), 
         Description = ifelse(Variable == "CRP_min", "C-reactive protein - minimum", Description), 
         Description = ifelse(Variable == "CRP_max", "C-reactive protein - maximum", Description), 
         Description = ifelse(Variable == "LYMP_min", "Lymphocytes - minimum", Description), 
         Description = ifelse(Variable == "GLUC_min", "Glucose - minimum", Description), 
         Description = ifelse(Variable == "GLUC_max", "Glucose - maximum", Description), 
         Description = ifelse(Variable == "PLAQ_max", "Platelets - maximum", Description), 
         Description = ifelse(Variable == "NEUT_max", "Neutrophils - maximum", Description), 
         Description = ifelse(Variable == "GLUC_mean", "Glucose - mean", Description), 
         Description = ifelse(Variable == "PH_mean", "pH - mean", Description), 
         Description = ifelse(Variable == "PO2_mean", "pO2 - mean", Description), 
         Description = ifelse(Variable == "CREA_min", "Creatinine - minimum", Description)) %>% 
  mutate(Contribution = format(round(Contribution, 3), nsmall = 3)) %>% 
  fwrite("output/tables/xg_importance.csv", sep = ";")

