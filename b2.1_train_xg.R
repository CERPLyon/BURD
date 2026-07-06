# just_rf
sessionInfo()

print("loading packages")

library(Matrix)
library(tidyverse)
library(caret)
library(xgboost)
library(PRROC)

source("xgb_ncv.R")

# library(randomForest)

print("getting control variables")
cmds = commandArgs(trailingOnly = T)
virus_index = as.numeric(cmds[1])
nrounds = as.numeric(cmds[2])
optimise = (cmds[3] == "optimise")
auc = (cmds[4] == "auc")
execute = (cmds[5] == "execute")
uncertainty = (cmds[6] == "uncertainty")
# virus_index = 9
# nrounds = 5000
# optimise = F
# auc = F
# execute = F
# uncertainty = T

# print(cmds)
# print(c(virus_index, nrounds, execute_only))

p_training = 0.7

# load files
print("loading files")
load("data/data_NDA_reduced.RData", verbose = T)
load("data/data_PCR_NDA.RData", verbose = T)

# print("removing biomarker columns with missing data")
# # better to do this currently, otherwise the algorithm is presumably removing any lines with missing data
# # only a few individuals will have results for every biomarker
# data_NDA_reduced = data_NDA_reduced %>%
#   select(1:po2_n)

# prep data
print("source prep file")
source("b2.2_prep_xg.R")


if(optimise){
  print("optimise XGBoost")
  
  data_NDA_sparse_testResult_xgb.D <- xgb.DMatrix(data_NDA_sparse_testResult[, -1], 
                                                  label = data_NDA_sparse_testResult[, 1])
  
  
  # Grid search for hyperparameter tuning
  search_grid <- expand.grid(
    max_depth = c(3, 5, 7), 
    min_child_weight = c(1, 3, 5), 
    subsample = c(0.6, 0.8, 1.0), 
    colsample_bytree = c(0.6, 0.8, 1.0), 
    learning_rate = c(0.01, 0.1, 0.3)
    
    # gamma, 
    # reg_alpha, 
    # reg_lambda, 
    # scale_pos_weight, 
    # n_estimators, 
  )
  
  
  best_auc <- 0
  best_params <- list()
  # i = 243
  for (i in 1:nrow(search_grid)) {
    print(i)
    
    test_params = c(list(objective = "binary:logistic"),  search_grid[i, ] %>% unlist)
    
    cv_results <- xgb.cv(
      params = test_params,
      data = data_NDA_sparse_testResult_xgb.D,
      nfold = 5,
      nrounds = nrounds,
      early_stopping_rounds = 10,
      verbose = F
    )
    
    
    mean_auc <- max(cv_results$evaluation_log$test_logloss_mean)
    
    if (mean_auc > best_auc) {
      best_auc <- mean_auc
      best_params <- test_params
      best_nrounds <- cv_results$early_stop$best_iteration
      
      print(c(best_auc, best_params, best_nrounds))
      save(best_auc, best_params, best_nrounds, file = paste0("ml/optmiseXG_", output_stem, ".RData"))
      
    }
  }
  
} 


load(file = paste0("ml/optmiseXG_", output_stem, ".RData"))

if(auc){
  ncv_out <- xgb.ncv_wrapper(x = data_NDA_sparse_testResult[, -1], 
                             y = data_NDA_sparse_testResult[, 1], 
                             params = best_params, 
                             nrounds = best_nrounds, 
                             k_folds = 10, 
                             n_repeats = 5
  )
  print(paste0("AUC: ", paste0(ncv_out$auc, collapse = "\n"), " - baseline: ", ncv_out$baseline))
  
  print("save cross-validation results")
  save(ncv_out, file = paste0("ml/cvXG_", output_stem, ".RData"))
}

if(execute){
  print("run XGBoost")
  xg_out <- xgboost(x = data_NDA_sparse_train[, -1], 
                    y = factor(data_NDA_sparse_train[, 1], levels = c(0, 1)), 
                    # xgb_model = bstSparse,
                    # objective = "binary:logistic",
                    params = best_params, 
                    nrounds = best_nrounds,
                    # max_depth = 10, 
                    # nthreads = 2, 
                    # min_child_weight = 1, 
                    # objective = "binary:logistic", 
                    
                    # params = xgparams, 
                    verbosity = 1
  )
  
  
  print("save training object")
  save(xg_out, file = paste0("ml/trainXG_", output_stem, ".RData"))
  xgb.save(xg_out, fname = paste0("ml/trainXG_", output_stem, ".ubj"))
  
}

if(uncertainty){
  n_training = nrow(data_NDA_sparse_train)
  # i = 1
  n_boots = 1000
  for(i in 1:n_boots){
    print(paste("boot", i))
    
    bootstrap_data_NDA_sparse_train <- data_NDA_sparse_train[sample(n_training, size = n_training, replace = T), ]
    print("run XGBoost bootstrap")
    boot_xg_out <- xgboost(x = bootstrap_data_NDA_sparse_train[, -1], 
                           y = factor(bootstrap_data_NDA_sparse_train[, 1], levels = c(0, 1)), 
                           # xgb_model = bstSparse,
                           # objective = "binary:logistic",
                           params = best_params, 
                           nrounds = best_nrounds,
                           # max_depth = 10, 
                           # nthreads = 2, 
                           # min_child_weight = 1, 
                           # objective = "binary:logistic", 
                           
                           # params = xgparams, 
                           verbosity = 0
    )
    
    
    boot_prediction_allNDA <- predict(boot_xg_out, newdata = data_NDA_sparse[,-1], type = "response")
    
    boot_NDA_virus_prediction_piece <- tibble(boot = i, NDA_pseudo = data_NDA_thisVirus$NDA_pseudo, season_admitted = data_NDA_thisVirus$season_admitted, Virus = this_virus, prediction = boot_prediction_allNDA) %>% 
      mutate(prediction = ifelse((Virus != "SARS-CoV-2") | (season_admitted %in% season_vec[-(1:3)]), prediction, 0)) %>% 
      select(-season_admitted)
    
    
    if(i == 1){
      boot_NDA_virus_prediction = boot_NDA_virus_prediction_piece
    } else {
      boot_NDA_virus_prediction = rbind(boot_NDA_virus_prediction, 
                                     boot_NDA_virus_prediction_piece)
    }
    
    boot_NDA_virus_prediction %>% 
      group_by(NDA_pseudo, Virus) %>% 
      summarise(predictionLo = quantile(prediction, 0.025), predictionHi = quantile(prediction, 0.975)) %>% 
      write_csv2(paste0("ml/bootPredictions_", output_stem, ".csv"))
    
    # boot_NDA_virus_prediction %>% 
    #   pivot_wider(id_cols = c("NDA_pseudo", "Virus"), names_from = "boot", values_from = "prediction") %>% 
    #   write_csv2(paste0("ml/bootPredictions_", output_stem, ".csv"))
    
  }
  

  
  
}

print("end")
