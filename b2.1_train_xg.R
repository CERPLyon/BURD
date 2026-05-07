# just_rf
sessionInfo()

print("loading packages")

library(Matrix)
library(tidyverse)
library(caret)
library(xgboost)

# library(randomForest)

print("getting control variables")
cmds = as.numeric(commandArgs(trailingOnly = T))
virus_index = cmds[1]
nrounds = cmds[2]
execute_only = F
# nrounds = 5000
# optimise = (cmds[2] == "optimise")
# virus_index = 9
# optimise = T

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


if(!execute_only){
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



print("end")
