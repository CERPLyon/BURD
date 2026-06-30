


dt_piece = xgb.importance(model = xg_out)



for(virus_index in 1:9){
  
  this_virus = virus_vec[virus_index]
  
  output_stem = paste(this_virus)
  print(output_stem)
  
  xg_out <- xgb.load(modelfile = paste0("ml/trainXG_", output_stem, ".ubj"))
  load(paste0("ml/trainXG_", output_stem, ".RData"), verbose = T)
  
  source("b2.2_prep_xg.R") #this ensures that the data prep is the same as in the actual training script
  p0 = data_NDA_sparse_testResult[, 1] %>% mean
  
  
  variable.names(xg_out) %>% length
  data_NDA_sparse_test %>% dim
  explanation = explain(model = xg_out, 
                        # x_explain = as.matrix(data_NDA_sparse_test[, -1]), 
                        # x_train = as.matrix(data_NDA_sparse_train[, -1]),
                        # y_train = as.matrix(data_NDA_sparse_train[, 1]),
                        x_explain = as.matrix(data_NDA_sparse_test[, 2:6]),
                        x_train = as.matrix(data_NDA_sparse_train[, 2:6]),
                        y_train = as.matrix(data_NDA_sparse_train[, 1]),
                        iterative = T, 
                        phi0 = p0, approach = "empirical", )
  
  ?explain
  shap_values <- shap.values(xgb_model = xg_out, X_train = data_NDA_sparse_testResult)
  
  names(xg_out)
  shap_contrib <- predict(xg_out, as.matrix(data_NDA_sparse_testResult), 
                          predcontrib = TRUE)
 
  colnames(shap_contrib) 
}
