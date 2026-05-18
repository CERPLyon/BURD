
xgb.ncv_wrapper <- function(x, y, params, nrounds, k_folds, n_repeats){
  
  
  all_results_list <- list()
  all_predictions <- matrix(nrow = n_repeats, ncol = length(y))
  all_auc = double(n_repeats)
  
  rep = 1
  fold = 1
  for (rep in 1:n_repeats) {
    
    # Stratified fold assignment
    pos_idx <- which(y == 1)
    neg_idx <- which(y == 0)
    
    pos_folds <- sample(rep(1:k_folds, length.out = length(pos_idx)))
    neg_folds <- sample(rep(1:k_folds, length.out = length(neg_idx)))
    
    fold_id <- integer(length(y))
    fold_id[pos_idx] <- pos_folds
    fold_id[neg_idx] <- neg_folds
    
    repeat_results <- data.frame()
    repeat_predictions <- rep(NA, length(y))
    
    for (fold in 1:k_folds) {
      
      print(paste0("rep ", rep, "/", n_repeats, "; fold ", fold, "/", k_folds))
      
      test_idx <- which(fold_id == fold)
      train_idx <- which(fold_id != fold)
      
      dtrain <- xgb.DMatrix(
        data = x[train_idx, ],
        label = y[train_idx]
      )
      
      dtest <- xgb.DMatrix(
        data = x[test_idx, ],
        label = y[test_idx]
      )
      
      watchlist <- list(
        train = dtrain,
        eval = dtest
      )
      
      # -----------------------------------------------------
      # Train model
      # -----------------------------------------------------
      
      model <- xgb.train(
        params = c(params, list(eval_metric = "aucpr")),
        data = dtrain,
        nrounds = nrounds,
        evals = watchlist,
        early_stopping_rounds = 20,
        maximize = TRUE,
        verbose = F
      )
      
      
      # -----------------------------------------------------
      # Predictions
      # -----------------------------------------------------
      
      pred_prob <- predict(model, dtest)
      
      # Store out-of-fold predictions
      repeat_predictions[test_idx] <- pred_prob
      
      pr <- pr.curve(
        scores.class0 = pred_prob[y[test_idx] == 1],
        scores.class1 = pred_prob[y[test_idx] == 0],
        curve = T
      )
      if(F){
        plot(pr)  
      }
      
      
      fold_aucpr = pr$auc.integral    
      
      # Save results
      repeat_results <- bind_rows(
        repeat_results,
        data.frame(
          rep = rep,
          fold = fold,
          aucpr = fold_aucpr,
          best_iteration = attributes(model)$early_stop$best_iteration
        )
      )
      
    }
    
    repeat_pr <- pr.curve(
      scores.class0 = repeat_predictions[y == 1],
      scores.class1 = repeat_predictions[y == 0],
      curve = TRUE
    )
    
    all_auc <- c(repeat_pr$auc.integral)
    
    all_results_list[[rep]] <- repeat_results
    
    all_predictions[rep, ] <- repeat_predictions
    
  }
  
  all_results <- do.call(rbind, all_results_list)
  
  list(auc = all_auc, baseline = mean(y), results = all_results, predictions = all_predictions)
  
}



