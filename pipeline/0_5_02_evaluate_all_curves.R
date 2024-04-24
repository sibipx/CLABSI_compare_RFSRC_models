# source all files in the R directory
files_to_source <- list.files("R/", recursive = TRUE, full.names = TRUE)
invisible(lapply(files_to_source, function(x) source(x, chdir = TRUE)))

library(ROCR)

pred_mat_x_y <- function(preds){
  
  train_sets <- unique(preds$train_set)
  pred_mat_list <- list()
  
  for (i in 1:length(train_sets)){
    preds_ts <- preds %>% filter(train_set == train_sets[i]) %>% 
      mutate(y_true_0_1 = if_else(y_true_cat == positive_class, 1, 0)) %>% 
      select(y_true_0_1, preds)
    
    pred_mat <- prediction(preds_ts$preds, labels = preds_ts$y_true_0_1)
    pred_mat_list[[i]] <- pred_mat
  }
  
  return(pred_mat_list)
}

lift_table <- function(y_true, preds, bin_number = 10) {
  cbind(y_true, preds) %>% as.data.frame() %>% 
    dplyr::mutate(yhat_bin = ggplot2::cut_number(preds, bin_number)) %>%
    dplyr::group_by(yhat_bin) %>%
    dplyr::summarise(mean_y = mean(y_true), mean_yhat = mean(preds)) %>% 
    dplyr::select(mean_y, mean_yhat)
}

# function to subsample only n x points from a curve 
# AUROC & AUPRC have points = number of predictions - too high - inefficient for storing and plotting
subsample_x <- function(df, n = 2000) {
  ideal_spacing <- seq(min(df$x), max(df$x), length.out=n)
  ids <- sapply(ideal_spacing, function(x) which.min(abs(df$x-x)) )
  df <- df[ids,]
  return(df)
}

# prediction files
preds <- init_preds()
pred_files_base <- list.files(preds_dir_base, full.names=TRUE)
pred_files_base <- pred_files_base[str_detect(pred_files_base, "preds_BASE_")]

pred_files_dyn <- list.files(preds_dir_dyn, full.names=TRUE)
pred_files_dyn <- pred_files_dyn[str_detect(pred_files_dyn, "preds_DYN_")]

pred_files <- c(pred_files_base, pred_files_dyn)
pred_files <- pred_files[!str_detect(pred_files, "OOB_preds_")]

positive_class <- "CLABSI"

print(pred_files)

# run only for selected models
# pred_files <- pred_files[str_detect(pred_files, "DYN")]
# pred_files <- pred_files[str_detect(pred_files, "CR7d") | str_detect(pred_files, "surv7d")]

for (pf in pred_files){
  
  print(pf)
  start_time <- Sys.time()
  
  load(pf)
  
  model <- predictions %>% distinct(model) %>% pull(model)
  if (length(model) > 1)  stop("More than 1 model in a file")
  
  pred_mat <- pred_mat_x_y(predictions)
  
  # ROC curve
  pred_mat_obj <- lapply(pred_mat, function(x) ROCR::performance(x, "tpr", "fpr"))
  pred_mat_obj <- lapply(pred_mat_obj, function(obj) tibble(x = obj@x.values[[1]],
                                                            y = obj@y.values[[1]]))
  
  # sample only 200 points from the x of the curve (equally spaced) - more efficient
  pred_mat_obj <- lapply(pred_mat_obj, subsample_x)
  
  names(pred_mat_obj) <- if (is.null(names(pred_mat_obj))) seq_len(length(pred_mat_obj)) else names(pred_mat_obj)
  
  pred_mat_df <- do.call("rbind", lapply(names(pred_mat_obj), function(x) cbind(index = x, pred_mat_obj[[x]])))
  
  pred_mat_df_AUROC <- pred_mat_df %>% 
    add_column(model = model,
               curve_type = "AUROC",
               y_all = NA_real_,
               y_none = NA_real_) %>% 
    mutate(index = as.double(index))
  
  rm(pred_mat_obj, pred_mat_df)
  
  # PR curve
  pred_mat_obj <- lapply(pred_mat, function(x) ROCR::performance(x, "prec", "rec"))
  pred_mat_obj <- lapply(pred_mat_obj, function(obj) tibble(x = obj@x.values[[1]],
                                                            y = obj@y.values[[1]]))
  
  # sample only 200 points from the x of the curve (equally spaced) - more efficient
  pred_mat_obj <- lapply(pred_mat_obj, subsample_x)
  
  names(pred_mat_obj) <- if (is.null(names(pred_mat_obj))) seq_len(length(pred_mat_obj)) else names(pred_mat_obj)
  
  pred_mat_df <- do.call("rbind", lapply(names(pred_mat_obj), function(x) cbind(index = x, pred_mat_obj[[x]])))
  
  pred_mat_df_AUPRC <- pred_mat_df %>% 
    add_column(model = model,
               curve_type = "AUPRC",
               y_all = NA_real_,
               y_none = NA_real_) %>% 
    mutate(index = as.double(index))
  
  rm(pred_mat_obj, pred_mat_df)
  
  # calibration curves - deciles
  calib_curves_deciles <- predictions %>% 
    mutate(y_true = if_else(y_true_cat == positive_class, 1, 0)) %>% 
    select(preds, y_true, train_set) %>% 
    group_by(train_set) %>% 
    mutate(calibration_values = list(lift_table(y_true, preds))) %>%
    ungroup() %>% 
    distinct(train_set, calibration_values) %>%
    unnest(calibration_values) %>% 
    rename(x = mean_yhat, y = mean_y) %>% 
    mutate(index = as.numeric(str_sub(train_set,-3,-1))) %>% 
    select(-train_set)
  
  calib_curves_deciles <- calib_curves_deciles %>% 
    add_column(model = model,
               curve_type = "calib_deciles",
               y_all = NA_real_,
               y_none = NA_real_) 
  
  # calibration curves - splines
  calib_curves_splines <- predictions %>%
    mutate(preds = if_else(preds == 0, 0.0000000001, preds),
           preds = if_else(preds == 1, 0.9999999999, preds)) %>% 
    mutate(y_true = if_else(y_true_cat == positive_class, 1, 0)) %>% 
    group_by(train_set, test_set) %>%
    mutate(calibration_values = list(calib_curves_x_y(y_true, preds))) %>%
    ungroup() %>%
    distinct(train_set, test_set, calibration_values) %>%
    unnest(calibration_values) %>% 
    mutate(index = as.numeric(str_sub(train_set,-3,-1))) %>% 
    select(-c(train_set, test_set))
  
  calib_curves_splines <- calib_curves_splines %>% 
    add_column(model = model,
               curve_type = "calib_splines",
               y_all = NA_real_,
               y_none = NA_real_)
  
  # decision curves
  dca_df <- predictions %>% 
    select(preds, y_true_cat, train_set, test_set) %>% 
    mutate(y_true = if_else(y_true_cat == positive_class, 1, 0)) %>% 
    as.data.frame()
  
  all_test_sets <- unique(dca_df$test_set)
  
  dca_df_1 <- dca_df %>% 
    filter(test_set == all_test_sets[[1]]) %>% 
    as.data.frame()
  
  dca_results_1 <- dca(outcome = dca_df_1$y_true,
                       preds = dca_df_1$preds,
                       xstart = 0.001,
                       xstop = 0.22,
                       xby = 0.0006)
  
  dca_results <- data.frame(matrix(ncol = ncol(dca_results_1) + 1, nrow = 0))
  colnames(dca_results) <- c(colnames(dca_results_1), "test_set")
  dca_results$test_set <- as.character(dca_results$test_set)
  
  for (ts in all_test_sets) {
    dca_df_this <- dca_df %>%
      filter(test_set == ts) %>%
      as.data.frame()
    
    dca_results_this <- dca(outcome = dca_df_this$y_true,
                            preds = dca_df_this$preds,
                            xstart = 0.001,
                            xstop = 0.22,
                            xby = 0.0006)
    dca_results_this$test_set <- ts
    
    dca_results <- dca_results %>%
      add_row(dca_results_this)
  }
  
  dca_results <- dca_results %>% 
    mutate(index = as.numeric(str_sub(test_set,-3,-1))) %>% 
    select(-c(test_set)) %>% 
    rename(x = threshold,
           y = pred,
           y_all = all,
           y_none = none) %>% 
    add_column(model = model,
               curve_type = "DCA")
  
  # write to disk
  model_curves <- pred_mat_df_AUROC %>% 
    add_row(pred_mat_df_AUPRC) %>% 
    add_row(calib_curves_deciles) %>% 
    add_row(calib_curves_splines) %>% 
    add_row(dca_results) 
  
  save(model_curves, file = paste0("predictions/2012_2013/curves/curves_", model, ".Rdata"))
  
  rm(model_curves, pred_mat_df_AUROC, pred_mat_df_AUPRC, 
     calib_curves_deciles, calib_curves_splines, dca_results)
  
  message(sprintf("DONE in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
}

