build_RFSRC_BASE <- function(model, 
                             path_data_complete,
                             model_type = NULL,
                             var_sel = NULL,
                             horizon = 7,
                             tune = TRUE,
                             iters_design = 20,
                             iters_optim = 30,
                             tuning_metric = "logloss", # character to support custom functions?
                             importance_type = "none",
                             adm_cens = NULL,
                             ...) {
  
  # fixed RF configuration
  num_trees <- 1000
  
  n_cores <- detectCores()
  options(rf.cores = n_cores, mc.cores = n_cores)
  print(sprintf("Cores: %s", n_cores))
  
  # build file names
  preds_name <- paste("preds", model, sep = "_")
  var_imp_name <- paste("var_imp", model, sep = "_")
  hyperparams_name <- paste("hyperparams", model, sep = "_")
  results_name <- paste("results", model, sep = "_")
  timings_name <- paste("timings", model, sep = "_")
  
  preds_file <- paste0(preds_dir_base, preds_name)
  var_imp_file <- paste0(var_imp_dir_base, var_imp_name)
  hyperparams_file <- paste0(models_path, hyperparams_name)
  results_file <- paste0(preds_dir_base, results_name)
  timings_file <- paste0(models_path, timings_name)
  
  OOB_results_name <- paste("OOB_results", model, sep = "_")
  OOB_results_file <- paste0(preds_dir_base, OOB_results_name)
  OOB_preds_name <- paste("OOB_preds", model, sep = "_")
  OOB_preds_file <- paste0(preds_dir_base, OOB_preds_name)
  
  # get filenames for imputed datasets 
  datasets_files <- list.files(path_data_complete, 
                               recursive = TRUE, full.names = TRUE)
  train_files <- datasets_files[str_detect(datasets_files, "train")]
  test_files <- datasets_files[str_detect(datasets_files, "test")]
  
  # keep results
  predictions <- init_preds()
  var_imp <- init_var_imp()
  hyperparams <- init_hyperparams()
  timings <- init_timings()
  
  OOB_predictions <- init_preds()
  
  positive_class <- "CLABSI"
  negative_class <- "no_CLABSI"
  
  obj_fun <- makeSingleObjectiveFunction(
    name = "RFSRC_mlrMBO",
    fn =   function(x){
      
      RF_obj <- rfsrc(form, data_fit,
                      nodesize = x["nodesize"],
                      mtry = x["mtry"],
                      ntree = num_trees,
                      # specify subsamples
                      bootstrap = "by.user",
                      samp = inbags,
                      save.memory = TRUE, 
                      do.trace = FALSE,
                      importance = "none",
                      ...)
      
      # take binary OOB predictions for all model types
      if (model_type == "bin"){
        OOB_preds <- RF_obj$predicted.oob[,c(positive_class, negative_class)]
      } else if (model_type == "multinomial"){
        OOB_preds <- RF_obj$predicted.oob %>% as.data.frame()
        cols_neg <- colnames(OOB_preds)[!colnames(OOB_preds) %in% positive_class]
        OOB_preds[,negative_class] <- rowSums(OOB_preds[,cols_neg])
        OOB_preds <- OOB_preds[,c(positive_class, negative_class)]
      } else if (str_detect(model_type, "surv")){
        preds_surv_x_days <- RF_obj$survival.oob[,horizon]
        preds_event_x_days <- 1 - preds_surv_x_days
        OOB_preds <- cbind(preds_event_x_days, preds_surv_x_days) %>% as.data.frame()
        colnames(OOB_preds) <- c(positive_class, negative_class)
      } else if (str_detect(model_type, "CR")){
        preds_CIF_x_days <- RF_obj$cif.oob[,horizon,1]
        OOB_preds <- cbind(preds_CIF_x_days, 1 - preds_CIF_x_days) %>% as.data.frame()
        colnames(OOB_preds) <- c(positive_class, negative_class)
      } else {
        stop("model type not supported")
      }
      
      if (tuning_metric == "BS"){
        tuning_metric_value <- BS(OOB_preds[,c(positive_class, negative_class)], 
                                  y_true_matrix)
      } else if (tuning_metric == "AUROC"){
        tuning_metric_value <- ModelMetrics::auc(y_true_matrix[,positive_class], 
                                                 OOB_preds[,positive_class])
        tuning_metric_value <- - tuning_metric_value # to minimize
      } else if (tuning_metric == "BSS"){
        tuning_metric_value <- 1 - BSnorm(OOB_preds[,c(positive_class, negative_class)], 
                                          y_true_matrix)
        tuning_metric_value <- - tuning_metric_value # to minimize
      } else if (tuning_metric == "logloss"){
        obs <- y_true_matrix[,positive_class]
        pred <- OOB_preds[,positive_class]
        pred <- pmax(pmin(pred, 1-10^-15), 10^-15)
        tuning_metric_value <- -mean(obs * log(pred) + (1 - obs) * log(1 - pred))
      } else {
        stop ("tuning_metric not supported")
      }
      
      tuning_metric_value
      
    },
    par.set = makeParamSet(
      makeIntegerParam("nodesize", lower = 50L, upper = 4000L), 
      makeIntegerParam("mtry", lower = 2L, upper = 15L)
    ),
    minimize = TRUE
  )
  
  do_bayes <- function(n_design = NULL, opt_steps = NULL, of = obj_fun, plot = FALSE) {
    des <- generateDesign(n = n_design,
                          par.set = getParamSet(of),
                          fun = lhs::randomLHS)
    control <- makeMBOControl() %>%
      setMBOControlTermination(., iters = opt_steps)
    ## kriging with a matern(3,2) covariance function is the default surrogate model for numerical domains
    ## but if you wanted to override this you could modify the makeLearner() call below to define your own
    ## GP surrogate model with more or less smoothness, or use an entirely different method
    run <- mbo(fun = of,
               design = des,
               learner = makeLearner("regr.km", predict.type = "se", 
                                     covtype = "matern3_2", 
                                     nugget.stability = 1e-8,
                                     control = list(trace = FALSE)),
               control = control,
               show.info = FALSE)
    if (plot){
      opt_plot <- run$opt.path$env$path %>%
        mutate(Round = row_number()) %>%
        mutate(type = case_when(Round <= n_design ~ "Design",
                                TRUE ~ "mlrMBO optimization")) %>%
        ggplot(aes(x= Round, y= y, color= type)) +
        geom_point() +
        labs(title = "mlrMBO optimization") +
        ylab("Brier")
    } else {
      opt_plot <- NULL
    }
    print(run$x)
    return(list(run = run, plot = opt_plot))
  }
  
  set.seed(2023)
  
  # build model for each df
  for (f in train_files){
    
    print(f)
    start_time <- Sys.time()
    
    # load data
    load(f) # loads train data named data_train
    test_file <- str_replace(f, "train", "test") # corresponding test set file
    load(test_file) 
    
    # filter LMs for which eventtime is at exact LM time
    data_train <- data_train %>% 
      filter(eventtime > 0) 
    
    data_test <- data_test %>% 
      filter(eventtime > 0)
    
    if (var_sel == "LITDOMK"){
      cols_keep <- vars_selected_DOMK_LIT[vars_selected_DOMK_LIT %in% colnames(data_train)]
      cols_keep <- cols_keep[!cols_keep %in% c("functioneelDossierNr", "type", 
                                               "eventtime", "LM")]
    } else if (var_sel == "ALL") {
      cols_keep <- colnames(data_train)
      cols_keep <- cols_keep[!cols_keep %in% c("functioneelDossierNr", "type", 
                                               "eventtime", "LM")]
      
      # filter MED_3d (leave only MED_7d)
      cols_med_remove <- cols_keep[str_detect(cols_keep, "_3d_")]
      cols_med_remove <- cols_med_remove[str_detect(cols_med_remove, "MED_")]
      cols_keep <- cols_keep[!cols_keep %in% cols_med_remove]    
    } else {
      stop("var_sel value not supported yet")
    }
    
    # keep X and Y as matrices
    train_X <- data_train %>% 
      select(all_of(cols_keep)) %>% 
      mutate_if(is.character, as.factor)
    
    test_X <- data_test %>% 
      select(all_of(cols_keep)) %>% 
      mutate_if(is.character, as.factor)
    
    # outcome with horizon x days
    if (model_type == "bin") {
      train_y <- data_train %>% 
        select(c("type", "eventtime")) %>% 
        mutate(CLABSI = if_else(type == "CLABSI" & eventtime <= horizon, "CLABSI", "no_CLABSI")) %>% 
        pull(CLABSI) %>% as.factor()
      
    } else if (model_type == "multinomial") {
      train_y <- data_train %>% 
        select(c("type", "eventtime")) %>% 
        mutate(CLABSI = case_when(type == "CLABSI" & eventtime <= horizon ~ "CLABSI", 
                                  type == "Death" & eventtime <= horizon ~ "Death",
                                  type == "Discharge" & eventtime <= horizon ~ "Discharge",
                                  TRUE ~ "Censored")) %>% 
        pull(CLABSI) %>% as.factor()
      
    } else if (str_detect(model_type, "surv")){
      # first, make survival and ceiling
      train_y <- data_train %>%
        select(c("type", "eventtime")) %>% 
        mutate(type = if_else(type == "CLABSI", 1, 0), # make survival
               eventtime = ceiling(eventtime)) # ceiling for computational time
      
      # then, apply adm censoring
      if (!is.null(adm_cens)){
        train_y <- train_y %>%
          mutate(type = if_else(eventtime > adm_cens, 0, type),
                 eventtime = if_else(eventtime > adm_cens, adm_cens, eventtime))
      }
      
      # finally move all events censored before 7 at 7 (keep in risk set)
      if (str_detect(model_type, "cens7")){ # only cens 7 for now (hardcoded at 7)
        train_y <- train_y %>%
          mutate(eventtime = if_else(type == 0 & eventtime <= 7, 7, eventtime)) 
      } 
      
    } else if (str_detect(model_type, "CR")){
      train_y <- data_train %>%
        select(c("type", "eventtime")) %>%
        mutate(type = case_when(type == "CLABSI" ~ 1,
                                type == "Discharge" ~ 2,
                                type == "Death" ~ 3,
                                TRUE ~ NA_real_),
               eventtime = ceiling(eventtime))
      
      # apply adm censoring
      if (!is.null(adm_cens)){
        train_y <- train_y %>%
          mutate(type = if_else(eventtime > adm_cens, 0, type),
                 eventtime = if_else(eventtime > adm_cens, adm_cens, eventtime))
      }
      
    } else {
      stop("model type not supported")
    }
    
    test_y <- data_test %>% 
      select(c("type", "eventtime")) %>% 
      mutate(CLABSI = if_else(type == "CLABSI" & eventtime <= horizon, "CLABSI", "no_CLABSI")) %>% 
      pull(CLABSI) %>% as.factor()
    
    if (model_type %in% c("bin", "multinomial")){
      y_true_0_1 <- ifelse(train_y == positive_class, 1, 0)
    } else { # survival, CR
      y_true_0_1 <- ifelse(train_y$type == 1 & train_y$eventtime <= 7, 1, 0)
    }

    y_true_matrix <- matrix(cbind(y_true_0_1, 1 - y_true_0_1), ncol = 2)
    colnames(y_true_matrix) <- c(positive_class, negative_class)
    
    train_y_bin <- data_train %>% 
      select(c("type", "eventtime")) %>% 
      mutate(CLABSI = if_else(type == "CLABSI" & eventtime <= horizon, "CLABSI", "no_CLABSI")) %>% 
      pull(CLABSI) %>% as.factor()
    
    # data fit
    data_fit <- cbind(train_X, train_y)
    
    if (model_type %in% c("bin", "multinomial")){
      form <- as.formula("train_y ~ .")
    } else if (str_detect(model_type, "surv") | str_detect(model_type, "CR")){
      form <- as.formula("Surv(eventtime, type) ~ .")
    }
    
    # get inbag samples for all trees using sampling by adm id
    start_time_read_inbag <- Sys.time()
    inbag_file <- get_inbags_filename_boot(boot_file_name_prefix_minsize, f)
    load(inbag_file)
    message(sprintf("Read inbags in %s secs", 
                    as.numeric(difftime(Sys.time(), start_time_read_inbag, units = "secs"))))
    
    start_time_tuning <- Sys.time()
    runs <- do_bayes(n_design = iters_design, of = obj_fun, opt_steps = iters_optim)
    time_tuning <- as.numeric(difftime(Sys.time(), start_time_tuning, units = "secs"))
    
    best_fit <- runs$run$x
    print(best_fit)
    
    # build final model 
    start_time_final_model <- Sys.time()
    RF_model <- rfsrc(form, data_fit,
                      nodesize = best_fit$nodesize,
                      mtry = best_fit$mtry,
                      ntree = num_trees,
                      # specify subsamples
                      bootstrap = "by.user",
                      samp = inbags,
                      save.memory = TRUE, 
                      do.trace = FALSE,
                      split.depth = "all.trees",
                      importance="none",
                      ...)
    time_final_model <- as.numeric(difftime(Sys.time(), start_time_final_model, units = "secs"))
    
    # save hyperparams
    best_results <- best_fit %>% unlist()
    
    hyperparams <- hyperparams %>%
      add_row(hyperparameter = best_results %>% names(),
              value = best_results %>% as.character(),
              train_set = str_replace(f, path_data_complete, ""),
              model = model,
              LM = 0)
    
    # predict on test set
    if (model_type %in% c("bin", "multinomial")) {
      start_time_predict <- Sys.time()
      test_preds <- predict(RF_model, test_X)$predicted[,positive_class]
      time_predict <- as.numeric(difftime(Sys.time(), start_time_predict, units = "secs"))
    } else if (str_detect(model_type, "surv")){
      start_time_predict <- Sys.time()
      test_preds <- predict(RF_model, test_X)$survival[,horizon]
      test_preds <- 1 - test_preds
      time_predict <- as.numeric(difftime(Sys.time(), start_time_predict, units = "secs"))
      
    } else if (str_detect(model_type, "CR")){
      start_time_predict <- Sys.time()
      test_preds <- predict(RF_model, test_X)$cif[,horizon,1]
      time_predict <- as.numeric(difftime(Sys.time(), start_time_predict, units = "secs"))
      
    } else {
      stop("model type not supported")
    }
    
    predictions <- predictions %>% 
      add_row(preds = test_preds,
              y_true_cat = test_y,
              y_true_time = NA_real_,
              train_set = str_replace(f, path_data_complete, ""),
              test_set = str_replace(test_file, path_data_complete, ""),
              model = model,
              LM = 0,
              functioneelDossierNr = data_test$functioneelDossierNr,
              CAT_catheter_episode = data_test$CAT_catheter_episode)
    
    # save OOB predictions 
    if (model_type %in% c("bin", "multinomial")) {
      OOB_preds <- RF_model$predicted.oob[,positive_class]
    } else if (str_detect(model_type, "surv")){
      OOB_preds <- RF_model$survival.oob[,horizon]
      OOB_preds <- 1 - OOB_preds
    } else if (str_detect(model_type, "CR")){
      OOB_preds <- RF_model$cif.oob[,horizon,1]
    } else {
      stop("model type not supported")
    }
    
    OOB_predictions <- OOB_predictions %>% 
      add_row(preds = OOB_preds,
              y_true_cat = train_y_bin,
              y_true_time = NA_real_,
              train_set = str_replace(f, path_data_complete, ""),
              test_set = str_replace(test_file, path_data_complete, ""),
              model = model,
              LM = 0,
              functioneelDossierNr = data_train$functioneelDossierNr,
              CAT_catheter_episode = data_train$CAT_catheter_episode)
    
    max_subtree <- max.subtree(RF_model, conservative = FALSE)
    max_subtree_cons <- max.subtree(RF_model, conservative = TRUE)
    
    var_imp <- var_imp %>% 
      # split.depth var imp
      add_row(variable = colnames(train_X),
              value = RF_model$split.depth %>% colMeans(), 
              train_set = str_replace(f, path_data_complete, ""),
              model = model,
              LM = 0,
              var_imp_type = "split_depth") %>% 
      add_row(variable = names(max_subtree$order[,1]),
              value = max_subtree$order[,1], 
              train_set = str_replace(f, path_data_complete, ""),
              model = model,
              LM = 0,
              var_imp_type = "max_subtree") %>% 
      add_row(variable = names(max_subtree_cons$order[,1]),
              value = max_subtree_cons$order[,1], 
              train_set = str_replace(f, path_data_complete, ""),
              model = model,
              LM = 0,
              var_imp_type = "max_subtree_cons")
    
    # save timings 
    timings <- timings %>% 
      add_row(type = c("tuning", "build final model", "predict"),
              value = c(time_tuning, time_final_model, time_predict),
              train_set = str_replace(f, path_data_complete, ""),
              model = model,
              LM = NA_real_) 
    
    message(sprintf("DONE in %s minutes.", 
                    difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  }
  
  # save predictions, variable importance, ...
  save(predictions, file = preds_file)
  save(var_imp, file = var_imp_file)
  save(hyperparams, file = hyperparams_file)
  save(timings, file = timings_file)
  save(OOB_predictions, file = OOB_preds_file)
}
