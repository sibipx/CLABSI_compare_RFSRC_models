#library(precrec)

init_preds <- function(){
  
  results <- tibble(preds = double(),
                    y_true_cat = character(),
                    y_true_time = double(),
                    train_set = character(),
                    test_set = character(),
                    model = character(),
                    LM = double(),
                    functioneelDossierNr = double(),
                    CAT_catheter_episode = double())
  
  return(results) 
}

init_var_imp <- function(){
  
  results <- tibble(variable = character(),
                    value = numeric(),
                    train_set = character(),
                    model = character(),
                    LM = double(),
                    var_imp_type = character())
  
  return(results)
}

init_results <- function(){
  
  results <- tibble(model = character(),
                    train_set = character(),
                    test_set = character(),
                    metric = character(),
                    value = numeric(),
                    type = character(),
                    subgroup = character(),
                    LM = double())
  
  return(results)
}

init_hyperparams <- function(){
  
  results <- tibble(hyperparameter = character(),
                    value = character(),
                    train_set = character(),
                    model = character(),
                    LM = double())
  
  return(results)
}

init_timings <- function(){
  
  results <- tibble(type = character(),
                    value = double(),
                    train_set = character(),
                    model = character(),
                    LM = double())
  
  return(results)
}

logit <- function (p) log(p/(1 - p))
expit <- function (x) exp(x)/(1 + exp(x))

cox_first_degree <- function(y, prob){
  dat <- data.frame(e = prob, o = y)
  dat$e[dat$e == 0] = 0.0000000001
  dat$e[dat$e == 1] = 0.9999999999
  dat$logite <- logit(dat$e)
  
  mfit1 = glm(formula = o~I(logite),
              family = binomial(link = "logit"), dat)
  
  slope = mfit1$coefficients[2]
  
  mfit2 = glm(formula = o~offset(logite),
              family = binomial(link = "logit"), dat)
  
  intercept = mfit2$coefficients[1]
  
  return(list(slope = slope, intercept = intercept))
}

# stolen and adapted from library gbm to return x and y values iso plotting
calib_curves_x_y <- function(y,p,
                             distribution="bernoulli",
                             replace=TRUE,
                             line.par=list(col="black"),
                             shade.col="lightyellow",
                             shade.density=NULL,
                             rug.par=list(side=1),
                             xlab="Predicted value",
                             ylab="Observed average",
                             xlim=NULL,ylim=NULL,
                             knots=NULL,df=6,
                             ...)
{
  
  df_result <- NULL
  
  do_it <- function(y,p,
                    distribution="bernoulli",
                    replace=TRUE,
                    line.par=list(col="black"),
                    shade.col="lightyellow",
                    shade.density=NULL,
                    rug.par=list(side=1),
                    xlab="Predicted value",
                    ylab="Observed average",
                    xlim=NULL,ylim=NULL,
                    knots=NULL,df=6,
                    ...){
    p <- logit(p)
    data <- data.frame(y=y,p=p)
    
    if(is.null(knots) && is.null(df))
      stop("Either knots or df must be specified")
    if((df != round(df)) || (df<1))
      stop("df must be a positive integer")
    
    if(distribution=="bernoulli")
    {
      family1 = binomial
    } else if(distribution=="poisson")
    {
      family1 = poisson
    } else
    {
      family1 = gaussian
    }
    gam1 <- glm(y~splines::ns(p,df=df,knots=knots),data=data,family=family1)
    
    x <- seq(min(p),max(p),length=200)
    yy <- predict(gam1,newdata=data.frame(p=x),se.fit=TRUE,type="response")
    
    x <- x[!is.na(yy$fit)]
    yy$se.fit <- yy$se.fit[!is.na(yy$fit)]
    yy$fit <- yy$fit[!is.na(yy$fit)]
    
    if(!is.na(shade.col))
    {
      se.lower <- yy$fit-2*yy$se.fit
      se.upper <- yy$fit+2*yy$se.fit
      if(distribution=="bernoulli")
      {
        se.lower[se.lower < 0] <- 0
        se.upper[se.upper > 1] <- 1
      }
      if(distribution=="poisson")
      {
        se.lower[se.lower < 0] <- 0
      }
      if(distribution=="gamma")
      {
        se.lower[se.lower < 0] <- 0
      }
      if(distribution=="tweedie")
      {
        se.lower[se.lower < 0] <- 0
      }
      if(is.null(xlim)) xlim <- range(se.lower,se.upper,x)
      if(is.null(ylim)) ylim <- range(se.lower,se.upper,x)
    }
    else
    {
      if(is.null(xlim)) xlim <- range(yy$fit,x)
      if(is.null(ylim)) ylim <- range(yy$fit,x)
    }
    
    x <- expit(x)
    
    df_result <- data.frame(x = x, y = yy$fit)
    return(df_result)
  }
  
  # return NULL on a dataset on which the curve fails completely
  df_result <- tryCatch({
    do_it(y,p,
          distribution="bernoulli",
          replace=TRUE,
          line.par=list(col="black"),
          shade.col="lightyellow",
          shade.density=NULL,
          rug.par=list(side=1),
          xlab="Predicted value",
          ylab="Observed average",
          xlim=NULL,ylim=NULL,
          knots=NULL,df=6,
          ...)}, 
    error = function(e){
      NULL
    })
  
  return(df_result)
}

AUPRC <- function(preds, y_true, positive_class){
  curves <- precrec::evalmod(scores = preds, 
                             labels = y_true, 
                             posclass = positive_class)
  aucs <- precrec::auc(curves)
  AUPRC <- aucs[aucs$curvetypes == "PRC", "aucs"]
  
  return(AUPRC)
}

# decision curve analysis
# shamelessly stolen from: https://www.mskcc.org/departments/epidemiology-biostatistics/biostatistics/decision-curve-analysis

#dca <- function(data, outcome, preds, xstart=0.01, xstop=0.99, xby=0.01, 
#                ymin=-0.05, probability=NULL, harm=NULL,graph=TRUE, intervention=FALSE, 
#                interventionper=100, smooth=FALSE,loess.span=0.10) {
dca <- function(outcome, preds, xstart=0.01, xstop=0.99, xby=0.01, 
                ymin=-0.05, harm=NULL,graph=TRUE, intervention=FALSE, 
                interventionper=100, smooth=FALSE,loess.span=0.10) {
  # LOADING REQUIRED LIBRARIES
  require(stats)
  
  #INITIALIZING DEFAULT VALUES FOR PROBABILITES AND HARMS IF NOT SPECIFIED
  harm <- 0
  
  #########  CALCULATING NET BENEFIT   #########
  N <- length(outcome)
  event_rate <- mean(outcome)
  
  thresholds <- seq(from = xstart, to = xstop, by = xby)
  
  nb_all <- event_rate - (1 - event_rate) * thresholds/(1 - thresholds)
  nb_none <- 0
  
  # # CYCLING THROUGH EACH PREDICTOR AND CALCULATING NET BENEFIT
  nb_preds <- sapply(thresholds, function(t) {
    TP <- sum((preds >= t) & (outcome == 1))
    FP <- sum((preds >= t) & (outcome == 0))
    
    nb <- TP/N - FP/N * (t/(1 - t)) - harm
    return(nb)
  })
  
  
  # CYCLING THROUGH EACH PREDICTOR AND SMOOTH NET BENEFIT AND INTERVENTIONS AVOIDED 
  if (smooth==TRUE){
    
    lws=loess(data.matrix(nb_preds[!is.na(nb_preds)]) ~ 
                data.matrix(thresholds[!is.na(nb_preds)]),
              span=loess.span)
    nb_preds_sm <- nb_preds
    nb_preds_sm[!is.na(nb_preds)] <- lws$fitted
    
  }
  
  return(data.frame(threshold = thresholds,
                    pred = nb_preds,
                    all = nb_all,
                    none = nb_none))
  
}  

log_loss <- function(obs, pred){
  
  pred <- pmax(pmin(pred, 1-10^-15), 10^-15)
  
  logloss <- -mean(obs * log(pred) + (1 - obs) * log(1 - pred))
  
  return(logloss)
}

# evaluates different metrics
evaluate_model <- function(preds, y_true, positive_class = "CLABSI", 
                           full_calib = TRUE,
                           eval_ECE = TRUE,
                           eval_ECI = TRUE){
  
  negative_class <- "no_CLABSI"
  
  # make categorical if multiclass
  y_true <- ifelse(y_true == positive_class, positive_class, negative_class)
  
  y_true_0_1 <- ifelse(y_true == positive_class, 1, 0)
  
  # if only 1 class is present in test data return all NA
  if (length(unique(y_true_0_1)) == 1 | sum(y_true_0_1) < 5){
    metrics <- c("AUROC" = NA_real_)
    metrics["mean_calibration"] <- NA_real_
    metrics["BS"] <- NA_real_
    metrics["BSS"] <- NA_real_
    metrics["logloss"] <- NA_real_
    
    if (full_calib){
      metrics["slope"] <- NA_real_
      metrics["intercept"] <- NA_real_
    }
    
    if (eval_ECE) {
      metrics["ECE_CalibratR"] <- NA_real_
      metrics["ECE_Shan"] <- NA_real_
    }
    if (eval_ECI) {
      metrics["ECI_pmcal_rcs"] <- NA_real_
      metrics["ECI_Shan_loess"] <- NA_real_
    }
    
    metrics["AUPRC"] <- NA_real_
    metrics["Sens_at_Sens_80"] <- NA_real_
    metrics["Spec_at_Sens_80"] <- NA_real_
    metrics["Cutoff_for_Sens_80"] <- NA_real_
    metrics["Sens_at_Sens_50"] <- NA_real_
    metrics["Spec_at_Sens_50"] <- NA_real_
    metrics["Cutoff_for_Sens_50"] <- NA_real_
    metrics["Alert_rate_at_Sens_80"] <- NA_real_
    metrics["Alert_rate_at_Sens_50"] <- NA_real_
    metrics["PPV_at_Sens_80"] <- NA_real_
    metrics["NPV_at_Sens_80"] <- NA_real_
    metrics["PPV_at_Sens_50"] <- NA_real_
    metrics["NPV_at_Sens_50"] <- NA_real_
    metrics["NB_at_Sens_80"] <- NA_real_
    metrics["NB_at_Sens_50"] <- NA_real_
    
    return(metrics)
    
  }
  
  # make y matrixs
  y_test_matrix <- matrix(cbind(y_true_0_1, 1 - y_true_0_1), ncol = 2)
  colnames(y_test_matrix) <- c(positive_class, negative_class)
  
  # make preds matrix
  preds_matrix <- matrix(cbind(preds, 1 - preds), ncol = 2)
  colnames(preds_matrix) <- c(positive_class, negative_class)
  
  metrics <- c("AUROC" = ModelMetrics::auc(y_true_0_1, preds))
  metrics["mean_calibration"] <- mean(preds) / mean(y_true_0_1) # estimated over observed
  metrics["BS"] <- BS(preds_matrix, y_test_matrix[,colnames(y_test_matrix)])
  metrics["BSS"] <- 1 - BSnorm(preds_matrix, y_test_matrix[,colnames(y_test_matrix)])
  metrics["logloss"] <- log_loss(y_true_0_1, preds)
  
  if (full_calib){
    slope_intercept <- cox_first_degree(y_true_0_1, preds)
    metrics["slope"] <- slope_intercept$slope
    metrics["intercept"] <- slope_intercept$intercept
  }
  
  # ECE
  if (eval_ECE){
    metrics["ECE_CalibratR"] <- CalibratR::getECE(y_true_0_1, preds)
    metrics["ECE_Shan"] <- ECE(y_true_0_1, preds, n_bins = 10)
  }
  
  # ECI
  if (eval_ECI){
    get_ECI <- function(y_true_0_1, preds){
      pmcalib_curve <- pmcalibration::pmcalibration(y_true_0_1, preds, 
                                                    smooth = "rcs", nk = 5, ci = "pw")
      metrics["ECI_pmcal_rcs"] <- pmcalib_curve$metrics["ECI"]
    }
    metrics["ECI_pmcal_rcs"] <- NA_real_
    try(
      metrics["ECI_pmcal_rcs"] <- get_ECI(y_true_0_1, preds)
    )
    
    get_ECI_Shan_loess <- function(y_true_0_1, preds){
      pmcalib_curve <- ECI(y_true_0_1, preds)
      metrics["ECI_Shan_loess"] <- pmcalib_curve
    }
    metrics["ECI_Shan_loess"] <- NA_real_
    try(
      metrics["ECI_Shan_loess"] <- get_ECI_Shan_loess(y_true_0_1, preds)
    )
    
  }
  
  # AUPRC
  curves <- precrec::evalmod(scores = preds, labels = y_true, posclass = positive_class)
  aucs <- precrec::auc(curves)
  AUPRC <- aucs[aucs$curvetypes == "PRC", "aucs"]
  metrics["AUPRC"] <- AUPRC
  
  # Metrics at fixed Sensitivity
  # ----------------------------
  
  # Specificity at 80% Sensitivity
  mod_pred <- ROCR::prediction(preds, y_true, label.ordering = c("no_CLABSI", "CLABSI"))
  mod_perf <- ROCR::performance(mod_pred,"tpr", "fpr")
  
  se_sp_df <- tibble(cutoff = mod_perf@alpha.values[[1]],
                     sensitivity = mod_perf@y.values[[1]],
                     specificity = 1 - mod_perf@x.values[[1]]) 
  
  se_sp_df <- se_sp_df[se_sp_df$sensitivity != 1,]
  
  ind_se <- which.min(abs(se_sp_df$sensitivity - 0.8))
  Spec_at_Sens_80 <- se_sp_df[ind_se,]$specificity
  Sens_at_Sens_80 <- se_sp_df[ind_se,]$sensitivity
  Cutoff_for_Sens_80 <- se_sp_df[ind_se,]$cutoff
  metrics["Sens_at_Sens_80"] <- Sens_at_Sens_80
  metrics["Spec_at_Sens_80"] <- Spec_at_Sens_80
  metrics["Cutoff_for_Sens_80"] <- Cutoff_for_Sens_80
  
  # Specificity at 50% Sensitivity
  ind_se <- which.min(abs(se_sp_df$sensitivity - 0.5))
  Spec_at_Sens_50 <- se_sp_df[ind_se,]$specificity
  Sens_at_Sens_50 <- se_sp_df[ind_se,]$sensitivity
  Cutoff_for_Sens_50 <- se_sp_df[ind_se,]$cutoff
  metrics["Sens_at_Sens_50"] <- Sens_at_Sens_50
  metrics["Spec_at_Sens_50"] <- Spec_at_Sens_50
  metrics["Cutoff_for_Sens_50"] <- Cutoff_for_Sens_50
  
  # Confusion matrix metrics at 80% Sensitivity
  N <- length(y_true)
  #N_pos <- sum(y_true_0_1)
  class_preds_80 <- ifelse(preds >= Cutoff_for_Sens_80, 1, 0) 
  nr_alerts_80 <- sum(class_preds_80)
  nr_predicted_negative_80 <- N - nr_alerts_80
  TP_80 <- sum((preds >= Cutoff_for_Sens_80) & (y_true_0_1 == 1))
  FP_80 <- sum((preds >= Cutoff_for_Sens_80) & (y_true_0_1 == 0))
  TN_80 <- sum((preds < Cutoff_for_Sens_80) & (y_true_0_1 == 0))
  
  # Confusion matrix metrics at 50% Sensitivity
  class_preds_50 <- ifelse(preds >= Cutoff_for_Sens_50, 1, 0) 
  nr_alerts_50 <- sum(class_preds_50)
  nr_predicted_negative_50 <- N - nr_alerts_50
  TP_50 <- sum((preds >= Cutoff_for_Sens_50) & (y_true_0_1 == 1))
  FP_50 <- sum((preds >= Cutoff_for_Sens_50) & (y_true_0_1 == 0))
  TN_50 <- sum((preds < Cutoff_for_Sens_50) & (y_true_0_1 == 0))
  
  # Alert rate at 80% Sensitivity
  Alert_rate_at_Sens_80 <- nr_alerts_80/N
  metrics["Alert_rate_at_Sens_80"] <- Alert_rate_at_Sens_80
  
  # Alert rate at 50% Sensitivity
  Alert_rate_at_Sens_50 <- nr_alerts_50/N
  metrics["Alert_rate_at_Sens_50"] <- Alert_rate_at_Sens_50
  
  # PPV at 80% Sensitivity
  PPV_at_Sens_80 <- TP_80/nr_alerts_80
  metrics["PPV_at_Sens_80"] <- PPV_at_Sens_80
  
  # NPV at 80% Sensitivity
  NPV_at_Sens_80 <- TN_80/nr_predicted_negative_80
  metrics["NPV_at_Sens_80"] <- NPV_at_Sens_80
  
  # PPV at 50% Sensitivity
  PPV_at_Sens_50 <- TP_50/nr_alerts_50
  metrics["PPV_at_Sens_50"] <- PPV_at_Sens_50
  
  # NPV at 50% Sensitivity
  NPV_at_Sens_50 <- TN_50/nr_predicted_negative_50
  metrics["NPV_at_Sens_50"] <- NPV_at_Sens_50
  
  # NB at 80% Sensitivity
  NB_at_Sens_80 <- TP_80/N - (FP_80/N) * (Cutoff_for_Sens_80/(1-Cutoff_for_Sens_80))
  metrics["NB_at_Sens_80"] <- NB_at_Sens_80
  
  # NB at 50% Sensitivity
  NB_at_Sens_50 <- TP_50/N - (FP_50/N) * (Cutoff_for_Sens_50/(1-Cutoff_for_Sens_50))
  metrics["NB_at_Sens_50"] <- NB_at_Sens_50
  
  # Metrics at fixed thresholds
  # ---------------------------
  
  for (i in 1:length(thresholds)){
    
    t <- thresholds[i]
    
    N <- length(y_true)
    N_pos <- sum(y_true_0_1)
    N_neg <- N - N_pos 
    
    # Confusion matrix metrics t
    class_preds_t <- ifelse(preds >= t, 1, 0) 
    nr_alerts_t <- sum(class_preds_t)
    nr_predicted_negative_t <- N - nr_alerts_t
    TP_t <- sum((preds >= t) & (y_true_0_1 == 1))
    FP_t <- sum((preds >= t) & (y_true_0_1 == 0))
    TN_t <- sum((preds < t) & (y_true_0_1 == 0))
    
    # Sensitivity at t
    Sensitvity_at_t <- TP_t/N_pos
    metrics[paste0("Sens_at_thresh_", i)] <- Sensitvity_at_t
    
    # Specificity at t
    Specificity_at_t <- TN_t/N_neg
    metrics[paste0("Spec_at_thresh_", i)] <- Specificity_at_t
    
    # Alert rate at t
    Alert_rate_at_t <- nr_alerts_t/N
    metrics[paste0("Alert_rate_at_thresh_", i)] <- Alert_rate_at_t
    
    # PPV at t
    PPV_at_t <- TP_t/nr_alerts_t
    metrics[paste0("PPV_at_thresh_", i)] <- PPV_at_t
    
    # NPV at t
    NPV_at_t <- TN_t/nr_predicted_negative_t
    metrics[paste0("NPV_at_thresh_", i)] <- NPV_at_t
    
    # NB at t
    NB_at_t <- TP_t/N - (FP_t/N) * (t/(1-t))
    metrics[paste0("NB_at_thresh_", i)] <- NB_at_t
    
  }
  
  return(metrics)
  
}

BS <- function (probabilities, y) {
  mean(rowSums((probabilities - y)^2))
}

#' Normalized Brier Score
#'
#' @param probabilities matrix of predicted probabilities for each class (classes in columns, observations in rows)
#' @param y matrix of true values for y; each column contains a class ordered in the same order as probabilities
#'
#' @return Normalized Brier Score
#' @keywords internal
#' @noRd

BSnorm <- function (probabilities, y) {
  # reference is a "no skill learner" that predicts the class prevalence
  BS_reference <- BS(matrix(rep(colMeans(y), nrow(y)), nrow = nrow(y), byrow = TRUE), y)
  if (BS_reference == 0){ # avoid division by 0
    return(0)
  } else {
    return(BS(probabilities, y) / BS_reference)
  }
}

# evaluate all metrics per using the predictions and LM_data
eval_base_ward <- function(predictions, LM_data, 
                           positive_class = "CLABSI"){
  
  # evaluate per physical ward
  preds_ward <- predictions %>% 
    left_join(LM_data, by = join_by(functioneelDossierNr, CAT_catheter_episode)) 
  
  events_per_ward_PM <- preds_ward %>% 
    group_by(test_set, MS_physical_ward) %>% 
    count(y_true_cat) %>% 
    mutate(p = n/sum(n)) %>% 
    ungroup() 
  
  events_per_ward_PM_rank <- events_per_ward_PM %>% 
    group_by(MS_physical_ward, y_true_cat) %>% 
    summarise(median_n = median(n)) %>% 
    ungroup() %>% 
    filter(y_true_cat == positive_class) %>% 
    arrange(desc(median_n)) %>% 
    mutate(rank = str_pad(row_number(), 2, side = "left", pad = "0"),
           type = paste(rank, MS_physical_ward, sep = "_")) %>% 
    select(MS_physical_ward, type)
  
  events_per_ward_PM <- events_per_ward_PM %>% 
    left_join(events_per_ward_PM_rank, by = join_by(MS_physical_ward))
  
  events_per_ward_MS <- preds_ward %>% 
    group_by(test_set, MS_medical_specialty) %>% 
    count(y_true_cat) %>% 
    mutate(p = n/sum(n)) %>% 
    ungroup() 
  
  events_per_ward_MS_rank <- events_per_ward_MS %>% 
    group_by(MS_medical_specialty, y_true_cat) %>% 
    summarise(median_n = median(n)) %>% 
    ungroup() %>% 
    filter(y_true_cat == positive_class) %>% 
    arrange(desc(median_n)) %>% 
    mutate(rank = str_pad(row_number(), 2, side = "left", pad = "0"),
           type = paste(rank, MS_medical_specialty, sep = "_")) %>% 
    select(MS_medical_specialty, type)
  
  events_per_ward_MS <- events_per_ward_MS %>% 
    left_join(events_per_ward_MS_rank, by = join_by(MS_medical_specialty))
  
  eval_metrics_PW <- preds_ward %>%
    group_by(model, train_set, test_set, MS_physical_ward) %>%
    mutate(eval = list(evaluate_model(preds, y_true_cat, positive_class = "CLABSI",
                                      eval_ECI = FALSE, eval_ECE = FALSE) %>%
                         t() %>% as_tibble())) %>%
    ungroup() %>%
    distinct(model, train_set, test_set, eval, MS_physical_ward) %>%
    unnest(eval) %>% 
    filter(!is.na(AUROC)) # wards with less than 5 events (5 is ok)
  
  eval_metrics_PW <- eval_metrics_PW %>% 
    left_join(events_per_ward_PM_rank, by = join_by(MS_physical_ward))
  
  eval_metrics_MS <- preds_ward %>%
    group_by(model, train_set, test_set, MS_medical_specialty) %>%
    mutate(eval = list(evaluate_model(preds, y_true_cat, positive_class = "CLABSI",
                                      eval_ECI = FALSE, eval_ECE = FALSE) %>%
                         t() %>% as_tibble())) %>%
    ungroup() %>%
    distinct(model, train_set, test_set, eval, MS_medical_specialty) %>%
    unnest(eval) %>% 
    filter(!is.na(AUROC)) # wards with less than 5 events (5 is ok)
  
  eval_metrics_MS <- eval_metrics_MS %>% 
    left_join(events_per_ward_MS_rank, by = join_by(MS_medical_specialty))
  
  # prettify and return
  eval_metrics_PW <- eval_metrics_PW %>% 
    rename(subgroup = type) %>% 
    add_column(type = "MS_physical_ward") %>% 
    select(-MS_physical_ward) 
  
  eval_metrics_MS <- eval_metrics_MS %>% 
    rename(subgroup = type) %>% 
    add_column(type = "MS_medical_specialty") %>% 
    select(-MS_medical_specialty)
  
  eval_metrics_ward <- eval_metrics_PW %>% 
    add_row(eval_metrics_MS)
  
  return(eval_metrics_ward)
  
}
