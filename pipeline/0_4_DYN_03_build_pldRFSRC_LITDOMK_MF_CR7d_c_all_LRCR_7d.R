# source all files in the R directory
files_to_source <- list.files("R/", recursive = TRUE, full.names = TRUE)
invisible(lapply(files_to_source, function(x) source(x, chdir = TRUE)))

library(randomForestSRC)
library(mlrMBO)
library(ParamHelpers)
library(DiceKriging)

# config for this model
# config for this model; follows the naming convention: 
# [BASE/DYN]_[model type]_[outcome type]_[horizon]_[imputation]_[var sel]_[other...]
# model name should be unique across files!!!
model <- "DYN_pldRFSRC_LITDOMK_MF_CR7d_LRCR_c_all_7d"
print(model)

# data path
path_data_complete <- data_path_play_dyn_complete_MF
model_type <- "CR7d_LRCR_c_all"
var_sel <- "LITDOMK"
horizon <- 7
tune <- TRUE
iters_design <- 20
iters_optim <- 30
tuning_metric <- "logloss"
importance_type <- "none"
# CR specific params
adm_cens <- 7
ntime <- 1:7
cause <- c(1, 1, 1)
splitrule <- "logrankCR"

build_RFSRC_DYN(model, 
                path_data_complete,
                model_type = model_type,
                var_sel = var_sel,
                horizon = horizon,
                tune = tune,
                iters_design = iters_design,
                iters_optim = iters_optim,
                tuning_metric = tuning_metric, 
                importance_type = importance_type,
                # CR specific params
                adm_cens = adm_cens,
                ntime = ntime, 
                cause = cause,
                splitrule = splitrule)

# # evaluate & save results
# preds_name <- paste("preds", model, sep = "_")
# preds_file <- paste0(preds_dir_dyn, preds_name)
# results_name <- paste("results", model, sep = "_")
# results_file <- paste0(preds_dir_dyn, results_name)
# OOB_results_name <- paste("OOB_results", model, sep = "_")
# OOB_results_file <- paste0(preds_dir_dyn, OOB_results_name)
# OOB_preds_name <- paste("OOB_preds", model, sep = "_")
# OOB_preds_file <- paste0(preds_dir_dyn, OOB_preds_name)
# 
# load(OOB_preds_file)
# eval_metrics_OOB <- OOB_predictions %>%
#   group_by(model, train_set, test_set) %>%
#   summarise(eval = list(evaluate_model(preds, y_true_cat, positive_class = "CLABSI",
#                                     full_calib = TRUE, eval_ECI = FALSE, eval_ECE = TRUE) %>%
#                        t() %>% as_tibble())) %>%
#   ungroup() %>%
#   distinct(model, train_set, test_set, eval) %>%
#   unnest(eval) %>%
#   add_column(type = "all",
#              subgroup = NA_character_,
#              LM = NA_real_)
# save(eval_metrics_OOB, file = OOB_results_file)
# 
# load(preds_file)
# eval_metrics <- predictions %>%
#   group_by(model, train_set, test_set) %>%
#   summarise(eval = list(evaluate_model(preds, y_true_cat, positive_class = "CLABSI",
#                                     full_calib = TRUE) %>%
#                        t() %>% as_tibble())) %>%
#   ungroup() %>%
#   distinct(model, train_set, test_set, eval) %>%
#   unnest(eval) %>%
#   add_column(type = "all",
#              subgroup = NA_character_,
#              LM = NA_real_)
# 
# # read LM data
# con <- db_connect("R/db_config.R")
# docu_path <- "docu/docu_df.xlsx"
# docu_df <- docu_path %>%
#   excel_sheets() %>%
#   set_names() %>%
#   map(read_excel, path = docu_path) %>%
#   `[[`("Landmark dataframe")
# 
# LM_data <- read_from_DB(con, "LM_data_12_13", docu_df) %>% as_tibble()
# LM_data <- LM_data %>%
#   select(functioneelDossierNr, CAT_catheter_episode, LM,
#          MS_medical_specialty, MS_physical_ward, LM_end_time)
# 
# # dynamic eval per LM
# eval_metrics_t <- predictions %>%
#   filter(LM <= max_LM_eval) %>%
#   group_by(model, train_set, test_set, LM) %>%
#   summarise(eval = list(evaluate_model(preds, y_true_cat, positive_class = "CLABSI",
#                                     full_calib = TRUE) %>%
#                        t() %>% as_tibble())) %>%
#   ungroup() %>%
#   distinct(model, train_set, test_set, LM, eval) %>%
#   unnest(eval) %>%
#   add_column(type = "per_LM",
#              subgroup = NA_character_)
# 
# results <- eval_metrics %>%
#   add_row(eval_metrics_t)
# 
# cols_wide <- colnames(results)
# cols_wide <- cols_wide[!cols_wide %in% c("model", "train_set", "test_set",
#                                          "type","subgroup", "LM")]
# results <- results %>%
#   pivot_longer(cols = all_of(cols_wide), names_to = "metric",
#                values_to = "value")
# 
# save(results, file = results_file)
# 
# # baseline eval
# eval_metrics_base <- eval_metrics_t %>% filter(LM == 0) %>%
#   mutate(type = "all")
# 
# predictions_base <- predictions %>% filter(LM == 0)
# LM_data_base <- LM_data %>% filter(LM == 0)
# 
# eval_metrics_ward <- eval_base_ward(predictions_base, LM_data_base)
# 
# results_base <- eval_metrics_base %>%
#   add_row(eval_metrics_ward)
# 
# cols_wide <- colnames(results_base)
# cols_wide <- cols_wide[!cols_wide %in% c("model", "train_set", "test_set",
#                                          "type","subgroup", "LM")]
# results_base <- results_base %>%
#   pivot_longer(cols = all_of(cols_wide), names_to = "metric",
#                values_to = "value")
# results <- results_base # rename for saving withe the same name as others
# 
# results_file_base <- paste0(preds_dir_base, results_name)
# save(results, file = results_file_base)
