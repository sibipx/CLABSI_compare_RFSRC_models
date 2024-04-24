# source all files in the R directory
files_to_source <- list.files("R/", recursive = TRUE, full.names = TRUE)
invisible(lapply(files_to_source, function(x) source(x, chdir = TRUE)))

# data path
path_data_complete <- data_path_play_base_complete_MF

# get filenames for imputed datasets 
datasets_files <- list.files(path_data_complete, 
                             recursive = TRUE, full.names = TRUE)
train_files <- datasets_files[str_detect(datasets_files, "train")]

num_trees <- 1000

sample_adm_ids_boot <- function(adm_ids, adm_ids_unique, n) {
  # Generate a bootstrap sample
  s <- sample(adm_ids_unique, n, replace = TRUE)
  
  # Count occurrences of each unique admission ID in the bootstrap sample
  s_count <- table(s)
  
  # Map the counts back to the original admission IDs
  s_count_adm_ids <- s_count[match(adm_ids, names(s_count))]
  s_count_adm_ids[is.na(s_count_adm_ids)] <- 0
  
  names(s_count_adm_ids) <- NULL
  
  
  return(s_count_adm_ids)
}

set.seed(2023)

# create bootstraps for each df 
for (f in train_files){
  
  print(f)
  start_time <- Sys.time()
  
  # load data
  load(f) # loads train data named data_train
  
  # do the same filtering as in model building 
  data_train <- data_train %>% 
    filter(eventtime > LM) 
  
  # create inbags
  adm_ids <- data_train$functioneelDossierNr
  adm_ids_unique <- unique(adm_ids)
  n_id <- length(adm_ids_unique)
  
  inbags <- replicate(num_trees, sample_adm_ids_boot(adm_ids, adm_ids_unique, n_id))
  
  # save inbags
  boot_file_name <- get_inbags_filename_boot(boot_file_name_prefix, f)
  save(inbags, file = boot_file_name)
  
  message(sprintf("DONE in %s minutes.", 
                  difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
}


