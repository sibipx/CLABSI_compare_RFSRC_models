# source all files in the R directory
files_to_source <- list.files("R/", recursive = TRUE, full.names = TRUE)
invisible(lapply(files_to_source, function(x) source(x, chdir = TRUE)))

subsample_inbag <- function(inbag, subsample_size, len_inbags) {
  ids <- which(inbag == 1)
  ids_selected <- sort(sample(ids, subsample_size, replace = FALSE))
  inbag_selected <- rep(0, len_inbags)
  inbag_selected[ids_selected] <- 1
  
  return(inbag_selected)
}

# data path
path_data_complete <- data_path_play_dyn_complete_MF

datasets_files <- list.files(path_data_complete, 
                             recursive = TRUE, full.names = TRUE)
train_files <- datasets_files[str_detect(datasets_files, "train")]

for (f in train_files){
  
  print(f)
  start_time <- Sys.time()
  
  for (sample_fraction in 30:80){
    
    inbag_file <- get_inbags_filename(subsamp_file_name_prefix_LM_30, f, sample_fraction)
    load(inbag_file)
    
    n_inbag <- colSums(inbags)
    min_n_inbag <- min(n_inbag)
    
    len_inbags <- dim(inbags)[[1]]
    
    # adjust inbag to min size of all bags
    inbags <- apply(inbags, 2, function(x) subsample_inbag(x, min_n_inbag, len_inbags))
    
    subsamp_file_name <- get_inbags_filename(subsamp_file_name_prefix_LM_30_minsize, f, sample_fraction)
    save(inbags, file = subsamp_file_name)
  }
  
  message(sprintf("DONE in %s minutes.", 
                  difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
}