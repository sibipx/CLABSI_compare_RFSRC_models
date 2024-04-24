# libraries
library(glmnet)
library(doParallel)

registerDoParallel(detectCores())

# common config
# -------------

N_iter <- 100

frac_test <- 1/3

# models_path
# -----------

models_path <- "models/"
var_sel_path <- "playground/variable_selection/selection_by_reviewers/"

# BASELINE config
# ---------------

data_path_play_base_miss <- "data_for_models/2012_2013/missing/BASE/"
data_path_play_base_complete_MF <- "data_for_models/2012_2013/imputed/BASE/MF/"
data_path_play_base_complete_mm <- "data_for_models/2012_2013/imputed/BASE/mean_mode/"

cat_cols_base <- c("MS_medical_specialty", 
                   "GEN_LM_day_categ", 
                   "GEN_LM_month_categ", 
                   "GEN_LM_season_categ")

var_imp_dir_base <- "variable_importance/2012_2013/BASE/"
preds_dir_base <- "predictions/2012_2013/BASE/"

# bootsrap dir for trees in random forests
inbag_boot_dir <- "data_for_models/2012_2013/imputed/BASE/train_boot_by_adm_id_1000/"
boot_file_name_prefix <- paste0(inbag_boot_dir, "inbag_boot_train")

inbag_boot_dir_minsize <- "data_for_models/2012_2013/imputed/BASE/train_boot_by_adm_id_1000_minsize/"
boot_file_name_prefix_minsize  <- paste0(inbag_boot_dir_minsize, "inbag_boot_train")

get_inbags_filename_boot <- function(file_name_prefix, f){ # f = train file name
  train_id <- str_sub(f, -3, -1)
  paste(file_name_prefix, train_id, sep = "_")
}

vars_selected_DOMK_LIT <- c("CAT_catheter_type_binary_all_CVC", 
                            "CAT_catheter_type_binary_all_Port_a_cath",
                            "CAT_catheter_type_binary_all_Tunneled_CVC", 
                            "CAT_catheter_type_binary_all_PICC", 
                            "CAT_catheter_location_binary_all_Collarbone",
                            "CAT_catheter_location_binary_all_Neck",
                            "MED_7d_TPN", 
                            "MED_L2_7d_J01_ANTIBACTERIALS_FOR_SYSTEMIC_USE", 
                            "MED_L2_7d_L01_ANTINEOPLASTIC_AGENTS", 
                            "CLABSI_history",
                            "COM_PATH_tumor_before_LM",
                            "CARE_VS_temperature_max",
                            "CARE_VS_systolic_BP_last",
                            "LAB_WBC_count_last",
                            "COM_lymphoma_before_LM",
                            "COM_PATH_transplant_before_LM",
                            "MB_other_infection_than_BSI_during_window",
                            "LAB_CRP_last",
                            "ADM_admission_source_binary_all_Home",
                            "CARE_VS_MV",
                            "MS_is_ICU_unit")


# DYNAMIC config
# --------------
data_path_play_dyn_miss <- "data_for_models/2012_2013/missing/DYN/"
data_path_play_dyn_complete_MF <- "data_for_models/2012_2013/imputed/DYN/MF/"
data_path_play_dyn_complete_LOCF <- "data_for_models/2012_2013/imputed/DYN/LOCF/"

var_imp_dir_dyn <- "variable_importance/2012_2013/DYN/"
preds_dir_dyn <- "predictions/2012_2013/DYN/"

# subsamples dir for trees in random forests
inbag_subsamp_dir <- "data_for_models/2012_2013/imputed/DYN/subsamples_by_adm_id_1000/"
subsamp_file_name_prefix <- paste0(inbag_subsamp_dir, "inbag_subsamp_train")

inbag_subsamp_dir_LM_30 <- "data_for_models/2012_2013/imputed/DYN/train_LM_30_subsamples_by_adm_id_1000/"
subsamp_file_name_prefix_LM_30 <- paste0(inbag_subsamp_dir_LM_30, "inbag_subsamp_train")

inbag_subsamp_dir_LM_30_minsize <- "data_for_models/2012_2013/imputed/DYN/train_LM_30_subsamples_by_adm_id_1000_minsize/"
subsamp_file_name_prefix_LM_30_minsize  <- paste0(inbag_subsamp_dir_LM_30_minsize, "inbag_subsamp_train")

get_inbags_filename <- function(subsamp_file_name_prefix, f, subsample_size){ # f = train file name
  train_id <- str_sub(f, -3, -1)
  paste(subsamp_file_name_prefix, train_id, 
        str_pad(subsample_size, 3, pad = "0"), sep = "_")
}

# configure columns for which we don't allow missingess
# (on training it will probably be different)
cols_not_missing_playground <- c("functioneelDossierNr",                                         
                                 "CAT_catheter_episode",                                         
                                 "LM",                                                         
                                 "CAT_catheter_type_binary_all_Tunneled_CVC",                    
                                 "CAT_catheter_type_binary_all_Port_a_cath",                     
                                 "CAT_catheter_type_binary_all_CVC",                             
                                 "CAT_catheter_type_binary_all_PICC",                            
                                 "CAT_catheter_type_binary_all_Dialysis_CVC",                    
                                 "CAT_catheter_location_binary_all_Collarbone",                  
                                 "CAT_catheter_location_binary_all_Neck",                        
                                 "CAT_catheter_location_binary_all_Arm",                        
                                 "CAT_catheter_location_binary_all_Groin",                       
                                 "CAT_catheter_location_binary_all_Other",                       
                                 "CAT_catheter_placement_binary_all_OR",                         
                                 "CAT_catheter_placement_binary_all_bedside",                    
                                 "CAT_number_central_lines",                                     
                                 "CAT_number_peripheral_catheters",                              
                                 "CAT_total_episode_days_CVC",                                   
                                 "CAT_consecutive_current_days_CVC",                             
                                 "CAT_total_episode_days_Tunneled_CVC",                          
                                 "CAT_consecutive_current_days_Tunneled_CVC",                    
                                 "CAT_total_episode_days_Port_a_cath",                           
                                 "CAT_consecutive_current_days_Port_a_cath",                     
                                 "CAT_total_episode_days_PICC",                                  
                                 "CAT_consecutive_current_days_PICC",                            
                                 "CAT_total_episode_days_Dialysis_CVC",                          
                                 "CAT_consecutive_current_days_Dialysis_CVC",                    
                                 "CAT_start_episode_hour_num",                                   
                                 "eventtime",                                                    
                                 "type",                                                         
                                 "CLABSI_history",                                               
                                 "CAT_nr_bandage_obersations",                                   
                                 "CAT_days_since_last_bandage_obs",                              
                                 "CAT_bandage_change",                                           
                                 "CAT_days_since_last_bandage_changed",                          
                                 "CAT_tube_change",                                              
                                 "CAT_days_since_last_tube_change",                              
                                 "MS_is_ICU_unit",                                               
                                 "MS_ICU_time_before_catheter",                                  
                                 "MS_ICU_time_before_LM",                                        
                                 "MS_net_OR_time_before_catheter",                               
                                 "MS_is_24h_OR_unit",                                            
                                 "MS_net_OR_time_before_LM",                                     
                                 "MS_is_24h_net_OR_unit",                                        
                                 "MS_endoscopy",                                                 
                                 "MS_IRCC",                                                      
                                 "MED_3d_TPN",                                                   
                                 "MED_L2_3d_L01_ANTINEOPLASTIC_AGENTS",                          
                                 "MED_L2_3d_J02_ANTIMYCOTICS_FOR_SYSTEMIC_USE",                  
                                 "MED_L2_3d_J01_ANTIBACTERIALS_FOR_SYSTEMIC_USE",                
                                 "MED_L2_3d_B01_ANTITHROMBOTIC_AGENTS",                          
                                 "MED_L2_3d_H02_CORTICOSTEROIDS_FOR_SYSTEMIC_USE",               
                                 "MED_L2_3d_L04_IMMUNOSUPPRESSANTS",                             
                                 "MED_L2_3d_J05_ANTIVIRALS_FOR_SYSTEMIC_USE",                    
                                 "MED_L2_3d_J04_ANTIMYCOBACTERIALS",                             
                                 "MED_L2_3d_D03_PREPARATIONS_FOR_TREATMENT_OF_WOUNDS_AND_ULCERS",
                                 "MED_L2_3d_J06_IMMUNE_SERA_AND_IMMUNOGLOBULINS",                
                                 "MED_L5_3d_B05BA03_carbohydrates",                              
                                 "MED_L5_3d_B05BA10_combinations",                               
                                 "MED_L5_3d_B05BA01_amino_acids",                                
                                 "MED_L5_3d_B05BA02_fat_emulsions",                              
                                 "MED_3d_VANCO_CEFTA_LOCK",                                      
                                 "MED_3d_CITRA_LOCK",                                            
                                 "MED_3d_immunoglobulins",                                       
                                 "MED_3d_number_of_IV_med",                                      
                                 "MED_3d_number_of_ORAL_med",                                    
                                 "MED_7d_TPN",                                                   
                                 "MED_L2_7d_L01_ANTINEOPLASTIC_AGENTS",                          
                                 "MED_L2_7d_J02_ANTIMYCOTICS_FOR_SYSTEMIC_USE",                  
                                 "MED_L2_7d_J01_ANTIBACTERIALS_FOR_SYSTEMIC_USE",                
                                 "MED_L2_7d_B01_ANTITHROMBOTIC_AGENTS",                          
                                 "MED_L2_7d_H02_CORTICOSTEROIDS_FOR_SYSTEMIC_USE",               
                                 "MED_L2_7d_L04_IMMUNOSUPPRESSANTS",                             
                                 "MED_L2_7d_J05_ANTIVIRALS_FOR_SYSTEMIC_USE",                    
                                 "MED_L2_7d_J04_ANTIMYCOBACTERIALS",                             
                                 "MED_L2_7d_D03_PREPARATIONS_FOR_TREATMENT_OF_WOUNDS_AND_ULCERS",
                                 "MED_L2_7d_J06_IMMUNE_SERA_AND_IMMUNOGLOBULINS",                
                                 "MED_L5_7d_B05BA03_carbohydrates",                              
                                 "MED_L5_7d_B05BA10_combinations",                               
                                 "MED_L5_7d_B05BA01_amino_acids",                                
                                 "MED_L5_7d_B05BA02_fat_emulsions",                              
                                 "MED_7d_VANCO_CEFTA_LOCK",                                      
                                 "MED_7d_CITRA_LOCK",                                            
                                 "MED_7d_immunoglobulins",                                       
                                 "MED_7d_number_of_IV_med",                                      
                                 "MED_7d_number_of_ORAL_med",                                    
                                 "PAT_age",                                                      
                                 "PAT_gender_M",                                                 
                                 "ADM_unplanned_readmission",                                    
                                 "ADM_nr_emergency_adm_past_180_days",                           
                                 "ADM_nr_adm_past_180_days",                                     
                                 "ADM_admission_to_catheter",                                    
                                 "COM_PATH_tumor_before_LM",                                     
                                 "COM_PATH_multiple_pathologies_before_LM",                      
                                 "COM_PATH_inflammation_infection_immunology_before_LM",         
                                 "COM_PATH_functional_before_LM",                                
                                 "COM_PATH_congenital_genetic_before_LM",                        
                                 "COM_PATH_vascular_before_LM",                                  
                                 "COM_PATH_degeneration_before_LM",                              
                                 "COM_PATH_trauma_toxicology_before_LM",                         
                                 "COM_PATH_rehabilitation_before_LM",                            
                                 "COM_PATH_transplant_before_LM",                                
                                 "COM_PATH_psychiatry_before_LM",                                
                                 "COM_ORG_kidney_and_urinary_tract_before_LM",                   
                                 "COM_ORG_skin_and_breast_before_LM",                            
                                 "COM_ORG_multiple_organs_before_LM",                            
                                 "COM_ORG_liver_bile_and_pancreas_before_LM",                    
                                 "COM_ORG_nervous_system_or_psychiatric_before_LM",              
                                 "COM_ORG_musculoskeletal_system_before_LM",                     
                                 "COM_ORG_respiratory_system_before_LM",                         
                                 "COM_ORG_intestinal_system_before_LM",                          
                                 "COM_ORG_reproductive_system_before_LM",                        
                                 "COM_ORG_otorhinolaryngology_before_LM",                        
                                 "COM_ORG_heart_and_circulatory_system_before_LM",               
                                 "COM_ORG_lymphatic_system_before_LM",                           
                                 "COM_ORG_eyes_before_LM",                                       
                                 "COM_ORG_hormonal_system_before_LM",                            
                                 "COM_ORG_neurology_before_LM",                                  
                                 "COM_valvular_disease_before_LM",                               
                                 "COM_lymphoma_before_LM",                                       
                                 "COM_chronic_kidney_disease_before_LM",                         
                                 "COM_arterial_occlusive_disease_before_LM",                     
                                 "COM_cirrhosis_before_LM",                                      
                                 "COM_thyroid_adenoma_before_LM",                                
                                 "COM_diabetes_before_LM",                                       
                                 "COM_HIV_before_LM",                                            
                                 "COM_COPD_before_LM",                                           
                                 "COM_renal_failure_before_LM",                                  
                                 "COM_heart_failure_before_LM",                                  
                                 "BLD_admin_binary_all_PLATELETS",                               
                                 "BLD_admin_binary_all_PLASMA",                                  
                                 "BLD_admin_binary_all_RBC_CONCENTRATE",                         
                                 "MB_infection_binary_all_lung",                                 
                                 "MB_infection_binary_all_skin",                                 
                                 "MB_infection_binary_all_catheter",                             
                                 "MB_infection_binary_all_urogen",                               
                                 "MB_infection_binary_all_sputum",                               
                                 "MB_infection_binary_all_blood",                                
                                 "MB_infection_binary_all_deep_tissue",                          
                                 "MB_infection_binary_all_drain",                                
                                 "MB_infection_binary_all_GI",                                   
                                 "MB_other_infection_than_BSI",                                  
                                 "MB_infection_time_window_binary_all_lung",                     
                                 "MB_infection_time_window_binary_all_drain",                    
                                 "MB_infection_time_window_binary_all_skin",                     
                                 "MB_infection_time_window_binary_all_sputum",                   
                                 "MB_infection_time_window_binary_all_catheter",                 
                                 "MB_infection_time_window_binary_all_blood",                    
                                 "MB_infection_time_window_binary_all_urogen",                   
                                 "MB_infection_time_window_binary_all_deep_tissue",              
                                 "MB_infection_time_window_binary_all_GI",                       
                                 "MB_other_infection_than_BSI_during_window",                    
                                 "MB_number_contaminants_in_blood",                              
                                 "GEN_LM_day_categ",                                             
                                 "GEN_LM_month_categ",                                           
                                 "GEN_LM_season_categ",                                          
                                 "HC_2617_Adverse_effects_of_medical_drugs",                     
                                 "HC_42_Secondary_malignancies",                                 
                                 "HC_55_Fluid_and_electrolyte_disorders",                        
                                 "HC_59_Deficiency_and_other_anemia",                            
                                 "HC_98_Essential_hypertension",                                 
                                 "HC_155_Other_gastrointestinal_disorders",                      
                                 "HC_158_Chronic_kidney_disease",                                
                                 "HC_238_Complications_of_surgical_procedures_or_medical_care",  
                                 "HC_53_Disorders_of_lipid_metabolism",                          
                                 "HC_58_Other_nutritional_endocrine_and_metabolic_disorders",    
                                 "HC_101_Coronary_atherosclerosis_and_other_heart_disease",      
                                 "HC_253_Allergic_reactions",                                    
                                 "HC_117_Other_circulatory_disease",                             
                                 "HC_96_Heart_valve_disorders",                                  
                                 "HC_106_Cardiac_dysrhythmias",                                  
                                 "HC_3_Bacterial_infection_unspecified_site",                    
                                 "HC_45_Maintenance_chemotherapy_radiotherapy",                  
                                 "CARE_VS_CVP_measured",                                         
                                 "CARE_VS_MV",                                                   
                                 "CARE_VS_breathing_aid",                                        
                                 "CARE_SAF_delirium_binary_last_YES",                            
                                 "CARE_ISO_binary_all_protective_isolation",                     
                                 "CARE_ISO_binary_all_source_isolation",                         
                                 "CARE_EXC_faecal_incontinence",                                 
                                 "CARE_EXC_urinary_incontinence",                                
                                 "CARE_SYM_pruritus_binary_all_YES",                             
                                 "CARE_PHY_flap",                                                
                                 "CARE_PHY_drain",                                               
                                 "CARE_ICU_ECMO",                                                
                                 "CARE_DIA_binary_all_hemodialysis",                             
                                 "CARE_PHY_weight_MVI",                                          
                                 "CARE_PHY_length_MVI",                                          
                                 "CAT_lumens_total")

cols_do_not_impute_MF_playground <- c("MS_medical_specialty", # less than 3% missing
                                      "MS_alternative_flag", # less than 3% missing
                                      "ADM_admission_referral_binary_all_GP", # less than 3% missing
                                      "ADM_admission_source_binary_all_Home", # less than 3% missing
                                      "ADM_admission_reason_binary_all_Accident", # less than 3% missing
                                      "ADM_admission_type_binary_all_Emergency") # less than 3% missing

# model evaluation
# ----------------

thresholds <- c(1/21, 1/6)
text_thresholds <- c("20 false alerts for 1 true alert", 
                     "5 false alerts for 1 true alert")

max_LM_eval <- 30
