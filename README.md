# Comparison of RFSRC models on CLABSI data

The project shares the code used to compare RF models (binary, multinomial, survival and competing risks) for 7-days CLABSI prediction, as studied in: https://arxiv.org/abs/2404.16127

The **pipeline** directory contains numbered R scripts used to:

- create baseline and dynamic inbags (starting from imputed train sets): (**0_4_BASE_00_xxx** and **0_4_DYN_00_xxx**)

- build the baseline models (**0_4_BASE_02_build_RFSRC_xxx**)

- build the dynamic models (**0_4_DYN_03_build_RFSRC_xxx**)

- evaluate curves (**0_5_xxx**); basic and dynamic models are evaluated at the end of each script

The **R** directory contains functions used to build and evaluate the models

- **build_RFSRC_BASE** and **build_RFSRC_DYN** are the wrapper functions around rfsrc and mlrMBO to build the models for all model types (binary, multinomial, survival, CR)

- **config_playgorund_model_building** contains some general configuration loaded before model building

- **eval_functions** contains functions used to evaluate the models on test sets
