#################################################
################ RR DISTRIBUTIONs ###############
#################################################

# Files needed :
  # rr_table_quanti.xlsx


################################################################################################################################
#                                                    1. LOAD PACKAGES                                                          #
################################################################################################################################

pacman::p_load(
  rio,                 # file import/export
  here,                # file path management
  dplyr,               # data manipulation
  tidyr,               # data manipulation
  tidyverse,           # Data management, ggplot included
  patchwork,           # graphs combination
  splines              # interpolation
)


################################################################################################################################
#                                                     2. IMPORT DATA                                                           #
################################################################################################################################

# RR table for each outcome

  # Central values
  rr_table_mid <- import(here("data", "rr_table_quanti.xlsx"), sheet = "Mid")
  
  # IC95 lower
  rr_table_low <- import(here("data", "rr_table_quanti.xlsx"), sheet = "Lower")
  
  # IC95 upper
  rr_table_up <- import(here("data", "rr_table_quanti.xlsx"), sheet = "Upper") 


  ################################################################################################################################
  #                                                      3. PARAMETERS                                                           #
  ################################################################################################################################
  
  # Number of simulations
  n <- 1000
  
  
  ################################################################################################################################
  #                                                     4. PREPARE DATA                                                          #
  ################################################################################################################################
  
  # Diseases considered
  dis_vec = c("mort", "cvd", "cancer", "diab2", "dem", "dep")
  
  # One unique table 
  rr_table <- rr_table_mid %>% 
    left_join(rr_table_low, by = c("step")) %>%
    left_join(rr_table_up, by = c("step"))
  


  
  
  
  
  
  
  

















