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
  patchwork            # graphs combination
)


################################################################################################################################
#                                                     2. IMPORT DATA                                                           #
################################################################################################################################

# RR table for each food group, assigned to absolute quantities

  # Central values
  rr_table_mid <- import(here("data", "rr_table_quanti.xlsx"), sheet = "Mid")
  
  # IC95 lower
  rr_table_low <- import(here("data", "rr_table_quanti.xlsx"), sheet = "Lower")
  
  # IC95 upper
  rr_table_up <- import(here("data", "rr_table_quanti.xlsx"), sheet = "Upper") 




















