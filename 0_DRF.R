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
  left_join(rr_table_up, by = c("step")) %>% 
  mutate(across(where(is.character), as.numeric))



################################################################################################################################
#                                             5. GENERATE RR NORMAL DISTRIBUTIONS                                              #
################################################################################################################################


rr_table_long <- tibble()
set.seed(123)
for (dis in dis_vec) {
  rr_distrib <- rr_table %>%
    rowwise() %>%
    mutate(
      disease = dis,
      simulated_rr = list(generate_RR_distrib(
        get(dis),
        get(paste0(dis, "_low")),
        get(paste0(dis, "_up")),
        1000
      ))
    ) %>%
    unnest_longer(simulated_rr, indices_to = "simulation_id") %>%
    ungroup() %>% 
    select(1, disease, simulation_id, simulated_rr)
  
  rr_table_long <- bind_rows(rr_table_long, rr_distrib)
}

 


################################################################################################################################
#                                                       7. INTERPOLATION                                                       #
################################################################################################################################


  
  

















