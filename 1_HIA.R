#################################################
############ HEALTH IMPACT ASSESSMENT ###########
#################################################

# Files needed :
  # EMP_walkers.xlsx
  # 0_Functions.R
  # 0_Parameters.R

# Files outputted :



###########################################################################################################################################################################
###########################################################################################################################################################################
#                                                                          HIA - 2019                                                                                     #
###########################################################################################################################################################################
###########################################################################################################################################################################


################################################################################################################################
#                                                    1. LOAD PACKAGES                                                          #
################################################################################################################################
pacman :: p_load(
  rio,          # Data importation
  here,         # Localization of files 
  dplyr,        # Data management
  srvyr,        # Survey
  survey,
  ggplot2       # Data visualization
)




################################################################################################################################
#                                                     2. IMPORT DATA                                                           #
################################################################################################################################
# Walkers dataset
emp_walk <- import(here("data_clean", "EMP_walkers.xlsx"))


# RR by step, simulated dose-response relationships
rr_table <- import(here("data_clean", "DRF", "rr_interpolated_mean.csv"))


# Import functions
source(here("0_Functions.R"))



################################################################################################################################
#                                                      3. PARAMETERS                                                           #
################################################################################################################################

# Import parameters
source(here("0_Parameters.R"))

# Diseases considered
dis_vec = c("mort", "cvd", "diab2", "dem")



################################################################################################################################
#                                              4. HEALTH IMPACT ASSESSMENT                                                     #
################################################################################################################################

# Initialization
health_walkers <- emp_walk %>% 
  # Round the number of steps to the nearest ten
  mutate(step = pmin(12000, round(step / 10) * 10))

# Separate the datasets for every disease
for (dis in dis_vec) {
  dis_walkers <- paste0(dis, "_walkers")

  assign(dis_walkers,
         health_walkers %>%
           select(
             ident_ind,
             step,
             paste0(dis, "_rate")
           )
  )
}


# Associate simulated RR
RR_walkers <- tibble()

for (dis in dis_vec) {
  message("Processing disease: ", dis)
  
  # Filter rr_table for the corresponding disease
  rr_dis <- rr_table %>% 
    filter(disease == dis)
  
  # Baseline : zero walk
  rr_dis_zero <- rr_table %>%
    filter(disease == dis & step == 0) %>%
    select(simulation_id, rr_zero = rr_interpolated)
  
  
  # Associate the simultaed RR
  RR_dis <- get(paste0(dis, "_walkers")) %>% 
    inner_join(rr_dis, by = "step", relationship = "many-to-many") %>% 
    left_join(rr_dis_zero, by = "simulation_id") %>%
    group_by(simulation_id) %>% 
  
  # Calculate the disease risk reduction percentage for each individual
    mutate(!!paste0(dis, "_reduction_risk") := 1 - rr_interpolated / rr_zero) %>% 
    ungroup()
  
  # Stocker le résultat dans une liste
  RR_walkers <- bind_rows(RR_walkers, RR_dis)
}



  












