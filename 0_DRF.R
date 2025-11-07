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


# Import functions
source(here("0_Functions.R"))  


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
#                                                       6. INTERPOLATION                                                       #
################################################################################################################################

rr_table_interpolated <- rr_table_long %>% 
  group_by(disease, simulation_id) %>% 
  complete(step = full_seq(2000:12000, 1)) %>% 
  arrange(step) %>% 
  mutate(rr_interpolated = case_when(
    # quadratic (degree = 2)
    disease %in% c("mort", "cvd")   ~ if_else(is.na(simulated_rr),spline(step, simulated_rr, xout = step, method = "fmm")$y, simulated_rr),
    # cubic (degree = 3)
    disease %in% c("dem")           ~ if_else(is.na(simulated_rr), spline(step, simulated_rr, xout = step, method = "natural")$y, simulated_rr),
    # linear
    TRUE                            ~ approx(step, simulated_rr, xout = step, method = "linear", rule = 1)$y
  )) %>%
  # $y, rr, take the interpolated values in y from the approx function and assign them to rr
  mutate(rr_interpolated = if_else(step > max(step[!is.na(simulated_rr)]), NA_real_, rr_interpolated)) %>%
  ungroup() %>% 
  select("simulation_id", "disease", "step", "rr_interpolated")


# Random mix
set.seed(123)
rr_table_interpolated <- rr_table_interpolated %>%
  group_by(disease) %>%
  mutate(simulation_id = match(simulation_id, unique(simulation_id)),
         simulation_id = sample(unique(simulation_id))[simulation_id]) %>%
  ungroup()


# Mean and IC
ic95_rr <- rr_table_interpolated %>%
  group_by(disease, step) %>%
  summarise(
    rr_mean = mean(rr_interpolated, na.rm = TRUE),
    rr_lci = quantile(rr_interpolated, 0.025, na.rm = TRUE), # Lower limit of the 95% CI
    rr_uci = quantile(rr_interpolated, 0.975, na.rm = TRUE)  # Upper limit of the 95% CI
  ) %>% 
  ungroup()



################################################################################################################################
#                                                 7. VISUALIZATION OF DRF                                                      #
################################################################################################################################

# Disease name
names_disease <- c(
  "cancer" = "Cancer incidence",
  "cvd" = "Cardiovascular disease incidence",
  "dem" = "Dementia",
  "diab2" = "Type 2 diabetes",
  "dep" = "Depressive symptoms",
  "mort" = "All-cause mortality"
)

# Disease colour
colors_disease <- c(
  "cancer" = "firebrick2",      
  "cvd" = "gold" ,
  "dem" = "pink" ,
  "diab2" = "palegreen3",
  "dep" = "slateblue",
  "mort" = "steelblue"
)



# All-cause mortality
  # Simulated RR normal distributions
graph_drf_sim_mort <- graph_sim_DRF("mort")
plot(graph_drf_sim_mort)

  # RR mean and IC95 points
graph_drf_mort <- graph_DRF("mort")
plot(graph_drf_mort)




# Cardiovascular disease incidence
  # Simulated RR normal distributions
graph_drf_sim_cvd <- graph_sim_DRF ("cvd")
plot(graph_drf_sim_cvd)

  # RR mean and IC95 points
graph_drf_cvd <- graph_DRF ("cvd")
plot(graph_drf_cvd)




# Cancer incidence
  # Simulated RR normal distributions
graph_drf_sim_cancer <- graph_sim_DRF ("cancer")
plot(graph_drf_sim_cancer)

  # RR mean and IC95 points
graph_drf_cancer <- graph_DRF ("cancer")
plot(graph_drf_cancer)




# Type 2 diabetes
  # Simulated RR normal distributions
graph_drf_sim_diab2 <- graph_sim_DRF ("diab2")
plot(graph_drf_sim_diab2)

  # RR mean and IC95 points
graph_drf_diab2 <- graph_DRF ("diab2")
plot(graph_drf_diab2)



# Dementia
  # Simulated RR normal distributions
graph_drf_sim_dem <- graph_sim_DRF ("dem")
plot(graph_drf_sim_dem)

  # RR mean and IC95 points
graph_drf_dem <- graph_DRF ("dem")
plot(graph_drf_dem)










