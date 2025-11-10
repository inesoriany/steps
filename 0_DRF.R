#################################################
################ RR DISTRIBUTIONs ###############
#################################################

# Files needed:
  # rr_table_quanti.xlsx


# Files outputted:





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
  rr_table_mid <- import(here("data", "rr_table_quanti.xlsx"), sheet = "Central")
  
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
  left_join(rr_table_low, by = "step") %>%
  left_join(rr_table_up, by = "step") %>% 
  mutate(across(where(is.character), as.numeric))
  
for (dis in dis_vec) {
  rr_table <- rr_table %>% 
    rename(
      !!paste0(dis, "_mid") := !!sym(paste0(dis, ".x")),
      !!paste0(dis, "_low") := !!sym(paste0(dis, ".y")),
      !!paste0(dis, "_up")  := !!sym(paste0(dis))
    )
}
  


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
        get(paste0(dis, ".x")),
        get(paste0(dis, ".y")),
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
  complete(step = full_seq(0:12000, 1)) %>% 
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



# Depressive symptoms
  # Simulated RR normal distributions
  graph_drf_sim_dep <- graph_sim_DRF ("dep")
  plot(graph_drf_sim_dep)

  # RR mean and IC95 points
  graph_drf_dep <- graph_DRF ("dep")
  plot(graph_drf_dep)




# All DRF in one plot
  # All DRF curves simulated
  list_drf <- list(graph_drf_sim_mort, graph_drf_sim_cvd, graph_drf_sim_cancer, graph_drf_sim_diab2, graph_drf_sim_dem,
                  graph_drf_sim_dep)
  
  common_theme <- theme(
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7)
  )
  
  list_drf <- lapply(list_drf, function(p) p + common_theme)
  
  combined_plot_drf <- reduce(list_drf, `+`) + plot_layout(ncol = 3)
  
  print(combined_plot_drf) 


  # Mean + IC95
  list_mean_drf <- list(graph_drf_mort, graph_drf_cvd, graph_drf_cancer, graph_drf_diab2, graph_drf_dem,
                   graph_drf_dep)
  
  common_theme <- theme(
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7)
  )
  
  list_mean_drf <- lapply(list_drf, function(p) p + common_theme)
  
  combined_plot_mean_drf <- reduce(list_mean_drf, `+`) + plot_layout(ncol = 3)
  
  print(combined_plot_mean_drf) 




################################################################################################################################
#                                                   8. DATA EXPORTATION                                                        #
################################################################################################################################

# Table of DRF simulated
export(rr_table_interpolated, here("data_clean", "DRF", "rr_sim_interpolated.csv"))
export(ic95_rr, here("data_clean", "rr_interpolated_mean.csv"))



# Graphs (all DRF curves simulated)
ggsave(here("output", "DRF", "drf_mort.png"), plot = graph_drf_sim_mort)
ggsave(here("output", "DRF", "drf_cvd.png"), plot = graph_drf_sim_cvd)
ggsave(here("output", "DRF", "drf_cancer.png"), plot = graph_drf_sim_cancer)
ggsave(here("output", "DRF", "drf_diab2.png"), plot = graph_drf_sim_diab2)
ggsave(here("output", "DRF", "drf_dem.png"), plot = graph_drf_sim_dem)
ggsave(here("output", "DRF", "drf_dep.png"), plot = graph_drf_sim_dep)

ggsave(here("output", "DRF", "drf_all.png"), plot = combined_plot_drf)



# Graphs (mean + IC95)
ggsave(here("output", "DRF", "drf_mort_mean.png"), plot = graph_drf_mort)
ggsave(here("output", "DRF", "drf_cvd_mean.png"), plot = graph_drf_cvd)
ggsave(here("output", "DRF", "drf_cancer_mean.png"), plot = graph_drf_cancer)
ggsave(here("output", "DRF", "drf_diab2_mean.png"), plot = graph_drf_diab2)
ggsave(here("output", "DRF", "drf_dem_mean.png"), plot = graph_drf_dem)
ggsave(here("output", "DRF", "drf_dep_mean.png"), plot = graph_drf_dep)

ggsave(here("output", "DRF", "drf_all_mean.png"), plot = combined_plot_mean_drf)


















