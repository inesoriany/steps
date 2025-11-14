#################################################
#################  PARAMETERS ###################
#################################################


## STEP LENGTH (Murtagh et al., 2020)
step_length_women <- 0.656 * 1e-3   # km
step_length_men <- 0.704 *1e-3     # km


## WALKING SPEED (Barban et al, 2022) ----
walk_speed <- 4.8  # km/h

## DRIVING SPEED (Kahlmeier, Götschi et al, 2017) ----
paris_car_speed <- 31  # km/h 
urban_car_speed <-  32
rural_car_speed <- 60 


##############################################################
#                          Diseases                          #
##############################################################

## DISABILITY WEIGHTS ----

dem_dw <-0.1518996
dem_dw_lb <-0.1250537
dem_dw_ub <-0.1758752

cvd_dw <-0.0526328
cvd_dw_lb <-0.04023609
cvd_dw_ub <-0.0645608

diab2_dw <- 0.06806817
diab2_dw_lb <-0.0504114
diab2_dw_ub <-0.08533913

mort_dw <- 1
mort_dw_lb <- 1
mort_dw_ub <- 1



## MEDICAL COSTS ----
cc_cost <- 26716
dem_cost <- 22748
bc_cost <- 46968
cvd_cost <- 20938
diab2_cost <- 36514
mort_cost <- NA




##############################################################
#                       Social cost                          #
##############################################################

## VALUE OF A STATISTICAL LIFE YEAR FOR 2019 FRANCE ----
vsl <- 133000



##############################################################
#                      CO2 emissions                         #
##############################################################

# CO2 emissions per km driven
CO2_emit <- 124                    # 124g CO2 per km








