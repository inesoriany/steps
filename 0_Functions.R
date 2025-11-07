#################################################
################## FUNCTIONS ####################
#################################################



################################################################################################################################
################################################################################################################################
#                                                   1. DRF & RESAMPLING                                                        #
################################################################################################################################
################################################################################################################################


# FUNCTION generate_RR : Generate random RR values in a normal distribution based on existing RR and their IC (Monte-Carlo)
#set.seed()
generate_RR_distrib = function (RR, low, sup, N) {          # N : number of random values
  lRR <- log(RR)                                            # Conversion in log scale
  l_low <- log(low)
  l_sup <- log(sup)
  
  sd1 <- (lRR - l_low) / qnorm(1-0.05/2)
  sd2 <- (l_sup - lRR) / qnorm(1-0.05/2) 
  sd <- mean( c(sd1, sd2))                          # Estimation of standard deviation assuming symmetrical confidence intervals
  
  distr_RR <- exp(rnorm(N, lRR, sd))                     # Generation of log-normal distribution (random samples)
  
  distr_RR[distr_RR>1]=1                                 # just need to truncat values
  distr_RR[distr_RR<0]=0
  
  return(distr_RR)                                       # Return simulated RR value
}


# FUNCTION graph_sim_DRF : Graphical representation of the n possible DRF from normal distributions
graph_sim_DRF = function (dis) {
  graph_drf_sim_dis <- ggplot(rr_table_interpolated %>% 
                                 filter(disease == dis,
                                        step %in% 2000:12000),
                               aes(x = step,
                                   y = rr_interpolated,
                                   group = simulation_id,
                                   color = disease))+
    scale_color_manual(values = colors_disease[dis])+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = names_disease[dis],
         x = "Steps per day",
         y = "RR")+
    theme(legend.position = "none")
  
  return(graph_drf_sim_dis)
}


# FUNCTION graph_DRF : Graphical representation of the mean DRF (with IC95)
graph_DRF = function (dis) {
  graph_drf_dis <- ggplot(ic95_rr %>% 
                             filter(disease == dis,
                                    step %in% 2000:12000),
                           aes(x = step))+
    geom_ribbon(aes(ymin = rr_lci,
                    ymax = rr_uci),
                fill = colors_disease[dis],
                alpha = 0.3)+
    geom_line(aes(y = rr_mean),
              color = colors_disease[dis],
              linewidth
              = 1)+
    labs(title = names_disease [dis],
         x = "Steps per day",
         y = "RR")+
    theme(legend.position = "none")
 
  return(graph_drf_dis) 
}


















