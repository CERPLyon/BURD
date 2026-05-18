library(FME)
library(tidyverse)
library(data.table)

source("b0.R")

# source("~/FIRESP/FIRESP_analysis/burden_triangle_v3.R")

# consider the first 100 days since admission to estimate the model
big_day_limit = 28 #number of days since admission for estimating the model
acute_day_limit = 3 #threshold to define a known community infection
suspected_day_limit = 7 #threshold for a suspcected community infection

interactive = F

# basic model control parameters
jump_par = c(sensAdm = 0.001, dailClr = 0.01, falsPos = 0.01)
lo_par = c(sensAdm = 0, dailClr = 0, falsPos = 0)
hi_par = c(sensAdm = 1, dailClr = NA, falsPos = 0.3)

# prevalence model control parameters
jump_prev = c(prev = 0.01)
# lo_prev = c(prev = 0)
hi_prev = c(prev = 1)


prior_function <- function(par){
  prior_loglik = dunif(x = par["sensAdm"], min = 0.5, max = 1, log = T) + 
    dbeta(x = par["falsPos"], shape1 = 1, shape2 = 1000, log = T)
    # dunif(x = par["falsPos"], min = 0, max = 0.3, log = T)
  
  #dbeta(x = par["sensAdm"], shape1 = 20, shape2 = 1, log = T) #+ 
    # dbeta(x = par["spec"], shape1 = 10, shape2 = 1, log = T)
    # dbeta(x = par["prev"], shape1 = 2, shape2 = 2, log = T) + 
    # dbeta(x = par["dailClr"], shape1 = 1, shape2 = 10, log = T)
  
  return(prior_loglik*-2)
}

# X = seq(-0.2, 1.2, by = 0.01)
# X = seq(0, 1, by = 0.01)
# plot(X, dbeta(x = X, shape1 = 10, shape2 = 1), type = "l")
# plot(X, dlnorm(x = X, meanlog = -2, sdlog = 0.3), type = "l")



fits = NULL
estStatus = NULL

if(F){ # use this to do a partial loop
  fits <- read_csv(file = paste0("output/fits.csv"))
  estStatus <- read_csv(file = paste0("output/estStatus.csv"))
  
  virus_done = fits$Virus

  # for(this_virus in virus_order[!(virus_order %in% virus_done)]){
}

this_virus = virus_vec[4]
# for(this_virus in setdiff(virus_order, "InfluenzaNT")){
for(this_virus in virus_vec){

    print(this_virus)
  
  this_virus_filename = gsub("/", "", this_virus) #remove slash from Enterovirus/Rhinovirus
  this_seed = which(virus_vec == this_virus)
  set.seed(this_seed)
  
  print("get master dataset")
  this_dataset <- data_PCR_sejour_collapseSubtypes %>% 
    filter(resultatbis %in% c("POSITIVE", "NEGATIVE")) %>% 
    filter(Virus == this_virus) %>% 
    filter(testday < big_day_limit) %>% 
    group_by(IPP_sejour) %>% 
    mutate(knownCommunity = as.numeric(any((testday < acute_day_limit) & resultatbis == "POSITIVE"))) %>% 
    ungroup %>% 
    transmute(patient_id = IPP_sejour, testday, y = (0:1)[match(resultatbis, c("NEGATIVE", "POSITIVE"))], knownCommunity) 
    

  print("get dataset of community infections")
  this_dataset_knownCommunity <- this_dataset %>% 
    filter(knownCommunity == 1)
  
  print("get lowerbound on prevalence")
  lowerbound_prev <- this_dataset %>% 
    group_by(patient_id) %>% 
    summarise(knownCommunity = knownCommunity[1]) %>% 
    ungroup %>% 
    pull(knownCommunity) %>% mean
  
  
  # this_dataset_knownCommunity %>% 
  #   group_by(testday) %>% 
  #   summarise(p = mean(y), n = n()) %>% 
  #   ggplot(aes(x = testday, y = p, size = n)) + geom_point()
  
  start_par = c(sensAdm = 1, dailClr = 0.05, falsPos = 0)
  
  run = T
  while(run){
  
    print("begin MCMC")
    mcmc_res <- modMCMC(niter = 5000, updatecov = 100, verbose = 100, 
                        f = calc_loglik_wrapper, 
                        prior = prior_function,
                        p = start_par, jump = jump_par, lower = lo_par, upper = hi_par, 
                        unfit_pars = c(prev = 1),
                        dt = as.data.table(this_dataset_knownCommunity))
    
    # estStatus_piece <- calc_loglik_wrapper(pars = mcmc_res$bestpar, 
    #                                        unfit_pars = c(prev = 1)
    #                                        this_dataset = this_dataset, return_pinfected = T) %>% 
    #   mutate(Virus = this_virus)
    

    if(interactive){
      mcmc_res %>% plot
    } else {
      png(file = paste0("diag/", this_virus_filename, "_plot1.png"))
      mcmc_res %>% plot
      mtext(this_virus, line = 3, adj = 0)
      dev.off()
    }

    if(T){
      
    # diagnostic plot 2 #
    plot2 <- this_dataset_knownCommunity %>% 
      group_by(testday) %>% 
      summarise(p_pos = mean(y), total = n()) %>% 
      mutate(remaining_sensitivity = calculate_remaining_sensitivity(mcmc_res$bestpar["sensAdm"], mcmc_res$bestpar["dailClr"], testday)) %>% 
      mutate(expected_positive = remaining_sensitivity*1) %>% #assume prev in this group is 1
      ungroup %>% 
      ggplot(aes(x = testday)) + 
      geom_point(aes(y = p_pos, size = total)) + 
      geom_line(aes(y = expected_positive), colour = "red", linetype = "dashed") + 
      # facet_wrap(Virus~., scales = "free") + 
      scale_size_continuous(trans = "log10") + 
      labs(x = "Days after admission", y = "Proportion of tests positive", title = this_virus)
    
    
    
    if(interactive){
      print(plot2)
    } else {
      ggsave(plot2, file = paste0("diag/", this_virus_filename, "_plot2.png"),
             width = 15, height = 8, units = "cm")
    }
    
    
    }
    
    run = F
    if(interactive){
      run = menu(c("n", "y"), title=paste0(this_virus, ": Run again?")) == 2 #includes a control point to rerun if it hasn't converged
    }
    
    start_par = mcmc_res$bestpar
    
  }
  
  lo_prev = c(prev = lowerbound_prev)
  start_prev = c(prev = sqrt(lowerbound_prev))
  
  run = T
  while(run){
    
    mcmc_prev <- modMCMC(niter = 1000, updatecov = 100, verbose = 100, 
                        f = calc_loglik_wrapper, 
                        # prior = prior_function,
                        p = start_prev, jump = jump_prev, lower = lo_prev, upper = hi_prev, 
                        unfit_pars = mcmc_res$bestpar,
                        dt = as.data.table(this_dataset)) #OR without the known community infections?
    
    estStatus_piece <- calc_loglik_wrapper(fit_pars = mcmc_prev$bestpar, unfit_pars = mcmc_res$bestpar, 
                                           dt = as.data.table(this_dataset), return_pinfected = T) %>% 
      mutate(Virus = this_virus)
    
    # diagnostic plot 1b #
    if(interactive){
      mcmc_prev %>% plot
    } else {
      png(file = paste0("diag/", this_virus_filename, "_plot1b.png"))
      mcmc_prev %>% plot
      mtext(this_virus, line = 3, adj = 0)
      dev.off()
    } 
    
    if(T){
      
      # diagnostic plot 3 #
      plot3 <- this_dataset %>% 
        group_by(patient_id) %>% 
        summarise(confirmedCommunity = as.numeric(any((testday < acute_day_limit) & y == 1))) %>% 
        left_join(estStatus_piece) %>% 
        ggplot(aes(x = p_infected, fill = as.factor(confirmedCommunity))) + 
        geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5) + 
        theme_bw()
      
      
      
      
      plot4 <- this_dataset %>% 
        group_by(patient_id) %>% 
        filter(!any((testday < acute_day_limit) & y == 1)) %>% 
        mutate(firstpos = min(testday[y == 1])) %>% 
        left_join(estStatus_piece) %>% 
        mutate(firstpos = ifelse(is.finite(firstpos), firstpos, -1)) %>% 
        group_by(firstpos) %>% 
        mutate(p_infected_mean = mean(p_infected)) %>% 
        ggplot(aes(x = firstpos)) + 
        geom_line(aes(y = p_infected_mean), colour = "red") + 
        geom_point(aes(y = p_infected),alpha = 0.2) + 
        theme_bw()
      
      # this_dataset %>% 
      #   group_by(patient_id) %>% 
      #   filter(!any((testday < acute_day_limit) & y == 1)) %>% 
      #   mutate(firstpos = min(testday[y == 1])) %>% 
      #   left_join(estStatus_piece) %>% 
      #   filter(p_infected > 0.25, p_infected < 0.5) %>% 
      #   View
      
      
      result_summary <- this_dataset %>% 
        group_by(patient_id) %>% 
        summarise(knownCommunity = as.numeric(any(testday < acute_day_limit & y == 1)), 
                  suspectedCommunity = (!knownCommunity) & as.numeric(any(testday >= acute_day_limit & testday < 7 & y == 1))) %>% 
        
        left_join(estStatus_piece) %>%
        # left_join(estStatus %>% filter(Virus == this_virus)) %>% 
        ungroup %>% 
        summarise(confirmed = sum(knownCommunity), 
                  suspected = sum(suspectedCommunity), 
                  inferredSuspected = sum(p_infected[suspectedCommunity == 1]), 
                  inferredAll = sum(ifelse(knownCommunity == 1, 1, p_infected)))

      
      if(interactive){
        
        print(plot3)
        print(plot4)
      } else {
        ggsave(plot3, file = paste0("diag/", this_virus_filename, "_plot3.png"),
               width = 15, height = 8, units = "cm")
        ggsave(plot4, file = paste0("diag/", this_virus_filename, "_plot4.png"),
               width = 15, height = 8, units = "cm")
      }
        

      
    }
    
    run = F
    if(interactive){
      run = menu(c("n", "y"), title=paste0(this_virus, ": Run again?")) == 2 #includes a control point to rerun if it hasn't converged
    }
    start_prev = mcmc_prev$bestpar
    
  }
  
  
  # store the results
  fits_piece = cbind(tibble(Virus = this_virus), c(mcmc_res$bestpar,mcmc_prev$bestpar) %>% t %>% as_tibble, 
                     result_summary)

  
  if(is.null(fits)){
    fits = fits_piece
    estStatus = estStatus_piece
  } else {
    fits <- fits %>% rows_upsert(fits_piece, by = "Virus")
    estStatus <- estStatus %>% rows_upsert(estStatus_piece, by = c("Virus", "patient_id"))
    # fits = rbind(fits, fits_piece)
    # estStatus = rbind(estStatus, estStatus_piece)
  }
  
  fits %>% write_csv(file = paste0("output/fits.csv"))
  estStatus %>% write_csv(file = paste0("output/estStatus.csv"))
  
  pause = F
  if(interactive){
    pause = menu(c("n", "y"), title=paste0("Finished ", this_virus, ": Take a pause?")) == 2 #includes a control point to rerun if it hasn't converged
    if(pause){
      stop()
    }
  }    
  
} #end of virus loop

data_PCR_sejour_age %>%
  filter(resultatbis %in% c("POSITIVE", "NEGATIVE")) %>%
  filter(testday < big_day_limit) %>%
  mutate(Virus = factor(Virus, virus_vec_slash)) %>%
  group_by(Virus, IPP_sejour) %>%
  summarise(knownCommunity = as.numeric(any((testday < acute_day_limit) & resultatbis == "POSITIVE")),
         probableCommunity = as.numeric(any((testday >= acute_day_limit & testday < 7) & resultatbis == "POSITIVE")) & (!knownCommunity) 
         ) %>%
  group_by(Virus) %>%
  summarise(confirmed = sum(knownCommunity), probable = sum(probableCommunity))


fits <- read_csv(file = paste0("output/fits.csv")) %>% 
  mutate(across(sensAdm:prev, as.numeric))

# 
# 
data_PCR_sejour_collapseSubtypes %>%
  filter(Virus != "InfluenzaNT") %>%
  filter(testday < big_day_limit) %>%
  # filter(Virus == this_virus) %>%
  filter(resultatbis %in% c("POSITIVE", "NEGATIVE")) %>%
  transmute(patient_id = IPP_sejour, Virus, testday, y = (0:1)[match(resultatbis, c("NEGATIVE", "POSITIVE"))]) %>%
  group_by(Virus, patient_id) %>% 
  mutate(knownCommunity = as.numeric(any((testday < acute_day_limit) & y == 1))) %>% 
  filter(knownCommunity == 1) %>% 
  group_by(Virus, testday) %>%
  summarise(p_pos = mean(y), total = n()) %>%
  left_join(fits %>%
              cross_join(tibble(testday = 0:big_day_limit)) %>%
              transmute(Virus, testday, seij = calculate_remaining_sensitivity(sensAdm, dailClr, testday),
                        # model = prev*(1-(1-seij)*(1-falsPos)) 
                        model = seij, falsPos
                        )) %>%
  mutate(Virus = factor(Virus, virus_vec_slash)) %>%
  ggplot(aes(x = testday)) +
  geom_point(aes(y = p_pos, size = total)) +
  geom_line(aes(y = model, colour = "Community infection"), linetype = "dashed", size = 1) +
  geom_line(aes(y = falsPos, colour = "False positive\nOR nosocomial infection"), linetype = "dashed", size = 1) +
  scale_colour_manual(values = c(`Community infection` = "red", `False positive\nOR nosocomial infection` = "blue")) +
  facet_wrap(Virus~., scales = "free") +
  labs(x = "Days after admission", y = "Proportion of tests positive", size = "Number of\nhospital stays", colour = "") +
  theme_bw() + 
  coord_cartesian(ylim = c(0, 1))

# ggsave("plots/data_knownCommunity.png", width = 20, height = 14, units = "cm")
ggsave("output/plots/data_model_knownCommunity.png", width = 22, height = 14, units = "cm")



data_PCR_sejour_age %>%
  filter(Virus != "InfluenzaNT") %>%
  filter(testday < big_day_limit) %>%
  # filter(Virus == this_virus) %>%
  filter(resultatbis %in% c("POSITIVE", "NEGATIVE")) %>%
  transmute(patient_id = IPP_sejour, Virus, testday, y = (0:1)[match(resultatbis, c("NEGATIVE", "POSITIVE"))]) %>%
  group_by(Virus, patient_id) %>% 
  mutate(knownCommunity = as.numeric(any((testday < acute_day_limit) & y == 1))) %>% 
  # filter(knownCommunity == 1) %>% 
  group_by(Virus, testday) %>%
  summarise(p_pos = mean(y), total = n()) %>%
  left_join(fits %>%
              cross_join(tibble(testday = 0:big_day_limit)) %>%
              transmute(Virus, testday, seij = prev*calculate_remaining_sensitivity(sensAdm, dailClr, testday),
                        # model = prev*(1-(1-seij)*(1-falsPos)) 
                        model = seij, falsPos
              )) %>%
  mutate(Virus = factor(Virus, virus_vec_slash)) %>%
  ggplot(aes(x = testday)) +
  geom_point(aes(y = p_pos, size = total)) +
  geom_line(aes(y = model, colour = "Community infection"), linetype = "dashed", size = 1) +
  geom_line(aes(y = falsPos, colour = "False positive\nOR nosocomial infection"), linetype = "dashed", size = 1) +
  scale_colour_manual(values = c(`Community infection` = "red", `False positive\nOR nosocomial infection` = "blue")) +
  facet_wrap(Virus~., scales = "free") +
  labs(x = "Days after admission", y = "Proportion of tests positive", size = "Number of\nhospital stays", colour = "") +
  theme_bw() #+ 
  # coord_cartesian(ylim = c(0, 0.4))

ggsave("output/plots/data_model_knownCommunity_wholeSample.png", width = 22, height = 14, units = "cm")




