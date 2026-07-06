library(FME)
library(tidyverse)
library(data.table)

local = grepl("^C:/", getwd())

if(local){
  virus_index = 1
} else {
  cmds = commandArgs(trailingOnly = T)
  virus_index = as.numeric(cmds[1])
}

load("data/data_latent.RData", verbose = T)

decay_iterations = 20000
prevalence_iterations = 1000
minRows_forCI = 200

# consider the first 100 days since admission to estimate the model
acute_day_limit = 3 #threshold to define a known community infection
suspected_day_limit = 7 #threshold for a suspected community infection
big_day_limit = 28 #number of days since admission for estimating the model


# basic model control parameters
jump_par = c(sensAdm = 0.001, dailClr = 0.01, falsPos = 0.01)
lo_par = c(sensAdm = 0, dailClr = 0, falsPos = 0)
hi_par = c(sensAdm = 1, dailClr = NA, falsPos = 0.3)

# prevalence model control parameters
jump_prev = c(prev = 0.01)
# lo_prev = c(prev = 0)
hi_prev = c(prev = 1)
hi_prev_vec = jump_prev_vec <- numeric(length(age_vec)) %>% setNames(paste0("prev", age_vec))
hi_prev_vec[] = hi_prev
jump_prev_vec[] = jump_prev

prior_function <- function(par){
  prior_loglik = dunif(x = par["sensAdm"], min = 0.5, max = 1, log = T) + 
    dbeta(x = par["falsPos"], shape1 = 1, shape2 = 100, log = T)
  # dunif(x = par["falsPos"], min = 0, max = 0.3, log = T)
  
  #dbeta(x = par["sensAdm"], shape1 = 20, shape2 = 1, log = T) #+ 
  # dbeta(x = par["spec"], shape1 = 10, shape2 = 1, log = T)
  # dbeta(x = par["prev"], shape1 = 2, shape2 = 2, log = T) + 
  # dbeta(x = par["dailClr"], shape1 = 1, shape2 = 10, log = T)
  
  return(prior_loglik*-2)
}

# X = seq(-0.2, 1.2, by = 0.01)
# X = seq(0, 1, by = 0.01)
# plot(X, dbeta(x = X, shape1 = 1, shape2 = 10), type = "l")
# plot(X, dlnorm(x = X, meanlog = -2, sdlog = 0.3), type = "l")


# latent class model functions

calculate_remaining_sensitivity = function(pars, t){
  
  sensAdm = pars["sensAdm"]
  dailClr = pars["dailClr"]
  # this function calculates the sensitivity at day t, based on sensitivity at admission and the rate at which virus is cleared by the patient
  sensAdm*exp(-dailClr*t)
  # pmax(sens_adm - d_clear*t, 0)
}

calc_loglik_wrapper_age <- function(fit_pars, unfit_pars, dt, FUN, return_loglik = F, return_pinfected = F){
  # pars["dailClr"]
  # pars["sensAdm"]
  # pars["falsPos"]
  
  pars = c(fit_pars, unfit_pars)
  
  
  
  
  
  
  patient_tib <- dt[pars[grepl("prev", names(pars))] %>% 
                      enframe(name = "age_group", value = "prev") %>% 
                      mutate(age_group = gsub("prev", "", age_group)) %>% 
                      as.data.table, on = "age_group"] %>% 
    .[, seij := FUN(pars, t = testday)] %>% 
    .[, # p_y_d0 := pars["falsPos"]^y*(1-pars["falsPos"])^(1-y)][,
      # p_y_d1 := (1-(1-seij)*(1-pars["falsPos"]))^y*((1-seij)*(1-pars["falsPos"]))^(1-y)][,
      log_p_y_d0 := log(pars["falsPos"]^y*(1-pars["falsPos"])^(1-y))] %>% 
    .[, log_p_y_d1 := log((1-(1-seij)*(1-pars["falsPos"]))^y*((1-seij)*(1-pars["falsPos"]))^(1-y))] %>% 
    .[, .(log_lik_neg = sum(log_p_y_d0), log_lik_pos = sum(log_p_y_d1), prev = unique(prev)), by = patient_id] %>% 
    .[, log_lik := log(exp(log_lik_neg + log(1 - prev)) + exp(log_lik_pos + log(prev)))] 
  
  
  if(return_pinfected){
    return(patient_tib %>% 
             as_tibble %>% 
             transmute(patient_id, p_infected = exp(log_lik_pos + log(prev))/(exp(log_lik)))
    )
  }
  
  loglik <- patient_tib %>% 
    pull(log_lik) %>% 
    sum
  
  if(return_loglik){
    return(loglik)
  } else {
    return(loglik*-2)
  }
}

this_virus = virus_vec[virus_index]
this_virus_filename = gsub("/", "", this_virus) #remove slash from Enterovirus/Rhinovirus
this_seed = which(virus_vec == this_virus)
set.seed(this_seed)

print("get master dataset")
this_dataset <- data_PCR_sejour_collapseSubtypes %>% 
  filter(!(season_admitted %in% c("20/21", "21/22"))) %>% #take out the pandemic period for everyone
  filter((this_virus != "SARS-CoV-2") | (season_admitted %in% c("22/23", "23/24", "24/25"))) %>% #and for COVID make sure it's only post-pandemic
  filter(resultatbis %in% c("POSITIVE", "NEGATIVE")) %>% 
  filter(Virus == this_virus) %>% 
  filter(testday < big_day_limit) %>% 
  group_by(IPP_sejour, age_group, season_admitted) %>% 
  mutate(knownCommunity = as.numeric(any((testday < acute_day_limit) & resultatbis == "POSITIVE"))) %>% 
  ungroup %>% 
  transmute(patient_id = IPP_sejour, age_group, season_admitted, testday, y = (0:1)[match(resultatbis, c("NEGATIVE", "POSITIVE"))], knownCommunity) 


print("get dataset of community infections")
this_dataset_knownCommunity <- this_dataset %>% 
  filter(knownCommunity == 1)



source("b1.2_latent_analysis.R")