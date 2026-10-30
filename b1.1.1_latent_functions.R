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