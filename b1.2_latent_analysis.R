

print("get lowerbound on prevalence")
# lowerbound_prev <- this_dataset %>% 
#   group_by(patient_id) %>% 
#   summarise(knownCommunity = knownCommunity[1]) %>% 
#   ungroup %>% 
#   pull(knownCommunity) %>% mean
lowerbound_prev_table <- this_dataset %>% 
  select(patient_id, age_group, knownCommunity) %>% 
  unique %>% 
  group_by(age_group) %>% 
  summarise(prev = mean(knownCommunity))




# analysis 1

start_par = c(sensAdm = 1, dailClr = 0.05, falsPos = 0)

print("begin MCMC")
mcmc_res <- modMCMC(niter = decay_iterations, updatecov = 100, verbose = 100, 
                    f = calc_loglik_wrapper_age, 
                    prior = prior_function,
                    FUN = calculate_remaining_sensitivity, 
                    p = start_par, 
                    jump = jump_par, lower = lo_par, upper = hi_par, 
                    unfit_pars = hi_prev_vec,
                    dt = as.data.table(this_dataset_knownCommunity))


post_res <- cbind(as_tibble(mcmc_res$pars)) %>% 
  mutate(Loglik = mcmc_res$SS*-0.5 + mcmc_res$prior*-0.5) %>% 
  mutate(Iteration = 1:n()) %>% 
  select(Iteration, Loglik, everything())

plot1 <- post_res %>% 
  pivot_longer(-c("Iteration")) %>% 
  ggplot(aes(x = Iteration, y = value)) + geom_line() + 
  facet_wrap(.~name, scales = "free_y")

ggsave(plot1, file = paste0("output/diag/", this_virus_filename, "_plot1.png"),
       width = 20, height = 12, units = "cm")

# diagnostic plot 2 #
plot2 <- this_dataset_knownCommunity %>% 
  group_by(testday, age_group) %>% 
  summarise(p_pos = mean(y), total = n()) %>% 
  mutate(remaining_sensitivity = calculate_remaining_sensitivity(mcmc_res$bestpar, t = testday)) %>% 
  mutate(expected_positive = remaining_sensitivity*1) %>% #assume prev in this group is 1
  ungroup %>% 
  ggplot(aes(x = testday)) + 
  geom_point(aes(y = p_pos, size = total)) + 
  geom_line(aes(y = expected_positive), colour = "red", linetype = "dashed") + 
  facet_wrap(age_group~., scales = "free") +
  scale_size_continuous(trans = "log10") + 
  labs(x = "Days after admission", y = "Proportion of tests positive", title = this_virus)

ggsave(plot2, file = paste0("output/diag/", this_virus_filename, "_plot2.png"),
       width = 20, height = 12, units = "cm")

start_par = mcmc_res$bestpar

res_possibleParams <- post_res %>% 
  arrange(-Loglik) %>% 
  mutate(maxLoglik = max(Loglik, na.rm = T), 
         deltaLoglik = Loglik - maxLoglik, 
         LRTstat = -2*deltaLoglik, 
         LRTp = pchisq(q = LRTstat, df = 3, lower.tail = F), 
         inCI = (0.01 <= LRTp)) %>% 
  filter(inCI) %>%
  select(sensAdm:falsPos
         # , Loglik
         ) %>% 
  unique

post_res %>% 
  arrange(-Loglik) %>% 
  mutate(maxLoglik = max(Loglik, na.rm = T), 
         deltaLoglik = Loglik - maxLoglik, 
         LRTstat = -2*deltaLoglik, 
         LRTp = pchisq(q = LRTstat, df = 3, lower.tail = F), 
         inCI = (0.05 <= LRTp)) %>% 
  filter(inCI) %>%
  select(sensAdm:falsPos
         , Loglik
  ) %>% 
  unique %>% 
  write_csv2(file = paste0("output/piecewise/validParams_decay_", this_virus_filename, ".csv"))

# analysis 2

lo_prev_vec = lowerbound_prev_table$prev %>% setNames(paste0("prev", lowerbound_prev_table$age_group))
# lo_prev = c(prev = lowerbound_prev)
# start_prev_vec = c(prev = sqrt(lowerbound_prev))
start_prev_vec = sqrt(lo_prev_vec)

start_res = mcmc_res$bestpar

run = T
prev_pars = NULL
# prev_Loglik = NULL
jump_factor = 1
while(run){
  
  print("starting parameters")
  print(start_res)
  print(start_prev_vec)
  
  mcmc_prev <- modMCMC(niter = prevalence_iterations, updatecov = 100, verbose = 100, 
                       f = calc_loglik_wrapper_age, 
                       # prior = prior_function,
                       p = start_prev_vec,
                       # p = mcmc_prev$bestpar,
                       jump = jump_prev_vec*jump_factor, 
                       lower = lo_prev_vec, 
                       upper = hi_prev_vec, 
                       unfit_pars = start_res,
                       FUN = calculate_remaining_sensitivity, 
                       dt = as.data.table(this_dataset)) #OR without the known community infections?
  

  # MAKE NEW LINES FROM THESE (add in the prior SS for the start_res parameters)
  post_prev <- cbind(as.data.frame(t(start_res)), mcmc_prev$pars) %>% 
    as_tibble %>% 
    mutate(Loglik = mcmc_prev$SS*-0.5 + mcmc_prev$prior*-0.5 + prior_function(par = start_res)) %>% 
    mutate(Iteration = 1:n()) %>% 
    select(Iteration, everything(), Loglik)
  
  
  analysis1_update <- cbind(res_possibleParams, t(mcmc_prev$bestpar)) %>% 
    as_tibble %>% 
    {mutate(., PosteriorLoglik = apply(., 1, function(x) calc_loglik_wrapper_age(fit_pars = x, unfit_pars = NULL, dt = as.data.table(this_dataset), FUN = calculate_remaining_sensitivity, return_loglik = T)))} %>% 
    {mutate(., PriorLoglik = apply(., 1, function(x) prior_function(par = x)))} %>% 
    mutate(Loglik = PriorLoglik + PosteriorLoglik) %>% 
    select(-PriorLoglik, -PosteriorLoglik)
  
  prev_pars <- rbind(prev_pars, post_prev %>% select(-Iteration), analysis1_update)
  
  # THEN IDENTIFY VALID PARAMS
  
  validParams_beforeFilter <- prev_pars %>% 
    arrange(-Loglik) %>% 
    mutate(maxLoglik = max(Loglik, na.rm = T), 
           deltaLoglik = Loglik - maxLoglik, 
           LRTstat = -2*deltaLoglik, 
           LRTp = pchisq(q = LRTstat, df = 9, lower.tail = F), 
           inCI = (0.05 <= LRTp))
  
  # print("validParams unfiltered")
  # print(validParams_beforeFilter %>% 
  #         arrange(-Loglik) %>% 
  #         slice(1:100))
  
  validParams <- validParams_beforeFilter %>% 
    filter(inCI) %>% 
    mutate(Virus = this_virus) %>% 
    select(-maxLoglik, -LRTstat, -LRTp, -inCI) %>% 
    select(Virus, everything()) %>% 
    unique
  
  print("validParams filtered")
  print(validParams)
  
  validParams %>% write_csv2(file = paste0("output/piecewise/validParams_age_", this_virus_filename, ".csv"))
  
  
  param_CIs <- validParams %>% 
    summarise(across(sensAdm:`prev80+`, list(min = min, max = max))) %>% 
    unlist
  
  # FROM THIS, GET THE NEW STARTING PARAMETERS
  
  best_res <- validParams %>% 
    slice(1) %>% 
    select(sensAdm:falsPos) %>% 
    unlist
  
  best_prev <- validParams %>% 
    slice(1) %>% 
    select(`prev0-1`:`prev80+`) %>% 
    unlist
  
  # store the results
  estStatus_piece <- calc_loglik_wrapper_age(fit_pars = best_res, unfit_pars = best_prev, #this specifically is not necessary, it will be reestimated locally
                                             FUN = calculate_remaining_sensitivity, 
                                             dt = as.data.table(this_dataset), return_pinfected = T) %>% 
    mutate(Virus = this_virus)
  
  result_summary <- this_dataset %>% 
    group_by(patient_id) %>% 
    summarise(knownCommunity = as.numeric(any(testday < acute_day_limit & y == 1)), 
              suspectedCommunity = (!knownCommunity) & as.numeric(any(testday >= acute_day_limit & testday < 7 & y == 1))) %>% 
    
    left_join(estStatus_piece) %>%
    # left_join(estStatus %>% filter(Virus == this_virus)) %>% 
    ungroup %>% 
    summarise(confirmed = sum(knownCommunity), 
              suspected = sum(suspectedCommunity), 
              inferredSuspected = sum(p_infected[suspectedCommunity == 1])
              # , inferredAll = sum(ifelse(knownCommunity == 1, 1, p_infected))
    )
  
  
  
  fits_piece = cbind(tibble(Virus = this_virus), 
                     c(best_res, best_prev) %>% t %>% as_tibble, 
                     c(param_CIs) %>% t %>% as_tibble, 
                     result_summary) %>% 
    as_tibble
  
  fits_piece %>% write_csv2(file = paste0("output/piecewise/fits_age_", this_virus_filename, ".csv"))
  estStatus_piece %>% write_csv2(file = paste0("output/piecewise/estStatus_age_", this_virus_filename, ".csv"))
  
  
  # AND DO ANY OF THE REDUCING JUMP SIZE ETC if necessary
  
  if (mcmc_prev$naccepted < 5) {
    jump_factor = jump_factor/2
    print(paste("reducing jump size to factor", jump_factor))
  }
  
  run = F
  if(nrow(validParams) < minRows_forCI){
    print(paste0("validParams: ", nrow(validParams)))
    
    run = T
  }

  validParams
  start_prev_vec = best_prev
  start_res <- best_res #replaces the starting res with the best parameters from the posterior of the previous analysis

  
  plot3 <- this_dataset %>% 
    group_by(age_group, testday) %>% 
    summarise(p_pos = mean(y), total = n()) %>% 
    mutate(remaining_sensitivity = calculate_remaining_sensitivity(best_res, t = testday)) %>% 
    left_join(best_prev %>% 
                enframe(name = "age_group", value = "prev") %>% 
                mutate(age_group = gsub("prev", "", age_group))) %>% 
    mutate(expected_positive = remaining_sensitivity*prev) %>% 
    ungroup %>% 
    ggplot(aes(x = testday)) + 
    geom_point(aes(y = p_pos, size = total)) + 
    geom_line(aes(y = expected_positive), colour = "red", linetype = "dashed") + 
    facet_wrap(age_group~., scales = "free") +
    scale_size_continuous(trans = "log10") + 
    labs(x = "Days after admission", y = "Proportion of tests positive", title = this_virus)
  
  # # diagnostic plot 4 #
  plot4 <- this_dataset %>%
    group_by(patient_id) %>%
    summarise(confirmedCommunity = as.numeric(any((testday < acute_day_limit) & y == 1))) %>%
    left_join(estStatus_piece) %>%
    ggplot(aes(x = p_infected, fill = as.factor(confirmedCommunity))) +
    geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5) +
    theme_bw()
  
  ggsave(plot3, file = paste0("output/diag/", this_virus_filename, "_plot3.png"),
         width = 15, height = 8, units = "cm")
  ggsave(plot4, file = paste0("output/diag/", this_virus_filename, "_plot4.png"),
         width = 20, height = 12, units = "cm")
  
  
}
