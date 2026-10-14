# BURD0
library(tidyverse)
library(data.table)

source("b0.1_get_relativeWeek_relativeDay.R")

# virus_vec = c("InfluenzaA", "InfluenzaB", "SARS-CoV-2", "RSV", "hMPV", "Rhinovirus", "EnterovirusRhinovirus", 
#               "Enterovirus", "ADV","PIV","hCoV")
# virus_vec_slash = c("InfluenzaA", "InfluenzaB", "SARS-CoV-2", "RSV", "hMPV", "Rhinovirus", "Enterovirus/Rhinovirus", 
#               "Enterovirus", "ADV","PIV","hCoV")

# remove(virus_vec)
# remove(virus_vec_slash)

virus_vec = c("InfluenzaA", "InfluenzaB", "SARS-CoV-2", "RSV", 
              "hMPV", 
              "RVEV", 
              # "Rhinovirus", "EnterovirusRhinovirus", "Enterovirus", 
              "ADV","PIV","hCoV")

age_vec = c("0-1", "2-4", "5-17", "18-64", "65-79", "80+")

# load the FIRESP data (not the biomarker data until necessary, it's big)
# path.data="Z:/FIRESP 2/data/"
path.out = "Z:/FIRESP 2/output/"
# path.out = "Z:/FIRESP 2/output/Before IPP_sejour_noreadm30/"

tmp_savefile = paste0(path.out, "clean_tmp.RData")
biomarker_savefile = paste0(path.out, "biomarker_tmp.RData")
bact_savefile = paste0(path.out, "bact_tmp.RData")

load(tmp_savefile, verbose = T)
load(biomarker_savefile)
load(bact_savefile)

# load files
# catchment_rhone_interp <- read_delim(file = "Z:/data/INSEE/Rhone_PopAge_extrapolated.csv", delim = ";", show_col_types = FALSE)
# catchment_rhone_interp <- read_delim(file = "~/FIRESP/FIRESP_analysis/Rhone_PopAge_interpolated - copy.csv", delim = ";", show_col_types = FALSE)

load(file = "~/ZoneCap/output/HCLcatchment_ageyear.RData", verbose = T)
HCLcatchment_ageyear

# load("Z:/output/clean_tmp.RData")

# define key datasets

data_PCR_sejour_age <- data_PCR %>% 
  left_join(data_sejours %>% select(NDA_pseudo_ordre, age, type_sej, year_admitted, week_admitted, date_ent_sejour), by = "NDA_pseudo_ordre") %>% 
  mutate(testday = difftime(date_preleve, date_ent_sejour, units = "days") %>% as.numeric)# %>% 

data_PCR_sejour_collapseSubtypes <- data_PCR_sejour_age %>% 
  filter(Virus != "Enterovirus", Virus != "InfluenzaNT") %>% 
  mutate(Virus = fct_collapse(Virus, RVEV = c("Rhinovirus", "Enterovirus/Rhinovirus"))) %>% 
  mutate(Virus = factor(Virus, virus_vec)) %>% 
  group_by(IPP_alea, IPP_sejour_noreadmin30, IPP_sejour, NDA_pseudo, NDA_pseudo_ordre, type_sej, Virus, season_admitted, year_admitted, week_admitted, pandemic_status, age, age_group, date_ent_sejour, date_preleve, testday, ordre) %>% 
  summarise(resultatbis = case_when(any(resultatbis == "POSITIVE") ~ "POSITIVE", 
                                    any(resultatbis == "NEGATIVE") ~ "NEGATIVE", 
                                    T ~ "UNINTERPRETABLE"))

remove(data_PCR_sejour_age)

# virus_order_basic <- data_PCR$Virus %>% table %>% sort(decreasing = T) %>% names
# virus_order <- paste0(c("Influenza"), c("A", "B", "NT")) %>% {c(., setdiff(virus_order_basic, .))}

age_vec = data_sejours$age_group %>% levels
season_vec = data_sejours$season_admitted %>% unique %>% sort


summ_stats <- rbind(as.data.table(biochimie), as.data.table(hematologie))[, .(n_obs = .N, 
                                                                              max_obs = max(result_with_limits), 
                                                                              min_obs = min(result_with_limits), 
                                                                              median_obs = median(result_with_limits), 
                                                                              mean_obs = mean(result_with_limits)), 
                                                                          .(NDA_pseudo, IPP_sejour, analysis)] %>% 
  as_tibble


# biochimie_testday <- biochimie %>% 
#   left_join(data_sejours %>% select(IPP_sejour, date_ent_sejour) %>% unique) %>% 
#   mutate(marker_testday = as.numeric(date_preleve - date_ent_sejour))
# 
# hematologie_testday <- hematologie %>% 
#   left_join(data_sejours %>% select(IPP_sejour, date_ent_sejour) %>% unique) %>% 
#   mutate(marker_testday = as.numeric(date_preleve - date_ent_sejour))
# 
# summ_stats_02 <- rbind(as.data.table(biochimie_testday), as.data.table(hematologie_testday))[marker_testday <= 2, .(n_obs = .N, 
#                                                                               max_obs = max(result_with_limits), 
#                                                                               min_obs = min(result_with_limits), 
#                                                                               median_obs = median(result_with_limits), 
#                                                                               mean_obs = mean(result_with_limits)), 
#                                                                           .(NDA_pseudo, IPP_sejour, analysis)] %>% 
#   as_tibble


# summ_stats <- rbind(biochimie, hematologie) %>% 
#   group_by(NDA_pseudo, IPP_sejour, analysis) %>% 
#   summarise(n_obs = n(), max_obs = max(result_with_limits), min_obs = min(result_with_limits), median_obs = median(result_with_limits), mean_obs = mean(result_with_limits))

summ_stats_wide <- summ_stats %>%
  # summ_stats_02_wide <- summ_stats_02 %>%
  rename_all(~gsub("_obs", "", .x)) %>% 
  pivot_wider(id_cols = c("NDA_pseudo", "IPP_sejour"), names_from = analysis, names_glue = "{analysis}_{.value}",
              values_from = c("n", "max", "min", "mean", "median")) %>% 
  rename_all(~gsub("dfg 2009", "dfg", .x)) %>% 
  full_join(data_sejours %>% 
              select(NDA_pseudo, IPP_sejour) %>% 
              unique) %>% 
  mutate(across(ends_with("_n"), ~replace_na(.x, 0)))

summ_stats_wide %>% 
  write_csv("data/biomarkers_summ_stat.csv")


remove(biochimie)
remove(hematologie)
remove(summ_stats)
remove(diag_sejour_long_beforeExclusions)

sort( sapply(ls(),function(x){object.size(get(x))})) 
# summ_stats_02_wide %>% 
#   write_csv("data/biomarkers_summ_stat_02.csv")

# save.image()

# calc_loglik_wrapper <- function(fit_pars, unfit_pars, this_dataset, return_loglik = F, return_pinfected = F){
#   # pars["dailClr"]
#   # pars["sensAdm"]
#   # pars["falsPos"]
#   
#   pars = c(fit_pars, unfit_pars)
#   
#   n = n_distinct(this_dataset$patient_id)
#   
#   patient_tib <- this_dataset %>% 
#     mutate(seij = calculate_remaining_sensitivity(sens_adm = pars["sensAdm"], d_clear = pars["dailClr"], t = testday), 
#            p_y_d0 = pars["falsPos"]^y*(1-pars["falsPos"])^(1-y), 
#            p_y_d1 = (1-(1-seij)*(1-pars["falsPos"]))^y*((1-seij)*(1-pars["falsPos"]))^(1-y),
#            
#            log_p_y_d0 = log(p_y_d0), 
#            log_p_y_d1 = log(p_y_d1)#, 
#            # log_p_y_d0 = log(p_y_d0)
#            
#     ) %>% 
#     # summarise(log_lik_neg = sum(log_p_y_d0)+n*log(1-pars["prev"]),
#     #           log_lik_pos = sum(log_p_y_d1)+n*log(pars["prev"])
#     # ) %>%
#     group_by(patient_id) %>%
#     summarise(log_lik_neg = sum(log_p_y_d0)+log(1-pars["prev"]),
#               log_lik_pos = sum(log_p_y_d1)+log(pars["prev"])
#     ) %>%
#     mutate(log_lik = log(exp(log_lik_neg) + exp(log_lik_pos))) %>% 
#     ungroup
#   
#   if(return_pinfected){
#     return(patient_tib %>% 
#              transmute(patient_id, p_infected = exp(log_lik_pos)/(exp(log_lik)))
#            )
#   }
#   
#   loglik <- patient_tib %>% 
#     pull(log_lik) %>% 
#     sum
#   
#   if(return_loglik){
#     return(loglik)
#   } else {
#     return(loglik*-2)
#   }
# }

# calc_loglik_latentclass <- function(pars, this_dataset, return_loglik = F, return_pinfected = F){
#   # pars["prev"]
#   # pars["spec"]
#   # pars["dailClr"]
#   # pars["sensAdm"]
#   patient_tib <- this_dataset %>% 
#     mutate(seij = calculate_remaining_sensitivity(sens_adm = pars["sensAdm"], d_clear = pars["dailClr"], t = testday), 
#            p_y_d1 = seij^y * (1-seij)^(1-y), 
#            p_y_d0 = pars["spec"]^(1-y) * (1-pars["spec"])^y, 
#            log_p_y_d1 = log(p_y_d1),
#            log_p_y_d0 = log(p_y_d0)
#     ) %>% 
#     group_by(patient_id) %>% 
#     summarise(log_lik_pos = sum(log_p_y_d1) + log(pars["prev"]), 
#               log_lik_neg = sum(log_p_y_d0) + log(1-pars["prev"])) %>% 
#     ungroup
#   
#   if(return_pinfected){
#     return(patient_tib %>% 
#              transmute(patient_id, p_infected = exp(log_lik_pos)/(exp(log_lik_pos) + exp(log_lik_neg))))
#   }
#   
#   loglik <- patient_tib %>% 
#     mutate(loglik = log(exp(log_lik_pos) + exp(log_lik_neg))) %>% 
#     pull(loglik) %>% 
#     sum
#   
#   if(return_loglik){
#     return(loglik)
#   } else {
#     return(loglik*-2)
#   }
# }

