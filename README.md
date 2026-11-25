

**Initial data load**

b0.0.R - initial data setup

b0.1_problem.R - testing plot

b0.2_bact_classification.R - classification of bacteriological data

b0.3_get_relativeWeek_relativeDay.R - functions



**Viral clearance latent class analysis**

b1.0_latent_setup.R - setup latent class analysis

b1.1.1_latent_functions.R - additional functions

b1.1_latent_launch.R - launch analysis (on SLURM)

b1.1_latent_launch_sensAnal.R - launch a sensitivity analysis (on SLURM)

b1.2_latent_analysis.R - analysis code

b1.3_latent_synthesis.R - process results of analysis

b1.3_latent_synthesis_sensAnal.R - process results of sensitivity analysis



**Machine-learning classification analysis**

b2.0_setup_xg.R - setup classification analysis

b2.1_train_xg.R - train XG model

b2.2_xgb_ncv.R - functions

b2.3_prep_xg.R - load correct dataset

b2.4_test_predictions.R - process results of analysis


**Death after discharge**

b3.0_make_DP_fullList.R - get full list of DP (diagnostique principale) in our dataset

b3.1_SNDS_deathRates.R - process data from SNDS



**Analysis**

b4.1_analysis.R - combine predictions

b4.2_analysis_DaysSensAnal.R - combine predictions (sensitivity analysis)

b4.3_analysis_TrainingSensAnal.R - combine predictions (sensitivity analysis)

b4.4_analysis_NoReadm30SensAnal.R - combine predictions (sensitivity analysis)


**Plots and tables**

b5.1_tables.R - produce tables

b5.2_figures.R - produce figures
