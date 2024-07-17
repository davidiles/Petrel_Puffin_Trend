my_packs = c('tidyverse',
             'jagsUI',
             'mgcv',
             'scales',
             'ggthemes')

#,
#             'faux')

if (any(!my_packs %in% installed.packages()[, 'Package'])) {install.packages(my_packs[which(!my_packs %in% installed.packages()[, 'Package'])],dependencies = TRUE)}
lapply(my_packs, require, character.only = TRUE)

rm(list=ls())

# ------------------------------------------------
# Set working directory
# ------------------------------------------------

setwd("C:/Users/IlesD/OneDrive - EC-EC/Iles/Projects/Seabirds/Petrel_Puffin_Trend/code")

# ------------------------------------------------
# ggplot theme
# ------------------------------------------------

CustomTheme <- theme_set(theme_bw())
CustomTheme <- theme_update(legend.key = element_rect(colour = NA), 
                            legend.key.height = unit(1.2, "line"),
                            panel.grid.major = element_line(colour = 'transparent'),
                            panel.grid.minor = element_line(colour = 'transparent'),
                            panel.border = element_rect(linetype = "solid",
                                                        colour = "black",
                                                        size = 1, fill = NA),
                            axis.line = element_line(colour = "black"),
                            strip.text = element_text(size = 12, colour = "black"),
                            strip.background = element_rect(colour = "black",
                                                            fill = "lightblue2",
                                                            linetype = "solid"),
                            axis.title.y = element_text(margin = margin(0,10,0,0)),
                            axis.title.x = element_text(margin = margin(10,0,0,0)),
                            panel.background = element_rect(fill = "white"))

`%!in%` <- Negate(`%in%`)

# ----------------------------------------------------------
# Part 1: Simulate an entire 50-year time series for each of 15 colonies
# ----------------------------------------------------------

simulation_results <- data.frame()

for (run in 1:500){
  
  # How variable is the "shared" component of environmental variation?
  for (sd_shared in c(0,0.1)){
    
    set.seed(run)
    
    # Load results that have completed so far
    if (file.exists("../output/model_output/simulation_results.rds")) simulation_results <- readRDS("../output/model_output/simulation_results.rds")
    
    # Skip this iteration, if it has already been run
    if (nrow(simulation_results) > 0 & sum(simulation_results$run == run & simulation_results$sd_shared == sd_shared)) next
    
    ncolony <- 15
    nyears <- 50
    
    # Time-varying environmental covariate
    env_shared <- cumsum(rnorm(nyears,0,sd_shared))

    N_matrix <- matrix(NA,nrow=ncolony,ncol = nyears)
    
    # Initial abundance of each colony
    N_matrix[,1] <- rlnorm(ncolony, meanlog = log(10000), sdlog = 0.5) %>% sort()
    
    # Simulate trajectories at each colony
    for (i in 1:ncolony){
      
      env_unshared <- cumsum(rnorm(nyears,0,0.1))
      
      # Generate a random trajectory at the colony (random walk)
      for (t in 2:nyears){
        N_matrix[i,t] <- exp(log(N_matrix[i,1]) + env_shared[t] + env_unshared[t] + rnorm(1,0,0.1))
      }
    }
    
    # Convert to dataframe (to plot with ggplot)
    N_df <- reshape2::melt(N_matrix) %>% 
      rename(Colony = Var1,Year=Var2,N=value)
    
    # Plot dynamics (on log10 scale)
    ggplot()+
      geom_line(aes(x = 1:nyears, y = colSums(N_matrix), col = "Regional Sum"),linewidth = 1)+
      geom_line(data = N_df, aes(x = Year, y = N, col = factor(Colony)))+
      theme_few()+
      scale_y_continuous(labels = comma, trans = "log10")+
      ggtitle("Simulated trajectories at each of 15 colonies")+
      ylab("Abundance")+
      xlab("Year")
    
    ggplot()+
      geom_line(aes(x = 1:nyears, y = colSums(N_matrix), col = "Regional Sum"),linewidth = 1)+
      theme_few()+
      scale_y_continuous(labels = comma, trans = "log10")+
      ggtitle("Simulated trajectories at each of 9 colonies")+
      ylab("Abundance")+
      xlab("Year")
    
    # ----------------------------------------------------------
    # Part 2: Simulate intermittent surveys (2-6 surveys at each colony)
    # ----------------------------------------------------------
    
    intercept_SE = -0.95
    slope_SE = 0.86
    sd_SE = 0.45
    
    # Magnitude of observation error varies among surveys
    N_df$survey_SE <- rlnorm(nrow(N_df),intercept_SE + slope_SE * log(N_df$N),sd_SE)
    N_df$lambda_obs <- rnorm(nrow(N_df), N_df$N, N_df$survey_SE)
    N_df$lambda_obs[N_df$lambda_obs <0] <- 0
    
    # -------------------------------
    # Which surveys were actually conducted?
    # -------------------------------
    N_df$SurveyCount <-  NA
    
    for (i in 1:ncolony){
      
      # Rows in N_df corresponding to observations at this colony
      colony_rows <- which(N_df$Colony == i)
      
      survey_years <- c()
      
      # Simulate one count in first 5 years of surveys
      survey_years <- c(survey_years, sample(1:5,1))
      
      # Simulate one count in final 5 years of surveys
      survey_years <- c(survey_years, sample(nyears:(nyears-5),1))
      
      # Simulate 2-4 additional surveys
      survey_years <- c(survey_years, sample(6:(nyears-6),sample(0:4,1)))
      
      # Poisson observations
      N_df$SurveyCount[(N_df$Colony == i) & 
                         (N_df$Year %in% survey_years)] <- rpois(n = length(survey_years), 
                                                                 lambda = N_df$lambda_obs[(N_df$Colony == i) & (N_df$Year %in% survey_years)])
      
    }
    
    # Omit 35% of standard errors to mimick missing information in empirical data
    SEs_to_drop <- sample(1:nrow(N_df), size = round(0.35*nrow(N_df)))
    N_df$survey_SE[SEs_to_drop] <- NA
    
    # Confidence intervals on counts
    N_df$SurveyCount_lci <- N_df$SurveyCount - 1.96*N_df$survey_SE
    N_df$SurveyCount_uci <- N_df$SurveyCount + 1.96*N_df$survey_SE
    
    # Plot survey counts at each of the colonies
    ggplot()+
      geom_line(data = N_df, aes(x = Year, y = N), col = "gray80")+
      geom_errorbar(data = N_df, aes(x = Year, ymin = SurveyCount_lci, ymax = SurveyCount_uci), col = "black", width = 0)+
      geom_point(data = N_df, aes(x = Year, y = SurveyCount), col = "black")+
      geom_point(data = subset(N_df, is.na(survey_SE)), aes(x = Year, y = SurveyCount), col = "black", pch = 1, size =5)+
      
      theme_few()+
      facet_wrap(Colony~., scales = "free_y")+
      scale_y_continuous(labels = comma)+
      ggtitle("Simulated surveys at colonies")
    
    # ----------------------------------------------------------
    # Part 3: Fit model to simulated survey data
    # ----------------------------------------------------------
    
    spdat <- N_df %>% subset(!is.na(SurveyCount)) %>% dplyr::select(Colony,Year,SurveyCount,survey_SE)
    
    # Data for import into jags
    nknots = 6
    year <- spdat$Year
    ymax <- nyears
    colony = spdat$Colony
    count <- spdat$SurveyCount
    ncounts = length(count)
    obs_tau = 1/spdat$log_observation_SE^2
    
    # Use jagam to prepare basis functions
    nyearspred = length(1:ymax)
    preddat = data.frame(yrs = 1:ymax,count = 1)
    form = as.formula(paste("count ~ s(yrs,k =",nknots,")"))
    gamprep = jagam(formula = form,
                    data = preddat,
                    file = "tempgam.txt",
                    centred = T)
    
    # Package data into a list for JAGS
    jags_data = list(X = gamprep$jags.data$X,
                     S1 = gamprep$jags.data$S1,
                     zero = gamprep$jags.data$zero,
                     colony = colony,
                     ncounts = ncounts,
                     ncolony = ncolony,
                     count = count,
                     nknots = nknots,
                     nyearspred = nyearspred,
                     year = year,
                     survey_count = count,
                     survey_SE = spdat$survey_SE)
    
    # Fit model using JAGS
    parameters.to.save = c("intercept_SE","slope_SE","sd_SE","population_index")
    out <- jags(data = jags_data,
                parameters.to.save = parameters.to.save,
                inits = NULL,
                n.iter = 200000,
                n.burnin = 100000,
                n.thin = 50,
                model.file = "Seabird_Model.jags",
                n.chains = 3,
                parallel = TRUE)
    
    out$mcmc.info$elapsed.mins # ~1.5 mins
    
    # ----------------------------------------------------------
    # Part 4: Summarize predictions and compare to true (i.e., simulated) trajectories
    # ----------------------------------------------------------
    
    # Extract predictions in dataframe format
    fit_samples = reshape2::melt(out$sims.list$population_index) %>%
      rename(samp = Var1, Year = Var2, Colony = Var3, N_pred = value)
    
    N_summary_colony = fit_samples %>% 
      group_by(Colony, Year) %>%
      summarize(q025 = quantile(N_pred,0.025),
                q50 = quantile(N_pred,0.500),
                mean = mean(N_pred),
                q975 = quantile(N_pred,0.975)) %>%
      
      # Join with true values
      full_join(N_df)
    
    # Plot estimates
    ggplot()+
      geom_ribbon(data = N_summary_colony, aes(x = Year, ymin = q025, ymax = q975), alpha = 0.2, fill = "dodgerblue")+
      geom_line(data = N_summary_colony, aes(x = Year, y = q50, col = "Estimate"))+
      
      geom_line(data = N_df, aes(x = Year, y = N, col = "True Trajectory"))+
      geom_errorbar(data = N_df, aes(x = Year, ymin = SurveyCount_lci, ymax = SurveyCount_uci), col = "black", width = 0)+
      geom_point(data = N_df, aes(x = Year, y = SurveyCount, col = "Observed Count"))+
      theme_few()+
      facet_wrap(Colony~.)+
      scale_y_continuous(trans="log10", labels = comma)+
      ggtitle("Simulated regional trajectory")+
      scale_color_manual(values = c("dodgerblue","black","red"), name = "")+
      ylab("Index of abundance")
    
    # ----------------------------------------------------------
    # Part 5: Calculate regional annual indices as sum of individual colony annual indices
    # ----------------------------------------------------------
    
    # True regional annual indices
    regional_df <- N_df %>%
      group_by(Year) %>%
      summarize(N = sum(N))
    
    # Fit a GAM through the regional indices.  
    # This is the "true" trajectory we are trying to estimate
    regional_df$log_N <- log(regional_df$N)
    gam_true <- gam(log_N~s(Year, k = -1), data = regional_df)
    regional_df$population_index <- exp(predict(gam_true) )
    
    # Estimated regional annual indices
    regional_samples = fit_samples %>% 
      group_by(Year,samp) %>%
      summarize(N_pred = sum(N_pred)) 
    
    # Summary (mean and 95% CI)
    N_summary_regional <- regional_samples %>%
      group_by(Year) %>%
      summarize(q025 = quantile(N_pred,0.025),
                q50 = quantile(N_pred,0.500),
                mean = mean(N_pred),
                q975 = quantile(N_pred,0.975)) %>%
      
      # Join with true values
      full_join(regional_df)
    
    # ----------------------------------------------------------
    # 50-year trend estimate
    # ----------------------------------------------------------
    
    baseline_year <- 1
    trend_true_50yr <- 100*((regional_df$population_index[regional_df$Year == nyears]/regional_df$population_index[regional_df$Year == baseline_year])^(1/(nyears-baseline_year))-1)
    
    trend_est_50yr <- 100*((regional_samples$N_pred[regional_samples$Year == nyears]/regional_samples$N_pred[regional_samples$Year == baseline_year])^(1/(nyears-baseline_year))-1)
    trend_est_50yr <- quantile(trend_est_50yr,c(0.025,0.5,0.975))
    
    regional_plot_50yr <- ggplot()+
      geom_ribbon(data = N_summary_regional, aes(x = Year, ymin = q025, ymax = q975), alpha = 0.2, fill = "dodgerblue")+
      geom_line(data = N_summary_regional, aes(x = Year, y = q50, col = "Estimate"))+
      
      geom_point(data = regional_df, aes(x = Year, y = N))+
      geom_line(data = regional_df,aes(x = Year, y = population_index, col = "True Trajectory"))+
      theme_few()+
      scale_y_continuous(labels = comma)+
      ggtitle("Regional trajectory")+
      scale_color_manual(values = c("dodgerblue","black","red"), name = "")+
      ylab("Index of abundance")+
      geom_text(aes(x = 0, 
                    y = max(c(N_summary_regional$q975,N_summary_regional$N))), 
                label = paste0("True trend = ",round(trend_true_50yr,2),"% per year\nEst trend = ",round(trend_est_50yr[2],2),"% (",round(trend_est_50yr[1],2)," to ",round(trend_est_50yr[3],2),")"), hjust=0)
    
    # ----------------------------------------------------------
    # 10-year trend estimate
    # ----------------------------------------------------------
    
    baseline_year <- 41
    trend_true_10yr <- 100*((regional_df$population_index[regional_df$Year == nyears]/regional_df$population_index[regional_df$Year == baseline_year])^(1/(nyears-baseline_year))-1)
    
    trend_est_10yr <- 100*((regional_samples$N_pred[regional_samples$Year == nyears]/regional_samples$N_pred[regional_samples$Year == baseline_year])^(1/(nyears-baseline_year))-1)
    trend_est_10yr <- quantile(trend_est_10yr,c(0.025,0.5,0.975))
    
    regional_plot_10yr <- ggplot()+
      geom_ribbon(data = N_summary_regional, aes(x = Year, ymin = q025, ymax = q975), alpha = 0.2, fill = "dodgerblue")+
      geom_line(data = N_summary_regional, aes(x = Year, y = q50, col = "Estimate"))+
      
      geom_point(data = regional_df, aes(x = Year, y = N))+
      geom_line(data = regional_df,aes(x = Year, y = population_index, col = "True Trajectory"))+
      theme_few()+
      scale_y_continuous(labels = comma)+
      ggtitle("Regional trajectory")+
      scale_color_manual(values = c("dodgerblue","black","red"), name = "")+
      ylab("Index of abundance")+
      geom_text(aes(x = 0, 
                    y = max(c(N_summary_regional$q975,N_summary_regional$N))), 
                label = paste0("True trend = ",round(trend_true_10yr,2),"% per year\nEst trend = ",round(trend_est_10yr[2],2),"% (",round(trend_est_10yr[1],2)," to ",round(trend_est_10yr[3],2),")"), hjust=0)
    
    # ----------------------------------------------------------
    # Append results for this simulation run to dataframe
    # ----------------------------------------------------------
    if (file.exists("../output/model_output/simulation_results.rds")) simulation_results <- readRDS("../output/model_output/simulation_results.rds")
    
    simulation_results <- rbind(simulation_results,data.frame(run = run,
                                                              sd_shared = sd_shared,
                                                              trend_true_50yr = trend_true_50yr,
                                                              trend_est_50yr_q025 = trend_est_50yr[1],
                                                              trend_est_50yr_q500 = trend_est_50yr[2],
                                                              trend_est_50yr_q975 = trend_est_50yr[3],
                                                              cov_50yr = trend_true_50yr > trend_est_50yr[1] & trend_true_50yr < trend_est_50yr[3],
                                                              
                                                              trend_true_10yr = trend_true_10yr,
                                                              trend_est_10yr_q025 = trend_est_10yr[1],
                                                              trend_est_10yr_q500 = trend_est_10yr[2],
                                                              trend_est_10yr_q975 = trend_est_10yr[3],
                                                              cov_10yr = trend_true_10yr > trend_est_10yr[1] & trend_true_10yr < trend_est_10yr[3],
                                                              
                                                              max_Rhat = max(out$Rhat$population_index)))
    
    saveRDS(simulation_results, file = "../output/model_output/simulation_results.rds")
    
  }  # sd_shared
  
  # ----------------------------------------------------------
  # Plot results
  # ----------------------------------------------------------
  simulation_results <- readRDS("../output/model_output/simulation_results.rds")
  
  lim = range(simulation_results[,c("trend_true_50yr","trend_est_50yr_q025","trend_est_50yr_q975",
                                    "trend_true_10yr","trend_est_10yr_q025","trend_est_10yr_q975")])
  trend_plot_50yr <- ggplot(data = simulation_results, aes(x = trend_true_50yr, y = trend_est_50yr_q500, ymin = trend_est_50yr_q025, ymax = trend_est_50yr_q975,col=cov_50yr))+
    geom_abline(intercept=0,slope=1,col="gray85")+
    geom_errorbar(width=0)+
    geom_point()+
    coord_cartesian(ylim=lim,xlim=lim)+
    theme_bw()+
    xlab("True (simulated) regional 50-year trend")+
    ylab("Estimated regional 50-year trend")+
    scale_color_manual(values=c("red","dodgerblue"), name = "Coverage")+
    ggtitle("50-year trend estimates")+
    facet_grid(sd_shared~.)
  
  trend_plot_10yr <- ggplot(data = simulation_results, aes(x = trend_true_10yr, y = trend_est_10yr_q500, ymin = trend_est_10yr_q025, ymax = trend_est_10yr_q975,col=cov_10yr))+
    geom_abline(intercept=0,slope=1,col="gray85")+
    geom_errorbar(width=0)+
    geom_point()+
    coord_cartesian(ylim=lim,xlim=lim)+
    theme_bw()+
    xlab("True (simulated) regional 10-year trend")+
    ylab("Estimated regional 10-year trend")+
    scale_color_manual(values=c("red","dodgerblue"), name = "Coverage")+
    ggtitle("10-year trend estimates")+
    facet_grid(sd_shared~.)
  
}

# ----------------------------------------------------------
# Summarize results across repeated simulations
# ----------------------------------------------------------

simulation_results <- readRDS("../output/model_output/simulation_results.rds")

# Remove runs that failed to converge
simulation_results_converged <- subset(simulation_results, max_Rhat <= 1.1)

# ----------------------------------------------------------
# Plot results
# ----------------------------------------------------------

lim = range(simulation_results_converged[,c("trend_true_50yr","trend_est_50yr_q025","trend_est_50yr_q975",
                                  "trend_true_10yr","trend_est_10yr_q025","trend_est_10yr_q975")])
trend_plot_50yr <- ggplot(data = simulation_results_converged, aes(x = trend_true_50yr, y = trend_est_50yr_q500, ymin = trend_est_50yr_q025, ymax = trend_est_50yr_q975,col=cov_50yr))+
  geom_abline(intercept=0,slope=1,col="gray85")+
  geom_errorbar(width=0)+
  geom_point()+
  coord_cartesian(ylim=lim,xlim=lim)+
  theme_bw()+
  xlab("True (simulated) regional 50-year trend")+
  ylab("Estimated regional 50-year trend")+
  scale_color_manual(values=c("red","dodgerblue"), name = "Coverage")+
  ggtitle("50-year trend estimates")+
  facet_grid(sd_shared~.)

print(trend_plot_50yr)

trend_plot_10yr <- ggplot(data = simulation_results_converged, aes(x = trend_true_10yr, y = trend_est_10yr_q500, ymin = trend_est_10yr_q025, ymax = trend_est_10yr_q975,col=cov_10yr))+
  geom_abline(intercept=0,slope=1,col="gray85")+
  geom_errorbar(width=0)+
  geom_point()+
  coord_cartesian(ylim=lim,xlim=lim)+
  theme_bw()+
  xlab("True (simulated) regional 10-year trend")+
  ylab("Estimated regional 10-year trend")+
  scale_color_manual(values=c("red","dodgerblue"), name = "Coverage")+
  ggtitle("10-year trend estimates")+
  facet_grid(sd_shared~.)

print(trend_plot_10yr)

# Summarize accuracy and bias for 50-year trend estimates
simulation_results_converged %>%
  group_by(sd_shared) %>%
  summarize(n = n(),
            mean_bias = mean(trend_est_50yr_q500 - trend_true_50yr),
            SE_bias = sd(trend_est_50yr_q500 - trend_true_50yr),
            coverage = mean(cov_50yr))

# Summarize accuracy and bias for 10-year trend estimates
simulation_results_converged %>%
  group_by(sd_shared) %>%
  summarize(n = n(),
            mean_bias = mean(trend_est_10yr_q500 - trend_true_10yr),
            SE_bias = sd(trend_est_10yr_q500 - trend_true_10yr),
            coverage = mean(cov_10yr))