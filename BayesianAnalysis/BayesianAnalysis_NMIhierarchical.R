## Title: Analysis of entropy scores per context granularity
## Description: Analyses reported in section 3.2 of the paper
## Author: Kristina Kobrock
## Contact: kristina.kobrock@uni-osnabrueck.de
## Last Edit: 2026-05-27

setwd(normalizePath(dirname(rstudioapi::getActiveDocumentContext()$path)))

# BEST package is no longer active, but can still be downloaded from CRAN archive
# Download package tarball from CRAN archive
#url <- "https://cran.r-project.org/src/contrib/Archive/BEST/BEST_0.5.4.tar.gz"
#pkgFile <- "BEST_0.5.4.tar.gz"
#download.file(url = url, destfile = pkgFile)
# make sure the dependencies coda and rjags are installed
# Install package from downloaded file
#install.packages(pkgs=pkgFile, type="source", repos=NULL)

library(BEST)
library(tidyverse)

# NMI----------------------------

df_NMI <- read.csv('data_for_R_NMI_hierarchical.csv')

agg_NMI <- df_NMI %>%
  summarize(mean = mean(value),
            sd = sd(value))

grand_mean_NMI <- agg_NMI$mean
grand_sd_NMI <- agg_NMI$sd

# calculate a region of practical equivalence with zero according to recommendation by Kruschke (2018)
rope_NMI <- c(-0.1*grand_sd_NMI, 0.1*grand_sd_NMI)

priors_NMI <- list(muM = grand_mean_NMI, muSD = grand_sd_NMI)

fine_generic_concepts <- df_NMI %>% 
  filter(hierarchy_level == 0, condition == 'fine') %>% 
  select(value, condition)

fine_specific_concepts <- df_NMI %>% 
  filter(hierarchy_level == 4, condition == 'fine') %>% 
  select(value, condition)

mixed_generic_concepts <- df_NMI %>% 
  filter(hierarchy_level == 0, condition == 'mixed') %>% 
  select(value, condition)

mixed_specific_concepts <- df_NMI %>% 
  filter(hierarchy_level == 4, condition == 'mixed') %>% 
  select(value, condition)

coarse_generic_concepts <- df_NMI %>% 
  filter(hierarchy_level == 0, condition == 'coarse') %>% 
  select(value, condition)

coarse_specific_concepts <- df_NMI %>% 
  filter(hierarchy_level == 4, condition == 'coarse') %>% 
  select(value, condition)

# either load or generate models
#load("BEST_fine_NMI_hierarchical.Rda")
#load("BEST_mixed_NMI_hierarchical.Rda")
#load("BEST_coarse_NMI_hierarchical.Rda")
BEST_fine_NMI_hierarchical <- BESTmcmc(fine_specific_concepts$value, fine_generic_concepts$value, priors=priors_NMI, parallel=TRUE)
BEST_mixed_NMI_hierarchical <- BESTmcmc(mixed_specific_concepts$value, mixed_generic_concepts$value, priors=priors_NMI, parallel=TRUE)
BEST_coarse_NMI_hierarchical <- BESTmcmc(coarse_specific_concepts$value, coarse_generic_concepts$value, priors=priors_NMI, parallel=TRUE)

# check for convergence
print(BEST_fine_NMI_hierarchical)
print(BEST_mixed_NMI_hierarchical)
print(BEST_coarse_NMI_hierarchical)
# -> all models converged

Diff_fine <- (BEST_fine_NMI_hierarchical$mu1 - BEST_fine_NMI_hierarchical$mu2)
meanDiff_fine <- round(mean(Diff_fine), 2)
hdiDiff_fine <- round(hdi(BEST_fine_NMI_hierarchical$mu1 - BEST_fine_NMI_hierarchical$mu2),2)
plotAll(BEST_fine_NMI_hierarchical)
plot(BEST_fine_NMI_hierarchical, ROPE=rope_NMI)
summary(BEST_fine_NMI_hierarchical)
# CrI does not include 0
# 97.8% probability that the difference in means is larger than 0 (pd)
# 3% in ROPE

Diff_mixed <- (BEST_mixed_NMI_hierarchical$mu1 - BEST_mixed_NMI_hierarchical$mu2)
meanDiff_mixed <- round(mean(Diff_mixed), 2)
hdiDiff_mixed <- round(hdi(BEST_mixed_NMI_hierarchical$mu1 - BEST_mixed_NMI_hierarchical$mu2),2)
plotAll(BEST_mixed_NMI_hierarchical)
plot(BEST_mixed_NMI_hierarchical, ROPE=rope_NMI)
summary(BEST_mixed_NMI_hierarchical)
# CrI includes 0
# 91.5% probability that the difference in means is smaller than 0 (pd), 
# i.e. negative and large enough
# 19% in ROPE

Diff_coarse <- (BEST_coarse_NMI_hierarchical$mu1 - BEST_coarse_NMI_hierarchical$mu2)
meanDiff_coarse <- round(mean(Diff_coarse), 2)
hdiDiff_coarse <- round(hdi(BEST_coarse_NMI_hierarchical$mu1 - BEST_coarse_NMI_hierarchical$mu2), 2)
plotAll(BEST_coarse_NMI_hierarchical)
plot(BEST_coarse_NMI_hierarchical, ROPE=rope_NMI)
summary(BEST_coarse_NMI_hierarchical)
# CrI does not include 0
# 100% probability that the difference in means is larger than 0 (pd)
# 0% in ROPE

# save all models for reproducibility
write.csv(BEST_fine_NMI_hierarchical, "BEST_fine_NMI_hierarchical.csv", row.names=FALSE, quote=FALSE) 
save(BEST_fine_NMI_hierarchical,file="BEST_fine_NMI_hierarchical.Rda")
write.csv(BEST_fine_NMI_hierarchical, "BEST_mixed_NMI_hierarchical.csv", row.names=FALSE, quote=FALSE) 
save(BEST_fine_NMI_hierarchical,file="BEST_mixed_NMI_hierarchical.Rda")
write.csv(BEST_coarse_NMI_hierarchical, "BEST_coarse_NMI_hierarchical.csv", row.names=FALSE, quote=FALSE) 
save(BEST_coarse_NMI_hierarchical,file="BEST_coarse_NMIhierarchical.Rda")

# effectiveness----------------------------

df_effectiveness <- read.csv('data_for_R_effectiveness_hierarchical.csv')

agg_effectiveness <- df_effectiveness %>%
  summarize(mean = mean(value),
            sd = sd(value))

grand_mean_effectiveness <- agg_effectiveness$mean
grand_sd_effectiveness <- agg_effectiveness$sd

# calculate a region of practical equivalence with zero according to recommendation by Kruschke (2018)
rope_effectiveness <- c(-0.1*grand_sd_effectiveness, 0.1*grand_sd_effectiveness)

priors <- list(muM = grand_mean_effectiveness, muSD = grand_sd_effectiveness)

fine_generic_concepts <- df_effectiveness %>% 
  filter(hierarchy_level == 0, condition == 'fine') %>% 
  select(value, condition)

fine_specific_concepts <- df_effectiveness %>% 
  filter(hierarchy_level == 4, condition == 'fine') %>% 
  select(value, condition)

mixed_generic_concepts <- df_effectiveness %>% 
  filter(hierarchy_level == 0, condition == 'mixed') %>% 
  select(value, condition)

mixed_specific_concepts <- df_effectiveness %>% 
  filter(hierarchy_level == 4, condition == 'mixed') %>% 
  select(value, condition)

coarse_generic_concepts <- df_effectiveness %>% 
  filter(hierarchy_level == 0, condition == 'coarse') %>% 
  select(value, condition)

coarse_specific_concepts <- df_effectiveness %>% 
  filter(hierarchy_level == 4, condition == 'coarse') %>% 
  select(value, condition)

# either load or generate models
load("BEST_fine_effectiveness_hierarchical.Rda")
load("BEST_mixed_effectiveness_hierarchical.Rda")
load("BEST_coarse_effectiveness_hierarchical.Rda")
BEST_fine_effectiveness_hierarchical <- BESTmcmc(fine_specific_concepts$value, fine_generic_concepts$value, priors=priors, parallel=TRUE)
BEST_mixed_effectiveness_hierarchical <- BESTmcmc(mixed_specific_concepts$value, mixed_generic_concepts$value, priors=priors, parallel=TRUE)
BEST_coarse_effectiveness_hierarchical <- BESTmcmc(coarse_specific_concepts$value, coarse_generic_concepts$value, priors=priors, parallel=TRUE)

# check for convergence
print(BEST_fine_effectiveness_hierarchical)
print(BEST_mixed_effectiveness_hierarchical)
print(BEST_coarse_effectiveness_hierarchical)
# -> all models converged

Diff_fine <- (BEST_fine_effectiveness_hierarchical$mu1 - BEST_fine_effectiveness_hierarchical$mu2)
meanDiff_fine <- round(mean(Diff_fine), 2)
hdiDiff_fine <- round(hdi(BEST_fine_effectiveness_hierarchical$mu1 - BEST_fine_effectiveness_hierarchical$mu2),2)
plotAll(BEST_fine_effectiveness_hierarchical)
plot(BEST_fine_effectiveness_hierarchical, ROPE=rope_effectiveness)
summary(BEST_fine_effectiveness_hierarchical)
# CrI includes 0
# 94.9% probability that the difference in means is larger than 0 (pd)
# 21% in ROPE

Diff_mixed <- (BEST_mixed_effectiveness_hierarchical$mu1 - BEST_mixed_effectiveness_hierarchical$mu2)
meanDiff_mixed <- round(mean(Diff_mixed), 2)
hdiDiff_mixed <- round(hdi(BEST_mixed_effectiveness_hierarchical$mu1 - BEST_mixed_effectiveness_hierarchical$mu2),2)
plotAll(BEST_mixed_effectiveness_hierarchical)
plot(BEST_mixed_effectiveness_hierarchical, ROPE=rope_effectiveness)
summary(BEST_mixed_effectiveness_hierarchical)
# CrI does not include 0
# 99.4% probability that the difference in means is smaller than 0 (pd), 
# i.e. negative and large enough
# 3% in ROPE

Diff_coarse <- (BEST_coarse_effectiveness_hierarchical$mu1 - BEST_coarse_effectiveness_hierarchical$mu2)
meanDiff_coarse <- round(mean(Diff_coarse), 2)
hdiDiff_coarse <- round(hdi(BEST_coarse_effectiveness_hierarchical$mu1 - BEST_coarse_effectiveness_hierarchical$mu2), 2)
plotAll(BEST_coarse_effectiveness_hierarchical)
plot(BEST_coarse_effectiveness_hierarchical, ROPE=rope_effectiveness)
summary(BEST_coarse_effectiveness_hierarchical)
# CrI does not include 0
# 100% probability that the difference in means is smaller than 0 (pd)
# 0% in ROPE

# save all models for reproducibility
write.csv(BEST_fine_effectiveness_hierarchical, "BEST_fine_effectiveness_hierarchical.csv", row.names=FALSE, quote=FALSE) 
save(BEST_fine_effectiveness_hierarchical,file="BEST_fine_effectiveness_hierarchical.Rda")
write.csv(BEST_fine_effectiveness_hierarchical, "BEST_mixed_effectiveness_hierarchical.csv", row.names=FALSE, quote=FALSE) 
save(BEST_fine_effectiveness_hierarchical,file="BEST_mixed_effectiveness_hierarchical.Rda")
write.csv(BEST_coarse_effectiveness_hierarchical, "BEST_coarse_effectiveness_hierarchical.csv", row.names=FALSE, quote=FALSE) 
save(BEST_coarse_effectiveness_hierarchical,file="BEST_coarse_effectiveness_hierarchical.Rda")

# consistency----------------------------

df_consistency <- read.csv('data_for_R_consistency_hierarchical.csv')

agg_consistency <- df_consistency %>%
  summarize(mean = mean(value),
            sd = sd(value))

grand_mean_consistency <- agg_consistency$mean
grand_sd_consistency <- agg_consistency$sd

# calculate a region of practical equivalence with zero according to recommendation by Kruschke (2018)
rope_consistency <- c(-0.1*grand_sd_consistency, 0.1*grand_sd_consistency)

priors <- list(muM = grand_mean_consistency, muSD = grand_sd_consistency)

fine_generic_concepts <- df_consistency %>% 
  filter(hierarchy_level == 0, condition == 'fine') %>% 
  select(value, condition)

fine_specific_concepts <- df_consistency %>% 
  filter(hierarchy_level == 4, condition == 'fine') %>% 
  select(value, condition)

mixed_generic_concepts <- df_consistency %>% 
  filter(hierarchy_level == 0, condition == 'mixed') %>% 
  select(value, condition)

mixed_specific_concepts <- df_consistency %>% 
  filter(hierarchy_level == 4, condition == 'mixed') %>% 
  select(value, condition)

coarse_generic_concepts <- df_consistency %>% 
  filter(hierarchy_level == 0, condition == 'coarse') %>% 
  select(value, condition)

coarse_specific_concepts <- df_consistency %>% 
  filter(hierarchy_level == 4, condition == 'coarse') %>% 
  select(value, condition)

# either load or generate models
load("BEST_fine_consistency_hierarchical.Rda")
load("BEST_mixed_consistency_hierarchical.Rda")
load("BEST_coarse_consistency_hierarchical.Rda")
BEST_fine_consistency_hierarchical <- BESTmcmc(fine_specific_concepts$value, fine_generic_concepts$value, priors=priors, parallel=TRUE)
BEST_mixed_consistency_hierarchical <- BESTmcmc(mixed_specific_concepts$value, mixed_generic_concepts$value, priors=priors, parallel=TRUE)
BEST_coarse_consistency_hierarchical <- BESTmcmc(coarse_specific_concepts$value, coarse_generic_concepts$value, priors=priors, parallel=TRUE)

# check for convergence
print(BEST_fine_consistency_hierarchical)
print(BEST_mixed_consistency_hierarchical)
print(BEST_coarse_consistency_hierarchical)
# -> all models converged

Diff_fine <- (BEST_fine_consistency_hierarchical$mu1 - BEST_fine_consistency_hierarchical$mu2)
meanDiff_fine <- round(mean(Diff_fine), 2)
hdiDiff_fine <- round(hdi(BEST_fine_consistency_hierarchical$mu1 - BEST_fine_consistency_hierarchical$mu2),2)
plotAll(BEST_fine_consistency_hierarchical)
plot(BEST_fine_consistency_hierarchical, ROPE=rope_consistency)
summary(BEST_fine_consistency_hierarchical)
# CrI does not include 0
# 97.3% probability that the difference in means is larger than 0 (pd)
# 2% in ROPE

Diff_mixed <- (BEST_mixed_consistency_hierarchical$mu1 - BEST_mixed_consistency_hierarchical$mu2)
meanDiff_mixed <- round(mean(Diff_mixed), 2)
hdiDiff_mixed <- round(hdi(BEST_mixed_consistency_hierarchical$mu1 - BEST_mixed_consistency_hierarchical$mu2),2)
plotAll(BEST_mixed_consistency_hierarchical)
plot(BEST_mixed_consistency_hierarchical, ROPE=rope_consistency)
summary(BEST_mixed_consistency_hierarchical)
# CrI includes 0
# 86.2% probability that the difference in means is smaller than 0 (pd), 
# 9% in ROPE

Diff_coarse <- (BEST_coarse_consistency_hierarchical$mu1 - BEST_coarse_consistency_hierarchical$mu2)
meanDiff_coarse <- round(mean(Diff_coarse), 2)
hdiDiff_coarse <- round(hdi(BEST_coarse_consistency_hierarchical$mu1 - BEST_coarse_consistency_hierarchical$mu2), 2)
plotAll(BEST_coarse_consistency_hierarchical)
plot(BEST_coarse_consistency_hierarchical, ROPE=rope_consistency)
summary(BEST_coarse_consistency_hierarchical)
# CrI does not include 0
# 99.8% probability that the difference in means is smaller than 0 (pd)
# 0% in ROPE

# save all models for reproducibility
write.csv(BEST_fine_consistency_hierarchical, "BEST_fine_consistency_hierarchical.csv", row.names=FALSE, quote=FALSE) 
save(BEST_fine_consistency_hierarchical,file="BEST_fine_consistency_hierarchical.Rda")
write.csv(BEST_fine_consistency_hierarchical, "BEST_mixed_consistency_hierarchical.csv", row.names=FALSE, quote=FALSE) 
save(BEST_fine_consistency_hierarchical,file="BEST_mixed_consistency_hierarchical.Rda")
write.csv(BEST_coarse_consistency_hierarchical, "BEST_coarse_consistency_hierarchical.csv", row.names=FALSE, quote=FALSE) 
save(BEST_coarse_consistency_hierarchical,file="BEST_coarse_consistency_hierarchical.Rda")
