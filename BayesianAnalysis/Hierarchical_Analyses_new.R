## Title: Analysis of entropy scores per concept hierarchy level
## Description: Analyses reported in section 3.3 of the paper
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

library(tidyverse)
library(brms)
library(dplyr)

# NMI----------------------------

df_NMI <- read.csv('data_for_R_NMI_hierarchical.csv')

df_NMI <- df_NMI %>%
  dplyr::rename(entropy = value)

df_NMI$condition <- factor(df_NMI$condition, levels = c("mixed", "fine", "coarse"))
#contrasts(df_NMI$condition) <- contr.sum(2)
fit_NMI_hier <- brm(entropy ~ condition * hierarchy_level + (1|dataset),
                    data=df_NMI,
                    prior =
                      c(prior(uniform(0, 1), class = Intercept, lb = 0, ub = 1),
                        prior(uniform(0, 0.1), class = sigma, lb = 0, ub = 0.1)),
                    # increasing adapt_delta because of few divergent transitions
                    # after warmup
                    control = list(adapt_delta = 0.99),
                    file = "LM_fit_NMI_hier"
)
summary(fit_NMI_hier)
pp_check(fit_NMI_hier)
describe_posterior(fit_NMI_hier, rope_range = rope_range(fit_NMI_hier))
# no difference between fine and mixed
# significant difference between coarse and mixed

# difference between NMI scores for mixed vs. fine condition and specific concepts:
# NMI----------------------------

agg_NMI <- df_NMI %>%   
  summarize(mean = mean(entropy),
            sd = sd(entropy))

grand_mean_NMI <- agg_NMI$mean
grand_sd_NMI <- agg_NMI$sd

# calculate a region of practical equivalence with zero according to recommendation by Kruschke (2018)
rope_NMI <- c(-0.1*grand_sd_NMI, 0.1*grand_sd_NMI)

priors_NMI <- list(muM = grand_mean_NMI, muSD = grand_sd_NMI)

fine_specific_concepts <- df_NMI %>% 
  filter(hierarchy_level == 4, condition == 'fine') %>% 
  dplyr::select(entropy, condition)

mixed_specific_concepts <- df_NMI %>% 
  filter(hierarchy_level == 4, condition == 'mixed') %>% 
  dplyr::select(entropy, condition)

# either load or generate models
#load("BEST_specific_fine_mixed_NMI_hierarchical.Rda")
BEST_specific_fine_mixed_NMI_hierarchical <- BESTmcmc(fine_specific_concepts$entropy, mixed_specific_concepts$entropy, priors=priors_NMI, parallel=TRUE)

# check for convergence
print(BEST_specific_fine_mixed_NMI_hierarchical)

Diff_specific <- (BEST_specific_fine_mixed_NMI_hierarchical$mu1 - BEST_specific_fine_mixed_NMI_hierarchical$mu2)
meanDiff_specific <- round(mean(Diff_specific), 2)
hdiDiff_specific <- round(hdi(BEST_specific_fine_mixed_NMI_hierarchical$mu1 - BEST_specific_fine_mixed_NMI_hierarchical$mu2),2)
plotAll(BEST_specific_fine_mixed_NMI_hierarchical)
plot(BEST_specific_fine_mixed_NMI_hierarchical, ROPE=rope_NMI)
summary(BEST_specific_fine_mixed_NMI_hierarchical)
# CrI does not include 0
# 99% probability that the difference in means is larger than 0 (pd)
# 1% in ROPE


# effectiveness----------------------------

df_effectiveness <- read.csv('data_for_R_effectiveness_hierarchical.csv')

df_effectiveness <- df_effectiveness %>% 
  dplyr::rename(entropy = value)

agg_effectiveness <- df_effectiveness %>%
  summarize(mean = mean(entropy),
            sd = sd(entropy))

grand_mean_effectiveness <- agg_effectiveness$mean
grand_sd_effectiveness <- agg_effectiveness$sd

df_effectiveness$condition <- factor(df_effectiveness$condition, levels = c("mixed", "fine", "coarse"))
#contrasts(df_NMI$condition) <- contr.sum(2)
fit_effectiveness_hier <- brm(entropy ~ condition * hierarchy_level + (1|dataset),
                    data=df_effectiveness,
                    prior =
                      c(prior(uniform(0, 1), class = Intercept, lb = 0, ub = 1),
                        prior(uniform(0, 0.1), class = sigma, lb = 0, ub = 0.1)),
                    # increasing adapt_delta because of few divergent transitions
                    # after warmup
                    control = list(adapt_delta = 0.99),
                    file = "LM_fit_effectiveness_hier"
)
summary(fit_effectiveness_hier)
pp_check(fit_effectiveness_hier)
describe_posterior(fit_effectiveness_hier, rope_range = rope_range(fit_effectiveness_hier))
# no difference between fine and mixed
# significant difference between coarse and mixed

# consistency----------------------------

df_consistency <- read.csv('data_for_R_consistency_hierarchical.csv')

df_consistency <- df_consistency %>% 
  dplyr::rename(entropy = value)

agg_consistency <- df_consistency %>%
  summarize(mean = mean(entropy),
            sd = sd(entropy))

grand_mean_consistency <- agg_consistency$mean
grand_sd_consistency <- agg_consistency$sd

df_consistency$condition <- factor(df_consistency$condition, levels = c("mixed", "fine", "coarse"))
#contrasts(df_NMI$condition) <- contr.sum(2)
fit_consistency_hier <- brm(entropy ~ condition * hierarchy_level + (1|dataset),
                              data=df_consistency,
                              prior =
                                c(prior(uniform(0, 1), class = Intercept, lb = 0, ub = 1),
                                  prior(uniform(0, 0.1), class = sigma, lb = 0, ub = 0.1)),
                              # increasing adapt_delta because of few divergent transitions
                              # after warmup
                              control = list(adapt_delta = 0.99),
                              file = "LM_fit_consistency_hier"
)
summary(fit_consistency_hier)
describe_posterior(fit_consistency_hier, rope_range = rope_range(fit_consistency_hier))
pp_check(fit_consistency_hier)
# difference between fine and mixed
# significant difference between coarse and mixed
# significant effects of hierarchy level in fine and coarse condition