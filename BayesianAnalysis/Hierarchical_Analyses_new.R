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
agg_NMI <- df_NMI %>%   
  summarize(mean = mean(entropy),
            sd = sd(entropy))

grand_mean_NMI <- agg_NMI$mean
grand_sd_NMI <- agg_NMI$sd

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