## Loading necessary packages 
library(tidyverse)      # loads all tidyverse packages
library(haven)          # reads foreign data sources (e.g., Stata)
library(lme4)           # random effects models
library(lavaan)         # latent variable analysis
library(GGally)         # correlation matrix
options(scipen = 999)   # no scientific notation


# demograhpics
demographics <- read.csv("data/phdcn_neighborhoods/demographics/data/export/ltdb_nc_factors.csv") %>%
  dplyr::select(nc_num, year, FAC_disadv, FAC_hispimm, FAC_stability)

# read phdcn data (community survey) from the CSI hard drive
phdcn <- read_dta("/Volumes/TS_CSIGroup/CSI Projects/PHDCN/Data/PHDCN Community Survey/CS_PHDCN.dta")

#  read phdcn data (community survey, scales) from the CSI hard drive
phdcn_cs <- read_dta("/Volumes/TS_CSIGroup/CSI Projects/PHDCN/Data/PHDCN Community Survey/ncscales_original.dta")

# wd back to RProj
setwd("~/Dropbox/Papers/English/Working papers/measuring neighb")

###############################################

## measuring moral cynicism (sampson and bartusch)

phdcn <- 
  phdcn %>%
  dplyr::select(RC_NUM, NC_NUM,
                moral1 = Q41A, moral2 = Q41B, moral3 = Q41C, moral4 = Q41D, moral5 = Q41F) %>%
  mutate(across(c(moral1:moral5), na_if, -96)) %>%
  # making sure higher scores = more cynicism
  mutate(across(c(moral1:moral5), ~case_when(
    . == 5 ~ 1,
    . == 4 ~ 2,
    . == 3 ~ 3,
    . == 2 ~ 4,
    . == 1 ~ 5,
    TRUE ~ as.numeric(NA)
  )))

#####################
## 1) HLM approach ##
#####################

phdcn_raudenbush <-
  phdcn %>%
  left_join(demographics %>% 
              rename(NC_NUM = nc_num) %>% 
              filter(year == 1990)
            ) %>%
  dplyr::select(RC_NUM, NC_NUM, moral1:moral5, FAC_disadv, FAC_hispimm, FAC_stability) %>%
  pivot_longer(cols = moral1:moral5)

# HLM with neighb covariates
three_level_moral <-
  lmer(value ~ FAC_disadv + FAC_hispimm + FAC_stability + (1 | NC_NUM / RC_NUM), phdcn_raudenbush)

# HLM without neighb covariates
three_level_moral_nocov <-
  lmer(value ~ (1 | NC_NUM / RC_NUM), phdcn_raudenbush)

############################
## two-level CFA approach ##
############################

# 2-level CFA with covariates
cfa_moral <-
  '
  level: 1
  moral_cynicism_CFA =~ moral1 + moral2 + moral3 + moral4 + moral5
  
  level: 2
  moral_cynicism_CFA =~ moral1 + moral2 + moral3 + moral4 + moral5
  moral_cynicism_CFA ~ FAC_disadv + FAC_hispimm + FAC_stability
  ' %>%
  sem(phdcn %>%
        left_join(demographics %>% 
                    rename(NC_NUM = nc_num) %>% 
                    filter(year == 1990)
                  ), 
      cluster = "NC_NUM", estimator = "MLR", std.lv = T, #missing = "ML", 
      optim.method = "em", em.iter.max = 20000,
      em.fx.tol = 1e-08, em.dx.tol = 1e-04)

cfa_moral_nocov <-
  '
  level: 1
  moral_cynicism_CFA_nocov =~ moral1 + moral2 + moral3 + moral4 + moral5
  
  level: 2
  moral_cynicism_CFA_nocov =~ moral1 + moral2 + moral3 + moral4 + moral5
  ' %>%
  sem(phdcn %>%
        filter(!is.na(NC_NUM)) %>%
        left_join(demographics %>% 
                    rename(NC_NUM = nc_num) %>% 
                    filter(year == 1990)
                  ),
      cluster = "NC_NUM", estimator = "MLR", std.lv = T, #missing = "ML", 
      optim.method = "em", em.iter.max = 20000,
      em.fx.tol = 1e-08, em.dx.tol = 1e-04)



############################
neighb_measures <- 
  demographics %>%
  filter(year == 1990) %>%
  dplyr::select(NC_NUM = nc_num) %>%
  mutate(morcyn_HLM_cond = ranef(three_level_moral)$NC_NUM$`(Intercept)`
         , morcyn_HLM_uncond = ranef(three_level_moral_nocov)$NC_NUM$`(Intercept)`
         , morcyn_CFA_cond = lavPredict(cfa_moral, level = 2)[,'moral_cynicism_CFA']
         , morcyn_CFA_uncond = lavPredict(cfa_moral_nocov, level = 2)[,'moral_cynicism_CFA_nocov']
  ) %>%
  left_join(phdcn_cs %>% 
              dplyr::select(NC_NUM, ANOMIE, EBANOMIE))

###################################################

# correlation matrix
pdf("plots/correlation_matrix.pdf", width = 10, height = 10, paper = 'a4r')
neighb_measures %>% dplyr::select(-NC_NUM) %>% ggpairs
dev.off()
