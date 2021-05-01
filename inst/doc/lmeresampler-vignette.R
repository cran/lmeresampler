## ----init, include = FALSE----------------------------------------------------
is_check <- ("CheckExEnv" %in% search()) || any(c("_R_CHECK_TIMINGS_",
             "_R_CHECK_LICENSE_") %in% names(Sys.getenv()))

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = !is_check,
  fig.height = 3, fig.width = 5
)

## ----setup--------------------------------------------------------------------
library(lmeresampler)

## ----fit models, results = FALSE, message = FALSE-----------------------------
library(lme4)
vcmodA <- lme4::lmer(mathAge11 ~ mathAge8 + gender + class + (1 | school), data = jsp728)

library(nlme)
vcmodB <- nlme::lme(mathAge11 ~ mathAge8 + gender + class, random = ~1|school, data = jsp728)

## ----parameteric example------------------------------------------------------
# let's set .f = fixef to specify that we want only the fixed effects bootstrapped

# lme4
lmer_par_boot <- bootstrap(vcmodA, .f = fixef, type = "parametric", B = 100)

# nlme
lme_par_boot  <- bootstrap(vcmodB, .f = fixef, type = "parametric", B = 100)

## -----------------------------------------------------------------------------
names(lmer_par_boot)

## ----cases example------------------------------------------------------------
# lme4
# resample the schools, but not the students
lmer_case_boot <- bootstrap(vcmodA, .f = fixef, type = "case", B = 100, resample = c(TRUE, FALSE))

# nlme
# do not resample the schools, but resample the students
lme_cases_boot1 <- bootstrap(vcmodB, .f = fixef, type = "case", B = 100, resample = c(FALSE, TRUE))

# nlme
# resample both the schools and the students
lme_cases_boot2 <- bootstrap(vcmodB, .f = fixef, type = "case", B = 100, resample = c(TRUE, TRUE))

## ----residual example---------------------------------------------------------
# lme4
lmer_res_boot <- bootstrap(vcmodA, .f = fixef, type = "residual", B = 100)

# nlme
lme_res_boot <- bootstrap(vcmodB, .f = fixef, type = "residual", B = 100)

## ----wild examples------------------------------------------------------------
# lme4
lmer_wild_boot <- bootstrap(vcmodA, .f = fixef, type = "wild", hccme = "hc2", aux.dist = "f1", B = 100)
lmer_wild_boot <- bootstrap(vcmodA, .f = fixef, type = "wild", hccme = "hc3", aux.dist = "f2", B = 100)

# nlme
lme_wild_boot <- bootstrap(vcmodB, .f = fixef, type = "wild", hccme = "hc2", aux.dist = "f2", B = 100)
lme_wild_boot <- bootstrap(vcmodB, .f = fixef, type = "wild", hccme = "hc3", aux.dist = "f1", B = 100)

## ----REB example, message = FALSE, warning = FALSE----------------------------
# lme4
lmer_reb_boot0 <- bootstrap(vcmodA, .f = fixef, type = "reb", B = 100, reb_type = 0)
lmer_reb_boot1 <- bootstrap(vcmodA, .f = fixef, type = "reb", B = 100, reb_type = 1)

# nlme
lme_reb_boot2  <- bootstrap(vcmodB, .f = fixef, type = "reb", B = 100, reb_type = 2)

## ----summary method, warning = FALSE, message = FALSE-------------------------
summary(lmer_reb_boot1)

## ----print method, warning = FALSE, message = FALSE---------------------------
print(lmer_reb_boot0)

print(lme_reb_boot2, ci = TRUE) 

## ----confints, message = FALSE, warning = FALSE-------------------------------
# all ci types, 0.95 confidence level
confint(lme_reb_boot2)

# 90% percentile bootstrap intervals
confint(lmer_reb_boot0, method = "perc", level= 0.90)

## ----message = FALSE, warning = FALSE-----------------------------------------
plot(lmer_par_boot) # defaults to printing first variable

plot(lmer_par_boot, "mathAge8") # plot mathAge8 variable

plot(lmer_par_boot, 1) # plots first variable

## ----parallel example, message = FALSE, warning = FALSE-----------------------
library(purrr)
library(foreach)
library(doParallel)
set.seed(1234)
numCores <- 2

cl <- makeCluster(numCores, type = "PSOCK") # make a socket cluster
doParallel::registerDoParallel(cl)          # how the CPU knows to run in parallel

b_parallel2 <- foreach(B = rep(250, 2), 
                       .combine = combine_lmeresamp,
                       .packages = c("lmeresampler", "lme4")) %dopar% {
                         bootstrap(vcmodA, .f = fixef, type = "parametric", B = B)
                       }

stopCluster(cl)

## ----timings, message = FALSE, warning = FALSE--------------------------------
system.time(b_nopar  <- bootstrap(vcmodA, .f = fixef, type = "parametric", B = 5000))

numCores <- 2
cl <- makeCluster(numCores, type = "PSOCK")
doParallel::registerDoParallel(cl) 

system.time({
  b_parallel2 <- foreach(B = rep(2500, 2), 
                         .combine = combine_lmeresamp,
                         .packages = c("lmeresampler", "lme4", "purrr", "dplyr")) %dopar% {
                           bootstrap(vcmodA, .f = fixef, type = "parametric", B = B)
                         }
})

stopCluster(cl) 

