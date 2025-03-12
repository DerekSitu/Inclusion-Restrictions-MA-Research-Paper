# -------------------------------------------------------------------------- #
# This script runs the regression discontinuity analyses and saves results.
# -------------------------------------------------------------------------- #


# Setup -------------------------------------------------------------------

# Load packages
pacman::p_load(tidyverse, here, janitor, fixest, rddtools, rdrobust, rdd,
               rddensity, modelsummary, patchwork)


# Load data
data <- readRDS(here("data", "processed", "data.rds")) %>%
  filter(major == "Economics")

# Main datasets

data_bachelor <- data %>%
  filter(ban_state == 1,
         level == "Bachelor's degree")

data_master <- data %>%
  filter(ban_state == 1,
         level == "Master's degree")

data_doctor <- data %>%
  filter(ban_state == 1,
         level == "Doctoral degree")


# Heterogeneity datasets

data_bachelor_top <- data_bachelor %>%
  filter(top_institution == 1)

data_bachelor_notop <- data_bachelor %>%
  filter(top_institution == 0)

data_bachelor_red <- data_bachelor %>%
  filter(state_color == "Red")

data_bachelor_blue <- data_bachelor %>%
  filter(state_color == "Blue")

data_master_top <- data_master %>%
  filter(top_institution == 1)

data_master_notop <- data_master %>%
  filter(top_institution == 0)

data_master_red <- data_master %>%
  filter(state_color == "Red")

data_master_blue <- data_master %>%
  filter(state_color == "Blue")

data_doctor_top <- data_doctor %>%
  filter(top_institution == 1)

data_doctor_notop <- data_doctor %>%
  filter(top_institution == 0)

data_doctor_red <- data_doctor %>%
  filter(state_color == "Red")

data_doctor_blue <- data_doctor %>%
  filter(state_color == "Blue")


# RD Plots ----------------------------------------------------------------
# Fig size: 812x552

# Uncomment desired dataset
attach(data_bachelor)
# attach(data_bachelor_top)
# attach(data_bachelor_notop)
# attach(data_bachelor_red)
# attach(data_bachelor_blue)

# Bachelor's, minorities
plot_bachelor_minority <-
  rdplot(y = prop_minority,
         x = years_after_ban,
         c = 4,
         p = 1,
         x.label = "Years after ban",
         y.label = "Proportion of degrees awarded",
         title = "Bachelor's, Minorities",
         kernel = "epanechnikov")

# Bachelor's, women
plot_bachelor_women <-
  rdplot(y = prop_women,
         x = years_after_ban,
         c = 4,
         p = 1,
         x.label = "Years after ban",
         y.label = "Proportion of degrees awarded",
         title = "Bachelor's, Women",
         kernel = "epanechnikov")

# Uncomment desired dataset
attach(data_master)
# attach(data_master_top)
# attach(data_master_notop)
# attach(data_master_red)
# attach(data_master_blue)

# Master's, minorities
plot_master_minority <-
  rdplot(y = prop_minority,
         x = years_after_ban,
         c = 1,
         p = 1,
         x.label = "Years after ban",
         y.label = "Proportion of degrees awarded",
         title = "Master's, Minorities",
         kernel = "epanechnikov")

# Master's, women
plot_master_women <-
  rdplot(y = prop_women,
         x = years_after_ban,
         c = 1,
         p = 1,
         x.label = "Years after ban",
         y.label = "Proportion of degrees awarded",
         title = "Master's, Women",
         kernel = "epanechnikov")

# Uncomment desired dataset
attach(data_doctor)
# attach(data_doctor_top)
# attach(data_doctor_notop)
# attach(data_doctor_red)
# attach(data_doctor_blue)

# Doctoral, minorities
plot_doctor_minority <-
  rdplot(y = prop_minority,
         x = years_after_ban,
         c = 5,
         p = 1,
         x.label = "Years after ban",
         y.label = "Proportion of degrees awarded",
         title = "Doctor's, Minorities",
         kernel = "epanechnikov")

# Doctoral, women
plot_doctor_women <-
  rdplot(y = prop_women,
         x = years_after_ban,
         c = 5,
         p = 1,
         x.label = "Years after ban",
         y.label = "Proportion of degrees awarded",
         title = "Doctor's, Women",
         kernel = "epanechnikov")

# Generate plot with sub-plots arranged in a grid
plot_bachelor_minority$rdplot + plot_bachelor_women$rdplot +
  plot_master_minority$rdplot + plot_master_women$rdplot +
  plot_doctor_minority$rdplot + plot_doctor_women$rdplot

# Remove plots from memory to save space
rm(plot_bachelor_minority, plot_bachelor_women,
   plot_master_minority, plot_master_women,
   plot_doctor_minority, plot_doctor_women)

# Main RD Models -----------------------------------------------------------

# Helper functions from
#   https://gist.github.com/vincentarelbundock/5ee12d715d59266beb81ff654d2227d5
tidy.rdrobust <- function(object, ...){
  ret <- data.frame(term = row.names(object$coef),
                    estimate = object$coef[, 1],
                    std.error = object$se[, 1],
                    statistic = object$z[, 1],
                    p.value = object$pv[, 1],
                    conf.low = object$ci[,1],
                    conf.high = object$ci[, 2])
  row.names(ret) <- NULL
  ret
}

glance.rdrobust <- function(object, ...){
  ret <- data.frame(nobs.left = object$N[1],
                    nobs.right = object$N[2],
                    nobs.effective.left = object$N_h[1],
                    nobs.effective.right = object$N_h[2],
                    cutoff = object$c,
                    order.regression = object$p,
                    order.bias = object$q,
                    kernel = object$kernel,
                    bwselect = object$bwselect)
  ret
}

# Bachelor's models
attach(data_bachelor)

rd_bachelor_minor <- rdrobust(y = prop_minority,
                              x = years_after_ban,
                              c = 4,
                              covs = cbind(as.factor(institution),
                                           as.factor(year)),
                              cluster = state,
                              kernel = "epanechnikov")

rd_bachelor_women <- rdrobust(y = prop_women,
                              x = years_after_ban,
                              c = 4,
                              covs = cbind(as.factor(institution),
                                           as.factor(year)),
                              cluster = state,
                              kernel = "epanechnikov")

# Master's models
attach(data_master)

rd_master_minor <- rdrobust(y = prop_minority,
                            x = years_after_ban,
                            c = 1,
                            covs = cbind(as.factor(institution),
                                         as.factor(year)),
                            cluster = state,
                            kernel = "epanechnikov")

rd_master_women <- rdrobust(y = prop_women,
                            x = years_after_ban,
                            c = 1,
                            covs = cbind(as.factor(institution),
                                         as.factor(year)),
                            cluster = state,
                            kernel = "epanechnikov")

# Doctoral models
attach(data_doctor)

rd_doctor_minor <- rdrobust(y = prop_minority,
                            x = years_after_ban,
                            c = 5,
                            covs = cbind(as.factor(institution),
                                         as.factor(year)),
                            cluster = state,
                            kernel = "epanechnikov")

rd_doctor_women <- rdrobust(y = prop_women,
                            x = years_after_ban,
                            c = 5,
                            covs = cbind(as.factor(institution),
                                         as.factor(year)),
                            cluster = state,
                            kernel = "epanechnikov")

# Generate LaTeX tables

# Goodness-of-fit settings
gof <- data.frame(
  raw = c("nobs.left", "nobs.right", "nobs.effective.left",
          "nobs.effective.right", "cutoff"),
  clean = c("Obs. Left", "Obs. Right",
            "Effective Obs. Left", "Effective Obs. Right",
            "Cutoff"),
  fmt = c(0, 0, 0, 0, 0))

# Minorities RD table
modelsummary(list("Bachelors" = rd_bachelor_minor,
                  "Masters" = rd_master_minor,
                  "Doctorates" = rd_doctor_minor),
             stars = TRUE,
             fmt = list("cutoff" = 0),
             title =
               "Regression Discontinuity Results: Underrepresented Minorities",
             gof_map = gof,
             output = "tables/table_a01_rd_minorities.tex",
             notes =
               ("This table presents regression discontinuity estimates
                with proportion of degrees awarded to underrepresented minorities
                as the outcome. Bias-corrected estimates use second-order polynomial
                to approximate curvatures near the cutoff. Heteroskedasticity-robust
                standard errors are also reported for bias-corrected estimates."))

# Women RD table
modelsummary(list("Bachelors" = rd_bachelor_women,
                  "Masters" = rd_master_women,
                  "Doctorates" = rd_doctor_women),
             stars = TRUE,
             fmt = list("cutoff" = 0),
             title = "Regression Discontinuity Results: Women",
             gof_map = gof,
             output = "tables/table_a02_rd_women.tex",
             notes =
               ("This table presents regression discontinuity estimates
                with proportion of degrees awarded to women
                as the outcome. Bias-corrected estimates use second-order polynomial
                to approximate curvatures near the cutoff. Heteroskedasticity-robust
                standard errors are also reported for bias-corrected estimates."))

# Heterogeneity Analysis --------------------------------------------------

# Bachelor's heterogeneity models

attach(data_bachelor_top)

rd_bachelor_minor_top <- rdrobust(y = prop_minority,
                                  x = years_after_ban,
                                  c = 4,
                                  covs = cbind(as.factor(year)),
                                  cluster = state,
                                  kernel = "epanechnikov")

rd_bachelor_women_top <- rdrobust(y = prop_women,
                                  x = years_after_ban,
                                  c = 4,
                                  covs = cbind(as.factor(year)),
                                  cluster = state,
                                  kernel = "epanechnikov")


attach(data_bachelor_notop)

rd_bachelor_minor_notop <- rdrobust(y = prop_minority,
                                    x = years_after_ban,
                                    c = 4,
                                    covs = cbind(as.factor(year)),
                                    cluster = state,
                                    kernel = "epanechnikov")

rd_bachelor_women_notop <- rdrobust(y = prop_women,
                                    x = years_after_ban,
                                    c = 4,
                                    covs = cbind(as.factor(year)),
                                    cluster = state,
                                    kernel = "epanechnikov")

attach(data_bachelor_blue)

rd_bachelor_minor_blue <- rdrobust(y = prop_minority,
                                   x = years_after_ban,
                                   c = 4,
                                   covs = cbind(as.factor(year)),
                                   cluster = state,
                                   kernel = "epanechnikov")

rd_bachelor_women_blue <- rdrobust(y = prop_women,
                                   x = years_after_ban,
                                   c = 4,
                                   covs = cbind(as.factor(year)),
                                   cluster = state,
                                   kernel = "epanechnikov")


attach(data_bachelor_red)

rd_bachelor_minor_red <- rdrobust(y = prop_minority,
                                  x = years_after_ban,
                                  c = 4,
                                  covs = cbind(as.factor(year)),
                                  cluster = state,
                                  kernel = "epanechnikov")

rd_bachelor_women_red <- rdrobust(y = prop_women,
                                  x = years_after_ban,
                                  c = 4,
                                  covs = cbind(as.factor(year)),
                                  cluster = state,
                                  kernel = "epanechnikov")

# Generate LaTeX tables

# Top/No top RD table
modelsummary(list("T75, Minorities" = rd_bachelor_minor_top,
                  "Non-T75, Minorities" = rd_bachelor_minor_notop,
                  "T75, Women" = rd_bachelor_women_top,
                  "Non-T75, Women" = rd_bachelor_women_notop),
             stars = TRUE,
             fmt = list("cutoff" = 0),
             title =
               "RD Results: Top 75 & Non-Top 75 Institutions, Bachelor's Degree",
             gof_map = gof,
             output = "tables/table_a07_rd_top75_bachelors.tex",
             notes =
               ("This table presents regression discontinuity estimates
                for top 75 institutions and non-top 75 instutitions separately.
                Bias-corrected estimates use second-order polynomial
                to approximate curvatures near the cutoff. Heteroskedasticity-robust
                standard errors are also reported for bias-corrected estimates."))

# Blue/Red RD table
modelsummary(list("Blue, Minorities" = rd_bachelor_minor_blue,
                  "Red, Minorities" = rd_bachelor_minor_red,
                  "Blue, Women" = rd_bachelor_women_blue,
                  "Red, Women" = rd_bachelor_women_red),
             stars = TRUE,
             fmt = list("cutoff" = 0),
             title = "RD Results: Blue & Red States, Bachelor's Degree",
             gof_map = gof,
             output = "tables/table_a08_rd_redblue_bachelors.tex",
             notes =
               ("This table presents regression discontinuity estimates
                for blue states and red states separately.
                Bias-corrected estimates use second-order polynomial
                to approximate curvatures near the cutoff. Heteroskedasticity-robust
                standard errors are also reported for bias-corrected estimates."))
