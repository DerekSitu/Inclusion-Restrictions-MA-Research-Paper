# -------------------------------------------------------------------------- #
# This script runs the difference-in-differences analyses and saves results.
# -------------------------------------------------------------------------- #


# Setup -------------------------------------------------------------------
# Load packages
pacman::p_load(tidyverse, here, janitor, fixest, texreg, broom, socviz,
               patchwork, car)

# Load data
data <- readRDS(here("data", "processed", "data.rds")) %>%
  filter(major == "Economics")

data_bachelor <- data %>%
  filter(level == "Bachelor's degree")

data_master <- data %>%
  filter(level == "Master's degree")

data_doctor <- data %>%
  filter(level == "Doctoral degree")


# TWFE --------------------------------------------------------------------

# Define the TWFE function
run_twfe_models <- function(data) {

  # Set up the model specifications based on the model type
  model_results <- list()

  # TWFE minorities with institution FE
  model_results[["minor_institution"]] <- feols(
    prop_minority ~ treat_post | as.factor(year) + institution,
    cluster = ~state,
    data = data)

  # TWFE minorities with state FE
  model_results[["minor_state"]] <- feols(
    prop_minority ~ treat_post | as.factor(year) + state,
    cluster = ~state,
    data = data)

  # TWFE women with institution FE
  model_results[["women_institution"]] <- feols(
    prop_women ~ treat_post | as.factor(year) + institution,
    cluster = ~state,
    data = data)

  # TWFE women with state FE
  model_results[["women_state"]] <- feols(
    prop_women ~ treat_post | as.factor(year) + state,
    cluster = ~state,
    data = data)

  # TWFE with top institution interaction for minorities
  model_results[["minor_top"]] <- feols(
    prop_minority ~ treat_post + top_institution + treat_post:top_institution |
      as.factor(year) + state,
    cluster = ~state,
    data = data)

  # TWFE with top institution interaction for women
  model_results[["women_top"]] <- feols(
    prop_women ~ treat_post + top_institution + treat_post:top_institution |
      as.factor(year) + state,
    cluster = ~state,
    data = data)

  # TWFE with red/blue state interaction (purple is base level) for minorities
  model_results[["minor_redblue"]] <- feols(
    prop_minority ~ treat_post + state_color + treat_post:state_color |
      as.factor(year),
    cluster = ~state,
    data = data)

  # TWFE with red/blue state interaction (purple is base level) for women
  model_results[["women_redblue"]] <- feols(
    prop_women ~ treat_post + state_color + treat_post:state_color |
      as.factor(year),
    cluster = ~state,
    data = data)

  return(model_results)
}

# Run models for each degree level
result_bachelor <- run_twfe_models(data_bachelor)
result_master <- run_twfe_models(data_master)
result_doctor <- run_twfe_models(data_doctor)


# Event Study Models ------------------------------------------------------

# Define the event study and Sun-Abraham model function
run_event_study_models <- function(data) {

  # Store results in a list for easy access
  model_results <- list()

  # Event study models
  # Event study for minorities
  model_results[["eventstudy_minor"]] <- feols(
    prop_minority ~ i(years_after_ban, ban_state, ref = 0) |
      as.factor(year) + state,
    cluster = ~state,
    data = data)

  # Event study for women
  model_results[["eventstudy_women"]] <- feols(
    prop_women ~ i(years_after_ban, ban_state, ref = 0) |
      as.factor(year) + state,
    cluster = ~state,
    data = data)

  # Sun-Abraham models (heterogeneity by state x time)
  # Sun-Abraham model for minorities
  model_results[["sunab_minor"]] <- feols(
    prop_minority ~ sunab(state, years_after_ban, ref.p = 0) |
      as.factor(year) + state,
    cluster = ~state,
    data = data)

  # Aggregating Sun-Abraham effects for minorities
  model_results[["sunab_minor_agg_time"]] <- aggregate(
    model_results[["sunab_minor"]],
    agg = "(ye.*an)::(-?[[:digit:]]+)")
  model_results[["sunab_minor_agg_cohort"]] <- aggregate(
    model_results[["sunab_minor"]],
    agg = "(cohort)::([[:alnum:]]+)")

  # Sun-Abraham model for women
  model_results[["sunab_women"]] <- feols(
    prop_women ~ sunab(state, years_after_ban, ref.p = 0) |
      as.factor(year) + state,
    cluster = ~state,
    data = data)

  # Aggregating Sun-Abraham effects for women
  model_results[["sunab_women_agg_time"]] <- aggregate(
    model_results[["sunab_women"]],
    agg = "(ye.*an)::(-?[[:digit:]]+)")
  model_results[["sunab_women_agg_cohort"]] <- aggregate(
    model_results[["sunab_women"]],
    agg = "(cohort)::([[:alnum:]]+)")

  return(model_results)
}

# Run event study models for each degree level
result_bachelor_es_sa <- run_event_study_models(data_bachelor)
result_master_es_sa <- run_event_study_models(data_master)
result_doctor_es_sa <- run_event_study_models(data_doctor)



# Generate tables ---------------------------------------------------------

# Function to generate TWFE table
twfe_table <- function(result, degree, file) {
  texreg(list(result$minor_institution,
              result$minor_state,
              result$women_institution,
              result$women_state),
         file = file,
         stars = c(0.001, 0.01, 0.05, 0.1),
         symbol = "+",
         custom.header = list("Minorities" = 1:2, "Women" = 3:4),
         digits = 3,
         caption =
           paste("Difference-in-Differences Results:", degree, "Degree",
                 sep = " "),
         caption.above = TRUE,
         booktabs = TRUE,
         custom.coef.names = c("Treat $\\times$ Post"),
         custom.gof.rows = list("Institution FE" = c("Yes", "No", "Yes", "No"),
                                "State FE" = c("No", "Yes", "No", "Yes")),
         custom.note = (paste("\\parbox{1\\linewidth}{\\vspace{2pt}%stars. \\\\
                      This table presents difference-in-differences estimates of the
                      treatment effect at the ", degree, " level.}",
                              sep = "")))
}

twfe_table(result_bachelor, "Bachelor's", "tables/table_a03_did_bachelor.tex")
twfe_table(result_master, "Master's", "tables/table_a04_did_master.tex")
twfe_table(result_doctor, "Doctorate", "tables/table_a05_did_doctor.tex")

# Bachelor's heterogeneity analysis
texreg(list(result_bachelor$minor_top,
            result_bachelor$women_top,
            result_bachelor$minor_redblue,
            result_bachelor$women_redblue),
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "+",
       custom.header = list("Top/Non-Top 75" = 1:2, "Red/Blue State" = 3:4),
       digits = 3,
       caption = "DiD Heterogeneity Analysis: Bachelor's Degree",
       caption.above = TRUE,
       booktabs = TRUE,
       custom.model.names = c("Minorities", "Women", "Minorities", "Women"),
       custom.gof.rows = list("Year FE" = c("Yes", "Yes", "Yes", "Yes"),
                              "State FE" = c("Yes", "Yes", "No", "No")),
       custom.coef.names = c("Treat $\\times$ Post", "Top Institution",
                             "Treat $\\times$ Post $\\times$ Top Institution",
                             "Red State", "Blue State",
                             "Treat $\\times$ Post $\\times$ Red State",
                             "Treat $\\times$ Post $\\times$ Blue State"),
       custom.note = ("\\parbox{1\\linewidth}{\\vspace{2pt}%stars. \\\\
                      This table presents difference-in-differences estimates of heterogenous
                      treatment effects at the bachelor's level for top 75
                      institutions vs. non-top 75 institutions (left 2 columns),
                      and red/blue/purple states (right 2 columns). For top vs.
                      non-top regressions, non-top serves as the baseline level,
                      and for red/blue/purple states, purple is the baseline
                      level.}"),
       file = "tables/table_a06_did_hetero_bachelor.tex")


# Generate plots ----------------------------------------------------------

# Function to generate event study plot
es_plot <- function(result, level) {
  result %>%
    tidy(conf.int = TRUE) %>%
    round_df(3) %>%
    # Remove strings "years_after_ban::" and ":ban_state" from term
    mutate(term = as.numeric(gsub("years_after_ban::|:ban_state", "", term))) %>%
    ggplot(aes(x = term,
               y = estimate,
               ymin = conf.low,
               ymax = conf.high)) +
    geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
    geom_vline(xintercept = case_when(level == "Bachelors" ~ 4,
                                      level == "Masters" ~ 1,
                                      level == "Doctorates" ~ 5),
               #linetype = "dashed",
               col = "blue",
               linetype = "dashed") +
    geom_pointrange(col = "black") +
    labs(x = "Years After Ban",
         y = "Estimate",
         title = level) +
    theme_minimal()
}

# Generate event study plots

# Fig 04: Event study plots for minorities
es_plot(result_bachelor_es_sa$eventstudy_minor, "Bachelors") /
  (es_plot(result_master_es_sa$eventstudy_minor, "Masters") |
     es_plot(result_doctor_es_sa$eventstudy_minor, "Doctorates"))

# Fig 05: Event study plots for women
es_plot(result_bachelor_es_sa$eventstudy_women, "Bachelors") /
  (es_plot(result_master_es_sa$eventstudy_women, "Masters") |
      es_plot(result_doctor_es_sa$eventstudy_women, "Doctorates"))


# Function to tidy Sun-Abraham results
tidy_sunab <- function(result) {
  data.frame(result) %>%
    rownames_to_column("state") %>%
    clean_names() %>%
    # Remove string "cohort::" from state
    mutate(state = gsub("cohort::", "", state),
           state = ifelse(state == "New", "New Hampshire", state),
           state = factor(state,
                          levels = state[order(estimate, decreasing = TRUE)]),
           conf.low = estimate - 1.96 * std_error,
           conf.high = estimate + 1.96 * std_error) %>%
    arrange(estimate)
}

tidy_minor <- tidy_sunab(result_bachelor_es_sa$sunab_minor_agg_cohort) %>%
  mutate(outcome = "Underrepresented Minorities")
tidy_women <- tidy_sunab(result_bachelor_es_sa$sunab_women_agg_cohort) %>%
  mutate(outcome = "Women")
tidied_sunab <- bind_rows(tidy_minor, tidy_women)

# # Figure A3: Sun-Ab bachelor's effects by state
tidied_sunab %>%
  ggplot(aes(x = state,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high)) +
  coord_flip() +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
  labs(x = "State",
       y = "Weighted Avg. Estimate") +
  theme_minimal() +
  facet_wrap(~outcome, scales = "free_y")


# Wald tests --------------------------------------------------------------

# Wald tests for linear combinations of coefficients for bachelor's degree
#   heterogeneity results

# Top 75 institutions - Minorities
linearHypothesis(result_bachelor$minor_top,
                 c("treat_post + treat_post:top_institution = 0"))

# Top 75 institutions - Women
linearHypothesis(result_bachelor$women_top,
                 c("treat_post + treat_post:top_institution = 0"))

# Red states - Minorities
linearHypothesis(result_bachelor$minor_redblue,
                 c("state_colorRed + treat_post:state_colorRed -
                   state_colorBlue - treat_post:state_colorBlue = 0"))

# Red states - Women
linearHypothesis(result_bachelor$women_redblue,
                 c("state_colorRed + treat_post:state_colorRed -
                   state_colorBlue - treat_post:state_colorBlue = 0"))



