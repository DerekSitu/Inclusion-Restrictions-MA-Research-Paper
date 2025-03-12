# -------------------------------------------------------------------------- #
# This script creates the pre-analysis figures.
# -------------------------------------------------------------------------- #
# Fig size: 812x552

# Load packages -----------------------------------------------------------
pacman::p_load(tidyverse, here, readxl, gridExtra)


# Load data ---------------------------------------------------------------
# Load main dataset
data_raw <- readRDS(here("data", "processed", "data.rds"))

# Load population race data
pop_race_raw <- read_excel(here("data", "raw", "pop_race.xlsx"),
                           sheet = "Filtered data")


# Clean data --------------------------------------------------------------
# Clean population race data
pop_race <- pop_race_raw %>%
  mutate(across(!year, ~ . / 100),
         aanhpi = asian + pacific_islander) %>%
  select(-c(asian, pacific_islander)) %>%
  rename_with(~ paste0("prop_", .), -year) %>%
  mutate(prop_minority =
           prop_black + prop_hispanic + prop_native) %>%
  pivot_longer(cols = starts_with("prop"),
               names_to = "type",
               values_to = "prop",
               names_prefix = "prop_") %>%
  mutate(type = case_when(type == "native" ~ "Native American",
                          type == "black" ~ "Black",
                          type == "hispanic" ~ "Hispanic",
                          type == "white" ~ "White",
                          type == "aanhpi" ~
                            "Asian/Native Hawaiian/Pacific Islander",
                          type == "minority" ~ "Underrepresented Minority"),
         level = NA,
         major = "Population")

# Clean main data
# Get national-level proportions by year, level, and major
data <- data_raw %>%
  ungroup() %>%
  group_by(institution_id, institution, year, level, major) %>%
  mutate(aanhpi3 = sum(aanhpi, aanhpi1, aanhpi2, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(year, level, major) %>%
  summarise(across(c("men", "women", "white", "black",
                     "hispanic", "native", "aanhpi3"),
                   sum,
                   na.rm = TRUE)) %>%
  mutate(prop_women = women / (women + men),
         prop_white = white / (white + black + hispanic + native + aanhpi3),
         prop_black = black / (white + black + hispanic + native + aanhpi3),
         prop_hispanic = hispanic /
           (white + black + hispanic + native + aanhpi3),
         prop_native = native / (white + black + hispanic + native + aanhpi3),
         prop_aanhpi = aanhpi3 /
           (white + black + hispanic + native + aanhpi3),
         prop_minority = prop_black + prop_hispanic + prop_native) %>%
  select(-c(men:aanhpi3)) %>%
  pivot_longer(cols = starts_with("prop_"),
               names_to = "type",
               values_to = "prop",
               names_prefix = "prop_") %>%
  mutate(type = case_when(type == "women" ~ "Women",
                          type == "white" ~ "White",
                          type == "black" ~ "Black",
                          type == "hispanic" ~ "Hispanic",
                          type == "native" ~ "Native American",
                          type == "aanhpi" ~
                            "Asian/Native Hawaiian/Pacific Islander",
                          type == "minority" ~ "Underrepresented Minority"))

# Merge main dataset with population data
merged_data <- bind_rows(data, pop_race) %>%
  mutate(major = factor(major,
                        levels = c("Population", "All subjects", "STEM",
                                   "Economics")),
         type = factor(type,
                       levels = c("Asian/Native Hawaiian/Pacific Islander",
                                  "White", "Black", "Hispanic",
                                  "Native American",
                                  "Underrepresented Minority")))


# Figures -----------------------------------------------------------------

# Figure 1
# Historic Representation of Women by Degree Level and Major
data %>%
  filter(type == "Women") %>%
  ggplot(aes(x = year, y = prop, color = major)) +
  geom_point(aes(shape = major)) +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(x = "Year",
       y = "Proportion",
       color = "Major",
       shape = "Major") +
  scale_color_manual(values = c("All subjects" = "orange",
                                "STEM" = "blue",
                                "Economics" = "red")) +
  # Change legend position to bottom
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~level)

# Figure 2, A1, A2
# Function to plot historic representation by race and major for given degree
#   level
# Arguments:
# lvl: Degree level. Options are "Bachelor's degree", "Master's degree", and
#   "Doctoral degree".
plot_representation <- function(lvl) {
  merged_data %>%
    filter(!(type == "Women"),
           level %in% c(NA, lvl)) %>%
    ggplot(aes(x = year, y = prop, color = major)) +
    geom_point(aes(shape = major)) +
    geom_line() +
    xlim(2000, 2023) +
    labs(#title = paste("Historic Representation in ",
                      # str_to_title(lvl), "s by Race/Ethnicity and Major",
                       #sep = ""),
         x = "Year",
         y = "Proportion",
         color = "Represented in",
         shape = "Represented in") +
    scale_color_manual(values = c("Population" = "black",
                                  "All subjects" = "orange",
                                  "STEM" = "blue",
                                  "Economics" = "red")) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    facet_wrap(vars(type), scales = "free_y")
}

# Figure 2
plot_representation("Bachelor's degree")

# Figure A1
plot_representation("Master's degree")

# Figure A2
plot_representation("Doctoral degree")
