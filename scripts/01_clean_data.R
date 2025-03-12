# -------------------------------------------------------------------------- #
# This script processes the raw data into a clean dataset for analysis and
# saves the clean data.
# -------------------------------------------------------------------------- #


# Load packages -----------------------------------------------------------
pacman::p_load(tidyverse, here, janitor, data.table)


# Load data ---------------------------------------------------------------
# Load states data
states_raw <- list.files(path = here("data", "raw", "states"),
                         pattern = "\\.csv$",
                         full.names = TRUE) %>%
  map_df(~fread(.))

# Load red/blue states data
red_blue_states_raw <- read_csv(here("data", "raw", "red_blue_states.csv"))

# Load bans data
bans <- read_csv(here("data", "raw", "bans.csv"))

# Function to read degrees data
read_degrees_data <- function(year) {
  list.files(path = here("data", "raw", "degrees", paste0(year, "_degrees")),
             pattern = "\\.csv$",
             full.names = TRUE) %>%
    map_df(~fread(., fill = TRUE))
}

years <- c(1996,1998:2023) # relevant years to load

# vector of names to assign to the dataframes
raw_degrees_data_names <- map_chr(years, ~paste0("degrees_", .x, "_raw"))

# Load degrees data
for (i in 1:length(years)) {
  assign(raw_degrees_data_names[i], read_degrees_data(years[i]))
}

# Load top institutions data
top_institutions <- read_csv(here("data", "raw", "top_institutions.csv"))


# Clean data --------------------------------------------------------------
# Clean states data
states <- states_raw %>%
  select(institution_id = unitid,
         institution = `institution name`,
         state = `HD2022.State abbreviation`)

# Clean red/blue states data
red_blue_states <- red_blue_states_raw %>%
  mutate(state_color = factor(state_color,
                              levels = c("Purple", "Red", "Blue")))

# Function to clean degrees data
clean_degrees_data <- function(data) {
  data %>%
    select(
      institution_id = unitid,
      institution = `institution name`,
      year,
      level = contains("Award"),
      major = CipTitle,
      men = contains(" men"),
      women = contains("women"),
      white = contains("White"),
      black = contains("Black"),
      hispanic = contains(".Hispanic"),
      native = contains("Indian"),
      aanhpi = contains("Asian") |
        contains("Pacific Islander") |
        contains("Native Hawaiian")) %>%
    filter(level %in% c(5, 7, 9, 17, 18, 19) |
             str_detect(level, "Bachelor's|Master's|Doctor's")) %>%
    mutate(level = ifelse(level == 5 |
                            str_detect(level, "Bachelor's"),
                          "Bachelor's degree",
                          ifelse(level == 7 |
                                   str_detect(level, "Master's"),
                                 "Master's degree",
                                 "Doctoral degree")),
           level = factor(level,
                          levels = c("Bachelor's degree",
                                     "Master's degree",
                                     "Doctoral degree")),
           major = case_when(str_detect(major, "Biological") ~ "STEM",
                             str_detect(major, "Engineering") ~ "STEM",
                             str_detect(major, "Mathematics") ~ "STEM",
                             str_detect(major, "Physical") ~ "STEM",
                             str_detect(tolower(major), "total") ~ "All subjects",
                             str_detect(major, "Economics") ~ "Economics",
                             str_detect(major, "Computer Science") ~ "STEM"),
           major = factor(major,
                          levels = c("All subjects", "STEM", "Economics"))) %>%
    # Add up the different types of doctoral degrees
    #   (professional, research, other) into one category.
    #   Also add up students with Econ as first major and second major
    group_by(institution_id, institution, year, level, major) %>%
    summarise_all(sum, na.rm = TRUE) %>%
    mutate(minority = black + hispanic + native,
           prop_minority = minority / (minority + white),
           prop_women = women / (women + men))
}

# Clean degrees data
for (i in 1:length(years)) {
  assign(paste0("degrees_", years[i]),
         clean_degrees_data(get(raw_degrees_data_names[i])))
}

# Define (1) a list of dataframes to bind, and
#   (2) a vector of names of dataframes to remove from memory
degrees_data_list <- map(years, ~get(paste0("degrees_", .x)))
degrees_data_names <- map_chr(years, ~paste0("degrees_", .x))

# Bind all degrees data
degrees <- bind_rows(degrees_data_list)

# Remove raw data and intermediate degrees data from memory
rm(list =
     c("states_raw", raw_degrees_data_names, degrees_data_names,
       "degrees_data_list", "degrees_data_names", "raw_degrees_data_names"))

# Merge states and bans data
states_w_bans <- left_join(states, bans, by = "state")

# Merge states/bans and degrees data
data <-
  left_join(degrees, states_w_bans,
            by = c("institution_id", "institution")) %>%
  mutate(ban_state = ifelse(is.na(ban_start),
                            0,
                            1),
         years_after_ban = ifelse(ban_state == 1,
                                  year - ban_start,
                                  -9999),
         treat_post = ifelse(ban_state == 1 & year >= ban_start,
                             1,
                             0),
         ban_start = ifelse(is.na(ban_start), -9999, ban_start)) %>%
  # -9999 is used instead of NA so that the regression does not drop
  # observations without bans. Does not affect estimates since the coefficient
  # is multipled by 0 (since ban_state = 0 in these cases) as intended.
  relocate(state, ban_state, ban_start, years_after_ban, treat_post,
           .after = institution)

# Add top institutions data
data <- data %>%
  mutate(top_institution = ifelse(institution %in% top_institutions$institution,
                                  1,
                                  0)) %>%
  relocate(top_institution, .after = institution)

# Merge red/blue states data
data <- data %>%
  left_join(red_blue_states, by = "state") %>%
  relocate(state_color, .after = state)


# Save data ---------------------------------------------------------------
saveRDS(data, here("data", "processed", "data.rds"))
write_csv(data, here("data", "processed", "data.csv"))
