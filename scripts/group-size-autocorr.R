
# ---- set-up-env ---------------------------------------------------------

# Load packages and make connections to babase
Sys.setenv(TZ = 'UTC')
list.of.packages <- list("devtools", "viridis", "tidyverse", "stringr",
                         "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(unlist(new.packages))
lapply(list.of.packages, require, character.only = T)

# Check if ramboseli already installed
if (!("ramboseli" %in% installed.packages()[,"Package"])) {
  devtools::install_github("amboseli/ramboseli")
}
library(ramboseli)

# This is a test comment!


# ---- database-connections-hidden ----------------------------------------

# These are hidden from the Rmarkdown output
# Make connection to database
babase <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                         host = "localhost",
                         port = 2222,
                         user = "fac13",
                         dbname = "babase")

# Remote data sources in babase
members <- tbl(babase, "members")
biograph <- tbl(babase, "biograph")
maturedates <- tbl(babase, "maturedates")
parents <- tbl(babase, "parents")
maternities <- tbl(babase, "maternities")
pregs <- tbl(babase, "pregs")
statuses <- tbl(babase, "statuses")

# Local copy of members for using in joins
members_local <- collect(members)


# ---- database-connections-general ---------------------------------------

# You will need to change user to your personal babase login AND get your password
# One approach to doing that is through Rstudio
# You could also just type it in your script, but that's not recommended for security.
babase <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                         host = "localhost",
                         port = 2222,
                         user = "YOUR_BABASE_USERNAME",
                         dbname = "babase",
                         password = rstudioapi::askForPassword("Database password"))

# Remote data sources in babase
members <- tbl(babase, "members")
biograph <- tbl(babase, "biograph")
maturedates <- tbl(babase, "maturedates")
parents <- tbl(babase, "parents")
maternities <- tbl(babase, "maternities")
pregs <- tbl(babase, "pregs")
statuses <- tbl(babase, "statuses")

# Local copy of members for using in joins
members_local <- collect(members)


# ---- find-kids ----------------------------------------------------------

# Find all live-born offspring with known mother
kids <- biograph %>%
  inner_join(select(parents, kid, mom), by = c("sname" = "kid")) %>%
  left_join(select(pregs, pid, parity), by = "pid") %>%
  inner_join(select(biograph, mom = sname, mom_dob = birth), by = "mom") %>%
  rename(kid = sname) %>%
  filter(sex == "F") %>%
  collect()

# Calculate age at depart
kids <- kids %>%
  mutate(age_depart = interval(birth, statdate) %/% days(1) / 365.25)

# Function to create a sequence of ages (in years) for each individual
# From 0(birth) to last birthday before departure
get_age_seq <- function(df) {
  return(seq(0, floor(df$age_depart)))
}

# As above, but in monthly time steps
get_age_seq_months <- function(df) {
  return(seq(0, floor(df$age_depart * 12)))
}

# Create a data frame for holding these sequences of ages
# For each kid, apply the get_age_seq functions and store the results
age_seq <- kids %>%
  group_by(kid) %>%
  nest() %>%
  mutate(age_seq = map(data, get_age_seq),
         age_seq_months = map(data, get_age_seq_months)) %>%
  select(kid, age_seq_months) %>%
  unnest()

# Expand kids to create row for each census age for that individual in age_seq
kid_seq <- kids %>%
  inner_join(select(age_seq, kid, age_months = age_seq_months), by = "kid")

kid_seq$census_day <- kid_seq$birth %m+% months(kid_seq$age_months)


# ---- match-grp-size -----------------------------------------------------

# Find group on birthday
kid_seq <- kid_seq %>%
  left_join(select(members_local, kid = sname, census_day = date, current_grp = grp),
            by = c("kid", "census_day"))

# Find all members present on census_day
members_on_census <- kid_seq %>%
  left_join(members_local, by = c("current_grp" = "grp", "census_day" = "date"))

# Count members on census day
n_on_census <- members_on_census %>%
  mutate(age = age_months / 12) %>%
  group_by(kid, census_day, sex, age, age_months, current_grp) %>%
  summarise(n = n())

# Get group size on date of birth
n_on_bdate <- n_on_census %>%
  ungroup() %>%
  filter(age_months == 0) %>%
  select(kid, n_on_bdate = n)

# Join to n_on_census --> adds n_on_bdate column
n_on_census <- n_on_census %>%
  inner_join(n_on_bdate, by = "kid")

# Change all counts for group 9 to NA, since we don't really know
n_on_census$n <- ifelse(n_on_census$current_grp == 9, NA, n_on_census$n)

# Calculate incremental and total change in group size, time diff
# grp_increment = from previous census to current census
# grp_change = from birth to current census
# time_diff = time elapsed from previous census to current census
n_on_census <- n_on_census %>%
  ungroup() %>%
  arrange(kid, age_months) %>%
  group_by(kid) %>%
  mutate(grp_increment = n - lag(n),
         grp_change = n - n_on_bdate,
         time_diff = census_day - lag(census_day))

# Define group size categories in increments of 10
n_on_census$bdate_grp_size <- cut(n_on_census$n_on_bdate,
                                  breaks = seq(0, 120, 10))


# ---- save-data ----------------------------------------------------------

# Save data for future use
# saveRDS(n_on_census, "scripts/early_adversity/n_on_census.RDS")
# n_on_census <- readRDS("scripts/early_adversity/n_on_census.RDS")


# ---- plot_experienced_group_size ----------------------------------------

ggplot() +
  geom_line(data = n_on_census,
            aes(x = age, y = grp_change, group = kid),
            alpha = 0.15) +
  geom_hline(yintercept = 0) +
  stat_smooth(data = n_on_census,
              aes(x = age, y = grp_change),
              color = "firebrick", fill = "firebrick", size = 0.5) +
  facet_wrap(~bdate_grp_size) +
  theme_journal_x2() +
  labs(x = "Age", y = "Change in Group Size",
       title = "Experienced change in group size",
       subtitle = "Categories of birth group sizes")



# ---- group-size-acf -----------------------------------------------------

# Make nested df for applying acf function to each kid
grp_size_acf <- n_on_census %>%
  ungroup() %>%
  group_by(kid, bdate_grp_size) %>%
  nest()

# Function to apply R's acf function on df
# Only returns the vector of acf values
get_acf <- function(df) {
  res <- acf(df$n, plot = FALSE, na.action = na.pass, lag.max = 120)$acf[,,1]
}

# Apply the function to each nested df, store result in new column called acf
grp_size_acf <- grp_size_acf %>%
  mutate(acf = map(data, get_acf))

# Drop the column called data, and unnest
# Also add a row number, which is the lag (in months)
grp_size_acf <- grp_size_acf %>%
  select(kid, bdate_grp_size, acf) %>%
  unnest() %>%
  group_by(kid) %>%
  mutate(lag = row_number() - 1)

# Summarize, with set sizes and mean
grp_size_acf_summary <- grp_size_acf %>%
  group_by(bdate_grp_size, lag) %>%
  mutate(set_size = n(),
         mean = mean(acf, na.rm = TRUE))


# ---- plot-group-size-acf ------------------------------------------------

ggplot() +
  geom_line(data = grp_size_acf,
            aes(x = lag / 12, y = acf, group = kid),
            size = 0.1, alpha = 0.25, color = "gray50") +
  geom_hline(yintercept = 0) +
  geom_line(data = grp_size_acf_summary,
            aes(x = lag / 12, y = mean, group = bdate_grp_size,
                color = bdate_grp_size), size = 1) +
  scale_color_manual(values = viridis::viridis(12, option = "plasma"),
                     name = "Group Size at Birth") +
  # facet_wrap(~bdate_grp_size) +
  theme_journal_x2() +
  labs(x = "Lag (years)", y = "Autocorrelation",
       title = "Autocorrelation of experienced group size")






# ---- group_size_time_series ---------------------------------------------

grp_members <- make_date_cols(members_local, date)
grp_members <- grp_members %>%
  group_by(grp, year_of, month_of) %>%
  summarise(n = n()) %>%
  filter(grp < 3)

gm <- grp_members %>%
  ungroup() %>%
  group_by(grp) %>%
  nest()

gm <- gm %>%
  mutate(acf = map(data, get_acf))

# Critical values
ci <- 0.95
crit <- qnorm((1 + ci)/2)

gm2 <- gm %>%
  mutate(n_used = map_dbl(data, nrow)) %>%
  select(grp, acf, n_used) %>%
  unnest() %>%
  group_by(grp) %>%
  mutate(lag = row_number(),
         lower = -crit / sqrt(n_used),
         upper = crit / sqrt(n_used))

ggplot(gm2) +
  geom_segment(aes(x = lag / 12, xend = lag / 12, y = 0, yend = acf),
               alpha = 0.5) +
  geom_ribbon(aes(x = lag / 12, ymin = lower, ymax = upper, group = grp),
              fill = "firebrick",
              alpha = 0.5) +
  geom_hline(yintercept = 0) +
  facet_wrap(~grp, ncol = 4) +
  coord_cartesian(ylim = c(-1, 1)) +
  theme_journal() +
  labs(x = "Lag (years)", y = "ACF",
       title = "ACF of group size for each group")

gm2$lag_jitter <- jitter(gm2$lag, amount = 1)

ggplot(gm2) +
  geom_segment(aes(x = lag / 12, xend = lag / 12,
                   y = 0, yend = acf),
               alpha = 0.25) +
  geom_ribbon(aes(x = lag / 12, ymin = lower, ymax = upper, group = grp),
              fill = "firebrick",
              alpha = 0.1) +
  geom_hline(yintercept = 0) +
  # facet_wrap(~grp, ncol = 4) +
  coord_cartesian(ylim = c(-1, 1)) +
  theme_journal() +
  labs(x = "Lag (years)", y = "ACF",
       title = "ACF of group size for each group")

gm2$lag_fct <- factor(gm2$lag / 12)

ggplot(gm2) +
  geom_boxplot(aes(x = lag_fct, y = acf), outlier.shape = NA) +
  scale_x_discrete(breaks = as.character(0:10)) +
  geom_hline(yintercept = 0) +
  theme_journal_x2() +
  labs(x = "Lag (years)", y = "ACF",
       title = "Range of ACFs for each lag")

gm3 <- gm2 %>%
  ungroup() %>%
  group_by(lag) %>%
  summarise(mean_acf = mean(acf),
            set_size = n()) %>%
  mutate(lower = -crit / sqrt(set_size),
         upper = crit / sqrt(set_size),
         mean = mean(mean_acf, na.rm = TRUE))

ggplot(gm3) +
  geom_segment(aes(x = lag / 12, xend = lag / 12, y = 0, yend = mean_acf)) +
  geom_hline(yintercept = 0) +
  theme_journal() +
  labs(x = "Lag (years)", y = "ACF",
       title = "ACF for each lag averaged across groups")

grp_members$date_of <- ymd(paste(grp_members$year_of, grp_members$month_of, "15", sep = "-"))

ggplot(grp_members, aes(x = date_of, y = n, group = grp)) +
  geom_line() +
  theme_journal_x2()


# What is the maximum age?
biograph %>%
  filter(status == 1) %>%
  mutate(age_at_death = interval(birth, statdate) %/% days(1) / 365.25) %>%
  select(sname, age_at_death) %>%
  arrange(-age_at_death)
