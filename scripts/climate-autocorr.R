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


# ---- database-connections-hidden ----------------------------------------

# These are hidden from the Rmarkdown output
# Make connection to database
babase <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                         host = "localhost",
                         port = 2222,
                         user = "fac13",
                         dbname = "babase")

# Pull rainfall data from babase
wreadings <- tbl(babase, "wreadings") %>% collect()
raingauges <- tbl(babase, "raingauges") %>% collect()


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

# Pull rainfall data from babase
wreadings <- tbl(babase, "wreadings") %>% collect()
raingauges <- tbl(babase, "raingauges") %>% collect()


# ---- prep-rainfall-data -------------------------------------------------

# Join the tables
rain <- inner_join(wreadings, raingauges, by = "wrid")

# Create date columns
rain <- make_date_cols(rain, wrdaytime)
rain$date_of <- as.Date(rain$wrdaytime)

# There is one weird case of two rainfall measurements taken on the same day
# 2010-11-05
# Add all measurements taken on same date
rain <- rain %>%
  group_by(year_of, month_of, date_of) %>%
  summarise(rain = sum(rain)) %>%
  ungroup()


# ---- monthly-rainfall ---------------------------------------------------

# Calculate monthly totals
rain_monthly <- rain %>%
  group_by(year_of, month_of) %>%
  summarise(rain_monthly = sum(rain)) %>%
  ungroup()

# Calculate monthly long-term means
rain_ltm <- rain_monthly %>%
  group_by(month_of) %>%
  summarise(rain_ltm = mean(rain_monthly)) %>%
  ungroup()

# Calculate monthly rainfall anomaly by subtracting long-term mean
rain_monthly <- rain_monthly %>%
  inner_join(rain_ltm, by = "month_of") %>%
  mutate(rain_anomaly = rain_monthly - rain_ltm)

# Create dummy date for each month (~middle of month)
rain_monthly <- rain_monthly %>%
  mutate(date_of = ymd(paste(year_of, month_of, "16", sep = "-")))


# ---- annual-rainfall ----------------------------------------------------

# Calculate annual rainfall total
rain_annual <- rain %>%
  group_by(year_of) %>%
  summarise(rain_annual = sum(rain))


# ---- rain-monthly-acf ---------------------------------------------------

rain_monthly_acf <- acf(rain_monthly$rain_monthly, lag.max = 240, plot = FALSE)
rain_monthly_acf <- data_frame(lag = rain_monthly_acf$lag[,,1],
                               acf = rain_monthly_acf$acf[,,1])

ci <- 0.99
crit <- qnorm((1 + ci)/2)

rain_monthly_acf$sig <- abs(rain_monthly_acf$acf) > crit/sqrt(nrow(rain_monthly))

ggplot(rain_monthly_acf) +
  # geom_segment(aes(x = lag / 12, xend = lag / 12, y = acf, yend = 0),
  #              size = 0.25) +
  # geom_point(aes(x = lag / 12, y = acf),
  #            shape = 21, color = "white", size = 0.75) +
  geom_segment(aes(x = lag / 12, xend = lag / 12, y = acf, yend = 0, color = sig),
               size = 0.5) +
  geom_point(aes(x = lag / 12, y = acf, fill = sig),
             shape = 21, color = "white", size = 1.25) +
  geom_ribbon(aes(x = lag / 12, ymin = crit/sqrt(nrow(rain_monthly)),
                  ymax = -crit/sqrt(nrow(rain_monthly))),
              fill = "firebrick", alpha = 0.2) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-0.27, 1), xlim = c(0, 10)) +
  scale_y_continuous(breaks = seq(-0.2, 1, by = 0.2)) +
  scale_fill_manual(values = c("black", "firebrick3"), guide = FALSE) +
  scale_color_manual(values = c("black", "firebrick3"), guide = FALSE) +
  theme_journal_x2() +
  labs(x = "Lag (years)", y = "Autocorrelation function",
       title = "ACF of Monthly Rainfall")


# ---- rain-anomaly-acf ---------------------------------------------------

rain_anomaly_acf <- acf(rain_monthly$rain_anomaly, lag.max = 240, plot = FALSE)
rain_anomaly_acf <- data_frame(lag = rain_anomaly_acf$lag[,,1],
                       acf = rain_anomaly_acf$acf[,,1])

rain_anomaly_acf$sig <- abs(rain_anomaly_acf$acf) > crit/sqrt(nrow(rain_monthly))

ggplot(rain_anomaly_acf) +
  geom_segment(aes(x = lag / 12, xend = lag / 12, y = acf, yend = 0, color = sig),
               size = 0.5) +
  geom_point(aes(x = lag / 12, y = acf, fill = sig),
             shape = 21, color = "white", size = 1.25) +
  coord_cartesian(ylim = c(-0.27, 1), xlim = c(0, 10)) +
  scale_y_continuous(breaks = seq(-0.2, 1, by = 0.2)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(x = lag / 12, ymin = crit/sqrt(nrow(rain_monthly)),
                  ymax = -crit/sqrt(nrow(rain_monthly))),
              fill = "firebrick", alpha = 0.2) +
  scale_fill_manual(values = c("black", "firebrick3"), guide = FALSE) +
  scale_color_manual(values = c("black", "firebrick3"), guide = FALSE) +
  theme_journal_x2() +
  labs(x = "Lag (years)", y = "Autocorrelation function",
       title = "ACF of Monthly Rainfall Anomaly")


# ---- rain-annual-acf ----------------------------------------------------

rain_annual_acf <- acf(rain_annual$rain_annual, lag.max = 30, plot = FALSE)

rain_annual_acf <- data_frame(lag = rain_annual_acf$lag[,,1],
                              acf = rain_annual_acf$acf[,,1])

rain_annual_acf$sig <- abs(rain_annual_acf$acf) > crit/sqrt(nrow(rain_annual))

ggplot(rain_annual_acf) +
  geom_segment(aes(x = lag, xend = lag, y = acf, yend = 0, color = sig),
               size = 0.75) +
  geom_point(aes(x = lag, y = acf, fill = sig),
             shape = 21, color = "white", size = 2, stroke = 0.75) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(x = lag, ymin = crit/sqrt(nrow(rain_annual)),
                  ymax = -crit/sqrt(nrow(rain_annual))),
              fill = "firebrick", alpha = 0.2) +
  scale_fill_manual(values = c("black", "firebrick3"), guide = FALSE) +
  scale_color_manual(values = c("black", "firebrick3"), guide = FALSE) +
  theme_journal_x2() +
  labs(x = "Lag (years)", y = "Autocorrelation function",
       title = "ACF of Total Annual Rainfall")
