wreadings <- tbl(babase, "wreadings") %>% collect()
raingauges <- tbl(babase, "raingauges") %>% collect()

rain <- inner_join(wreadings, raingauges)

rain <- make_date_cols(rain, wrdaytime)
rain$date_of <- as.Date(rain$wrdaytime)

rain <- rain %>%
  group_by(year_of, month_of, date_of) %>%
  summarise(rain = sum(rain))

rain_monthly <- rain %>%
  ungroup() %>%
  group_by(year_of, month_of) %>%
  summarise(rain_monthly = sum(rain))

rain_ltm <- rain_monthly %>%
  ungroup() %>%
  group_by(month_of) %>%
  summarise(rain_ltm = mean(rain_monthly))

rain_monthly <- rain_monthly %>%
  ungroup() %>%
  inner_join(rain_ltm, by = "month_of") %>%
  mutate(rain_anomaly = rain_monthly - rain_ltm)

rain_annual <- rain %>%
  group_by(year_of) %>%
  summarise(rain_annual = sum(rain))

rain_monthly <- rain_monthly %>%
  mutate(date_of = ymd(paste(year_of, month_of, "01", sep = "-")),
         chr_date = strftime(date_of, format = "%d/%m/%Y"))

rain_acf <- acf(rain_monthly$rain_monthly, lag.max = 240, plot = FALSE)
rain_acf <- data_frame(lag = rain_acf$lag[,,1],
                       acf = rain_acf$acf[,,1])

ci <- 0.99
crit <- qnorm((1 + ci)/2)

ggplot(rain_acf) +
  geom_segment(aes(x = lag / 12, xend = lag / 12, y = acf, yend = 0), size = 0.25) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(x = lag / 12, ymin = crit/sqrt(nrow(rain_monthly)),
                  ymax = -crit/sqrt(nrow(rain_monthly))),
              fill = "firebrick", alpha = 0.2) +
  theme_journal_x2() +
  labs(x = "Lag (years)", y = "Autocorrelation function",
       title = "ACF of Monthly Rainfall")

rain_acf <- acf(rain_monthly$rain_anomaly, lag.max = 240, plot = FALSE)
rain_acf <- data_frame(lag = rain_acf$lag[,,1],
                       acf = rain_acf$acf[,,1])

ggplot(rain_acf) +
  geom_segment(aes(x = lag / 12, xend = lag / 12, y = acf, yend = 0), size = 0.25) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(x = lag / 12, ymin = crit/sqrt(nrow(rain_monthly)),
                  ymax = -crit/sqrt(nrow(rain_monthly))),
              fill = "firebrick", alpha = 0.2) +
  theme_journal_x2() +
  labs(x = "Lag (years)", y = "Autocorrelation function",
       title = "ACF of Monthly Rainfall Anomaly")


rain_annual_acf <- acf(rain_annual$rain_annual, lag.max = 30, plot = FALSE)

rain_annual_acf <- data_frame(lag = rain_annual_acf$lag[,,1],
                              acf = rain_annual_acf$acf[,,1])

ggplot(rain_annual_acf) +
  geom_segment(aes(x = lag, xend = lag, y = acf, yend = 0), size = 0.25) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(x = lag, ymin = crit/sqrt(nrow(rain_annual)),
                  ymax = -crit/sqrt(nrow(rain_annual))),
              fill = "firebrick", alpha = 0.2) +
  theme_journal_x2() +
  labs(x = "Lag (years)", y = "Autocorrelation function",
       title = "ACF of Total Annual Rainfall")
