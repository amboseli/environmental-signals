rain_selected <- read_csv("data/rain_selected.csv")

site_map <- c(
  "rppn-fma" = "Muriqui",
  "amboseli" = "Baboon",
  "kakamega" = "Blue Monkey",
  "gombe" = "Chimpanzee",
  "karisoke" = "Gorilla",
  "beza" = "Sifaka",
  "ssr" = "Capuchin"
)

tmp <- rain_monthly %>%
  add_column(site = "amboseli") %>%
  rename(rain = rain_monthly) %>%
  select(site, year_of, month_of, rain, date_of)

rain_selected <- filter(rain_selected, site != "amboseli")

rain_selected <- bind_rows(rain_selected, tmp)

rain_selected$site <- plyr::revalue(as.character(rain_selected$site), site_map)
rain_selected$site <- factor(rain_selected$site,
                             levels = c("Sifaka", "Capuchin", "Muriqui", "Blue Monkey", "Baboon",
                                        "Chimpanzee", "Gorilla"))

temp <- rain_selected %>%
  arrange(site, year_of, date_of) %>%
  group_by(site) %>%
  nest() %>%
  mutate(rain_acf = map(data, ~ acf(.$rain, lag.max = 120, plot = FALSE)),
         lag = map(rain_acf, pluck("lag")),
         acf = map(rain_acf, pluck("acf"))) %>%
  unnest(lag, acf)

ci <- 0.99
crit <- qnorm((1 + ci)/2)

temp <- temp %>%
  group_by(site) %>%
  mutate(n = n(),
         sig = abs(acf) > crit / sqrt(n))

ggplot(temp) +
  geom_segment(aes(x = lag / 12, xend = lag / 12, y = acf, yend = 0, color = sig),
               size = 0.5) +
  geom_point(aes(x = lag / 12, y = acf, fill = sig),
             shape = 21, color = "white", size = 1.25) +
  geom_ribbon(aes(x = lag / 12, ymin = crit / sqrt(n),
                  ymax = -crit / sqrt(n)),
              fill = "firebrick", alpha = 0.2) +
  geom_hline(yintercept = 0) +
  facet_wrap(~site, ncol = 2, scales = "free_x") +
  coord_cartesian(xlim = c(0, 10), ylim = c(-0.61, 1)) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  # scale_y_continuous(breaks = seq(-0.2, 1, by = 0.2)) +
  scale_fill_manual(values = c("black", "firebrick3"), guide = FALSE) +
  scale_color_manual(values = c("black", "firebrick3"), guide = FALSE) +
  theme_minimal() +
  labs(x = "Lag (years)", y = "Autocorrelation function",
       title = "ACF of Monthly Rainfall")


# Calculate monthly long-term means
rain_selected_ltm <- rain_selected %>%
  group_by(site, month_of) %>%
  summarise(rain_ltm = mean(rain)) %>%
  ungroup()

# Calculate monthly rainfall anomaly by subtracting long-term mean
rain_selected <- rain_selected %>%
  inner_join(rain_selected_ltm, by = c("site", "month_of")) %>%
  mutate(rain_anomaly = rain - rain_ltm)

temp <- rain_selected %>%
  arrange(site, year_of, date_of) %>%
  group_by(site) %>%
  nest() %>%
  mutate(rain_acf = map(data, ~ acf(.$rain_anomaly, lag.max = 120, plot = FALSE)),
         lag = map(rain_acf, pluck("lag")),
         acf = map(rain_acf, pluck("acf"))) %>%
  unnest(lag, acf)

ci <- 0.99
crit <- qnorm((1 + ci)/2)

temp <- temp %>%
  group_by(site) %>%
  mutate(n = n(),
         sig = abs(acf) > crit / sqrt(n))

ggplot(temp) +
  geom_segment(aes(x = lag / 12, xend = lag / 12, y = acf, yend = 0, color = sig),
               size = 0.5) +
  geom_point(aes(x = lag / 12, y = acf, fill = sig),
             shape = 21, color = "white", size = 1.25) +
  geom_ribbon(aes(x = lag / 12, ymin = crit / sqrt(n),
                  ymax = -crit / sqrt(n)),
              fill = "firebrick", alpha = 0.2) +
  geom_hline(yintercept = 0) +
  facet_wrap(~site, ncol = 2, scales = "free_x") +
  coord_cartesian(xlim = c(0, 10), ylim = c(-0.61, 1)) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  # scale_y_continuous(breaks = seq(-0.2, 1, by = 0.2)) +
  scale_fill_manual(values = c("black", "firebrick3"), guide = FALSE) +
  scale_color_manual(values = c("black", "firebrick3"), guide = FALSE) +
  theme_minimal() +
  labs(x = "Lag (years)", y = "Autocorrelation function",
       title = "ACF of Monthly Rainfall Anomaly")



rain_annual <- rain_selected %>%
  group_by(site, year_of) %>%
  summarise(rain_annual = sum(rain),
            n = n()) %>%
  filter(n == 12)

temp <- rain_annual %>%
  arrange(site, year_of) %>%
  group_by(site) %>%
  nest() %>%
  mutate(rain_acf = map(data, ~ pacf(.$rain_annual, lag.max = 20, plot = FALSE)),
         lag = map(rain_acf, pluck("lag")),
         acf = map(rain_acf, pluck("acf"))) %>%
  unnest(lag, acf)

ci <- 0.99
crit <- qnorm((1 + ci)/2)

temp <- temp %>%
  group_by(site) %>%
  mutate(n = n(),
         sig = abs(acf) > crit / sqrt(n))

ggplot(temp) +
  geom_segment(aes(x = lag, xend = lag, y = acf, yend = 0, color = sig),
               size = 0.5) +
  geom_point(aes(x = lag, y = acf, fill = sig),
             shape = 21, color = "white", size = 1.25) +
  geom_ribbon(aes(x = lag, ymin = crit / sqrt(n),
                  ymax = -crit / sqrt(n)),
              fill = "firebrick", alpha = 0.2) +
  geom_hline(yintercept = 0) +
  facet_wrap(~site, ncol = 2, scales = "free_x") +
  coord_cartesian(xlim = c(0, 20), ylim = c(-0.61, 1)) +
  scale_x_continuous(breaks = seq(0, 20, by = 1)) +
  # scale_y_continuous(breaks = seq(-0.2, 1, by = 0.2)) +
  scale_fill_manual(values = c("black", "firebrick3"), guide = FALSE) +
  scale_color_manual(values = c("black", "firebrick3"), guide = FALSE) +
  theme_minimal() +
  labs(x = "Lag (years)", y = "Autocorrelation function",
       title = "ACF of Annual Rainfall")
