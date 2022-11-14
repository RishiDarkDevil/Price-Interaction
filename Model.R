fit <- lm(`NIFTY AUTO` ~ `NIFTY BANK`, data = data_close)
summary(fit)

data_close %>%
  ggplot(aes(`NIFTY AUTO`, `NIFTY BANK`)) +
  geom_point()
