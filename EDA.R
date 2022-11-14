library(tidyverse)
library(lubridate)
library(viridis)
library(plotly)
library(fmsb)
library(GGally)
library(quantmod)

# Import Data
data <- read_csv('./Data/NIFTY ALL_15_01-01-2010_11-11-2022.csv')
data

data_close <- data %>%
  select(Date, name, Close) %>%
  spread(name, Close)
data_close

# set theme
my_theme <- theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank()) +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    legend.position = "bottom"
  )

# Plot All Index
p <- data %>%
  ggplot(aes(Date, Close, color=name)) +
  geom_line() +
  labs(
    x = 'Date',
    y = 'Close',
    color = NULL
  ) +
  my_theme  +
  guides(color = guide_legend(nrow = 2)) +
  scale_color_viridis(discrete = TRUE)
ggplotly(p)

# Radar Plots
radar_helper <- data_close %>%
  select(-Date) %>%
  map(~c(max(.x, na.rm = T), min(.x, na.rm = T))) %>%
  map_df(~.x)

radarchart_date <- function(date_string) {
  p <- radar_helper %>%
    full_join(
      data_close %>%
        filter(Date == ymd(date_string)) %>%
        select(-Date)
    ) %>%
    radarchart()
  return(p)
}

radarchart_date('20210312')

# Correlation Plots
data_close %>%
  select(-Date) %>%
  ggcorr(label = T) +
  theme_bw() +
  scale_fill_viridis()

# CandleStick
candlestick_index <- function(index_name) {
  p <- data %>%
    filter(name == index_name) %>%
    plot_ly(
      x = ~Date, type = 'candlestick',
      open = ~Open, close = ~Close,
      high = ~High, low = ~Low
    ) %>%
    rangeslider() %>%
    layout(
      title = index_name,
      yaxis = list(title = 'Index Value')
    )
  return(p)
}

candlestick_index('NIFTY 100')

