# Import libraries
library(tidyverse)
library(lubridate)

# Import data
data_names <- strsplit(list.files('./Data'), '_')
index_names <- rep('', length(data_names))
for (i in 1:length(index_names)) {
  index_names[i] <- data_names[[i]][1]
}

data_paths <- paste0('./Data/', list.files('./Data'))

data <- data_paths %>%
  map(~ read_csv(.x))
data

# sample plot 
data[[26]] %>%
  filter(!is.na(Open)) %>%
  ggplot(aes(Date, Close)) +
  geom_line()

# combining all the data files
data_full <- data[[1]] %>%
  mutate(name=index_names[1])
for (i in 2:length(data)) {
  data_full <- data_full %>%
    full_join(data[[i]] %>% mutate(name=index_names[i]))
}

data_full <- data_full %>%
  arrange(Date) %>%
  select(Date, name, everything())
data_full  

# writing the data for use further
write_csv(data_full, './Data/NIFTY ALL_15_01-01-2010_11-11-2022.csv')
