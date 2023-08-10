library(tidyverse)

travel_time <- seq(0, 60, by=1)
median = 30
stdev = 5

logistic = 1.00 - cumsum(dlogis(travel_time, 
                             location = median, 
                             scale = stdev, log = FALSE))

tibble(`Travel time` = travel_time,
       `Weighted destination value` = logistic) |>
  ggplot() +
  geom_line(aes(x = `Travel time`,
                y = `Weighted destination value`)) +
  scale_x_continuous(breaks = seq(0, 60, by=10)) + 
  scale_y_continuous(breaks = breaks <- seq(0, 1, by= 0.1),
                     labels = paste0(breaks*100, "%"))


tibble(`Travel time` = c(0, 30, 30, 60),
       `Weighted destination value` = c(1, 1, 0, 0)) |>
  ggplot() +
  geom_line(aes(x = `Travel time`,
                y = `Weighted destination value`)) +
  scale_x_continuous(breaks = seq(0, 60, by=10)) + 
  scale_y_continuous(breaks = breaks <- seq(0, 1, by= 0.1),
                     labels = paste0(breaks*100, "%"))
