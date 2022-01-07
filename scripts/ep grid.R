# Create grid of EP at 1st & 10 from each yard line
library(tidyverse)
library(nflfastR)

pbp_data <- tibble(season = 2019, home_team = "LA", posteam = "LA", roof = NA, half_seconds_remaining = 1800,  yardline_100 = 1:99, down = 1, ydstogo = 10, posteam_timeouts_remaining = 3, defteam_timeouts_remaining = 3)
ep_calc <- calculate_expected_points(pbp_data) %>%
  select(yardline_100, ep)
ep_df <- tibble(yardline_100 = 0, ep = 6.95) %>%
  bind_rows(ep_calc) %>%
  bind_rows(tibble(yardline_100 = 100, ep = -6.95))
ep_grid <- ep_df$ep
write.table(ep_grid, "ep_grid.csv")
