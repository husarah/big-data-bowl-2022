# field control -----------------------------------------------------------
library(gganimate)
library(magick)

#adapted from Adam Sonty's Kaggle 
#https://www.kaggle.com/adamsonty/nfl-big-data-bowl-a-basic-field-control-model/notebook


#compute rotation and scaling matrix for covariance matrix
#direction player is moving
compute_rotation_matrix <- function(v_theta) {
  R <- matrix(
    c(cos(v_theta), -sin(v_theta),
      sin(v_theta),  cos(v_theta)),
    nrow = 2,
    byrow = TRUE
  )
  return(R)
}
#player speed ratio and distance from ball
compute_scaling_matrix <- function(radius_of_influence, s_ratio) {
  S <- matrix(
    c(radius_of_influence * (1 + s_ratio), 0,
      0, radius_of_influence * (1 - s_ratio)),
    nrow = 2,
    byrow = TRUE
  )
  return(S)
}
#controls shape of distribution
compute_covariance_matrix <- function(v_theta, radius_of_influence, s_ratio) {
  R <- compute_rotation_matrix(v_theta)
  S <- compute_scaling_matrix(radius_of_influence, s_ratio)
  Sigma <- R %*% S %*% S %*% solve(R)
  return(Sigma)
}
#2D normal distribution ZOI
compute_player_zoi <- function(player_frame_tracking_data, field_grid = NULL) {
  if(is.null(field_grid)) {
    field_grid <- expand_grid(
      x = seq(0, 120, length.out = 120),
      y = seq(0, 160/3, length.out = 160/3)
    )
  }
  #retrieve variables
  playId_ <- player_frame_tracking_data %>% pull(playId)
  frameId_ <- player_frame_tracking_data %>% pull(frameId)
  gameId_ <- player_frame_tracking_data %>% pull(gameId)
  displayName_ <- player_frame_tracking_data %>% pull(displayName) 
  jerseyNumber_ <- player_frame_tracking_data %>% pull(jerseyNumber) 
  team_name_ <- player_frame_tracking_data %>% pull(team_name) 
  homeTeamAbbr_ <- player_frame_tracking_data %>% pull(homeTeamAbbr) 
  visitorTeamAbbr_ <- player_frame_tracking_data %>% pull(visitorTeamAbbr) 
  zoi_center_x_ <- player_frame_tracking_data %>% pull(x_next)
  zoi_center_y_ <- player_frame_tracking_data %>% pull(y_next)
  v_theta_ <- player_frame_tracking_data %>% pull(v_theta)
  radius_of_influence_ <- player_frame_tracking_data %>% pull(radius_of_influence)
  s_ratio_ <- player_frame_tracking_data %>% pull(s_ratio)
  mu <- c(zoi_center_x_, zoi_center_y_)
  Sigma <- compute_covariance_matrix(v_theta_, radius_of_influence_, s_ratio_)
  player_zoi <- field_grid %>%
    dplyr::mutate(
      influence = mvtnorm::dmvnorm(x = field_grid, mean = mu, sigma = Sigma),
      influence = influence / max(influence),
      playId = playId_,
      frameId = frameId_,
      gameId = gameId_,
      displayName = displayName_,
      jerseyNumber = jerseyNumber_,
      team_name = team_name_,
      homeTeamAbbr = homeTeamAbbr_,
      visitorTeamAbbr = visitorTeamAbbr_
    )
  return(player_zoi)
}

#for every location of field compute control values
compute_team_frame_control <- function(frame_tracking_data) {
  team_frame_control <- frame_tracking_data %>%
    dplyr::filter(team != "football") %>%
    dplyr::group_split(displayName) %>%
    purrr::map_dfr(., compute_player_zoi) %>%
    dplyr::mutate(
      influence = dplyr::case_when(
        team_name == homeTeamAbbr ~ -1 * influence,
        TRUE ~ influence
      )
    )  %>%
    dplyr::group_by(gameId, playId, frameId, x, y) %>%
    dplyr::summarise(control = sum(influence), .groups = "keep") %>%
    dplyr::mutate(control = 1 / (1 + exp(control))) #which team has control based on 0.5 threshold
  return(team_frame_control)
}

ball_by_frame <- df_tracking %>%
  group_by(gameId, playId, frameId) %>%
  summarise(xFootball = mean(xFootball), 
            yFootball = mean(yFootball))

returner_by_frame <- df_tracking %>%
  group_by(gameId, playId, frameId) %>%
  filter(returner_p == T) %>%
  summarise(dirReturner = mean(dir),
            oReturner = mean(o),
            xReturner = mean(x),
            yReturner = mean(y))

normalizeFun <- function(x) {
  return ((x - max(x)) / (min(x) - max(x)))
}

df_control <- df_tracking %>%
  filter(gameId == 2020100410, 
         playId == 127) %>%
  dplyr::filter(team != "football") %>%
  dplyr::group_split(gameId, frameId, playId) %>%
  purrr::map_dfr(., compute_team_frame_control) %>%
  ungroup() %>%
  left_join(ball_by_frame, by = c('frameId', 'playId')) %>%
  left_join(returner_by_frame, by = c('frameId', 'playId')) %>%
  mutate(disFromBall = sqrt((x - xFootball)^2 + (y - yFootball)^2),
         disFromReturner = sqrt((x - xReturner)^2 + (y - yReturner)^2),
         # make adjustments to direction and orientation as arctan limited from -90 to 90
         dirXY = atan((y - yReturner) / (x - xReturner)) * 180/pi,
         dirChange = abs(dirXY - dirReturner),
         oXY = atan((y - yReturner) / (x - xReturner)) * 180/pi,
         oChange = abs(oXY - oReturner),
         disFromBallNorm = disFromBall,
         disFromReturnerNorm = disFromReturner,
         dirChangeNorm = dirChange,
         oChangeNorm = oChange) %>%
  mutate_at(c("disFromBallNorm", "disFromReturnerNorm", 
              "dirChangeNorm", 'oChangeNorm'), ~(normalizeFun(.x))) %>%
  mutate(controlAdj = control * disFromReturnerNorm * 
           dirChangeNorm * oChangeNorm)

df_control_play = df_control %>%
  group_by(gameId, playId, frameId) %>%
  summarise(frameControl = sum(controlAdj))

# field control animation ---------------------------------------------------------------

# visualize play and control
# also adapted from Adam Sonty's kaggle
# https://www.kaggle.com/adamsonty/nfl-big-data-bowl-a-basic-field-control-model

# plot image settings
options(repr.plot.width=20, repr.plot.height = 10)


# read in play data
play_ <- plays %>%
  filter(gameId ==2020100410, playId == 127)
game_ <- games %>%
  filter(gameId == 2020100410)

df_track <- tracking_all %>%
  dplyr::filter(gameId == play_$gameId, playId == play_$playId)
df_track_2 <- df_tracking %>%
  dplyr::filter(gameId == play_$gameId, playId == play_$playId)

play_direction_ <- df_track %>% head(1) %>% dplyr::pull(playDirection)

df_track_2 <- df_track_2 %>%
  left_join(ball_by_frame, on = c('gameId', 'playId', 'frameId')) %>%
  dplyr::select(frameId, event, team = team_name, jerseyNumber, displayName, x, y, s, v_theta, v_x, v_y,
                xFootball, yFootball) 

#change tracking data
df_track <- df_track %>%
  dplyr::mutate(
    dir_rad = dir * pi / 180,
    v_x = sin(dir_rad) * s,
    v_y = cos(dir_rad) * s,
    v_theta = atan(v_y / v_x),
    v_theta = ifelse(is.nan(v_theta), 0, v_theta),
    team_name = case_when(
      team == "home" ~ game_$homeTeamAbbr,
      team == "away" ~ game_$visitorTeamAbbr,
      TRUE ~ team,
    )
  ) %>%
  dplyr::select(frameId, event, team = team_name, jerseyNumber, displayName, x, y, s, v_theta, v_x, v_y)

# field plot
plot_field <- function(field_color="#ffffff", line_color = "#212529", number_color = "#adb5bd") {
  field_height <- 160/3
  field_width <- 120
  
  field <- ggplot() +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 13, hjust = 0.5),
      plot.subtitle = element_text(hjust = 1),
      legend.position = "bottom",
      legend.title.align = 1,
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.line = element_blank(),
      panel.background = element_rect(fill = field_color, color = "white"),
      panel.border = element_blank(),
      aspect.ratio = field_height/field_width
    ) +
    # major lines
    annotate(
      "segment",
      x = c(0, 0, 0,field_width, seq(10, 110, by=5)),
      xend = c(field_width,field_width, 0, field_width, seq(10, 110, by=5)),
      y = c(0, field_height, 0, 0, rep(0, 21)),
      yend = c(0, field_height, field_height, field_height, rep(field_height, 21)),
      colour = line_color
    ) +
    # hashmarks
    annotate(
      "segment",
      x = rep(seq(10, 110, by=1), 4),
      xend = rep(seq(10, 110, by=1), 4),
      y = c(rep(0, 101), rep(field_height-1, 101), rep(160/6 + 18.5/6, 101), rep(160/6 - 18.5/6, 101)),
      yend = c(rep(1, 101), rep(field_height, 101), rep(160/6 + 18.5/6 + 1, 101), rep(160/6 - 18.5/6 - 1, 101)),
      colour = line_color
    ) +
    # yard numbers
    annotate(
      "text",
      x = seq(20, 100, by = 10),
      y = rep(12, 9),
      label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))),
      size = 10,
      colour = number_color,
    ) +
    # yard numbers upside down
    annotate(
      "text",
      x = seq(20, 100, by = 10),
      y = rep(field_height-12, 9),
      label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))),
      angle = 180,
      size = 10,
      colour = number_color, 
    )
  
  return(field)
}


fetch_team_colors <- function(team_colors_=NULL, h_team_, a_team_, diverge_=FALSE) {
  colors_url <- "https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/data/nfl_team_colors.tsv"
  
  if (is.null(team_colors_)) {
    team_colors_ <- suppressMessages(readr::read_tsv(colors_url))
  }
  
  h_team_color1 <- team_colors_ %>% filter(teams == h_team_) %>% pull(color1)
  h_team_color2 <- team_colors_ %>% filter(teams == h_team_) %>% pull(color2)
  a_team_color1 <- team_colors_ %>% filter(teams == a_team_) %>% pull(color1)
  a_team_color2 <- team_colors_ %>% filter(teams == a_team_) %>% pull(color2)
  
  if (diverge_ == TRUE) {
    h_team_color1_family <- team_colors_ %>% filter(teams == h_team_) %>% select(color1_family) %>% pull()
    a_team_color1_family <- team_colors_ %>% filter(teams == a_team_) %>% select(color1_family) %>% pull()
    
    if (h_team_color1_family == a_team_color1_family) {
      a_team_color1 <- team_colors_ %>% filter(teams == a_team_) %>% select(color2) %>% pull()
      a_team_color2 <- team_colors_ %>% filter(teams == a_team_) %>% select(color1) %>% pull()
    }
  }
  
  df_colors <- tibble(
    home_1 = h_team_color1, home_2 = h_team_color2, away_1 = a_team_color1, away_2 = a_team_color2
  )
  
  
  return(df_colors)
}

#if (play_direction_ == "right") {
#  line_of_scrimmage = play_$absoluteYardlineNumber
#  to_go_line = line_of_scrimmage + play_$yardsToGo
#} else {
#  line_of_scrimmage = 100 - play_$absoluteYardlineNumber
#  to_go_line = line_of_scrimmage - play_$yardsToGo
#}
line_of_scrimmage = 32
to_go_line = 51

df_colors <- fetch_team_colors(h_team_ = game_$homeTeamAbbr, a_team_ = game_$visitorTeamAbbr, diverge_ = T)

play_frames <- plot_field() + 
  #control
  geom_raster(
    data = df_control, 
    mapping = aes(x = x, y = y, fill = control), alpha = 0.7, interpolate = T
  ) +
  scale_fill_gradient2(
    low = df_colors$away_1, high = df_colors$home_1, mid = "white", midpoint = 0.5, 
    name = "Team Field Control", limits = c(0,1), breaks = c(0, 1), labels = c(game_$visitorTeamAbbr, game_$homeTeamAbbr)
  ) +
  # line of scrimmage
  annotate(
    "segment",
    x = line_of_scrimmage, xend = line_of_scrimmage, y = 0, yend = 160/3,
    colour = "#0d41e1", size = 1.5
  ) +
  # 1st down marker
  annotate(
    "segment",
    x = to_go_line, xend = to_go_line, y = 0, yend = 160/3,
    colour = "#f9c80e", size = 1.5
  ) +
  # away team velocities
  geom_segment(
    data = df_track_2 %>% dplyr::filter(team == game_$visitorTeamAbbr),
    mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y),
    colour = df_colors$away_1, size = 1, arrow = arrow(length = unit(0.01, "npc"))
  ) + 
  # home team velocities
  geom_segment(
    data = df_track_2 %>% dplyr::filter(team == game_$homeTeamAbbr),
    mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y),
    colour = df_colors$home_2, size = 1, arrow = arrow(length = unit(0.01, "npc"))
  ) +
  # away team locs and jersey numbers
  geom_point(
    data = df_track_2 %>% dplyr::filter(team == game_$visitorTeamAbbr),
    mapping = aes(x = x, y = y),
    fill = "#f8f9fa", colour = df_colors$away_2,
    shape = 21, alpha = 1, size = 8, stroke = 1.5
  ) +
  geom_text(
    data = df_track_2 %>% dplyr::filter(team == game_$visitorTeamAbbr),
    mapping = aes(x = x, y = y, label = jerseyNumber),
    colour = df_colors$away_1, size = 4.5
  ) +
  # home team locs and jersey numbers
  geom_point(
    data = df_track_2 %>% dplyr::filter(team == game_$homeTeamAbbr),
    mapping = aes(x = x, y = y),
    fill = df_colors$home_1, colour = df_colors$home_2,
    shape = 21, alpha = 1, size = 8, stroke = 1.5
  ) +
  geom_text(
    data = df_track_2 %>% dplyr::filter(team == game_$homeTeamAbbr),
    mapping = aes(x = x, y = y, label = jerseyNumber),
    colour = df_colors$home_2, size = 4.5, 
  ) +
  # ball
  geom_point(
    data = df_track_2 %>% dplyr::filter(),
    mapping = aes(x = xFootball, y = yFootball),
    fill = "#935e38", colour = "#d9d9d9",
    shape = 21, alpha = 1, size = 4, stroke = 1
  ) +
  # title 
  labs(title = play_$playDescription) +
  # animation stuff
  transition_time(frameId)  +
  ease_aes('linear') + 
  NULL

play_length <- length(unique(df_track_2$frameId))
play_anim <- animate(
  play_frames,
  fps = 10, 
  nframe = play_length,
  width = 800,
  height = 400,
  end_pause = 0
)

play_anim
anim_save("myanimation.gif", play_anim)
