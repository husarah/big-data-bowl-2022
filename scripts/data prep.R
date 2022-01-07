library(tidyverse)
library(ggvoronoi)
library(deldir)
library(lubridate)

setwd("~/Downloads/nfl-big-data-bowl-2022-data")
# download all data provided by NFL
tracking2018 <- read_csv('tracking2018.csv') 
tracking2019 <- read_csv('tracking2019.csv') 
tracking2020 <- read_csv('tracking2020.csv') 
tracking_all <- bind_rows(tracking2018, tracking2019) %>%
  bind_rows(tracking2020)
rm(tracking2018)
rm(tracking2019)
rm(tracking2020)
setwd("~/Downloads/nfl-big-data-bowl-2022/data/raw")
scouting <- read_csv('PFFScoutingData.csv')
players <- read_csv('players.csv')
plays <- read_csv('plays.csv') %>%
  filter(specialTeamsPlayType == 'Punt', #only punts
         specialTeamsResult %in% c('Out of Bounds', 'Downed', 'Touchback', 'Fair Catch',
                                   'Muffed', 'Return'))
games <- read_csv('games.csv')

#change all Oakland Raiders to Las Vegas after move
games <- games %>%
  mutate(homeTeamAbbr = recode(homeTeamAbbr, 'OAK' = 'LV'),
         visitorTeamAbbr = recode(visitorTeamAbbr, 'OAK' = 'LV'))
plays <- plays %>%
  mutate(possessionTeam = recode(possessionTeam, 'OAK' = 'LV'),
         yardlineSide = recode(yardlineSide, 'OAK' = 'LV')) 


#merge plays to scouting df
plays_scouting <- plays %>% 
  left_join(scouting %>%
              mutate(kickDirectionCorrect = kickDirectionIntended == kickDirectionActual,
                     returnDirectionCorrect = returnDirectionIntended == returnDirectionActual), 
            by = c('gameId', 'playId')) %>%
  mutate(numGunners = ifelse(is.na(gunners), 0, count.fields(textConnection(gunners), sep = ';')),
         numVises = ifelse(is.na(vises), 0, count.fields(textConnection(vises), sep = ';')),
         numRushers = ifelse(is.na(puntRushers), 0, count.fields(textConnection(puntRushers), sep = ';'))) 

#merge tracking with play level data
tracking <- tracking_all %>%
  inner_join(plays_scouting, 
             on = c('gameId', 'playId')) %>%
  left_join(games, on = "gameId") %>%
  #logic for team name
  mutate(team_name = case_when(team =='football' ~ 'football', 
                               team == 'home' ~ homeTeamAbbr,
                               TRUE ~ visitorTeamAbbr),
         new_player_id = paste(team_name, jerseyNumber)) %>%
  #parse string of players in each role
  mutate(returner_p = returnerId == nflId, 
         #determine if kicking or return team
         kickingTeam = possessionTeam,
         returnTeam = ifelse(possessionTeam == homeTeamAbbr, visitorTeamAbbr, homeTeamAbbr),
         #standardize line of scrimmage
         xLineOfScrimmage = case_when(
           yardlineNumber == 50 ~ 60,
           kickingTeam == yardlineSide ~ 10 + yardlineNumber, 
           returnTeam == yardlineSide ~ 110 - yardlineNumber
         ),
         #determine score differential 
         pointDiff = ifelse(team == 'home',
                            preSnapHomeScore - preSnapVisitorScore,
                            preSnapVisitorScore - preSnapHomeScore),
         endYardLine = yardlineNumber + playResult) %>%
  select(-missedTackler, -assistTackler, -gunners, -tackler, -puntRushers, 
         -specialTeamsSafeties, -vises, -kickerId, -returnerId, -kickBlockerId, 
         -kickDirectionIntended, 
         -returnDirectionIntended) %>%
  filter(!(gameId == 2018102106 & playId == 581),
         !(gameId == 2019100606 & playId == 2509),
         !(gameId == 2020110200 & playId == 1572),
         !(gameId == 2020112905 & playId == 145),
         !(gameId == 2020092002 & playId == 2683))

rm(tracking_all)

df_tracking <- tracking %>%
  #standardize player coordinates
  mutate(x = ifelse(playDirection == "left", 120-x, x),
         y = ifelse(playDirection == "left", 160/3 - y, y),
         xDepth = abs(x - xLineOfScrimmage),
         dir_rad = pi*dir/180,
         dir = ifelse(playDirection == "left", # standardize direction
                      ifelse(dir <= 90,
                             dir + 90,
                             dir - 90),
                      dir),
         dir = ifelse(dir > 180, #shifts dir so that 0 is pointing downfields
                      180 - dir,
                      dir),
         o = ifelse(playDirection == "left", # standardize orientation
                    ifelse(o <= 90,
                           o + 90,
                           o - 90),
                    o),
         o = ifelse(o > 180, #shifts o so that 0 is pointing downfields
                    180 - o,
                    o),
         v_x = sin(dir_rad) * s, # get x-velocity
         v_y = cos(dir_rad) * s, # get y-velocity
         #compute angle between players
         v_theta = atan(v_y / v_x),
         v_theta = ifelse(is.nan(v_theta), 0, v_theta),
         s_ratio = s / 13, #speed as function of max speed (13 lines up with NGS data)
         #predict coordinates of next frame
         x_next = x + v_x * 0.5,
         y_next = y + v_y * 0.5) %>%
  group_by(gameId, playId, nflId) %>%
  mutate(operationTime = ifelse(is.na(operationTime), 
                                (frameId[event == 'punt'] - frameId[event == 'ball_snap']) / 10, 
                                operationTime)) %>%
  ungroup() %>%
  group_by(gameId, playId, frameId) %>%
  #get coordinates of football
  mutate(xFootball = x[displayName == 'football'],
         yFootball = y[displayName == 'football'],
         NewPosition = recode(position,
                              'C' = 'OL',
                              'CB' = 'DB',
                              'DB' = 'DB',
                              'DE' = 'DL',
                              'DT' = 'DL',
                              'FB' = 'RB',
                              'FS' = 'DB',
                              'G' = 'OL',
                              'HB' = 'RB',
                              'ILB' = 'LB',
                              'K' = 'ST',
                              'LS' = 'ST',
                              'MLB' = 'LB',
                              'NT' = 'DL',
                              'OG' = 'OL',
                              'OLB' = 'LB',
                              'OT' = 'OL',
                              'P' = 'ST',
                              'S' = 'DB',
                              'SS' = 'DB',
                              'T' = 'OL')) %>%
  ungroup() %>%
  #compute distance of player to football
  mutate(disBall = sqrt((x - xFootball)^2 + (y - yFootball)^2)) %>%
  #create radius of influence based on distance from ball
  mutate(radius_of_influence = 4 + disBall^3 * (10-4) / 20,
         radius_of_influence = dplyr::case_when(
           radius_of_influence > 10 ~ 10,
           TRUE ~ radius_of_influence
         )) 

rm(tracking)

returners <- df_tracking %>% 
  select(gameId, playId, frameId, xDepth, xLineOfScrimmage, x, nflId, displayName, position) %>%
  group_by(gameId, playId) %>%
  filter(frameId == 1) %>%
  arrange(desc(xDepth)) %>%
  filter(row_number() == 1) %>%
  filter(!position %in% c('K', 'P')) %>% #filters out block scenarios
  select(nflId) %>%
  ungroup()

returner_df <- df_tracking %>%
  right_join(returners, 
             by = c('gameId', 'playId', 'nflId')) %>%
  select(
    # play description columns
    time, week, 
    # frame/ event cols
    gameId, playId, frameId, event, playDirection,
    # kicking columns
    nflIdRet = nflId, playerNameRet = displayName, posRet = position, teamRet = team_name, 
    xRet = x, yRet = y, sRet = s, aRet = a, disRet = dis, oRet = o, dirRet = dir,
    # other columns
    xLineOfScrimmage, xDepth, quarter, yardsToGo, gameClock, 
    kickLength, playResult, hangTime, pointDiff,
    kickReturnYardage, operationTime, endYardLine, specialTeamsResult,
    numGunners, numVises, numRushers, NewPosition
  )

#new column for returner (needed for non-return plays)
df_tracking <- df_tracking %>%
  right_join(returners %>%
               rename('returnerId' = 'nflId'), 
             by = c('gameId', 'playId')) %>%
  mutate(returner_p = nflId == returnerId)

# Function to find separation adapted from Jarrod Pelkofer's Kaggle notebook
get_all_separation <- function(play_data_df) {
  
  # data frame of returner
  ret_team_df <- play_data_df %>% 
    filter(returner_p) %>% 
    select(
      # play description columns
      time, week, 
      # frame/ event cols
      frameId, event,
      # kicking columns
      nflIdRet = nflId, playerNameRet = displayName, posRet = position, teamRet = team_name, 
      xRet = x, yRet = y, sRet = s, aRet = a, disRet = dis, oRet = o, dirRet = dir,
      # other columns
      xLineOfScrimmage, xDepth 
    )
  
  # data frame of all players
  kick_team_df <- 
    play_data_df %>% 
    select(
      # frame
      frameId,
      # returning columns
      nflIdKick = nflId, playerNameKick = displayName, posKick = position, teamKick = team_name, 
      xKick = x, yKick = y, sKick = s, aKick = a, disKick = dis, oKick = o, dirKick = dir
    )
  
  # join data on frameId, calculate distances from returner
  distance_to_kick_ret_df <- 
    kick_team_df %>% left_join(ret_team_df, by = "frameId") %>% 
    mutate(distanceFromPlayer = sqrt((xKick - xRet)^2 + (yKick - yRet)^2),
           dirKick = ifelse(dirKick > 0,
                            dirKick - 180,
                            dirKick + 180),
           directionRelativeToPlayer = dirKick - dirRet,
           xRelativeToPlayer = xKick - xRet,
           yRelativeToPlayer = yKick - yRet)
  return(distance_to_kick_ret_df)
  
}

# calculate frame-by-frame separation for each player combination
opp_sep_df <- df_tracking %>%
  nest(play_data = c(-gameId, -playId)) %>%
  mutate(separation_data = map(play_data, ~ get_all_separation(.x))) %>% 
  select(-play_data) %>% 
  unnest(separation_data) %>%
  filter(nflIdKick != nflIdRet) %>%
  select(gameId, playId, frameId, teamRet, nflIdRet, 
         playerNameRet, teamKick, nflIdKick, playerNameKick, distanceFromPlayer)

opp_sep <- opp_sep_df %>%
  mutate(side = ifelse(teamKick != teamRet, #determine if other player is opponent or teammate
                       'opp', 'team')) %>%
  group_by(gameId, playId, frameId, side) %>%
  #get ranking of closest player to returner by team
  arrange(distanceFromPlayer) %>%
  mutate(rank = row_number()) %>% 
  filter(rank <= 3) %>%
  left_join(df_tracking %>%
              select(gameId, playId, frameId, nflId, s,a,o,dir),
            by = c('gameId', 'playId', 'frameId', 'nflIdKick' = 'nflId')) %>%
  mutate(dir = abs(dir), 
         o = abs(o)) %>%
  ungroup() %>%
  select(-teamKick, -nflIdKick, -playerNameKick, -nflIdRet, -playerNameRet, -teamRet,
         sep = distanceFromPlayer) %>%
  #format data so all features are columns
  pivot_longer(cols = c(sep,s,a,o,dir),
               names_to = 'metric',
               values_to = 'value') %>%
  pivot_wider(names_from = c(metric,side,rank),
              values_from = value
  ) %>%
  left_join(
    #add back returner data
    returner_df %>%
      select(gameId, playId, frameId, event,#id's
             xRet, yRet, sRet, aRet, dirRet, oRet, #returner data
             quarter, yardsToGo,  gameClock, #situation
             kickLength, xLineOfScrimmage, hangTime, 
             operationTime, pointDiff, endYardLine, numGunners, numVises, numRushers,
             NewPosition,
             kickReturnYardage,  playerNameRet, playDirection, specialTeamsResult), 
    by = c('gameId', 'playId', 'frameId')) 
rm(opp_sep_df)


#final data for continuous PYOE modeling
training_data <- opp_sep %>%
  filter(specialTeamsResult == 'Return') %>%
  group_by(gameId, playId) %>%
  mutate(xRetOriginal = xRet[event %in% c('punt_received', 'fair_catch')]) %>%
  ungroup() %>%
  group_by(gameId, playId, frameId) %>%
  mutate(adjKickReturnYardage =  kickReturnYardage + (xRet - xRetOriginal),
         dirRet = abs(dirRet), 
         oRet = abs(oRet)) %>%
  filter(!is.na(adjKickReturnYardage)) %>%
  select(-kickReturnYardage, -event, -gameClock) %>%
  drop_na() 

write.csv(training_data, 'continuous_ryoe.csv')


tracking_football = df_tracking %>%
  filter(displayName == "football")

add_z_with_velocity <- function(football_data){
  g <- 10.725 #yards per second per second
  begin_punt_frame = football_data %>%
    group_by(gameId, playId) %>%
    filter(event == "punt") %>%
    select(frameId, x, y) %>%
    rename("beginPuntFrame" = "frameId", "beginPuntX" = "x", "beginPuntY" = "y")
  last_punt_frame = football_data %>%
    group_by(gameId, playId) %>%
    filter(event == "punt_land" | event == "punt_received" | event == "fair_catch") %>%
    arrange(frameId) %>%
    filter(row_number() == 1) %>%
    select(frameId, x, y) %>%
    rename("endPuntFrame" = "frameId", "endPuntX" = "x", "endPuntY" = "y")
  new_football_data = football_data %>%
    left_join(begin_punt_frame, on = c("gameId", "playId")) %>%
    group_by(gameId, playId) %>%
    filter(frameId >= beginPuntFrame) %>%
    ungroup() %>%
    left_join(last_punt_frame, on = c("gameId", "playId")) %>%
    group_by(gameId, playId) %>%
    filter(frameId <= endPuntFrame) %>%
    ungroup() %>%
    mutate(secondsSincePunt = (frameId - beginPuntFrame) / 10,
           d = sqrt((beginPuntX - endPuntX)*(beginPuntX - endPuntX) + 
                      (beginPuntY - endPuntY)*(beginPuntY - endPuntY)),
           vxy = d / ((endPuntFrame - beginPuntFrame)/10)) %>%
    group_by(gameId, playId) %>%
    mutate(bigT = max(secondsSincePunt)) %>%
    ungroup() %>%
    mutate(vz = (bigT * g) / 2,
           vz1 = (0.5+0.5*g*bigT*bigT)/bigT,
           vz2 = (-0.5+0.5*g*bigT*bigT)/bigT,
           v_0 = sqrt(vz*vz + vxy*vxy),
           launch_angle = atan(vz/vxy) * 180 / 3.14,
           z = vz*secondsSincePunt - 0.5*g*(secondsSincePunt^2),
           z1 = -0.5 + vz1*secondsSincePunt - 0.5*g*(secondsSincePunt^2),
           z2 = 0.5 + vz2*secondsSincePunt - 0.5*g*(secondsSincePunt^2)
    )
  return(new_football_data)
}

location_of_ball = add_z_with_velocity(tracking_football)

location_of_ball_no_bounce_end = location_of_ball %>%
  group_by(gameId, playId) %>%
  filter(secondsSincePunt >= bigT / 2) %>%
  mutate(zDistance = abs(z-1)) %>%
  arrange(zDistance) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  rename("ballFinalX" = "x", "ballFinalY" = "y", "puntTime" = "secondsSincePunt") %>%
  select(gameId, playId, ballFinalX, ballFinalY, puntTime)

location_of_ball_no_bounce_apex = location_of_ball %>%
  group_by(gameId, playId) %>%
  filter(secondsSincePunt == round(bigT / 2, 1)) %>%
  ungroup() %>%
  rename("ballApexX" = "x", "ballApexY" = "y", "apexTime" = "secondsSincePunt") %>%
  select(gameId, playId, ballApexX, ballApexY, beginPuntFrame, endPuntFrame,
         apexTime, vxy, vz, v_0, launch_angle, z)

filtered_apex_frames <- opp_sep %>% 
  group_by(gameId, playId) %>%
  right_join(location_of_ball_no_bounce_apex, 
             by = c('gameId', 'playId')) %>% select(frameId, beginPuntFrame, apexTime) %>%
  filter(frameId == beginPuntFrame + apexTime * 10) 




returner_info <- returner_df %>%
  group_by(gameId, playId) %>%
  right_join(location_of_ball_no_bounce_apex, by = c('gameId', 'playId')) %>%
  filter(frameId == beginPuntFrame + apexTime * 10) %>%
  ungroup() %>%
  group_by(gameId, playId) %>%
  right_join(location_of_ball_no_bounce_end, by = c('gameId', 'playId')) %>%
  mutate(distanceFromBallFinal = sqrt((xRet - ballFinalX)*(xRet - ballFinalX) + 
                                        (yRet - ballFinalY)*(yRet - ballFinalY)),
         distanceFromBallApex = sqrt((xRet - ballApexX)*(xRet - ballApexX) + 
                                       (yRet - ballApexY)*(yRet - ballApexY)),
         distanceBallToGo = sqrt((ballApexX - ballFinalX)*(ballApexX - ballFinalX) + 
                                   (ballApexY - ballFinalY)*(ballApexY - ballFinalY)),
         dirChange = atan((ballFinalY - yRet)/(ballFinalX - xRet)),
         timeLeft = puntTime - apexTime,
         muffed_punt = (specialTeamsResult == 'Muffed') * 1) %>%
  ungroup() %>%
  left_join(opp_sep %>%
              select(-event, -playDirection, -playerNameRet,
                     -xRet, -yRet, -sRet, -aRet, -oRet, -dirRet, -xLineOfScrimmage,
                     -quarter, -yardsToGo, -gameClock, -kickLength, -hangTime,
                     -pointDiff, -kickReturnYardage, -operationTime, 
                     -specialTeamsResult, -numGunners, -numRushers, -numVises,
                     -endYardLine, -NewPosition), 
            by = c('gameId', 'playId', 'frameId'))

returner_info$time <- str_extract(returner_info$time, "\\d\\d\\:\\d\\d")
returner_info$time <- as.numeric(str_extract(
  returner_info$time, "\\d\\d"))

# Weather Data
weather.data <- read.csv("games_weather.csv")

# Game Data
games.data <- read.csv("games2.csv")

#Cleaning Weather Data Times and standardizing timezones
split.times <- str_split(weather.data$TimeMeasure, " ", n = 2)

for (i in 1:length(weather.data$TimeMeasure)){
  weather.data$TimeMeasure[i] <- str_split(split.times[[i]][2],":",n =2)[[1]][1]
}

weather.data <- merge(weather.data, games.data[,c(1,6)], by = "game_id")

weather.data <- weather.data[-c(which(is.na(weather.data$EstimatedCondition))),]

weather.data$TimeMeasure <- as.numeric(weather.data$TimeMeasure) - 
  weather.data$TZOffset

weather.data <- weather.data %>% 
  mutate(TimeMeasure = ifelse(TimeMeasure >= 24, TimeMeasure - 24, TimeMeasure)) %>%
  select(game_id, TimeMeasure, Temperature, Humidity, Precipitation, WindSpeed, Pressure)


df_with_weather <- merge(returner_info, weather.data, by.x = c("gameId", "time"), 
                         by.y = c("game_id", "TimeMeasure"))

voronoi <- df_tracking %>% 
  group_by(gameId, playId, frameId) %>%
  right_join(filtered_apex_frames, 
             by = c('gameId', 'playId', 'frameId')) %>%
  mutate(voronoi_area = deldir(x, y)$summary[8] %>% unlist()) %>%
  filter(returner_p) %>% 
  select(gameId, playId, frameId, voronoi_area) 

df_with_voronoi <- df_with_weather %>% 
  left_join(voronoi, 
            by = c('gameId', 'playId', 'frameId')) %>%
  mutate(NewPosition = recode(posRet,
                              'C' = 'OL',
                              'CB' = 'DB',
                              'DB' = 'DB',
                              'DE' = 'DL',
                              'DT' = 'DL',
                              'FB' = 'RB',
                              'FS' = 'DB',
                              'G' = 'OL',
                              'HB' = 'RB',
                              'ILB' = 'LB',
                              'K' = 'ST',
                              'LS' = 'ST',
                              'MLB' = 'LB',
                              'NT' = 'DL',
                              'OG' = 'OL',
                              'OLB' = 'LB',
                              'OT' = 'OL',
                              'P' = 'ST',
                              'S' = 'DB',
                              'SS' = 'DB',
                              'T' = 'OL'),
         oRet = abs(oRet),
         dirRet = abs(dirRet),
         timeRemaining = 3600 - ((as.numeric(quarter) - 1) * 900 ) - (900 - (period_to_seconds(hms(gameClock)) / 60))) %>%
  left_join(opp_sep %>% 
              group_by(gameId, playId) %>%
              summarise(fumble = 1 * any(event %in% c('fumble_offense-recovered', 'fumble_defense_recovered', 'fumble'))) %>%
              select(gameId, playId, fumble),
            by = c('gameId', 'playId')) %>%
  left_join(df_tracking %>% 
              filter(displayName == 'football') %>% 
              select(gameId, playId, frameId, event, x) %>% 
              group_by(gameId, playId) %>% 
              arrange(desc(frameId)) %>% 
              slice(1) %>%
              rename('last_x_frame' = x) %>% 
              select(-frameId, -event),
            by = c('gameId', 'playId')
  )


write.csv(df_with_voronoi, 'df_w_voronoi.csv')
