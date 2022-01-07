library(data.table)
library(DT)
library(kableExtra)
library(reactable)
library(gt)
library(paletteer)
library(tidyverse)



# DEEPS RANKING -----------------------------------------------------------

deeps <- read_csv('final_deeps.csv') %>%
  select(-X1) %>%
  filter(Player != 'Tommylee Lewis') %>%
  arrange(DEEPS) %>%
  head(10)
deeps$face <- c('https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3116406.png',
                'https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/2577641.png',
                'https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/2973405.png',
                'https://a.espncdn.com/i/headshots/nfl/players/full/3121422.png',
                'https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/2577327.png',
                'https://a.espncdn.com/i/headshots/nfl/players/full/3039725.png',
                'https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/14100.png',
                'https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3040569.png',
                'https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/15921.png',
                'https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/4212909.png'
                )
deeps <- deeps[, c(1,7,2,4,5,6, 3)]
colnames(deeps) <- c(' ', '  ', 'DEEPS / Play', 'Let It Bounce', 'Fair Catch', 'Return', 'n')

deeps %>%
  gt() %>%
  fmt_number(columns = 3:6) %>%
  text_transform(
    locations = cells_body(vars('  ')),
    fn = function(x) {
      web_image(
        url = x,
        height = 35
      )
    }
  ) %>%
  tab_header(
    title = md("**Final DEEPS Rankings For Returners**"),
    subtitle = md("2018-20 NFL Seasons")
  ) %>%
  tab_spanner(
    label = 'EP Surrendered in Decision Making',
    columns = c('Fair Catch', 'Let It Bounce', 'Return')
  ) %>%
  tab_options(
    heading.title.font.size = 30,
    heading.subtitle.font.size = 18,
    heading.align = "middle",
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width= px(2.5),
    column_labels.border.top.width = px(2.5)
  ) %>%
  tab_footnote(
    footnote = "Higher DEEPS means more expected points surrendered",
    locations = cells_column_labels(3)
  ) %>%
  data_color(
    columns = vars('DEEPS / Play'),
    colors = scales::col_numeric(
      palette = c('#d0e6ea', '#c6e0e5', '#bddbe1', '#b3d6dd', '#aad1d8', '#a0ccd4', '#90b8bf', '#80a3aa', '#708f94', '#607a7f'),
      domain = NULL
    )
  )  %>%
  cols_align(
    align = 'center',
    columns = vars('DEEPS / Play', 'Fair Catch', 'Let It Bounce', 'Return')
  ) 

# PREPA RANKING -----------------------------------------------------------

prepas <- read_csv('playerRankings.csv') %>%
  select(-X1) %>%
  arrange(desc(PREPA)) %>%
  head(10)
prepas$face <- c('https://a.espncdn.com/i/headshots/nfl/players/full/3909416.png',
                'https://a.espncdn.com/i/headshots/nfl/players/full/4241389.png',
                'https://a.espncdn.com/i/headshots/nfl/players/full/16790.png',
                'https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3119317.png',
                'https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/2574918.png',
                'https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3115255.png',
                'https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3040035.png',
                'https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/2973405.png',
                'https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/4035170.png',
                'https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3917960.png'
)
prepas <- prepas[, c(1,4, 2,3)]
colnames(prepas) <- c(' ','  ', 'PREPA / Play', 'n')

prepas %>%
  gt() %>%
  fmt_number(columns = 3) %>%
  text_transform(
    locations = cells_body(vars('  ')),
    fn = function(x) {
      web_image(
        url = x,
        height = 35
      )
    }
  ) %>%
  tab_header(
    title = md("**Final PREPA Rankings For Returners**"),
    subtitle = md("2018-20 NFL Seasons")
  ) %>%
  tab_options(
    heading.title.font.size = 30,
    heading.subtitle.font.size = 18,
    heading.align = "middle",
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width= px(2.5),
    column_labels.border.top.width = px(2.5)
  ) %>%
  data_color(
    columns = vars('PREPA / Play'),
    colors = scales::col_numeric(
      palette = c('#e88aaa', '#e37298', '#de5b87', '#d94376', '#d52c65', '#d01454', '#bb124c','#a61043', '#920e3b', '#7d0c32'),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = 'center',
    columns = vars('PREPA / Play')
  ) 



# PLAY OPTIONS (KUPP) -----------------------------------------------------

cupp_options <-read_csv('playOptions.csv') %>%
  select(-X1, -gameId, -playId) %>%
  select(playerNameRet, fair_catch_ep, let_it_go_ep, return_ep, pred_muff, pred_fumble)
colnames(cupp_options) <- c(' ', 'Fair Catch EP','Let It Bounce EP', 'Return EP', 'Muff Prob (%)', 'Fumble Prob(%)')
cupp_options %>%
  gt() %>%
  fmt_number(columns = 2:6) %>%
  tab_header(
    title = md("**Decision Making Snapshot**")
  )


# 2X2 PREPA DEEPS ---------------------------------------------------------
deeps <- read_csv('final_deeps.csv') %>%
  select(-X1)
prepas <- read_csv('playerRankings.csv') %>%
  select(-X1)
combined <- deeps %>%
  inner_join(prepas, 
             by = 'Player') %>%
  filter(!Player %in% c('Tommylee Lewis', 'Dontrell Hilliard', 'Isaiah McKenzie'),
         n > 20, 
         N > 30) 
mean(combined$DEEPS)
mean(combined$PREPA)
combined %>%
  ggplot(aes(x = PREPA, y = DEEPS)) +
  geom_point() +
  geom_text(
    label = combined$Player,
    check_overlap = T
  ) +
  geom_hline(yintercept=mean(combined$DEEPS), linetype = 'dashed') +
  geom_vline(xintercept = mean(combined$PREPA), linetype = 'dashed') +
  labs(x = 'Return Skill (PREPA)', 
       y = 'Decision Making (DEEPS)',
       title = 'Decision Making and Return Skill Comparison on Punts') +
  theme_minimal() +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) 



# feature importance ------------------------------------------------------
muff_imp <- read_csv('muff_model_importance.csv')
fumble_imp <- read_csv('fumble_model_importance.csv')

muff_imp %>%
  select(-X1)







