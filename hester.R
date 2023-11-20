library('tidyverse')
library('nflfastR')
library('gt')
library('ranger')
library('xgboost')
library('caret')
library('vip')
data <- load_pbp(2000:2022)
options(scipen = 9999)
data_reg <- data %>%
  filter(season_type == 'REG')
data_post <- data %>%
  filter(season_type == 'POST')
kickoffs <- data_reg %>%
  filter(kickoff_attempt == 1)
kickoff_returns <- kickoffs %>%
  group_by(kickoff_returner_player_name) %>%
  filter(!is.na(kickoff_returner_player_name)) %>%
  summarise(
    total_kr_epa = sum(epa),
    total_kr_wpa = sum(wpa),
    kickoff_returns = n(),
    kickoff_touchdowns = sum(touchdown),
    total_kr_yds = sum(return_yards),
    kr_yds_attempt = total_kr_yds/kickoff_returns,
    avg_kr_epa = mean(epa),
    avg_kr_wpa = mean(wpa),
    kr_touchdownrate = kickoff_touchdowns/kickoff_returns
  ) %>%
  filter(kickoff_returns > 30)
punts <- data_reg %>%
  filter(punt_attempt == 1)
home_returns <- punts %>%
  filter(defteam == home_team)
away_returns <- punts %>%
  filter(defteam == away_team)
home_returns <- home_returns %>%
  group_by(punt_returner_player_name) %>%
  filter(!is.na(punt_returner_player_name)) %>%
  summarise(
    total_pr_wpa = sum(vegas_home_wpa),
    punt_returns = n(),
    punt_touchdowns = sum(touchdown),
    total_pr_yds = sum(return_yards),
    pr_yds_attempt = total_pr_yds/punt_returns,
    avg_pr_wpa = mean(vegas_home_wpa),
    pr_touchdownrate = punt_touchdowns/punt_returns
  ) %>%
  filter(punt_returns > 30)
away_returns <- away_returns %>%
  group_by(punt_returner_player_name) %>%
  filter(!is.na(punt_returner_player_name)) %>%
  summarise(
    total_pr_wpa = sum(vegas_wpa),
    punt_returns = n(),
    punt_touchdowns = sum(touchdown),
    total_pr_yds = sum(return_yards),
    pr_yds_attempt = total_pr_yds/punt_returns,
    avg_pr_wpa = mean(vegas_wpa),
    pr_touchdownrate = punt_touchdowns/punt_returns
  ) %>%
  filter(punt_returns > 30)
returns <- home_returns %>%
  inner_join(away_returns, by = 'punt_returner_player_name')
returns <- returns %>%
  summarise(
    returner = punt_returner_player_name,
    total_returns = punt_returns.x + punt_returns.y,
    total_wpa = total_pr_wpa.x + total_pr_wpa.y,
    total_touchdowns = punt_touchdowns.x + punt_touchdowns.y,
    total_yards = total_pr_yds.x + total_pr_yds.y,
    wpa_return = total_wpa/total_returns,
    touchdowns_return = total_touchdowns/total_returns,
    yards_return = total_yards/total_returns
  )
receivers <- data_reg %>%
  filter(pass == 1) %>%
  group_by(receiver_player_name) %>%
  filter(!is.na(receiver_player_name)) %>%
  summarise(
    total_receiving_epa = sum(epa),
    total_receiving_wpa = sum(wpa),
    total_yards = sum(yards_gained),
    passes = n(),
    avg_yards = total_yards/passes,
    avg_receiving_epa = mean(epa),
    avg_receiving_wpa = mean(wpa),
    touchdowns = sum(touchdown),
    receiving_touchdownrate = touchdowns/passes
  ) %>%
  filter(passes > 30)
kickoff_returns %>%
  filter(kickoff_returner_player_name == 'D.Hester') %>%
  select(kickoff_returns, total_kr_epa, total_kr_wpa, avg_kr_epa, avg_kr_wpa, total_kr_yds, kr_yds_attempt, kickoff_touchdowns, kr_touchdownrate) %>%
  gt() %>%
  tab_spanner(
    label = 'Devin Hester Career Performance on Kick Returns',
    columns = c(kickoff_returns, total_kr_epa, total_kr_wpa, avg_kr_epa, avg_kr_wpa, total_kr_yds, kr_yds_attempt, kickoff_touchdowns, kr_touchdownrate)
  ) %>%
  cols_label(
    kickoff_returns = 'Total Kick Returns',
    total_kr_epa = 'Total Expected Points Added',
    total_kr_wpa = 'Total Win Probability Added',
    avg_kr_epa = 'Expected Points Added per Return',
    avg_kr_wpa = 'Average Win Probability Added per Return',
    total_kr_yds = 'Total Kick Return Yards',
    kr_yds_attempt = 'Yards per Return',
    kickoff_touchdowns = 'Kick Return Touchdowns',
    kr_touchdownrate = 'Touchdowns per Return'
  ) %>%
  data_color(
    columns = c(kickoff_returns, total_kr_epa, total_kr_wpa, avg_kr_epa, avg_kr_wpa, total_kr_yds, kr_yds_attempt, kickoff_touchdowns, kr_touchdownrate),
    colors = scales::col_numeric(
      'orange2',
               domain = NULL
    )
  ) %>%
  tab_options(table.background.color = "blue2") %>%
  cols_align(
    align = c('center'), columns = everything()) %>%
  opt_table_font(font = 'Roboto Condensed') %>%
  opt_table_outline() %>%
  tab_source_note(
    source_note = 'Jarrett Markman | Data: nflverse'
  )
receivers %>%
  filter(total_receiving_epa > 100 & total_receiving_wpa > 3) %>%
  arrange(total_receiving_epa) %>%
  select(receiver_player_name, total_receiving_epa, total_receiving_wpa, total_yards, touchdowns, receiving_touchdownrate) %>%
  head(10) %>%
  gt() %>%
  tab_spanner(
    label = 'Lowest 10 Receivers With a Total Receiving EPA above 100 in the 21st Century (ascending)',
    columns = c(receiver_player_name, total_receiving_epa, total_receiving_wpa, total_yards, touchdowns, receiving_touchdownrate)
  ) %>%
  cols_label(
    receiver_player_name = 'Receiver',
    total_receiving_epa = 'Total Expected Points Added on a Reception',
    total_receiving_wpa = 'Total Win Probability Added on a Reception',
    total_yards = 'Total Receiving Yards',
    touchdowns = 'Total Receiving Touchdowns',
    receiving_touchdownrate = 'Receiving Touchdown Rate'
  ) %>%
  tab_options(table.background.color = "black") %>%
  cols_align(
    align = c('center'), columns = everything()) %>%
  opt_table_font(font = 'Roboto Condensed') %>%
  opt_table_outline() %>%
  tab_source_note(
    source_note = 'Jarrett Markman | Data: nflverse'
  )
returns %>%
  select(returner, total_returns, total_wpa, total_touchdowns, total_yards, wpa_return, touchdowns_return, yards_return) %>%
  arrange(-total_wpa) %>%
  filter(returner == 'D.Hester') %>%
  gt() %>%
  tab_spanner(
    label = 'Best Punt Returners from 2000-2022',
    columns = c(returner, total_returns, total_wpa, total_touchdowns, total_yards, wpa_return, touchdowns_return, yards_return)
  ) %>%
  cols_label(
    returner = 'Returner',
    total_returns = '# of Punt Returns',
    total_wpa = 'Total Win Probability Added',
    total_touchdowns = 'Total Touchdowns',
    total_yards = 'Total Yards',
    wpa_return = 'Total Win Probability Added per Return',
    touchdowns_return = 'Touchdowns per Return',
    yards_return = 'Yards per Return'
  ) %>%
  data_color(
    columns = c(NULL, total_returns, total_wpa, total_touchdowns, total_yards, wpa_return, touchdowns_return, yards_return),
    colors = scales::col_numeric(
      'orange2',
               domain = NULL
    )
  ) %>%
  tab_options(table.background.color = "blue2") %>%
  cols_align(
    align = c('center'), columns = everything()) %>%
  opt_table_font(font = 'Roboto Condensed') %>%
  opt_table_outline() %>%
  tab_source_note(
    source_note = 'Jarrett Markman | Data: nflverse'
  )
receivers %>%
  filter(total_receiving_wpa > 1 & receiving_touchdownrate > .05) %>%
  arrange(total_receiving_wpa) %>%
  select(receiver_player_name, total_receiving_wpa, touchdowns,  total_yards,receiving_touchdownrate) %>%
  head(10) %>%
  gt() %>%
  tab_spanner(
    label = 'Lowest 10 Receivers With a Total Receiving WPA above 1 in the 21st Century (ascending)',
    columns = c(receiver_player_name, total_receiving_wpa, touchdowns, total_yards, receiving_touchdownrate)
  ) %>%
  cols_label(
    receiver_player_name = 'Receiver',
    total_receiving_wpa = 'Total Win Probability Added on a Reception',
    touchdowns = 'Total Receiving Touchdowns',
    total_yards = 'Total Receiving Yards',
    receiving_touchdownrate = 'Receiving Touchdown Rate'
  ) %>%
  tab_options(table.background.color = "black") %>%
  cols_align(
    align = c('center'), columns = everything()) %>%
  opt_table_font(font = 'Roboto Condensed') %>%
  opt_table_outline() %>%
  tab_source_note(
    source_note = 'Jarrett Markman | Data: nflverse'
  )
combined <- kickoff_returns %>%
  inner_join(returns, by = c('kickoff_returner_player_name' = 'returner'))
combined <- combined %>%
  summarise(
    total_returns = kickoff_returns + total_returns,
    returner = kickoff_returner_player_name,
    total_return_wpa = total_kr_wpa + total_wpa,
    total_tds = total_touchdowns + kickoff_touchdowns,
    total_return_yards = total_kr_yds + total_yards
  )
combined %>%
  arrange(-total_return_wpa) %>%
  filter(returner == 'D.Hester') %>%
  gt() %>%
  tab_spanner(
    label = 'Best Returners of the 21st Century',
    columns = c(returner, total_returns, total_return_wpa, total_tds, total_return_yards)
  ) %>%
  cols_label(
    returner = 'Returner',
    total_returns = '# of Returns',
    total_return_wpa = 'Total Win Probability Added',
    total_tds = 'Total Touchdowns',
    total_return_yards = 'Total Yards'
  ) %>%
  data_color(
    columns = c(NULL, total_returns, total_return_wpa, total_tds, total_return_yards),
    colors = scales::col_numeric(
      'orange2',
               domain = NULL
    )
  ) %>%
  tab_options(table.background.color = "blue2") %>%
  cols_align(
    align = c('center'), columns = everything()) %>%
  opt_table_font(font = 'Roboto Condensed') %>%
  opt_table_outline() %>%
  tab_source_note(
    source_note = 'Jarrett Markman | Data: nflverse'
  )
receivers %>%
  filter(total_receiving_wpa > 4) %>%
  arrange(total_receiving_wpa) %>%
  select(receiver_player_name, total_receiving_wpa, touchdowns, total_yards) %>%
  head(10) %>%
  gt() %>%
  tab_spanner(
    label = 'Lowest 10 Receivers With a Total Receiving WPA above 4 in the 21st Century (ascending)',
    columns = c(receiver_player_name, total_receiving_wpa, touchdowns, total_yards)
  ) %>%
  cols_label(
    receiver_player_name = 'Receiver',
    total_receiving_wpa = 'Total Win Probability Added on a Reception',
    touchdowns = 'Total Receiving Touchdowns',
    total_yards = 'Total Receiving Yards'
  ) %>%
  tab_options(table.background.color = "black") %>%
  cols_align(
    align = c('center'), columns = everything()) %>%
  opt_table_font(font = 'Roboto Condensed') %>%
  opt_table_outline() %>%
  tab_source_note(
    source_note = 'Jarrett Markman | Data: nflverse'
  )
kickoff_returns %>%
  select(kickoff_returner_player_name, kickoff_returns, total_kr_epa, total_kr_wpa, avg_kr_epa, avg_kr_wpa, total_kr_yds, kr_yds_attempt, kickoff_touchdowns, kr_touchdownrate) %>%
  arrange(-total_kr_epa) %>%
  head(18) %>%
  gt() %>%
  tab_spanner(
    label = 'Best Kick Returners from 2000-2022',
    columns = c(kickoff_returner_player_name, kickoff_returns, total_kr_epa, total_kr_wpa, avg_kr_epa, avg_kr_wpa, total_kr_yds, kr_yds_attempt, kickoff_touchdowns, kr_touchdownrate)
  ) %>%
  cols_label(
    kickoff_returner_player_name = 'Returner',
    kickoff_returns = 'Total Kick Returns',
    total_kr_epa = 'Total Expected Points Added',
    total_kr_wpa = 'Total Win Probability Added',
    avg_kr_epa = 'Expected Points Added per Return',
    avg_kr_wpa = 'Average Win Probability Added per Return',
    total_kr_yds = 'Total Kick Return Yards',
    kr_yds_attempt = 'Yards per Return',
    kickoff_touchdowns = 'Kick Return Touchdowns',
    kr_touchdownrate = 'Touchdowns per Return'
  ) %>%
  tab_options(table.background.color = "black") %>%
  cols_align(
    align = c('center'), columns = everything()) %>%
  opt_table_font(font = 'Roboto Condensed') %>%
  opt_table_outline() %>%
  tab_source_note(
    source_note = 'Jarrett Markman | Data: nflverse'
  )
returns %>%
  select(returner, total_returns, total_wpa, total_touchdowns, total_yards, wpa_return, touchdowns_return, yards_return) %>%
  arrange(-total_wpa) %>%
  head(20) %>%
  gt() %>%
  tab_spanner(
    label = 'Best Punt Returners from 2000-2022',
    columns = c(returner, total_returns, total_wpa, total_touchdowns, total_yards, wpa_return, touchdowns_return, yards_return)
  ) %>%
  cols_label(
    returner = 'Returner',
    total_returns = '# of Punt Returns',
    total_wpa = 'Total Win Probability Added',
    total_touchdowns = 'Total Touchdowns',
    total_yards = 'Total Yards',
    wpa_return = 'Total Win Probability Added per Return',
    touchdowns_return = 'Touchdowns per Return',
    yards_return = 'Yards per Return'
  ) %>%
  tab_options(table.background.color = "black") %>%
  cols_align(
    align = c('center'), columns = everything()) %>%
  opt_table_font(font = 'Roboto Condensed') %>%
  opt_table_outline() %>%
  tab_source_note(
    source_note = 'Jarrett Markman | Data: nflverse'
  )
combined %>%
  arrange(-total_return_wpa) %>%
  head(13) %>%
  gt() %>%
  tab_spanner(
    label = 'Best Returners of the 21st Century',
    columns = c(returner, total_returns, total_return_wpa, total_tds, total_return_yards)
  ) %>%
  cols_label(
    returner = 'Returner',
    total_returns = '# of Returns',
    total_return_wpa = 'Total Win Probability Added',
    total_tds = 'Total Touchdowns',
    total_return_yards = 'Total Yards'
  ) %>%
  tab_options(table.background.color = "black") %>%
  cols_align(
    align = c('center'), columns = everything()) %>%
  opt_table_font(font = 'Roboto Condensed') %>%
  opt_table_outline() %>%
  tab_source_note(
    source_note = 'Jarrett Markman | Data: nflverse'
  )
kickoffs$temp[is.na(kickoffs$temp)] = 68
kickoffs$wind[is.na(kickoffs$wind)] = 0
kickoffs$kick_distance[is.na(kickoffs$kick_distance)] = 55.6
kickoffs$score_differential[is.na(kickoffs$score_differential)] = 0
return_yds_allowed <- kickoffs %>%
  filter(!is.na(defteam)) %>%
  filter(touchback == 0) %>%
  group_by(defteam, season) %>%
  summarise(
    yds_allowed = mean(return_yards)
  ) %>%
  ungroup()
kickoffs <- kickoffs %>%
  left_join(return_yds_allowed, by = c('defteam', 'season'))
kickoff_data <- kickoffs %>%
  ungroup() %>%
  select(
    label = return_yards, kick_distance, touchback, yds_allowed, half_seconds_remaining, quarter_seconds_remaining,
    score_differential, ep, wp, temp, wind, roof
  )
kickoff_data$label[is.na(kickoffs$return_yards)] = 0
dmy <- dummyVars(' ~ .', data = kickoff_data)
kickoff_model_data <- data.frame(predict(dmy, newdata = kickoff_data))
smp_size <- floor(0.80 * nrow(kickoff_data))
set.seed(1234)
ind <- sample(seq_len(nrow(kickoff_model_data)), size = smp_size)
train <- as.matrix(kickoff_model_data[ind, ])
test <- as.matrix(kickoff_model_data[-ind, ])
x_kryards <- xgboost(
  data = train[, 2:15], 
  label = train[, 1],
  nrounds = 1000,
  objective = 'reg:squarederror', 
  early_stopping_rounds = 5, 
  max_depth = 6,
  eta = .25
)
values <- predict(x_kryards, test[, 2:15])
y <- test[, 1]
postResample(values, y)
expected_return_yards <- as.data.frame(
  matrix(predict(x_kryards, as.matrix(kickoff_model_data %>% select(-label))))
) %>%
  rename(x_return_yards = V1)
expected_return_yards <- cbind(kickoffs, expected_return_yards)
punts$temp[is.na(punts$temp)] = 68
punts$wind[is.na(punts$wind)] = 0
punts$kick_distance[is.na(punts$kick_distance)] = 43.4
punt_return_allowed <- punts %>%
  filter(!is.na(posteam)) %>%
  filter(touchback == 0) %>%
  group_by(posteam, season) %>%
  summarise(
    yds_allowed = mean(return_yards)
  )
punts <- punts %>%
  left_join(punt_return_allowed, by = c('posteam', 'season'))
punt_data <- punts %>%
  ungroup() %>%
  select(
    label = return_yards, kick_distance, touchback, yds_allowed, half_seconds_remaining, quarter_seconds_remaining,
    score_differential, ep, wp, temp, wind, roof
  )
dmy <- dummyVars(' ~ .', data = punt_data)
punt_model_data <- data.frame(predict(dmy, newdata = punt_data))
smp_size <- floor(0.80 * nrow(punt_data))
set.seed(1234)
ind <- sample(seq_len(nrow(punt_model_data)), size = smp_size)
train <- as.matrix(punt_model_data[ind, ])
test <- as.matrix(punt_model_data[-ind, ])
x_pryards <- xgboost(
  data = train[, 2:15], 
  label = train[, 1],
  nrounds = 1000,
  objective = 'reg:squarederror', 
  early_stopping_rounds = 5, 
  max_depth = 6,
  eta = .25
)
values <- predict(x_pryards, test[, 2:15])
y <- test[, 1]
postResample(values, y)
expected_punt_return_yards <- as.data.frame(
  matrix(predict(x_pryards, as.matrix(punt_model_data %>% select(-label))))
) %>%
  rename(x_return_yards = V1)
expected_punt_return_yards <- cbind(punts, expected_punt_return_yards)
expected_return_yards %>% 
  mutate(return_yoe = return_yards - x_return_yards) %>%
  group_by(kickoff_returner_player_name) %>%
  filter(!is.na(kickoff_returner_player_name)) %>%
  summarise(
    returns = n(),
    total_return_yoe = sum(return_yoe),
    return_yoe_per_return = total_return_yoe/returns
  ) %>%
  filter(returns > 100) %>%
  arrange(-total_return_yoe) %>%
  head(30) %>%
  gt() %>%
  tab_spanner(
    label = 'Best Kick Returners from 2000-2022',
    columns = c(kickoff_returner_player_name, returns, total_return_yoe, return_yoe_per_return)
  ) %>%
  data_color(
    columns = return_yoe_per_return,
    colors = scales::col_numeric(
      palette = c('white', 'red'),
      domain = NULL
    )) %>%
  cols_label(
    kickoff_returner_player_name = 'Returner',
    returns = '# of Kick Returns',
    total_return_yoe = 'Total Return Yards Over Expected',
    return_yoe_per_return = 'Total Return Yards Over Expected Per Return'
  ) %>%
  tab_options(table.background.color = "black") %>%
  cols_align(
    align = c('center'), columns = everything()) %>%
  opt_table_font(font = 'Roboto Condensed') %>%
  opt_table_outline() %>%
  tab_source_note(
    source_note = 'Jarrett Markman | Data: nflverse'
  )
expected_punt_return_yards %>% 
  mutate(return_yoe = return_yards - x_return_yards) %>%
  group_by(punt_returner_player_name) %>%
  filter(!is.na(punt_returner_player_name)) %>%
  summarise(
    returns = n(),
    total_return_yoe = sum(return_yoe),
    return_yoe_per_return = total_return_yoe/returns
  ) %>%
  filter(returns > 100) %>%
  arrange(-total_return_yoe) %>%
  head(10) %>%
  gt() %>%
  tab_spanner(
    label = 'Best Punt Returners from 2000-2022',
    columns = c(punt_returner_player_name, returns, total_return_yoe, return_yoe_per_return)
  ) %>%
  data_color(
    columns = return_yoe_per_return,
    colors = scales::col_numeric(
      palette = c('white', 'red'),
      domain = NULL
    )) %>%
  cols_label(
    punt_returner_player_name = 'Returner',
    returns = '# of Punt Returns',
    total_return_yoe = 'Total Return Yards Over Expected',
    return_yoe_per_return = 'Total Return Yards Over Expected Per Return'
  ) %>%
  tab_options(table.background.color = "black") %>%
  cols_align(
    align = c('center'), columns = everything()) %>%
  opt_table_font(font = 'Roboto Condensed') %>%
  opt_table_outline() %>%
  tab_source_note(
    source_note = 'Jarrett Markman | Data: nflverse'
  )
kr <- expected_return_yards %>% 
  mutate(return_yoe = return_yards - x_return_yards) %>%
  group_by(kickoff_returner_player_name) %>%
  filter(!is.na(kickoff_returner_player_name)) %>%
  summarise(
    returns = n(),
    total_return_yoe = sum(return_yoe),
    return_yoe_per_return = total_return_yoe/returns
  ) %>%
  filter(returns > 100) %>%
  arrange(-total_return_yoe)
pr <- expected_punt_return_yards %>% 
  mutate(return_yoe = return_yards - x_return_yards) %>%
  group_by(punt_returner_player_name) %>%
  filter(!is.na(punt_returner_player_name)) %>%
  summarise(
    returns = n(),
    total_return_yoe = sum(return_yoe),
    return_yoe_per_return = total_return_yoe/returns
  ) %>%
  filter(returns > 100) %>%
  arrange(-total_return_yoe)
returners <- kr %>%
  inner_join(pr, by = c('kickoff_returner_player_name' = 'punt_returner_player_name'))
returners <- returners %>%
  summarise(
    returner = kickoff_returner_player_name,
    returns = returns.x + returns.y,
    total_return_yoe = total_return_yoe.x + total_return_yoe.y,
    total_return_yoe_return = total_return_yoe/returns
  )
returners %>%
  select(returner, returns, total_return_yoe, total_return_yoe_return) %>%
  arrange(-total_return_yoe) %>%
  head(10) %>%
  gt() %>%
  tab_spanner(
    label = 'Best Returners of the 21st Century',
    columns = c(returner, returns, total_return_yoe, total_return_yoe_return)
  ) %>%
  data_color(
    columns = total_return_yoe_return,
    colors = scales::col_numeric(
      palette = c('white', 'red'),
      domain = NULL
    )
  ) %>%
  cols_label(
    returner = 'Returner',
    returns = '# of Total Returns',
    total_return_yoe = 'Total Return Yards Over Expected',
    total_return_yoe_return = 'Total Return Yards Over Expected Per Return'
  ) %>%
  tab_options(table.background.color = "black") %>%
  cols_align(
    align = c('center'), columns = everything()) %>%
  opt_table_font(font = 'Roboto Condensed') %>%
  opt_table_outline() %>%
  tab_source_note(
    source_note = 'Jarrett Markman | Data: nflverse'
  )
