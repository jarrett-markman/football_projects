library(nflverse)
library(tidyverse)
library(readxl)
library(readr)
data10 <- load_schedules(2010) %>%
  filter(game_type == 'REG')
data11 <- load_schedules(2011) %>%
  filter(game_type == 'REG')
data12 <- load_schedules(2012) %>%
  filter(game_type == 'REG')
data13 <- load_schedules(2013) %>%
  filter(game_type == 'REG')
data14 <- load_schedules(2014) %>%
  filter(game_type == 'REG')
data15 <- load_schedules(2015) %>%
  filter(game_type == 'REG')
data16 <- load_schedules(2016) %>%
  filter(game_type == 'REG')
data17 <- load_schedules(2017) %>%
  filter(game_type == 'REG')
data18 <- load_schedules(2018) %>%
  filter(game_type == 'REG')
data19 <- load_schedules(2019) %>%
  filter(game_type == 'REG')
awayqb10 <- load_pbp(2010) %>%
  group_by(passer_id) %>%
  filter(!is.na(epa)) %>%
  filter(passer_id != 'N/A') %>%
  summarise(
    away_epa = mean(epa)
  )
awayqb11 <- load_pbp(2011) %>%
  group_by(passer_id) %>%
  filter(!is.na(epa)) %>%
  filter(passer_id != 'N/A') %>%
  summarise(
    away_epa = mean(epa)
  )
awayqb12 <- load_pbp(2012) %>%
  group_by(passer_id) %>%
  filter(!is.na(epa)) %>%
  filter(passer_id != 'N/A') %>%
  summarise(
    away_epa = mean(epa)
  )
awayqb13 <- load_pbp(2013) %>%
  group_by(passer_id) %>%
  filter(!is.na(epa)) %>%
  filter(passer_id != 'N/A') %>%
  summarise(
    away_epa = mean(epa)
  )
awayqb14 <- load_pbp(2014) %>%
  group_by(passer_id) %>%
  filter(!is.na(epa)) %>%
  filter(passer_id != 'N/A') %>%
  summarise(
    away_epa = mean(epa)
  )
awayqb15 <- load_pbp(2015) %>%
  group_by(passer_id) %>%
  filter(!is.na(epa)) %>%
  filter(passer_id != 'N/A') %>%
  summarise(
    away_epa = mean(epa)
  )
awayqb16 <- load_pbp(2016) %>%
  group_by(passer_id) %>%
  filter(!is.na(epa)) %>%
  filter(passer_id != 'N/A') %>%
  summarise(
    away_epa = mean(epa)
  )
awayqb17 <- load_pbp(2017) %>%
  group_by(passer_id) %>%
  filter(!is.na(epa)) %>%
  filter(passer_id != 'N/A') %>%
  summarise(
    away_epa = mean(epa)
  )
awayqb18 <- load_pbp(2018) %>%
  group_by(passer_id) %>%
  filter(!is.na(epa)) %>%
  filter(passer_id != 'N/A') %>%
  summarise(
    away_epa = mean(epa)
  )
awayqb19 <- load_pbp(2019) %>%
  group_by(passer_id) %>%
  filter(!is.na(epa)) %>%
  filter(passer_id != 'N/A') %>%
  summarise(
    away_epa = mean(epa)
  )
homeqb10 <- load_pbp(2010) %>%
  group_by(passer_id) %>%
  filter(!is.na(epa)) %>%
  filter(passer_id != 'N/A') %>%
  summarise(
    home_epa = mean(epa)
  )
homeqb11 <- load_pbp(2011) %>%
  group_by(passer_id) %>%
  filter(!is.na(epa)) %>%
  filter(passer_id != 'N/A') %>%
  summarise(
    home_epa = mean(epa)
  )
homeqb12 <- load_pbp(2012) %>%
  group_by(passer_id) %>%
  filter(!is.na(epa)) %>%
  filter(passer_id != 'N/A') %>%
  summarise(
    home_epa = mean(epa)
  )
homeqb13 <- load_pbp(2013) %>%
  group_by(passer_id) %>%
  filter(!is.na(epa)) %>%
  filter(passer_id != 'N/A') %>%
  summarise(
    home_epa = mean(epa)
  )
homeqb14 <- load_pbp(2014) %>%
  group_by(passer_id) %>%
  filter(!is.na(epa)) %>%
  filter(passer_id != 'N/A') %>%
  summarise(
    home_epa = mean(epa)
  )
homeqb15 <- load_pbp(2015) %>%
  group_by(passer_id) %>%
  filter(!is.na(epa)) %>%
  filter(passer_id != 'N/A') %>%
  summarise(
    home_epa = mean(epa)
  )
homeqb16 <- load_pbp(2016) %>%
  group_by(passer_id) %>%
  filter(!is.na(epa)) %>%
  filter(passer_id != 'N/A') %>%
  summarise(
    home_epa = mean(epa)
  )
homeqb17 <- load_pbp(2017) %>%
  group_by(passer_id) %>%
  filter(!is.na(epa)) %>%
  filter(passer_id != 'N/A') %>%
  summarise(
    home_epa = mean(epa)
  )
homeqb18 <- load_pbp(2018) %>%
  group_by(passer_id) %>%
  filter(!is.na(epa)) %>%
  filter(passer_id != 'N/A') %>%
  summarise(
    home_epa = mean(epa)
  )
homeqb19 <- load_pbp(2019) %>%
  group_by(passer_id) %>%
  filter(!is.na(epa)) %>%
  filter(passer_id != 'N/A') %>%
  summarise(
    home_epa = mean(epa)
  )
data10 <- data10 %>%
  left_join(awayqb10, by = c(away_qb_id = 'passer_id'))
data10 <- data10 %>%
  left_join(homeqb10, by = c(home_qb_id = 'passer_id'))
data11 <- data11 %>%
  left_join(awayqb11, by = c(away_qb_id = 'passer_id'))
data11 <- data11 %>%
  left_join(homeqb11, by = c(home_qb_id = 'passer_id'))
data12 <- data12 %>%
  left_join(awayqb12, by = c(away_qb_id = 'passer_id'))
data12 <- data12 %>%
  left_join(homeqb12, by = c(home_qb_id = 'passer_id'))
data13 <- data13 %>%
  left_join(awayqb13, by = c(away_qb_id = 'passer_id'))
data13 <- data13 %>%
  left_join(homeqb13, by = c(home_qb_id = 'passer_id'))
data14 <- data14 %>%
  left_join(awayqb14, by = c(away_qb_id = 'passer_id'))
data14 <- data14 %>%
  left_join(homeqb14, by = c(home_qb_id = 'passer_id'))
data15 <- data15 %>%
  left_join(awayqb15, by = c(away_qb_id = 'passer_id'))
data15 <- data15 %>%
  left_join(homeqb15, by = c(home_qb_id = 'passer_id'))
data16 <- data16 %>%
  left_join(awayqb16, by = c(away_qb_id = 'passer_id'))
data16 <- data16 %>%
  left_join(homeqb16, by = c(home_qb_id = 'passer_id'))
data17 <- data17 %>%
  left_join(awayqb17, by = c(away_qb_id = 'passer_id'))
data17 <- data17 %>%
  left_join(homeqb17, by = c(home_qb_id = 'passer_id'))
data18 <- data18 %>%
  left_join(awayqb18, by = c(away_qb_id = 'passer_id'))
data18 <- data18 %>%
  left_join(homeqb18, by = c(home_qb_id = 'passer_id'))
data19 <- data19 %>%
  left_join(awayqb19, by = c(away_qb_id = 'passer_id'))
data19 <- data19 %>%
  left_join(homeqb19, by = c(home_qb_id = 'passer_id'))
data10 <- data10 %>%
  mutate(
    star = case_when(
      home_epa > 0 | away_epa > 0 ~ 1,
      home_epa <= 0 & away_epa <= 0 ~ 0
    )
  )
data11 <- data11 %>%
  mutate(
    star = case_when(
      home_epa > 0 | away_epa > 0 ~ 1,
      home_epa <= 0 & away_epa <= 0 ~ 0
    )
  )
data12 <- data12 %>%
  mutate(
    star = case_when(
      home_epa > 0 | away_epa > 0 ~ 1,
      home_epa <= 0 & away_epa <= 0 ~ 0
    )
  )
data13 <- data13 %>%
  mutate(
    star = case_when(
      home_epa > 0 | away_epa > 0 ~ 1,
      home_epa <= 0 & away_epa <= 0 ~ 0
    )
  )
data14 <- data14 %>%
  mutate(
    star = case_when(
      home_epa > 0 | away_epa > 0 ~ 1,
      home_epa <= 0 & away_epa <= 0 ~ 0
    )
  )
data15 <- data15 %>%
  mutate(
    star = case_when(
      home_epa > 0 | away_epa > 0 ~ 1,
      home_epa <= 0 & away_epa <= 0 ~ 0
    )
  )
data16 <- data16 %>%
  mutate(
    star = case_when(
      home_epa > 0 | away_epa > 0 ~ 1,
      home_epa <= 0 & away_epa <= 0 ~ 0
    )
  )
data17 <- data17 %>%
  mutate(
    star = case_when(
      home_epa > 0 | away_epa > 0 ~ 1,
      home_epa <= 0 & away_epa <= 0 ~ 0
    )
  )
data18 <- data18 %>%
  mutate(
    star = case_when(
      home_epa > 0 | away_epa > 0 ~ 1,
      home_epa <= 0 & away_epa <= 0 ~ 0
    )
  )
data19 <- data19 %>%
  mutate(
    star = case_when(
      home_epa > 0 | away_epa > 0 ~ 1,
      home_epa <= 0 & away_epa <= 0 ~ 0
    )
  )
full_dataset <- read_excel("full_dataset.xlsx")
full_dataset <- full_dataset %>%
  separate(tempr, 'temper', 'f') %>%
  separate(humidity, c('not_humidity', 'humidity'), ': ')
full_dataset <- full_dataset %>%
  rename(home_division = division,
         home_conference = division.1)
full_dataset <- full_dataset %>%
  mutate(
    away_division = case_when(
      away_team == "ARI" ~ "NFC West",
      away_team == 'SEA' ~ "NFC West",
      away_team == 'LAR' ~ "NFC West",
      away_team == 'SF' ~ "NFC West",
      away_team == 'NYG' ~ 'NFC East',
      away_team == 'DAL' ~ "NFC East",
      away_team == 'PHI' ~ "NFC East",
      away_team == 'WAS' ~ "NFC East",
      away_team == 'GB' ~ 'NFC North',
      away_team == 'DET' ~ "NFC North",
      away_team == 'MIN' ~ "NFC North",
      away_team == 'CHI' ~ "NFC North",
      away_team == 'TB' ~ "NFC South",
      away_team == 'NO' ~ "NFC South",
      away_team == 'CAR' ~ "NFC South",
      away_team == 'ATL' ~ "NFC South",
      away_team == 'BUF' ~ "AFC East",
      away_team == 'NE' ~ "AFC East",
      away_team == 'NYJ' ~ "AFC East",
      away_team == 'MIA' ~ "AFC East",
      away_team == 'BAL' ~ "AFC North",
      away_team == 'CLE' ~ "AFC North",
      away_team == 'CIN' ~ "AFC North",
      away_team == 'PIT' ~ "AFC North",
      away_team == 'HOU' ~ "AFC South",
      away_team == 'TEN' ~ "AFC South",
      away_team == 'JAX' ~ "AFC South",
      away_team == 'IND' ~ "AFC South",
      away_team == 'KC' ~ "AFC West",
      away_team == 'LAC' ~ "AFC West",
      away_team == 'LV' ~ "AFC West",
      away_team == 'DEN' ~ "AFC West",
      away_team == 'OAK' ~ "AFC West",
      away_team == 'SD' ~ 'AFC West',
      away_team == 'STL' ~ 'NFC West',
      away_team == 'LA' ~ 'NFC West'
      ),
    away_conference = case_when(
      away_team == "ARI" ~ "NFC",
      away_team == 'SEA' ~ "NFC",
      away_team == 'LAR' ~ "NFC",
      away_team == 'SF' ~ "NFC",
      away_team == 'NYG' ~ 'NFC',
      away_team == 'DAL' ~ "NFC",
      away_team == 'PHI' ~ "NFC",
      away_team == 'WAS' ~ "NFC",
      away_team == 'GB' ~ 'NFC',
      away_team == 'DET' ~ "NFC",
      away_team == 'MIN' ~ "NFC",
      away_team == 'CHI' ~ "NFC",
      away_team == 'TB' ~ "NFC",
      away_team == 'NO' ~ "NFC",
      away_team == 'CAR' ~ "NFC",
      away_team == 'ATL' ~ "NFC",
      away_team == 'BUF' ~ "AFC",
      away_team == 'NE' ~ "AFC",
      away_team == 'NYJ' ~ "AFC",
      away_team == 'MIA' ~ "AFC",
      away_team == 'BAL' ~ "AFC",
      away_team == 'CLE' ~ "AFC",
      away_team == 'CIN' ~ "AFC",
      away_team == 'PIT' ~ "AFC",
      away_team == 'HOU' ~ "AFC",
      away_team == 'TEN' ~ "AFC",
      away_team == 'JAX' ~ "AFC",
      away_team == 'IND' ~ "AFC",
      away_team == 'KC' ~ "AFC",
      away_team == 'LAC' ~ "AFC",
      away_team == 'LV' ~ "AFC",
      away_team == 'DEN' ~ "AFC",
      away_team == 'OAK' ~ 'AFC',
      away_team == 'SD' ~ "AFC",
      away_team == 'STL' ~ 'NFC',
      away_team == 'LA' ~ "NFC"
      )
  )
fdata10 <- full_dataset %>%
  filter(season == 2010)
fdata11 <- full_dataset %>%
  filter(season == 2011)
fdata12 <- full_dataset %>%
  filter(season == 2012)
fdata13 <- full_dataset %>%
  filter(season == 2013)
fdata14 <- full_dataset %>%
  filter(season == 2014)
fdata15 <- full_dataset %>%
  filter(season == 2015)
fdata16 <- full_dataset %>%
  filter(season == 2016)
fdata17 <- full_dataset %>%
  filter(season == 2017)
fdata18 <- full_dataset %>%
  filter(season == 2018)
fdata19 <- full_dataset %>%
  filter(season == 2019)
attendance <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv")
attendance <- attendance %>%
  mutate(
    team_name = case_when(
      team_name == 'Redskins' ~ 'Commanders',
      team_name == 'Commanders' ~ 'Commanders',
      team_name == 'Cardinals' ~ 'Cardinals',
      team_name == 'Falcons' ~ 'Falcons',
      team_name == 'Ravens' ~ 'Ravens',
      team_name == 'Bills' ~ 'Bills',
      team_name == 'Panthers' ~ 'Panthers',
      team_name == 'Bears' ~ 'Bears',
      team_name == 'Bengals' ~ 'Bengals',
      team_name == 'Browns' ~ 'Browns',
      team_name == 'Cowboys' ~ 'Cowboys',
      team_name == 'Broncos' ~ 'Broncos',
      team_name == 'Lions' ~ 'Lions',
      team_name == 'Packers' ~ 'Packers',
      team_name == 'Colts' ~ 'Colts',
      team_name == 'Jaguars' ~ 'Jaguars',
      team_name == 'Chiefs' ~ 'Chiefs',
      team_name == 'Dolphins' ~ 'Dolphins',
      team_name == 'Vikings' ~ 'Vikings',
      team_name == 'Patriots' ~ 'Patriots',
      team_name == 'Saints' ~ 'Saints',
      team_name == 'Giants' ~ 'Giants',
      team_name == 'Jets' ~ 'Jets',
      team_name == 'Raiders' ~ 'Raiders',
      team_name == 'Eagles' ~ 'Eagles',
      team_name == 'Steelers' ~ 'Steelers',
      team_name == 'Chargers' ~ 'Chargers',
      team_name == '49ers' ~ '49ers',
      team_name == 'Seahawks' ~ 'Seahawks',
      team_name == 'Rams' ~ 'Rams',
      team_name == 'Buccaneers' ~ 'Buccaneers',
      team_name == 'Titans' ~ 'Titans',
      team_name == 'Texans' ~ 'Texans',
    )
  )
attendance10 <- attendance %>%
  filter(year == '2010')
attendance11 <- attendance %>%
  filter(year == '2011')
attendance12 <- attendance %>%
  filter(year == '2012')
attendance13 <- attendance %>%
  filter(year == '2013')
attendance14 <- attendance %>%
  filter(year == '2014')
attendance15 <- attendance %>%
  filter(year == '2015')
attendance16 <- attendance %>%
  filter(year == '2016')
attendance17 <- attendance %>%
  filter(year == '2017')
attendance18 <- attendance %>%
  filter(year == '2018')
attendance19 <- attendance %>%
  filter(year == '2019')
teams <- load_teams()
data10 <- data10 %>%
  left_join(teams, by = c('home_team'='team_abbr'))
data11 <- data11 %>%
  left_join(teams, by = c('home_team'='team_abbr'))
data12 <- data12 %>%
  left_join(teams, by = c('home_team'='team_abbr'))
data13 <- data13 %>%
  left_join(teams, by = c('home_team'='team_abbr'))
data14 <- data14 %>%
  left_join(teams, by = c('home_team'='team_abbr'))
data15 <- data15 %>%
  left_join(teams, by = c('home_team'='team_abbr'))
data16 <- data16 %>%
  left_join(teams, by = c('home_team'='team_abbr'))
data17 <- data17 %>%
  left_join(teams, by = c('home_team'='team_abbr'))
data18 <- data18 %>%
  left_join(teams, by = c('home_team'='team_abbr'))
data19 <- data19 %>%
  left_join(teams, by = c('home_team'='team_abbr'))
data10 <- data10 %>%
  left_join(attendance10, by = c('team_nick' = 'team_name', 'week' = 'week'))
data11 <- data11 %>%
  left_join(attendance11, by = c('team_nick' = 'team_name', 'week' = 'week'))
data12 <- data12 %>%
  left_join(attendance12, by = c('team_nick' = 'team_name', 'week' = 'week'))
data13 <- data13 %>%
  left_join(attendance13, by = c('team_nick' = 'team_name', 'week' = 'week'))
data14 <- data14 %>%
  left_join(attendance14, by = c('team_nick' = 'team_name', 'week' = 'week'))
data15 <- data15 %>%
  left_join(attendance15, by = c('team_nick' = 'team_name', 'week' = 'week'))
data16 <- data16 %>%
  left_join(attendance16, by = c('team_nick' = 'team_name', 'week' = 'week'))
data17 <- data17 %>%
  left_join(attendance17, by = c('team_nick' = 'team_name', 'week' = 'week'))
data18 <- data18 %>%
  left_join(attendance18, by = c('team_nick' = 'team_name', 'week' = 'week'))
data19 <- data19 %>%
  left_join(attendance19, by = c('team_nick' = 'team_name', 'week' = 'week'))
data10 <- data10 %>%
  left_join(fdata10, by = 'game_id')
data11 <- data11 %>%
  left_join(fdata11, by = 'game_id')
data12 <- data12 %>%
  left_join(fdata12, by = 'game_id')
data13 <- data13 %>%
  left_join(fdata13, by = 'game_id')
data14 <- data14 %>%
  left_join(fdata14, by = 'game_id')
data15 <- data15 %>%
  left_join(fdata15, by = 'game_id')
data16 <- data16 %>%
  left_join(fdata16, by = 'game_id')
data17 <- data17 %>%
  left_join(fdata17, by = 'game_id')
data18 <- data18 %>%
  left_join(fdata18, by = 'game_id')
data19 <- data19 %>%
  left_join(fdata19, by = 'game_id')
standings10 <- load_schedules(2010) %>%
  filter(game_type == 'REG') %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  mutate(wins = lag(cumsum(win), default = 0),
         losses = lag(cumsum(loss), default = 0),
         ties = lag(cumsum(tie), default = 0)) %>%
  select(team, week, wins, losses, ties)
standings11 <- load_schedules(2011) %>%
  filter(game_type == 'REG') %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  mutate(wins = lag(cumsum(win), default = 0),
         losses = lag(cumsum(loss), default = 0),
         ties = lag(cumsum(tie), default = 0)) %>%
  select(team, week, wins, losses, ties)
standings12 <- load_schedules(2012) %>%
  filter(game_type == 'REG') %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  mutate(wins = lag(cumsum(win), default = 0),
         losses = lag(cumsum(loss), default = 0),
         ties = lag(cumsum(tie), default = 0)) %>%
  select(team, week, wins, losses, ties)
standings13 <- load_schedules(2013) %>%
  filter(game_type == 'REG') %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  mutate(wins = lag(cumsum(win), default = 0),
         losses = lag(cumsum(loss), default = 0),
         ties = lag(cumsum(tie), default = 0)) %>%
  select(team, week, wins, losses, ties)
standings14 <- load_schedules(2014) %>%
  filter(game_type == 'REG') %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  mutate(wins = lag(cumsum(win), default = 0),
         losses = lag(cumsum(loss), default = 0),
         ties = lag(cumsum(tie), default = 0)) %>%
  select(team, week, wins, losses, ties)
standings15 <- load_schedules(2015) %>%
  filter(game_type == 'REG') %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  mutate(wins = lag(cumsum(win), default = 0),
         losses = lag(cumsum(loss), default = 0),
         ties = lag(cumsum(tie), default = 0)) %>%
  select(team, week, wins, losses, ties)
standings16 <- load_schedules(2016) %>%
  filter(game_type == 'REG') %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  mutate(wins = lag(cumsum(win), default = 0),
         losses = lag(cumsum(loss), default = 0),
         ties = lag(cumsum(tie), default = 0)) %>%
  select(team, week, wins, losses, ties)
standings17 <- load_schedules(2017) %>%
  filter(game_type == 'REG') %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  mutate(wins = lag(cumsum(win), default = 0),
         losses = lag(cumsum(loss), default = 0),
         ties = lag(cumsum(tie), default = 0)) %>%
  select(team, week, wins, losses, ties)
standings18 <- load_schedules(2018) %>%
  filter(game_type == 'REG') %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  mutate(wins = lag(cumsum(win), default = 0),
         losses = lag(cumsum(loss), default = 0),
         ties = lag(cumsum(tie), default = 0)) %>%
  select(team, week, wins, losses, ties)
standings19 <- load_schedules(2019) %>%
  filter(game_type == 'REG') %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  mutate(wins = lag(cumsum(win), default = 0),
         losses = lag(cumsum(loss), default = 0),
         ties = lag(cumsum(tie), default = 0)) %>%
  select(team, week, wins, losses, ties)
data10 <- data10 %>%
  left_join(standings10, by = c('week.x' = 'week', 'away_team.x' = 'team')) %>%
  rename(
    away_wins = wins,
    away_losses = losses,
    away_ties = ties
  )
data10 <- data10 %>%
  left_join(standings10, by = c('week.x' = 'week', 'home_team.x' = 'team')) %>%
  rename(
    home_wins = wins,
    home_losses = losses,
    home_ties = ties
  )
data11 <- data11 %>%
  left_join(standings11, by = c('week.x' = 'week', 'away_team.x' = 'team')) %>%
  rename(
    away_wins = wins,
    away_losses = losses,
    away_ties = ties
  )
data11 <- data11 %>%
  left_join(standings11, by = c('week.x' = 'week', 'home_team.x' = 'team')) %>%
  rename(
    home_wins = wins,
    home_losses = losses,
    home_ties = ties
  )
data12 <- data12 %>%
  left_join(standings12, by = c('week.x' = 'week', 'away_team.x' = 'team')) %>%
  rename(
    away_wins = wins,
    away_losses = losses,
    away_ties = ties
  )
data12 <- data12 %>%
  left_join(standings12, by = c('week.x' = 'week', 'home_team.x' = 'team')) %>%
  rename(
    home_wins = wins,
    home_losses = losses,
    home_ties = ties
  )
data13 <- data13 %>%
  left_join(standings13, by = c('week.x' = 'week', 'away_team.x' = 'team')) %>%
  rename(
    away_wins = wins,
    away_losses = losses,
    away_ties = ties
  )
data13 <- data13 %>%
  left_join(standings13, by = c('week.x' = 'week', 'home_team.x' = 'team')) %>%
  rename(
    home_wins = wins,
    home_losses = losses,
    home_ties = ties
  )
data14 <- data14 %>%
  left_join(standings14, by = c('week.x' = 'week', 'away_team.x' = 'team')) %>%
  rename(
    away_wins = wins,
    away_losses = losses,
    away_ties = ties
  )
data14 <- data14 %>%
  left_join(standings14, by = c('week.x' = 'week', 'home_team.x' = 'team')) %>%
  rename(
    home_wins = wins,
    home_losses = losses,
    home_ties = ties
  )
data15 <- data15 %>%
  left_join(standings15, by = c('week.x' = 'week', 'away_team.x' = 'team')) %>%
  rename(
    away_wins = wins,
    away_losses = losses,
    away_ties = ties
  )
data15 <- data15 %>%
  left_join(standings15, by = c('week.x' = 'week', 'home_team.x' = 'team')) %>%
  rename(
    home_wins = wins,
    home_losses = losses,
    home_ties = ties
  )
data16 <- data16 %>%
  left_join(standings16, by = c('week.x' = 'week', 'away_team.x' = 'team')) %>%
  rename(
    away_wins = wins,
    away_losses = losses,
    away_ties = ties
  )
data16 <- data16 %>%
  left_join(standings16, by = c('week.x' = 'week', 'home_team.x' = 'team')) %>%
  rename(
    home_wins = wins,
    home_losses = losses,
    home_ties = ties
  )
data17 <- data17 %>%
  left_join(standings17, by = c('week.x' = 'week', 'away_team.x' = 'team')) %>%
  rename(
    away_wins = wins,
    away_losses = losses,
    away_ties = ties
  )
data17 <- data17 %>%
  left_join(standings17, by = c('week.x' = 'week', 'home_team.x' = 'team')) %>%
  rename(
    home_wins = wins,
    home_losses = losses,
    home_ties = ties
  )
data18 <- data18 %>%
  left_join(standings18, by = c('week.x' = 'week', 'away_team.x' = 'team')) %>%
  rename(
    away_wins = wins,
    away_losses = losses,
    away_ties = ties
  )
data18 <- data18 %>%
  left_join(standings18, by = c('week.x' = 'week', 'home_team.x' = 'team')) %>%
  rename(
    home_wins = wins,
    home_losses = losses,
    home_ties = ties
  )
data19 <- data19 %>%
  left_join(standings19, by = c('week.x' = 'week', 'away_team.x' = 'team')) %>%
  rename(
    away_wins = wins,
    away_losses = losses,
    away_ties = ties
  )
data19 <- data19 %>%
  left_join(standings19, by = c('week.x' = 'week', 'home_team.x' = 'team')) %>%
  rename(
    home_wins = wins,
    home_losses = losses,
    home_ties = ties
  )
data <- bind_rows(data10, data11, data12, data13, data14, data15, data16, data17, data18, data19)
write_csv(data, file='Attendance Model.csv')
Attendance_Model <- read_csv("~/Desktop/SAL 213/module 4/Attendance Model.csv")
sumry <- Attendance_Model %>%
  select(home_epa, spread_line.x, total_line.x, home_wins, away_wins)
summary(sumry)
sd(sumry$home_epa)
sd(sumry$spread_line.x)
sd(sumry$total_line.x)
sd(sumry$home_wins)
sd(sumry$away_wins)
