library('tidyverse')
library('nflverse')
library('stargazer')
# DATA COLLECTION W/ NFLFASTR
standings21 <- load_schedules(2021) %>%
  filter(game_type == 'REG')
standings20 <- load_schedules(2020) %>%
  filter(game_type == 'REG')
standings19 <- load_schedules(2019) %>%
  filter(game_type == 'REG')
standings18 <- load_schedules(2018) %>%
  filter(game_type == 'REG')
standings17 <- load_schedules(2017) %>%
  filter(game_type == 'REG')
standings16 <- load_schedules(2016) %>%
  filter(game_type == 'REG')
onescore_standings20 <- standings20 %>%
  filter(game_type == 'REG') %>%
  filter(result >= -8 & result <= 8)
onescore_standings19 <- standings19 %>%
  filter(game_type == 'REG') %>%
  filter(result >= -8 & result <= 8)
onescore_standings18 <- standings18 %>%
  filter(game_type == 'REG') %>%
  filter(result >= -8 & result <= 8)
onescore_standings17 <- standings17 %>%
  filter(game_type == 'REG') %>%
  filter(result >= -8 & result <= 8)
onescore_standings16 <- load_schedules(2016) %>%
  filter(game_type == 'REG') %>%
  filter(result >= -8 & result <= 8)
defepa16 <- load_pbp(2016) %>%
  filter(season_type == 'REG') %>%
  filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>%
  group_by(defteam) %>%
  summarise(defensive_epa = mean(epa, na.rm = TRUE)) %>%
  rename(team = defteam)
offepa16 <- load_pbp(2016) %>%
  filter(season_type == 'REG') %>%
  filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>%
  group_by(posteam) %>%
  summarise(offensive_epa = mean(epa, na.rm = TRUE)) %>%
  rename(team = posteam)
epa16 <- offepa16 %>%
  left_join(defepa16, by = 'team')
defepa17 <- load_pbp(2017) %>%
  filter(season_type == 'REG') %>%
  filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>%
  group_by(defteam) %>%
  summarise(defensive_epa = mean(epa, na.rm = TRUE)) %>%
  rename(team = defteam)
offepa17 <- load_pbp(2017) %>%
  filter(season_type == 'REG') %>%
  filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>%
  group_by(posteam) %>%
  summarise(offensive_epa = mean(epa, na.rm = TRUE)) %>%
  rename(team = posteam)
epa17 <- offepa17 %>%
  left_join(defepa17, by = 'team')
defepa18 <- load_pbp(2018) %>%
  filter(season_type == 'REG') %>%
  filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>%
  group_by(defteam) %>%
  summarise(defensive_epa = mean(epa, na.rm = TRUE)) %>%
  rename(team = defteam)
offepa18 <- load_pbp(2018) %>%
  filter(season_type == 'REG') %>%
  filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>%
  group_by(posteam) %>%
  summarise(offensive_epa = mean(epa, na.rm = TRUE)) %>%
  rename(team = posteam)
epa18 <- offepa18 %>%
  left_join(defepa18, by = 'team')
defepa19 <- load_pbp(2019) %>%
  filter(season_type == 'REG') %>%
  filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>%
  group_by(defteam) %>%
  summarise(defensive_epa = mean(epa, na.rm = TRUE)) %>%
  rename(team = defteam)
offepa19 <- load_pbp(2019) %>%
  filter(season_type == 'REG') %>%
  filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>%
  group_by(posteam) %>%
  summarise(offensive_epa = mean(epa, na.rm = TRUE)) %>%
  rename(team = posteam)
epa19 <- offepa19 %>%
  left_join(defepa19, by = 'team')
defepa20 <- load_pbp(2020) %>%
  filter(season_type == 'REG') %>%
  filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>%
  group_by(defteam) %>%
  summarise(defensive_epa = mean(epa, na.rm = TRUE)) %>%
  rename(team = defteam)
offepa20 <- load_pbp(2020) %>%
  filter(season_type == 'REG') %>%
  filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>%
  group_by(posteam) %>%
  summarise(offensive_epa = mean(epa, na.rm = TRUE)) %>%
  rename(team = posteam)
epa20 <- offepa20 %>%
  left_join(defepa20, by = 'team')
#DATA CLEANING/MANIPULATION
standings21 <- standings21 %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  summarize(wins = sum(win),
            losses = sum(loss),
            ties = sum(tie),
            gp = wins+losses+ties,
            win_pct = wins/gp,
            point_differential = sum(result))
standings20 <- standings20 %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  summarize(wins = sum(win),
            losses = sum(loss),
            ties = sum(tie),
            gp = wins+losses+ties,
            win_pct = wins/gp,
            point_differential = sum(result))
standings19 <- standings19 %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  summarize(wins = sum(win),
            losses = sum(loss),
            ties = sum(tie),
            gp = wins+losses+ties,
            win_pct = wins/gp,
            point_differential = sum(result))
standings18 <- standings18 %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  summarize(wins = sum(win),
            losses = sum(loss),
            ties = sum(tie),
            gp = wins+losses+ties,
            win_pct = wins/gp,
            point_differential = sum(result))
standings17 <- standings17 %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  summarize(wins = sum(win),
            losses = sum(loss),
            ties = sum(tie),
            gp = wins+losses+ties,
            win_pct = wins/gp,
            point_differential = sum(result))
standings16 <- standings16 %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  summarize(wins = sum(win),
            losses = sum(loss),
            ties = sum(tie),
            gp = wins+losses+ties,
            win_pct = wins/gp,
            point_differential = sum(result))
onescore_standings20 <- onescore_standings20 %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  summarize(wins = sum(win),
            losses = sum(loss),
            ties = sum(tie),
            gp = wins+losses+ties,
            win_pct = wins/gp,
            point_differential = sum(result))
onescore_standings19 <- onescore_standings19 %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  summarize(wins = sum(win),
            losses = sum(loss),
            ties = sum(tie),
            gp = wins+losses+ties,
            win_pct = wins/gp,
            point_differential = sum(result))
onescore_standings18 <- onescore_standings18 %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  summarize(wins = sum(win),
            losses = sum(loss),
            ties = sum(tie),
            gp = wins+losses+ties,
            win_pct = wins/gp,
            point_differential = sum(result))
onescore_standings17 <- onescore_standings17 %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  summarize(wins = sum(win),
            losses = sum(loss),
            ties = sum(tie),
            gp = wins+losses+ties,
            win_pct = wins/gp,
            point_differential = sum(result))
onescore_standings16 <- onescore_standings16 %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  summarize(wins = sum(win),
            losses = sum(loss),
            ties = sum(tie),
            gp = wins+losses+ties,
            win_pct = wins/gp,
            point_differential = sum(result))
#JOINING DATA
data16 <- onescore_standings16 %>%
  left_join(standings16, by = 'team') %>%
  left_join(epa16, by = 'team') %>%
  left_join(standings17, by = 'team') %>%
  rename(
    win_pct_n_onescore = win_pct.x,
    win_pct_n = win_pct.y,
    'win_pct_(n+1)' = win_pct,
    point_diff_n_onescore = point_differential.x,
    point_diff_n = point_differential.y,
  )
data17 <- onescore_standings17 %>%
  left_join(standings17, by = 'team') %>%
  left_join(epa17, by = 'team') %>%
  left_join(standings18, by = 'team') %>%
  rename(
    win_pct_n_onescore = win_pct.x,
    win_pct_n = win_pct.y,
    'win_pct_(n+1)' = win_pct,
    point_diff_n_onescore = point_differential.x,
    point_diff_n = point_differential.y,
  )
data18 <- onescore_standings18 %>%
  left_join(standings18, by = 'team') %>%
  left_join(epa18, by = 'team') %>%
  left_join(standings19, by = 'team') %>%
  rename(
    win_pct_n_onescore = win_pct.x,
    win_pct_n = win_pct.y,
    'win_pct_(n+1)' = win_pct,
    point_diff_n_onescore = point_differential.x,
    point_diff_n = point_differential.y,
  )
data19 <- onescore_standings19 %>%
  left_join(standings19, by = 'team') %>%
  left_join(epa19, by = 'team') %>%
  left_join(standings20, by = 'team') %>%
  rename(
    win_pct_n_onescore = win_pct.x,
    win_pct_n = win_pct.y,
    'win_pct_(n+1)' = win_pct,
    point_diff_n_onescore = point_differential.x,
    point_diff_n = point_differential.y,
  )
data20 <- onescore_standings20 %>%
  left_join(standings20, by = 'team') %>%
  left_join(epa20, by = 'team') %>%
  left_join(standings21, by = 'team') %>%
  rename(
    win_pct_n_onescore = win_pct.x,
    win_pct_n = win_pct.y,
    'win_pct_(n+1)' = win_pct,
    point_diff_n_onescore = point_differential.x,
    point_diff_n = point_differential.y,
  )
data <- bind_rows(data16, data17, data18, data19, data20) %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))
#PLOTS
data %>%
  ggplot(aes(x=win_pct_n_onescore, y=`win_pct_(n+1)`)) +
  geom_point(col = 'red2') +
  geom_smooth(method = 'lm', col = 'blue3') +
  theme_bw() +
  labs(
    x= "Win Percentage in One-Score Games in the n Year",
    y = "Win Percentage in the n+1 Year",
    title = "How Predictive is Win Percentage in One Score Games for Win Percentage in the Following Year?",
    caption = "Jarrett Markman | Data: nflverse"
  ) +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
data %>%
  ggplot(aes(x=win_pct_n, y=`win_pct_(n+1)`)) +
  geom_point(col = 'red2') +
  geom_smooth(method = 'lm', col = 'blue3') +
  theme_bw() +
  labs(
    x= "Win Percentage in the n Year",
    y = "Win Percentage in the n+1 Year",
    title = "How Predictive is Win Percentage for Win Percentage in the Following Year?",
    caption = "Jarrett Markman | Data: nflverse"
  ) +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
data %>%
  ggplot(aes(x=offensive_epa, y=`win_pct_(n+1)`)) +
  geom_point(col = 'red2') +
  geom_smooth(method = 'lm', col = 'blue3') +
  theme_bw() +
  labs(
    x= "Offensive EPA in the n Year",
    y = "Win Percentage in the n+1 Year",
    title = "How Predictive is Offensive EPA for Win Percentage in the Following Year?",
    caption = "Jarrett Markman | Data: nflverse"
  ) +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
data %>%
  ggplot(aes(x=defensive_epa, y=`win_pct_(n+1)`)) +
  geom_point(col = 'red2') +
  geom_smooth(method = 'lm', col = 'blue3') +
  theme_bw() +
  labs(
    x= "Defensive EPA in the n Year",
    y = "Win Percentage in the n+1 Year",
    title = "How Predictive is Defensive EPA for Win Percentage in the Following Year?",
    caption = "Jarrett Markman | Data: nflverse"
  ) +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
data %>%
  ggplot(aes(x=point_diff_n_onescore, y=`win_pct_(n+1)`)) +
  geom_point(col = 'red2') +
  geom_smooth(method = 'lm', col = 'blue3') +
  theme_bw() +
  labs(
    x= "Point Differential in One-Score Games in the n Year",
    y = "Win Percentage in the n+1 Year",
    title = "How Predictive is Point Differential in One Score Games for Win Percentage in the Following Year?",
    caption = "Jarrett Markman | Data: nflverse"
  ) +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
data %>%
  ggplot(aes(x=point_diff_n, y=`win_pct_(n+1)`)) +
  geom_point(col = 'red2') +
  geom_smooth(method = 'lm', col = 'blue3') +
  theme_bw() +
  labs(
    x= "Point Differential in the n Year",
    y = "Win Percentage in the n+1 Year",
    title = "How Predictive is Point Differential for Win Percentage in the Following Year?",
    caption = "Jarrett Markman | Data: nflverse"
  ) +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
#MODEL
model <- lm(`win_pct_(n+1)` ~ (win_pct_n_onescore) + (win_pct_n) + (offensive_epa) + (defensive_epa) + point_diff_n_onescore + point_diff_n, data)
stargazer(model, type = 'text', title = 'What Makes a Winner in the NFL?',
          out = 'model.html', dep.var.labels = 'Win Percentage in the n+1 Year',
          covariate.labels = c("Win Percentage in One Score Games in the n Year",
                               "Win Percentage in the n Year", "Offensive EPA in the n Year",
                               "Defensive EPA in the n Year", "Point Differential in One Score Games in the n Year",
                               "Point Differential in the n Year")
          )
#PREDICTING THE 2022 SEASON
data21 <- load_schedules(2021) %>%
  filter(game_type == 'REG') %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  summarize(wins = sum(win),
            losses = sum(loss),
            ties = sum(tie),
            gp = wins+losses+ties,
            win_pct = wins/gp,
            point_differential = sum(result))
data21onescore <- load_schedules(2021) %>%
  filter(game_type == 'REG') %>%
  filter(result >= -8 & result <= 8) %>%
  clean_homeaway() %>%
  mutate(win = result > 0,
         loss = result < 0,
         tie = result == 0) %>%
  group_by(team) %>%
  summarize(wins = sum(win),
            losses = sum(loss),
            ties = sum(tie),
            gp = wins+losses+ties,
            win_pct = wins/gp,
            point_differential = sum(result))
data21 <- data21 %>%
  left_join(data21onescore, by = 'team')
offepa21 <- load_pbp(2021) %>%
  filter(season_type == 'REG') %>%
  filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>%
  group_by(posteam) %>%
  summarise(offensive_epa = mean(epa, na.rm = TRUE)) %>%
  rename(team = posteam)
defepa21 <- load_pbp(2021) %>%
  filter(season_type == 'REG') %>%
  filter(!is.na(defteam) & (rush == 1 | pass == 1)) %>%
  group_by(defteam) %>%
  summarise(defensive_epa = mean(epa, na.rm = TRUE)) %>%
  rename(team = defteam)
data21 <- data21 %>%
  left_join(offepa21,by = 'team') %>%
  left_join(defepa21, by = 'team')
data21 <- data21 %>%
  mutate(
    team_index = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32),
    expected_win_pct = -.123*(win_pct.y) - .181*(win_pct.x) - .062*(offensive_epa) + .008*(defensive_epa) + .002*(point_differential.y) + .0004*(point_differential.x) + .688
  )
data21 %>%
  ggplot(aes(x=team_index, y=expected_win_pct)) +
  geom_nfl_logos(aes('team_abbr' = team), width = 0.05625) +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size = 12, face = 'bold', hjust=0.5)) +
  labs(
    x= '',
    y= 'Expected Win Percentage in 2022',
    title = '2022 Win Percentage Projections Based on the Model',
    caption = 'Jarrett Markman | Data: nflverse'
  )
sumstats <- data %>%
  select(win_pct_n_onescore, win_pct_n, offensive_epa, defensive_epa, point_diff_n_onescore, point_diff_n)
summary(sumstats)
sd(sumstats$win_pct_n_onescore)
sd(sumstats$win_pct_n)
sd(sumstats$point_diff_n_onescore)
sd(sumstats$point_diff_n)
off_def <- sumstats %>%
  filter(offensive_epa != 'N/A')
sd(off_def$offensive_epa)
sd(off_def$defensive_epa)
