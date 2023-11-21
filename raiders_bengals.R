library(nflfastR)
nflreadr::.clear_cache()
library(tidyverse)
library(ggrepel)
library(ggimage)
library(gt)
library(ggpubr)
data <- load_pbp(2021) %>%
  filter(season_type == "POST") %>%
  filter(posteam == "LV") %>%
  filter(defteam == "CIN") %>%
  filter(qtr == 4)
data <- data %>%
  filter(quarter_seconds_remaining < 60)
names(data)
data %>%
  filter(quarter_seconds_remaining < 47 & quarter_seconds_remaining > 26) %>%
  select(posteam, defteam, score_differential, quarter_seconds_remaining, down, yrdln, ep, epa, wp, wpa, td_prob, no_score_prob) %>%
  arrange(-quarter_seconds_remaining) %>%
  head()
data <- data %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))
data %>%
  filter(quarter_seconds_remaining < 47 & quarter_seconds_remaining > 26) %>%
  ggplot(aes(x=-quarter_seconds_remaining, y=wp)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_line() +
  labs(x = "Seconds Remaining",
       y = "Win Probability",
       title = "Time and Win Probability on the Three Plays",
       caption = "Jarrett Markman | Sports Analytics")
data %>%
  filter(quarter_seconds_remaining < 47 & quarter_seconds_remaining > 26) %>%
  ggplot(aes(x=-quarter_seconds_remaining, y=ep)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_line() +
  labs(x = "Seconds Remaining",
       y = "Expected Points",
       title = "Time and Expected Points on the Three Plays",
       caption = "Jarrett Markman | Sports Analytics")
data %>%
  filter(quarter_seconds_remaining < 47 & quarter_seconds_remaining > 26) %>%
  ggplot(aes(x=-quarter_seconds_remaining, y=no_score_prob)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_line() +
  labs(x = "Seconds Remaining",
       y = "Probability the Raiders do not Score",
       title = "No-Score Probability and the Three Plays",
       caption = "Jarrett Markman | Sports Analytics")
data %>%
  filter(quarter_seconds_remaining < 47 & quarter_seconds_remaining > 26) %>%
  ggplot(aes(x=-quarter_seconds_remaining, y=td_prob)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_line() +
  labs(x = "Seconds Remaining",
       y = "Touchdown Probability",
       title = "Touchdown Probability and Time on the Three Plays",
       caption = "Jarrett Markman | Sports Analytics")
