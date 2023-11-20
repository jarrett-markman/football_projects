# download libraries and data (i already have the packages loaded into my Rstudio)
library(tidyverse)
library(ggrepel)
library(ggimage)
library(gt)
library(caret)
library(nflfastR)
options(scipen = 9999)
nflreadr::.clear_cache()
# now i want to create a data frame for all quarterback spikes in the fourth quarter in a one-score game
# i can use the pipe function as well as filter() commands to create the data frame
fourth_quarter_spikes <- load_pbp(2012:2021) %>%
  filter(qb_spike == 1) %>%
  filter(qtr == 4) %>%
  filter(score_differential >=-8)
# in addition to fourth quarter spikes, i wanted to also look at 2nd quarter spikes, not including the score differential
second_quarter_spikes <- load_pbp(2012:2021) %>%
  filter(qb_spike == 1) %>%
  filter(qtr == 2)
# there are 333 and 258 second and fourth quarter spikes
# how many spikes have a positive or negative epa or wpa?
second_quarter_spikes %>%
  filter(epa > 0)
second_quarter_spikes %>%
  filter(epa < 0)
(226/(107+226)) * 100
# ~ 67.9 % of quarterback spikes in the second quarter have a negative epa
second_quarter_spikes %>%
  filter(wpa > 0)
second_quarter_spikes %>%
  filter(wpa < 0)
(307/(307+26)) * 100
# ~ 92.2 % of quarterback spikes in the second quarter have a negative wpa
fourth_quarter_spikes %>%
  filter(epa > 0)
fourth_quarter_spikes %>%
  filter(epa < 0)
(188/(188+70)) * 100
# ~ 72.9% of quarterback spikes in the fourth quarter have a negative epa
fourth_quarter_spikes %>%
  filter(wpa > 0)
fourth_quarter_spikes %>%
  filter(wpa < 0) %>%
  mutate(x=count())
(207/(207+51)) * 100
# ~ 80.2% of quarterback spikes in the fourth quarter have a negative win probability
# now i want to plot the data for all fourth quarter spikes do get a good sense of where it lies for both epa and wpa
fourth_quarter_spikes %>%
  ggplot(aes(x=epa, y=wpa)) +
  geom_point(color = "black", shape = 1) +
  geom_hline(yintercept = mean(fourth_quarter_spikes$epa), color = 'red') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = mean(fourth_quarter_spikes$wpa), color = 'red') +
  geom_vline(xintercept = 0) +
  labs(x = "Fourth Quarter Spike EPA",
       y = "Fourth Quarter Spike WPA",
       title = "EPA and WPA for Fourth Quarter Spikes",
       caption = "Jarrett Markman | Sports Analytics") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
# a lot of the data is clustered around the 0 to -0.5 and 0 to -0.05 range for epa and wpa
second_quarter_spikes %>%
  ggplot(aes(x=epa, y=wpa)) +
  geom_point(color = 'black', shape = 1) +
  geom_hline(yintercept = mean(second_quarter_spikes$epa), color = 'red') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = mean(second_quarter_spikes$wpa), color = 'red') +
  geom_vline(xintercept = 0) +
  labs(x = "Second Quarter Spike EPA",
       y = "Second Quarter Spike WPA",
       title = "EPA and WPA for Second Quarter Spikes",
       caption = "Jarrett Markman | Sports Analytics") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
# like the fourth quarter data a lot of it's clustered around the 0 to -0.5 and 0 to -0.05 range for epa and wpa
# now i just want to get a feel for the data surrounding each spike, looking at other variables like seconds remaining, yard line, field goal and touchdown probability, and score differential (along with epa and wpa)
fourth_quarter_spikes %>% 
  select(posteam, defteam, epa, time, yrdln, score_differential, wpa, fg_prob) %>%
  filter(score_differential >= -3) %>%
  arrange(time)
fourth_quarter_spikes %>% 
  select(posteam, defteam, epa, time, yrdln, score_differential, wpa, fg_prob) %>%
  filter(score_differential >= -3) %>%
  arrange(-fg_prob)
fourth_quarter_spikes %>%
  select(posteam, defteam, epa, time, yrdln, score_differential, wpa, td_prob) %>%
  filter(score_differential <= -4 & score_differential >= -8) %>%
  arrange(time)
fourth_quarter_spikes %>%
  select(posteam, defteam, epa, time, yrdln, score_differential, wpa, td_prob) %>%
  filter(score_differential <= -4 & score_differential >= -8) %>%
  arrange(-td_prob)
# after looking through the data, it seems that epa applies to real game situations better than wpa
# i also want to make a plot for time and epa in field goal score games
fourth_quarter_spikes %>%
  filter(score_differential >= -3) %>%
  ggplot(aes(x=epa, y=time)) +
  geom_point(shape = 1, color = 'red') +
  labs(x = "Spike EPA",
       y = "Time Remaining",
       title = "Spike EPA and Time in a field goal-score game",
       caption = "Jarrett Markman | Sports Analytics") +
  theme(plot.title = element_text(size = 8, face = 'bold', hjust = 0.5))
# you can see some data clustered around a low time and positive epa which shows that epa is effectively measuring that spikes are positive in these game-time scenarios
# does the same apply with touchdowns?
fourth_quarter_spikes %>%
  filter(score_differential <= -4 & score_differential >= -8) %>%
  ggplot(aes(x=epa, y=time)) +
  geom_point(shape = 1, color = 'red') +
  labs(x = "Spike EPA",
       y = "Time Remaining",
       title = "Spike EPA and Time in a touchdown-score game",
       caption = "Jarrett Markman | Sports Analytics") +
  theme(plot.title = element_text(size = 8, face = 'bold', hjust = 0.5))
# with teams in need of a touchdown it isn't nearly that effective to spike the ball for one last play with little time left, so a majority of the data consists of a negative epa
# after looking at the two graphs i want to look at spikes and how effective they are for epa in both touchdown and field-goal "games"
# how does epa apply with high field goal and touchdown probabilities?
fourth_quarter_spikes %>%
  select(epa, score_differential) %>%
  filter(score_differential <= -4 & score_differential >= -8) %>%
  filter(epa < 0)
fourth_quarter_spikes %>%
  select(epa, score_differential) %>%
  filter(score_differential <= -4 & score_differential >= -8) %>%
  filter(epa > 0)
(105/(14+105)) * 100
# ~ 88.24% of spikes that are not within a field goal score have a negative epa
fourth_quarter_spikes %>%
  select(epa, score_differential) %>%
  filter(score_differential >= -3) %>%
  filter(epa < 0)
fourth_quarter_spikes %>%
  select(epa, score_differential) %>%
  filter(score_differential >= -3) %>%
  filter(epa > 0)
(83/129) * 100
# ~64.34% of spikes in a "field goal game" have a negative epa
# for the sake of argument field goal probabilities should be included with field-goal scores, and vice-versa with touchdown probability
fg_4q_spikes <- fourth_quarter_spikes %>%
  filter(score_differential >= -3)
fg_4q_spikes %>%
  ggplot(aes(x=epa, y=fg_prob)) +
  geom_point(color = 'black', shape = 1) +
  geom_vline(xintercept = 0, color = 'red') +
  geom_smooth(se = FALSE, color = "black", method = "lm")
cor(fg_4q_spikes$epa, fg_4q_spikes$fg_prob)
# r value of ~ .4
# meaning that there is some form of a correlation with a higher epa and higher touchdown probability in "field-goal" games
td_4q_spikes <- fourth_quarter_spikes %>%
  filter(score_differential >= -8 & score_differential <=-4)
td_4q_spikes %>%
  ggplot(aes(x=epa, y=td_prob)) +
  geom_point(color = 'black', shape = 1) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm')
cor(td_4q_spikes$epa, td_4q_spikes$td_prob)
# r value of ~ -.4 meaning that spikes in a touchdown game really shouldn't be used.
# maybe certain teams are better at using quarterback spikes?
# i can use the join command with teams_colors_logos which is built in the nflfastR functions
fourth_quarter_spikes <- fourth_quarter_spikes %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))
second_quarter_spikes <- second_quarter_spikes %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))
fourth_quarter_spikes %>%
  ggplot(aes(x=epa, y=wpa)) +
  geom_image(aes(image = team_logo_espn), size = .05) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(x = "Fourth Quarter Spike EPA",
       y = "Fourth Quarter Spike WPA",
       title = "EPA and WPA for Fourth Quarter Spikes",
       caption = "Jarrett Markman | Sports Analytics") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
# teams are clustered around the 0 to -0.5 and 0 to -0.1 range for epa and wpa
second_quarter_spikes %>%
  ggplot(aes(x=epa, y=wpa)) +
  geom_image(aes(image = team_logo_espn), size = .025) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(x = "Second Quarter Spike EPA",
       y = "Second Quarter Spike WPA",
       title = "EPA and WPA for Second Quarter Spikes",
       caption = "Jarrett Markman | Sports Analytics") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
# looking the graphs, it's hard to judge teams based on all of their spikes
# given, i decide to find the average epa and wpa per spike for each team
avg_4q_spikes <- fourth_quarter_spikes %>%
  select(posteam, epa, wpa) %>%
  group_by(posteam) %>%
  summarise(avg_epa = mean(epa), avg_wpa = mean(wpa)) %>%
  arrange(-avg_epa) %>%
  view()
avg_4q_spikes %>%
  select(posteam, avg_epa, avg_wpa) %>%
  arrange(-avg_epa) %>%
  view()
avg_4q_spikes %>%
  select(posteam, avg_wpa, avg_epa) %>%
  arrange(-avg_wpa) %>%
  view()
# # 11 teams have a positive average epa on their spikes and 3 teams have a positive average wpa on their spikes
avg_4q_spikes %>%
  select(posteam, avg_wpa, avg_epa) %>%
  filter(avg_wpa > 0 & avg_epa > 0)
# the vikings, eagles and 49ers are the only teams that have a positive epa and wpa on average
# now i want to plot the data with team logos and their average epa and wpa
avg_4q_spikes <- avg_4q_spikes %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))
fourth_quarter_spikes %>%
  select(posteam, epa, wpa) %>%
  group_by(posteam) %>%
  summarise(avg_epa = mean(epa), avg_wpa = mean(wpa)) %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr')) %>%
  ggplot(aes(x=avg_epa, y=avg_wpa)) +
  geom_image(aes(image = team_logo_espn), size = 0.05) +
  labs(x= "EPA",
       y = "WPA",
       title = "Average Fourth Quarter Spike EPA and WPA by team",
       caption = "Jarrett Markman | Sports Analytics | Data: nflfastR") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5)) +
  geom_segment(aes(x = mean(avg_epa), y = mean(avg_wpa), xend = mean(avg_epa), yend = 0.01)) +
  geom_segment(aes(x = mean(avg_epa), y = mean(avg_wpa), xend = 0.5, yend = mean(avg_wpa))) +
  geom_segment(aes(x = mean(avg_epa), y = mean(avg_wpa), xend = mean(avg_epa), yend = -0.05)) +
  geom_segment(aes(x = mean(avg_epa), y = mean(avg_wpa), xend = -0.3, yend = mean(avg_wpa))) 
# the geom_segment measures the average average epa and wpa by team, so each quadrant represents how every team compares to the rest of the league
fourth_quarter_spikes %>%
  select(posteam, defteam, posteam_score, defteam_score, time, yrdln, epa, fg_prob, score_differential) %>%
  filter(score_differential > -3) %>%
  arrange(-fg_prob) %>%
  view()
# when arranged by field goal probability and a chance to win the game with a field-goal the data tends to have a positive epa
fourth_quarter_spikes %>%
  select(posteam, defteam, posteam_score, defteam_score, time, yrdln, epa, fg_prob, score_differential) %>%
  filter(score_differential < -3) %>%
  arrange(fg_prob) %>%
  view()
# with a low field goal probability epa is mostly negative
# this additionally highlights my hypothesis that spikes are mostly ineffective, except in situations where there's a high likelihood of a positive result post-spike
# i want to look at every example and the surrounding variables
# one thing i haven't looked at yet is time, which is a character instead of a number
# it's something complicated to do but with separate, as.numeric and mutate i can find the amount of time remaining
fourth_quarter_spikes <- fourth_quarter_spikes %>%
  separate(time, c('minute', 'second'), ':') %>%
  mutate(minute = as.numeric(minute),
         second = as.numeric(second))
fourth_quarter_spikes <- fourth_quarter_spikes %>%
  mutate(seconds_in_minutes = minute * 60)
fourth_quarter_spikes <- fourth_quarter_spikes %>%
  mutate(seconds_remaining = seconds_in_minutes + second)
# now i have time as a number instead of a character i can plot time and epa
fg_model %>%
  ggplot(aes(x=epa, y=seconds_remaining)) +
  geom_point(color = 'black', shape = 1) +
  geom_smooth(color = 'black', method = 'lm')
td_model %>%
  ggplot(aes(x=epa, y=seconds_remaining)) +
  geom_point(color = 'black', shape = 1) +
  geom_smooth(color = 'black', method = 'lm')
# i can also evaluate any data with seconds remaining as a numerical value
# is there a correlation with seconds_remaining and higher epa?
fg_4q_spikes <- fourth_quarter_spikes %>%
  filter(score_differential >= -3)
td_4q_spikes <- fourth_quarter_spikes %>%
  filter(score_differential <= -4 & score_differential >= -8)
-cor(fg_4q_spikes$seconds_remaining, fg_4q_spikes$epa)
-cor(td_4q_spikes$seconds_remaining, td_4q_spikes$epa)
# the r for field goals is about .6, meaning with less time epa tends to be higher
# the r for touchdown scores is about .14 which is super low meaning there is essentially no correlation between time and epa in a toucdown score game
# is there a way to predict what the epa will be for any spike given a variety of statistics such as time, down, timeouts, yard line and score differential?
# i want to create new data frames separately for a field goal and touchdown model with only a few data points
fg_model <- fourth_quarter_spikes %>%
  filter(score_differential >= -3) %>%
  select(epa, game_seconds_remaining, down, yardline_100, posteam_timeouts_remaining, score_differential, fg_prob)
td_model <- fourth_quarter_spikes %>%
  filter(score_differential <= -4) %>%
  select(epa, game_seconds_remaining, down, yardline_100, posteam_timeouts_remaining, score_differential, td_prob)
# i can use the library(caret) and the function train to create models for epa in both field goal and touchdown score games
# here i can create two epa model sets for field goals and touchdown score games
epa_fg_model <- train(epa ~ .,
                      data = fg_model,
                      method = "lm")
epa_td_model <- train(epa ~ .,
                      data = td_model,
                      method = "lm")
# with varImp we can look at how the variables factor into epa
ggplot(varImp(epa_fg_model))
ggplot(varImp(epa_td_model))
# here we can see that field goal probability and touchdown probabilities are both very important, however, in field goal games, there are more important 
fg_predictive_epa <- predict(epa_fg_model, fg_model)
td_predictive_epa <- predict(epa_td_model, td_model)
mean(fg_predictive_epa)
mean(td_predictive_epa)
# based on my predictive model on average epa in a field goal score game is about 0.04 while in a touchdown score game it's about -0.16
# adding to my hypothesis that spikes in a touchdown score game are very inefficient and the occasional quarterback spike in a field-goal score game can be a very good and efficient move
# now i made predicted values for each sequence, but i also want to run a multivariable regression model to create a model for epa
# a lot of the dataframes have similar names so it may seem confusing
fg_epa_model <- lm(epa ~ ., data = fg_model)
td_epa_model <- lm(epa ~ ., data = td_model)
summary(fg_epa_model)
summary(td_epa_model)
# with coefficient estimates we can actually have an equation where you can input game_seconds_remaining, down, yard line, timeouts remaining, score differential and field goal/touchdown probability
# field_goal_score_epa = -.188539 - .009315(seconds remaining) - .104053(down) + .006632(yard line (100)) - .273008(timeouts remaining (possessing team)) + .012043(score differential) + 1.021204(field goal probability)
# touchdown_score_epa = -.098829 - .001565(seconds remaining) - .058500(down) + .001378(yard line (100)) + .040786(timeouts remaining (possessing team)) - .011245(score differential) - .485172(touchdown probability)