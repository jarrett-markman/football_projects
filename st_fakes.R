library('tidyverse')
library('nflfastR')
library('gt')
data <- load_pbp(2000:2022)
options(scipen = 9999)
data_reg <- data %>%
  filter(season_type == 'REG')
data_post <- data %>%
  filter(season_type == 'POST')
punt_passes <- data_reg %>%
  filter(down == 4 & play_type == 'pass' & special_teams_play == 1 & grepl('Punt formation', desc)) %>%
  group_by(ydstogo) %>%
  filter(first_down != 'NA') %>%
  summarise(
    Plays = n(),
    first_downs = sum(first_down),
    firstdownrate = round((first_downs/Plays) * 100, 2),
    Key = 'Fake Punt Pass')
punt_runs <- data_reg %>%
  filter(down == 4 & play_type == 'run' & special_teams_play == 1 & grepl('Punt formation', desc)) %>%
  group_by(ydstogo) %>%
  filter(first_down != 'NA') %>%
  summarise(
    Plays = n(),
    first_downs = sum(first_down),
    firstdownrate = round((first_downs/Plays) * 100, 2),
    Key = 'Fake Punt Run')
fg_passes <- data_reg %>%
  filter(down == 4 & play_type == 'pass' & special_teams_play == 1 & grepl('Field Goal formation', desc)) %>%
  group_by(ydstogo) %>%
  filter(first_down != 'NA') %>%
  summarise(
    Plays = n(),
    first_downs = sum(first_down),
    firstdownrate = round((first_downs/Plays) * 100, 2),
    Key = 'Fake FG Pass')
fg_runs <- data_reg %>%
  filter(down == 4 & play_type == 'run' & special_teams_play == 1 & grepl('Field Goal formation', desc)) %>%
  group_by(ydstogo) %>%
  filter(first_down != 'NA') %>%
  summarise(
    Plays = n(),
    first_downs = sum(first_down),
    firstdownrate = round((first_downs/Plays) * 100, 2),
    Key = 'Fake FG Run')
run_play <- data_reg %>%
  filter(special_teams_play == 0 & down == 4 & rush == 1) %>%
  group_by(ydstogo) %>%
  filter(first_down != 'NA') %>%
  summarise(
    Plays = n(),
    first_downs = sum(first_down),
    firstdownrate = round((first_downs/Plays) * 100, 2),
    Key = 'Normal Run'
  )
pass_play <- data_reg %>%
  filter(special_teams_play == 0 & down == 4 & pass == 1) %>%
  group_by(ydstogo) %>%
  filter(first_down != 'NA') %>%
  summarise(
    Plays = n(),
    first_downs = sum(first_down),
    firstdownrate = round((first_downs/Plays) * 100, 2),
    Key = 'Normal Pass'
  )
ggplot(NULL, aes(ydstogo, firstdownrate, color = Key)) +
  geom_point(data = fg_passes, aes(size = Plays)) +
  geom_point(data = fg_runs, aes(size = Plays)) +
  geom_point(data = punt_passes, aes(size = Plays)) +
  geom_point(data = punt_runs, aes(size = Plays)) +
  geom_line(data = fg_passes) +
  geom_line(data = fg_runs) +
  geom_line(data = punt_passes) +
  geom_line(data = punt_runs) +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  labs(
    x='Yards Needed for a First Down',
    y= 'First Down Percentage',
    title = 'Conversion Rates on Fake Special Teams Plays',
    caption = 'Jarrett Markman | Data: nflverse'
  ) +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = .5))
ggplot(NULL, aes(ydstogo, firstdownrate, color = Key)) +
  geom_point(data = pass_play, aes(size = Plays)) +
  geom_point(data = run_play, aes(size = Plays)) +
  geom_line(data = pass_play) +
  geom_line(data = run_play) +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  labs(
    x='Yards Needed for a First Down',
    y= 'First Down Percentage',
    title = 'Conversion Rates on Normal 4th Down Plays',
    caption = 'Jarrett Markman | Data: nflverse'
  ) +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = .5))
ggplot(NULL, aes(ydstogo, firstdownrate, color = Key)) +
  geom_point(data = pass_play) +
  geom_point(data = run_play) +
  geom_point(data = punt_passes) +
  geom_point(data = punt_runs) +
  geom_point(data = fg_passes) +
  geom_point(data = fg_runs) +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  labs(
    x='Yards Needed for a First Down',
    y= 'First Down Percentage',
    title = 'Are Special Team "Fakes" more Effective than Regular 4th Down Plays?',
    caption = 'Jarrett Markman | Data: nflverse'
  ) +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = .5))
normal <- pass_play %>%
  left_join(run_play, by = 'ydstogo')
normal <- normal %>%
  left_join(punt_passes, by = 'ydstogo')
normal <- normal %>%
  left_join(punt_runs, by = 'ydstogo')
normal <- normal %>%
  left_join(fg_passes, by = 'ydstogo')
normal <- normal %>%
  left_join(fg_runs, by = 'ydstogo')
normal <- normal %>%
  rename(
    'Pass Plays n' = Plays.x,
    'Pass Plays' = firstdownrate.x,
    'Rush Plays n' = Plays.y,
    'Rush Plays' = firstdownrate.y,
    'Punt Passes n' = Plays.x.x,
    'Punt Passes' = firstdownrate.x.x,
    'Punt Runs n' = Plays.y.y,
    'Punt Runs' = firstdownrate.y.y,
    'Field Goal Passes n' = Plays.x.x.x,
    'Field Goal Passes' = firstdownrate.x.x.x,
    'Field Goal Runs n' = Plays.y.y.y,
    'Field Goal Runs' = firstdownrate.y.y.y
  )
table <- normal %>%
  mutate(
    pass_Plays = as.character(`Pass Plays n`),
    pass_Plays = paste('(', pass_Plays, sep = ''),
    pass_Plays = paste(pass_Plays, ')', sep = ''),
    normalpass=paste(`Pass Plays`, pass_Plays, sep = ' '),
    run_Plays = as.character(`Rush Plays n`),
    run_Plays = paste('(', run_Plays, sep = ''),
    run_Plays = paste(run_Plays, ')', sep = ''),
    normalrun=paste(`Rush Plays`, run_Plays, sep = ' '),
    punt_pass = as.character(`Punt Passes n`),
    punt_pass = paste('(', punt_pass, sep = ''),
    punt_pass = paste(punt_pass, ')', sep = ''),
    puntpass=paste(`Punt Passes`, punt_pass, sep = ' '),
    punt_run = as.character(`Punt Runs n`),
    punt_run = paste('(', punt_run, sep = ''),
    punt_run = paste(punt_run, ')', sep = ''),
    puntrun = paste(`Punt Runs`, punt_run, sep = ' '),
    fg_pass = as.character(`Field Goal Passes n`),
    fg_pass = paste('(', fg_pass, sep = ''),
    fg_pass = paste(fg_pass, ')', sep = ''),
    fgpass =paste(`Field Goal Passes`, fg_pass, sep = ' '),
    fg_run = as.character(`Field Goal Runs n`),
    fg_run = paste('(', fg_run, sep = ''),
    fg_run = paste(fg_run, ')', sep = ''),
    fgrun = paste(`Field Goal Runs`, fg_run, sep = ' ')
  ) %>%
  select(ydstogo, normalpass, normalrun, puntpass, puntrun, fgpass, fgrun)
table %>%
  ungroup() %>%
  select(ydstogo, normalpass, normalrun, puntpass, puntrun, fgpass, fgrun) %>%
  gt() %>%
  tab_spanner(
    label = 'First Downs on Different Play Types, by Yards to Go (2000-2022)',
    columns = c(ydstogo, normalpass, normalrun, puntpass, puntrun, fgpass, fgrun)
  ) %>%
  cols_align(
    align = c('center'), columns = everything()
  ) %>%
  cols_label(
    ydstogo = 'Yards to Gain for a First Down',
    normalpass = 'First Down Rate on Normal Passes (# of Plays)',
    normalrun = 'First Down Rate on Normal Runs (# of Plays)',
    puntpass = 'First Down Rate on Fake Punt Passes (# of Plays)',
    puntrun = 'First Down Rate on Fake Punt Runs (# of Plays)',
    fgpass = 'First Down Rate on Fake Field Goal Passes (# of Plays)',
    fgrun = 'First Down Rate on Fake Field Goal Runs (# of Plays)'
  ) %>%
  opt_table_font(font = 'Roboto Condensed') %>%
  opt_table_outline() %>%
  tab_source_note(
    source_note = 'Jarrett Markman | Data: nflverse'
  ) 
normal %>%
  select(ydstogo, `Pass Plays`, `Rush Plays`, `Punt Passes`,
         `Punt Runs`, `Field Goal Passes`, `Field Goal Runs`) %>%
  gt() %>%
  tab_spanner(
    label = 'First Downs on Different Play Types, by Yards to Go (2000-2022)',
    columns = c(ydstogo, `Pass Plays`, `Rush Plays`, `Punt Passes`, `Punt Runs`, `Field Goal Passes`, `Field Goal Runs`)
  ) %>%
  data_color(
    columns = c(NULL, `Pass Plays`, `Rush Plays`, `Punt Passes`, `Punt Runs`, `Field Goal Passes`, `Field Goal Runs`),
    colors = scales::col_numeric(
      palette = c('white', 'yellow2', 'green3'),
      domain = NULL
    )) %>%
  data_color(
    columns = c(ydstogo),
    colors = scales::col_numeric(
      palette = c('green3', '#FFBF00', 'white'),
      domain = NULL
    )
  ) %>%
  cols_label(
    ydstogo = 'Yards Required for a First Down', 
    `Pass Plays` = 'Conversion Rate on Regular Passes',
    `Rush Plays` = 'Conversion Rate on Regular Runs',
    `Punt Passes` = 'Conversion Rate on Fake Punt Passes', 
    `Punt Runs` = 'Conversion Rate on Fake Punt Runs', 
    `Field Goal Passes` = 'Conversion Rate on Fake Field Goal Passes',
    `Field Goal Runs` = 'Conversion Rate on Fake Field Goal Runs'
  ) %>%
  cols_align(
    align = c('center'), columns = everything()) %>%
  opt_table_font(font = 'Roboto Condensed') %>%
  opt_table_outline() %>%
  tab_source_note(
    source_note = 'Jarrett Markman | Data: nflverse'
  )
