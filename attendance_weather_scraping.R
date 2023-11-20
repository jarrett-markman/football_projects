library(rvest)
library(nflverse)
library(tidyverse)
data <- load_schedules(2010:2019) %>%
  filter(game_type == 'REG')
data$home_win <- ifelse(data$home_score > data$away_score, 1, 0)
data$away_win <- ifelse(data$home_score < data$away_score, 1, 0)
data = subset(data, select = -c(temp))

team <- c("giants", "cardinals", "falcons", "panthers", "bears", "cowboys", "lions",
         "packers", "rams", "rams", "vikings", "saints", "eagles", "49ers", "seahawks", "buccaneers",
         "redskins", "ravens", "bills", "bengals", "browns", "broncos", "texans", "colts", "jaguars", "chiefs",
         "raiders", "chargers", "chargers", "dolphins", "patriots", "jets", "steelers", "titans") 
team_code <- c("NYG", "ARI", "ATL", "CAR", "CHI", "DAL", "DET", "GB", "LA", "STL", "MIN",
               "NO", "PHI", "SF", "SEA", "TB", "WAS", "BAL", "BUF", "CIN", "CLE", "DEN", "HOU", 
               "IND", "JAX", "KC", "OAK", "SD", "LAC", "MIA", "NE", "NYJ", "PIT", "TEN")

codes <- data.frame(team, team_code)
weather_df <- data.frame(data$week, data$season, data$home_team, data$away_team)

for (i in 1:nrow(weather_df)){
  weather_df$away_team[i] = codes$team[codes$team_code == weather_df$data.away_team[i]]
}
for (i in 1:nrow(weather_df)){
  weather_df$home_team[i] = codes$team[codes$team_code == weather_df$data.home_team[i]]
}
away = weather_df$away_team
home = weather_df$home_team


getweather <- function(year, number, away, home){
    paste0('https://www.nflweather.com/en/game/', year, '/week-', number, '/', away, '-at-', home) %>%
    read_html() %>%
    html_nodes(".span3 p") %>%
    html_text() %>%
    data.frame() %>%
    tail(-2) %>%
    return(year, number, away, home)

}
getweather1 <- function(year, number, away, home){
  paste0('https://www.nflweather.com/en/game/', year, '/week-', number, '-2/', away, '-at-', home) %>%
    read_html() %>%
    html_nodes(".span3 p") %>%
    html_text() %>%
    data.frame() %>%
    tail(-2) %>%
    return(year, number, away, home)
  
}

weather_df10 <- filter(weather_df, weather_df$data.season == 2010)
weather_df1 <- filter(weather_df, weather_df$data.season == 2011)
weather_df2 <- filter(weather_df, weather_df$data.season == 2012)
weather_df3 <- filter(weather_df, weather_df$data.season == 2013)
weather_df4 <- filter(weather_df, weather_df$data.season == 2014)
weather_df5 <- filter(weather_df, weather_df$data.season == 2015)
weather_df6 <- filter(weather_df, weather_df$data.season == 2016)
weather_df7 <- filter(weather_df, weather_df$data.season == 2017)
weather_df8 <- filter(weather_df, weather_df$data.season == 2018)
weather_df9 <- filter(weather_df, weather_df$data.season == 2019)

y2010 <- pmap(list(weather_df10$data.season, weather_df10$data.week, weather_df10$away_team, weather_df10$home_team), getweather1)
y2011 <- pmap(list(weather_df1$data.season, weather_df1$data.week, weather_df1$away_team, weather_df1$home_team), getweather)
y2012 <- pmap(list(weather_df2$data.season, weather_df2$data.week, weather_df2$away_team, weather_df2$home_team), getweather)
y2013 <- pmap(list(weather_df3$data.season, weather_df3$data.week, weather_df3$away_team, weather_df3$home_team), getweather)
y2014 <- pmap(list(weather_df4$data.season, weather_df4$data.week, weather_df4$away_team, weather_df4$home_team), getweather)
y2015 <- pmap(list(weather_df5$data.season, weather_df5$data.week, weather_df5$away_team, weather_df5$home_team), getweather)
y2016 <- pmap(list(weather_df6$data.season, weather_df6$data.week, weather_df6$away_team, weather_df6$home_team), getweather)
y2017 <- pmap(list(weather_df7$data.season, weather_df7$data.week, weather_df7$away_team, weather_df7$home_team), getweather)
y2018 <- pmap(list(weather_df8$data.season, weather_df8$data.week, weather_df8$away_team, weather_df8$home_team), getweather)
y2019 <- pmap(list(weather_df9$data.season, weather_df9$data.week, weather_df9$away_team, weather_df9$home_team), getweather)

years1 <- c(y2010, y2011, y2012, y2013, y2014, y2015)
years2 <- c(y2016, y2017, y2018, y2019)

cleanWeather <- function(game){
    game[[1]] %>% 
      head(8)
    
  
}
correct_weather <- function(year){
  map(year, cleanWeather)
  
}
years1 <- correct_weather(years1)
years2 <- correct_weather(years2)
years <- c(years1, years2)

  

rain <- sapply(years, "[", 2)
rain <- as.character(rain)
tempr <- sapply(years, "[", 3)
tempr <- str_extract(tempr, " \\w+")
wind <- sapply(years, "[", 4) %>%
  unlist()
humidity <- sapply(years, "[", 5)
visibility <- sapply(years, "[", 6)
barometer <- sapply(years, "[", 7)
barometer <- str_extract(barometer, "Barometer: \\w+")
dew_point <- sapply(years, "[", 8) 
dew_point  <- str_extract(dew_point, "Dew Point: \\w+")
wind1 <- sapply(years, "[", 5) %>%
  unlist()
visibility1 <- sapply(years, "[", 7)
humidity1 <- sapply(years, "[", 6)
barometer1 <- sapply(years, "[", 8)
barometer1 <- str_extract(barometer, "Barometer: \\w+")
dew_point1 <- sapply(years, "[", 9) 
dew_point1  <- str_extract(dew_point, "Dew Point: \\w+")

l <- (rain) 
l <- data.frame(sort(l$rain, decreasing = TRUE)) 



weather2 <- data.frame(weather_df, rain, tempr, humidity, visibility, barometer, dew_point)
weather <- data.frame(weather2, wind, humidity1, visibility1, barometer1, dew_point1, wind1)

write_csv(weather, "weather.csv")

weathercsv <- read_csv("weather.csv")
l <- data.frame(weathercsv$rain) 
l$snow <- str_detect(l$weathercsv.rain, "Snow")






