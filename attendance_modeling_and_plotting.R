library(readr)
library(tidyverse)
library(stargazer)
library(car)
Attendance_Model <- read_csv("~/Desktop/SAL 213/module 4/Attendance Model.csv")
Attendance_Model$humidity <- str_extract(Attendance_Model$humidity, "\\d+") 
Attendance_Model$humidity <- as.numeric(Attendance_Model$humidity)
Attendance_Model$Monday <- ifelse(Attendance_Model$weekday.x == "Monday", 1, 0)
Attendance_Model$OtherweekDays <- 
  ifelse(Attendance_Model$weekday.x == "Tuesday" | Attendance_Model$weekday.x == "Wednesday"|
         Attendance_Model$weekday.x == "Friday" | Attendance_Model$weekday.x == "Saturday", 1, 0)
Attendance_Model$Thursday <- ifelse(Attendance_Model$weekday.x == "Thursday", 1, 0)
Attendance_Model$Sunday <- ifelse(Attendance_Model$weekday.x == "Sunday", 1, 0)
Attendance_Model$Dome <- ifelse(Attendance_Model$roof.x == "dome", 1, 0)
Attendance_Model$Outdoors <- ifelse(Attendance_Model$roof.x == "outdoors", 1, 0)
Attendance_Model$Open <- ifelse(Attendance_Model$roof.x == "open", 1, 0)


Attendance_Model$primetime <- ifelse(Attendance_Model$Monday == 1 | Attendance_Model$Thursday == 1 |
                                       Attendance_Model$Sunday == 1 | Attendance_Model$weekday.x == "Saturday"
                                     & Attendance_Model$gametime.x > "20:30:00",
                                     1, 0)

Attendance_Model2 <- filter(Attendance_Model,  is.na(Attendance_Model$wind.x))

model <- lm(weekly_attendance ~ temper + I(temper^2) + rain + humidity +
              snow + wind.x , data = Attendance_Model)
summary(model)

model2 <- lm(weekly_attendance ~ temper + I(temper^2) + rain + humidity +
              snow + Open + Outdoors, data = Attendance_Model)
summary(model2)


stargazer(model, type = "text", title = "Weather Model on Attendance",
          out = "Weather Model on Attendance.html", 
          covariate.labels = c("Temperature (ºF)", "Temperature^2 (ºF)", 
                               "Rain", "Humidity (%)", "Snow", "Wind"))

stargazer(model2, type = "text", title = "Weather Model on Indoor Attendance",
          out = "Weather Model on Indoor Attendance.html", 
          covariate.labels = c("Temperature (ºF)", "Temperature^2 (ºF)", 
                               "Rain", "Humidity (%)", "Snow", "Open", "Outdoors"))

stargazer(Attendance_Model[c("temper", "rain", "humidity", "snow", "wind.x")], 
          type = "text", title="Table 1: Summary Statistics",
          out="table1.html")


model3 <- lm(weekly_attendance ~ Monday + Thursday + Sunday + primetime + 
               total_line.x + div_game.x + spread_line.x  +
               star_power + away_wins + home_wins + epa_home +
               home_team.x, data = Attendance_Model)


summary(model3)

stargazer(model3, type = "text", title = "Game Importance on Attendance",
          out = "Game Importance on Attendance.txt", 
          covariate.labels = c("Monday", "Thursday", "Sunday", "Prime Time", "Over/Under", 
                               "Division Game", "Spread Odds", "Star Power on Team", "Away Record", "Home Record",
                               "Home EPA", "Falcons", "Ravens", "Bills", "Panthers", "Bears", "Bengals",
                               "Browns", "Cowboys", "Broncos", "Lions", "Packers", "Texans", 
                               "Colts", "Jaguars", "Chiefs", "LA Rams", "LA Chargers", "Dolphins",
                               "Vikings", "Patriots", "Saints", "Giants", "Jets", "Eagles", 
                               "Steelers", "SD Chargers", "Seahawks", "49ers", "STL Rams", 
                               "Buccaneers", "Titans", "Commanders"))
vif(model3)
anova(model3)

library(interplot)

interplot(m=model, var2 = "temper", var1 = "temper", ercolor = 'brown1', ci = .8) +
  xlab("Temperature Values") +
  ylab("Marginal Effect on Attendance") + 
  ggtitle('Marginal Effect of Temperature on Attendance') +
  theme_bw() +
  geom_line(col = 'blue4', size = 1) + 
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'brown1', size = 0.5)

sumry <- Attendance_Model %>%
  select(home_epa, spread_line.x, total_line.x, home_wins, away_wins)
summary(sumry)
sd(sumry$home_epa)
sd(sumry$spread_line.x)
sd(sumry$total_line.x)
sd(sumry$home_wins)
sd(sumry$away_wins)
