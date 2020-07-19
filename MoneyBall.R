# Importing required libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(ggthemes)

# Reading the csv file containing the batting details of each players
batting <- read.csv('Batting.csv')

# Feature Engineering
# We need 3 more statistics values that are used in MoneyBall
batting$BA <- batting$H / batting$AB

batting$OBP <- (batting$H + batting$BB + batting$HBP) / (batting$AB + batting$BB + batting$HBP + batting$SF)

batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

batting$SLG <- ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR)) / batting$AB

# Reading the Salary details of the players
sal <- read.csv("Salaries.csv")

batting <- subset(batting, yearID >= 1985)

# Merging Salary Data with Batting Data
combo <- merge(batting,sal,by=c('playerID','yearID'))


# Analyzing the Lost Players
lost_players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01'))

lost_players <- subset(lost_players,yearID == 2001)

lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]

# Find the replacement players who are under valued
# Replacement conditions
# 1469 AB
# AVG 0.364 OBP
# 15 Million

combo <- subset(combo, yearID == 2001)

print(ggplot(combo, aes(x=OBP, y=salary)) + geom_point(size =2))

# Filtering out the data
combo <- subset(combo,salary < 8000000 & OBP > 0)
combo <- subset(combo, AB >= 450)

options <- head(arrange(combo,desc(OBP)),10)

print(options[,c('playerID','AB','salary','OBP')])

pl <- ggplot(options,aes(x=OBP, y=salary)) + geom_point(size = 1, color = 'red')
pl2 <- pl + xlab('On Base %') + ylab('Salary') + ggtitle("Replacement Players")
pl3 <- pl2 + theme_dark()

gpl <- ggplotly(pl3)
print(gpl)





