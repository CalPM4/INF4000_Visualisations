#BEFORE RUNNING THE CODE, THE WORKING DIRECTORY MUST BE SET AS WHERE ALL THE CSV FILES ARE FOR
#THE MATCH DATA

install.packages("tidyverse")
install.packages("ggmap")
install.packages("treemapify")

library(tidyverse)
library(dplyr)

#Reading in every UEFA Euro Tournament
Euros1960 <- read_csv("1960.csv")
Euros1964 <- read_csv("1964.csv")
Euros1968 <- read_csv("1968.csv")
Euros1972 <- read_csv("1972.csv")
Euros1976 <- read_csv("1976.csv")
Euros1980 <- read_csv("1980.csv")
Euros1984 <- read_csv("1984.csv")
Euros1988 <- read_csv("1988.csv")
Euros1992 <- read_csv("1992.csv")
Euros1996 <- read_csv("1996.csv")
Euros2000 <- read_csv("2000.csv")
Euros2004 <- read_csv("2004.csv")
Euros2008 <- read_csv("2008.csv")
Euros2012 <- read_csv("2012.csv")
Euros2016 <- read_csv("2016.csv")
Euros2020 <- read_csv("2020.csv")
Euros2024 <- read_csv("2024.csv")

#Selecting only the columns that are necessary to me from each UEFA Euro Tournament
Euros1960Selected <- select(Euros1960, round, year, date, home_team, away_team, home_team_code,
                            away_team_code, home_score, away_score, match_attendance,
                            stadium_capacity, stadium_latitude, stadium_longitude, stadium_city)
Euros1964Selected <- select(Euros1964, round, year, date, home_team, away_team, home_team_code,
                            away_team_code, home_score, away_score, match_attendance,
                            stadium_capacity, stadium_latitude, stadium_longitude, stadium_city)
Euros1968Selected <- select(Euros1968, round, year, date, home_team, away_team, home_team_code,
                            away_team_code, home_score, away_score, match_attendance,
                            stadium_capacity, stadium_latitude, stadium_longitude, stadium_city)
Euros1972Selected <- select(Euros1972, round, year, date, home_team, away_team, home_team_code,
                            away_team_code, home_score, away_score, match_attendance,
                            stadium_capacity, stadium_latitude, stadium_longitude, stadium_city)
Euros1976Selected <- select(Euros1976, round, year, date, home_team, away_team, home_team_code,
                            away_team_code, home_score, away_score, match_attendance,
                            stadium_capacity, stadium_latitude, stadium_longitude, stadium_city)
Euros1980Selected <- select(Euros1980, round, year, date, home_team, away_team, home_team_code,
                            away_team_code, home_score, away_score, match_attendance,
                            stadium_capacity, stadium_latitude, stadium_longitude, stadium_city)
Euros1984Selected <- select(Euros1984, round, year, date, home_team, away_team, home_team_code,
                            away_team_code, home_score, away_score, match_attendance,
                            stadium_capacity, stadium_latitude, stadium_longitude, stadium_city)
Euros1988Selected <- select(Euros1988, round, year, date, home_team, away_team, home_team_code,
                            away_team_code, home_score, away_score, match_attendance,
                            stadium_capacity, stadium_latitude, stadium_longitude, stadium_city)
Euros1992Selected <- select(Euros1992, round, year, date, home_team, away_team, home_team_code,
                            away_team_code, home_score, away_score, match_attendance,
                            stadium_capacity, stadium_latitude, stadium_longitude, stadium_city)
Euros1996Selected <- select(Euros1996, round, year, date, home_team, away_team, home_team_code,
                            away_team_code, home_score, away_score, match_attendance,
                            stadium_capacity, stadium_latitude, stadium_longitude, stadium_city)
Euros2000Selected <- select(Euros2000, round, year, date, home_team, away_team, home_team_code,
                            away_team_code, home_score, away_score, match_attendance,
                            stadium_capacity, stadium_latitude, stadium_longitude, stadium_city)
Euros2004Selected <- select(Euros2004, round, year, date, home_team, away_team, home_team_code,
                            away_team_code, home_score, away_score, match_attendance,
                            stadium_capacity, stadium_latitude, stadium_longitude, stadium_city)
Euros2008Selected <- select(Euros2008, round, year, date, home_team, away_team, home_team_code,
                            away_team_code, home_score, away_score, match_attendance,
                            stadium_capacity, stadium_latitude, stadium_longitude, stadium_city)
Euros2012Selected <- select(Euros2012, round, year, date, home_team, away_team, home_team_code,
                            away_team_code, home_score, away_score, match_attendance,
                            stadium_capacity, stadium_latitude, stadium_longitude, stadium_city)
Euros2016Selected <- select(Euros2016, round, year, date, home_team, away_team, home_team_code,
                            away_team_code, home_score, away_score, match_attendance,
                            stadium_capacity, stadium_latitude, stadium_longitude, stadium_city)
Euros2020Selected <- select(Euros2020, round, year, date, home_team, away_team, home_team_code,
                            away_team_code, home_score, away_score, match_attendance,
                            stadium_capacity, stadium_latitude, stadium_longitude, stadium_city)
Euros2024Selected <- select(Euros2024, round, year, date, home_team, away_team, home_team_code,
                            away_team_code, home_score, away_score, match_attendance,
                            stadium_capacity, stadium_latitude, stadium_longitude, stadium_city)


#Combining all the tournaments together in the same data frame
AllTournaments <- Euros1960Selected %>%
  bind_rows(Euros1964Selected) %>%
  bind_rows(Euros1968Selected) %>%
  bind_rows(Euros1972Selected) %>%
  bind_rows(Euros1976Selected) %>%  
  bind_rows(Euros1980Selected) %>%  
  bind_rows(Euros1984Selected) %>%  
  bind_rows(Euros1988Selected) %>%  
  bind_rows(Euros1992Selected) %>%  
  bind_rows(Euros1996Selected) %>%  
  bind_rows(Euros2000Selected) %>%  
  bind_rows(Euros2004Selected) %>%  
  bind_rows(Euros2008Selected) %>%
  bind_rows(Euros2012Selected) %>%
  bind_rows(Euros2016Selected) %>%  
  bind_rows(Euros2020Selected) %>%  
  bind_rows(Euros2024Selected)

#Sorting the table by capacity, with attendance as the secondary sorting column
SortedByCapacity <- arrange(AllTournaments, desc(stadium_capacity), desc(match_attendance))

### MAP PLOT ###
library(ggmap)

install.packages("ggtext")
library(ggtext)

register_stadiamaps("51e2a035-8965-4ec3-8081-1aeb81adda9f", FALSE)

#Mapping where games have been played
#The 'theme()' function removes the 'lat' and 'lon' axis labels as well as the markers
Europe <- c(left=-25.93, bottom=34.52, right=55, top=70.05)
get_stadiamap(Europe, zoom=5, maptype="stamen_toner_lite") %>%
  ggmap()+
  geom_point(data=AllTournaments, aes(x=stadium_longitude, y=stadium_latitude), 
             size=3, colour=rgb(0,0,1,0.15), position=position_jitter(w=0.5, h=0.5))+
#Adding a line to split Europe into West and East
#The line is at Zagreb because from West to East of the 41 different countries to play in 
#European Championships, Croatia is the 21st value (the midpoint) and Zagreb is the capital of Croatia. 
  geom_segment(data = EuropeanCapitals, 
               aes(x=15.978817124478496, y=70.05, xend=15.978817124478496, yend=34.52), 
               colour="red", size=1, alpha=0.2)+
  labs(title="Stadiums That Have Hosted a UEFA European Championship Match",
       caption="(Data: UEFA Euro 1960-2024)")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  geom_richtext(aes(x=-14, y=69, label="**WESTERN EUROPE**"))+
  geom_richtext(aes(x=43, y=69, label="**EASTERN EUROPE**"))

#Count of where the nations have got to
RoundCountHome <- count(AllTournaments, round, home_team_code, home_team)

RoundCountAway <- count(AllTournaments, round, away_team_code, away_team)

### STACKED BAR CHART ###
#Creating a stacked bar chart for frequency of appearances in each round by nation

#Changing column names so I can combine the data frames of the Home and Away teams
HomeTeams <- select(AllTournaments, round, home_team, home_team_code)
colnames(HomeTeams)[which(names(HomeTeams) == "home_team")] <- "Nation"
colnames(HomeTeams)[which(names(HomeTeams) == "home_team_code")] <- "Nation Code"

AwayTeams <- select(AllTournaments, round, away_team, away_team_code)
colnames(AwayTeams)[which(names(AwayTeams) == "away_team")] <- "Nation"
colnames(AwayTeams)[which(names(AwayTeams) == "away_team_code")] <- "Nation Code"

AllTeams <- HomeTeams %>%
  bind_rows(AwayTeams)

#Creating a data frame of the capital cities of nations that have competed in the European
#Championships to be able to reorder the rows in the stacked bar chart based on the latitude
#of each capital city

Nation <- c("Albania", "Austria", "Belgium", "Bulgaria", 
            "Commonwealth of Independent States", "Croatia", "Czechia", "Czechoslovakia",
            "Denmark", "England", "Finland", "France", "Georgia", "Germany", "Greece",
            "Hungary", "Iceland", "Italy", "Latvia", "Netherlands", "North Macedonia",
            "Northern Ireland", "Norway", "Poland", "Portugal", "Republic of Ireland",
            "Romania", "Russia", "Scotland", "Serbia", "Slovakia", "Slovenia", "Spain",
            "Sweden", "Switzerland", "Türkiye", "Ukraine", "USSR", "Wales", "West Germany",
            "Yugoslavia")

Capital <- c("Tirana", "Vienna", "Brussels", "Sofia", "Moscow", "Zagreb", "Prague", "Prague",
             "Copenhagen", "London", "Helsinki", "Paris", "Tbilisi", "Berlin", "Athens",
             "Budapest", "Reykjavik", "Rome", "Riga", "Amsterdam", "Skopje", "Belfast", "Oslo",
             "Warsaw", "Lisbon", "Dublin", "Bucharest", "Moscow", "Edinburgh", "Belgrade",
             "Bratislava", "Ljubljana", "Madrid", "Stockholm", "Bern", "Ankara", "Kyiv",
             "Moscow", "Cardiff", "Bonn", "Belgrade")

#Latitudes acquired from Google Maps
CapitalLongitudes <- c("19.8218910264885", "16.369761540822523", "4.355930415814893",
                       "23.327666779600438", "37.622870988676695", "15.978817124478496",
                       "14.436627794890542", "14.436627794890542", "12.555225357678223",
                       "-0.12701913708442597", "24.934944851932038", "2.348798143997351",
                       "44.80255110327942", "13.407755960865636", "23.7309573326683",
                       "19.05759005740737", "-21.93762350963081", "12.494998842138724",
                       "54.12220557856849", "4.907331542777053", "21.426492128640337",
                       "-5.922431705688854", "10.758995911755141", "21.003685654552342",
                       "-9.139454297684402", "-6.258862239771413", "26.099870926206364",
                       "37.622870988676695", "-3.1874115436642665", "20.46356867326226",
                       "17.11851476528249", "14.511059849827804", "-3.6941742992510544",
                       "18.05507348452372", "7.447398347866268", "32.86029029122721",
                       "30.522352697480844", "37.622870988676695", "-3.167978554715528",
                       "7.101328565529626", "20.46356867326226")

EuropeanCapitals <- data.frame(Nation, Capital, CapitalLongitudes)

#Changing the Round names
AllTeams <- AllTeams %>%
  mutate(round=recode(round, FINAL="Final", THIRD_PLAY_OFF="Third Place Playoff",
                        SEMIFINAL="Semi Final", QUARTER_FINALS="Quarter Final",
                        ROUND_OF_16="Round of 16", GROUP_STANDINGS="Group Stage"))


#Merging the 'EuropeanCapitals' data frame with the 'AllTeams' data frame
AllTeams <- merge(AllTeams, EuropeanCapitals,by="Nation")

#Reordering the way the bar chart will be stacked, from top down
AllTeams$round <- factor(AllTeams$round, levels=c("Final", "Third Place Playoff", "Semi Final", 
                                                  "Quarter Final", "Round of 16", "Group Stage"))

#Converting the Longitudes to numeric data
AllTeams$CapitalLongitudes <- as.numeric(AllTeams$CapitalLongitudes)

#Downloading a colour palette that is accessible for colour blind people
library(viridis)

#Plotting the stacked bar chart
ggplot(AllTeams, aes(x = reorder(Nation, CapitalLongitudes), fill = round))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  labs(x="Nation (Ordered: West to East)", y = "Frequency", fill = "Round",
       title="Frequency of Appearances in Each Round of the European Championships",
       caption="*Commonwealth of Independent States (CIS) is nations made up of the former Soviet Union\n
                  **CIS, Czechoslovakia, USSR, West Germany and Yugoslavia no longer exist\n
                  (Data: UEFA Euro 1960-2024)")+
  scale_fill_viridis(discrete = TRUE)

### LINE CHART ###
#Creating a line chart mapping Attendances vs Capacity of Stadia

#Creating a data frame with the average capacities and attendances from each tournament
CapacityAndAttendance <- AllTournaments %>%
  group_by(year) %>%
  summarize(AverageAttendance = mean(match_attendance),
    AverageCapacity = mean(stadium_capacity))

#Plotting the line chart

ggplot(CapacityAndAttendance, aes(x = year))+
  theme_bw()+
  geom_line(aes(y=AverageAttendance, col="Average Match\nAttendance"), size=1.5)+
  geom_line(aes(y=AverageCapacity, col="Average Stadium\nCapacity"), size=1.5)+
  labs(x="Year", y="Attendance and Capacity",
       title="Attendance vs Capacity at each UEFA European Championships Tournament",
       caption="(Data: UEFA Euro 1960-2024)", colour="Key")+
  #Setting the colours of each line  
  scale_fill_viridis(discrete=TRUE)+
  scale_x_continuous(breaks=seq(1960, 2024, by=4))+
  scale_y_continuous(breaks=seq(20000, 90000, by=10000))

### TREE MAP ###

#Changing the column names so the data frames can be combined
HomeGoals <- select(AllTournaments, home_team, home_team_code, home_score)
colnames(HomeGoals)[which(names(HomeGoals) == "home_team")] <- "Nation"
colnames(HomeGoals)[which(names(HomeGoals) == "home_team_code")] <- "NationCode"
colnames(HomeGoals)[which(names(HomeGoals) == "home_score")] <- "Goals"

AwayGoals <- select(AllTournaments, away_team, away_team_code, away_score)
colnames(AwayGoals)[which(names(AwayGoals) == "away_team")] <- "Nation"
colnames(AwayGoals)[which(names(AwayGoals) == "away_team_code")] <- "NationCode"
colnames(AwayGoals)[which(names(AwayGoals) == "away_score")] <- "Goals"

#Combining (binding) the data frames
AllGoals <- HomeGoals %>%
  bind_rows(AwayGoals)

#Grouping the goals by nation, so each nation only has one row with their total goals
GoalsByNation <- AllGoals %>%
  group_by(Nation, NationCode) %>%
  summarise(TotalGoals = sum(Goals))

#Merging the data frames so that the capital longitudes are present to be able to label each
#nation as 'Western Europe' or 'Eastern Europe'
GoalsByNation <- merge(GoalsByNation, EuropeanCapitals, by="Nation")

#Converting CapitalLongitudes data to be numeric
GoalsByNation$CapitalLongitudes <- as.numeric(GoalsByNation$CapitalLongitudes)
WestToEast$CapitalLongitudes <- as.numeric(WestToEast$CapitalLongitudes)

#Arranging the nations so they are listed from furthest West to furthest East
WestToEast <- GoalsByNation %>% arrange(CapitalLongitudes)

GoalsByNation <- GoalsByNation %>%
  mutate(Region=ifelse(CapitalLongitudes >= 15.978817124478496, "Eastern Europe", "Western Europe"))

#Creating the plot
library(treemapify)

ggplot(GoalsByNation, aes(area = TotalGoals, fill = CapitalLongitudes, label = Nation, subgroup = Region))+
  geom_treemap()+geom_treemap_text(colour="white", place="centre")+
  geom_treemap_subgroup_border() + geom_treemap_subgroup_text(colour="gold", fontface="bold")+
  labs(title="Proportion of Total Goals Scored by each Nation at The European Championships",
       fill="Longitude\nof the\nNation's\nCapital City",
       caption="(Data: UEFA Euro 1960-2024)")+
  scale_fill_gradient(low="deepskyblue", high="purple4")