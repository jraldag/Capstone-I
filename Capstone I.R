# Pull athlete_events.csv data into R.
athlete_events <- read_csv("C:\\Users\\jrald\\Documents\\Data Analytics\\Capstone Projects\\Capstone I\\athlete_events.csv")


# View all athlete_events.csv data.
view(athlete_events)


# Begin data analysis.

# List of Olympic games.
list_games <- athlete_events %>%
  group_by(Year, Season, City) %>%
  summarize() %>%
  print(n = 52)


# Total number of Olympic seasons.
olympic_season_count=n_distinct(athlete_events$Games)
cat("There were",olympic_season_count,"Olympic seasons from Athens 1896 to Rio 2016.")

olympic_summer_count <- athlete_events %>%
  filter(Season == 'Summer')
olympic_summer_count=n_distinct(olympic_summer_count$Year)
cat("There were",olympic_summer_count,"Olympic summer seasons from Athens 1896 to Rio 2016.")

olympic_winter_count <- athlete_events %>%
  filter(Season == 'Winter')
olympic_winter_count=n_distinct(olympic_winter_count$Year)
cat("There were",olympic_winter_count,"Olympic winter seasons from Athens 1896 to Rio 2016.")


# Total number of athletes.
athlete_count=n_distinct(athlete_events$Name)
cat("A total of",athlete_count,"different athletes have participated in Olympic games from Athens 1896 to Rio 2016.")

athlete_summer_count <- athlete_events %>%
  filter(Season == 'Summer')
athlete_summer_count=n_distinct(athlete_summer_count$Name)
cat("A total of",athlete_summer_count,"different athletes have participated in Olympic summer games from Athens 1896 to Rio 2016.")

athlete_winter_count <- athlete_events %>%
  filter(Season == 'Winter')
athlete_winter_count=n_distinct(athlete_winter_count$Name)
cat("A total of",athlete_winter_count,"different athletes have participated in Olympic winter games from Athens 1896 to Rio 2016.")


# Total number of countries.
country_count=n_distinct(athlete_events$NOC)
cat("A total of",country_count,"different countries have participated in Olympic games from Athens 1896 to Rio 2016.")

country_summer_count <- athlete_events %>%
  filter(Season == 'Summer')
country_summer_count=n_distinct(country_summer_count$NOC)
cat("A total of",country_summer_count,"different countries have participated in Olympic summer games from Athens 1896 to Rio 2016.")

country_winter_count <- athlete_events %>%
  filter(Season == 'Winter')
country_winter_count=n_distinct(country_winter_count$NOC)
cat("A total of",country_winter_count,"different countries have participated in Olympic winter games from Athens 1896 to Rio 2016.")


# Total number of events and athletes per Olympic season.
summer_events <- athlete_events %>%
  filter(Season == 'Summer') %>%
  group_by(Year) %>%
  summarize(olympic_season = 'Summer', total_events = n_distinct(Event), total_athletes = n_distinct(Name)) %>%
  print(n = 29)

winter_events <- athlete_events %>%
  filter(Season == 'Winter') %>%
  group_by(Year) %>%
  summarize(olympic_season = 'Winter', total_events = n_distinct(Event), total_athletes = n_distinct(Name)) %>%
  print(n = 22)

colors <- c('Summer' = 'red', 'Winter' = 'blue')
ggplot() +
  geom_line(data = summer_events, aes(x = Year, y = total_events, color = olympic_season), linetype = 2, size = 1) +
  geom_line(data = winter_events, aes(x = Year, y = total_events, color = olympic_season), linetype = 2, size = 1) +
  geom_point(data = summer_events, aes(x = Year, y = total_events, color = olympic_season), size = 4) +
  geom_point(data = winter_events, aes(x = Year, y = total_events, color = olympic_season), size = 4) +
  labs(x = 'Year', y = 'Number of Olympic Events', color = 'Olympic Season') +
  scale_color_manual(values = colors) +
  scale_linetype("dashed") +
  theme(
    axis.title = element_text(color = 'black', size = 12, face = 'bold'),
    legend.title = element_text(color = 'black', size = 12, face = 'bold')) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 40))


colors <- c('Summer' = 'red', 'Winter' = 'blue')
ggplot() +
  geom_line(data = summer_events, aes(x = Year, y = total_athletes, color = olympic_season), linetype = 2, size = 1) +
  geom_line(data = winter_events, aes(x = Year, y = total_athletes, color = olympic_season), linetype = 2, size = 1) +
  geom_point(data = summer_events, aes(x = Year, y = total_athletes, color = olympic_season), size = 4) +
  geom_point(data = winter_events, aes(x = Year, y = total_athletes, color = olympic_season), size = 4) +
  labs(x = 'Year', y = 'Number of Olympic Athletes', color = 'Olympic Season') +
  scale_color_manual(values = colors) +
  theme(
    axis.title = element_text(color = 'black', size = 12, face = 'bold'),
    legend.title = element_text(color = 'black', size = 12, face = 'bold')) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 40)) 


# Total medals won by country for all Olympic seasons.
medals_won <- athlete_events %>%
  filter(Medal != 'NA') %>%
  group_by(NOC) %>%
  summarize(medals_won = n_distinct(Event, Medal))
print(medals_won[order(medals_won$medals_won, decreasing = TRUE), ], n = 25)

medals_won_summer <- athlete_events %>%
  filter(Season == 'Summer') %>%
  filter(Medal != 'NA') %>%
  group_by(NOC) %>%
  summarize(olympic_season = 'Summer', medals_won_summer = n_distinct(Event, Medal))
print(medals_won_summer[order(medals_won_summer$medals_won_summer, decreasing = TRUE), ], n = 25)

gold_medals_won_summer <- athlete_events %>%
  filter(Season == 'Summer') %>%
  filter(Medal == 'Gold') %>%
  group_by(NOC) %>%
  summarize(olympic_season = 'Summer', gold_medals_won_summer = n_distinct(Event, Medal))
print(gold_medals_won_summer[order(gold_medals_won_summer$gold_medals_won_summer, decreasing = TRUE), ], n = 107)

silver_medals_won_summer <- athlete_events %>%
  filter(Season == 'Summer') %>%
  filter(Medal == 'Silver') %>%
  group_by(NOC) %>%
  summarize(olympic_season = 'Summer', silver_medals_won_summer = n_distinct(Event, Medal))
print(silver_medals_won_summer[order(silver_medals_won_summer$silver_medals_won_summer, decreasing = TRUE), ], n = 127)

bronze_medals_won_summer <- athlete_events %>%
  filter(Season == 'Summer') %>%
  filter(Medal == 'Bronze') %>%
  group_by(NOC) %>%
  summarize(olympic_season = 'Summer', bronze_medals_won_summer = n_distinct(Event, Medal))
print(bronze_medals_won_summer[order(bronze_medals_won_summer$bronze_medals_won_summer, decreasing = TRUE), ], n = 124)

medals_won_winter <- athlete_events %>%
  filter(Season == 'Winter') %>%
  filter(Medal != 'NA') %>%
  group_by(NOC) %>%
  summarize(olympic_season = 'Winter', medals_won_winter = n_distinct(Event, Medal))
print(medals_won_winter[order(medals_won_winter$medals_won_winter, decreasing = TRUE), ], n = 46)

gold_medals_won_winter <- athlete_events %>%
  filter(Season == 'Winter') %>%
  filter(Medal == 'Gold') %>%
  group_by(NOC) %>%
  summarize(olympic_season = 'Winter', gold_medals_won_winter = n_distinct(Event, Medal))
print(gold_medals_won_winter[order(gold_medals_won_winter$gold_medals_won_winter, decreasing = TRUE), ], n = 38)

silver_medals_won_winter <- athlete_events %>%
  filter(Season == 'Winter') %>%
  filter(Medal == 'Silver') %>%
  group_by(NOC) %>%
  summarize(olympic_season = 'Winter', silver_medals_won_winter = n_distinct(Event, Medal))
print(silver_medals_won_winter[order(silver_medals_won_winter$silver_medals_won_winter, decreasing = TRUE), ], n = 41)

bronze_medals_won_winter <- athlete_events %>%
  filter(Season == 'Winter') %>%
  filter(Medal == 'Bronze') %>%
  group_by(NOC) %>%
  summarize(olympic_season = 'Winter', bronze_medals_won_winter = n_distinct(Event, Medal))
print(bronze_medals_won_winter[order(bronze_medals_won_winter$bronze_medals_won_winter, decreasing = TRUE), ], n = 40)

medals_won %>%
  arrange(desc(medals_won)) %>%
  slice(1:5) %>%
  ggplot(., aes(x = NOC, y = medals_won, fill = NOC)) +
  geom_col() +
  labs(title = "Top 5 Countries in Olympic Medals Won from 1896 to 2016",
       x = "Country",
       y = "Medal Count") +
  guides(fill = FALSE) +
  scale_y_continuous(breaks = seq(100, 1000, by = 100)) +
  theme(
    plot.title = element_text(color = 'black', size = 12, face = 'bold'),
    axis.title.x = element_text(color = 'black', size = 12, face = 'bold'),
    axis.title.y = element_text(color = 'black', size = 12, face = 'bold'))

medals_won_summer %>%
  arrange(desc(medals_won_summer)) %>%
  slice(1:5) %>%
  ggplot(., aes(x = NOC, y = medals_won_summer, fill = NOC)) +
  geom_col() +
  labs(title = "Top 5 Countries in Summer Olympic Medals Won",
       x = "Country",
       y = "Medal Count") +
  guides(fill = FALSE) +
  scale_y_continuous(breaks = seq(100, 1000, by = 100)) +
  theme(
    plot.title = element_text(color = 'black', size = 12, face = 'bold'),
    axis.title.x = element_text(color = 'black', size = 12, face = 'bold'),
    axis.title.y = element_text(color = 'black', size = 12, face = 'bold'))

medals_won_winter %>%
  arrange(desc(medals_won_winter)) %>%
  slice(1:5) %>%
  ggplot(., aes(x = NOC, y = medals_won_winter, fill = NOC)) +
  geom_col() +
  labs(title = "Top 5 Countries in Winter Olympic Medals Won",
       x = "Country",
       y = "Medal Count") +
  guides(fill = FALSE) +
  scale_y_continuous(breaks = seq(10, 500, by = 10)) +
  theme(
    plot.title = element_text(color = 'black', size = 12, face = 'bold'),
    axis.title.x = element_text(color = 'black', size = 12, face = 'bold'),
    axis.title.y = element_text(color = 'black', size = 12, face = 'bold'))

# Total medals won by country per Olympic season.
# Total medals won by country per Olympic event/season.
# Trend medals won by USA in each Olympic season.
# Trend medals won by USA per Olympic event.
# Which NOC hasn't won a gold, ever.