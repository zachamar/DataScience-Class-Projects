##Setup Flight Data & Packages
library(readr)
library(dplyr)
require(Hmisc)

## get main data set
flights <- read_csv("Downloads/434 Project/flights.csv")
attach(flights)

## Summary Statistics
describe(flights)

## cut down to only necessary columns
flights = flights %>%
  select("YEAR", "MONTH", "DAY", "DAY_OF_WEEK", "AIRLINE", "ORIGIN_AIRPORT", "DESTINATION_AIRPORT", "DEPARTURE_DELAY", "ARRIVAL_DELAY", "DIVERTED", "CANCELLED", "CANCELLATION_REASON", "AIR_SYSTEM_DELAY", "SECURITY_DELAY", "AIRLINE_DELAY", "LATE_AIRCRAFT_DELAY", "WEATHER_DELAY")

## Join in full airline names from another CSV
airlines <- read_csv("Downloads/434 Project/airlines.csv")
attach(airlines)
flights = left_join(flights, airlines, by = c("AIRLINE" = "IATA_CODE"))

## rename column
names(flights)[18]<-"AIRLINE_NAME"

## Most Flights Flown
flights %>%
  group_by(AIRLINE_NAME) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

## Most Delayed (arrival)
flights %>%
  group_by(AIRLINE_NAME) %>%
  summarise(avg_delay = mean(ARRIVAL_DELAY, na.rm = TRUE)) %>%
  arrange(desc(avg_delay))

## Most Delayed (departure)
flights %>%
  group_by(AIRLINE_NAME) %>%
  summarise(avg_delay = mean(DEPARTURE_DELAY, na.rm = TRUE)) %>%
  arrange(desc(avg_delay))

## Most Delayed (arrival)
flights %>%
  group_by(AIRLINE_NAME) %>%
  summarise(avg_delay = mean(ARRIVAL_DELAY, na.rm = TRUE)) %>%
  arrange(desc(avg_delay))

## Most Delayed by Late Aircraft (relative frequency)
late_aircr_delays = flights %>%
  group_by(AIRLINE_NAME) %>%
  filter(LATE_AIRCRAFT_DELAY > 0) %>%
  tally()
late_aircr_delays

avg_delays = left_join(flight_freq, late_aircr_delays, by = "AIRLINE_NAME")
avg_delays$relative_freq = avg_delays$n/avg_delays$count

avg_delays %>%
  arrange(desc(relative_freq))

## Most Delayed by Airline Issues (relative frequency)
airline_delay = flights %>%
  group_by(AIRLINE_NAME) %>%
  filter(AIRLINE_DELAY > 0) %>%
  tally()
airline_delay

avg_airline_delay = left_join(flight_freq, airline_delay, by = "AIRLINE_NAME")
avg_airline_delay$relative_freq = avg_airline_delay$n/avg_airline_delay$count

avg_airline_delay %>%
  arrange(desc(relative_freq))

## Most Delayed by Weather (relative frequency)
weather_delay = flights %>%
  group_by(AIRLINE_NAME) %>%
  filter(WEATHER_DELAY > 0) %>%
  tally()

avg_weather_delay = left_join(flight_freq, weather_delay, by = "AIRLINE_NAME")
avg_weather_delay$relative_freq = avg_weather_delay$n/avg_weather_delay$count

avg_weather_delay %>%
  arrange(desc(relative_freq))

## Most Cancelled (percentage)
flights %>%
  group_by(AIRLINE_NAME) %>%
  summarise(pct_cancelled = sum(CANCELLED, na.rm = TRUE)/n()) %>%
  arrange(desc(pct_cancelled))

## Most Diverted (percentage)
flights %>%
  group_by(AIRLINE_NAME) %>%
  summarise(pct_divert = sum(DIVERTED, na.rm = TRUE)/n()) %>%
  arrange(desc(pct_divert))

#looking at sentiment
## rank by rating
Airline_Sentiment_Clean <- read_csv("Downloads/434 Project/Airline_Sentiment_Clean.csv")
avg_rating = Airline_Sentiment_Clean %>%
  group_by(airline) %>%
  summarise(avg_rating = mean(`rating (1-5)`)) %>%
  arrange(desc(avg_rating))
avg_rating 

## convert sentiment to numeric from character
Airline_Sentiment_Clean$Positive = as.numeric(Airline_Sentiment_Clean$Positive)
Airline_Sentiment_Clean$Negative = as.numeric(Airline_Sentiment_Clean$Negative)

## create a net sentiment variable
Airline_Sentiment_Clean$net_sentiment = Airline_Sentiment_Clean$Positive + Airline_Sentiment_Clean$Negative

#rank by sentiment
avg_sentiment = Airline_Sentiment_Clean %>%
  group_by(airline) %>%
  summarise(avg_sent = mean(net_sentiment)) %>%
  arrange(desc(avg_sent))
avg_sentiment

## Transform the data for graphing 

reduced_data = flights %>%
  select("ORIGIN_AIRPORT", "DESTINATION_AIRPORT", "AIRLINE_NAME")
write.csv(reduced_data, file="reduced_flights.csv")
##read this back in after converting in Excel to text 

## get SW flight network for graphing
Sw_flight_routes = reduced_flights %>%
  filter(AIRLINE_NAME == "Southwest Airlines Co.") %>%
  group_by(ORIGIN_AIRPORT, DESTINATION_AIRPORT) %>%
  tally()

View(Sw_flight_routes)
write.csv(Sw_flight_routes, file="SW_flight_network.csv")

## get American Airlines flight network for graphing
AA_flight_routes = reduced_flights %>%
  filter(AIRLINE_NAME == "American Airlines Inc.") %>%
  group_by(ORIGIN_AIRPORT, DESTINATION_AIRPORT) %>%
  tally()

View(AA_flight_routes)
write.csv(AA_flight_routes, file="AA_flight_network.csv")

