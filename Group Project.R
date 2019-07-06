library(readr)
library(dplyr)
library(reshape2)


#combined <- read_csv("combineduary 2019.csv")

my_col_types <- cols(
  FL_DATE = col_date(format = ""),
  OP_UNIQUE_CARRIER = col_factor(),
  OP_CARRIER_FL_NUM = col_character(),
  ORIGIN = col_factor(),
  ORIGIN_CITY_NAME = col_character(),
  ORIGIN_STATE_ABR = col_factor(),
  DEST = col_factor(),
  DEST_CITY_NAME = col_character(),
  DEST_STATE_ABR = col_factor(),
  DEP_TIME = col_character(),
  DEP_DELAY = col_double(),
  ARR_TIME = col_character(),
  ARR_DELAY = col_double(),
  CANCELLED = col_double(),
  CANCELLATION_CODE = col_character(),
  CARRIER_DELAY = col_double(),
  WEATHER_DELAY = col_double(),
  NAS_DELAY = col_double(),
  SECURITY_DELAY = col_double(),
  LATE_AIRCRAFT_DELAY = col_double(),
  X21 = col_skip())

jan <- read_csv("January 2019.csv", col_types = my_col_types, na = "")
feb <- read_csv("February 2019.csv", col_types = my_col_types, na = "")
nov <- read_csv("November 2018.csv", col_types = my_col_types, na = "")
dec <- read_csv("December 2018.csv", col_types = my_col_types, na = "")

rbind1 <- rbind(jan, feb)
rbind2 <- rbind(nov, dec)
combined <- rbind(rbind1, rbind2)

names(combined)[names(combined) == "FL_DATE"] <- "Date"
names(combined)[names(combined) == "OP_UNIQUE_CARRIER"] <- "Carrier"
names(combined)[names(combined) == "OP_CARRIER_FL_NUM"] <- "FlightNumber"
names(combined)[names(combined) == "ORIGIN"] <- "DepAirport"
names(combined)[names(combined) == "ORIGIN_CITY_NAME"] <- "DepCity"
names(combined)[names(combined) == "ORIGIN_STATE_ABR"] <- "DepState"
names(combined)[names(combined) == "DEST"] <- "ArrAirport"
names(combined)[names(combined) == "DEST_CITY_NAME"] <- "ArrCity"
names(combined)[names(combined) == "DEST_STATE_ABR"] <- "ArrState"
names(combined)[names(combined) == "DEP_TIME"] <- "DepTime"
names(combined)[names(combined) == "DEP_DELAY"] <- "DepDelay"
names(combined)[names(combined) == "DEST_STATE_ABR"] <- "ArrState"
names(combined)[names(combined) == "ARR_TIME"] <- "ArrTIme"
names(combined)[names(combined) == "ARR_DELAY"] <- "ArrDelay"
names(combined)[names(combined) == "CANCELLED"] <- "Cancelled"
names(combined)[names(combined) == "CANCELLATION_CODE"] <- "CancelReason"
names(combined)[names(combined) == "CARRIER_DELAY"] <- "CarrierDelay"
names(combined)[names(combined) == "WEATHER_DELAY"] <- "WeatherDelay"
names(combined)[names(combined) == "NAS_DELAY"] <- "NASDelay"
names(combined)[names(combined) == "SECURITY_DELAY"] <- "SecurityDelay"
names(combined)[names(combined) == "LATE_AIRCRAFT_DELAY"] <- "LateAircraftDelay"

# Replace CARRIER code with name
levels(combined$Carrier)[levels(combined$Carrier) == "AA"] <- "American"
levels(combined$Carrier)[levels(combined$Carrier) == "MQ"] <- "Envoy"
levels(combined$Carrier)[levels(combined$Carrier) == "9E"] <- "Endeavor"
levels(combined$Carrier)[levels(combined$Carrier) == "G4"] <- "Allegiant"
levels(combined$Carrier)[levels(combined$Carrier) == "OH"] <- "Comair"
levels(combined$Carrier)[levels(combined$Carrier) == "NK"] <- "Spirit"
levels(combined$Carrier)[levels(combined$Carrier) == "OO"] <- "SkyWest"
levels(combined$Carrier)[levels(combined$Carrier) == "DL"] <- "Delta"
levels(combined$Carrier)[levels(combined$Carrier) == "YV"] <- "Mesa"
levels(combined$Carrier)[levels(combined$Carrier) == "EV"] <- "ExpressJey"
levels(combined$Carrier)[levels(combined$Carrier) == "YX"] <- "Republic"
levels(combined$Carrier)[levels(combined$Carrier) == "UA"] <- "United"
levels(combined$Carrier)[levels(combined$Carrier) == "WN"] <- "Southwest"
levels(combined$Carrier)[levels(combined$Carrier) == "AS"] <- "Alaska"
levels(combined$Carrier)[levels(combined$Carrier) == "F9"] <- "Frontier"
levels(combined$Carrier)[levels(combined$Carrier) == "B6"] <- "JetBlue"
levels(combined$Carrier)[levels(combined$Carrier) == "HA"] <- "Hawaiian"

#Change cancellation codes to cancel reason
levels(combined$CancelReason)[levels(combined$CancelReason) == "A"] <-  "Carrier"
levels(combined$CancelReason)[levels(combined$CancelReason) == "B"]<- "Weather"
levels(combined$CancelReason)[levels(combined$CancelReason) == "C"] <- "National Air System"
levels(combined$CancelReason)[levels(combined$CancelReason) == "D"] <- "Security"

#Need to read library after data import.  Scales interferes with my_col_types read.
library(ggplot2)
library(scales)

# select midwest cities with airports.  Note that Chicago has 2 airports, MDW and ORD
x <- c("Chicago, IL", "Moline, IL", "Rockford, IL", "Peoria, IL", "Cedar Rapids/Iowa City, IA", "Des Moines, IA", "St. Louis, MO", 
       "Minneapolis, MN", "Detroit, MI")
combined <- filter(combined, DepCity == x)

#Find the average arrival delay for flight
combined <- group_by(combined, DepCity, ArrCity) 
summ <- summarise(combined, TotalFlights = n(), AvgDepDelay = round(mean(DepDelay)), AvgArrDelay = round(mean(ArrDelay)))
AvgArrDelayPivot <-dcast(summ, ArrCity ~ DepCity, value.var = "AvgArrDelay")    

# find average arrival delay by cities and airports and Carriers
ungroup(combined)
combined <- group_by(combined, DepAirport, Carrier)
summ2 <- summarise(combined, TotalFlights = n(), AvgDepDelay = round(mean(DepDelay)), AvgArrDelay = round(mean(ArrDelay)))
ArrDelayCityAirport <-dcast(summ2, Carrier ~ DepAirport, value.var = "AvgArrDelay")   

# find cancelled flights
ungroup(combined)
combined <- group_by(combined, DepAirport, CancelReason)
summ3 <- summarise(combined, TotalFlights = n())
CancelReasonPivot <-dcast(summ3, CancelReason ~ DepAirport, value.var = "TotalFlights", na.rm = TRUE)



#combined <- subset(combined, ArrState == "NV")
ungroup(combined)
combined$ArrStatus <- NA
combined$ArrStatus[combined$ArrDelay <= 0] <- "On Time"
combined$ArrStatus[combined$ArrDelay > 0] <- "Late"
combined$ArrStatus[combined$Cancelled == 1] <- "Cancelled"
combined$ArrStatus <- factor(combined$ArrStatus, levels = c("On Time", "Late", "Cancelled"))
levels(combined$ArrStatus)


combinedtmp <- group_by(combined, Carrier, ArrStatus)
summ4 <- summarize(combinedtmp, num_late = n())
ArrStatusPivot <- dcast(summ4, Carrier ~ ArrStatus, value.var = "num_late")

ungroup(combined)
combined$DepStatus <- NA
combined$DepStatus[combined$DepDelay <= 0] <- "On Time"
combined$DepStatus[combined$DepDelay > 0] <- "Late"
combined$DepStatus[combined$Cancelled == 1] <- "Cancelled"
combined$DepStatus <- factor(combined$DepStatus, levels = c("On Time", "Late", "Cancelled"))
levels(combined$DepStatus)


combinedtmp2 <- group_by(combined, DepAirport, DepStatus)
summ5 <- summarize(combinedtmp2, num_delay = n())
DepStatusPivot <- dcast(summ5, DepAirport ~ DepStatus, value.var = "num_delay")

ungroup(combined)
combinedtmp3 <- group_by(combined, Carrier, DepStatus)
summ6 <- summarize(combinedtmp3, num_delay = n())
CarrierStatusPivot <- dcast(summ6, Carrier ~ DepStatus, value.var = "num_delay")

ungroup(combined)
combined$DelayCause <- NA
combined$DelayCause[combined$CarrierDelay > 0] <- "Carrier"
combined$DelayCause[combined$WeatherDelay > 0] <- "Weather"
#combined$DelayCause[combined$NASDelay > 0] <- "NAS"
combined$DelayCause[combined$SecurityDelay > 0] <- "Security"
combined$DelayCause[combined$LateAircraftDelay > 0] <- "Late Aircraft"
combined$DelayCause <- factor(combined$DelayCause)
levels(combined$DelayCause)

combined$Date <- months(combined$Date)
combined$Date[combined$Date == "November"] <- "Nov"
combined$Date[combined$Date == "December"] <- "Dec"
combined$Date[combined$Date == "January"] <- "Jan"
combined$Date[combined$Date == "February"] <- "Feb"
combined$Date <- factor(combined$Date, levels = c("Nov", "Dec", "Jan", "Feb"))
levels(combined$Date)

combinedtmp4 <- group_by(combined, Date, Carrier)
summ7 <- summarize(combinedtmp4, num_flights = n())
FlightsbyCarrierPivot <- dcast(summ7, Carrier ~ Date, value.var = "num_flights")

ungroup(combined)
combinedtmp5 <- group_by(combined, DepAirport, Carrier)
summ8 <- summarize(combinedtmp5, num_flights = n())
AirportsbyCarrierPivot <- dcast(summ8, DepAirport ~ Carrier, value.var = "num_flights")

ungroup(combined)
combinedtmp6 <- group_by(combined, DepAirport, Date)
summ9 <- summarize(combinedtmp6, num_flights = n())
FlightsbyAirportPivot <- dcast(summ9, DepAirport ~ Date, value.var = "num_flights")

#p <- qplot(DepAirport, DepStatus, data = combined,  geom = "bin2d")
#print(p)

p2 <- qplot(Date, DepStatus, data = combined,  geom = "bin2d") + scale_y_discrete(name = "Departure Status")
p2 <- p2 + ggtitle("Departure Status")
p2 <- p2 + scale_color_gradient(labels = comma)
print(p2)
