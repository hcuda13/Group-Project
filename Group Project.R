# Clear workspace

rm(list = ls())

#Load necessary libraries
library(readr)
library(dplyr)
library(reshape2)

#Reassign column types appropriately
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

#Read-in each month's data
jan <- read_csv("January 2019.csv", col_types = my_col_types, na = "")
feb <- read_csv("February 2019.csv", col_types = my_col_types, na = "")
nov <- read_csv("November 2018.csv", col_types = my_col_types, na = "")
dec <- read_csv("December 2018.csv", col_types = my_col_types, na = "")

#Combine the four separate month's of data into one data frame named combined
rbind1 <- rbind(jan, feb)
rbind2 <- rbind(nov, dec)
combined <- rbind(rbind1, rbind2)

#Rename the column names apppropriately
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
levels(combined$Carrier)[levels(combined$Carrier) == "EV"] <- "ExpressJet"
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


#subset the combined df to only midwestern departure states into a new df called mda. mda = midwest departure airports (Stephanie)
mda <- subset(combined, subset = DepState == "ND" | DepState == "SD" |  DepState == "NE" |  DepState == "MN" |
                     DepState == "IA" | DepState == "MO" | DepState == "WI" |DepState == "IL" |
                     DepState == "KS" | DepState == "MI" | DepState == "IN" | DepState == "OH")

#If Nevada arrivals are desired, uncomment the below code to subset the mda df to Nevada arrival state.
#combined <- subset(combined, ArrState == "NV")

#Create a new factor column called ArrStatus that describes the flight's arrival status as "On Time", "Late" or "Cancelled".
mda$ArrStatus <- NA
mda$ArrStatus[mda$ArrDelay <= 0] <- "On Time"
mda$ArrStatus[mda$ArrDelay > 0] <- "Late"
mda$ArrStatus[mda$Cancelled == 1] <- "Cancelled"
mda$ArrStatus <- factor(mda$ArrStatus, levels = c("On Time", "Late", "Cancelled"))
levels(mda$ArrStatus)

#Create a new factor column called DepStatus that describes the flight's departure status as "On Time", "Late" or "Cancelled".
mda$DepStatus <- NA
mda$DepStatus[mda$DepDelay <= 0] <- "On Time"
mda$DepStatus[mda$DepDelay > 0] <- "Late"
mda$DepStatus[mda$Cancelled == 1] <- "Cancelled"
mda$DepStatus <- factor(mda$DepStatus, levels = c("On Time", "Late", "Cancelled"))
levels(mda$DepStatus)

#Create a new factor column that displays the date by month in the order: Nov, Dec, Jan, Feb.
mda$Date <- months(mda$Date)
mda$Date[mda$Date == "November"] <- "Nov"
mda$Date[mda$Date == "December"] <- "Dec"
mda$Date[mda$Date == "January"] <- "Jan"
mda$Date[mda$Date == "February"] <- "Feb"
mda$Date <- factor(mda$Date, levels = c("Nov", "Dec", "Jan", "Feb"))
levels(mda$Date)

#Load ggplot2 and scales libraries after data import.  Scales interferes with my_col_types read.
library(ggplot2)
library(scales)

#Plot displaying the count of flights by state by month.
p4 <- qplot(DepState, data = mda, geom = "bar", fill = DepStatus, facets = .~ Date)
p4 <- p4 + ggtitle("Count of Flights by State and Month")
#p4 <- p4 + scale_color_gradient(name = "Departure Status")
p4 <- p4 + scale_x_discrete(name = "Departure State")
#p4 <- p4 + scale_y_discrete(name = "Count of Flights", limits = c(0, 37000), breaks = 10000*(0:4))
print(p4)
ggsave(filename = "Count of Flights by State and Month.png", plot = p4, width = 12, height = 8,dpi = 600)

#Pivot table displaying how many "On Time", "Late" or "Cancelled" *arrival* flights in the Nov-Feb time period by airline.
mdatmp <- group_by(mda, Carrier, ArrStatus)
summtmp <- summarize(mdatmp, num_late = n())
ArrStatusPivot <- dcast(summtmp, Carrier ~ ArrStatus, value.var = "num_late")

#Pivot table displaying how many "On Time", "Late" or "Cancelled" *departure* flights in the Nov-Feb time period by airport.
ungroup(mda)
mdatmp2 <- group_by(mda, DepAirport, DepStatus)
summtmp2 <- summarize(mdatmp2, num_delay = n())
DepStatusPivot <- dcast(summtmp2, DepAirport ~ DepStatus, value.var = "num_delay")

#Pivot table displaying how many "On Time", "Late" or "Cancelled" *departure* flights in the Nov-Feb time period by carrier.
ungroup(mda)
mdatmp3 <- group_by(mda, Carrier, DepStatus)
summtmp3 <- summarize(mdatmp3, num_delay = n())
CarrierStatusPivot <- dcast(summtmp3, Carrier ~ DepStatus, value.var = "num_delay")

#ungroup(mda)
#mda$DelayCause <- NA
#mda$DelayCause[mda$CarrierDelay > 0] <- "Carrier"
#mda$DelayCause[mda$WeatherDelay > 0] <- "Weather"
#mda$DelayCause[mda$NASDelay > 0] <- "NAS"
#mda$DelayCause[mda$SecurityDelay > 0] <- "Security"
#mda$DelayCause[mda$LateAircraftDelay > 0] <- "Late Aircraft"
#mda$DelayCause <- factor(mda$DelayCause)
#levels(mda$DelayCause)

#Pivot table displaying the number of flights by carrier by month.
mdatmp4 <- group_by(mda, Date, Carrier)
summtmp4 <- summarize(mdatmp4, num_flights = n())
FlightsbyCarrierPivot <- dcast(summtmp4, Carrier ~ Date, value.var = "num_flights")

#Pivot table displaying the number of flights at each of the airports by carrier.
ungroup(mda)
mdatmp5 <- group_by(mda, DepAirport, Carrier)
summtmp5 <- summarize(mdatmp5, num_flights = n())
AirportsbyCarrierPivot <- dcast(summtmp5, DepAirport ~ Carrier, value.var = "num_flights")

#Pivot table displaying the number of flights at each of the airports.
ungroup(mda)
mdatmp6 <- group_by(mda, DepAirport, Date)
summtmp6 <- summarize(mdatmp6, num_flights = n())
FlightsbyAirportPivot <- dcast(summtmp6, DepAirport ~ Date, value.var = "num_flights")


#p <- qplot(DepAirport, DepStatus, data = mda,  geom = "bin2d")
#print(p)

#Plot dispalying the count of "On Time", "Late" and "Cancelled" departure flights by month.
p2 <- qplot(Date, DepStatus, data = mda,  geom = "bin2d") + scale_y_discrete(name = "Departure Status")
p2 <- p2 + ggtitle("Departure Status")
p2 <- p2 + scale_color_gradient(labels = comma)
print(p2)

#Plot dispalying the count of "On Time", "Late" and "Cancelled" arrival flights by month.
p3 <- qplot(Date, ArrStatus, data = mda,  geom = "bin2d") + scale_y_discrete(name = "Arrival Status")
p3 <- p3 + ggtitle("Arrival Status")
p3 <- p3 + scale_color_gradient(labels = comma)
print(p3)
#NA in arrival status indicates that no arrival delay was categorized and the arrival delay is unknown.

