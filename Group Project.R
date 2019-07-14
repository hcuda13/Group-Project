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


###########################################################################################################################
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

#Determine the airports that are most on-time
PercentofDepStatus <- (table(mda$DepAirport, mda$DepStatus)/rowSums(table(mda$DepAirport, mda$DepStatus))) * 100
PercentofDepStatus <- as.data.frame(PercentofDepStatus)
names(PercentofDepStatus)[names(PercentofDepStatus) == "Var1"] <- "DepAirport"
names(PercentofDepStatus)[names(PercentofDepStatus) == "Var2"] <- "DepStatus"
PercentofDepStatus <- subset(PercentofDepStatus, Freq != "NaN")
PDS <- PercentofDepStatus %>%
  ungroup() %>%
  mutate(rnFreq = row_number(desc(Freq))) %>%
  select("DepAirport", "DepStatus", "Freq") %>%
  arrange(desc(Freq))

#Determine the carriers that are most on-time
PercentofCarrierArrStatus <- (table(mda$Carrier, mda$ArrStatus) /rowSums(table(mda$Carrier, mda$ArrStatus))) * 100
PercentofCarrierArrStatus <- as.data.frame(PercentofCarrierArrStatus)
names(PercentofCarrierArrStatus)[names(PercentofCarrierArrStatus) == "Var1"] <- "Carrier"
names(PercentofCarrierArrStatus)[names(PercentofCarrierArrStatus) == "Var2"] <- "ArrStatus"
PercentofCarrierArrStatus <- subset(PercentofCarrierArrStatus, Freq != "NaN")
PCAS <- PercentofCarrierArrStatus %>%
  ungroup() %>%
  mutate(rnFreq = row_number(desc(Freq))) %>%
  select("Carrier", "ArrStatus", "Freq") %>%
  arrange(desc(Freq))

#Load ggplot2 and scales libraries after data import.  Scales interferes with my_col_types read.
suppressPackageStartupMessages(library(ggplot2))
# library(scales)

#Plot displaying the count of flights by state by month.
p4 <- qplot(DepState, data = mda, geom = "bar", fill = DepStatus, facets = .~ Date)
p4 <- p4 + ggtitle("Count of Flights by State and Month")
p4 <- p4 + xlab("Departure State")
p4 <- p4 + ylab("Count of Flights")
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

####################################### Cedar Rapids, Quad Cities, Des Moines airports###################################################

# From the master combined dataframe creating a 3 sub dataframe with data respective 
# airports related to desmoines, quad cities and cedar rapids.
# Among those three airport trying to figure out which airport by airlines are higher/lower departures % chance on ontime/cancel/late 

# Function to subseting the data frame by searching with specific airport

Airport <- function(airport= "") {
  airport <- as.data.frame(subset(mda, DepAirport == airport))
  return(airport)
}

# subseting the data frame to just Des Moines cities airport
Airport("DSM")
dsm <- Airport("DSM")

# subseting the data frame to just moline/quad cities airport
Airport("MLI")
mli <- Airport("MLI")

# subseting the data frame to just cedar rapids airport
Airport("MLI")
cid <- Airport("MLI")

#Pivot table displaying desmoines airport Departure airline status
dsm_tmp2 <- group_by(dsm, Carrier, DepStatus)
dsm_summ2 <- summarize(dsm_tmp2, num_delay = n())
dsm_DepStatusPivot <- dcast(dsm_summ2, Carrier ~ DepStatus, value.var = "num_delay")

# Replacing the NA's data in dsm_DepStatusPivot with 0's 
complete.cases(dsm_DepStatusPivot)
dsm_DepStatusPivot[is.na(dsm_DepStatusPivot)] <- 0

# changing the Carrier column to factor
dsm_DepStatusPivot$Carrier <- factor(dsm_DepStatusPivot$Carrier)
levels(dsm_DepStatusPivot$Carrier)

# Adding new rows with % on on ontime/cancel/late for desmoines
dsm_DepStatusPivot$OnTime_per <- ((dsm_DepStatusPivot$`On Time`)/(dsm_DepStatusPivot$`On Time` + dsm_DepStatusPivot$Late + dsm_DepStatusPivot$Cancelled))*100
dsm_DepStatusPivot$Late_per <- ((dsm_DepStatusPivot$Late)/(dsm_DepStatusPivot$`On Time` + dsm_DepStatusPivot$Late + dsm_DepStatusPivot$Cancelled))*100
dsm_DepStatusPivot$Cancelled_per <- ((dsm_DepStatusPivot$Cancelled)/(dsm_DepStatusPivot$`On Time`+ dsm_DepStatusPivot$Late + dsm_DepStatusPivot$Cancelled))*100

#Pivot table displaying quad cities airport Depature airline status
mli_tmp2 <- group_by(mli, Carrier, DepStatus)
mli_summ2 <- summarize(mli_tmp2, num_delay = n())
mli_DepStatusPivot <- dcast(mli_summ2, Carrier ~ DepStatus, value.var = "num_delay")

# Replacing the NA's data in mli_DepStatusPivot with 0's 
complete.cases(mli_DepStatusPivot)
mli_DepStatusPivot[is.na(mli_DepStatusPivot)] <- 0

# changing the Carrier column to factor
mli_DepStatusPivot$Carrier <- factor(mli_DepStatusPivot$Carrier)
levels(mli_DepStatusPivot$Carrier)

# Adding new rows with % on on ontime/cancel/late for moline
mli_DepStatusPivot$OnTime_per <- ((mli_DepStatusPivot$`On Time`)/(mli_DepStatusPivot$`On Time` + mli_DepStatusPivot$Late + mli_DepStatusPivot$Cancelled))*100
mli_DepStatusPivot$Late_per <- ((mli_DepStatusPivot$Late)/(mli_DepStatusPivot$`On Time` + mli_DepStatusPivot$Late + mli_DepStatusPivot$Cancelled))*100
mli_DepStatusPivot$Cancelled_per <- ((mli_DepStatusPivot$Cancelled)/(mli_DepStatusPivot$`On Time`+ mli_DepStatusPivot$Late + mli_DepStatusPivot$Cancelled))*100

#Pivot table displaying cedar rapids airport Depature airline status
cid_tmp2 <- group_by(cid, Carrier, DepStatus)
cid_summ2 <- summarize(cid_tmp2, num_delay = n())
cid_DepStatusPivot <- dcast(cid_summ2, Carrier ~ DepStatus, value.var = "num_delay")

# Replacing the NA's data in mli_DepStatusPivot with 0's 
complete.cases(cid_DepStatusPivot)
cid_DepStatusPivot[is.na(cid_DepStatusPivot)] <- 0

# changing the Carrier column to factor
cid_DepStatusPivot$Carrier <- factor(cid_DepStatusPivot$Carrier)
levels(cid_DepStatusPivot$Carrier)

# Adding new rows with % on on ontime/cancel/late for cedar rapids
cid_DepStatusPivot$OnTime_per <- ((cid_DepStatusPivot$`On Time`)/(cid_DepStatusPivot$`On Time` + cid_DepStatusPivot$Late + cid_DepStatusPivot$Cancelled))*100
cid_DepStatusPivot$Late_per <- ((cid_DepStatusPivot$Late)/(cid_DepStatusPivot$`On Time` + cid_DepStatusPivot$Late + cid_DepStatusPivot$Cancelled))*100
cid_DepStatusPivot$Cancelled_per <- ((cid_DepStatusPivot$Cancelled)/(cid_DepStatusPivot$`On Time`+ cid_DepStatusPivot$Late + cid_DepStatusPivot$Cancelled))*100


# Plot dispalying the Cedar Rapids on time departure by Carrier. 

p5 <- qplot(Carrier,OnTime_per, data = cid_DepStatusPivot, geom = "point", color = Carrier,size = I(4))
p5 <- p5 + ggtitle("Cedar Rapids On Time departure by Carrier")
p5 <- p5 + ylab("Late Depature %")
print(p5)
ggsave(filename = "CedarRapidsLate.png", plot = p5, width = 10, height = 6, dpi = 600)

# Plot dispalying the Cedar Rapids late departure by Carrier.
p6 <- qplot(Carrier,Late_per, data = cid_DepStatusPivot, geom = "point", color = Carrier,size = I(4))
p6 <- p6 + ggtitle("Cedar Rapids late departure by Carrier")
p6 <- p6 + ylab("Late Depature %")
print(p6)
ggsave(filename = "CedarRapidsLate.png", plot = p6, width = 10, height = 6, dpi = 600)


# Plot dispalying the Desmoines on time departure by Carrier. 
p7 <- qplot(Carrier,OnTime_per, data = dsm_DepStatusPivot, geom = "point", color = Carrier,size = I(4))
p7 <- p7 + ggtitle("Desmoines on time departure by Carrier")
p7 <- p7 + ylab("On Time Depature %")
print(p7)
ggsave(filename = "DesmoinesOnTime.png", plot = p7, width = 10, height = 6, dpi = 600)

# Plot dispalying the Desmoines late departure by Carrier. 
p8 <- qplot(Carrier,Late_per, data = dsm_DepStatusPivot, geom = "point", color = Carrier,size = I(4))
p8 <- p8 + ggtitle("Desmoines late departure by Carrier")
p8 <- p8 + ylab("Late Depature %")
print(p8)
ggsave(filename = "DesmoinesLate.png", plot = p8, width = 10, height = 6, dpi = 600)

# Plot dispalying the Moline on time departure by Carrier. 
p9 <- qplot(Carrier,OnTime_per, data = mli_DepStatusPivot, geom = "point", color = Carrier,size = I(4))
p9 <- p9 + ggtitle("Moline on time departure by Carrier")
p9 <- p9 + ylab("On Time Depature %")
print(p9)
ggsave(filename = "MolineOnTime.png", plot = p9, width = 10, height = 6, dpi = 600)

# Plot dispalying the Moline late departure by Carrier. 
p10 <- qplot(Carrier,Late_per, data = mli_DepStatusPivot, geom = "point", color = Carrier,size = I(4))
p10 <- p10 + ggtitle("Moline late departure by Carrier")
p10 <- p10 + ylab("Late Depature %")
print(p10)
ggsave(filename = "MolineLate.png", plot = p10, width = 10, height = 6, dpi = 600)


###############################################Orlando ####################################################

# select midwest cities with airports.  Note that Chicago has 2 airports, MDW and ORD
x <- c("Chicago, IL", "Moline, IL", "Rockford, IL", "Peoria, IL", "Cedar Rapids/Iowa City, IA", "Des Moines, IA", "St. Louis, MO", 
       "Minneapolis, MN", "Detroit, MI")
sma <- filter(mda, DepCity == x)

#Find the average arrival delay for flight
sma <- group_by(sma, DepCity, ArrCity) 
hc1 <- summarise(sma, TotalFlights = n(), AvgDepDelay = round(mean(DepDelay)), AvgArrDelay = round(mean(ArrDelay)))
AvgArrDelayPivot <-dcast(hc1, ArrCity ~ DepCity, value.var = "AvgArrDelay")    

# find average arrival delay by cities and airports and Carriers
ungroup(sma)
sma <- group_by(sma, DepAirport, Carrier)
hc2 <- summarise(sma, TotalFlights = n(), AvgDepDelay = round(mean(DepDelay)), AvgArrDelay = round(mean(ArrDelay)))
ArrDelayCityAirport <-dcast(hc2, Carrier ~ DepAirport, value.var = "AvgArrDelay")   

# find cancelled flights
ungroup(combined)
sma <- group_by(sma, DepAirport, CancelReason)
hc3 <- summarise(sma, TotalFlights = n())
CancelReasonPivot <-dcast(hc3, CancelReason ~ DepAirport, value.var = "TotalFlights", na.rm = TRUE)

#Find the average arrival delay for flight
#find flight info to Orlando, FL
ungroup(sma)
ORL <- filter(sma, ArrCity == "Orlando, FL")

#To Orlando find cancel reason by airport
ORL <- group_by(ORL, DepAirport, CancelReason)
hcsumm10 <- summarize(ORL, TotalFlights = n())
ORLCancAirport <- dcast(hcsumm10, DepAirport ~ CancelReason, value.var = "TotalFlights")  

#To Orlando:if flight is delayed, what is the average delay by carrier.  
ungroup (ORL)
tmpORL <- filter(ORL, ArrDelay > 0)
tmpORL <- group_by(tmpORL, DepAirport, Carrier)
hcsumm11 <- summarize(tmpORL, TotalFlights = n(), AvgDelay = round(mean(ArrDelay)))
ORLDelays <- dcast (hcsumm11, DepAirport ~ Carrier, value.var = "AvgDelay")

# To Orlando: flights by status Dep Airport
ungroup(ORL)
ORL <- group_by(ORL,DepAirport, ArrStatus)
hcsumm12 <- summarize(ORL, Flights = n())
ORLStatusAirport <- dcast(hcsumm12, DepAirport ~ ArrStatus, value.var = "Flights")

# To Orlando: flights by status Carrier
ungroup(ORL)
ORL <- group_by(ORL, Carrier, ArrStatus)
hcsumm12 <- summarize(ORL, Flights = n())
ORLStatusCarrier <- dcast(hcsumm12, Carrier ~ ArrStatus, value.var = "Flights")

# find data from airport and carrier on count of delays
ungroup(ORL)
ORL <- group_by(ORL, Carrier, DepAirport, ArrStatus)
hc13 <- summarize(ORL, Flights = n())
ORLCarrierDepAIR <- dcast(hc13, Carrier ~ DepAirport, value.var = "Flights")


#Arrival Status to Orlando from Departure Airports by Carrier

ungroup(ORL)
p10 <- qplot(Carrier,  data = ORL, geom = "bar", facets = . ~ DepAirport, fill = ArrStatus)
p10 <- p10 + ggtitle("To Orlando: Arrival Status by Carrier from Departure Airport")
p10 <- p10 + xlab("Carrier")
p10 <- p10 + ylab("Total Flights")
p10 <- p10 + theme(axis.text.x = element_text(angle = 90, size = 10))
p10 <- p10 + theme(axis.title.y = element_text(size = 12, face = "bold"))
p10 <- p10 + theme(axis.title.x = element_text(size = 12, face = "bold"))
p10 <- p10 + theme(axis.ticks.x = element_blank())
p10 <- p10 + labs(fill= "Arrival Status")
p10 <- p10 + theme(legend.title = element_text(size = 12, face = "bold"))
p10

ggsave(filename = "Orlando: Arrival Status by Carrier from Departure Airport.png", plot = p4, width = 12, height = 8,dpi = 600)

############################### FLight Map to Orlando ############################################################################

#Create new database for fights to ORL and remove columns    
dfm <- ORL

dfm$Date <- NULL
dfm$Carrier <- NULL
dfm$FlightNumber <- NULL
dfm$DepState <- NULL
dfm$DepTime <- NULL
dfm$DepDelay <- NULL
dfm$ArrTIme <- NULL
dfm$ArrDelay <- NULL
dfm$Cancelled <- NULL
dfm$CancelReason <- NULL
dfm$CarrierDelay <- NULL
dfm$WeatherDelay <- NULL
dfm$NASDelay <- NULL
dfm$SecurityDelay <- NULL
dfm$LateAircraftDelay <- NULL
dfm$ArrStatus <- NULL
dfm$DepStatus <- NULL

# Airport latitude and longitude data downloaded from Federal Aviation Administration
#link: http://ais-faa.opendata.arcgis.com/datasets/e747ab91a11045e8b3f8a3efd093d3b5_0

my_col_types2 <-
  cols(
    X = col_double(),
    Y = col_double(),
    OBJECTID = col_skip(),
    GLOBAL_ID = col_skip(),
    IDENT = col_character(),
    NAME = col_character(),
    LATITUDE = col_character(),
    LONGITUDE = col_character(),
    ELEVATION = col_double(),
    ICAO_ID = col_skip(),
    TYPE_CODE = col_skip(),
    SERVCITY = col_skip(),
    STATE = col_skip(),
    COUNTRY = col_skip(),
    OPERSTATUS = col_skip(),
    PRIVATEUSE = col_skip(),
    IAPEXISTS = col_skip(),
    DODHIFLIP = col_skip(),
    FAR91 = col_skip(),
    FAR93 = col_skip(),
    MIL_CODE = col_skip(),
    AIRANAL = col_skip(),
    US_HIGH = col_skip(),
    US_LOW = col_skip(),
    AK_HIGH = col_skip(),
    AK_LOW = col_skip(),
    US_AREA = col_skip(),
    PACIFIC = col_skip())

df2 <- read_csv("Airports.csv", col_types = my_col_types2)
df2<- rename(df2, DepAirport = IDENT)

#Get Orlando Airport coordinates
MCO <- filter(df2, DepAirport == "MCO")
#join dfm and df2 on column DepAirport
Airports <- inner_join(mda, df2)

Airports <- rename(Airports, lon == X, lat = Y)
# get single flight from each departure airport

Airports <- unique(Airports)
dfORD <- filter(Airports, DepAirport == "ORD")
DTW <- filter(Airports, DepAirport == "DTW")
STL <- filter(Airports, DepAirport == "STL")
MDW <- filter(Airports, DepAirport == "MDW")
MSP <- filter(Airports, DepAirport == "MSP")
DSM <- filter(Airports, DepAirport == "DSM")

# Information about mapping and code for base map downloaded from website: 
#https://www.gis-blog.com/flight-connection-map-with-r/

#install.packages("maps")
#install.packages("geosphere")
suppressPackageStartupMessages(library(maps))
suppressPackageStartupMessages(library(geosphere))
#install.packages("ggmap")
suppressPackageStartupMessages(library(ggmap))
#install.packages("mapdata")
suppressPackageStartupMessages(library(mapdata))

#create basemap
dev.off()
par(mar=c(0,0,0,0))
p11 <-map("world", regions=c("usa"), fill=T, col="grey8", bg="grey15", ylim=c(21.0,50.0), xlim=c(-130.0,-65.0))

#overlay airports
p11 <- p11 + points(Airports$X,Airports$Y, pch= 1, cex = 0.1, col = "chocolate1")

#flights from midwest states to Orlando, Fl.
p11<- p11 + for (i in (1:dim(Airports)[1])) { 
  inter <- gcIntermediate(c(MCO$X[1],  MCO$Y[1]), c(Airports$X[i],Airports$Y[i]), n=200)
  lines(inter, lwd=0.005, col="turquoise2")    
}
#to save use export tab in plot window
