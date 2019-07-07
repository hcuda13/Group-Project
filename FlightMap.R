library(readr)
library(dplyr)
library(reshape2)

my_col_types <-
  cols(
    Date = col_skip(),
    Carrier = col_skip(),
    FlightNumber = col_skip(),
    DepAirport = col_character(),
    DepCity = col_character(),
    DepState = col_skip(),
    ArrAirport = col_character(),
    ArrCity = col_character(),
    ArrState = col_character(),
    DepTime = col_skip(),
    DepDelay = col_skip(),
    ArrTIme = col_skip(),
    ArrDelay = col_skip(),
    Cancelled = col_skip(),
    CancelReason = col_skip(),
    CarrierDelay = col_skip(),
    WeatherDelay = col_skip(),
    NASDelay = col_skip(),
    SecurityDelay = col_skip(),
    LateAircraftDelay = col_skip(),
    ArrStatus = col_skip(),
    DepStatus = col_skip(),
    DelayCause = col_skip())

df <- read_csv("midwest.csv", col_types = my_col_types)
spec(df)

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
df2 <- rename(df2, ArrAirport = IDENT)
Airports <- inner_join(df, df2)

Airports <- rename(Airports, lon == X, lat = Y)

ORD <- filter(Airports, DepAirport == "ORD")
DTW <- filter(Airports, DepAirport == "DTW")
CID <- filter(Airports, DepAirport == "CID")
MDW <- filter(Airports, DepAirport == "MDW")
PIA <- filter(Airports, DepAirport == "PIA")
MSP <- filter(Airports, DepAirport == "MSP")
DSM <- filter(Airports, DepAirport == "DSM")
MLI <- filter(Airports, DepAirport == "MLI")
STL <- filter(Airports, DepAirport == "STL")

Airports <- unique(Airports)
ORD <-unique(ORD)
DTW <-unique(DTW)
CID <-unique(CID)
MDW <-unique(MDW)
PIA <- unique(PIA)
MSP <- unique(MSP)
DSM <- unique(MSP)
MLI <- unique(MLI)
STL <- unique(STL)

OHare <- unique(filter(Airports, ArrAirport == "ORD"))

# Information about mapping and code for base map downloaded from website: 
#https://www.gis-blog.com/flight-connection-map-with-r/

install.packages("maps")
install.packages("geosphere")
library(maps)
library(geosphere)
library(ggplot2)
install.packages("ggmap")
library(ggmap)
install.packages("mapdata")
library(mapdata)

#create basemap
dev.off()
par(mar=c(0,0,0,0))
p <-map("world", regions=c("usa"), fill=T, col="grey8", bg="grey15", ylim=c(21.0,50.0), xlim=c(-130.0,-65.0))
#overlay airports
p <- p + points(Airports$X,Airports$Y, pch=1, cex=0.1, col="chocolate1")

p<- p + for (i in (1:dim(Airports)[1])) { 
  inter <- gcIntermediate(c(OHare$X[1], OHare$Y[1]), c(ORD$X[i], ORD$Y[i]), n=200)
  lines(inter, lwd=0.005, col="turquoise2")    
}


#try diff map
usa <- map_data("usa")
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)

#another try
dev.off()
par(mar=c(0,0,0,0))
map('world',regions = c("usa"),col="grey15", fill=TRUE, bg="white", lwd=0.05,mar=rep(0,4),border=0, ylim=c(21,50), xlim = c(-130, -65))