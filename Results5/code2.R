#Creating complete data set with missing counties included

#########################################
######Magnolia Warbler###################



library(auk)
library(readr)
library(dplyr)
library(lubridate)
library(tibble)
library(usmap)
library(plotly)
library(ggmap)
library(gganimate)
library(choroplethr)
library(choroplethrMaps)
library(elevatr)
library(sp)
library(maps)
library(maptools)


#Load the data
ebd<-read_tsv("C:\\Users\\dhanu\\OneDrive\\FirstArrival\\ebd_magwar\\ebd_magwar_relNov-2019.txt", col_types = cols("LAST EDITED DATE"=col_skip(),"TAXONOMIC ORDER"=col_skip(),"SCIENTIFIC NAME"=col_skip(),"SUBSPECIES COMMON NAME"=col_skip(),"SUBSPECIES SCIENTIFIC NAME"=col_skip(),"BREEDING BIRD ATLAS CODE"=col_skip(),"BREEDING BIRD ATLAS CATEGORY"=col_skip(),"LOCALITY"=col_skip(),
                                                                                                                   "LOCALITY ID"=col_skip(),"LOCALITY TYPE"=col_skip(),"TIME OBSERVATIONS STARTED"=col_skip(),"OBSERVER ID"=col_skip(),"SAMPLING EVENT IDENTIFIER"=col_skip(),"PROTOCOL TYPE"=col_skip(),"PROTOCOL CODE"=col_skip(),
                                                                                                                   "PROJECT CODE"=col_skip(),"DURATION MINUTES"=col_skip(),"EFFORT DISTANCE KM"=col_skip(),"EFFORT AREA HA"=col_skip(),"NUMBER OBSERVERS"=col_skip(),"ALL SPECIES REPORTED"=col_skip(),"GROUP IDENTIFIER"=col_skip(),"HAS MEDIA"=col_skip(),
                                                                                                                   "APPROVED"=col_skip(),"REVIEWED"=col_skip(),"REASON"=col_skip(),"TRIP COMMENTS"=col_skip(),"SPECIES COMMENTS"=col_skip(),X47=col_skip()))


##Remove all countries other than the US
ebd<-ebd %>% filter((`COUNTRY CODE` %in% c("US")))

##Remove Alaska and Hawaii
ebd<-ebd %>% filter(!(`STATE` %in% c("Hawaii", "Alaska")))

##Remove NAs for OBSERVATION DATE
ebd<-ebd %>% filter(!is.na(`OBSERVATION DATE`)  )

#There are 62 observations with no county. Impute county using GPS coordinates

library(Hmisc)

latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

#Get county using lon/lat coordinates
ebd$COUNTY<-unlist(lapply(c(1:nrow(ebd)), function(x){if(!is.na(ebd$COUNTY[x])){ebd$COUNTY[x]}else{capitalize(strsplit(latlong2county(as.data.frame(ebd[x,c(17,16)])), ",")[[1]][2])}}))

#get county code using county and state 
county_statedf<-distinct(ebd[,c(10, 8, 11)])
county_statedf<-na.omit(county_statedf)

ebd$`COUNTY CODE`<-unlist(lapply(c(1:nrow(ebd)), function(x){if(!is.na(ebd$`COUNTY CODE`[x])){ebd$`COUNTY CODE`[x]}else{county_statedf[county_statedf$COUNTY==ebd$COUNTY[x] & county_statedf$STATE==ebd$STATE[x],3]}}))


ebd<-mutate(ebd, YEAR=year(ebd$`OBSERVATION DATE`))
plot(table(ebd$YEAR))

##Get distribution over months

plot(table(month(ebd$`OBSERVATION DATE`)), xlab="Month", ylab="Frequency")
#majority in april and may

#Retain only 2004 onwards

ebd<-ebd %>% filter(ebd$YEAR>=2004)

#Filter out to get north east counties since this is where the data is most prevalent
ebd_1<- ebd %>% filter(ebd$LATITUDE>35 & ebd$LONGITUDE>(-97))
ebd_1<- ebd_1 %>% filter(!(ebd_1$STATE %in% c("Tennessee", "Kentucky", "Missouri", "Iowa", "Arkansas", "Minnesota", "Kansas", 
                                              "Nebraska", "North Carolina", "North Dakota","Oklahoma", "South Carolina", "South Dakota" )))




#Check occurence in jan and feb

which(month(ebd_1$`OBSERVATION DATE`) %in% c(1,2))

table(ebd_1$STATE[which(month(ebd_1$`OBSERVATION DATE`) %in% c(1,2))])
#1 such incidence in Ohio 2016/01/06

ebd_1[which(month(ebd_1$`OBSERVATION DATE`) %in% c(1,2)),c(8, 10, 18, 19) ]


################################
####Define first arrival as the number of days from the first day of spring (taken as 20th of March every year)
#Remove all data from before this date 

ebd_1<-ebd_1 %>% filter(month(ebd_1$`OBSERVATION DATE`)>=3 )
ebd_1<-ebd_1 %>% filter(!(month(ebd_1$`OBSERVATION DATE`)==3 & day(ebd_1$`OBSERVATION DATE`)<20))
ebd_1<-ebd_1 %>% filter(!(month(ebd_1$`OBSERVATION DATE`)>=8))
ebd_1<-ebd_1 %>% filter(!(month(ebd_1$`OBSERVATION DATE`)==7 & day(ebd_1$`OBSERVATION DATE`)>20))


# Format names of independent cities

ebd_1$COUNTY[ebd_1$COUNTY=="Richmond City"]<-"Richmond"
# ebd_1$COUNTY[ebd_1$COUNTY=="Fairfax City"]<-"Fairfax"
# ebd_1$COUNTY[ebd_1$COUNTY=="Roanoke City"]<-"Roanoke"
ebd_1$COUNTY[ebd_1$COUNTY=="DuPage"]<-"Dupage"
ebd_1$COUNTY[ebd_1$COUNTY=="St. Joseph"]<-"Saint Joseph"
ebd_1$COUNTY[ebd_1$COUNTY=="St. Lawrence"]<-"Saint Lawrence"
ebd_1$COUNTY[ebd_1$COUNTY=="St. Clair"]<-"Saint Clair"
ebd_1$COUNTY[ebd_1$COUNTY=="McKean"]<-"Mc Kean"
ebd_1$COUNTY[ebd_1$COUNTY=="DeKalb"]<-"De Kalb"
ebd_1$COUNTY[ebd_1$COUNTY=="St. Mary's"]<-"Saint Mary's"
ebd_1$COUNTY[ebd_1$COUNTY=="St. Croix"]<-"Saint Croix"

# state        county
# 1          <NA>          <NA>
# 2      Illinois        DuPage
# 3       Indiana    St. Joseph
# 4      New York  St. Lawrence
# 5      Michigan     St. Clair
# 6  Pennsylvania        McKean
# 7      Virginia Richmond City
# 8      Illinois        DeKalb
# 9      Maryland    St. Mary's
# 10     Illinois     St. Clair
# 11    Wisconsin     St. Croix
# 12     Michigan    St. Joseph
# 13      Indiana        DeKalb

length(unique(ebd_1$COUNTY))

# #Consider counties with atleast 10 observations in 10 years
# countdf<-data.frame(Counties=unique(ebd_1$`COUNTY CODE`), Year2004=NA, Year2005=NA, Year2006=NA,
#                     Year2007=NA, Year2008=NA, Year2009=NA, Year2010=NA,
#                     Year2011=NA, Year2012=NA, Year2013=NA, Year2014=NA,
#                     Year2015=NA, Year2016=NA, Year2017=NA, Year2018=NA,
#                     Year2019=NA)



######
##Plot all the counties to ensure that the necessary geographical region is chosen

County<-ebd_1[, c(10,11, 8)]
County<-distinct(County)

County$region<-unlist(lapply(c(1:nrow(County)), function(x){  
  part<-county.regions[county.regions$county.name==tolower(County$COUNTY[x]) & county.regions$state.name==tolower(County$STATE[x]), 1]
  if(length(part)==0){return(NA)} else {return(part[1])} }))


County[which(is.na(County$region)),]

# These 4 are independent cities

# A tibble: 4 x 4
# COUNTY        `COUNTY CODE` STATE    region
# <chr>         <chr>         <chr>     <dbl>
# 1 Richmond City US-VA-760     Virginia     NA
# 2 La Salle      US-IL-099     Illinois     NA
# 3 Fairfax City  US-VA-600     Virginia     NA
# 4 Roanoke City  US-VA-770     Virginia     NA

# 51059 and 51600 fairfax virginia
# 24005 and 24510 baltimore maryland
# 29189 and 29510 st. louis missouri

# missing: 158, 134, Richmond City, St. Louis City both independent cities

County_1<-na.omit(County)
County_1$value<-10
county_choropleth(County_1[,c(4,5)])+scale_fill_discrete(na.value="white")

#The above plot does not include independent cities and La salle

library(raster)
library(RColorBrewer)

#Plot the desired region
getdat1 <- getData(name="GADM", country="USA", level=2)
check1 <- subset(getdat1, NAME_1 %in% unique(ebd_1$STATE))
county_borders <- as(check1, "SpatialLinesDataFrame")
state_borders <- unionSpatialPolygons(check1,
                                      as.numeric(as.factor(check1$NAME_1)))
xbox = c(-95, -65)
ybox = c(35,  50)

spplot(as(state_borders, "SpatialPolygonsDataFrame"),sp.layout=list(check1, col="grey"), xlim=xbox, ylim=ybox, colorkey = F, col="black", fill=NA)

#############
#Identify counties that are missing in the ebd_1 data and include them

state_county<-distinct(data.frame(state=check1$NAME_1, county=check1$NAME_2))

missing<-data.frame(state=NA, county=NA)

for(i in c(1:nrow(state_county))){
  
  if(!(state_county$county[i] %in% unique(County$COUNTY))){
    
    missing<-rbind(missing, c(as.character(state_county$state[i]), as.character(state_county$county[i])))
    
  }else{
    
    multcounty<-County[County$COUNTY==state_county$county[i],]
    
    if(!(state_county$state[i] %in% unique(multcounty$STATE))){
      missing<-rbind(missing, c(as.character(state_county$state[i]), as.character(state_county$county[i])))
      
      
    }
  
      }
  
}

#Check to see if all counties in County are present in state_county
missing2<-data.frame(state=NA, county=NA)

for(i in c(1:nrow(County))){
  
  if(!(County$COUNTY[i] %in% unique(state_county$county))){
    
    missing2<-rbind(missing2, c(as.character(County$STATE[i]), as.character(County$COUNTY[i])))
    
  }else{
    
    multcounty<-state_county[state_county$county==County$COUNTY[i],]
    
    if(!(County$STATE[i] %in% unique(multcounty$state))){
      missing2<-rbind(missing2, c(as.character(County$STATE[i]), as.character(County$COUNTY[i])))
      
      
    }
    
  }
  
}



#Remove Lakes

library(stringr)
missing<-na.omit(missing)
missing<-missing[(!(str_detect(missing$county, "Lake"))),]
missing$`County Code`<-seq(1, nrow(missing))
names(missing)<-c("STATE","COUNTY", "COUNTY CODE")

#70 counties with no data at all

#Combine County with missing data

County<-rbind(County, missing)


Year<-seq(2004, 2019, 1)
Counties<-unique(County$`COUNTY CODE`)

ebd_1$`ARRIVAL TIME`<- unlist(lapply(c(1:nrow(ebd_1)), function(x){ebd_1$`OBSERVATION DATE`[x]-as.Date(paste(ebd_1$YEAR[x],"-03-20", sep="" ))}))

early_arr<-matrix(NA, nrow=length(unique(ebd_1$YEAR)), ncol=length(unique(County$`COUNTY CODE`)))



# Compute first arrival for each county for every year from 2004 to 2019. Calculated if atleast 12 observations in a given year
for(i in c(1:length(Year))){
  cat(i, "\n")
  
  ebd_year<-ebd_1 %>% filter(ebd_1$YEAR==Year[i])
  
  for(j in c(1:length(Counties))){
    # cat(j, "\n")
    
    ebd_county<-ebd_year %>% filter(ebd_year$`COUNTY CODE`==Counties[j])
   if(nrow(ebd_county)>=12){
      early_arr[i,j]<-min(ebd_county$`ARRIVAL TIME`)
      
    }
    
    
    
  }
  
}

#Find centroid of each county
# library(rgeos)
# check2<-gCentroid(check1, byid=TRUE)
# plot(check1, main="Centroid calculated using the gCentroid function in the rgeos package")
# points(check2, pch=19, col="red", cex=0.2)

plot(check1, main="Centroid calculated using the coordinates function in the sp package")
points(coordinates(check1), pch=19, col="red", cex=0.2)

state_county$LONGITUDE<-coordinates(check1)[,1]
state_county$LATITUDE<-coordinates(check1)[,2]

County$LONGITUDE<-NA
County$LATITUDE<-NA

for(i in c(1:nrow(County))){
  cat(i, "\n")
  County$LONGITUDE[i]<-state_county[state_county$state==County$STATE[i] & state_county$county==County$COUNTY[i], 3]
  County$LATITUDE[i]<-state_county[state_county$state==County$STATE[i] & state_county$county==County$COUNTY[i], 4]
  
}

#Save data so far
#save state_county, County, early_arr, early_arr_rmNA

NA_ind<-unlist(lapply(c(1:ncol(early_arr)), function(x){if(sum(!is.na(early_arr[,x]))<10){FALSE}else{TRUE}}))
early_arr_rmNA<-early_arr[,NA_ind]

save(state_county, County, early_arr, early_arr_rmNA,NA_ind,  file="C:\\Users\\dhanu\\OneDrive\\FirstArrival\\SpatialMaxID\\Results5\\magwarnew.Rdata")

for(c in c(1:ncol(early_arr))){
  if(c==1){  
    plot(y=early_arr[,1],x = seq(2004, 2019, 1),  type="l", ylim=c(12,90), xlab="Year", ylab="First Arrival", main="All data")
  }else{
    
    points(y=early_arr[,c],x = seq(2004, 2019, 1),  type="l", col=rgb(runif(1),runif(1),runif(1))
    )
  }  
  
}


for(c in c(1:ncol(ear_arr_rmNA))){
  if(c==1){  
    plot(y=early_arr_rmNA[,1],x = seq(2004, 2019, 1),  type="l", ylim=c(12,90), xlab="Year", ylab="First Arrival", main="Only counties with atleast 10 years of non NA early arrival")
  }else{
    
    points(y=early_arr_rmNA[,c],x = seq(2004, 2019, 1),  type="l", col=rgb(runif(1),runif(1),runif(1))
    )
  }  
  
}


#Get average elevation averaged over raster

elev_raster<-get_elev_raster(check1, z=7)

