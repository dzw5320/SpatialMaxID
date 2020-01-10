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


##Read in data

ebd<-read_tsv("C:\\Users\\dhanu\\OneDrive\\FirstArrival\\ebd_magwar\\ebd_magwar_relNov-2019.txt", col_types = cols("LAST EDITED DATE"=col_skip(),"TAXONOMIC ORDER"=col_skip(),"SCIENTIFIC NAME"=col_skip(),"SUBSPECIES COMMON NAME"=col_skip(),"SUBSPECIES SCIENTIFIC NAME"=col_skip(),"BREEDING BIRD ATLAS CODE"=col_skip(),"BREEDING BIRD ATLAS CATEGORY"=col_skip(),"LOCALITY"=col_skip(),
                                                                                                                           "LOCALITY ID"=col_skip(),"LOCALITY TYPE"=col_skip(),"TIME OBSERVATIONS STARTED"=col_skip(),"OBSERVER ID"=col_skip(),"SAMPLING EVENT IDENTIFIER"=col_skip(),"PROTOCOL TYPE"=col_skip(),"PROTOCOL CODE"=col_skip(),
                                                                                                                           "PROJECT CODE"=col_skip(),"DURATION MINUTES"=col_skip(),"EFFORT DISTANCE KM"=col_skip(),"EFFORT AREA HA"=col_skip(),"NUMBER OBSERVERS"=col_skip(),"ALL SPECIES REPORTED"=col_skip(),"GROUP IDENTIFIER"=col_skip(),"HAS MEDIA"=col_skip(),
                                                                                                                           "APPROVED"=col_skip(),"REVIEWED"=col_skip(),"REASON"=col_skip(),"TRIP COMMENTS"=col_skip(),"SPECIES COMMENTS"=col_skip(),X47=col_skip()))


##Remove Alaska and Hawaii

ebd<-ebd %>% filter((`COUNTRY CODE` %in% c("US")))

##Remove NAs for OBSERVATION DATE

ebd<-ebd %>% filter(!is.na(`OBSERVATION DATE`)  )

##Remove NAs for COUNTY

#ebd<-ebd %>% filter(!is.na(COUNTY))

##Get year from observation date and create new variable YEAR

ebd<-mutate(ebd, YEAR=year(ebd$`OBSERVATION DATE`))
plot(table(ebd$YEAR))

##Get distribution over months

plot(table(month(ebd$`OBSERVATION DATE`)), xlab="Month", ylab="Frequency")
#majority in april and may


ebd <- ebd %>%
  arrange(`OBSERVATION DATE`)



map<-get_map(location='united states', zoom=4, maptype = "terrain",
             source='google',color='color')


p<-ggmap(map) + geom_point(
  aes(x=LONGITUDE, y=LATITUDE, colour="red"), 
  data=ebd[c(730000:1937199),], alpha=.5, na.rm = T) 

testplot<-p+ggtitle("{frame_time}")+transition_time(`OBSERVATION DATE`)

animate(testplot, renderer = ffmpeg_renderer(), width = 800, height = 450)
anim_save("C:\\Users\\dhanu\\OneDrive\\FirstArrival\\nations.mp4")
## Some basic summaries

ebd %>% count(STATE, sort=TRUE)
ebd %>% count(COUNTY, sort=TRUE)

#######

ebd_check<-ebd %>% filter(month(ebd$`OBSERVATION DATE`) %in% c(1,2) )

# which counties have recrods for jan, feb and march
table(ebd_check$STATE)




##########





county_year<-ebd %>% 
  group_by(COUNTY, YEAR) %>% tally()


#######
###Plot # of counties vs. No. of years with records

df_county_years<-data.frame(Years=seq(5, 50, 1), Frequency=NA)

df_county_years$Frequency<-unlist(lapply(c(1:nrow(df_county_years)), function(x){length(which(table(county_year$COUNTY)>df_county_years$Years[x]))}))

plot(df_county_years$Years, df_county_years$Frequency, type="l", xlab="No of years", ylab="No. of counties", col="blue")


###Plot # of counties vs. No. of records per year (atleast 10 years of data present)

df_county_records<-data.frame(Records=seq(20, 300, 10), Frequency=NA)



df_county_records$Frequency<-unlist(lapply(c(1:nrow(df_county_records)), function(x){
  
  mid<-county_year %>% filter(n>df_county_records$Records[x])
  
  mid<-mid %>% filter(COUNTY %in% names(which(table(mid$COUNTY)>10)))
  
  length(unique(mid$COUNTY))
  
}))

plot(df_county_records$Records, df_county_records$Frequency, xlab="No. of records per year", ylab="No of counties", col="blue", type="l")


#######

#Exclude Florida, Texas, California, Louisiana, Arizona


ebd_1<- ebd %>% filter(!(ebd$STATE %in% c("Florida", "Texas", "California", "Louisiana", "Arizona")))


#Check occurence in jan and feb

which(month(ebd_1$`OBSERVATION DATE`) %in% c(1,2))
#18 such incidences

ebd_1[which(month(ebd_1$`OBSERVATION DATE`) %in% c(1,2)),c(8, 10, 19) ]

# A tibble: 18 x 3
# STATE          COUNTY      YEAR
# <chr>          <chr>      <dbl>
#   1 Wisconsin      Marinette   2015
# 2 Ohio           Erie        2016
# 3 South Carolina Charleston  2017
# 4 Maine          Franklin    2018
# 5 Wisconsin      Langlade    2018
# 6 Oregon         Lincoln     2019
# 7 Oregon         Lincoln     2019
# 8 Oregon         Lincoln     2019
# 9 Oregon         Lincoln     2019
# 10 Oregon         Lincoln     2019
# 11 Oregon         Lincoln     2019
# 12 Oregon         Lincoln     2019
# 13 Oregon         Lincoln     2019
# 14 Tennessee      Knox        2019
# 15 Oregon         Lincoln     2019
# 16 Oregon         Lincoln     2019
# 17 Oregon         Lincoln     2019
# 18 Oregon         Lincoln     2019

# 12 in lincoln oregon in 2019


################################
####Define first arrival as the number of days from the first day of spring (taken as 20th of March every year)
#Remove all data from before this date 

ebd_1<-ebd_1 %>% filter(month(ebd_1$`OBSERVATION DATE`)>=3 )
ebd_1<-ebd_1 %>% filter(!(month(ebd_1$`OBSERVATION DATE`)==3 & day(ebd_1$`OBSERVATION DATE`)<20))

#Retain only 2004 onwards

ebd_1<-ebd_1 %>% filter(ebd_1$YEAR>=2004)


#Unique counties (1165)
length(unique(ebd_1$COUNTY))

#Consider counties with atleast 10 observations in 10 years
countdf<-data.frame(Counties=unique(ebd_1$`COUNTY CODE`), Year2004=NA, Year2005=NA, Year2006=NA,
                                                  Year2007=NA, Year2008=NA, Year2009=NA, Year2010=NA,
                                                  Year2011=NA, Year2012=NA, Year2013=NA, Year2014=NA,
                                                  Year2015=NA, Year2016=NA, Year2017=NA, Year2018=NA,
                                                  Year2019=NA)


Year<-seq(2004, 2019, 1)
Counties<-unique(ebd_1$`COUNTY CODE`)

for(i in c(1:length(Year))){
  cat(i, "\n")
  
  ebd_year<-ebd_1 %>% filter(ebd_1$YEAR==Year[i])
  
  for(j in c(1:length(Counties))){
    
    ebd_county<-ebd_year %>% filter(ebd_year$`COUNTY CODE`==Counties[j])
    countdf[j,(i+1)]<-nrow(ebd_county)
    
  }
  
}


countcheck<-unlist(lapply(c(1:nrow(countdf)), function(x){if(sum(countdf[x, 2:17]>10)>10){TRUE}else{FALSE}}))
#202 counties are filtered

countyfilter<-Counties[which(countcheck)]

ebd_1<-ebd_1 %>% filter(ebd_1$`COUNTY CODE` %in% countyfilter)

#Plot the counties on the US map to identify where there is prevelance and long history of data

#County name and code

County<-ebd_1[, c(10,11, 8)]
County<-distinct(County)

County$region<-unlist(lapply(c(1:nrow(County)), function(x){  
  part<-county.regions[county.regions$county.name==tolower(County$COUNTY[x]) & county.regions$state.name==tolower(County$STATE[x]), 1]
  if(length(part)==0){return(NA)} else {return(part[1])} }))


# 51059 and 51600 fairfax virginia
# 24005 and 24510 baltimore maryland
# 29189 and 29510 st. louis missouri

# missing: 158, 134, Richmond City, St. Louis City both independent cities

County_1<-na.omit(County)
County_1$value<-10
county_choropleth(County_1[,c(4,5)])+scale_fill_discrete(na.value="white")


############################################
###Create the dataset#######################
############################################


#Start with ebd_1
#Calculate arrival date for each observation (observation date-20th of march of the correponding year)

ebd_1$`ARRIVAL TIME`<- unlist(lapply(c(1:nrow(ebd_1)), function(x){ebd_1$`OBSERVATION DATE`[x]-as.Date(paste(ebd_1$YEAR[x],"-03-20", sep="" ))}))

uniq_CountyCode<-unique(ebd_1$`COUNTY CODE`)#202 counties

early_arr<-matrix(NA, nrow=length(unique(ebd_1$YEAR)), ncol=length(unique(ebd_1$`COUNTY CODE`)))
s_obsloc<-matrix(NA, nrow=length(unique(ebd_1$`COUNTY CODE`)), ncol=2)


for(i in c(1:length(unique(ebd_1$`COUNTY CODE`)))){
  
 ebd_county<-subset(ebd_1, `COUNTY CODE`==uniq_CountyCode[i])
 county_split<-{}
 for(Y in c(1:16)){
   county_split[[Y]]<-subset(ebd_county, YEAR==(2003+Y))
   
 }
 min_arr<-{}
 # names(min_arr)<-names(ebd_county)
 for(t in c(1:16)){
   if(nrow(county_split[[t]])>0){
   min_arr<-rbind(min_arr, county_split[[t]][which.min(county_split[[t]]$"ARRIVAL TIME"),])
   }else{
     min_arr<-rbind(min_arr, rep(NA, 20))
     min_arr<-as.data.frame(min_arr)
     names(min_arr)<-names(ebd_county)
     
   }
   
 }
 
 early_arr[,i]<-min_arr$`ARRIVAL TIME`
 s_obsloc[i, ]<-c(mean(min_arr$LATITUDE, na.rm = T), mean(min_arr$LONGITUDE, na.rm = T))
 
  
  
}



for(c in c(1:ncol(early_arr))){
if(c==1){  
plot(early_arr[,1], type="l", ylim=c(12,211))
}else{
  
  points(early_arr[,c], type="l", col=rgb(runif(1),runif(1),runif(1))
)
}  
  
}


#For the initial case impute NA values by mean

for(c in c(1:ncol(early_arr))){
  
  if(length(which(is.na(early_arr[,c])))>0){
    
    early_arr[which(is.na(early_arr[,c])),c]<-mean(early_arr[,c], na.rm = TRUE)
    
  }
  
}


p<-ggmap(map) + geom_point(
  aes(x=LONGITUDE, y=LATITUDE, colour="red"),data=locs, alpha=.5, na.rm = T) 


locs<-as.data.frame(s_obsloc)
names(locs)<-c("LATITUDE", "LONGITUDE")


#Save early_arr and s_obsloc


save(early_arr, s_obsloc, file="C:\\Users\\dhanu\\OneDrive\\FirstArrival\\SpatialMaxID\\Results2\\magwarearlyarrival.Rdata")
