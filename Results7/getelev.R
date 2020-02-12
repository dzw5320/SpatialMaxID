#Get elevation raster


if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('raster')) install.packages('raster'); library('raster')
if (!require('elevatr')) install.packages('elevatr'); library('elevatr')
if (!require('sf')) install.packages('sf'); library('sf')
if (!require('exactextractr')) install.packages('exactextractr'); library('exactextractr')

Counties<-c("New Hampshire","Pennsylvania","Michigan","Maryland",          
            "Ohio","Illinois","Wisconsin","New Jersey",          
            "New York","Vermont","Maine","Connecticut",         
            "Rhode Island","District of Columbia", "Massachusetts", "Virginia",            
            "Indiana", "West Virginia", "Delaware" )

getdat1 <- getData(name="GADM", country="USA", level=2)
County_sp <- subset(getdat1, NAME_1 %in% Counties)

#I had trouble running the next line with zoom level (z)>6. If z=12 takes too much memory try 9<z<12.
elevation_data <-get_elev_raster(County_sp, z=6, src = "aws")#Elevation is in meters


County_sp$mean_elevation <- exact_extract(
  elevation_data,
  sf::st_as_sfc(County_sp),
  fun=function(x, w) { weighted.mean(x) })


#Visualize
if (!require('RColorBrewer')) install.packages('RColorBrewer'); library('RColorBrewer')


xbox = c(-95, -65)
ybox = c(35,  50)



my.palette <- brewer.pal(n=9, name = "OrRd")
spplot(County_sp,"mean_elevation",  xlim=xbox, ylim=ybox, col.regions=my.palette, cuts=8, main=list(label="mean elevation (m)"))


# Save Results
######edit path#######
save(County_sp, file="C:\\Users\\dhanu\\OneDrive\\FirstArrival\\SpatialMaxID\\Results7\\elev.Rdata")




state_county$elevation<-NA

for(i in c(1:nrow(state_county))){
  cat(i, "\n")
  state_county$elevation[i]<-County_sp$mean_elevation[County_sp$NAME_1==state_county$state[i] & County_sp$NAME_2==state_county$county[i]]
  
}

County$ELEVATION<-NA

for(i in c(1:nrow(County))){
  cat(i, "\n")
  County$ELEVATION[i]<-state_county[state_county$state==County$STATE[i] & state_county$county==County$COUNTY[i], 5]
 
}


save(state_county, County, early_arr, early_arr_rmNA,NA_ind,  file="C:\\Users\\dhanu\\OneDrive\\FirstArrival\\SpatialMaxID\\Results7\\magwarnew.Rdata")

