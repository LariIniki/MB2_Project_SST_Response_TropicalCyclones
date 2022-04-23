#####################################################################################################
# Sea Surface Temperature Response to Tropical Cyclones in 2020
# Project Work by Larissa Gorzawski

# MB 2: Introduction to Programming and Geostatistics
#####################################################################################################

#----------------------------------------------------------------------------------------------------
# PRELIMINARIES
#----------------------------------------------------------------------------------------------------

# Required packages

# An installation of RNetCDF is required to read the sea surface temperature files

#-----------------------------------

library(lubridate)
library(tidyverse)
library(sp)
library(sf)
library(raster)
library(rnaturalearth)
library(RCurl)
library(httr)
library(rgeos)
library(colorspace)
library(RNetCDF)

# Set working directory
setwd('L:/EAGLE/04_R_introduction/')



#  Import Data
#----------------------------------------------------------------------

# Tropical Cyclone Tracks
#--------------------------------------
# Download from NOAA's International Best Track Archive for Climate Stewardship (IBTrACS) data
# at https://www.ncei.noaa.gov/products/international-best-track-archive?name=ib-v4-access

# Files are available in NetCDF4, CSV and Shapefile format
# In this case, csv data from the past three years will be sufficient 
#   - since only storms in 2020 will be observed 
#   - and Lon/Lat Coordinates can be coerced to spatial points easily later in the workflow

# Set filetype csv and filename to download
# Main files to choose from are
#     - Active storms: ibtracs.ACTIVE.list.v04r00.csv
#     - All storms: ibtracs.ALL.list.v04r00.csv
#     - Storms since 1980 (beginning of satellite observation): ibtracs.since1980.list.v04r00.csv
#     - Storms in the last three years: ibtracs.last3years.list.v04r00.csv
filetype = 'csv'
http_file = 'ibtracs.last3years.list.v04r00.csv'
# Assign a filename to save the data to
tc_filename = './TCs.csv'

# Assemble URL to download 
url_base = 'https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/'
url= paste0(url_base,filetype,'/',http_file)

# Download the file...
download.file(url= url, destfile=tc_filename, method='curl')

# ... and read it in
tc_original <- read.csv(tc_filename, stringsAsFactors = FALSE, na.strings = '')



# Monthly Sea Surface Temperature (SST)
#--------------------------------------
# Obtained from https://podaac-tools.jpl.nasa.gov/drive/login?dest=L2RyaXZlLw
# Requires an Earthdata Login, will be deprececated from June 6th 2022


# As long as it still works, the drive can be accesssed directly, although it is quite slow
# MODIS SST PODAAC filepath
#modispath_podaac = 'P:/OceanTemperature/modis/L3/aqua/4um/v2019.0/4km/daily/2020/'

# Alternatively, use the custom copy.modis() function to copy necessry files to local hard drive later in the scipt
# Set MODIS SST local path
modispath = 'L:/EAGLE/04_R_introduction/MODIS_SST/'


# Ancilliary data
#--------------------------------------

# natural Earth Countries for plots
world = ne_countries(returnclass = "sf")


# Define helper function to copy files if necessary:
#----------------------------------------------------------------------

# Function to copy files from a list of subfolders (subfolder_list) from an original location (parent_original)
# to another location (parent_dest)
copy.files = function(subfolder_list, parent_original,parent_dest){
  
  for (i in 1:length(subfolder_list)){
    
    folder = subfolder_list[i]
    
    src_folder = paste0(parent_original,folder)
    files = list.files(src_folder, full.names = T)
    rm = list.files(src_folder, pattern = c('*.NRT.nc','*.NRT.nc.md5'),full.names = T)
    files = files[!files%in%rm]
    
    dst_folder = paste0(parent_dest,folder)
    if (!dir.exists(dst_folder)) {dir.create(dst_folder)}
    
    file.copy(files, dst_folder)
    
  }
  
}



#----------------------------------------------------------------------------------------------------
# PART ONE: FORMAT AND PLOT IBTRACKS DATA
#----------------------------------------------------------------------------------------------------

# Format
#----------------------------------------------------------------------
# Remove additional or provisional tracks since they don't have recorded wind speeds
tc_tab = tc_original[tc_original['TRACK_TYPE']=='main',]

# SUbset the 2020 season
tc_tab = tc_tab[tc_tab$SEASON == '2020',]

# Number of storms in 2020:
length(unique(tc_tab[,'NAME']))



# Plot Hurricanes
#----------------------------------------------------------------------
# First, coerce the table to spatial points:
# Subset lon and lat and covert to numeric
coords = tc_tab[,c('LON','LAT')]
coords = apply(coords, 2, function(x) as.numeric(x))
# Make a spatial points dataframe from the coordinates and the data
tc_sp = SpatialPointsDataFrame(coords, tc_tab, proj4string = CRS("+init=epsg:4326"))

# Filter the major storms (Tropical Depression or stronger):
# The USA_SSHS column contains the Saffir-Simpson Wind Scale (SSWS) Category for each point.
# The SSWS classifies Hurricanes (Western hemisphere tropical cyclones) exceeding a maximum mean 
# sustained windspeed of 74 mph (119 km/h) over one minute into five categories (Cat 1-5).
# Here, the two preceeding intensity classes of Tropical Depression and Tropical Storm (-1 and 0) will
# be included, since they allow a better visualization of the forming of the storms.
hurricanes = tc_sp[tc_sp$USA_SSHS >= -1,]

# Define a colour scale (from the viridis scale) and Labels for the Saffir-Simpson Scale
colorscale = c('#fde725','#90d743', '#35b779','#21918c','#31688e','#443983','#440154','#000004')
colorlabels = c('Tropical Depression','Tropical Storm','Cat 1','Cat 2','Cat 3','Cat 4','Cat 5')


# Plot all Hurricanes in 2020
#--------------------------------------

# Plot TC tracks coloured by SSWS category over the world map
ggplot(world) +
  geom_sf()+
  # Add the hurricanes
  geom_sf(data = st_as_sf(hurricanes), mapping = aes(color = factor(USA_SSHS)), size = 1.2)+
  # Assign defined color scale
  scale_color_manual(values = colorscale, labels = colorlabels, name = 'SSWS')+
  # Set a title
  ggtitle('Global Hurricanes in 2020')


#Plot the Atlantic Hurricane Season 2020
#--------------------------------------
# Subset by Basin (North Atlantic)
nab = hurricanes[hurricanes$BASIN == 'NA',]

# Plot TC tracks coloured by SSWS category
ggplot(world) +
  geom_sf()+
  # Zoom in on the storms by setting limits close to the Points' extent
  xlim(nab@bbox[1]-10, nab@bbox[3]+10)+
  ylim(nab@bbox[2]-10, nab@bbox[4]+10)+
  geom_sf(data = st_as_sf(nab), mapping = aes(color = factor(USA_SSHS)), size = 1.2)+
  scale_color_manual(values = colorscale, labels = colorlabels, name = 'SSWS')+
  ggtitle('Atlantic Hurricanes in 2020')


# Compute the maximum intensity per storm and filter the ones that acually reach Cat 1 or stronger (14 storms)
nab@data <- nab@data%>%
  group_by(NAME) %>%
  mutate(max_cat = max(USA_SSHS))%>%
  ungroup()

nab = nab[nab$max_cat>=1,]


#----------------------------------------------------------------------------------------------------
# PART TWO: TEMPERATUR RESPONSE OF ONE STORM
#----------------------------------------------------------------------------------------------------

# Subset one Storm and Plot:
#----------------------------------------------------------------------

# Print names of available storms for subsetting
unique(nab$NAME)

# Select one exemplary storm - in this case Teddy, the longest Cat 4 storm of the season.
name = 'TEDDY'
storm = nab[nab$NAME==name,]

ggplot(world) +
  geom_sf()+
  # Zoom in on the storms
  xlim(storm@bbox[1]-5, storm@bbox[3]+5)+
  ylim(storm@bbox[2]-5, storm@bbox[4]+5)+
  geom_sf(data = st_as_sf(storm), mapping = aes(color = factor(USA_SSHS)), size = 1.2)+
  scale_color_manual(values = colorscale, labels = colorlabels, name = 'SSWS')+ #
  ggtitle(paste0('Hurricane ',name))

# Here the storm's progression and development can be seen. It doesn't make landfall, 
# so the ocean temperature response can be fully observed.



# Read in and process Sea Surface Temperature data
#----------------------------------------------------------------------
# To investigate the temperature response in the wake of the hurricane, the satellite data will 
# be accessed from 10 days before to 5 days after the storm's passing in a 500km radius.

# The daily temperature data is organised in folders for each day, named after day of the year
# To get the full time range needed for the whole season, convert the dates to Day of year 
dates = yday(as.Date(nab$ISO_TIME))
fullrange = seq(dates[1]-10, dates[length(dates)]+5)

# if necessary, copy the files for the full date range from podaac-drive to your local system
#copy.files(fullrange,modispath_podaac,modispath)


# Function to read MODIS data
# Pass a range of days as DOY
read.sst = function(doys) {
  # Initialize empty list
  sst_list = list()
  
  # Read in the files
  for (j in 1:length(doys)){
    folder = paste0(modispath,doys[j])
    files = list.files(folder,pattern = '*.4km.nc', full.names = TRUE)
    
    rm = list.files(folder, pattern = '.md5', full.names = TRUE)
    files = files[!files%in%rm]
    
    sst_list = append(sst_list,files)
    
  }
  
  sst = stack(sst_list)
  return(sst)
}

# Function to extract mean temperature from raster time series
# Pass a raster time series stack, a spatial point and a buffer radius in meters
ts.buffered.mean = function(stack, point, radius) {
  
  # Buffer the point with the given radius
  # Use the raster buffer funtion, which works also with Lon/lat coordinates -> width in Meters!
  buffered = buffer(point, width = radius)
  
  # Crop and mask raster stack
  r2 <- crop(stack, extent(buffered))
  r3 <- mask(r2, buffered)
  
  # Calculate the average value over the time series
  mean = mean(r3, na.rm = TRUE)
  
  return(mean)
}

# Loop over the storm points and process the data for each location track point
#--------------------------------------------
# Also wrap into function for further use
delta.sst = function(storm, radius){
  # Initiate list
  stats_list = list()
  for (i in 1:length(storm)) {
    
    # Subset the point
    point = storm[i,]
    
    # Coerce date string to yday object
    date = yday(as.Date(point$ISO_TIME[1]))
    # Pre-storm conditions from 10 to 3 days prior
    pre = seq(date-10, date-3)
    # Post-storm conditions from 0 to 5 days after
    post = seq(date, date+5)
    
    # Use function to read in the modis temperature data
    pre_sst = read.sst(pre)
    post_sst = read.sst(post)
    
    # Use the function to extract the temporal mean for pre- and post storm conditions
    pre_mean = ts.buffered.mean(pre_sst,point, radius)
    post_mean = ts.buffered.mean(post_sst,point, radius)
    
    # Calculate the temperature difference 
    delta = post_mean - pre_mean
    delta@data@names = 'delta'
    
    # Append to the list
    stats_list = append(stats_list, delta)
    
  }
  
  # Return the list of rasters
  return(stats_list)
  
}

# Now, set the radius to 500km in meters
radius = 500000

# Apply on Teddy
raster_list = delta.sst(storm, radius)

# Plot one random raster to check
plot(raster_list[[20]])

# Mosaic the temperature raster by calling the mosaic function over the whole list
raster_list$fun <- median
mos <- do.call(mosaic, raster_list)


# Plot the mosaic with the storm points
plot(mos[[1]])
plot(storm, add = T)
plot(world, col = 'gray', add  = T)


# This already shows a visible cooling of the surface in the storms's wake!

# As an additional measure, the temperature difference in the storm's vicinity can be visualized over 
# its intentsity progression. With the distance to the point, the temperature difference can also 
# be plotted as a function of the distance to the storm center track.

#----------------------------------------------------------------------------------------------------
# PART THREE: ADDITIONAL ANALYSIS OF SST RESPONSE
#----------------------------------------------------------------------------------------------------

# Plot SST difference as a function of distance to the center track point
#----------------------------------------------------------------------


# Funtion to extract delta sst values along a horizontal profile line
# Pass a point of interest (spatial point), a raster layer of interest and 
# the desired number of values to extract (integer)
extract.hor.profile = function(poi, rasoi, npoints) {
  
  # Get coordinates for straight line from the raster extent
  #xhalf = (rasoi@extent@xmax - rasoi@extent@xmin)/2
  yhalf = (rasoi@extent@ymax - rasoi@extent@ymin)/2
  
  #east
  epx = rasoi@extent@xmax
  epy = rasoi@extent@ymin + yhalf
  #west
  wpx = rasoi@extent@xmin
  wpy = rasoi@extent@ymin + yhalf
  
  # Create an sf linestring from one side of the raster to the other
  sfc_line = st_as_sfc(c(paste0("LINESTRING(",as.character(wpx),' ',as.character(wpy),',',as.character(epx),' ',as.character(epy),')')))
  
  # Sample points from the line string
  points = st_line_sample(sfc_line, npoints)
  
  # Coerce to spatial points and assign projection
  ps = SpatialPoints(as_Spatial(points))
  ps@proj4string = poi@proj4string
  
  # Extract values and calculate distance to center point
  vals = raster::extract(rasoi$delta,ps)
  dist = pointDistance(poi, ps, lonlat=TRUE)
  
  # Combine into a dataframe
  valdf = data.frame(distance = dist, delta = vals)
  
  # Format a little: Assign the center point distance 0, convert distance to km and round
  valdf$distance[which.min(valdf$distance)]=0
  valdf$dist_km = round(valdf$distance/1000,2)
  valdf$delta = round(valdf$delta,3)
  
  # Append the point coordinates
  valdf$lon=as.numeric(ps@coords[,1])
  valdf$lat=as.numeric(ps@coords[,2])
  
  # Assign the western points a negative distance value
  valdf$dist_km[which(valdf$lon<valdf$lon[which.min(valdf$distance)])] = valdf$dist_km[which(valdf$lon<valdf$lon[which.min(valdf$distance)])]*-1
  
  # Append the storm name, ssws category and bin identifier
  valdf$name = poi$NAME
  valdf$cat = poi$USA_SSHS
  valdf$bin = seq(1,25)
 
  # return the dataframe 
  return(valdf)
  
}


# Caluculate delta per distance bin for a storm
sst.dist.loop = function(storm, raster_list, npoints){
  
  # Initiate a dataframe to store values
  single_dist_df = data.frame(distance = as.numeric(), delta=as.numeric(), dist_km=as.numeric(), 
                              lon=as.numeric(), lat=as.numeric(), name=as.character(), cat=as.numeric(), bin = as.numeric(),id = as.numeric())
  
  # Extract values for the whole storm
  for (i in 1:length(storm)) {
    
    # Subset the point from the storm and the respective delta raster from the list of rasters
    p = storm[i,]
    ras = raster_list[[i]]
    
    # Extract values
    df = extract.hor.profile(p,ras,npoints)
    # Set a unique point id
    df$id = i
    
    # Bind values to the master dataframe
    single_dist_df = rbind(single_dist_df,df)
    
  }
  
  
  # Get the mean delta values per Category and Distance bin
  av_df = single_dist_df%>%
    group_by(cat,bin)%>%
    summarise(med_delta = median(delta, na.rm = T), dist_km = mean(dist_km))%>%
    ungroup()
  
  av_df$name = storm$NAME[1]
  
  return(av_df)
  
}


# Set number of points (one center point + 12 bins on each side)
npoints = 25

# Apply:
av_single_dist = sst.dist.loop(storm, raster_list, npoints)

# Plot the temperatur difference as funtion of distance to storm center point
ggplot(av_single_dist, aes(x = dist_km, y = med_delta, color = factor(cat)))+
  geom_line(size = 1)+
  geom_hline(yintercept=0, color = "blue")+
  xlab('Distance to center track point [km]')+
  ylab('Median Delta SST after storm passing [°C]')+
  ggtitle(paste0('Sea surface temperature difference relative to the distance \nto storm center track point of ',name))+
  scale_color_manual(values = colorscale, labels = colorlabels, name = 'SSWS')

# Here can be seen that the lower temperatures occur mostly close to the storm center, and generally
# also during the stronger winds (Cat 1 and higher)

# Therefore, in a second analysis, the temperature difference close to the storm center will be 
# investigated along the progression of the storm.

# Aggregate mean temperature response within the storm radius
#----------------------------------------------------------------------

# Function to calculate the mean temperature difference within the storm's maximum wind radius for a point
# Pass a point and the corresponding temperature raster
mean.in.windradius=function(poi, rasoi) {
  
  # Maximum wind radius is given in USA_RMW in nautical miles
  rnm = as.numeric(poi$USA_RMW[1])
  
  # Convert to meters for buffering
  rm = rnm * 1852
  
  # Use the buffer function (works also with a single layer) to get buffered raster
  radras = ts.buffered.mean(rasoi,poi,rm)
  # Calculate mean value
  mean = cellStats(radras, stat='mean', na.rm=TRUE)
  
  return(mean)
  
}


sst.mean.wind = function(storm,raster_list){
  
  # Set up a dataframe
  sst_meanr_df = data.frame(id = as.numeric(), meandelta=as.numeric(), cat=as.numeric())
  
  for (i in 1:length(storm)){
    
    #Subset point
    poi = storm[i,]
    # Subset corresponding delta raster (first one in stack)
    rasoi = raster_list[[i]][[1]]
    
    # Caclulate mean temperature difference within radius
    m = mean.in.windradius(poi,rasoi)
    
    sst_meanr_df[nrow(sst_meanr_df) + 1,] = c(i, m, poi$USA_SSHS)
    
  }
  
  return(sst_meanr_df)
  
}

# Apply:
sst_meanr_df = sst.mean.wind(storm,raster_list)


# Plot
ggplot(sst_meanr_df, aes(x = id, y = meandelta))+
  geom_line(size = 1)+
  geom_point(aes(color = factor(cat )))+
  xlab('Day of progression')+
  ylab('Mean Delta SST within the maximum wind radius [°C]')+
  ggtitle('Mean SST Response over the storms progression')+
  scale_color_manual(values = colorscale, labels = colorlabels, name = 'SSWS')



#----------------------------------------------------------------------------------------------------
# PART THREE: SEASON AVERAGES
#----------------------------------------------------------------------------------------------------

# Finally, the analysis can be executed and averaged over all hurricanes in the 2020 season!
# Repeat the process over the whole NA basin dataframe instead of a single storm.
#----------------------------------------------------------------------

names = unique(nab$NAME)

# Re-set analysis variables
radius = 500000
npoints = 25

# Initiate dataframes:
# SST distance dataframe
nab_dist_df = data.frame(cat = as.numeric(), bin = as.numeric(), mean = as.numeric(),dist_km = as.numeric(), name = as.character()) 

# SST mean wind dataframe
nab_meanw_df =  data.frame(id = as.numeric(), meandelta=as.numeric(), cat=as.numeric(), name = as.character())

for (i in 1:length(names)){
  
  # Subset the storm
  storm = nab[nab$NAME==names[i],]
  
  
  # Calculate delta sst rasters:
  #----------------------------------------------------------
  raster_list = delta.sst(storm, radius)
  
  # Delta SST relative to distance to the center track point:
  #----------------------------------------------------------
  
  dist_single = sst.dist.loop(storm, raster_list, npoints)
  nab_dist_df = rbind(nab_dist_df,dist_single)
  
  # Delta SST within maximum wind radius:
  #----------------------------------------------------------
  meanr_single = sst.mean.wind(storm,raster_list)
  meanr_single$name = storm$NAME[1]
  nab_meanw_df = rbind(nab_meanw_df,meanr_single)
  
  
}


# Summarize and plot the median sst response
#----------------------------------------------------------------------

# First, for the sst response relatie to the distance to the storm center
mean_dist = nab_dist_df%>%
  group_by(cat,bin)%>%
  summarise(med_delta = median(med_delta, na.rm = T), dist_km = mean(dist_km))%>%
  ungroup()

# Plot the median temperatur difference as funtion of distance to storm center point for the whole season 2020 
ggplot(mean_dist, aes(x = dist_km, y = med_delta, color = factor(cat)))+
  geom_line(size = 1)+
  geom_hline(yintercept=0, color = "blue")+
  xlab('Distance to center track point [km]')+
  ylab('Median delta SST after storm passing [°C]')+
  ggtitle('Sea surface temperature response relative to the distance \nto storm center track point in 2020')+
  scale_color_manual(values = colorscale, labels = colorlabels, name = 'SSWS')



# And second, for the mean sst response within the maximum wind radius

# Reformat the data a little: add maximum category per storm
meanw_toplot = nab_meanw_df%>%
  group_by(name)%>%
  mutate(max_cat = max(cat))%>%
  ungroup()
# and merge with spatial points
points_toplot = nab[,c('NAME','USA_SSHS','LON','LAT')]
points_toplot@data = cbind(points_toplot@data,meanw_toplot)
# Remove points with NA mean temperature difference
points_toplot= points_toplot[!is.na(points_toplot@data$meandelta),]

# Plot TC tracks coloured by SST difference within maximum wind radius
ggplot(world) +
  geom_sf()+
  # Zoom in on the storms by setting limits close to the Points' extent
  xlim(nab@bbox[1]-3, nab@bbox[3]+3)+
  ylim(nab@bbox[2]-3, nab@bbox[4]+3)+
  geom_sf(data = st_as_sf(points_toplot), mapping = aes(color = meandelta))+
  scale_colour_continuous(type='viridis', name = 'Mean SST \ndifference')+
  ggtitle('Mean temperature response within maximum wind radius\nof the atlantic hurricanes 2020')

# The map shows the general cooling in most cases, and the more extreme temperature differences, which 
# occur also in more cases at the end of the tracks.
# There is also an oulier with a positive temperature difference, which could also be due to 
# other natural influences like the Wkulla river delta.


# Show delts SST value range for each storm with a boxplot
ggplot(meanw_toplot, aes(x = name, y = meandelta, fill = factor(max_cat)))+
  geom_boxplot()+
  scale_fill_manual(values = colorscale[3:7], labels = colorlabels[3:7], name = 'Maximum SSWS Category')+
  geom_hline(yintercept=0, color = "blue")+
  scale_y_continuous(breaks = seq(-5, 3, by = 1))+
  ggtitle('Mean SST Responses per Storm')+
  xlab('Storm')+
  ylab('Mean delta SST within the maximum wind radius [°C]')+
  coord_flip()

# This very nicely shows the well-known fact, that the ocean surface cools down in the wake of tropical
# cyclones. Although the amount of cooling seems to be generally higher when the storm reaches the 
# status of hurricane, the difference is not directly tied to the maximum reached category.

# Of course, clouds also cause gaps in the sst dataset used, and there may be other factors, especially
# closer to the coast, which influence the ocean temperature as well.



#####################################################################################################

# Methodology partly after:
#----------------------------------------------------------------------------------------------------
# Dare, R. A., & McBride, J. L. (2011). Sea surface temperature response to tropical cyclones. Monthly Weather Review, 139(12), 3798-3808.
# Reul, N., Chapron, B., Grodsky, S. A., Guimbard, S., Kudryavtsev, V., Foltz, G. R., & Balaguru, K. (2021). Satellite observations of the sea surface salinity response to tropical cyclones. Geophysical research letters, 48(1), e2020GL091478.