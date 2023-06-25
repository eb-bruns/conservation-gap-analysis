################################################################################

## proected_area_coverage.R
### Author: Emily Beckman Bruns
### Creation date: 03/21/2022
### Last updated: 05/23/2022

### FUNDING:
## USDA Forest Service (Cooperative Agreement 16-CA-11132546-045 with
#        The Morton Arboretum)

### DESCRIPTION:
## This script calculates protected area coverage for target species using two
#   methods...
## 1) point-in-polygon: count number of wild occurrence points that fall within
#     protected areas polygons
## 2) polygon intersection (buffer-in-polygon): add 50km buffers around
#     wild occurrence points, then calculate the buffer area covered by
#     protected areas polygons (intersection)
## The script does not run automatically but requires edits and decisions
#   depending on your target species, the format of your occurrence points, etc.
## The point-in-polygon method is RAM-intensive on a personal computer, but
#   relatively fast on a powerful machine (a few minutes on a 129MB server).
#   In contrast, the polygon intersection method is very memory-intensive and,
#   depending on the species distribution (smaller dist = faster calc), the
#   calculation can run a few hours to multiple days.

### DATA IN:
### TARGET SPECIES
## You need a list of target species, which can either be listed verbatim in
#   this script, or can be read in as a CSV file. If a CSV file, you need at 
#   least a "species_name_acc" (accepted name) column.
### OCCURRENCE POINTS
## In situ occurrence points (latitude and longitude in decimal degrees)
# 	Can use the output from 3-1_refine_occurrence_points.R
# 	https://github.com/MortonArb-CollectionsValue/OccurrencePoints/tree/master/scripts
# 	Each file has data for only one species and is named "Genus_species.csv"
# 	You can read in data for mult. species in one file but need to edit the
#		code to split after reading in
## Ex situ wild localities (latitude and longitude in decimal degrees)
# 	Can use the output from 3-1_refine_occurrence_points.R, which has a
#		"database" column that has "Ex_situ" to distinguish the ex situ records
#		from the rest of the in situ records
## The workflow below also uses other files that edit the species occurrence
#   points, but these are optional. They come from the workflow outlined here
#   https://github.com/MortonArb-CollectionsValue/OccurrencePoints and include:
#   1) manual_point_edits.csv & 2) target_taxa_with_native_dist.csv
### ECOREGIONS
## U.S. Ecoregions
#		EPA Level IV Ecoregions
#   https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l4_state_boundaries.zip
### PROTECTED AREAS (PAs)
## Global PAs
# 	World Database on Protected Areas (WDPA)
#		https://www.iucn.org/theme/protected-areas/our-work/world-database-protected-areas
### COUNTRY BOUNDARIES
## Global country boundaries
#		UIA World Countries Boundaries, UNIGIS Geospatial Education Resources, via
#		ArcGIS Hub Shapefile
#		https://hub.arcgis.com/datasets/252471276c9941729543be8789e06e12_0

### DATA OUT:
### CSV with results from analyses, including:
# species	     | EOO	                      | dist_filter	                                          | num_pt_unthinned                            | km_thin_value                     | num_pt_thinned              | num_pt_in_pa            | per_pt_in_pa          | area_buff                       | area_pa_in_buff            | per_pa_in_buff
# species name | Extent of Occurrence (km2) | [optional, filter used for countries of distribution] | number of occurrence points before thinning | kilometer value used for thinning | number of pts post-thinning | number of points in PAs | percent of pts in PAs | area of 50km buffers around pts | area of PAs within buffers | percent of buffer area covered by PAs
### Can also create maps that show examples of PA coverage using the two different
#   methods for calculating protected area coverage

################################################################################
# Load libraries
################################################################################

## [code chunk from Shannon M. Still]
my.packages <- c("raster","ConR","dplyr","sf","spThin","sp","lwgeom",
                 "rgdal","rgeos") #these will be retired by end of 2023
#install.packages(my.packages) # turn on to install current versions
lapply(my.packages, require, character.only=TRUE)

# packages for running parallel processing
library("foreach")
library("doSNOW")

sessionInfo()

################################################################################
# Set working directory
################################################################################

# either set manually:
setwd("Desktop/*work/PA-test")
#main_dir <- "/Volumes/GoogleDrive-103729429307302508433/Shared drives/IMLS MFA/occurrence_points"

# or use 0-1_set_workingdirectory.R script from OccurrencePoints repo:
#source("./Documents/GitHub/OccurrencePoints/scripts/0-1_set_workingdirectory.R")

################################################################################
################################################################################
# if desired, can run this to load data in next few sections, then skip to
#   "Calculate protected area coverage"
#load("~/PA-Analysis-Setup_04_25_22.RData")
################################################################################
################################################################################

################################################################################
# Load functions
################################################################################

# clip points by boundary so only in target area
# (helpful if focusing on one country/region)
clip.by.boundary <- function(pts,pt_proj,boundary){
  # select coordinate columns
  latlong <- pts %>% select(decimalLongitude,decimalLatitude)
  # turn occurrence point data into a SpatialPointsDataFrame
  spatial_pts <- SpatialPointsDataFrame(latlong, pts, proj4string = pt_proj)
  # clip by boundary created earlier
  spatial_pts <- spatial_pts[boundary, ]
  # keep just the data (not the spatial info you added)
  pts_new <- spatial_pts@data
  return(pts_new)
}

# create buffers around points, using specified projection
create.buffers <- function(df,radius,pt_proj,buff_proj,boundary){
  # select coordinate columns
  latlong <- df %>% select(decimalLongitude,decimalLatitude)
  # turn occurrence point data into a SpatialPointsDataFrame
  sp_df <- SpatialPointsDataFrame(latlong, df, proj4string = pt_proj)
  # reproject SpatialPointsDataFrame to specified projection
  proj_df <- spTransform(sp_df,buff_proj)
  # place buffer around each point
  buffers <- buffer(proj_df,width=radius,dissolve=T)
  # clip buffers by boundary (e.g., state, country)
  buffers_clip <- raster::intersect(buffers,boundary)
  # return buffer polygons
  return(buffers_clip)
}

# calculate percent of occurrence points in global protected areas (WDPA)
#		first thin points, then see which are in PAs, then calculate percent
pts.in.pa <- function(occ.pts,lat_col,long_col,sp_col,thin.km,my.proj,pa.list){
  # first thin points; thin.km gives the distance between thinned pts
  thin_pts <-
    spThin::thin(
      loc.data=occ.pts,
      lat.col=lat_col,
      long.col=long_col,
      spec.col=sp_col,
      thin.par=thin.km, #kilometers b/w pts
      reps=1, #right now we are just doing once, but should prob do more?
      locs.thinned.list.return=T,
      write.files=F,
      write.log.file=F)
  thinned_pts <- thin_pts[[1]]
  # count number of points after thinning
  num_thinned <- nrow(thinned_pts)
  # make thinned points a spatial object
  thinned_spatial <- SpatialPointsDataFrame(thinned_pts,thinned_pts,proj4string=my.proj)
  # see which points are in protected areas polygons
  #		WDPA comes in three parts; I've left it like that to
  #		hopefully make this calculation a little faster
  pt_in_pa_list <- lapply(pa.list, function(x) thinned_spatial[x,])
  # create one object with all pts found in PAs
  pt_in_pa <- do.call(rbind, pt_in_pa_list)
  # count number of points in PAs
  in_pa <- nrow(pt_in_pa@data)
  # create dataframe of values to return
  pt_in_pa_stats <- data.frame(
    num_pt_unthinned = nrow(occ.pts),
    km_thin_value = thin.km,
    num_pt_thinned = num_thinned,
    num_pt_in_pa = in_pa,
    per_pt_in_pa = round(((in_pa/num_thinned)*100),digits=2))
  return(pt_in_pa_stats)
}

# calculate area of intersection of 50km buffers around occurrence points and
#   global protected areas (WDPA)
buffer.in.pa <- function(occ.pts,radius,pt_proj,buff_proj,pa.list,boundary){
  #
  ## create 50km buffers around in situ points (aea proj) & calculate area
  ##   fast using sp method, slow using sf - not sure exactly why
  # sp method (fast):
  buff_50_sp <- create.buffers(occ.pts,radius,pt_proj,buff_proj,boundary)
  buff_50_sp <- raster::aggregate(buff_50_sp)
  # calculate area
  #buff_area_sp <- buff_50_sp@polygons[[1]]@area/1000000
  # sf method (slow):
  #insitu_sf <- st_as_sf(x = occ.pts,
  #  coords = c("decimalLongitude", "decimalLatitude"),
  #  crs = pt_proj)
  #buff_50_sf <- st_union(st_buffer(insitu_sf,radius))
  # can just convert sp version to sf
  buff_50_sf <- st_as_sf(buff_50_sp)
  # calculate area
  buff_area_sf <- as.numeric(st_area(buff_50_sf)/1000000)
  # see difference between buffer areas calculated using sp and sf
  #buff_diff_num <- (buff_area_sp - buff_area_sf)
  #buff_diff_per <- buff_diff_num/buff_area_sp*100
  #
  ## loop through PA layers (there are 3) and find intersection b/w
  ##  PAs and buffer around occurrence points
  pa_buff_intersect_lst <- list()
  for(i in 1:length(pa.list)){
    ## sp (spatial polygons) method --> SLOW
    #pa_layer_aea <- spTransform(pa.list[[1]],buff_proj)
    #pa_intersect_id <- sp::over(buff_50_sp,pa_layer_aea)
    #pa_intersect_sp <- gIntersection(buff_50_sp,pa.list[[1]],checkValidity=2L)
    ## sf (simple features) method --> faster
    # select PA features that overlap with buffer layer
    pa_intersect_id <- st_intersects(buff_50_sf,pa.list[[i]]) %>% unlist()
    pa_intersect <- st_union(pa.list[[i]][pa_intersect_id,])
    # intersect the buffer and PA layers to get overlap
    buffer_in_pa <- st_union(st_intersection(buff_50_sf,pa_intersect))
    # add results to list
    pa_buff_intersect_lst[[i]] <- buffer_in_pa
  }
  #
  ## combine all PA intersection layers & calc percent coverage
  pa_buff_intersect <- Reduce(st_union,pa_buff_intersect_lst)
    # visualize
    #plot(buff_50_sf); plot(pa_buff_intersect,add=T)
  #	compare the buffer area to the buff_pa_inter area to get % coverage
  pa_intersect_area <- as.numeric(st_area(pa_buff_intersect)/1000000)
  pa_intersect_per <- pa_intersect_area/buff_area_sf*100
  #
  ## create dataframe of values to return
  buff_in_pa_stats <- data.frame(
    area_buff = round(buff_area_sf,digits=2),
    #area_buff_sp = buff_area_sp,
    #compare_buff_num = buff_diff_num,
    #compare_buff_per = buff_diff_per,
    area_pa_in_buff = round(pa_intersect_area,digits=2),
    per_pa_in_buff = round(pa_intersect_per,digits=2))
  return(buff_in_pa_stats)
}

################################################################################
# Choose target species
################################################################################

# read in target taxa list
taxon_list <- read.csv("target_species_with_syn.csv",
                       header = T, na.strings = c("","NA"),colClasses = "character")
head(taxon_list)

# OPTIONAL, depending on your workflow:
#		read in manual edits to target species maps
pt_edits <- read.csv("manual_point_edits.csv",
                     header = T, na.strings = c("","NA"),colClasses = "character")
head(pt_edits)

# OPTIONAL: select target species
target_sp <- taxon_list %>% filter(grepl("^MAP",map_flag))
target_sp <- sort(gsub(" ","_",target_sp$species_name_acc))
length(target_sp) #30

# OPTIONAL: load native dist information for each species
native_dist <- read.csv("target_taxa_with_native_dist.csv",
                        header = T, na.strings = c("","NA"), colClasses = "character")
native_dist <- native_dist %>% dplyr::select(species_name_acc,rl_native_dist,
                                             gts_native_dist)

################################################################################
# Read in polygon data
################################################################################

## define projections
  # WGS84, used for point calculations
wgs.proj <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84
	+no_defs +towgs84=0,0,0")
  # Albers Equal Area, used for area calculations (meters)
aea.proj <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110
	+x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m")

## read in shapefile of global protected areas
## CITATION: UNEP-WCMC and IUCN (2022), Protected Planet: The World Database on Protected Areas (WDPA) [Online], February 2022, Cambridge, UK: UNEP-WCMC and IUCN. Available at: www.protectedplanet.net.
# As of January 2022, the WDPA includes data on 269,457 protected areas of
#		which 96% were polygons and 4% points.
# since the shapefile is large, the source has it split into three;
#		we will load each

# need to be unzipped the first time
#unzip(zipfile="WDPA_WDOECM_Mar2022_Public_all_shp/WDPA_WDOECM_Mar2022_Public_all_shp_0.zip",
#      exdir="WDPA_WDOECM_Mar2022_Public_all_shp_0")
#unzip(zipfile="WDPA_WDOECM_Mar2022_Public_all_shp/WDPA_WDOECM_Mar2022_Public_all_shp_1.zip",
#      exdir="WDPA_WDOECM_Mar2022_Public_all_shp_1")
#unzip(zipfile="WDPA_WDOECM_Mar2022_Public_all_shp/WDPA_WDOECM_Mar2022_Public_all_shp_2.zip",
#      exdir="WDPA_WDOECM_Mar2022_Public_all_shp_2")
# two ways to read in shapefiles...

## 1) read in using raster package (spatial polygons; for point-in-poly analysis)
  # WGS84 projection
pa0 <- readOGR("WDPA_WDOECM_Mar2022_Public_all_shp_0/WDPA_WDOECM_Mar2022_Public_all_shp-polygons.shp")
  # project to WGS proj that we've defined, so its exact
  pa0 <- spTransform(pa0,wgs.proj)
pa1 <- readOGR("WDPA_WDOECM_Mar2022_Public_all_shp_1/WDPA_WDOECM_Mar2022_Public_all_shp-polygons.shp")
  pa1 <- spTransform(pa1,wgs.proj)
pa2 <- readOGR("WDPA_WDOECM_Mar2022_Public_all_shp_2/WDPA_WDOECM_Mar2022_Public_all_shp-polygons.shp")
  pa2 <- spTransform(pa2,wgs.proj)
  # create list of layers for use later
pa_layers_sp_wgs <- list(pa0,pa1,pa2)
rm(pa0,pa1,pa2)
  #pa <- Reduce(rbind,c(pa0,pa1,pa2)) # takes too long, probably better to keep separate anyways for speed

## 2) read in using sf package (simple polygons; for polygon intersection analysis)
  # AEA projection
pa0 <- st_read("WDPA_WDOECM_Mar2022_Public_all_shp_0/WDPA_WDOECM_Mar2022_Public_all_shp-polygons.shp")
  # project to equal earth (meter measurements) for area calculations later
  pa0 <- pa0 %>% st_transform(crs = aea.proj)
pa1 <- st_read("WDPA_WDOECM_Mar2022_Public_all_shp_1/WDPA_WDOECM_Mar2022_Public_all_shp-polygons.shp")
  pa1 <- pa1 %>% st_transform(crs = aea.proj)
pa2 <- st_read("WDPA_WDOECM_Mar2022_Public_all_shp_2/WDPA_WDOECM_Mar2022_Public_all_shp-polygons.shp")
  pa2 <- pa2 %>% st_transform(crs = aea.proj)
# create list of layers for use later
pa_layers_sf_aea <- list(pa0,pa1,pa2)
rm(pa0,pa1,pa2)

# read in shapefile of U.S. EPA Level IV Ecoregions
  # spatial polygons
ecol4_sp <- readOGR("us_eco_l4_state_boundaries/us_eco_l4.shp")
ecol4_sp <- spTransform(ecol4_sp,aea.proj)
  # simple features
#ecol4_sf <- st_read("us_eco_l4_state_boundaries/us_eco_l4.shp")
#ecol4_sf <- ecol4_sf %>% st_transform(crs = aea.proj)

# read in shapefile of country boundaries
world_countries <- readOGR("UIA_World_Countries_Boundaries-shp/World_Countries__Generalized_.shp")
#world_countries.aea <- spTransform(world_countries,aea.proj)
# optional; if you are subsetting points to country(ies) of interest
target_iso <- c("US")
target_countries <- world_countries[world_countries@data$ISO %in% target_iso,]
# create polygon for clipping points later
target_countries.wgs <- spTransform(target_countries,wgs.proj)
boundary.wgs <- aggregate(target_countries.wgs,dissolve = TRUE)
# remove objects we don't use later
rm(target_iso,target_countries,target_countries.wgs,world_countries)

################################################################################
################################################################################
## Calculate protected area coverage
################################################################################
################################################################################

### START SUMMARY TABLE

# we add each target species as we go along
#summary_tbl <- data.frame(
#  species = "start",
#  EOO = "start",
#  dist_filter = "start",
#    #pt-in-pa
#  num_pt_unthinned = "start",
#  km_thin_value = "start",
#  num_pt_thinned = "start",
#  num_pt_in_pa = "start",
#  per_pt_in_pa = "start",
#    #buff-in-pa
#  area_buff = "start",
#  area_pa_in_buff = "start",
#  per_pa_in_buff = "start",
#  stringsAsFactors=F)

### CYCLE THROUGH TARGET SPECIES TO CALCULATE EX SITU COVERAGE

## can also test with one species first; comment out loop line
#sp <- 5

# old version, using just one core...
#for(sp in 1:length(target_sp)){

# start with subset of species that have smaller EOO
target_sp <- c("Pinus monticola","Taxus brevifolia","Pinus lambertiana")
target_sp <- gsub(" ","_",target_sp)
length(target_sp)

# new version, using parallel processing...
  # set up cluster
cl <- parallel::makeCluster(3, outfile="")
doParallel::registerDoParallel(cl)
  # set up printing of progress (copied from example, not sure exactly how
  #   it works)
registerDoSNOW(cl)
progress <- function(nfin, tag) {
  cat(sprintf('tasks completed: %d; tag: %d\n', nfin, tag))
}
opts <- list(progress=progress)
  # see time we are starting
Sys.time()
  # run loop using cluster
summary_list = foreach(sp = 1:length(target_sp), .packages = my.packages,
                       .options.snow = opts) %dopar% {

  # print progress
  cat("\n"); print(paste0("Starting ", target_sp[sp], " at ", Sys.time()))
  #print("Filtering occurrence points")

  ### READ IN AND PREP POINT DATA

  ## read in occurrence points (includes ex situ)
  insitu_raw <- read.csv(file.path("spp_edited_points",
                                   paste0(target_sp[sp],".csv")),
                         na.strings=c("","NA"), stringsAsFactors = F)
  nrow(insitu_raw)
  spp.rl.dist <- native_dist[which(native_dist$species_name_acc == gsub("_"," ",target_sp[sp])),]
  ## filter as desired
  insitu <- insitu_raw %>%
    filter(database == "Ex_situ" |
             (.cen & .inst & .con & .outl &
                ### for gap analysis of nine genera dataset
                .bonapnative & .yr1950 & .yrna &
                #.urb & .yr1950 & .yr1980 & .yrna &
                #(.gtsnative | is.na(.gtsnative)) &
                #(.rlnative  | is.na(.rlnative)) &
                #(.rlintroduced | is.na(.rlintroduced)) &
                basisOfRecord != "FOSSIL_SPECIMEN" & basisOfRecord != "LIVING_SPECIMEN" &
                ### for gap analysis of nine genera dataset
                basisOfRecord != "H?" &
                establishmentMeans != "INTRODUCED" & establishmentMeans != "MANAGED" &
                establishmentMeans != "INVASIVE"))
  if(!is.na(spp.rl.dist$rl_native_dist)){
    insitu <- insitu %>%
      filter(.rlnative | is.na(.rlnative))
    dist_filter_val <- "RL"
  } else if(!is.na(spp.rl.dist$gts_native_dist)){
    insitu <- insitu %>%
      filter(.gtsnative | is.na(.gtsnative))
    dist_filter_val <- "GTS"
  } else {
    dist_filter_val <- "N/A"
  }
  nrow(insitu)

  ## check document with manual point edits to see if anything needs to be added back or removed
  manual.edit <- pt_edits[which(pt_edits$species_name_acc == gsub("_"," ",target_sp[sp])),]
  # bounding box
  if(!is.na(manual.edit$bounding_box)){
    bounds <- unlist(strsplit(manual.edit$bounding_box,"; "))
    for(i in 1:length(bounds)){
      within <- unlist(strsplit(bounds[i],", "))
      insitu <- insitu %>% filter(!(decimalLongitude > as.numeric(within[1]) &
                                      decimalLongitude < as.numeric(within[3]) &
                                      decimalLatitude > as.numeric(within[2]) &
                                      decimalLatitude < as.numeric(within[4])))
    }
  }; nrow(insitu)
  # remove
  if(!is.na(manual.edit$remove)){
    remove <- unlist(strsplit(manual.edit$remove,"; "))
    insitu <- insitu %>% filter(!(UID %in% remove))
  }; nrow(insitu)
  ### for gap analysis of nine genera dataset; select US points only
  insitu <- clip.by.boundary(insitu,wgs.proj,boundary.wgs)
  # add back
  if(!is.na(manual.edit$keep)){
    keep <- unlist(strsplit(manual.edit$keep,"; "))
    add <- insitu_raw %>% filter(UID %in% keep)
    insitu <- suppressMessages(full_join(insitu,add))
  }; nrow(insitu)

  # print progress
  #print("Calculating EOO")

  ### CALCULATE EOO (convex hull)

  # using package ConR to calculate area of convex hull
  insitu_df <- insitu %>% select(decimalLatitude,decimalLongitude,species_name_acc)
  hull_area <- EOO.computing(XY=insitu_df,write_results=F)

  ### CALCULATE PROTECTED AREA COVERAGE

  ## POINT-IN-POLYGON METHOD

  # print progress
  #print("Starting point-in-PA analysis")

  # see function for methodology details
  pt_pa_stats <- pts.in.pa(insitu,"decimalLatitude","decimalLongitude",
                        "species_name_acc",2,wgs.proj,pa_layers_sp_wgs)

  ## BUFFER-IN-POLYGON METHOD

  # print progress
  #print(paste0("Starting buffer-PA-intersection analysis at ", Sys.time()))

  # see function for methodology details
  buff_pa_stats <- buffer.in.pa(insitu,50000,wgs.proj,aea.proj,
                               pa_layers_sf_aea,ecol4_sp)

  ### SUMMARY TABLE

  ## Add results to summary table
  summary_add <- data.frame(
    species = gsub("_"," ",target_sp[sp]),
    EOO = hull_area,
    dist_filter = dist_filter_val,
    stringsAsFactors=F)
  # add results from pt-in-pa analysis
  summary_add <- cbind(summary_add,pt_pa_stats)
  # add results from buffer-in-pa analysis
  summary_add <- cbind(summary_add,buff_pa_stats)

  # write file along the way in case everything doesn't run
  write.csv(summary_add, paste0("PA-coverage_",target_sp[sp],".csv"),
            row.names = F)

  # print progress
  cat("\n"); print(paste0("Ending ", target_sp[sp], " at ", Sys.time()))

  summary_add

  #summary_tbl[sp,] <- summary_add

}

Sys.time()

# create dataframe from list of outputs
summary_tbl <- do.call(rbind.data.frame, summary_list)
# write summary table
write.csv(summary_tbl, "PA-coverage_1-10_04-26-2022.csv", row.names = F)



################################################################################
################################################################################
## Map some examples of each method
################################################################################
################################################################################

# choose target species
target_sp
sp <- 1

cat("\n"); print(paste0("Starting ", target_sp[sp], " at ", Sys.time()))

### READ IN AND PREP POINT DATA

## read in occurrence points (includes ex situ)
insitu_raw <- read.csv(file.path("spp_edited_points",
                                 paste0(target_sp[sp],".csv")),
                       na.strings=c("","NA"), stringsAsFactors = F)
nrow(insitu_raw)
spp.rl.dist <- native_dist[which(native_dist$species_name_acc == gsub("_"," ",target_sp[sp])),]
## filter as desired
insitu <- insitu_raw %>%
  filter(database == "Ex_situ" |
           (.cen & .inst & .con & .outl &
              ### for gap analysis of nine genera dataset
                .bonapnative & .yr1950 & .yrna &
              basisOfRecord != "FOSSIL_SPECIMEN" & basisOfRecord != "LIVING_SPECIMEN" &
              ### for gap analysis of nine genera dataset
                basisOfRecord != "H?" &
                establishmentMeans != "INTRODUCED" & establishmentMeans != "MANAGED" &
                establishmentMeans != "INVASIVE"))
if(!is.na(spp.rl.dist$rl_native_dist)){
  insitu <- insitu %>%
    filter(.rlnative | is.na(.rlnative))
  dist_filter_val <- "RL"
} else if(!is.na(spp.rl.dist$gts_native_dist)){
  insitu <- insitu %>%
    filter(.gtsnative | is.na(.gtsnative))
  dist_filter_val <- "GTS"
} else {
  dist_filter_val <- "N/A"
}
nrow(insitu)

## check document with manual point edits to see if anything needs to be added back or removed
manual.edit <- pt_edits[which(pt_edits$species_name_acc == gsub("_"," ",target_sp[sp])),]
# bounding box
if(!is.na(manual.edit$bounding_box)){
  bounds <- unlist(strsplit(manual.edit$bounding_box,"; "))
  for(i in 1:length(bounds)){
    within <- unlist(strsplit(bounds[i],", "))
    insitu <- insitu %>% filter(!(decimalLongitude > as.numeric(within[1]) &
                                    decimalLongitude < as.numeric(within[3]) &
                                    decimalLatitude > as.numeric(within[2]) &
                                    decimalLatitude < as.numeric(within[4])))
  }
}; nrow(insitu)
# remove
if(!is.na(manual.edit$remove)){
  remove <- unlist(strsplit(manual.edit$remove,"; "))
  insitu <- insitu %>% filter(!(UID %in% remove))
}; nrow(insitu)
### for gap analysis of nine genera dataset; select US points only
insitu <- clip.by.boundary(insitu,wgs.proj,boundary.wgs)
# add back
if(!is.na(manual.edit$keep)){
  keep <- unlist(strsplit(manual.edit$keep,"; "))
  add <- insitu_raw %>% filter(UID %in% keep)
  insitu <- suppressMessages(full_join(insitu,add))
}; nrow(insitu)

### CALCULATE PROTECTED AREA COVERAGE

## BUFFER-IN-POLYGON METHOD

# calculate area of intersection of 50km buffers around occurrence points and
#   global protected areas (WDPA)
occ.pts <- insitu
radius <- 50000
pt_proj <- wgs.proj
buff_proj <- aea.proj
pa.list <- pa_layers_sf_aea
boundary <- ecol4_sp
  ## create 50km buffers around in situ points (aea proj) & calculate area
  ##   fast using sp method, slow using sf - not sure exactly why
  # sp method (fast):
  buff_50_sp <- create.buffers(occ.pts,radius,pt_proj,buff_proj,boundary)
  buff_50_sp <- raster::aggregate(buff_50_sp)
  # can just convert sp version to sf
  buff_50_sf <- st_as_sf(buff_50_sp)
  ## loop through PA layers (there are 3) and find intersection b/w
  ##  PAs and buffer around occurrence points
  pa_buff_intersect_lst <- list()
  for(i in 1:length(pa.list)){
    # select PA features that overlap with buffer layer
    pa_intersect_id <- st_intersects(buff_50_sf,pa.list[[i]]) %>% unlist()
    pa_intersect <- st_union(pa.list[[i]][pa_intersect_id,])
    # intersect the buffer and PA layers to get overlap
    buffer_in_pa <- st_union(st_intersection(buff_50_sf,pa_intersect))
    # add results to list
    pa_buff_intersect_lst[[i]] <- buffer_in_pa
  }
  ## combine all PA intersection layers & calc percent coverage
  pa_buff_intersect <- Reduce(st_union,pa_buff_intersect_lst)
  # visualize
  #plot(buff_50_sf); plot(pa_buff_intersect,add=T)
  # sp & WGS version for mapping
  pa_intersect_sp_wgs <- spTransform(as_Spatial(pa_buff_intersect),wgs.proj)
  buff_50_sp_wgs <- spTransform(buff_50_sp,wgs.proj)
library("leaflet")
coverage_map <- leaflet() %>%
	## background map
  addProviderTiles("Esri.WorldGrayCanvas",#"Esri.WorldShadedRelief",#"Esri.WorldTerrain",
		options = providerTileOptions(maxZoom = 10)) %>%
  ## ex situ buffers
  addPolygons(data = buff_50_sp_wgs,
    smoothFactor = 0.5,	weight = 1, opacity = 1, color = "#eee891",
    fillOpacity = 0.3) %>%
	## protected areas
	addPolygons(data = pa_intersect_sp_wgs,
	  smoothFactor = 0.5,	weight = 1, opacity = 1, color = "#005a0c",
	  fillOpacity = 0.3) %>%
	## in situ points, un-thinned
	#addCircleMarkers(data = insitu,
	#	lng = ~decimalLongitude, lat = ~decimalLatitude,
	#	radius = 4, fillOpacity = 1, stroke = F, color = "#9e0037") %>%
	## title
	addControl(paste0("Polygon intersection protected area analysis: ",
		gsub("_"," ",target_sp[sp])), position = "topright") %>%
	## legend
	addLegend(labels =
		c("Inferred native distribution (50km buffers around in situ occurrence points)",
		  "Protected areas (WDPA, 2022) within inferred native distribution"),
		colors = c("#eee891","#005a0c"), title = "Legend",
		position = "bottomright", opacity = 0.8) %>%
	## scale bar
	addScaleBar(position = "bottomright",
		options = scaleBarOptions(maxWidth = 150))
coverage_map

## POINT-IN-POLYGON METHOD

# calculate percent of occurrence points in global protected areas (WDPA)
#		first thin points, then see which are in PAs, then calculate percent
occ.pts <- insitu
lat_col <- "decimalLatitude"
long_col <- "decimalLongitude"
sp_col <- "species_name_acc"
thin.km <- 2
my.proj <- wgs.proj
pa.list <- pa_layers_sp_wgs
  # first thin points; thin.km gives the distance between thinned pts
  thin_pts <-
    spThin::thin(
      loc.data=occ.pts,
      lat.col=lat_col,
      long.col=long_col,
      spec.col=sp_col,
      thin.par=thin.km, #kilometers b/w pts
      reps=1, #right now we are just doing once, but should prob do more?
      locs.thinned.list.return=T,
      write.files=F,
      write.log.file=F)
  thinned_pts <- thin_pts[[1]]
  # make thinned points a spatial object
  thinned_spatial <- SpatialPointsDataFrame(thinned_pts,thinned_pts,proj4string=my.proj)
  # see which points are in protected areas polygons
  #		WDPA comes in three parts; I've left it like that to
  #		hopefully make this calculation a little faster
  pt_in_pa_list <- lapply(pa.list, function(x) thinned_spatial[x,])
  # create one object with all pts found in PAs
  pt_in_pa <- do.call(rbind, pt_in_pa_list)
# map results
library("leaflet")
coverage_map <- leaflet() %>%
	## background map
  addProviderTiles("Esri.WorldGrayCanvas",#"Esri.WorldShadedRelief",#"Esri.WorldTerrain",
		options = providerTileOptions(maxZoom = 10)) %>%
	## protected areas
	addPolygons(data = pa_intersect_sp_wgs,
	  smoothFactor = 0.5,	weight = 1, opacity = 1, color = "#005a0c",
	  fillOpacity = 0.3) %>%
	## in situ points, un-thinned
	addCircleMarkers(data = insitu,
		lng = ~decimalLongitude, lat = ~decimalLatitude,
		radius = 4, fillOpacity = 1, stroke = F, color = "#9e0037") %>%
	## in situ points, thinned
	addCircleMarkers(data = thinned_pts,
		lng = ~Longitude, lat = ~Latitude,
		radius = 4, fillOpacity = 1, stroke = F, color = "#ffa19d") %>%
  ## in situ points within PAs
  addCircleMarkers(data = pt_in_pa,
    lng = ~Longitude, lat = ~Latitude,
    radius = 4, fillOpacity = 1, stroke = F, color = "#6db9ff") %>%
	## title
	addControl(paste0("Point-in-polygon protected area analysis: ",
		gsub("_"," ",target_sp[sp])), position = "topright") %>%
	## legend
	addLegend(labels =
		c("Protected areas within 50km of occurrence points (WDPA, 2022)",
			"In situ occurrence points REMOVED during thinning",
			"In situ occurrence points WITHIN protected areas",
			"In situ occurrence points OUTSIDE protected areas"),
		colors = c("#005a0c","#9e0037","#6db9ff","#ffa19d"), title = "Legend",
		position = "bottomright", opacity = 0.8) %>%
	## scale bar
	addScaleBar(position = "bottomright",
		options = scaleBarOptions(maxWidth = 150))
coverage_map





################################################################################
################################################################################
### Calculations for gap analysis of US oaks 2020

################################################################################
# Choose target species
################################################################################

# create list of target species
target_sp <- c("Qacerifolia","Qajoensis","Qarkansana","Qaustrina","Qboyntonii",
               "Qcarmenensis","Qcedrosensis","Qchapmanii","Qcorneliusmulleri",
               "Qdumosa","Qengelmannii","Qgeorgiana","Qgraciliformis",
               "Qhavardii","Qhinckleyi","Qinopina","Qlaceyi","Qlobata",
               "Qoglethorpensis","Qpacifica","Qpalmeri","Qparvula","Qpumila",
               "Qrobusta","Qsadleriana","Qsimilis","Qtomentella","Qtoumeyi")

################################################################################
## Calculate geographic and ecological coverage of ex situ collections
################################################################################

### START SUMMARY TABLE

# we add each target species as we go along
summary_tbl <- data.frame(
  species = "start",
  EOO = "start",
  dist_filter = "start",
  #pt-in-pa
  num_pt_unthinned = "start",
  km_thin_value = "start",
  num_pt_thinned = "start",
  num_pt_in_pa = "start",
  per_pt_in_pa = "start",
  #buff-in-pa
  area_buff = "start",
  area_pa_in_buff = "start",
  per_pa_in_buff = "start",
  stringsAsFactors=F)

### CYCLE THROUGH TARGET SPECIES TO CALCULATE EX SITU COVERAGE

for(sp in 1:length(target_sp)){

  ## can also test with one species first
  sp <- 5

  # print progress
  cat("\n"); print(paste0("Starting ", target_sp[sp], " at ", Sys.time()))
  print("Filtering occurrence points")

  ### READ IN AND PREP POINT DATA

  # read in ex situ data (in case not in occurrence pts)
  exsitu_raw <- read.csv(file.path("quercus_2020_data",target_sp[sp],
                                   paste0(target_sp[sp],"_exsitu.csv")),
                         na.strings=c("","NA"), stringsAsFactors = F)
  nrow(exsitu_raw)
  exsitu <- exsitu_raw %>%
    filter(!is.na(latitude) & !is.na(longitude)) %>%
    rename(decimalLatitude = latitude,
           decimalLongitude = longitude)
  nrow(exsitu)
  # read in occurrence pts
  insitu_raw <- read.csv(file.path("quercus_2020_data",target_sp[sp],
                                   paste0(target_sp[sp],"_insitu.csv")),
                         na.strings=c("","NA"), stringsAsFactors = F)
  nrow(insitu_raw)
  insitu <- insitu_raw %>%
    filter(!is.na(latitude) & !is.na(longitude)) %>%
    rename(decimalLatitude = latitude,
           decimalLongitude = longitude)
  insitu <- full_join(insitu,exsitu)
  nrow(insitu)
  # clip to U.S. only (we don't have good data for MX for all target sp)
  insitu <- clip.by.boundary(insitu,wgs.proj,boundary.wgs)
  nrow(insitu)
  # make species name standard; needed for thin function later
  insitu$species_name_acc <- target_sp[sp]

  # print progress
  print("Calculating EOO")

  ### CALCULATE EOO (convex hull)

  # using package ConR to calculate area of convex hull
  insitu_df <- insitu %>% select(decimalLatitude,decimalLongitude,species_name_acc)
  hull_area <- EOO.computing(XY=insitu_df,write_results=F)

  ### CALCULATE PROTECTED AREA COVERAGE

  ## POINT-IN-POLYGON METHOD

  # print progress
  print("Starting point-in-PA analysis")

  ## plot all points, to see difference after thinning
  # select coordinate columns
  #latlong <- insitu %>% select(decimalLongitude,decimalLatitude)
  # turn occurrence point data into a SpatialPointsDataFrame
  #sp_df <- SpatialPointsDataFrame(latlong, insitu, proj4string = wgs.proj)
  #plot(sp_df)

  # see function for methodology details
  pt_pa_stats <- pts.in.pa(insitu,"decimalLatitude","decimalLongitude",
                           "species_name_acc",2,wgs.proj,pa_layers_sp_wgs)

  ## BUFFER-IN-POLYGON METHOD

  # print progress
  #print(paste0("Starting buffer-PA-intersection analysis at ", Sys.time()))

  # see function for methodology details
  #buff_pa_stats <- buffer.in.pa(insitu,50000,wgs.proj,aea.proj,
  #                             pa_layers_sf_aea,ecol4_sp)

  ### SUMMARY TABLE

  ## Add results to summary table
  summary_add <- data.frame(
    species = gsub("_"," ",target_sp[sp]),
    EOO = hull_area,
    dist_filter = dist_filter_val,
    stringsAsFactors=F)
  # add results from pt-in-pa analysis
  summary_add <- cbind(summary_add,pt_pa_stats)
  # add results from buffer-in-pa analysis
  #summary_add <- cbind(summary_add,buff_pa_stats)

  print(summary_add)
  #write.csv(summary_add, paste0("PA-coverage_",target_sp[sp],".csv"),
  #          row.names = F)

  summary_tbl[sp,] <- summary_add

}
