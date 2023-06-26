### 8-calc_map_protected_areas.R
### Author: Emily Beckman Bruns & Kate Good
### Supporting institutions: The Morton Arboretum, Botanic Gardens Conservation 
#   International-US, United States Botanic Garden, San Diego Botanic Garden,
#   Missouri Botanical Garden, USDA Forest Service
### Funding: 
#   -- USDA Forest Service (Cooperative Agreement 16-CA-11132546-045 with The
#        Morton Arboretum)
#   -- Institute of Museum and Library Services (IMLS MFA program grant
#        MA-30-18-0273-18 to The Morton Arboretum)
#   -- United States Botanic Garden (cooperative agreement with San Diego
#        Botanic Garden)
#   -- NSF (award 1759759 to The Morton Arboretum)
### Last Updated: June 2023
### R version 4.3.0

### DESCRIPTION:
  ## This script calculates protected area coverage for target taxa using the 
  #   point-in-polygon method ~ counting the number of occurrence points that
  #   fall within protected area features. There is also an option to create a 
  #   map of protected areas and points falling within and outside the areas.
  #   But, the output maps are very large for taxa with a wide distribution 
  #   and/or many protected areas in their range; it's not recommended to map
  #   these taxa and there is a "taxa_no_map" list where you can specify taxa
  #   you'd like to skip mapping.
  ## The larger the taxon's distribution and the more occurrence points and
  #   protected areas features within it's range, the longer the analysis and
  #   mapping will take. If you have very widespread target taxa (e.g. one 
  #   third of the US) this method may not work efficiently unless you get 
  #   multiple cores involved and/or lots of RAM.

### INPUTS:
  ## target taxa list 
  #   Can use target_taxa_with_synonyms.csv from 1-get_taxa_metadata, or can
  #   create own list of accepted taxon names
  ## occurrence point data, including ex situ wild collection locations
  #   The script currently reads in occurrence point data from 
  #   7-filter_occurrence_points.R (taxon_points_final folder), for calculations
  #   and mapping; you could also use occurrence points in a different format,
  #   as long as the following fields are present: taxon_name_accepted,
  #   decimalLatitude, and decimalLongitude
  # (if mapping) world_countries_10m.shp
  #   Shapefile created in 1-prep_gis_layers.R script. It's the Natural Earth 
  #   10m countries layer with the lakes cut out and some ISO_2A issues fixed.

### OUTPUTS:
  ## Table of protected area coverage, including...
  #   num_pt_unthinned: number of occurrence points before thinning
  #   km_thin_value: minimum distance between points, used for thinning
  #   num_pt_after_thin: number of points post-thinning
  #   num_pt_in_pa: number of points falling in a protected area
  #   percent_pt_in_pa: percent of points (post-thin) fallin in a protected area
  ## (optional) Maps with occurrence points and protected areas. The map is in 
  #   an interactive HTML format but has not been fully developed to be highly 
  #   interactive but rather for a screenshot to be used in a static report.

################################################################################
# Load libraries
################################################################################

# load packages
my.packages <- c('tidyverse','textclean','terra','sf','spThin','rnaturalearth',
                 'leaflet')
  # versions I used (in the order listed above): 2.0.0, 0.9.3, 1.7-29, 1.0-13, 0.2.0, 0.3.3, 2.1.2
#install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

################################################################################
# Create functions
################################################################################

# function to create ex situ coverage map, with ecoregions, buffers, and points
map.pa <- function(taxon,ctry_boundaries,protected_areas,pts,pts_in_pa){
  map <- leaflet() %>%
    ## background
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
    ## taxon name bolded at top right
    addControl(paste0("<b>",taxon), position = "topright") %>%
    ## protected areas
    addPolygons(
      data = protected_areas,                   
      smoothFactor = 0.5,	weight = 1, opacity = 1, color = "#4b965a",
      fillOpacity = 0.3) %>%
    ## country boundaries
    addPolygons(
      data = ctry_boundaries, fillColor = "transparent",
      weight = 1.5, opacity = 0.5, color = "#969696") %>%
    ## occurrence points
    addCircleMarkers(
      data = pts, lng = ~decimalLongitude, lat = ~decimalLatitude,
      color = "#d6569c", fillOpacity = 1, stroke = F,  
      # you may want to change the radius
      radius = 4) %>%
    ## occurrence points in protected areas
    addCircleMarkers(
      data = pts_in_pa,  lng = ~Longitude, lat = ~Latitude,
      color = "#231185", fillOpacity = 1, stroke = F,
      # you may want to change the radius
      radius = 4) %>%
    ## add scale bar
    addScaleBar(position = "bottomright",
                options = scaleBarOptions(maxWidth = 150)) %>%
    ## add legend
    addLegend(labels =
                c("Protected areas (WDPA, 2023)",
                  "Taxon occurrence <b>within</b> protected areas",
                  "Taxon occurrence <b>outside</b> protected areas"),
              colors = c("#4b965a","#231185","#d6569c"),
              position = "topright", opacity = 1) %>%
    ## set view (long and lat) and zoom level, for when map initially opens
    setView(-96, 40, zoom = 5)
}

# calculate percent of occurrence points in global protected areas (WDPA)
#		first thin points, then see which are in PAs, then calculate percent
pts.in.pa <- function(pts,lat_col,long_col,taxon_col,thin.km,my.proj,pa.list){
  # first thin points; thin.km gives the distance between thinned pts
  thin_pts <-
    spThin::thin(
      loc.data=pts,
      lat.col=lat_col,
      long.col=long_col,
      spec.col=taxon_col,
      thin.par=thin.km, #kilometers b/w pts
      reps=1, #right now we are just doing once, but should prob do more?
      locs.thinned.list.return=T,
      write.files=F,
      write.log.file=F)
  thinned_pts <- thin_pts[[1]]
  # count number of points after thinning
  num_thinned <- nrow(thinned_pts)
  # make thinned points a terra spatial object
  thinned_spatial <- vect(cbind(thinned_pts[,1], thinned_pts[,2]), crs = my.proj)
  # see which points are in protected areas polygons; we iterate through the 
  #   list of protected areas polygons
  pt_in_pa_list <- lapply(pa.list, function(x) extract(x,thinned_spatial))
  # create one object with all pts found in PAs
  pt_in_pa <- do.call(rbind, pt_in_pa_list)
  pt_in_pa <- pt_in_pa[!is.na(pt_in_pa$WDPAID),]
  # join to pt data to get the lat and long attached again
  thinned_pts$id.y <- 1:nrow(thinned_pts)
  pt_in_pa <- left_join(pt_in_pa,thinned_pts) 
  pt_in_pa <- pt_in_pa %>% distinct(id.y,.keep_all=T)
  # count number of points in PAs
  in_pa <- nrow(pt_in_pa)
  # create list of values to return; dataframe of stats & actual points in PAs
  pt_in_pa_stats <- data.frame(
    taxon = pts$taxon_name_accepted[1],
    num_pt_unthinned = nrow(pts),
    km_thin_value = thin.km,
    num_pt_after_thin = num_thinned,
    num_pt_in_pa = in_pa,
    percent_pt_in_pa = round(((in_pa/num_thinned)*100),digits=2))
  return_list <- list(pt_in_pa_stats,pt_in_pa)
  return(return_list)
}

# create buffers around points, using specified projection
create.buffers <- function(df,radius,pt_proj,buff_proj,boundary){
  # turn occurrence point data into a SpatVector
  spat_pts <- vect(df, geom=c("decimalLongitude", "decimalLatitude"),
                   crs=pt_proj)
  # reproject to specified projection
  proj_df <- project(spat_pts,buff_proj)
  # place buffer around each point, then dissolve into one polygon
  buffers <- buffer(proj_df,width=radius)
  buffers <- aggregate(buffers,dissolve = TRUE)
  # clip by boundary so they don't extend into the water
  boundary <- project(boundary,buff_proj)
  buffers_clip <- crop(buffers,boundary)
  # return buffer polygons
  return(buffers_clip)
}

################################################################################
# Set working directory
################################################################################

# use 0-set_working_directory.R script:
# update to your path
source("/Users/emily/Documents/GitHub/conservation-gap-analysis/spatial-analysis-workflow/0-set_working_directory.R")

# assign folder where you have input occurrence point data
data_in <- "taxon_points_final"

## DECIDE if you'd like to make maps, or run calculations only
#   no maps = FALSE
#   yes maps = TRUE
make_maps <- TRUE

if(make_maps){
  # create folder for output maps
  maps_out <- "protected_areas_maps"
  if(!dir.exists(file.path(main_dir,analysis_dir,data_out)))
    dir.create(file.path(main_dir,analysis_dir,data_out), 
               recursive=T)
}

# create folder for protected area layer downloads
pa_dir <- "protected_areas_downloads"
if(!dir.exists(file.path(main_dir,gis_dir,pa_dir)))
  dir.create(file.path(main_dir,gis_dir,pa_dir), 
             recursive=T)

################################################################################
# Download and read in protected areas layers
################################################################################

# you can download the global protected areas layer but it's very large
#   https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA
## we are going to download PA layers by country instead (which
##  are still very large), so we can just get them for countries that have
##  target taxa...

# CHOOSE target countries (countries with target taxa), using the 
#   3-digit ISO code
target_countries <- c("USA","MEX","CAN")

# SET current month, using 3-letter abbreviation
current_month <- "Jun"
# SET current year
current_year <- "2023"

# set timeout value for when download.file will stop; default is 60 seconds
#   and this is too short for some of the larger protected areas files; we will
#   change to 600, but you can increase further if needed
getOption('timeout') # check what you have right now
options(timeout=600) # set to new value

# download protected areas layers
pa_list <- list()
for(i in 1:length(target_countries)){
  
  # set up file paths
  shp_path <- paste0("https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_",
                     current_month,current_year,"_Public_",target_countries[i],
                     "_shp.zip")
  dest_dir <- file.path(main_dir,gis_dir,pa_dir)
  
  print(paste0("---Getting protected areas for ",target_countries[i],"---"))
  
  # download from web
  print("Downloading")
  if(grepl('http',shp_path)){
    dest <- paste0(dest_dir,"/",basename(shp_path))
    utils::download.file(shp_path, destfile = dest)
  }
  
  # unzip file downloaded
  print("Unzipping main download")
  if(grepl('.tgz$|.tar.gz$',dest)){
    utils::untar(dest, exdir = dest_dir)
  } else if(grepl('.zip$',dest)){
    utils::unzip(dest, exdir = dest_dir)
  } else{
    stop('Unsupported filetype')
  }
  
  # each protected areas download is split into three shapefiles;  
  #   we now unzip and read in each
  n <- 0
  shapes <- list()
  while(n < 3){
    path_now <- paste0(gsub(".zip","",dest),"_",n,".zip")
    dest_dir_now <- gsub(".zip","",path_now)
    dir.create(dest_dir_now)
    print(paste0("Unzipping part ",n))
    if(grepl('.tgz$|.tar.gz$',path_now)){
      utils::untar(path_now, exdir = dest_dir_now)
    } else if(grepl('.zip$',path_now)){
      utils::unzip(path_now, exdir = dest_dir_now)
    } else{
      stop('Unsupported filetype')
    }
    print(paste0("Reading in part ",n))
    shp_path <- paste0(gsub(".zip","",dest_dir_now),"/",
                       gsub(".zip","",basename(dest)),"-polygons.shp")
    temp_shape <- vect(shp_path)
    shapes[[n+1]] <- temp_shape
    n <- n + 1
  }
  
  # combine all shapefiles
  print("Combining into one shapefile")
  one_file <- Reduce(rbind,shapes)
  
  # add as item to list of country-level PAs
  pa_list[[i]] <- one_file
  
}

if(make_maps){
  # merge all PAs
  # we only need this for the map; for calculations we iterate instead
  # these functions can take a while...
  all_pa <- Reduce(rbind,pa_list)
}
rm(shapes,one_file,temp_shape,i,n)

# if you'd like, remove local downloads
unlink(file.path(main_dir,gis_dir,pa_dir), recursive = TRUE)

################################################################################
# Set up standards and read in additional polygon data if mapping
################################################################################

# define projections
#	points will be WGS84
pt.proj <- "+proj=longlat +datum=WGS84"
# for calculations, we need something with units in meters and equal area
calc.proj <- "+proj=eqearth +datum=WGS84"

# read in target taxa list
# you can also simply create the "target_taxa" and "target_files" lists
#   manually, if desired
taxon_list <- read.csv(file.path(main_dir,taxa_dir,"target_taxa_with_synonyms.csv"), 
                       header=T, colClasses="character",na.strings=c("","NA"))
# list of accepted taxa to cycle through
target_taxa <- unique(taxon_list$taxon_name_accepted)
# make taxon list with underscores added where spaces, to format for reading/
#   writing when we cycle through in our loop below
target_files <- unique(mgsub(taxon_list$taxon_name_accepted, 
                             c(" ","var.","subsp."), c("_","var","subsp")))

if(make_maps){
  
  # if you have very widespread taxa (e.g. span one third or more of the US)
  #   make a list of those widespread taxa to skip mapping (output too large)
  taxa_no_map <- c("Asimina parviflora","Asimina triloba")
  
  # read in world countries layer created in 1-prep_gis_layers.R
  # this will be used to clip buffers so they're not in the water, and
  #   also for adding to the map as a layer
  world_poly_clip <- vect(file.path(main_dir,gis_dir,"world_countries_10m",
                                    "world_countries_10m.shp"))
  world_poly_sf <- st_as_sf(world_poly_clip)

}

################################################################################
## Calculate & map protected areas coverage
################################################################################
  
# start summary table for analysis results
# we add each target taxon as we go along
summary_tbl <- data.frame(
  taxon = "start",
  num_pt_unthinned = "start",
  km_thin_value = "start",
  num_pt_after_thin = "start",
  num_pt_in_pa = "start",
  percent_pt_in_pa = "start",
  stringsAsFactors=F)

### CYCLE THROUGH TARGET TAXA TO CALCULATE PROTECTED AREAS COVERAGE

for(i in 1:length(target_taxa)){
  
  ## can test with one taxon first if you'd like - skip loop line above and
  ##  uncomment next line
  # i <- 1
  
  # print progress
  cat("\nStarting", target_taxa[i], "\n")
  
  ### READ IN AND PREP POINT DATA
  
  ## read in occurrence points (includes ex situ)
  occ_pts <- read.csv(file.path(main_dir,occ_dir,standardized_occ,data_in,
                                paste0(target_files[i],".csv")), 
                      na.strings=c("","NA"), stringsAsFactors = F)
  nrow(occ_pts)
  
  ### CALC PROTECTED AREAS COVERAGE
  
  pa_analysis_output <- pts.in.pa(occ_pts, "decimalLatitude", "decimalLongitude",
                                  "taxon_name_accepted", 1, pt.proj, pa_list)
  # add row to summary table
  summary_tbl[i,] <- pa_analysis_output[[1]]
  # points in protected areas df
  pts_in_pa_now <- pa_analysis_output[[2]]

  ### CREATE MAP
  
  if(make_maps & !(target_taxa[i] %in% taxa_no_map)){
    
    # add buffer around occurrence points and crop PAs to that buffer
    occ_buff <- create.buffers(occ_pts,100000,pt.proj,pt.proj,world_poly_clip)
    pa_crop <- mask(all_pa,occ_buff)
    # aggregate PAs so there are not multiple on top of each other
    pa_crop <- aggregate(pa_crop,by="ISO3")
    # make PAs into sf instead of terra
    pa_crop <- st_as_sf(pa_crop)
      
    # create map
    map <- map.pa(target_taxa[i],world_poly_sf,pa_crop,occ_pts,
                  pts_in_pa_now); map
      
    # save map
    htmlwidgets::saveWidget(map,file.path(main_dir,analysis_dir,maps_out,
                                          paste0(target_files[i],
                                                 "__protected_area_coverage_map",
                                                 ".html")))
  }
}

## write summary table
summary_tbl
write.csv(summary_tbl, file.path(main_dir,analysis_dir,
                                 paste0("protected_area_coverage_",Sys.Date(),".csv")),
          row.names = F)
  