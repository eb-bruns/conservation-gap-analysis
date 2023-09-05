### 8-calc_map_exsitu_coverage.R
### Author: Emily Beckman Bruns & Kate Good
### Supporting institutions: The Morton Arboretum, Botanic Gardens Conservation 
#   International-US, United States Botanic Garden, San Diego Botanic Garden,
#   Missouri Botanical Garden
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
  ## This script creates circular buffers around in situ points (wild occurrence
  #   records) and ex situ points (wild locations where seeds were collected for
  #   cultivation in a botanic garden or storage in a genebank) to calculate the 
  #   geographic (area within buffers) and ecological (number of ecoregions 
  #   within buffers) representation of ex situ living collections.
  ## Optionally, an interactive map can be created to view ecoregions, buffers, 
  #   and locations where exsitu germplasm was collected.

### INPUTS:
  ## target taxa list 
  #   Can use target_taxa_with_synonyms.csv from 1-get_taxa_metadata, or can
  #   create own list of accepted taxon names
  ## ecoregions shapefile
  #   For calculations, there are three options. Script 1-prep_gis_layer.R 
  #   describes each and instructions for download; options include:
  #     - Global terrestrial ecoregions from The Nature Conservancy
  #     - Ecoregions of North America (Canada, US, Mexico), Level III, from EPA
  #     - Ecoregions of the Continental US, Level IV (finest level), from EPA
  #   Right now everything is set up to use the global ecoregions layer. To
  #   use a different layer for calculations, there are appropriate functions
  #   already created and noted in that section of the script. For mapping 
  #   (functions and palette prep), further edits would be needed to use one of 
  #   the other ecoregion layers.
  ## world_countries_10m.shp
  #   Shapefile created in 1-prep_gis_layers.R script. It's the Natural Earth 
  #   10m countries layer with the lakes cut out and some ISO_2A issues fixed.
  ## occurrence point data, including ex situ wild collection locations
  #   The script currently reads in occurrence point data from 
  #   7-filter_occurrence_points.R (taxon_points_final folder), for calculations
  #   and mapping; you could also use occurrence points in a different format,
  #   as long as the following fields are present: decimalLatitude,
  #   decimalLongitude, database (in situ points can have any value here, but 
  #   ex situ points need to have "Ex_situ" in the database column)

### OUTPUTS:
  ## Table of geographic and ecological coverage (%) based on three buffer sizes
  #   (user defined, but defaults are 20, 50, and 100 km), plus Extent of 
  #   Occurrence (EOO; convex hull around occurrence points) in km2
  ## (optional) Maps with occurrence points and ex situ wild collection 
  #   locations - plus buffer layers around each point layer - and ecoregions
  #   and, optionally, state borders. The map is in an interactive HTML format
  #   but has not been fully developed to be highly interactive but rather
  #   for a screenshot to be used in a static report.

################################################################################
# Load libraries
################################################################################

# load packages
my.packages <- c('tidyverse','textclean','terra','leaflet','rnaturalearth',
                 'Polychrome','sf')
  # versions I used (in the order listed above): 2.0.0, 0.9.3, 1.7-29, 2.1.2, 0.3.3, 1.5.1, 1.0-13
#install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

################################################################################
# Create functions
################################################################################

# format text in cell for output table (to be used in a static report)
format.cell <- function(ex_result,in_result,final_result){
  cell <- paste0(round(final_result,2),"%","\n",
                 "(",format(round(ex_result,0),format="d",big.mark=",")," / ",
                 format(round(in_result,0),format="d",big.mark=","),")")
  return(cell)
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

# create buffers around in situ and ex situ spatial points, calculate areas,
#		then compare to calculate percent coverage
compare.buff.area <- function(insitu,exsitu,radius,pt_proj,buff_proj,boundary){
  # create buffers
  buffer_insitu <- create.buffers(insitu,radius,pt_proj,buff_proj,boundary)
  buffer_exsitu <- create.buffers(exsitu,radius,pt_proj,buff_proj,boundary)
  # calculate buffer area
  print(paste("Based on ",radius/1000," km radius..."))
  area_exsitu <- expanse(buffer_exsitu)/1000000
  print(paste("Area covered by ex situ buffers:", round(area_exsitu,0),"km²"))
  area_insitu <- expanse(buffer_insitu)/1000000
  print(paste("Area covered by in situ buffers:", round(area_insitu,0),"km²"))
  # calculate difference between in situ and ex situ buffer areas (% coverage)
  area_diff_percent <- (area_exsitu/area_insitu)*100
  print(paste0("Percent geographic coverage: ", round(area_diff_percent,2), "%"))
  txt <- format.cell(area_exsitu,area_insitu,area_diff_percent)
  return(txt)
}

# create data frame with ecoregion data extracted for area covered by buffers
intersect.eco.buff <- function(df,radius,pt_proj,buff_proj,eco,boundary){
  # create buffers
  buffers <- create.buffers(df,radius,pt_proj,buff_proj,boundary)
  # make sure ecoregions are in same projection as buffers
  eco_proj <- project(eco,buff_proj)
  # intersect buffers with ecoregions
  buff_join_eco <- intersect(buffers,eco_proj)
  return(buff_join_eco)
}

# create data frame with ecoregion data extracted for area covered by buffers,
#		for both in situ and ex situ points, then compare count of ecoregions.
#   uses the *global terrestrial ecoregions TNC* layer
compare.ecoGlobal.count <- function(insitu,exsitu,radius,pt_proj,buff_proj,eco,boundary){
  # create data frame of ecoregion-buffer intersection
  eco_insitu <- intersect.eco.buff(insitu,radius,pt_proj,buff_proj,eco,boundary)
  eco_exsitu <- intersect.eco.buff(exsitu,radius,pt_proj,buff_proj,eco,boundary)
  # count number of ecoregions under buffers
  print(paste("Based on ",radius/1000," km radius..."))
  count_exsitu <- length(unique(eco_exsitu$ECO_ID_U))
  print(paste0("Number of ecoregions under ex situ buffers: ",count_exsitu))
  count_insitu <- length(unique(eco_insitu$ECO_ID_U))
  print(paste0("Number of ecoregions under in situ buffers: ",count_insitu))
  # calculate difference in number of ecoregions
  eco_diff_percent <- (count_exsitu/count_insitu)*100
  print(paste0("Percent ecological coverage: ", round(eco_diff_percent,2), "%"))
  txt <- format.cell(count_exsitu,count_insitu,eco_diff_percent)
  return(txt)
}

# create data frame with ecoregion data extracted for area covered by buffers,
#		for both in situ and ex situ points, then compare count of ecoregions.
#   uses the *North American Level III EPA* ecoregions layer
compare.ecoNorthAm.count <- function(insitu,exsitu,radius,pt_proj,buff_proj,eco,boundary){
  # create data frame of ecoregion-buffer intersection
  eco_insitu <- intersect.eco.buff(insitu,radius,pt_proj,buff_proj,eco,boundary)
  eco_exsitu <- intersect.eco.buff(exsitu,radius,pt_proj,buff_proj,eco,boundary)
  # count number of ecoregions under buffers
  print(paste("Based on ",radius/1000," km radius..."))
  count_exsitu <- length(unique(eco_exsitu$NA_L3CODE))
  print(paste0("Number of ecoregions under ex situ buffers: ",count_exsitu))
  count_insitu <- length(unique(eco_insitu$NA_L3CODE))
  print(paste0("Number of ecoregions under in situ buffers: ",count_insitu))
  # calculate difference in number of ecoregions
  eco_diff_percent <- (count_exsitu/count_insitu)*100
  print(paste0("Percent ecological coverage: ", round(eco_diff_percent,2), "%"))
  txt <- format.cell(count_exsitu,count_insitu,eco_diff_percent)
  return(txt)
}

# create data frame with ecoregion data extracted for area covered by buffers,
#		for both in situ and ex situ points, then compare count of ecoregions.
#   uses the *United States Level IV EPA* ecoregions layer
compare.ecoUS.count <- function(insitu,exsitu,radius,pt_proj,buff_proj,eco,boundary){
  # create data frame of ecoregion-buffer intersection
  eco_insitu <- intersect.eco.buff(insitu,radius,pt_proj,buff_proj,eco,boundary)
  eco_exsitu <- intersect.eco.buff(exsitu,radius,pt_proj,buff_proj,eco,boundary)
  # count number of ecoregions under buffers
  print(paste("Based on ",radius/1000," km radius..."))
  count_exsitu <- length(unique(eco_exsitu$US_L4CODE))
  print(paste0("Number of US ecoregions under ex situ buffers: ",count_exsitu))
  count_insitu <- length(unique(eco_insitu$US_L4CODE))
  print(paste0("Number of US ecoregions under in situ buffers: ",count_insitu))
  # calculate difference in number of ecoregions
  eco_diff_percent <- (count_exsitu/count_insitu)*100
  print(paste0("Percent US ecological coverage: ", round(eco_diff_percent,2), "%"))
  txt <- format.cell(count_exsitu,count_insitu,eco_diff_percent)
  return(txt)
}

# clip points by boundary so only in target area
# (helpful if focusing on one country/region)
clip.pt.by.boundary <- function(pts,pt_proj,boundary){
  # turn occurrence point data into a SpatVector
  spat_pts <- vect(pts, geom=c("decimalLongitude", "decimalLatitude"),
                   crs=pt_proj)
  # clip by boundary created earlier
  spat_pts <- crop(spat_pts,boundary)
  # keep just the data (not the spatial info you added)
  pts_new <- as.data.frame(spat_pts)
  return(pts_new)
}

# function to create ex situ coverage map, with ecoregions, buffers, and points
map.exsitu <- function(taxon,eco_now,states,in_buff,ex_buff,ex1,ex2,ex3,in_pts){
    # you can set the "maxZoom" to the level you'd like, or remove; used to 
    #   protect locations of rare wild plants
  map <- leaflet(options = leafletOptions(maxZoom = 9)) %>%
    ## background
    addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
    ## taxon name bolded at top right
    addControl(paste0("<b>",taxon), position = "topright") %>%
    ## global ecoregions
      # if you want to label the ecoregions, I would suggest creating a map
      #   with no points, buffers, or state borders (but with ecoregions labeled)
      #   and using that as a reference guide for the taxon maps; labeling 
      #   ecoregions on the taxon maps is really difficult in leaflet
      # if you want to use other ecoregion layer, you just need to change the 
      #   ECO_ID column to the equivalent ecoregion ID column in your layer
    addPolygons(
      data = eco_now, fillColor = ~eco_pal(eco_now$ECO_ID),
      fillOpacity = 0.8, color = "#757575", weight = 1.5, opacity = 0.8) %>%
    ## state boundaries
    addPolygons(
      data = states, fillColor = "transparent",
      weight = 1.5, opacity = 0.3, color = "black") %>%
    ## in situ buffers
    addPolygons(
      data = in_buff,
      fillColor = "#a3a3a3", fillOpacity = 0.45,
      weight = 1.3, opacity = 0.9, color = "#c4c4c4",
      smoothFactor = 0) %>%
    ## ex situ buffers
    addPolygons(
      data = ex_buff,
      fillColor = "white", fillOpacity = 0.52,
      weight = 1.3, color = "white", opacity = 0,
      smoothFactor = 0) %>%
    ## ex situ points; three different sizes based on number of individuals
    addMarkers(data = ex1,
               lng = ~decimalLongitude, lat = ~decimalLatitude, icon = triangle_sm,
               popup = ex1$inst_short) %>%
    addMarkers(data = ex2,
               lng = ~decimalLongitude, lat = ~decimalLatitude, icon = triangle_md,
               popup = ex2$inst_short) %>%
    addMarkers(data = ex3,
               lng = ~decimalLongitude, lat = ~decimalLatitude, icon = triangle_lg,
               popup = ex3$inst_short) %>%
    ## in situ points
      # can remove if you don't want these! remember to remove from legend too below
      addCircleMarkers(
        data = in_pts, lng = ~decimalLongitude, lat = ~decimalLatitude,
      	color = "#dedcd7", stroke = F,  fillOpacity = 1,
      	# you may want to change the radius
      	radius = 1.5) %>%
    ## add scale bar
    addScaleBar(position = "bottomright",
                options = scaleBarOptions(maxWidth = 150)) %>%
    ## add legend
    ##	not perfect, but something! Used https://imgbb.com to host the buffer
    ##	PNG images! So you could do that for any shape you'd like
      # in situ and ex situ buffers
      addControl(
        html = "<img src='https://i.ibb.co/1dW95pC/Insitu-buffer.png'
      		style='width:40px;height:40px;'> Taxon's estimated native distribution<br/>
      		(50 km buffer around in situ occurrence points)<br/>
      		<img src='https://i.ibb.co/SR71N6k/Exsitu-buffer.png'
      		style='width:40px;height:40px;'> Estimated capture of ex situ collections<br/>
      		(50 km buffer around wild provenance localities)",
        position = "bottomleft") %>%
      # ex situ triangles
      addControl(
        html = "Source locality and number of wild provenance<br/>individuals in ex situ collections<br/>
      		<img src='https://www.freeiconspng.com/uploads/triangle-png-28.png'
      		style='width:8px;height:8px;'> 1-9
      		<img src='https://www.freeiconspng.com/uploads/triangle-png-28.png'
      		style='width:15px;height:15px;'> 10-25
      		<img src='https://www.freeiconspng.com/uploads/triangle-png-28.png'
      		style='width:22px;height:22px;'> 30+",
        position = "bottomleft") %>%
      # in situ occurrence points
      addControl(
        html = "<img src='https://www.freeiconspng.com/uploads/grey-circle-icon-8.png'
        		style='width:9px;height:9px;'> In situ occurrence points",
        position = "bottomleft") %>%
    ## set view (long and lat) and zoom level, for when map initially opens
    setView(-96, 40, zoom = 5)
  
  return(map)
}

# function to create map for taxa with no ex situ points
map.no.exsitu <- function(taxon,eco_now,states,in_buff,in_pts){
  # you can set the "maxZoom" to the level you'd like, or remove; used to 
  #   protect locations of rare wild plants
  map <- leaflet(options = leafletOptions(maxZoom = 9)) %>%
    ## background
    addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
    ## taxon name bolded at top right
    addControl(paste0("<b>",taxon), position = "topright") %>%
    ## global ecoregions
    addPolygons(
      data = eco_now,
      fillColor = ~eco_pal(eco_now$ECO_ID),
      fillOpacity = 0.8, color = "#757575", weight = 1.5, opacity = 0.8) %>%
    ## state boundaries
    addPolygons(
      data = states,
      fillColor = "transparent",
      weight = 1.5, opacity = 0.3, color = "black") %>%
    ## in situ buffers
    addPolygons(
      data = in_buff,
      fillColor = "#a3a3a3", fillOpacity = 0.45,
      weight = 1.3, opacity = 0.9, color = "#c4c4c4",
      smoothFactor = 0) %>%
    ## in situ points
      # can remove if you don't want these!
      addCircleMarkers(data = in_pts,
                       lng = ~decimalLongitude, lat = ~decimalLatitude,
                       color = "#dedcd7", 
                       # you may want to change the radius
                       radius = 3, fillOpacity = 1, stroke = F) %>%
    ## add scale bar
    addScaleBar(position = "bottomright",
                options = scaleBarOptions(maxWidth = 150)) %>%
    ## add legend
    ##	not perfect, but something! Used https://imgbb.com to host the buffer
    ##	PNG images! So you could do that for any shape you'd like
      # in situ buffers
      addControl(
        html = "<img src='https://i.ibb.co/1dW95pC/Insitu-buffer.png'
      		style='width:40px;height:40px;'> Taxon's estimated native distribution<br/>
      		(50 km buffer around in situ occurrence points)",
        position = "bottomleft") %>%
      # in situ occurrence points
      addControl(
        html = "<img src='https://www.freeiconspng.com/uploads/grey-circle-icon-8.png'
          		style='width:9px;height:9px;'> In situ occurrence points",
        position = "bottomleft") %>%
    ## set view (long and lat) and zoom level, for when map initially opens
    setView(-96, 40, zoom = 4)
  
  return(map)
}

################################################################################
# Set working directory
################################################################################

# use 0-set_working_directory.R script:
# update to your path
source("/Users/emily/Documents/GitHub/conservation-gap-analysis/spatial-analysis-workflow/0-set_working_directory.R")

# assign folder where you have input occurrence point data
data_in <- "taxon_points_final"

################################################################################
# Set up standards to use throughout
################################################################################

# choose buffer sizes (in meters!) to use in calculations;
#   we use the medium size for mapping
# note you could also make this into a list and update the calc section further 
#   down (note added) so it loops through each element, making the number of 
#   different-sized buffers flexible
large_buff <- 100000  # 100 km
med_buff <- 50000     # 50 km
small_buff <- 20000   # 20 km

## DECIDE if you'd like to make maps, or run calculations only
#   no maps = FALSE
#   yes maps = TRUE
make_maps <- TRUE

if(make_maps){
  
  # CHOOSE cutoffs used for grouping ex situ data by number of individuals in maps;
  #   three categories will be created: 
  #   1) x < few_indiv   2) x >= few_indiv & x < many_indiv   3) x >= many_indiv
  few_indiv <- 10
  many_indiv <- 30
  
  # get icons used to mark number of ex situ individuals on maps 
  triangle_sm <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/triangle-png-28.png",
                          iconWidth = 8, iconHeight = 8)
  triangle_md <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/triangle-png-28.png",
                          iconWidth = 15, iconHeight = 15)
  triangle_lg <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/triangle-png-28.png",
                          iconWidth = 22, iconHeight = 22)
  
  # SELECT target countries (countries with taxa you're mapping); these are
  #   for getting state boundaries from rnaturalearth package
  target_countries <- c("United States of America","Mexico","Canada","Cuba",
                        "Dominican Republic","Puerto Rico")
  
  # create folder for output maps
  maps_out <- "exsitu_coverage_maps"
  if(!dir.exists(file.path(main_dir,analysis_dir,maps_out)))
    dir.create(file.path(main_dir,analysis_dir,maps_out), 
               recursive=T)
  
}

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

################################################################################
# Read in and prep polygon data
################################################################################

# depending on the scale/region of your analysis, choose the ecoregion layer
#   you'd like to use (download instructions in 1-prep_gis_layers.R)...
## Global terrestrial ecoregions from The Nature Conservancy
ecoregions <- vect(file.path(main_dir,gis_dir,"Terrestrial_Ecoregions",
                             "Terrestrial_Ecoregions.shp"))
## Ecoregions of North America (Canada, US, Mexico), Level III, from the EPA
#ecoregions <- vect(file.path(main_dir,gis_dir,"NA_CEC_Eco_Level3",
#                             "NA_CEC_Eco_Level3.shp"))
## Ecoregions of the Continental US, Level IV (finest level), from the EPA
#ecoregions <- vect(file.path(main_dir,gis_dir,"us_eco_l4_state_boundaries",
#                             "us_eco_l4.shp"))

# read in world countries layer created in 1-prep_gis_layers.R
# this will be used to clip buffers so they're not in the water
world_poly_clip <- vect(file.path(main_dir,gis_dir,"world_countries_10m",
                                  "world_countries_10m.shp"))

if(make_maps){
  
  # read in world countries layer from rnaturalearth package
  # this is for adding as a map layer
  world_poly_map <- ne_countries(scale = 50, type = "countries", 
                                 returnclass = "sf")
  
  # optionally, if you'd like to have state borders on your map:
  # read in state polygons for target countries using rnaturalearth package
  state_ls <- list()
  for(i in 1:length(target_countries)){
    state_ls[[i]] <- ne_states(country=target_countries[i],returnclass = "sf")
    print(target_countries[i])
  }
  # merge all state polygons
  state_boundaries <- Reduce(rbind,state_ls)
  
  # prep ecoregions for mapping
  # transform ecoregion polygons to point projection, for mapping
  eco_map <- project(ecoregions,pt.proj)
  # select realm of interest; keep only ecoregions in that realm; this helps
  # with the mapping palette and size of output map
  #   OPTIONS:
  #     "Australasia" "Antarctic"   "Afrotropic"  "Indo-Malay"  "Nearctic"   
  #     "Neotropic"   "Oceania"     "Palearctic" 
  eco_map <- eco_map[eco_map$WWF_REALM2 == "Nearctic" | 
                     eco_map$WWF_REALM2 == "Neotropic" ,]
  # the global ecoregions layer does not have major lakes cut out, so we'll do 
  #   that; takes a little while; you can skip if needed
  eco_map <- crop(eco_map,world_poly_clip)
  # if you're using more than one realm and/or have concerns about the maps
  #   being too large, crop ecoregions by state layer as well
  eco_map <- crop(eco_map,state_boundaries)
  # make sf object instead of terra, for input into leaflet
  eco_map <- st_as_sf(eco_map)
  
  # create ecoregion color palette, based on manually-selected 'seed colors'
    # P.S. apparently the seed colors don't make much of a difference:
    #   https://cran.r-project.org/web/packages/Polychrome/vignettes/creatingPalettes.html
    # Every time you run this section it creates a new palette; if you
    #   want the same color ecoregions for every taxon, run them all in one go;
    #   if you don't like the ecoregion colors, run this again or edit it
    ## NOTE that if you have a smaller set of ecoregions, you can also use an 
    #   online color-picker (there are lots) to simply choose a full of colors 
    #   you want instead of using the createPalette function
  eco_pal_colors <- createPalette(length(unique(eco_map$ECO_ID)),
                                  seedcolors = c("#ba3c3c","#ba7d3c","#baab3c",
                                                 "#3ca7ba","#3c6aba","#573cba",
                                                 "#943cba","#ba3ca1","#ba3c55"),
                                  range = c(5,45), target = "normal", M=50000)
  swatch(eco_pal_colors)
  eco_pal_colors <- as.vector(eco_pal_colors)
  eco_pal <- colorFactor(eco_pal_colors,eco_map$ECO_ID)
  
}

################################################################################
## Calculate & map geographic and ecological coverage of ex situ collections
################################################################################

# start summary table for analysis results
  # we add each target taxon as we go along
# note that the formatting of this table was created as an easy input for a
#   table in a final report; you may want to format differently (split out
#   cell contents that are concatenated) if using for different output
summary_tbl <- data.frame(
  taxon = "start",
  geo_sm = "start", geo_md = "start",	geo_lg = "start",
  eco_sm = "start", eco_md = "start", eco_lg = "start",
  EOO = "start",
  stringsAsFactors=F)

### CYCLE THROUGH TARGET TAXA TO CALCULATE EX SITU COVERAGE

for(i in 1:length(target_taxa)){
  
  ## can test with one taxon first if you'd like - skip loop line above and
  ##  uncomment next line
  # i <- 1
  
  # print progress
  cat("\nStarting", target_taxa[i], "\n")
  
  ### READ IN AND PREP POINT DATA
  
  ## read in occurrence points (includes ex situ)
  insitu_pt <- read.csv(file.path(main_dir,occ_dir,standardized_occ,data_in,
                                   paste0(target_files[i],".csv")), 
                         na.strings=c("","NA"), stringsAsFactors = F)
  nrow(insitu_pt)

  ### CALCULATE EOO (Extent of Occurrence on the IUCN Red List; convex hull)
  
  # make points into vector spatial object
  spat_pts <- vect(insitu_pt, geom=c("decimalLongitude", "decimalLatitude"),
                   crs=pt.proj, keepgeom=FALSE)
  spat_pts.calc <- project(spat_pts,calc.proj)
  # calculate area of convex hull in km2
  hull_insitu <- convHull(spat_pts.calc)
  hull_area <- expanse(hull_insitu)/1000000
  print(paste("EOO:",round(hull_area,0),"km²"))
  
  ### CREATE DATA SUBSET WITH EX SITU ONLY
  
  # create subset with just ex situ points
  exsitu_pt <- insitu_pt %>% filter(database == "Ex_situ")
  # print number of individuals ex situ with lat-long data
  print(paste("Number of ex situ individuals:",
              sum(as.numeric(exsitu_pt$individualCount))))
  
  # if no ex situ points, skip calculations and create map without ex situ elements
  if(nrow(exsitu_pt) == 0){
    print("No ex situ points; skipping buffer calculations")
    
    # add text results to summary table
    summary_add <- data.frame(
      taxon = target_taxa[i],
      geo_sm = "0%", geo_md = "0%",	geo_lg = "0%",
      eco_sm = "0%", eco_md = "0%", eco_lg = "0%",
      EOO = round(hull_area,0),
      stringsAsFactors=F)
    summary_tbl[i,] <- summary_add
    
    if(make_maps){
      # create buffers around in situ points, for mapping
      insitu_buff <- sf::st_as_sf(create.buffers(insitu_pt,med_buff,pt.proj,
                                                 pt.proj,world_poly_clip))
      # create map
      map <- map.no.exsitu(target_taxa[i],eco_map,state_boundaries,insitu_buff,
                           insitu_pt); map
      # save map
      htmlwidgets::saveWidget(map,file.path(main_dir,analysis_dir,maps_out,
                                            paste0(target_files[i],
                                                   "__exsitu_coverage_map",
                                                   ".html")))
    }
  } else {
    
    ### CALCULATE EX SITU COVERAGE
    
    # Note you could also make this section into a loop based on the number
    #   of different-sized buffers you'd like to test
    
    ## Geographic Coverage
    cat("GEOGRAPHIC COVERAGE\n")
    # calculate area based on large buffers
    geo_coverage_lg <- compare.buff.area(insitu_pt,exsitu_pt,large_buff,
                                         pt.proj,calc.proj,world_poly_clip)
    # calculate area based on medium buffers
    geo_coverage_md <- compare.buff.area(insitu_pt,exsitu_pt,med_buff,
                                         pt.proj,calc.proj,world_poly_clip)
    # calculate area based on small buffers
    geo_coverage_sm <- compare.buff.area(insitu_pt,exsitu_pt,small_buff,
                                         pt.proj,calc.proj,world_poly_clip)
    
    ## Ecological Coverage
    cat("ECOLOGICAL COVERAGE\n")
    ##  change the function based on the ecoregions you want to use:
    ##    compare.ecoGlobal.count
    ##    compare.ecoNorthAm.count
    ##    compare.ecoUS.count
    # count ecoregions under large buffers
    eco_coverage_lg <- compare.ecoGlobal.count(insitu_pt,exsitu_pt,large_buff,
                                               pt.proj,calc.proj,ecoregions,
                                               world_poly_clip)
    # count ecoregions under medium buffers
    eco_coverage_md <- compare.ecoGlobal.count(insitu_pt,exsitu_pt,med_buff,
                                               pt.proj,calc.proj,ecoregions,
                                               world_poly_clip)
    # count ecoregions under small buffers
    eco_coverage_sm <- compare.ecoGlobal.count(insitu_pt,exsitu_pt,small_buff,
                                               pt.proj,calc.proj,ecoregions,
                                               world_poly_clip)

    ## Summary Table
    # add text results to summary table
    summary_add <- data.frame(
      taxon = target_taxa[i],
      geo_sm = geo_coverage_sm,
      geo_md = geo_coverage_md,
      geo_lg = geo_coverage_lg,
      eco_sm = eco_coverage_sm,
      eco_md = eco_coverage_md,
      eco_lg = eco_coverage_lg,
      EOO = round(hull_area,0),
      stringsAsFactors=F)
    summary_tbl[i,] <- summary_add
    
    ### CREATE MAP
    
    if(make_maps){
      
      # prep buffer layers for mapping
      insitu_buff <- st_as_sf(create.buffers(insitu_pt,med_buff,
                                             pt.proj,pt.proj,world_poly_clip))
      exsitu_buff <- st_as_sf(create.buffers(exsitu_pt,med_buff,
                                             pt.proj,pt.proj,world_poly_clip))
      
      # split ex situ data by number of individuals, to use different symbols
      exsitu1 <- exsitu_pt %>% arrange(individualCount) %>%
        filter(individualCount < few_indiv)
      exsitu2 <- exsitu_pt %>% arrange(individualCount) %>%
        filter(individualCount >= few_indiv & individualCount < many_indiv)
      exsitu3 <- exsitu_pt %>% arrange(individualCount) %>%
        filter(individualCount >= many_indiv)
      
      # create buffers around in situ points, for mapping
      insitu_buff <- sf::st_as_sf(create.buffers(insitu_pt,med_buff,pt.proj,
                                                 pt.proj,world_poly_clip))
      # create map
      map <- map.exsitu(target_taxa[i],eco_map,state_boundaries,insitu_buff,
                        exsitu_buff,exsitu1,exsitu2,exsitu3,insitu_pt); map
      # save map
      htmlwidgets::saveWidget(map,file.path(main_dir,analysis_dir,maps_out,
                                            paste0(target_files[i],
                                                   "__exsitu_coverage_map",
                                                   ".html")))
    } 
  }
}
  
## write summary table
summary_tbl
write.csv(summary_tbl, file.path(main_dir,analysis_dir,
                                  paste0("exsitu_coverage_",Sys.Date(),".csv")),
          row.names = F)  
  