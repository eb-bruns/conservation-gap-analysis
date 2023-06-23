### 6-visualize_occurrence_points.R
### Author: Emily Beckman Bruns
### Supporting institutions: The Morton Arboretum, Botanic Gardens Conservation 
#   International-US, United States Botanic Garden, San Diego Botanic Garden,
#   Missouri Botanical Garden
### Funding: 
#   -- Institute of Museum and Library Services (IMLS MFA program grant
#        MA-30-18-0273-18 to The Morton Arboretum)
#   -- United States Botanic Garden (cooperative agreement with San Diego
#        Botanic Garden)
#   -- NSF (award 1759759 to The Morton Arboretum)
### Last Updated: June 2023 ; first written March 2020
### R version 4.3.0

### DESCRIPTION:
  ## This script creates an interactive (HTML) occurrence point map for each 
  #   target taxon, for exploring and (optionally) retrieving the UIDs of points 
  #   to be removed before final anlayses. Includes checkbox toggles that show 
  #   points flagged in 5-flag_occurrence_points.R, plus some additional flags.
  ## Note that you can add additional flagging checkboxes to the map by adding
  #   additional 'addCircleMarkers' sections; for example, copy line 316-336 
  #   and edit the 'filter' arguments (second line) and the 'group' text 
  #   (last line) then go to 'overlayGroups' (line 424) and add an entry for 
  #   the group you just created. You can of course remove checkboxes by doing
  #   the opposite (remove the addCircleMarkers section and the corresponding  
  #   overlayGroups entry).

### INPUTS:
  ## target_taxa_with_synonyms.csv
  #   List of target taxa and synonyms; see example in the "Target taxa list"
  #   tab in Gap-analysis-workflow_metadata workbook; Required columns include: 
  #   taxon_name, taxon_name_accepted, and taxon_name_status (Accepted/Synonym).
  ## taxon_points_ready-to-vet (folder) 
  #   Occurrence data output from 5-flag_occurrence_points.R
  ## world_countries_10m.shp
  #   Shapefile created in 1-prep_gis_layers.R script. It's the Natural Earth 
  #   10m countries layer with the lakes cut out and some ISO_2A issues fixed.

### OUTPUTS:
  ## visualize_taxon_points (folder)
  #   For each taxon in your target taxa list, an interactive HTML map is
  #   created, which can be downloaded and opened in your browser for exploring 
  #   (e.g., Asimina_triloba__map-for-vetting.html)
  ## manual_point_edits.csv
  #   File that can be filled in manually while reviewing the maps - optional; 
  #   see instructions in the "Manual point edits" tab in 
  #   Gap-analysis-workflow_metadata workbook

################################################################################
# Load libraries
################################################################################

# install rnaturalearthdata package if you don't have it yet
install.packages('rnaturalearthdata') # my version is 0.1.0

# load packages
my.packages <- c('tidyverse','textclean','rnaturalearth','leaflet')
  # versions I used (in the order listed above): 2.0.0, 0.9.3, 1.7-29, 0.3.3, 2.1.2
#install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

################################################################################
# Set working directory
################################################################################

# use 0-set_working_directory.R script:
  # update to your path
source("/Users/emily/Documents/GitHub/conservation-gap-analysis/spatial-analysis-workflow/0-set_working_directory.R")

# create folder for output data
data_out <- "visualize_taxon_points"
if(!dir.exists(file.path(main_dir,occ_dir,standardized_occ,data_out)))
  dir.create(file.path(main_dir,occ_dir,standardized_occ,data_out), 
             recursive=T)

# assign folder where you have input data (saved in 5-flag_occurrence_points.R)
data_in <- "taxon_points_ready-to-vet"

################################################################################
# Use leaflet package to create interactive maps to explore (html)
################################################################################

# read in taxa list
taxon_list <- read.csv(file.path(main_dir,taxa_dir,"target_taxa_with_synonyms.csv"), 
                       header=T, colClasses="character",na.strings=c("","NA"))

# select accepted taxa
target_taxa <- taxon_list %>% filter(taxon_name_status == "Accepted")
nrow(target_taxa)

# save file that can be (optionally) used for manually flagging points for removal
edits <- data.frame(taxon_name_accepted = target_taxa$taxon_name_accepted,
                    remove_id	= "", remove_bounding_box = "", keep_id	= "", 
                    reviewer_notes	= "", reviewer_name = "")
if(file.exists(file.path(main_dir,occ_dir,standardized_occ,"manual_point_edits.csv"))){
  "You've already created this file; if you'd like to overwrite it, run the 'else' statement manually"
} else {
  write.csv(edits, file.path(main_dir,occ_dir,standardized_occ,
                             "manual_point_edits.csv"), row.names = F)
}

# make taxon list with underscores added where spaces, to format for reading/
#   writing when we cycle through in our loop below
taxa_cycle <- unique(mgsub(target_taxa$taxon_name_accepted, 
                           c(" ","var.","subsp."), c("_","var","subsp")))
taxa_cycle

# list of native countries for each target taxon
countries <- target_taxa$all_native_dist_iso2

# read in world countries layer from rnaturalearth package
world_polygons <- ne_countries(scale = 50, type = "countries", 
                                returnclass = "sf")

### cycle through each species file and create map
for(i in 1:length(taxa_cycle)){

  ## read in occurrence records (output from 5-flag_occurrence_points.R)
  taxon_now <- read.csv(file.path(main_dir,occ_dir,standardized_occ,data_in,
                                  paste0(taxa_cycle[i],".csv")))

  ## create a color palette for the map's points, based on source database
    # set database as factor and order as you'd like for viewing overlapping
    #   points; earlier databases will be shown on top of latter databases
  database_order <- c("Ex_situ","NorthAm_herbaria","iDigBio","GBIF","FIA",
                      "IUCN_RedList","BIEN")
  taxon_now$database <- factor(taxon_now$database, levels = database_order)
  taxon_now <- taxon_now %>% arrange(desc(database))
    # create color palette (one color for each database)
    # there are plenty of palette creation tools that can help you create 
    #   a palette that is accessible (friendly to those with color vision 
    #   deficiency); some examples:
    #     http://medialab.github.io/iwanthue/
    #     https://venngage.com/tools/accessible-color-palette-generator#colorGenerator
    #     https://toolness.github.io/accessible-color-matrix/
    #   or search "color picker" in Google and choose colors manually by copying
    #   the HEX number
  colors <- c("#adbb3f","#819756","#5fbb9a","#6a9ebd","#7b83cc","#7264de","#3c2c7a")
  database.pal <- colorFactor(palette=colors, levels = database_order)

  ## select native countries (found in 1-get_taxa_metadata.R), for outlining
  ##   on the map
  native_ctrys <- taxon_list %>% 
    filter(taxon_name_accepted == target_taxa$taxon_name_accepted[i])
  target_iso <- unlist(strsplit(native_ctrys$all_native_dist_iso2, split="; "))
  target_countries <- world_polygons[world_polygons$iso_a2 %in% target_iso,]
  
  ## create interactive map
  try(final_map <- leaflet() %>%
    ## Base layers
    addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Esri.WorldGrayCanvas") %>%
    addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
    addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri.WorldTopoMap") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB.Positron") %>%
    addControl(paste0("<b>",taxa_cycle[i]), position = "topright") %>%
    ## Notes text boxes
    addControl(
      "Toggle the checkboxes below on and off to view flagged<br/>  
      points (colored red). If no points turn red when the box<br/>
      is checked, there are no points flagged in that category.<br/>
      Click each point for details about the record. The taxon's<br/>
      native countries of occurrence are outlined grey.",
       position = "topright") %>%
    addControl(
      'See more information about the
      <a href="https://github.com/eb-bruns/conservation-gap-analysis/blob/main/spatial-analysis-workflow/3-get_occurrence_data.R">data sources</a> and 
      <a href="https://github.com/eb-bruns/conservation-gap-analysis/blob/main/spatial-analysis-workflow/5-flag_occurrence_points.R">flagging methodology</a>.',
        position = "bottomright") %>%      
    addControl(
      "Note that 'Ex_situ' does <b>not</b> indicate the locations of botanic gardens;</br> 
      it marks where ex situ germplasm was originally collected in the wild!",
        position = "bottomright") %>%
	  ## Native country outlines
	  addPolygons(data = target_countries, fillColor = "transparent",
		  weight = 2, opacity = 0.8, color = "#7a7a7a") %>%
    ## Occurrence points, colored by database
    addCircleMarkers(data = taxon_now, ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",taxon_name_accepted,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with similar record:</b> ",all_source_databases,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Reported country:</b> ",countryCode_standard,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Record year:</b> ",year,"<br/>",
        "<b>Latitude:</b> ",decimalLatitude,"<br/>",
        "<b>Longitude:</b> ",decimalLongitude,"<br/>",
        "<b>Coordinate uncertainty in meters:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>Native database ID:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>References:</b> ",references,"<br/>",
        "<b>UID:</b> ",UID),
      color = ~database.pal(database),radius = 4,
      fillOpacity = 0.9, stroke = T) %>%
    ## Occurrence points flagged in each group (can toggle)
    # .cen (points near state/country centroids)
    addCircleMarkers(data = taxon_now %>% filter(!.cen & database!="Ex_situ"),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",taxon_name_accepted,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with similar record:</b> ",all_source_databases,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Reported country:</b> ",countryCode_standard,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Record year:</b> ",year,"<br/>",
        "<b>Latitude:</b> ",decimalLatitude,"<br/>",
        "<b>Longitude:</b> ",decimalLongitude,"<br/>",
        "<b>Coordinate uncertainty in meters:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>Native database ID:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>References:</b> ",references,"<br/>",
        "<b>UID:</b> ",UID),
      radius=4,stroke=T,color="black",weight=1,fillColor="red",fillOpacity=0.8,
      group = "Within 500m of country/state centroid (.cen)") %>%
    # .urb (points in urban areas)
    addCircleMarkers(data = taxon_now %>% filter(!.urb & database!="Ex_situ"),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",taxon_name_accepted,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with similar record:</b> ",all_source_databases,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Reported country:</b> ",countryCode_standard,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Record year:</b> ",year,"<br/>",
        "<b>Latitude:</b> ",decimalLatitude,"<br/>",
        "<b>Longitude:</b> ",decimalLongitude,"<br/>",
        "<b>Coordinate uncertainty in meters:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>Native database ID:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>References:</b> ",references,"<br/>",
        "<b>UID:</b> ",UID),
      radius=4,stroke=T,color="black",weight=1,fillColor="red",fillOpacity=0.8,
      group = "In urban area (.urb)") %>%
    # .inst (points near biodiversity institutions)
    addCircleMarkers(data = taxon_now %>% filter(!.inst & database!="Ex_situ"),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",taxon_name_accepted,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with similar record:</b> ",all_source_databases,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Reported country:</b> ",countryCode_standard,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Record year:</b> ",year,"<br/>",
        "<b>Latitude:</b> ",decimalLatitude,"<br/>",
        "<b>Longitude:</b> ",decimalLongitude,"<br/>",
        "<b>Coordinate uncertainty in meters:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>Native database ID:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>References:</b> ",references,"<br/>",
        "<b>UID:</b> ",UID),
      radius=4,stroke=T,color="black",weight=1,fillColor="red",fillOpacity=0.8,
      group = "Within 100m of biodiversity institution (.inst)") %>%
    # .con (points in a different country than listed in the record)
    addCircleMarkers(data = taxon_now %>% filter(!.con & database!="Ex_situ"),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",taxon_name_accepted,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with similar record:</b> ",all_source_databases,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Reported country:</b> ",countryCode_standard,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Record year:</b> ",year,"<br/>",
        "<b>Latitude:</b> ",decimalLatitude,"<br/>",
        "<b>Longitude:</b> ",decimalLongitude,"<br/>",
        "<b>Coordinate uncertainty in meters:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>Native database ID:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>References:</b> ",references,"<br/>",
        "<b>UID:</b> ",UID),
      radius=4,stroke=T,color="black",weight=1,fillColor="red",fillOpacity=0.8,
      group = "Not in reported country (.con)") %>%
    # .outl (points that are outliers based on algorithm selected in script #5)
    addCircleMarkers(data = taxon_now %>% filter(!.outl & database!="Ex_situ"),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",taxon_name_accepted,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with similar record:</b> ",all_source_databases,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Reported country:</b> ",countryCode_standard,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Record year:</b> ",year,"<br/>",
        "<b>Latitude:</b> ",decimalLatitude,"<br/>",
        "<b>Longitude:</b> ",decimalLongitude,"<br/>",
        "<b>Coordinate uncertainty in meters:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>Native database ID:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>References:</b> ",references,"<br/>",
        "<b>UID:</b> ",UID),
      radius=4,stroke=T,color="black",weight=1,fillColor="red",fillOpacity=0.8,
      group = "Geographic outlier (.outl)") %>%
    # .nativectry (points that are not within the taxon's native countries)
    addCircleMarkers(data = taxon_now %>% filter(!.nativectry & database!="Ex_situ"),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",taxon_name_accepted,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with similar record:</b> ",all_source_databases,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Reported country:</b> ",countryCode_standard,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Record year:</b> ",year,"<br/>",
        "<b>Latitude:</b> ",decimalLatitude,"<br/>",
        "<b>Longitude:</b> ",decimalLongitude,"<br/>",
        "<b>Coordinate uncertainty in meters:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>Native database ID:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>References:</b> ",references,"<br/>",
        "<b>UID:</b> ",UID),
      radius=4,stroke=T,color="black",weight=1,fillColor="red",fillOpacity=0.8,
      group = "Outside native country (.nativectry)") %>%
    # basis of record (marked as fossil or living specimen)
    addCircleMarkers(data = taxon_now %>%
      filter(basisOfRecord == "FOSSIL_SPECIMEN" | basisOfRecord == "LIVING_SPECIMEN"),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",taxon_name_accepted,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with similar record:</b> ",all_source_databases,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Reported country:</b> ",countryCode_standard,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Record year:</b> ",year,"<br/>",
        "<b>Latitude:</b> ",decimalLatitude,"<br/>",
        "<b>Longitude:</b> ",decimalLongitude,"<br/>",
        "<b>Coordinate uncertainty in meters:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>Native database ID:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>References:</b> ",references,"<br/>",
        "<b>UID:</b> ",UID),
      radius=4,stroke=T,color="black",weight=1,fillColor="red",fillOpacity=0.8,
      group = "Fossil or living specimen (basisOfRecord)") %>%
    # establishment means (marked as introduced, managed, invasive, or cultivated)
    addCircleMarkers(data = taxon_now %>%
      filter(establishmentMeans == "INTRODUCED" | establishmentMeans == "MANAGED" |
               establishmentMeans == "INVASIVE" | establishmentMeans == "CULTIVATED"),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",taxon_name_accepted,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with similar record:</b> ",all_source_databases,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Reported country:</b> ",countryCode_standard,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Record year:</b> ",year,"<br/>",
        "<b>Latitude:</b> ",decimalLatitude,"<br/>",
        "<b>Longitude:</b> ",decimalLongitude,"<br/>",
        "<b>Coordinate uncertainty in meters:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>Native database ID:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>References:</b> ",references,"<br/>",
        "<b>UID:</b> ",UID),
      radius=4,stroke=T,color="black",weight=1,fillColor="red",fillOpacity=0.8,
      group = "Cultivated, introduced, or managed (establishmentMeans)") %>%
    # .yr1950 (recorded prior to 1950)
    addCircleMarkers(data = taxon_now %>% filter(!.yr1950 & database!="Ex_situ"),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",taxon_name_accepted,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with similar record:</b> ",all_source_databases,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Reported country:</b> ",countryCode_standard,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Record year:</b> ",year,"<br/>",
        "<b>Latitude:</b> ",decimalLatitude,"<br/>",
        "<b>Longitude:</b> ",decimalLongitude,"<br/>",
        "<b>Coordinate uncertainty in meters:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>Native database ID:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>References:</b> ",references,"<br/>",
        "<b>UID:</b> ",UID),
      radius=4,stroke=T,color="black",weight=1,fillColor="red",fillOpacity=0.8,
      group = "Recorded prior to 1950 (.yr1950)") %>%
    # .yr1980 (recorded prior to 1980)
    addCircleMarkers(data = taxon_now %>% filter(!.yr1980 & database!="Ex_situ"),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",taxon_name_accepted,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with similar record:</b> ",all_source_databases,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Reported country:</b> ",countryCode_standard,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Record year:</b> ",year,"<br/>",
        "<b>Latitude:</b> ",decimalLatitude,"<br/>",
        "<b>Longitude:</b> ",decimalLongitude,"<br/>",
        "<b>Coordinate uncertainty in meters:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>Native database ID:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>References:</b> ",references,"<br/>",
        "<b>UID:</b> ",UID),
      radius=4,stroke=T,color="black",weight=1,fillColor="red",fillOpacity=0.8,
      group = "Recorded prior to 1980 (.yr1980)") %>%
    # .yrna (no record year provided)
    addCircleMarkers(data = taxon_now %>% filter(!.yrna & database!="Ex_situ"),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",taxon_name_accepted,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with similar record:</b> ",all_source_databases,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Reported country:</b> ",countryCode_standard,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Record year:</b> ",year,"<br/>",
        "<b>Latitude:</b> ",decimalLatitude,"<br/>",
        "<b>Longitude:</b> ",decimalLongitude,"<br/>",
        "<b>Coordinate uncertainty in meters:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>Native database ID:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>References:</b> ",references,"<br/>",
        "<b>UID:</b> ",UID),
      radius=4,stroke=T,color="black",weight=1,fillColor="red",fillOpacity=0.8,
      group = "Record year unknown (.yrna)") %>%
    ## Layers control (check boxes you can toggle on and off; 'overlayGroup' names
    ##    are associated with the 'group' names assigned in each section above)
    addLayersControl(
      baseGroups = c("Esri.WorldGrayCanvas","OpenStreetMap",
                     "Esri.WorldTopoMap","CartoDB.Positron"),
      overlayGroups = c("Within 500m of country/state centroid (.cen)",
                        "In urban area (.urb)",
                        "Within 100m of biodiversity institution (.inst)",
                        "Not in reported country (.con)",
                        "Geographic outlier (.outl)",
                        "Outside native country (.nativectry)",
                        "Fossil or living specimen (basisOfRecord)",
                        "Cultivated, introduced, or managed (establishmentMeans)",
                        "Recorded prior to 1950 (.yr1950)",
                        "Recorded prior to 1980 (.yr1980)",
                        "Record year unknown (.yrna)"),
      options = layersControlOptions(collapsed = FALSE)) %>%
      # you can un-check groups in the control panel with a 'hideGroup';
      # it's nice to hide the filters you're not interested in
    hideGroup("In urban area (.urb)") %>%
    hideGroup("Recorded prior to 1980 (.yr1980)") %>%
    hideGroup("Record year unknown (.yrna)") %>%
    ## Legend
    addLegend(pal = database.pal, values = unique(taxon_now$database),
              title = "Occurrence point</br>source database", 
              position = "bottomright", opacity = 0.8)
  )
  final_map

  ## save map
  try(htmlwidgets::saveWidget(final_map, file.path(main_dir,occ_dir,
                                                   standardized_occ,data_out,
    paste0(taxa_cycle[i], "__map-for-vetting.html"))))

  cat("\tMapped ", taxa_cycle[i], ", ", i, " of ", length(taxa_cycle), ".\n\n", sep="")
}
