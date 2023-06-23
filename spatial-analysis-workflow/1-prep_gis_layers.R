### 1-prep_gis_layers.R
### Author: Emily Beckman Bruns
### Supporting institutions: The Morton Arboretum
### Funding:
#   -- NSF (award 1759759 to The Morton Arboretum)
### Last Updated: June 2023
### R version 4.3.0

### DESCRIPTION:
  ## This script provides instructions for manually downloading some shapefiles
  #   and automatically downloads & prepares other shapefiles, all of which we 
  #   use in the spatial-analysis-workflow scripts

## INPUTS
  ## None

## OUTPUTS
  ## Ecoregions shapefile, either global, North American, or US (see 
  #   descriptions within script below)
  ## world_countries_10m (folder with shapefile)
  #   World countries layer from Natural Earth (https://www.naturalearthdata.com), 
  #   downloaded using the rnaturalearth package and edited to remove lakes
  #   and fix some 2-digit ISO code issues
  ## urban_areas_50m (folder with shapefile)
  #   Urban areas layer from Natural Earth:
  #   https://www.naturalearthdata.com/downloads/50m-cultural-vectors/50m-urban-areas/ 

################################################################################
# Load libraries
################################################################################

# install rnaturalearthhires package from github (not in CRAN for this R version)
#install.packages("devtools") # first install the devtools package if you don't have
devtools::install_github("ropensci/rnaturalearthhires")

# load packages
my.packages <- c('rnaturalearth','terra')
  # versions I used (in the order listed above): 0.3.3, 1.7-29
# install.packages(my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

################################################################################
# Set working directory
################################################################################

# use 0-set_working_directory.R script:
  # update to your path
source("/Users/emily/Documents/GitHub/conservation-gap-analysis/spatial-analysis-workflow/0-set_working_directory.R")

################################################################################
# **Manually** download ecoregions shapefile(s)
################################################################################

# Depending on the scale/region of your analysis, choose which ecoregions you'd 
#   like to use...

  ## Global terrestrial ecoregions from The Nature Conservancy (48 MB)
  #   To download, go to 
  #   https://geospatial.tnc.org/datasets/7b7fb9d945544d41b3e7a91494c42930/explore
  #   Click the "Download" button on the left
  #   In the "Shapefile" box on the left, click "Download"
  #   Place the downloaded folder (Terrestrial_Ecoregions) in your gis_layers
  #   folder

  ## Ecoregions of North America (Canada, US, Mexico), Level III, from the EPA (34 MB)
  #   See metadata here: https://www.epa.gov/eco-research/ecoregions-north-america  
  #   To download, go to
  #   https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/NA_CEC_Eco_Level3.zip
  #   The files will automatically download
  #   Place the downloaded folder (NA_CEC_Eco_Level3) in your gis_layers folder

  ## Ecoregions of the Continental US, Level IV (finest level), from the EPA (69 MB)
  #   See metadata here: https://www.epa.gov/eco-research/level-iii-and-iv-ecoregions-continental-united-states
  #   To download, go to
  #   https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l4_state_boundaries.zip
  #   The files will automatically download
  #   Place the downloaded folder (us_eco_l4_state_boundaries) in your 
  #   gis_layers folder

################################################################################
# Create layer for add country codes and clipping occurrence points to land
################################################################################

# we clip the occurrence points to the countries layer since some analyses rely
#   on knowing which country (or sometimes state) the point falls within

# download world countries layer using rnaturalearth package; vect converts to
#   a terra object (SpatVector)
world_ctry <- vect(ne_countries(scale = 10, type = "countries", 
                                returnclass = "sf"))
  # download layer of lakes using rnaturalearth package
lakes <- vect(ne_download(scale = 10, type = "lakes", category = "physical", 
                          returnclass = "sf"))
  # remove lakes
world_ctry <- erase(world_ctry,lakes)

# there is an issue with the 10m countries dataset where some 2-digit country
#   codes (which we need later) are -99; fixing...
sort(unique(world_ctry$iso_a2)) # see if there is "-99" or NA.. if yes...
  # see which countries have the issue:
unique(as.data.frame(world_ctry[which(
  is.na(world_ctry$iso_a2) | world_ctry$iso_a2=="-99"),"name_en"])[,1])
  # looks like right now there are 22 "countries" with the issue, but most are
  #   islands or other small areas which we will ignore for now; if you see
  #   an area that is important to you, add it to the list below
  # fix the iso_a2 field
world_ctry[which(world_ctry$admin=="Somaliland"),"iso_a2"] <- "SO"
world_ctry[which(world_ctry$admin=="France"),"iso_a2"] <- "FR"
world_ctry[which(world_ctry$admin=="Norway"),"iso_a2"] <- "NO"
world_ctry[which(world_ctry$admin=="Kosovo"),"iso_a2"] <- "XK"

# save file
writeVector(world_ctry, file.path(main_dir,gis_dir,"world_countries_10m"),
            filetype = "ESRI Shapefile")

################################################################################
# Download and save urban areas layer, in case you use for vetting points later
################################################################################

# download urban areas layer using rnaturalearth package
urban <- vect(ne_download(scale = 50, type = "urban_areas", returnclass = "sf"))

# save file
writeVector(urban, file.path(main_dir,gis_dir,"urban_areas_50m"),
            filetype = "ESRI Shapefile")
