### 8-map_taxon_richness.R
### Author: Emily Beckman Bruns & Kate Good
### Supporting institutions: The Morton Arboretum
### Funding: 
#   -- Institute of Museum and Library Services (IMLS MFA program grant
#        MA-30-18-0273-18 to The Morton Arboretum)
#   -- NSF (award 1759759 to The Morton Arboretum)
### Last Updated: June 2023 ; first written August 2022
### R version 4.3.0

### DESCRIPTION:
  ## This script creates taxon richness maps at the country level (section 1) 
  #   and the state level (section 2). It could also be developed to create 
  #   taxon richness maps using other boundaries such as counties, ecoregions, 
  #   Key Biodiversity Areas (KBAs), Alliance for Zero Extinction (AZE) areas, 
  #   etc. The maps can also be edited to fit the formatting you would like...
  #   color, border weight, background tiles, etc.

### INPUTS:
  ## target_taxa_with_synonyms.csv
  #   If you ran 1-get_taxa_metdata.R and would like to map at the country level,
  #   you can use the target_taxa_with_synonyms.csv file
  ## occurrence point data
  #   If you're mapping at the state level, the script currently reads in 
  #   occurrence point data from 7-filter_occurrence_points.R 
  #   (taxon_points_final folder) to determine the number of taxa in each state;
  #   but you could also use occurrence points in a different format and edit 
  #   the code a little.
  ## (optional) geopolitical boundaries shapefile(s)
  #   Currently this script use country and state features from the rnaturalearth 
  #   package. But, if you would like to use other boundaries, you'll need to 
  #   have them as inputs.

### OUTPUTS:
  ## taxon_richness_maps (folder)
  #   If you run section 1: 
  #     Country-level taxon richness maps (HTML) for:
  #     - all taxa: country-level_taxon_richness_ALL.html
  #     - threatened taxa: country-level_taxon_richness_THREATENED.html
  #     - endemic taxa: country-level_taxon_richness_ENDEMIC.html
  #     [if there are no taxa in one of these subset, that map is skipped]
  #   If you run section 2: 
  #     State-level taxon richness maps (HTML), one for each country that has 
  #     your target taxa (e.g. state-level_taxon_richness_Mexico.html)

################################################################################
# Load libraries
################################################################################

# load packages
my.packages <- c('tidyverse','sf','RColorBrewer','leaflet','rnaturalearth',
                 'countrycode')
  # versions I used (in the order listed above): 2.0.0, 1.0-13, 1.1-3, 2.1.2, 0.3.3, 1.5.0
#install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

################################################################################
# Create functions
################################################################################

# function to create table of taxon richness and join to polygon data, for mapping
richness.countries <- function(df,polygons){
  # see max number of country codes for one taxon
  count_codes <- sapply(df$all_native_dist_iso2,function(x) str_count(x, pattern = "; "))
  # create an array of separated country codes
  iso_a2 <- str_split_fixed(df$all_native_dist_iso2, "; ", n = (max(count_codes)+1))
  # sum to calculate richness
  richness <- as.data.frame(table(iso_a2))
  # merge polygons with taxon richness data
  merged <- sp::merge(polygons,richness)
  # return shapefile with taxon richness added
  return(merged)
}

# create leaflet map for country-level taxon richness
map.countries <- function(countries,title,legend_text,legend_labels,pal){
  map <- leaflet() %>%
    # add background for the map; see additional background options here:
    #		https://leaflet-extras.github.io/leaflet-providers/preview/
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    # add countries to map
    addPolygons(data = countries,
                color = "grey", weight = 2, opacity = 1,
                fillColor = ~pal(countries$Freq), fillOpacity = 1) %>%
    # add legend with number of taxa represented by each color
    addLegend(values = countries$Freq,
              pal = pal, opacity = 1,
              title = legend_text,
              labFormat = function(type, cuts, p) {paste0(legend_labels)},
              position = "bottomright") %>%
    # add title
    addControl(title, position = "topright") %>%
    return(map)
}

# create leaflet map for state/province-level taxon richness
map.states <- function(state_richness,states_all,title,legend_text,
                       legend_labels,pal,centroids){
  map <- leaflet() %>%
    # add background for the map; see additional background options here:
    #		https://leaflet-extras.github.io/leaflet-providers/preview/
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    # add state/province taxon richness to map
    addPolygons(data = state_richness,
                stroke = FALSE,
                fillColor = ~pal(state_richness$Freq), fillOpacity = 1) %>%
    # add all state/province borders
    addPolygons(data = states_all,
                color = "grey", weight = 2, opacity = 1,
                fillOpacity = 0) %>%
    # add labels for number of taxa per state
    addLabelOnlyMarkers(data = state_richness,
                        lng = ~longitude, lat = ~latitude, 
                        label = ~Freq,
                        labelOptions = 
                          labelOptions(noHide = TRUE, textOnly = TRUE,
                                       # can change the text size as needed!
                                       textsize = "12px", 
                                       style = list("font-weight" = "bold"))) %>%
    # add legend with number of taxa represented by each color
    addLegend(values = state_richness$Freq,
              pal = pal, opacity = 1,
              title = legend_text,
              labFormat = function(type, cuts, p) {paste0(legend_labels)},
              position = "bottomright") %>%
    # add title
    addControl(title, position = "topright") %>%
    return(map)
}


################################################################################
# Set working directory
################################################################################

# use 0-set_working_directory.R script:
  # update to your path
source("/Users/emily/Documents/GitHub/conservation-gap-analysis/spatial-analysis-workflow/0-set_working_directory.R")

# create folder for output data
data_out <- "taxon_richness_maps"
if(!dir.exists(file.path(main_dir,occ_dir,standardized_occ,data_out)))
  dir.create(file.path(main_dir,occ_dir,standardized_occ,data_out), 
             recursive=T)


################################################################################
# 1. Country-level taxon richness
################################################################################

## note that these calculations are based on native countries for each target 
##  taxon, based on the native countries of occurrence pulled in script
##  1-get_taxa_metadata.R, which gets countries listed in IUCN Red List
##  assessments, BGCI's GlobalTreeSearch database, and any manual edits you add. 
##  Native countries are NOT pulled from the taxon's occurrence points; if you'd
##  like to do that, reference section 2 and create a new section as needed

# read in world countries layer from rnaturalearth package
world_polygons <- ne_countries(scale = 50, type = "countries", 
                               returnclass = "sf")

# read in target taxa list with native countries added in 1-get_taxa_metadata.R
taxon_list <- read.csv(file.path(main_dir,taxa_dir,"target_taxa_with_synonyms.csv"), 
                       header=T, colClasses="character",na.strings=c("","NA"))
# keep just our accepted taxa
taxon_list <- taxon_list %>% filter(taxon_name_status == "Accepted")


### calculate and map country-level taxon richness using functions we created above
# note you can do this for all taxa, or for subsets, as desired; below we create
#   maps for taxa assesses as Threatened on the IUCN Red List and endemic taxa;
#   change as you like!


####
## country-level richness for ALL taxa
####

## calculate richness
ctry_richness_all <- richness.countries(taxon_list,world_polygons)
## title text for map
my_title <- "Example taxon richness map"
## text for legend title
my_legend_title <- paste0("Number of native","<br/>"," target taxa")
## color bins and labels
  # can look at distribution of data to inform bins
hist(ctry_richness_all$Freq,breaks=20,xlim=c(0,50),ylim=c(0,25))
  # BASED ON YOUR DATA, assign bin breaks and labels 
  #   most color palettes work will with max of 9 bins (not counting Inf)
  # you'll need to decide if you want to use natural breaks, even, exponential, etc.
bins <- c(1,2,3,4,5,6,7,8,10,Inf)
my_legend_labels <- c("1","2","3","4","5","6","7","8-9","10+")
  # create color palette
  #   see palette options by running display.brewer.all()
my_palette <- colorBin(palette = "PuRd", bins = bins,
                                domain = ctry_richness_all$Freq, 
                                reverse = F, na.color = "white")
## create map
map_richness_all <- map.countries(ctry_richness_all,my_title,my_legend_title,
                                  my_legend_labels,my_palette)
## view map
map_richness_all
## save map
htmlwidgets::saveWidget(map_richness_all,
                        file.path(main_dir,occ_dir,standardized_occ,data_out,
                                  "country-level_taxon_richness_ALL.html"))

####
## country-level richness for THREATENED taxa
####

# select IUCN Red List Threatened taxa
threatened_taxa <- taxon_list %>%
	filter(rl_category == "Critically Endangered" | 
	       rl_category == "Endangered" |
	       rl_category == "Vulnerable")
if(nrow(threatened_taxa)>0){
  # reference comments in first section as needed; removed here since the same process
  ctry_richness_threatened <- richness.countries(threatened_taxa,world_polygons)
  my_title <- "Example taxon richness map"
  my_legend_title <- paste0("Number of native,","<br/>","threatened target taxa")
  hist(ctry_richness_threatened$Freq,breaks=20,xlim=c(0,50),ylim=c(0,25))
  bins <- c(1,2,3,4,5,6,10,15,Inf)
  my_legend_labels <- c("1","2","3","4","5","6-9","10-14","15+")
  my_palette <- colorBin(palette = "PuRd", bins = bins,
                         domain = ctry_richness_threatened$Freq, 
                         reverse = F, na.color = "white")
  map_richness_threatened <- map.countries(ctry_richness_threatened,my_title,
                                           my_legend_title,my_legend_labels,
                                           my_palette)
  map_richness_threatened
  htmlwidgets::saveWidget(map_richness_threatened,
                          file.path(main_dir,occ_dir,standardized_occ,data_out,
                                    "country-level_taxon_richness_THREATENED.html"))
} else { print("You have no IUCN Red List Threatened taxa") }


####
## country-level richness for ENDEMIC taxa
####

# select endemic taxa (only in one country)
endemic_taxa <- taxon_list %>% filter(nchar(all_native_dist_iso2) == 2)
if(nrow(endemic_taxa)>0){
  # reference comments in first section as needed; removed here since the same process
  ctry_richness_endemic <- richness.countries(endemic_taxa,world_polygons)
  my_title <- "Example taxon richness map"
  my_legend_title <- paste0("Number of endemic","<br/>","target taxa")
  hist(ctry_richness_endemic$Freq,breaks=20,xlim=c(0,50),ylim=c(0,25))
  bins <- c(1,2,3,4,5,6,7,8,10,Inf)
  my_legend_labels <- c("1","2","3","4","5","6","7","8-9","10+")
  my_palette <- colorBin(palette = "PuRd", bins = bins,
                         domain = ctry_richness_endemic$Freq, 
                         reverse = F, na.color = "white")
  map_richness_endemic <- map.countries(ctry_richness_endemic,my_title,
                                        my_legend_title,my_legend_labels,
                                        my_palette)
  map_richness_endemic
  htmlwidgets::saveWidget(map_richness_endemic,
                          file.path(main_dir,occ_dir,standardized_occ,data_out,
                                    "country-level_taxon_richness_ENDEMIC.html"))
} else { print("You have no endemic taxa") }



################################################################################
# 2. State/province-level taxon richness
################################################################################

## In this section we find native states for each target taxon by looking at 
## which states the points fall within.

# read in occurrence point files for all target taxa (output of 
#   7-filter_occurrence_points.R) and combine into one dataframe
taxon_files <- list.files(path=file.path(main_dir,occ_dir,standardized_occ,
                                         "taxon_points_final"), 
                          ignore.case=FALSE, full.names=TRUE, recursive=TRUE)
taxon_dfs <- lapply(taxon_files, read.csv, header = T, na.strings = c("","NA"),
                   colClasses = "character")
all_pts <- Reduce(bind_rows, taxon_dfs)
# keep only the columns we really need
all_pts <- all_pts %>% 
  select(taxon_name_accepted,decimalLatitude,decimalLongitude,
         latlong_countryCode,rl_category)
  # note that we are just creating a map that includes all target taxa; if you 
  #   want threatened, endemic, etc. you'll need to filter for that here and run 
  #   again (be sure to change the file output name in the loop below or it will
  #   save over your previous version)

# make latitude and longitude numeric
all_pts$decimalLatitude <- as.numeric(all_pts$decimalLatitude)
all_pts$decimalLongitude <- as.numeric(all_pts$decimalLongitude)
# convert point data to a spatial object
all_pts_sf <- st_as_sf(all_pts, coords = c("decimalLongitude","decimalLatitude"),
                       crs = 4326)

# create list of all countries with target taxa
target_codes <- unique(all_pts$latlong_countryCode)
target_countries <- countrycode(target_codes, origin = "iso2c",
                                destination = "country.name.en")
  # if you get the error "No such country ___ in the data" when running the loop
  #   below, you'll need to figure out which country name rnaturalearth uses 
  #   and replace it in your list of target countries; the US needs this:
  target_countries[target_countries=="United States"] <- "United States of America"
# see your list of target countries
target_countries
  
# create map text inputs for each country; can be the same or different, but
#   if you want them all to be the same you can skip the dataframe and assign
#   each to a variable as we did in section 1
  # dataframe has one row for each country; countries in the order listed in 
  #   target_countries:
map_input <- data.frame(
  country = target_countries,
  my_title = c("Example taxon richness map for the US",
               "Example taxon richness map for Canada",
               "Example taxon richness map for Mexico"),
  my_legend_title = c(paste0("Number of native","<br/>"," target taxa"),
                      paste0("Number of native","<br/>"," target taxa"),
                      paste0("Number of native","<br/>"," target taxa")),
  my_bins <- c("0,1,2,3,4,5",
               "0,1,2,3,4,5,6,7,Inf",
               "0,1,2"),
  my_legend_labels <- c('"0",1","2","3","4","5"',
                        '"0",1","2","3","4","5","6","7+"',
                        '"0",1","2"')
)

## loop through each country to read in a shapefile, calculate richness, and map
  # currently a separate map is created for each country; if you'd like one
  #   map for all countries, could add a section in the loop that joins the  
  #   country features so all can be visualized together.
for(i in 1:nrow(map_input)){
  
  # read in shapefile for the country, with state boundaries; we use the 
  #   rnaturalearth package to get the data
  state_polygons <- ne_states(country=map_input$country[i], returnclass = "sf")
  
  # join taxa points to states shapefile
  pts_state <- st_join(all_pts_sf, state_polygons)
  
  # count the number of target taxa in each state
  state_richness_df <- pts_state %>%
    group_by(name,taxon_name_accepted) %>%
    count() %>% ungroup() %>%
    group_by(name) %>%
    summarize(Freq = n_distinct(taxon_name_accepted))
  
  # rejoin this this richness data with the states shapefile
  state_richness <- st_join(state_polygons,state_richness_df)
  # replace NA values with 0
  state_richness$Freq[is.na(state_richness$Freq)] <- 0
  
  cat(paste0("--Number of taxa in states/provinces in ",map_input$country[i],": "))
  cat(sort(unique(state_richness$Freq)),"\n")
  
  # create palette for map; we'll use the same for every country
  my_palette <- colorBin(palette = "Greens", 
                         bins = as.numeric(unlist(strsplit(map_input$my_bins[i],split=","))),
                         domain = state_richness$Freq, 
                         reverse = F, na.color = "white")

  # create map
  map_richness <- map.states(state_richness,state_polygons,
                             map_input$my_title[i],
                             map_input$my_legend_title[i],
                             gsub("\"","",unlist(strsplit(map_input$my_legend_labels[i],split=","))),
                             my_palette, 
                             state_centroids)

  # save map
  htmlwidgets::saveWidget(map_richness,
                          file.path(main_dir,occ_dir,standardized_occ,data_out,
                                    paste0("state-level_taxon_richness_",
                                           gsub(" ","_",map_input$country[i]),
                                           ".html")))
  # print update
  cat("Created taxon richness map for ",map_input$country[i],"\n",sep="")
}




