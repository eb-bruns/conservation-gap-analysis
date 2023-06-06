### 4-compile_occurrence_data.R
### Author: Emily Beckman Bruns
### Supporting institutions: The Morton Arboretum, Botanic Gardens Conservation 
#   International-US, United States Botanic Garden, San Diego Botanic Garden,
#   Missouri Botanical Garden
### Funding: Base script funded by the Institute of Museum and Library 
#   Services (IMLS MFA program grant MA-30-18-0273-18 to The Morton Arboretum).
#   Moderate edits were added with funding from a cooperative agreement
#   between the United States Botanic Garden and San Diego Botanic Garden
#   (subcontracted to The Morton Arboretum), and NSF ABI grant #1759759
### Last Updated: June 2023 ; first written Feb 2020
### R version 4.2.2

### DESCRIPTION:
  ## This script compiles in situ occurrence point data that was downloaded in
  #   the 3-get_occurrence_data.R script. We remove any rows for taxa not in our 
  #   list (target_taxa_with_synonyms.csv), add a UID (universal identifier) 
  #   that can be used to manually flag points in the maps created in script
  #   6-visualize_occurrence_data.R, and check the standardization of key 
  #   columns including localityDescription, year, basisOfRecord,
  #   establishmentMeans, decimalLatitude, and decimalLongitude. Records without 
  #   coordinates are separated out and saved, then records with coordinates are 
  #   saved in taxon-specific files.
  
### INPUTS:
  ## target_taxa_with_synonyms.csv
  #   List of target taxa and synonyms; see example in the "Target taxa list"
  #   tab in Gap-analysis-workflow_metadata workbook; Required columns include: 
  #   taxon_name, taxon_name_accepted, and taxon_name_status (Accepted/Synonym).
  ## occurrence data downloaded in 3-get_occurrence_data.R
  #   Including any/all of: gbif.csv, idigbio.csv, redlist.csv, seinet.csv, 
  #   bien.csv, fia.csv, exsitu.csv, and/or additional files if added manually 
  #   via instructions provided in that script.
  ## World_Countries_(Generalized)/World_Countries__Generalized_.shp
  #   World country boundaries, used to remove points that are in the water; 
  #   shapefile can be downloaded from
  #   https://hub.arcgis.com/datasets/esri::world-countries-generalized/explore
  #   and placed in your gis_layers folder; if you ran 2-compile_exsitu_data.R
  #   you already have this layer downloaded and ready to use.

### OUTPUTS:
  ## "taxon_raw_points" folder
  #   For each taxon in your target taxa list, a CSV of occurrence records with 
  #   valid lat-long coordinates (e.g., Asimina_triloba.csv)
  ## need_geolocation.csv
  #   CSV of all occurrence records without valid lat-long but with locality 
  #   description; these could be manually geolocated if desired
  ## occurrence_record_summary.csv
  #   Summary table with one row for each target taxon, listing the number of 
  #   records with valid lat-long and number with locality description only
  
################################################################################
# Load libraries
################################################################################

my.packages <- c('tidyverse','textclean','data.table','CoordinateCleaner',
                 'terra','tidyterra','raster','countrycode')
# versions I used (in the order listed above): 2.0.0, 0.9.3, 1.14.8, 2.0-20,
#                                              1.7-29, 0.4.0, 3.6-13, 1.5.0

#install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

select <- dplyr::select

################################################################################
# Set working directory
################################################################################

# use 0-set_working_directory.R script:
source("/Users/emily/Documents/GitHub/conservation-gap-analysis/spatial-analysis-workflow/0-set_working_directory.R")

################################################################################
# Read in and compile occurrence point data
################################################################################

# create folder for output data
if(!dir.exists(file.path(main_dir,occ_dir,standardized_occ,"taxon_raw_points")))
   dir.create(file.path(main_dir,occ_dir,standardized_occ,"taxon_raw_points"), 
             recursive=T)

# read in raw occurrence data
file_list <- list.files(file.path(main_dir,occ_dir,standardized_occ), 
                        pattern = ".csv", full.names = T)
file_dfs <- lapply(file_list, read.csv, header = T, na.strings = c("","NA"),
                   colClasses = "character")
length(file_dfs)

# stack all datasets using bind_rows, which keeps non-matching columns
#   and fills with NA; 'Reduce' iterates through list and merges with previous;
#   this may take a few minutes if you have lots of data
all_data <- Reduce(bind_rows, file_dfs)
rm(nms, file_dfs, file_list)
nrow(all_data)
table(all_data$database)

################################################################################
# Filter by target taxa
################################################################################

# read in target taxa list
taxon_list <- read.csv(file.path(main_dir, taxa_dir,
                                 "target_taxa_with_synonyms.csv"),
                       header=T, colClasses="character",na.strings=c("","NA"))
target_taxa <- unique(taxon_list$taxon_name)
# if needed, add columns that separate out taxon name
taxon_list <- taxon_list %>%
  separate("taxon_name",c ("genus","species","infra_rank","infra_name"),
           sep=" ", remove=F, fill="right")
# create species name column
taxon_list$species_name <- paste(taxon_list$genus,taxon_list$species)

## join data to taxon list
all_data <- all_data %>% dplyr::select(-genus,-species_name)
all_data <- left_join(all_data,taxon_list)
## join again just by species name if no taxon match
need_match <- all_data[which(is.na(all_data$taxon_name_status)),]
nrow(need_match)
  # remove columns from first taxon name match
need_match <- need_match[,1:(ncol(all_data)-ncol(taxon_list)+1)]
  # rename column for matching and get just the genus and specific epithet
need_match <- need_match %>% rename(taxon_name_full = taxon_name)
need_match$taxon_name <- str_split_i(need_match$taxon_name, " var.| subsp.| f.",i = 1)
  # new join
need_match <- left_join(need_match,taxon_list)
need_match$taxon_name <- need_match$taxon_name_full
need_match <- need_match %>% select(-taxon_name_full)
  # bind together new matches and previously matched
matched <- all_data[which(!is.na(all_data$taxon_name_status)),]
all_data <- rbind(matched,need_match)
table(all_data$taxon_name_status)

# check names that got excluded.....
still_no_match <- all_data[which(is.na(all_data$taxon_name_status)),]
nrow(still_no_match)
sort(unique(still_no_match$taxon_name))
table(still_no_match$database)
  # ^ if you want to keep any of these, add them to your target taxa list
rm(matched,need_match,still_no_match)

# keep only rows for target taxa
all_data <- all_data %>% filter(!is.na(taxon_name_status))
nrow(all_data)

### see which target taxa have no occurrence data:
unique(taxon_list$taxon_name_acc)[
  !(unique(taxon_list$taxon_name_acc) %in% (unique(all_data$taxon_name_acc)))]

################################################################################
# Add UID (universal id) column 
################################################################################

# now we add a UID that can be used to manually flag points in the maps created 
#   later in script 6-visualize_occurrence_data.R

# !!!
## IT IS VERY IMPORTANT to be careful here IF you are adding additional 
#   occurrence data after already compiling and manually vetting (selecting
#   good/bad points) - you need to be sure any new data are sorted to the *end*
#   of your dataset before adding the UIDs, otherwise the IDs you've already
#   used for vetting will change (not good!). If this is your first time running
#   this script, you're all good.
## if needed, sort data so any new points are at the end; you can use whatever
#   field you'd like, but here is an example using the database column:
#all_data$database <- factor(all_data$database,
#                            levels = c("BIEN","Ex_situ","FIA","GBIF","iDigBio",
#                                       "IUCN_RedList","NorthAm_herbaria",
#                                       "<NEW_DATA>")) #change to your dataset name
#all_data <- all_data %>% arrange(database)

# add UID
nms <- names(all_data)
digits <- nchar(as.character(nrow(all_data)))
all_data <- all_data %>% 
  dplyr::mutate(UID=paste0('id', sprintf(paste0("%0",digits,"d"),
                                         1:nrow(all_data)))) %>% 
  dplyr::select(c('UID', all_of(nms)))

################################################################################
# Standardize/check some key columns
################################################################################

# create localityDescription column
#   "NAs introduced by coercion" warning ok
all_data <- all_data %>%
  unite("localityDescription",
        c(locality,municipality,higherGeography,county,stateProvince,country,
          countryCode,locationNotes,verbatimLocality), remove = F, 
        sep = " | ") %>%
  mutate(decimalLatitude=as.numeric(decimalLatitude),
         decimalLongitude=as.numeric(decimalLongitude))
# get rid of NAs but keep pipes, so you can split back into parts if desired
all_data$localityDescription <- mgsub(all_data$localityDescription,
                                      c("NA "," NA"), "")
# if no locality info at all, make it NA
all_data$localityDescription <- gsub("| | | | | | | |", NA,
                                     all_data$localityDescription, fixed = T)
# check it
head(unique(all_data$localityDescription))

# check year column
#   if ex situ years were not standardized, most of those get removed here :(
#   "NAs introduced by coercion" warning ok
all_data$year <- as.numeric(all_data$year)
# remove values less than 1500 or greater than current year
all_data$year[which(all_data$year < 1500 |
                    all_data$year > as.numeric(format(Sys.time(),"%Y")))] <- NA
sort(unique(all_data$year))

# check basis of record column
unique(all_data$basisOfRecord)
all_data$basisOfRecord[which(is.na(all_data$basisOfRecord))] <- "UNKNOWN"

# check establishment means
unique(all_data$establishmentMeans)
# use this example if you need to recode any mistakes:
#all_data <- all_data %>%
#  mutate(establishmentMeans = recode(establishmentMeans,
#    "Introduced" = "INTRODUCED",
#    "Uncertain" = "UNCERTAIN",
#    "Native" = "NATIVE"))
all_data$establishmentMeans[which(is.na(all_data$establishmentMeans))] <-
  "UNCERTAIN"

## check validity of lat and long; remove invalid coordinates
  # if coords are both 0, set to NA
zero <- which(all_data$decimalLatitude == 0 & all_data$decimalLongitude == 0)
all_data$decimalLatitude[zero] <- NA; all_data$decimalLongitude[zero] <- NA; rm(zero)
  # flag non-numeric and not available coordinates, and when not valid, i.e.:
  #   lat > 90, lat < -90, lon > 180, and lon < -180
coord_test <- cc_val(all_data, lon = "decimalLongitude", lat = "decimalLatitude",
                     value = "flagged", verbose = TRUE)
  # try switching lat and long for invalid points and check validity again
all_data[!coord_test,c("decimalLatitude","decimalLongitude")] <-
  all_data[!coord_test,c("decimalLongitude","decimalLatitude")]
coord_test <- cc_val(all_data, lon = "decimalLongitude",lat = "decimalLatitude",
                     value = "flagged", verbose = TRUE)
  # mark these as flagged
all_data$flag <- NA
all_data[!coord_test,]$flag <- "Coordinates invalid"
rm(coord_test)

# write file of raw data before selecting only geolocated records;
#   this will be used for the GapAnalysis package's summary of occurrences
write.csv(all_data, file.path(main_dir,occ_dir,raw_occ,
                              paste0("all_occurrence_data_raw_", Sys.Date(), 
                                     ".csv")), row.names = F)

################################################################################
# Select records that have coordinates and are on land
################################################################################

# move forward with subset of points that do have lat and long
geo_pts <- all_data %>% dplyr::filter(is.na(flag))
nrow(geo_pts)
no_geo_pts <- all_data %>% dplyr::filter(!is.na(flag))

# check if points are in water and mark
# you can also skip this if you don't mind the water points - can take 
# a few minutes or more to buffer and intersect the polygons
  # read in world polygons shapefile
world_polygons <- vect(file.path(main_dir,gis_dir,
   "World_Countries_(Generalized)/World_Countries__Generalized_.shp"))                             
  # add 1km buffer to ecoregion layer, so we keep pts close to land;
  # width is in meters - change as desired; > ~500 threw this error:
    # "IllegalArgumentException: point array must contain 0 or >1 elements"
world_buff <- terra::buffer(world_polygons, width=500)
rm(world_polygons)
  # make occurrence points a spatial object
geo_pts_spatial <- vect(
  cbind(geo_pts$decimalLongitude,geo_pts$decimalLatitude),
  atts=geo_pts, crs="EPSG:4326")
  # intersect points with ecoregions layer
land_pts <- terra::intersect(geo_pts_spatial,world_buff)
on_land <- as.data.frame(land_pts); nrow(on_land) #2185947
land_id <- unique(on_land$UID)
  # flag points not on land (in water)
geo_pts[which(!(geo_pts$UID %in% land_id)),"flag"] <- "Coordinates in water"
all_data <- rbind(no_geo_pts,geo_pts)
table(all_data$flag)
  # Coordinates in water  Coordinates invalid 
  # 10249                 317852

# separate out points with locality description only, including:
# lat-long is invalid or in water
locality_pts <- all_data %>% 
  dplyr::filter(!is.na(localityDescription) & !is.na(flag))
nrow(locality_pts) #326017
  # save the file, for reference
write.csv(locality_pts, file.path(main_dir,data,
  paste0("need_geolocation_", Sys.Date(), ".csv")),
  row.names = F)

# create final subset of geolocated points which are on land
geo_pts <- all_data %>% filter(is.na(flag))
nrow(geo_pts) #2184049
table(geo_pts$database)
# BIEN    Ex_situ   FIA      GBIF     iDigBio   IUCN_RedList  NorthAm_herbaria
# 808255  1429      917467   344725   38286     22429         51458

rm(all_data,geo_pts_spatial,land_pts,no_geo_pts,on_land,world_buff,land_id)

################################################################################
# Standardize country code column for checking against lat-long later
################################################################################

# country name to 3 letter ISO code
  # fix some issues first (can add anything that is not matched unambiguously)
geo_pts$country <- mgsub(geo_pts$country,
    c("Bolívia","Brasil","EE. UU.","ESTADOS UNIDOS DE AMERICA",
      "México","MÉXICO","Repubblica Italiana","U. S. A.","United Statese",
      "America","Atats-Unis","CAN","ESP","MA(C)xico","MEX",
      "MX","PER","Unknown","Cultivated"),
    c("Bolivia","Brazil","United States","United States",
      "Mexico","Mexico","Italy","United States","United States",
      "United States","United States","Canada","Spain","Mexico","Mexico",
      "Mexico","Peru",NA,NA))
  # find codes for names
country_codes1 <- as.data.frame(sort(unique(geo_pts$country))) %>%
  add_column(iso3c = countrycode(sort(unique(geo_pts$country)),
      origin="country.name", destination="iso2c")) %>%
  rename("country" = "sort(unique(geo_pts$country))",
         "countryCode_standard" = "iso3c")
  # add to data
geo_pts <- left_join(geo_pts,country_codes1)

# country code to standard 2 letter ISO code
  # fix some issues (can add anything that is not matched unambiguously)
geo_pts$countryCode <- mgsub(geo_pts$countryCode,
    c("XK","ZZ"),c("RS",NA))
geo_pts$countryCode <- str_to_upper(geo_pts$countryCode)
  # check codes 
country_codes2 <- as.data.frame(sort(unique(geo_pts$countryCode))) %>%
  add_column(iso3c = countrycode(sort(unique(geo_pts$countryCode)),
      origin="iso2c", destination="iso2c")) %>%
  rename("countryCode" = "sort(unique(geo_pts$countryCode))",
         "countryCode_standard2" = "iso3c")
# add to data
geo_pts <- left_join(geo_pts,country_codes2)
geo_pts <- geo_pts %>% 
  unite("countryCode_standard",
        c("countryCode_standard","countryCode_standard2"),
        sep=";",remove=T,na.rm=T) %>%
  separate("countryCode_standard","countryCode_standard",sep=";",extra="drop")
geo_pts$countryCode_standard[which(geo_pts$countryCode_standard == "")] <- NA
sort(table(geo_pts$countryCode_standard))

################################################################################
# Remove spatial duplicates
################################################################################

## this section could potentially be moved to another script

## OTHER WAYS OF REMOVING DUPLICATES ARE ALSO POSSIBLE AND COULD MAKE MORE
##    SENSE FOR A SPECIFIC WAY OF USING THE POINTS, including
##    by grid cell, distance between points (e.g., randomly via spThin), etc...
## The segment below removes spatial duplicates based on rounded latitude
##    and longitude. This is a simple fix that doesn't involve spatial 
##    calculations. One additional positive of this method is that you can
##    choose priority datasets to keep points from; this can also help with 
##    citations since data from some databases are harder to cite than others.

# first, if you're working at the taxon level, add infrataxon records to their 
# parent species too
add_again <- geo_pts %>% filter(grepl("var\\.|subsp\\.", taxon_name_accepted))
unique(add_again$taxon_name_accepted)
add_again$taxon_name_accepted <- gsub(" var\\.*\\s.+", "",
                                      add_again$taxon_name_accepted)
unique(add_again$taxon_name_accepted)
geo_pts <- rbind(geo_pts,add_again)
table(geo_pts$database)
# BIEN    Ex_situ   FIA      GBIF     iDigBio   IUCN_RedList  NorthAm_herbaria
# 815391  1530      917467   351671   42525     22893         59884

# create rounded latitude and longitude columns for removing duplicates;
#   number of digits can be changed based on how dense you want data
geo_pts$lat_round <- round(geo_pts$decimalLatitude,digits=2)
geo_pts$long_round <- round(geo_pts$decimalLongitude,digits=2)

# create subset of all ex situ points, to add back in at end, if desired
ex_situ <- geo_pts[which(geo_pts$database=="Ex_situ"),]
ex_situ <- ex_situ %>% 
  dplyr::select(-flag) %>%
  mutate(coordinateUncertaintyInMeters=as.numeric(coordinateUncertaintyInMeters))

# sort before removing duplicates;
# whatever you sort to the top will be kept when there is a duplicate further down;
# you can turn any of these steps on/off, or add others
  # sort by basis of record
unique(geo_pts$basisOfRecord)
geo_pts$basisOfRecord <- factor(geo_pts$basisOfRecord,
  levels = c("PRESERVED_SPECIMEN","MATERIAL_SAMPLE","MATERIAL_CITATION",
             "OBSERVATION","HUMAN_OBSERVATION","OCCURRENCE","PHYSICAL_SPECIMEN",
             "MACHINE_OBSERVATION","FOSSIL_SPECIMEN","LIVING_SPECIMEN",
             "UNKNOWN"))
geo_pts <- geo_pts %>% arrange(basisOfRecord)
  # sort by establishment means
unique(geo_pts$establishmentMeans)
geo_pts$establishmentMeans <- factor(geo_pts$establishmentMeans,
  levels = c("NATIVE","WILD","UNCERTAIN","INTRODUCED","MANAGED","CULTIVATED",
             "DEAD")) #"INVASIVE"
geo_pts <- geo_pts %>% arrange(establishmentMeans)
  # sort by coordinate uncertainty
geo_pts$coordinateUncertaintyInMeters <-
  as.numeric(geo_pts$coordinateUncertaintyInMeters)
geo_pts <- geo_pts %>% arrange(geo_pts$coordinateUncertaintyInMeters)
  # sort by year
geo_pts <- geo_pts %>% arrange(desc(year))
  # sort by source database
unique(geo_pts$database)
geo_pts$database <- factor(geo_pts$database,
  levels = c("GBIF","iDigBio","IUCN_RedList","NorthAm_herbaria",
             "FIA","BIEN","Ex_situ"))
geo_pts <- geo_pts %>% arrange(database)

# remove duplicates
# can create "all_source_databases" column, to capture
#    databases from which duplicates were removed
# can take a while to remove duplicates if there are lots a rows
geo_pts2 <- geo_pts %>%
  group_by(taxon_name_accepted,lat_round,long_round) %>%
  mutate(all_source_databases = paste(unique(database), collapse = ', ')) %>%
  distinct(taxon_name_accepted,lat_round,long_round,.keep_all=T) %>%
  ungroup() %>%
  dplyr::select(-flag)
nrow(geo_pts2) #366757

# add ex situ data back in
geo_pts2 <- full_join(geo_pts2,ex_situ)
nrow(geo_pts2) #367559

## set header/column name order
keep_col <- c( #data source and unique ID
  "UID","database","all_source_databases",
  #taxon
  "taxon_name_accepted",
  "taxon_name","scientificName","family","genus","specificEpithet",
  "taxonRank","infraspecificEpithet","taxonIdentificationNotes",
  #event
  "year","basisOfRecord",
  #record-level
  "nativeDatabaseID","institutionCode","datasetName","publisher",
  "rightsHolder","license","references","informationWithheld",
  "issue","recordedBy",
  #occurrence
  "establishmentMeans","individualCount",
  #location
  "decimalLatitude","decimalLongitude",
  "coordinateUncertaintyInMeters","geolocationNotes",
  "localityDescription","locality","verbatimLocality",
  "locationNotes","municipality","higherGeography","county",
  "stateProvince","country","countryCode","countryCode_standard",
  #additional optional data
  "taxon_name_status","iucnredlist_category",
  "natureserve_rank","fruit_nut")
# set column order and remove a few unnecessary columns
geo_pts2 <- geo_pts2[,keep_col]

# take a look
head(as.data.frame(geo_pts2))
nrow(geo_pts2) #367559
table(geo_pts2$database)
# BIEN    Ex_situ   FIA      GBIF     iDigBio   IUCN_RedList  NorthAm_herbaria
# 12703   1530      160099   176790   10345     3969          8879
rm(geo_pts)

# write file if you'd like
#write.csv(geo_pts2, file.path(main_dir,data,standard,
#  paste0("Occurrences_Compiled_", Sys.Date(), ".csv")),row.names = F)

################################################################################
# 6. Look at results
################################################################################

# summarize results for each target taxon
  # lat-long records
count_geo <- geo_pts2 %>% count(taxon_name_accepted)
names(count_geo)[2] <- "num_latlong_records"
  # locality-only records (invalid coords or in water)
count_locality <- locality_pts %>% count(taxon_name_accepted)
names(count_locality)[2] <- "num_locality_records"
  # make table
files <- list(count_geo,count_locality)
summary <- setorder(Reduce(full_join, files),num_latlong_records,na.last=F)
head(summary)
  # write file
write.csv(summary, file.path(main_dir,data,
  paste0("occurrence_record_summary_", Sys.Date(), ".csv")),row.names = F)
#as.data.frame(summary)

################################################################################
# Split by species to save
################################################################################

# split records to create one CSV for each target taxon
sp_split <- split(geo_pts2, as.factor(geo_pts2$taxon_name_accepted))
names(sp_split) <- gsub(" ","_",names(sp_split))
names(sp_split) <- gsub("\\.","",names(sp_split))

# write files
lapply(seq_along(sp_split), function(i) write.csv(sp_split[[i]],
  file.path(main_dir,data,standard,"taxon_raw_points",
  paste0(names(sp_split)[[i]],".csv")),row.names = F))
