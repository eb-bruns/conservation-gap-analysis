### 4-compile_occurrence_points.R
### Authors: Emily Beckman Bruns & Shannon M Still
### Supporting institutions: The Morton Arboretum, Botanic Gardens Conservation 
#   International-US, United States Botanic Garden, San Diego Botanic Garden,
#   Missouri Botanical Garden, UC Davis Arboretum & Botanic Garden
### Funding: 
#   -- Institute of Museum and Library Services (IMLS MFA program grant
#        MA-30-18-0273-18 to The Morton Arboretum)
#   -- United States Botanic Garden (cooperative agreement with San Diego
#        Botanic Garden)
#   -- NSF (award 1759759 to The Morton Arboretum)
### Last Updated: June 2023 ; first written Feb 2020
### R version 4.3.0

### DESCRIPTION:
  ## This script compiles in situ occurrence point data that was downloaded in
  #   the 3-get_occurrence_data.R script. We remove any rows for taxa not in our 
  #   list (target_taxa_with_synonyms.csv), add a UID (universal identifier) 
  #   that can be used to manually flag points in the maps created in script
  #   6-visualize_occurrence_points.R; we also check the standardization of key 
  #   columns including localityDescription, year, basisOfRecord,
  #   establishmentMeans, decimalLatitude, and decimalLongitude. Records without 
  #   valid coordinates or that fall in the water are separated out and saved, 
  #   then records with valid coordinates are thinned spatially and saved in 
  #   taxon-specific CSV files. There is also an option to add an elevationInMeters
  #   column that can be used later for filtering taxon points.
  ## NOTE THAT THE CoordinateCleaner PACKAGE CURRENT VERSION (2.0-20) DEPENDS ON
  ## rgeos and rgdal, WHICH WILL RETIRE SHORTLY! If the package does not release
  ## an update that removes these dependencies, the functions in this script 
  ## that use CoordinateCleaner will need to be replaced manually or skipped; 
  ## but, it looks like they are working on removing dependencies:
  ## https://github.com/ropensci/CoordinateCleaner/issues/78
  ## THIS IS ALSO TRUE FOR elevatr PACKAGE CURRENT VERSION (0.4.5) -- the next 
  ## version will remove dependency on rgdal

### INPUTS:
  ## target_taxa_with_synonyms.csv
  #   List of target taxa and synonyms; see example in the "Target taxa list"
  #   tab in Gap-analysis-workflow_metadata workbook; Required columns include: 
  #   taxon_name, taxon_name_accepted, and taxon_name_status (Accepted/Synonym).
  ## input_datasets (folder)
  #   Occurrence data downloaded in 3-get_occurrence_data.R, including
  #   any/all of: gbif.csv, idigbio.csv, redlist.csv, seinet.csv, 
  #   bien.csv, fia.csv, exsitu.csv, and/or additional files if added manually 
  #   via instructions provided in that script.
  ## world_countries_10m.shp
  #   Shapefile created in 1-prep_gis_layers.R script. It's the Natural Earth 
  #   10m countries layer with the lakes cut out and some ISO_2A issues fixed.

### OUTPUTS:
  ## all_occurrence_data_raw_YYYY_MM_DD.csv
  #   All occurrence data compiled, before removing any records; this is used
  #   in the GapAnalysis package
  ## could_attempt_geolocation_YYYY_MM_DD.csv
  #   All occurrence records without valid lat-long but with locality 
  #   description; these could be manually geolocated if desired, but usually
  #   that is only needed if you have a rare species without enough records
  ## removed_water_points_YYYY_MM_DD.csv
  #   Points that fall outside the country boundary features (in the water), and
  #   are therefore removed. Sometimes these are very close to land and the
  #   coordinates can be edited, as desired, to move them to land
  ## summary_of_occurrences_YYYY_MM_DD.csv
  #   Summary table with one row for each target taxon, listing the number of 
  #   records with valid lat-long and number with locality description only
  ## taxon_points_raw (folder)
  #   For each taxon in your target taxa list, a CSV of occurrence records with 
  #   valid lat-long coordinates (e.g., Asimina_triloba.csv)
  
################################################################################
# Load libraries
################################################################################

my.packages <- c('tidyverse','textclean','CoordinateCleaner','terra','countrycode'
                 # if you'd like to flag by elevation, you'll need:
                 ,'elevatr',"sf"
)
  # versions I used (in the order listed above): 2.0.0, 0.9.3, 2.0-20, 1.7-29, 1.5.0
  #                                              0.4.5
#install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

# be extra sure we are using dplyr when we mean to
select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
rename <- dplyr::rename
count <- dplyr::count

################################################################################
# Set working directory
################################################################################

# use 0-set_working_directory.R script:
  # update to your path
source("/Users/emily/Documents/GitHub/conservation-gap-analysis/spatial-analysis-workflow/0-set_working_directory.R")

# create folder for output data
data_out <- "taxon_points_raw"
if(!dir.exists(file.path(main_dir,occ_dir,standardized_occ,data_out)))
  dir.create(file.path(main_dir,occ_dir,standardized_occ,data_out), 
             recursive=T)

# assign folder where you have input data (saved in 3-get_occurrence_data.R)
data_in <- "input_datasets"

################################################################################
# Read in and compile occurrence point data
################################################################################

# read in raw occurrence data from 3-get_occurrence_data.R
file_list <- list.files(file.path(main_dir,occ_dir,standardized_occ,data_in), 
                        pattern = ".csv", full.names = T)
file_dfs <- lapply(file_list, read.csv, header = T, na.strings = c("","NA"),
                   colClasses = "character")
length(file_dfs)

# stack all datasets using bind_rows, which keeps non-matching columns
#   and fills with NA; 'Reduce' iterates through list and merges with previous;
#   this may take a few minutes if you have lots of data
all_data <- Reduce(bind_rows, file_dfs)
rm(file_dfs, file_list)
nrow(all_data)
table(all_data$database)

################################################################################
# Filter by target taxa
################################################################################

# read in target taxa list
taxon_list <- read.csv(file.path(main_dir, taxa_dir,"target_taxa_with_synonyms.csv"),
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
#   later in script 6-visualize_occurrence_points.R

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
  mutate(UID=paste0('id', sprintf(paste0("%0",digits,"d"),
                                         1:nrow(all_data)))) %>% 
  select(c('UID', all_of(nms)))

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

################################################################################
# Add lat-long country code column & flag points that are not on land
################################################################################

# we clip the occurrence points to the countries layer since some analyses rely
#   on knowing which country (or sometimes state) the point falls within
# IMPROVEMENT OPPORTUNITY: a buffer could be added to the countries layer, to 
#   catch points that are just outside the feature boundary but still could
#   be on land, since the features can't be so exact (too large)

# read in world countries layer created in 1-prep_gis_layers.R
world_ctry <- vect(file.path(main_dir,gis_dir,"world_countries_10m",
                                 "world_countries_10m.shp"))
# select just the 2-digit country code column we need
world_ctry <- world_ctry[,"iso_a2"]

# add country polygon data to each point based on lat-long location
  # turn occurrence point data into a spatial object
have_coord <- all_data %>% filter(!is.na(decimalLatitude) & !is.na(decimalLongitude))
pts_spatial <- vect(cbind(have_coord$decimalLongitude,have_coord$decimalLatitude),
                    atts=have_coord, crs="+proj=longlat +datum=WGS84")
no_coord <- anti_join(all_data,have_coord)
  # intersect with countries layer
  # if this takes too long, you may need to download the 50m layer in script 
  #   1-prep_gis_layers.R instead of the 10m layer
pts_spatial <- terra::intersect(pts_spatial,world_ctry)
  # if the following two lines are not zero, you should look at script
  #  1-prep_gis_layers.R and fill in an additional missing code(s)
nrow(pts_spatial[which(is.na(pts_spatial$iso_a2)),])
nrow(pts_spatial[which(pts_spatial$iso_a2=="-99"),])
  # bring all the data back together and flag water points
on_land <- as.data.frame(pts_spatial)
on_land <- on_land %>% rename(latlong_countryCode = iso_a2)
land_id <- unique(on_land$UID)
in_water <- have_coord %>% filter(!(UID %in% land_id)); nrow(in_water)
in_water$flag <- "Not on land"
have_coord <- full_join(on_land,in_water)
all_data2 <- full_join(have_coord,no_coord)
rm(have_coord,pts_spatial,no_coord,on_land,land_id,in_water)
nrow(all_data2)

################################################################################
# Select records that have valid coordinates and are not in the water
################################################################################

# first, if you're working at the taxon level, add infrataxon records to their 
#   parent species too
add_again <- all_data2 %>% filter(grepl("var\\.|subsp\\.", taxon_name_accepted))
unique(add_again$taxon_name_accepted)
add_again$taxon_name_accepted <- gsub(" var\\.*\\s.+", "",add_again$taxon_name_accepted)
unique(add_again$taxon_name_accepted)
all_data2 <- rbind(all_data2,add_again)
table(all_data2$database)

# write file of raw data before selecting only geolocated records;
#   this will be used for the GapAnalysis package's summary of occurrences
# if you don't plan to run the GapAnalysis package, you can skip this step
#   since it saves a pretty large file if lots of points for your taxa!
write.csv(all_data2, file.path(main_dir,occ_dir,standardized_occ,
                              paste0("all_occurrence_data_raw_", Sys.Date(), 
                                     ".csv")), row.names = F)

# separate out points with locality description but no valid lat-long
table(all_data2$flag)
locality_pts <- all_data2 %>% filter(!is.na(localityDescription) & 
                                     flag == "Coordinates invalid")
nrow(locality_pts)
  # save the file, for reference; you can geolocate these records if you want, 
  #   but usually only necessary for rare taxa without enough lat-long records
write.csv(locality_pts, file.path(main_dir,occ_dir,standardized_occ,
                                  paste0("could_attempt_geolocation_", 
                                         Sys.Date(), ".csv")), row.names = F)

# separate out points that will be removed because they're in the water; can
#   reference this, and potentially move points to the land if desired
water_pts <- all_data2 %>% filter(flag == "Not on land")
nrow(water_pts)
# save the file, for reference; you can geolocate these records if you want, 
#   but usually only necessary for rare taxa without enough lat-long records
write.csv(water_pts, file.path(main_dir,occ_dir,standardized_occ,
                               paste0("removed_water_points_", Sys.Date(), ".csv")),
          row.names = F)

### create final subset that is only points with valid lat-long on land
geo_pts <- all_data2 %>% filter(is.na(flag))
nrow(geo_pts)
  # see how many points are from each database
table(geo_pts$database)

################################################################################
# Standardize country code column for checking against lat-long later
################################################################################

# country name to 3 letter ISO code
  # fix some issues first (can add anything that is not matched unambiguously in
  # the 'find codes for names' step directly below; then rerun from here)
geo_pts$country <- mgsub(geo_pts$country,
    c("brasil","EE. UU.","estados unidos","estados unidos de america",
      "méxico","México"),#matched unambiguously
    c("Brazil","United States","United States","United States",
      "Mexico","Mexico")) #country name to use
  # find codes for names
country_codes1 <- as.data.frame(sort(unique(geo_pts$country))) %>%
  add_column(iso3c = countrycode(sort(unique(geo_pts$country)),
      origin="country.name", destination="iso2c")) %>%
  rename("country" = "sort(unique(geo_pts$country))",
         "countryCode_standard" = "iso3c")
  # add to data
geo_pts <- left_join(geo_pts,country_codes1)

# country code to standard 2 letter ISO code
  # fix some issues (can add anything that is not matched unambiguously in the
  # 'check codes' step directly below; then rerun from here)
geo_pts$countryCode <- mgsub(geo_pts$countryCode,
    c("AUT","CAN","CZE","DEU","MEX","NOR","SWE","USA","ZZ"), #matched unambiguously
    c("AU","CA","CZ","DE","MX","NO","SE","US",NA)) #2-letter code to use
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
  # see how many records have each country code:
sort(table(geo_pts$countryCode_standard))

################################################################################
# (Optionally) Add elevation data to points
################################################################################

# create an sf object of your coordinates
sf_points <- st_as_sf(geo_pts, 
                      coords = c("decimalLongitude","decimalLatitude"),
                      crs = 4326)
# add elevation column to the points
#   this can take a couple minutes or longer if you have many points; if it 
#   takes too long, you can move this step to script #5, inside the 
#   taxon-by-taxon loop
add_elev <- get_elev_point(locations = sf_points,
                           proj = "+proj=longlat +datum=WGS84",
                           src = "aws")
# add elevation column to our main data frame
geo_pts$elevationInMeters <- add_elev$elevation

################################################################################
# Remove spatial duplicates
################################################################################

## NOTE THAT IF YOUR TARGET TAXA ARE NOT WIDESPREAD, IT MAY BE BETTER TO SKIP
## THIS STEP, ESPECIALLY IF YOU'RE USING RESTRICTING FILTERS -- E.G. ONLY USING
## POINTS WITH A SMALL COORDINATE UNCERTAINTY -- SINCE YOU MAY NEED ALL THE 
## DATA YOU HAVE! This step was designed for taxa with many coordinates.

## In this section we "thin" our points by removing points that are near each
#   other, to make the data easier to vet and visualize. depending on your 
#   target taxa and analysis goals, you may want to filter the points more
#   or less.
## There are multiple ways to remove spatial duplicates, such as by grid cell, 
#   the distance between points (e.g., randomly via a package like spThin), etc.
## The section below removes spatial duplicates based on rounded latitude
#   and longitude. This is a simple fix that doesn't involve spatial 
#   calculations. One additional positive of this method is that you can
#   choose priority datasets to keep points from; this can also help with 
#   citations since data from some databases are harder to cite than others.

# create rounded latitude and longitude columns for removing duplicates;
#   number of digits can be changed based on how dense you want data; via this
#   StackExchange post (https://gis.stackexchange.com/a/8674/7913)...
#   "The first decimal place is worth up to 11.1 km: it can distinguish the position of one large city from a neighboring large city.
#    The second decimal place is worth up to 1.1 km: it can separate one village from the next.
#    The third decimal place is worth up to 110 m: it can identify a large agricultural field or institutional campus.
#    The fourth decimal place is worth up to 11 m: it can identify a parcel of land. It is comparable to the typical accuracy of an uncorrected GPS unit with no interference."
#   If your target taxa are rare, you may want more digits; if your target 
#   taxa are common/widespread, you probably want fewer digits (more points removed)
geo_pts$lat_round <- round(geo_pts$decimalLatitude,digits=2)
geo_pts$long_round <- round(geo_pts$decimalLongitude,digits=2)

# create subset of all ex situ points, to add back in at the end, if desired
ex_situ <- geo_pts[which(geo_pts$database=="Ex_situ"),]
ex_situ <- ex_situ %>% 
  select(-flag) %>%
  mutate(coordinateUncertaintyInMeters=as.numeric(coordinateUncertaintyInMeters))

## sort before removing duplicates --
# whatever you sort to the top will be kept when there is a duplicate further down;
# you can comment out any of these sorting steps, or add others...
  ## sort by basis of record
unique(geo_pts$basisOfRecord)
geo_pts$basisOfRecord <- factor(geo_pts$basisOfRecord,
  levels = c("PRESERVED_SPECIMEN","MATERIAL_SAMPLE","MATERIAL_CITATION",
             "OBSERVATION","HUMAN_OBSERVATION","OCCURRENCE","PHYSICAL_SPECIMEN",
             "MACHINE_OBSERVATION","FOSSIL_SPECIMEN","LIVING_SPECIMEN",
             "UNKNOWN"))
geo_pts <- geo_pts %>% arrange(basisOfRecord)
  ## sort by establishment means
unique(geo_pts$establishmentMeans)
geo_pts$establishmentMeans <- factor(geo_pts$establishmentMeans,
  levels = c("NATIVE","WILD","UNCERTAIN","INTRODUCED","MANAGED","CULTIVATED",
             "DEAD"))
geo_pts <- geo_pts %>% arrange(establishmentMeans)
  ## sort by coordinate uncertainty
geo_pts$coordinateUncertaintyInMeters <-
  as.numeric(geo_pts$coordinateUncertaintyInMeters)
geo_pts <- geo_pts %>% arrange(geo_pts$coordinateUncertaintyInMeters)
  ## sort by year
geo_pts <- geo_pts %>% arrange(desc(year))
  ## sort by source database
      # in the past I've sorted GBIF first since it's easier to cite, but you
      #   may want NorthAm_herbaria first since it has a nice link to the 
      #   herbariun specimen, which experts can use to review the record
unique(geo_pts$database)
geo_pts$database <- factor(geo_pts$database,
  levels = c("NorthAm_herbaria","iDigBio","GBIF","FIA","IUCN_RedList",
             "BIEN","Ex_situ"))
geo_pts <- geo_pts %>% arrange(database)

## remove duplicates --
# also creating an "all_source_databases" column to capture a list of all
#   databases from which duplicates were removed; this step can take a while 
#   if there are lots a rows
geo_pts2 <- geo_pts %>%
  group_by(taxon_name_accepted,lat_round,long_round) %>%
  mutate(all_source_databases = paste(unique(database), collapse = ', ')) %>%
  distinct(taxon_name_accepted,lat_round,long_round,.keep_all=T) %>%
  ungroup() %>%
  select(-flag)
nrow(geo_pts2)

# add ex situ data back in
geo_pts2 <- full_join(geo_pts2,ex_situ)
nrow(geo_pts2)

## set header/column name order and keep only necessary columns
keep_col <- c( 
  #universal ID and source database
  "UID","database","all_source_databases",
  #taxon
  "taxon_name_accepted","taxon_name_status",
  "taxon_name","scientificName","genus","specificEpithet",
  "taxonRank","infraspecificEpithet","taxonIdentificationNotes",
  "all_native_dist_iso2",
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
  "latlong_countryCode",
  #additional optional taxa metadata
  "rl_category","ns_rank")
geo_pts2 <- geo_pts2[,keep_col]

# take a look
head(as.data.frame(geo_pts2))
nrow(geo_pts2)
table(geo_pts2$database)
rm(geo_pts)

# write file if you'd like; not necessary since we write taxon-level files
#write.csv(geo_pts2, file.path(main_dir,occ_dir,standardized_occ,
#  paste0("all_occurrence_points_compiled_", Sys.Date(), ".csv")), row.names = F)

################################################################################
# Create taxon-level summary table
################################################################################

# summarize results for each target taxon
  # count lat-long records
count_geo <- geo_pts2 %>% count(taxon_name_accepted)
names(count_geo)[2] <- "num_latlong_records"
  # count records with invalid lat-long but have locality description
count_locality <- locality_pts %>% count(taxon_name_accepted)
names(count_locality)[2] <- "num_locality_records"
  # count records that fell outside country feature boundaries
count_water<- water_pts %>% count(taxon_name_accepted)
names(count_water)[2] <- "num_water_records"
  # make table
pieces <- list(count_geo,count_locality,count_water,
              data.frame(taxon_name_accepted=unique(taxon_list$taxon_name_accepted)))
summary <- Reduce(full_join, pieces)
summary <- summary %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  arrange(!is.na(num_latlong_records), num_latlong_records)
head(as.data.frame(summary))
  # write file
write.csv(summary, file.path(main_dir,occ_dir,standardized_occ,
  paste0("summary_of_occurrences_", Sys.Date(), ".csv")),row.names = F)

################################################################################
# Split by taxa to save
################################################################################

# split records to create one CSV for each target taxon
sp_split <- split(geo_pts2, as.factor(geo_pts2$taxon_name_accepted))
names(sp_split) <- gsub(" ","_",names(sp_split))
names(sp_split) <- gsub("\\.","",names(sp_split))

# write files
lapply(seq_along(sp_split), function(i) write.csv(sp_split[[i]],
  file.path(main_dir,occ_dir,standardized_occ,data_out,
  paste0(names(sp_split)[[i]],".csv")),row.names = F))
