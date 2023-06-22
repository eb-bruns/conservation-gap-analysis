### 5-flag_occurrence_points.R
### Authors: Emily Beckman Bruns & Shannon M Still
### Supporting institutions: The Morton Arboretum, Botanic Gardens Conservation 
#   International-US, United States Botanic Garden, San Diego Botanic Garden,
#   Missouri Botanical Garden, UC Davis Arboretum & Botanic Garden
### Funding: Base script funded by the Institute of Museum and Library 
#   Services (IMLS MFA program grant MA-30-18-0273-18 to The Morton Arboretum).
#   Moderate edits were added with funding from a cooperative agreement
#   between the United States Botanic Garden and San Diego Botanic Garden
#   (subcontracted to The Morton Arboretum), and NSF ABI grant #1759759
### Last Updated: June 2023 ; first written Feb 2020
### R version 4.3.0

### DESCRIPTION:
  ## This script flags potentially suspect points by adding a column for each 
  #   type of flag, where FALSE = flagged. 
  ## Much of the flagging is done through or inspired by the
  #   CoordinateCleaner package, which was created for "geographic cleaning
  #   of coordinates from biologic collections...Cleaning geographic coordinates
  #   by multiple empirical tests to flag potentially erroneous coordinates, 
  #   addressing issues common in biological collection databases."
  #   See the article here:
  #   https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13152

### INPUTS:
  ## target_taxa_with_synonyms.csv
  #   List of target taxa and synonyms; see example in the "Target taxa list"
  #   tab in Gap-analysis-workflow_metadata workbook; Required columns include: 
  #   taxon_name, taxon_name_accepted, and taxon_name_status (Accepted/Synonym).
  ## taxon_points_raw (folder)
  #   Occurrence data compiled in 4-compile_occurrence_points.R
  ## world_countries_10m.shp & urban_areas_50m.shp
  #   These shapefiles were created in 1-prep_gis_layers.R

### OUTPUTS:
  ## taxon_points_ready-to-vet (folder) 
  #   For each taxon in your target taxa list, a CSV of occurrence records with 
  #   newly-added flagging columns (e.g., Asimina_triloba.csv)
  ## occurrence_record_summary_YYYY_MM_DD.csv
  #   Add to the summary table created in 4-compile_occurrence_points.R: number of
  #   flagged records in each flag column

################################################################################
# Load libraries
################################################################################

my.packages <- c('tidyverse','textclean','CoordinateCleaner','tools','terra')
  # versions I used (in the order listed above): 2.0.0, 0.9.3, 2.0-20, 4.3.0, 1.7-29
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
data_out <- "taxon_points_ready-to-vet"
if(!dir.exists(file.path(main_dir,occ_dir,standardized_occ,data_out)))
  dir.create(file.path(main_dir,occ_dir,standardized_occ,data_out), 
             recursive=T)

# assign folder where you have input data (saved in 4-compile_occurrence_points.R)
data_in <- "taxon_points_raw"

################################################################################
# Read in data
################################################################################

# read in target taxa list
taxon_list <- read.csv(file.path(main_dir, taxa_dir,"target_taxa_with_synonyms.csv"),
                       header=T, colClasses="character",na.strings=c("","NA"))

# read in world countries layer created in 1-prep_gis_layers.R
world_polygons <- vect(file.path(main_dir,gis_dir,"world_countries_10m",
                             "world_countries_10m.shp"))

# read in urban areas layer created in 1-prep_gis_layers.R
urban_areas <- vect(file.path(main_dir,gis_dir,"urban_areas_50m",
                              "urban_areas_50m.shp"))

################################################################################
# Iterate through taxon files and flag potentially suspect points
################################################################################

# list of taxon files to iterate through
taxon_files <- list.files(path=file.path(main_dir,occ_dir,standardized_occ,data_in), 
                          ignore.case=FALSE, full.names=FALSE, recursive=TRUE)
target_taxa <- file_path_sans_ext(taxon_files)

# start a table to add summary of results for each species
summary_tbl <- data.frame(
  taxon_name_accepted = "start", 
  unflagged_pts = "start", 
  selected_pts = "start", 
  .cen = "start", 
  .urb = "start",
  .inst = "start",
  .con = "start", 
  .outl = "start", 
  .nativectry = "start", 
  .yr1950 = "start", 
  .yr1980 = "start",
  .yrna = "start", 
    stringsAsFactors=F)


## iterate through each species file to flag suspect points...

for (i in 1:length(target_taxa)){

  taxon_file <- target_taxa[i]
  taxon_nm <- mgsub(taxon_file, c("_","_var_","_subsp_"), 
                                c(" "," var. "," subsp. "))

  # bring in records
  taxon_now <- read.csv(file.path(main_dir,occ_dir,standardized_occ,data_in,
    paste0(taxon_file, ".csv")))
  # print the taxon name we're working with
  cat("-----\n","Starting ", taxon_nm, ", taxon ", i, " of ", length(target_taxa), ".\n", sep="")
  
  
  # now we will go through a set of tests to flag potentially suspect records...

  
  ### FLAG RECORDS THAT HAVE COORDINATES NEAR COUNTRY AND STATE/PROVINCE CENTROIDS
  flag_cen <- CoordinateCleaner::cc_cen(
    taxon_now,
    lon = "decimalLongitude", lat = "decimalLatitude",
    # buffer = radius around country/state centroids (meters); default=1000
    buffer = 500, value = "flagged")
  taxon_now$.cen <- flag_cen
  
  
  ## FLAG RECORDS THAT HAVE COORDINATES IN URBAN AREAS
  if(nrow(taxon_now)<2){
    taxon_now$.urb <- NA
    print("Taxa with fewer than 2 records will not be tested.")
  } else {
    taxon_now <- as.data.frame(taxon_now)
    flag_urb <- CoordinateCleaner::cc_urb(
      taxon_now,
      lon = "decimalLongitude",lat = "decimalLatitude",
      ref = urban_areas, value = "flagged")
    taxon_now$.urb <- flag_urb
  }
  
  
  ### FLAG RECORDS THAT HAVE COORDINATES NEAR BIODIVERSITY INSTITUTIONS
  flag_inst <- CoordinateCleaner::cc_inst(
    taxon_now,
    lon = "decimalLongitude", lat = "decimalLatitude",
    # buffer = radius around biodiversity institutions (meters); default=100
    buffer = 100, value = "flagged")
  taxon_now$.inst <- flag_inst
  
  
  ### COMPARE THE COUNTRY LISTED IN THE RECORD VS. THE LAT-LONG COUNTRY; flag
  #   when there is a mismatch; CoordinateCleaner package has something like 
  #   this but also flags when the record doesn't have a country..didn't love that
  taxon_now <- taxon_now %>% mutate(.con=(ifelse(
    (as.character(latlong_countryCode) == as.character(countryCode_standard) &
       !is.na(latlong_countryCode) & !is.na(countryCode_standard)) |
      is.na(latlong_countryCode) | is.na(countryCode_standard), TRUE, FALSE)))
  cat("Testing country listed\n",sep="")
  cat("Flagged ",length(taxon_now$.con[FALSE])," records.\n",sep="")
  
  
  ### FLAG SPATIAL OUTLIERS
  taxon_now <- as.data.frame(taxon_now)
  flag_outl <- CoordinateCleaner::cc_outl(
    taxon_now,
    lon = "decimalLongitude",lat = "decimalLatitude",
    species = "taxon_name_accepted", 
    # read more about options for the method and the multiplier:
    #   https://www.rdocumentation.org/packages/CoordinateCleaner/versions/2.0-20/topics/cc_outl
    # if you make the multiplier larger, it will flag less points.
    # the default is 5; you may need to experiment a little to see what works
    #   best for most of your target taxa (script #6 helps you view flagged pts)
    method = "quantile", mltpl = 7, 
    value = "flagged")
  taxon_now$.outl <- flag_outl
  
  
  ### CHECK LAT-LONG COUNTRY AGAINST "ACCEPTED" NATIVE COUNTRY DISTRUBUTION; 
  #   flag when the lat-long country is not in the list of native countries;
  #   we use the native countries compiled in 1-get_taxa_metadata.R, which
  #   combines the IUCN Red List, BGCI GlobalTreeSearch, and manually-added data
  native_ctrys <- unique(unlist(strsplit(taxon_now$all_native_dist_iso2, "; ")))
  if(!is.na(native_ctrys[1])){
  # flag records where native country doesn't match record's coordinate location
    taxon_now <- taxon_now %>% 
      mutate(.nativectry=(ifelse(latlong_countryCode %in% native_ctrys, 
                                 TRUE, FALSE)))
  } else {
    taxon_now$.nativectry <- NA
  }
  cat("Testing native countries\n",sep="")
  cat("Flagged ",length(taxon_now$.nativectry[FALSE])," records.\n",sep="")

  
  ### FLAG OLDER RECORDS, based on two different year cutoffs (1950 & 1980)
    # 1950 cutoff
  taxon_now <- taxon_now %>% mutate(.yr1950=(ifelse(
    (as.numeric(year)>1950 | is.na(year)), TRUE, FALSE)))
  cat("Testing year < 1950\n",sep="")
  cat("Flagged ",length(taxon_now$.yr1980[FALSE])," records.\n",sep="")
    # 1980 cutoff
  taxon_now <- taxon_now %>% mutate(.yr1980=(ifelse(
    (as.numeric(year)>1980 | is.na(year)), TRUE, FALSE)))
  cat("Testing year < 1980\n",sep="")
  cat("Flagged ",length(taxon_now$.yr1980[FALSE])," records.\n",sep="")
  
  
  ### FLAG RECORDS THAT DON'T HAVE A YEAR PROVIDED
  taxon_now <- taxon_now %>% mutate(.yrna=(ifelse(
    !is.na(year), TRUE, FALSE)))
  cat("Testing year NA\n",sep="")
  cat("Flagged ",length(taxon_now$.yrna[FALSE])," records.\n\n",sep="")
  
  
  # create some subsets to count how many records are in each, for summary table...

  # count of completely unflagged points
  total_unflagged <- taxon_now %>%
    filter(.cen & .urb & .inst & .con & .outl & .yr1950 & .yr1980 & .yrna &
             (.nativectry | is.na(.nativectry)) &
             basisOfRecord != "FOSSIL_SPECIMEN" & 
             basisOfRecord != "LIVING_SPECIMEN" &
             establishmentMeans != "INTRODUCED" & 
             establishmentMeans != "MANAGED" &
             establishmentMeans != "CULTIVATED"
    )
  
  # OPTIONAL count of unflagged points based on selected filters you'd like 
  #   to use, to see how many points there are; change as desired; commented- 
  #   out lines are the filters we don't want to use
  select_unflagged <- taxon_now %>%
    filter(
            .cen & 
            .inst & 
            .outl &
            .con & 
            #.urb & 
            .yr1950 & 
            #.yr1980 & 
            #.yrna &
            (.nativectry | is.na(.nativectry)) &
            basisOfRecord != "FOSSIL_SPECIMEN" &
            basisOfRecord != "LIVING_SPECIMEN" &
            establishmentMeans != "INTRODUCED" &
            establishmentMeans != "MANAGED" &
            establishmentMeans != "INVASIVE" &
            establishmentMeans != "CULTIVATED"
    )
  
  # add data to summary table
  summary_add <- data.frame(
    taxon_name_accepted = taxon_nm,
    unflagged_pts = nrow(total_unflagged),
    selected_pts = nrow(select_unflagged),
    .cen = sum(!taxon_now$.cen),
    .urb = sum(!taxon_now$.urb),
    .inst = sum(!taxon_now$.inst),
    .con = sum(!taxon_now$.con),
    .outl = sum(!taxon_now$.outl),
    .nativectry = sum(!taxon_now$.nativectry),
    .yr1950 = sum(!taxon_now$.yr1950),
    .yr1980 = sum(!taxon_now$.yr1980),
    .yrna = sum(!taxon_now$.yrna),
    stringsAsFactors=F)
  summary_tbl[i,] <- summary_add

  # WRITE NEW FILE
  write.csv(taxon_now, file.path(main_dir,occ_dir,standardized_occ,data_out,
    paste0(taxon_file, ".csv")), row.names=FALSE)

}

# add summary of points to summary we created in 4-compile_occurrence_points.R
file_nm <- list.files(path = file.path(main_dir,occ_dir,standardized_occ),
                      pattern = "summary_of_occurrences", full.names = T)
orig_summary <- read.csv(file_nm, colClasses = "character")
summary_tbl2 <- full_join(orig_summary,summary_tbl,by="taxon_name_accepted")
summary_tbl2

# write summary table
write.csv(summary_tbl2, file.path(main_dir,occ_dir,standardized_occ,
  paste0("summary_of_occurrences_", Sys.Date(), ".csv")),row.names = F)
