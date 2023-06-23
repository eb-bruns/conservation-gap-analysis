### 3-get_occurrence_data.R
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
  ## This script provides instructions and code chunks for downloading and
  #   creating standard column names for wild occurrence points from:
  # 1- GLOBAL DATABASES:
  #     A) Global Biodiversity Information Facility (GBIF)
  #     B) Integrated Digitized Biocollections (iDigBio)
  #     C) IUCN Red List of Threatened Species
  #     D) Regional network of North American herbaria (SEINet Portal Network)
  #     E) Botanical Information and Ecology Network (BIEN)
  # 2- U.S. NATIONAL DATABASES:
  #     F) Forest Inventory and Analysis (FIA) Program of USDA Forest Service
  #     *Note that BISON data used to be downloaded as well, but as of 2021
  #        it has now been subsumed under GBIF.us, which is part of GBIF
  # 3- EX SITU ACCESSION-LEVEL WILD COLLECTION LOCATION
  #     G) If you ran script 2-compile_exsitu_data.R, this script can be 
  #        used to prep wild collection location lat-longs to be used within the
  #        in situ occurrence data as well.
  # 4- OTHER SOURCES GATHERED AND STANRDARDIZED MANUALLY
  #     H) You can add other occurrence point data (e.g., expert comment, 
  #        NatureServe, regional floras, USDA PLANTS, BONAP, etc.) by 
  #        standardizing column names and formatting to match the schema in the 
  #        "Raw occurrence data schema" tab of Gap-analysis-workflow_metadata 
  #        workbook, then save as a CSV in 
  #        occurrence_data > standardized_occurrence_data folder
  ## Each of the lettered sources above (A-H) is optional - you can pick and
  #   choose which sections of the script to run; they all stand alone.
  ## Please note that not all data from these sources are reliable. The aim of 
  #   this script is to get all easily-downloadable occurrence data, which
  #   can then be sorted and vetted for the user's specific purposes.
  ## Also note that each database has it's own citation guidelines; please 
  #   review them prior to using the data. Information has been compiled in the
  #   "Occurrence data citation guidance" tab of Gap-analysis-workflow_metadata
  #   workbook.
  ## You can see an overview of which columns are used from each database and
  #   how they are standardized, in the "Raw occurrence data schema" tab of 
  #   Gap-analysis-workflow_metadata workbook.

### INPUTS:
  ## (optional) target_taxa_with_synonyms.csv
  #   List of target taxa and synonyms; see example in the "Target taxa list"
  #   tab in Gap-analysis-workflow_metadata workbook; Required columns include: 
  #   taxon_name, taxon_name_accepted, and taxon_name_status (Accepted/Synonym).
  ## You can also create this by hand if you have a short list.

### OUTPUTS:
  ## All outputs are optional; you simply run whichever sections you want, and 
  #   each produces an output. The raw data is downloaded to the corresponding
  #   folder in occurrence_data > raw_occurrence_data, then a file with 
  #   standardized columns is saved to standardized_occurrence_data > input_datasets
  #   folder and named like so:
  # A) gbif.csv
  # B) idigbio.csv
  # C) redlist.csv
  # D) seinet.csv
  # E) bien.csv
  # F) fia.csv
  # G) exsitu.csv
  # [additional files if added manually via instructions in section 4H above]

################################################################################
# Load libraries
################################################################################

my.packages <- c('tidyverse','textclean','data.table','rgbif','ridigbio','BIEN')
  # versions I used (in the order listed above): 2.0.0, 0.9.3, 1.14.8, 3.7.7, 0.3.6, 1.2.6
# install.packages(my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
    rm(my.packages)

################################################################################
# Set working directory
################################################################################

# use 0-set_working_directory.R script
  # change this path based on where the script is located on your computer:
source("/Users/emily/Documents/GitHub/conservation-gap-analysis/spatial-analysis-workflow/0-set_working_directory.R")

# create folder for output data
if(!dir.exists(file.path(main_dir,occ_dir,standardized_occ,"input_datasets")))
  dir.create(file.path(main_dir,occ_dir,standardized_occ,"input_datasets"), 
             recursive=T)
data_out <- "input_datasets"
    
################################################################################
# Load or create target taxa list
################################################################################

# read in taxa list
taxon_list <- read.csv(file.path(main_dir,taxa_dir,"target_taxa_with_synonyms.csv"),
                       header=T, colClasses="character",na.strings=c("","NA"))
head(taxon_list); nrow(taxon_list)

# list of target taxon names
taxon_names <- sort(taxon_list$taxon_name)

# create list of species names as well, to do initial removal of non-target 
#   taxa when downloading at the genus level; full name match in next script
target_sp_names <- unique(sapply(taxon_list$taxon_name, function(x)
  unlist(strsplit(x," var. | subsp. | f. "))[1]))

### you can also create a target taxa list by hand instead...
  # include synonyms if you want to find them:
#taxon_names <- c("Asimina incana","Asimina longifolia","Asimina triloba")
  # if you have any infrataxa, just keep the genus and specific epithet here:
#target_sp_names <- c("Asimina incana","Asimina longifolia","Asimina triloba")

################################################################################
# Download & do basic standardization of occurrence data from each database
################################################################################

## You can pick and choose any/all sections below (A-F), depending on
#  which databases you'd like to use
## Some sections have two options for downloading data: manually via
#  the website, or automatically using the API; choose whichever works for you
## Note that if you have a long taxa list, the downloaded data may be very
#  large; if this makes it unworkable for your computer setup, you may need to 
#  skip writing the files and move directly to 4-compile_occurrence_data.R to
#  compile and filter (edits will be needed in this script and script 4 to make 
#  that work)


###
###############
###############################################
### A) Global Biodiversity Information Facility (GBIF)
###    https://www.gbif.org
###############################################
###############
###

# create new folder if not already present
if(!dir.exists(file.path(main_dir,occ_dir,raw_occ,"GBIF")))
  dir.create(file.path(main_dir,occ_dir,raw_occ,"GBIF"), recursive=T)

###
### OPTION 1: automatic download via API
### (can go down to option 2 -manual download- if this isn't working)
###

# load GBIF account user information
  # if you don't have account yet, go to https://www.gbif.org then click
  # "Login" in top right corner, then click "Register"
# either read in a text file with username, password, and email (one on each
#   line) or manually fill in below (if you're not saving this script publicly):
login <- read_lines(log_loc)
  user  <- login[1] #username
  pwd   <- login[2] #password
  email <- login[3] #email
  rm(login)
# get GBIF taxon keys for all taxa in target list
keys <- sapply(taxon_names,function(x) name_backbone(name=x)$speciesKey,
  simplify = "array"); keys
# remove duplicate and NULL keys
keys_nodup <- keys[!duplicated(keys) & keys != "NULL"]
# create data frame of keys and matching taxon_name
gbif_codes <- map_df(keys_nodup,~as.data.frame(.x),.id="taxon_name")
names(gbif_codes)[2] <- "speciesKey"
# create vector of keys as input into gbif download
gbif_taxon_keys <- vector(mode="numeric")
for(i in 1:length(keys_nodup)){
  gbif_taxon_keys <- c(gbif_taxon_keys,keys_nodup[[i]][1])
}; sort(gbif_taxon_keys)
  rm(i)
# download GBIF data (Darwin Core Archive format)
gbif_download <- occ_download(
                pred_in("taxonKey", gbif_taxon_keys),
                  ## you can also add additional filters:
                #pred_in("basisOfRecord", c("PRESERVED_SPECIMEN",
                #    "HUMAN_OBSERVATION","FOSSIL_SPECIMEN","OBSERVATION",
                #    "UNKNOWN","MACHINE_OBSERVATION","MATERIAL_SAMPLE",
                #    "LITERATURE")),
                #pred("hasCoordinate", TRUE),
                #pred("hasGeospatialIssue", FALSE),
                  ## "SIMPLE_CSV" format is another option
                format = "DWCA",
                user=user,pwd=pwd,
                email=email)
  rm(user, pwd, email)
# load gbif data just downloaded
  # download and unzip before reading in
download_key <- gbif_download
  # must wait for download to complete before continuing;
  # it may take a while (up to 3 hours) if you have a large taxa list;
  # function below will pause script until the download is ready, though it
  #   seems like its not working quite right for me right now?
  # you can also log in on the GBIF website and go to your profile to see the
  #   progress of your download
occ_download_wait(download_key, status_ping=10, quiet=TRUE)
  # get download when its ready then unzip and read in
occ_download_get(key=download_key[1],
  path=file.path(main_dir,occ_dir,raw_occ,"GBIF"))
unzip(zipfile=paste0(
  file.path(main_dir,occ_dir,raw_occ,"GBIF",download_key[1]),".zip"),
  files="occurrence.txt",
  exdir=file.path(main_dir,occ_dir,raw_occ,"GBIF"))
  # create file list for next step (standardizing)
file_list <- list.files(path = file.path(main_dir,occ_dir,raw_occ,"GBIF"), 
                        pattern = "occurrence", full.names = T)
length(file_list) #1

# !!! NOW SCROLL DOWN TO "STANDARDIZE THE DATA" SECTION !!!

###
### OPTION 2: manual download from website
###

## First, download raw data:
# Go to https://www.gbif.org/occurrence/search
# Note that you need an account and to be logged in.
# You can query by genus or scientific name, and use other filters as desired.
# Remember that you need to cite these data using the DOI provided with the
#   download!
# Place all downloaded data (single or multiple) in the folder:
#   occurrence_data > raw_occurrence_data > GBIF
# Open the folder(s) you just added and pull the occurrence.txt file out into
#   the GBIF folder

# get path(s) to raw data; we will then loop through each file to standardize it
file_list <- list.files(path = file.path(main_dir,data,raw,"GBIF"),
  pattern = "occurrence", full.names = T)
length(file_list)

###
### STANDARDIZE THE DATA
### Run this section no matter which option your chose for getting the data !!
###

# loop through file(s) [you may have more than one if you downloaded manually]
for(i in 1:length(file_list)){
  # read in data
  gbif_raw <- fread(file_list[[i]], quote="", na.strings="")
  print(paste0("Total number of records: ",nrow(gbif_raw)))
  # remove any genus-level records
  gbif_raw <- gbif_raw %>% filter(taxonRank != "GENUS")
  # keep only necessary columns
  gbif_raw <- gbif_raw %>% select(
      # Taxon
    "scientificName","family","genus","specificEpithet","taxonRank",
    "infraspecificEpithet",
        # *concatenate into taxonIdentificationNotes
    "identificationRemarks","identificationVerificationStatus","identifiedBy",
    "taxonRemarks",
      # Event
    "year",
      # Record-level
    "basisOfRecord","gbifID","datasetName","publisher","institutionCode",
    "rightsHolder","license","references","informationWithheld","issue",
      # Occurrence
    "recordedBy","establishmentMeans",
      # Location
    "decimalLatitude","decimalLongitude","coordinateUncertaintyInMeters",
    "county","municipality","stateProvince","higherGeography","countryCode",
        # *concatenate into golocationNotes
    "georeferencedBy","georeferencedDate","georeferenceProtocol",
    "georeferenceRemarks","georeferenceSources",
    "georeferenceVerificationStatus",
        # *concatenate into locationNotes
    "locality","verbatimLocality","associatedTaxa","eventRemarks","fieldNotes",
    "habitat","locationRemarks","occurrenceRemarks","occurrenceStatus"
  ) %>% rename(nativeDatabaseID = gbifID)
  # add database column
  gbif_raw$database <- "GBIF"
  # combine a few similar columns
  gbif_raw <- gbif_raw %>% unite("taxonIdentificationNotes",
      identificationRemarks:taxonRemarks,na.rm=T,remove=T,sep=" | ")
    gbif_raw$taxonIdentificationNotes <-
      gsub("^$",NA,gbif_raw$taxonIdentificationNotes)
  gbif_raw <- gbif_raw %>% unite("locationNotes",
    associatedTaxa:occurrenceStatus,na.rm=T,remove=T,sep=" | ")
    gbif_raw$locationNotes <- gsub("^$",NA,gbif_raw$locationNotes)
  gbif_raw <- gbif_raw %>% unite("geolocationNotes",
    georeferencedBy:georeferenceVerificationStatus,na.rm=T,remove=T,sep=" | ")
    gbif_raw$geolocationNotes <- gsub("^$",NA,gbif_raw$geolocationNotes)
  # create taxon_name column
  gbif_raw$taxon_name <- NA
  subsp <- gbif_raw %>% filter(taxonRank == "SUBSPECIES")
    subsp$taxon_name <- paste(subsp$genus,subsp$specificEpithet,"subsp.",
      subsp$infraspecificEpithet)
  var <- gbif_raw %>% filter(taxonRank == "VARIETY")
    var$taxon_name <- paste(var$genus,var$specificEpithet,"var.",
      var$infraspecificEpithet)
  form <- gbif_raw %>% filter(taxonRank == "FORM")
    form$taxon_name <- paste(form$genus,form$specificEpithet,"f.",
      form$infraspecificEpithet)
  spp <- gbif_raw %>% filter(taxonRank == "SPECIES")
    spp$taxon_name <- paste(spp$genus,spp$specificEpithet)
  gbif_raw <- Reduce(rbind,list(subsp,var,form,spp))
    rm(subsp,var,form,spp)
  # fix hybrid names as needed (I don't think any were found)
  #print(sort(unique(gbif_raw$taxon_name)))
  gbif_raw$taxon_name <- mgsub(gbif_raw$taxon_name,
    c("Asimina xnashii"),
    c("Asimina x nashii"))
  # create species_name column
  gbif_raw$species_name <- NA
  gbif_raw$species_name <- sapply(gbif_raw$taxon_name, function(x)
    unlist(strsplit(x," var. | subsp. | f. "))[1])
  # keep only target species
  gbif_raw2 <- gbif_raw %>%
    filter(species_name %in% target_sp_names)
  print(paste0("Number of records for target taxa: ",nrow(gbif_raw)))
  print("Species removed; if you want to keep any of these, add them to your target taxa list...")
  print(unique(setdiff(gbif_raw,gbif_raw2)[,"taxon_name"]))
  # check a few standards and recode if needed
    # establishmentMeans
      # check and add below as needed
  print(sort(unique(gbif_raw2$establishmentMeans))) 
  gbif_raw2 <- gbif_raw2 %>%
    mutate(establishmentMeans = recode(establishmentMeans,
      "Uncertain" = "UNCERTAIN",
      "Native" = "NATIVE",
      "Introduced" = "INTRODUCED"))
  # write file
  write.csv(gbif_raw2, file.path(main_dir,occ_dir,standardized_occ,data_out,
    paste0("gbif",i,".csv")),row.names=FALSE)
  rm(gbif_raw); rm(gbif_raw2)
}



###
###############
###############################################
# B) Integrated Digitized Biocollections (iDigBio)
#    https://www.idigbio.org/portal/search
###############################################
###############
###

# create new folder if not already present
if(!dir.exists(file.path(main_dir,occ_dir,raw_occ,"iDigBio")))
  dir.create(file.path(main_dir,occ_dir,raw_occ,"iDigBio"), recursive=T)

###
### OPTION 1: automatic download via API
### (can go down to option 2 -manual download- if this isn't working;
###  also, all fields are not downloaded via the API... you can use
###  manual download method if you want *everything* that's available)
###

# download iDigBio data for target taxa
  # we have to go taxon by taxon; function can only return 100,000 records at
  # once and e.g. Quercus has more than that so can't download by genus
idigbio_raw <- data.frame()
for(i in 1:length(taxon_names)){
  output_new <- idig_search_records(rq=list(scientificname=taxon_names[[i]]),
    fields="all")
  idigbio_raw <- rbind(idigbio_raw,output_new)
  print(paste(round(i/length(taxon_names)*100,digits=1),"% complete",sep=""))
}
  rm(output_new, i)
nrow(idigbio_raw)
# remove rows that are lists
idigbio_raw <- idigbio_raw %>% dplyr::select(everything(),-commonnames,-flags,
  -mediarecords,-recordids)
# write raw file
write.csv(idigbio_raw,file.path(main_dir,occ_dir,raw_occ,"iDigBio",
                                "idigbio_R_download.csv"),row.names=FALSE)
# standardize a few fields:
# split date collected column to just get year
idigbio_raw <- idigbio_raw %>% separate("eventdate","year",sep="-",remove=T)
idigbio_raw$year <- gsub("[[:digit:]]+/[[:digit:]]+/","",idigbio_raw$year)
  sort(unique(idigbio_raw$year))
# keep only necessary columns & rename to fit standard
idigbio_raw <- idigbio_raw %>%
  select("scientificname","family","genus","specificepithet","taxonrank",
         "infraspecificepithet","year","basisofrecord","uuid","institutioncode",
         "occurrenceid","collectioncode","individualcount","geopoint.lon",
         "geopoint.lat","coordinateuncertainty","locality","verbatimlocality",
         "municipality","county","stateprovince","country","countrycode") %>%
  rename("scientificName" = "scientificname",
         "specificEpithet" = "specificepithet",
         "taxonRank" = "taxonrank",
         "infraspecificEpithet" = "infraspecificepithet",
         "decimalLongitude" = "geopoint.lon",
         "decimalLatitude" = "geopoint.lat",
         "coordinateUncertaintyInMeters" = "coordinateuncertainty",
         "basisOfRecord" = "basisofrecord",
         "nativeDatabaseID" = "uuid",
         "references" = "occurrenceid",
         "verbatimLocality" = "verbatimlocality",
         "stateProvince" = "stateprovince",
         "countryCode" = "countrycode",
         "institutionCode" = "institutioncode",
         "individualCount" = "individualcount")
idigbio_raw$datasetName <- idigbio_raw$institutionCode
idigbio_raw$rightsHolder <- idigbio_raw$institutionCode
idigbio_raw$establishmentMeans <- "UNCERTAIN"
# create taxon_name column
idigbio_raw$taxon_name <- str_to_sentence(idigbio_raw$scientificName)
sort(unique(idigbio_raw$taxon_name))
# fix name capitalization
idigbio_raw$family <- str_to_title(idigbio_raw$family)
idigbio_raw$genus <- str_to_title(idigbio_raw$genus)

# !!! NOW SCROLL DOWN TO "STANDARDIZE THE DATA" SECTION !!!

###
### OPTION 2: manual download from website
###

## First, download raw data:
# Go to https://www.idigbio.org/portal/search
# Click "Add a field" dropdown on the left and select "Genus"
# Type your target genus name into the "Genus" box
# Click the "Download" tab, type in your email, and
#   click the download button (down arrow within circle).
# If you have more than one target genus, repeat the above steps for the
#   other genera.
# Your downloads will pop up in the "Downloads" section
# "Click To Download" for each
# Move all the folders you downloaded into the "iDigBio" folder
#   in occurrence_data > raw_occurrence_data
# Open each folder you just added and pull the occurrence_raw.csv file out 
#   of each and into the iDigBio folder.

# read in raw occurrence points
file_list <- list.files(path = file.path(main_dir,data,raw,"iDigBio"),
  pattern = "occurrence", full.names = T)
file_dfs <- lapply(file_list, read.csv, colClasses = "character",
  na.strings=c("","NA"),strip.white=T,fileEncoding="UTF-8")
length(file_dfs)
# stack datasets to create one dataframe
idigbio_raw <- data.frame()
for(file in seq_along(file_dfs)){
  idigbio_raw <- rbind(idigbio_raw, file_dfs[[file]])
}; nrow(idigbio_raw)
# replace prefixes in column names
colnames(idigbio_raw) <- gsub(x = colnames(idigbio_raw),
  pattern = "dwc\\.", replacement = "")
# create taxon_name column
sort(unique(idigbio_raw$taxonRank))
subspecies <- c("ssp.","subsp.","subspecies","Subspecies")
variety <- c("var.","Varietas","variety","Variety")
forma <- c("f.","fm.","form","Form","form.","forma","Forma","Subform")
species <- c("espécie","sp.","specie","species","Species","spp.")
hybrid <- c("×","x","X")
aff <- c("aff.")
  # note that it's ok for these to error "replacement has 1 row, data has 0"
subsp <- idigbio_raw %>% filter(taxonRank %in% subspecies)
  subsp$taxon_name <- paste(subsp$genus,subsp$specificEpithet,"subsp.",
    subsp$infraspecificEpithet)
var <- idigbio_raw %>% filter(taxonRank %in% variety)
  var$taxon_name <- paste(var$genus,var$specificEpithet,"var.",
    var$infraspecificEpithet)
form <- idigbio_raw %>% filter(taxonRank %in% forma)
  form$taxon_name <- paste(form$genus,form$specificEpithet,"f.",
    form$infraspecificEpithet)
spp <- idigbio_raw %>% filter(taxonRank %in% species)
  spp$taxon_name <- paste(spp$genus,spp$specificEpithet)
h <- idigbio_raw %>% filter(taxonRank %in% hybrid)
  h$taxon_name <- paste(h$genus,"x",h$specificEpithet)
a <- idigbio_raw %>% filter(taxonRank %in% aff)
  a$taxon_name <- paste(a$genus,"aff.",a$specificEpithet)
idigbio_raw <- Reduce(bind_rows,list(subsp,var,form,spp,h,a))
# combine a few similar columns
idigbio_raw <- idigbio_raw %>% unite("taxonIdentificationNotes",
    c(identificationID:identifiedBy,taxonRemarks),na.rm=T,remove=T,sep=" | ")
  idigbio_raw$taxonIdentificationNotes <-
    gsub("^$",NA,idigbio_raw$taxonIdentificationNotes)
idigbio_raw <- idigbio_raw %>% unite("locationNotes",
  c(associatedTaxa,eventRemarks,fieldNotes,habitat,locationRemarks,
    occurrenceRemarks,occurrenceStatus),na.rm=T,remove=T,sep=" | ")
  idigbio_raw$locationNotes <- gsub("^$",NA,idigbio_raw$locationNotes)
idigbio_raw <- idigbio_raw %>% unite("geolocationNotes",
  georeferenceProtocol:georeferencedDate,na.rm=T,remove=T,sep=" | ")
  idigbio_raw$geolocationNotes <- gsub("^$",NA,idigbio_raw$geolocationNotes)
# split date collected column to just get year
idigbio_raw <- idigbio_raw %>% separate("eventDate","year",sep="-",remove=T)
idigbio_raw$year <- gsub("[[:digit:]]+/[[:digit:]]+/","",idigbio_raw$year)
idigbio_raw$year <- gsub("^[1-9][0-9][0-9][0-9]/","",idigbio_raw$year)
idigbio_raw$year <- gsub("^[0-9][0-9]/","",idigbio_raw$year)
idigbio_raw$year <- gsub("/","",idigbio_raw$year)
idigbio_raw$year <- as.numeric(idigbio_raw$year)
  sort(unique(idigbio_raw$year))
# keep only necessary columns & rename to fit standard
idigbio_raw <- idigbio_raw %>%
  select(
    "taxon_name","scientificName","taxonIdentificationNotes",
    "family","genus","specificEpithet","taxonRank","infraspecificEpithet",
    "year","basisOfRecord","coreid","institutionCode","datasetName",
    "rightsHolder","dcterms.license","dcterms.references","informationWithheld",
    "recordedBy","establishmentMeans","individualCount","decimalLongitude",
    "decimalLatitude","coordinateUncertaintyInMeters",
    "geolocationNotes","locality","verbatimLocality","locationNotes",
    "county","municipality","higherGeography","stateProvince","country",
    "countryCode") %>%
  rename("license" = "dcterms.license",
         "references" = "dcterms.references",
         "nativeDatabaseID" = "coreid")
# create species_name column
idigbio_raw$species_name <- NA
idigbio_raw$species_name <- sapply(idigbio_raw$taxon_name, function(x)
  unlist(strsplit(x," var. | subsp. | f. "))[1])
# keep only target species
idigbio_raw2 <- idigbio_raw %>%
  filter(species_name %in% target_sp_names)
print("Species removed; if you want to keep any of these, add them to your target taxa list...")
unique(setdiff(idigbio_raw,idigbio_raw2)[,"taxon_name"])
idigbio_raw <- idigbio_raw2; rm(idigbio_raw2)

###
### STANDARDIZE THE DATA
### Run this section no matter which option your chose for getting the data !!
###

# add database column
idigbio_raw$database <- "iDigBio"
# check a few standards and recode if needed
  # establishmentMeans
idigbio_raw$establishmentMeans <- str_to_upper(idigbio_raw$establishmentMeans)
unique(idigbio_raw$establishmentMeans) # check and add below as needed
idigbio_raw <- idigbio_raw %>%
  mutate(establishmentMeans = recode(establishmentMeans,
    "WILD: NATIVE/NATURALIZED" = "NATIVE",
    "WILD." = "NATIVE",
    "WILD" = "NATIVE",
    "WILD COLLECTION" = "NATIVE",
    "WILD CAUGHT" = "NATIVE",
    "LAWN DECORATION" = "CULTIVATED",
    "CAPTIVE" = "CULTIVATED",
    "SHADE TREE PLANTED" = "CULTIVATED",
    "CULTIVATED." = "CULTIVATED",
    "MANAGED OR ESCAPED" = "MANAGED",
    "ESCAPED" = "INTRODUCED",
    "ALIEN" = "INTRODUCED",
    "PERSISTING FROM CULTIVATION" = "INTRODUCED",
    .default = "UNCERTAIN",
    .missing = "UNCERTAIN"))
  # basisOfRecord
idigbio_raw$basisOfRecord <- str_to_lower(idigbio_raw$basisOfRecord)
idigbio_raw$basisOfRecord <- gsub(" |_","",idigbio_raw$basisOfRecord)
unique(idigbio_raw$basisOfRecord) # check and add below as needed
idigbio_raw <- idigbio_raw %>%
  mutate(basisOfRecord = recode(basisOfRecord,
    "preservedspecimen" = "PRESERVED_SPECIMEN",
    "machineobservation" = "MACHINE_OBSERVATION",
    "humanobservation" = "HUMAN_OBSERVATION",
    "fossilspecimen" = "FOSSIL_SPECIMEN",
    "materialsample" = "MATERIAL_SAMPLE",
    "physicalspecimen" = "PHYSICAL_SPECIMEN",
    "occurrence" = "OCCURRENCE",
    .default = "UNKNOWN",
    .missing = "UNKNOWN"))
# write file
write.csv(idigbio_raw, file.path(main_dir,occ_dir,standardized_occ,data_out,
                                 "idigbio.csv"), row.names=FALSE)
rm(idigbio_raw)



###
###############
###############################################
# C) IUCN Red List of Threatened Species
#    https://www.iucnredlist.org
###############################################
###############
###

# create new folder if not already present
if(!dir.exists(file.path(main_dir,occ_dir,raw_occ,"IUCN_RedList")))
  dir.create(file.path(main_dir,occ_dir,raw_occ,"IUCN_RedList"), recursive=T)

# There is an API for the IUCN Red List ('rredlist')
#   https://cran.r-project.org/web/packages/rredlist/rredlist.pdf
# but it does not have a function for downloading point data
# so, we must download manually.

## First, download raw data:
# Go to https://www.iucnredlist.org/search
# Click "Login/Register" in top bar; create an account if you don't have one,
#   then log in to your account
# Open the "Taxonomy" tab in the left bar, type in your target genus name
#   and check the box next to the genus name when it comes up - note that
#   sometimes there is a glitch where the box is grayed out; you can navigate
#   to the genus you want by going through the tree, starting with Plantae. 
#   Remember to search for any synonym genera as well, as applicable.
# Or, alternatively, if you are just looking for a few
#   taxa you can search for them individually.
# You should be able to add each genus/taxon to your search so only
#   one file needs to be exported.
# In the far-left bar, scroll down and, if desired, check
#   "Subspecies and varieties" (if you want infrataxa).
# Click the gray "Download" button and select "Range data - Points (CSV)"
#   then fill in the prompts in the popup window.
# Go to https://www.iucnredlist.org/account to find your query
# Click "Download" next to your query
# Move the folder you downloaded into the "IUCN_RedList" folder in
#   occurrence_data > raw_occurrence_data
# Open the folder you just added and pull the points_data.csv file out into 
#   the IUCN_RedList folder

# read in data
redlist_raw <- read.csv(file.path(main_dir,occ_dir,raw_occ,
  "IUCN_RedList","points_data.csv"), colClasses = "character",
  na.strings=c("", "NA"), strip.white=T, fileEncoding="UTF-8")
nrow(redlist_raw)
# create taxon_name column
spp <- redlist_raw %>% filter(is.na(subspecies))
spp$taxon_name <- spp$sci_name
subsp <- redlist_raw %>% filter(!is.na(subspecies))
if(nrow(subsp!=0)){
  subsp$taxon_name <- paste(subsp$sci_name,"subsp.",subsp$subspecies) }
redlist_raw <- rbind(subsp,spp)
sort(unique(redlist_raw$taxon_name))
# create species_name column
redlist_raw$species_name <- NA
redlist_raw$species_name <- sapply(redlist_raw$taxon_name, function(x)
  unlist(strsplit(x," subsp. "))[1])
# keep only target species
redlist_raw <- redlist_raw %>%
  filter(species_name %in% target_sp_names)
# combine a few similar columns
redlist_raw$rightsHolder <- paste(redlist_raw$citation,redlist_raw$year)
# keep only necessary columns
redlist_raw <- redlist_raw %>%
  select(taxon_name,tax_comm,event_year,
    basisofrec,origin,latitude,longitude,dist_comm,source,
    compiler,rightsHolder,presence,subspecies,id_no)
# check a few standards and recode if needed
  # establishmentMeans
redlist_raw <- redlist_raw %>%
  mutate(origin = recode(origin,
    "1" = "NATIVE",
    "2" = "REINTRODUCED",
    "3" = "INTRODUCED",
    "4" = "VAGRANT",
    "5" = "UNCERTAIN",
    "6" = "ASSISTED_COLONISATION"))
  # issue
redlist_raw <- redlist_raw %>%
  mutate(presence = recode(presence,
    "1" = "EXTANT",
    "2" = "EXTANT",
    "3" = "POSSIBLY_EXTANT",
    "4" = "POSSIBLY_EXTINCT",
    "5" = "EXTINCT",
    "6" = "PRESENCE_UNCERTAIN")) %>%
    # remove extinct rows
  filter(presence != "EXTINCT")
  # basisOfRecord
redlist_raw$basisofrec <- str_to_lower(redlist_raw$basisofrec)
redlist_raw$basisofrec <- gsub(" |_","",redlist_raw$basisofrec)
unique(redlist_raw$basisofrec) # check and add below as needed
redlist_raw <- redlist_raw %>%
  mutate(basisofrec = recode(basisofrec,
    "humanobservation" = "HUMAN_OBSERVATION",
    "preservedspecimen" = "PRESERVED_SPECIMEN",
    "literature" = "LITERATURE",
    "expert" = "HUMAN_OBSERVATION",
    "fossilspecimen" = "FOSSIL_SPECIMEN",
    "livingspecimen" = "LIVING_SPECIMEN",
    "unknown" = "UNKNOWN",
    "liturature" = "LITERATURE",
    "materialsample" = "MATERIAL_SAMPLE",
    "observation" = "OBSERVATION",
    "specimen" = "PRESERVED_SPECIMEN",
    .missing = "UNKNOWN"))
# rename to fit standard
redlist_raw <- redlist_raw %>%
  rename(taxonIdentificationNotes = tax_comm,
         year = event_year,
         basisOfRecord = basisofrec,
         decimalLatitude = latitude,
         decimalLongitude = longitude,
         locality = dist_comm,
         datasetName = source,
         recordedBy = compiler,
         issue = presence,
         establishmentMeans = origin,
         taxonRank = subspecies,
         nativeDatabaseID = id_no,
         references = compiler)
# add database column & publisher columns
redlist_raw$database <- "IUCN_RedList"
redlist_raw$publisher <- "IUCN Red List of Threatened Species"
# write file
write.csv(redlist_raw, file.path(main_dir,occ_dir,standardized_occ,data_out,
                                 "redlist.csv"), row.names=FALSE)
rm(redlist_raw)



###
###############
###############################################
# D) Regional network of North American herbaria (SEINet Portal Network)
#    https://symbiota.org/seinet/
###############################################
###############
###

# create new folder if not already present
if(!dir.exists(file.path(main_dir,occ_dir,raw_occ,"NorthAm_herbaria")))
  dir.create(file.path(main_dir,occ_dir,raw_occ,"NorthAm_herbaria"), recursive=T)

## First, download raw data:
# Go to https://swbiodiversity.org/seinet/collections/harvestparams.php
# We will now download data for each target genus individually (if you have
#   more than one); alternatively, if you are just looking for a few
#   taxa you can search for and download them individually.
# Type your target genus name into the "Scientific Name" box and click
#   "List Display"
# Click the Download Specimen Data button (grey square with arrow pointing
#   down into a box) in the top right corner.
# IMPORTANT: In the pop-up window, select the "Darwin Core" radio button,
#   un-check everything in the "Data Extensions" section, and
#   select the "UTF-8 (unicode)" radio button; leave other fields as-is.
# Click "Download Data"
# If you have more than one target genus, repeat the above steps for the
#   other genera.
# Move all the folders you downloaded into the folder
#   occurrence data > raw_occurrence_data > NorthAm_herbaria

# read in data
# this code pull the "occurrences.csv" from each genus folder, for compilation
file_list <- list.files(file.path(main_dir,occ_dir,raw_occ,"NorthAm_herbaria"),
  pattern = "SymbOutput", full.names = T)
file_dfs <- lapply(file_list, function(i){
  read.csv(file.path(i,"occurrences.csv"), colClasses = "character",
   na.strings=c("", "NA"), strip.white=T, fileEncoding="UTF-8")})
seinet_raw <- data.frame()
for(file in seq_along(file_dfs)){
  seinet_raw <- rbind(seinet_raw, file_dfs[[file]])
}; nrow(seinet_raw)
# remove genus-level records
seinet_raw <- seinet_raw %>% filter(taxonRank != "Genus")
# create taxon_name column
  # this method is not perfect; the taxonRank isn't always categorized correctly
subsp <- seinet_raw %>% filter(taxonRank == "Subspecies")
subsp$taxon_name <- paste(subsp$genus,subsp$specificEpithet,"subsp.",
  subsp$infraspecificEpithet)
var <- seinet_raw %>% filter(taxonRank == "Variety")
var$taxon_name <- paste(var$genus,var$specificEpithet,"var.",
  var$infraspecificEpithet)
form <- seinet_raw %>% filter(taxonRank == "Form")
form$taxon_name <- paste(form$genus,form$specificEpithet,"f.",
  form$infraspecificEpithet)
spp <- seinet_raw %>% filter(is.na(taxonRank) | taxonRank == "Species" |
  taxonRank == "Subform")
  spp$taxon_name <- paste(spp$genus,spp$specificEpithet)
seinet_raw <- Reduce(bind_rows,list(subsp,var,form,spp))
seinet_raw$taxon_name[which(is.na(seinet_raw$taxon_name))] <-
  seinet_raw$scientificName[which(is.na(seinet_raw$taxon_name))]
  # check out taxon names:
sort(unique(seinet_raw$taxon_name))
  # sometimes there are strange characters we need to replace:
#seinet_raw$taxon_name <- gsub("Ã\u0097","",seinet_raw$taxon_name)
#seinet_raw$taxon_name <- gsub("Ã«","e",seinet_raw$taxon_name)
#seinet_raw$taxon_name <- str_squish(seinet_raw$taxon_name)
# keep only necessary columns & rename to fit standard
seinet_raw <- seinet_raw %>% select(
  "taxon_name","family","genus","specificEpithet","taxonRank",
  "infraspecificEpithet","scientificName","identificationRemarks",
  "identifiedBy","taxonRemarks","decimalLatitude","decimalLongitude",
  "coordinateUncertaintyInMeters","basisOfRecord","year","id",
  "references","locality","county","municipality","stateProvince",
  "country","associatedTaxa","habitat","locationRemarks",
  "occurrenceRemarks","georeferencedBy","georeferenceProtocol",
  "georeferenceRemarks","georeferenceSources",
  "georeferenceVerificationStatus","institutionCode",
  "rightsHolder","recordedBy","individualCount",
  "establishmentMeans","informationWithheld")
seinet_raw <- seinet_raw %>% rename(nativeDatabaseID = id)
# add database column & datasetName column
seinet_raw$database <- "NorthAm_herbaria"
seinet_raw$datasetName <- seinet_raw$institutionCode
# combine a few similar columns
seinet_raw <- seinet_raw %>% unite("taxonIdentificationNotes",
  identificationRemarks:taxonRemarks,na.rm=T,remove=T,sep=" | ")
  seinet_raw$taxonIdentificationNotes <-
    gsub("^$",NA,seinet_raw$taxonIdentificationNotes)
seinet_raw <- seinet_raw %>% unite("locationNotes",
  associatedTaxa:occurrenceRemarks,na.rm=T,remove=T,sep=" | ")
  seinet_raw$locationNotes <- gsub("^$",NA,seinet_raw$locationNotes)
seinet_raw <- seinet_raw %>% unite("geolocationNotes",
  georeferencedBy:georeferenceVerificationStatus,na.rm=T,remove=T,sep=" | ")
  seinet_raw$geolocationNotes <- gsub("^$",NA,seinet_raw$geolocationNotes)
# create species_name column
seinet_raw$species_name <- NA
seinet_raw$species_name <- sapply(seinet_raw$taxon_name, function(x)
  unlist(strsplit(x," var. | subsp. | f. "))[1])
# keep only target species
seinet_raw2 <- seinet_raw %>%
  filter(species_name %in% target_sp_names)
print("Species removed; if you want to keep any of these, add them to your target taxa list...")
unique(setdiff(seinet_raw,seinet_raw2)[,"taxon_name"])
seinet_raw <- seinet_raw2; rm(seinet_raw2)
# check a few standards and recode if needed
  # basisOfRecord
seinet_raw$basisOfRecord <- str_to_upper(seinet_raw$basisOfRecord)
seinet_raw$basisOfRecord <- gsub(" ","_",seinet_raw$basisOfRecord)
unique(seinet_raw$basisOfRecord) # check and add below as needed
seinet_raw <- seinet_raw %>%
  mutate(basisOfRecord = recode(basisOfRecord,
    "PRESERVEDSPECIMEN" = "PRESERVED_SPECIMEN",
    "PHYSICALSPECIMEN" = "PHYSICAL_SPECIMEN",
    "ESPÉCIMEN_PRESERVADO" = "PRESERVED_SPECIMEN",
    "EJEMPLAR_HERBORIZADO" = "PRESERVED_SPECIMEN",
    "LIVINGSPECIMEN" = "LIVING_SPECIMEN",
    "HUMANOBSERVATION" = "HUMAN_OBSERVATION",
    .missing = "UNKNOWN"))
  # establishmentMeans
seinet_raw$establishmentMeans <- str_to_upper(seinet_raw$establishmentMeans)
seinet_raw$establishmentMeans <- gsub("\\.","",seinet_raw$establishmentMeans)
sort(unique(seinet_raw$establishmentMeans))
seinet_raw <- seinet_raw %>%
  mutate(establishmentMeans = recode(establishmentMeans,
    "NATIVE" = "NATIVE",
    "INTRODUCED" = "INTRODUCED",
    "UNCERTAIN" = "UNCERTAIN",
    "ALIEN" = "INTRODUCED",
    "CLONAL" = "UNCERTAIN",
    "WILD" = "NATIVE",
    "NATURALIZED" = "INTRODUCED",
    "ESCAPE FROM CULTIVATION" = "INTRODUCED",
    "ESTABLISHED NON-NATIVE" = "INTRODUCED",
    "INTRODUCED; VOLUNTEER" = "INTRODUCED",
    "NATIVE/NATURALIZING" = "NATIVE",
    "NATURALIZED" = "INTRODUCED",
    "NON-NATIVE" = "INTRODUCED",
    "NONNATIVE" = "INTRODUCED",
    "WILD CAUGHT" = "NATIVE",
    "VOLUNTEER" = "UNCERTAIN",
    "WILD COLLECTION" = "WILD",
    .default = "CULTIVATED"))
# write file
write.csv(seinet_raw, file.path(main_dir,occ_dir,standardized_occ,data_out,
                                "seinet.csv"), row.names=FALSE)
rm(seinet_raw)



###
###############
###############################################
# E) Botanical Information and Ecology Network (BIEN)
#    https://bien.nceas.ucsb.edu/bien/
###############################################
###############
###

# create new folder if not already present
if(!dir.exists(file.path(main_dir,occ_dir,raw_occ,"BIEN")))
  dir.create(file.path(main_dir,occ_dir,raw_occ,"BIEN"), recursive=T)

# This database can only be accessed via an API (no manual download option)
# For information about the functions in their package, run the next line:
#vignette("BIEN")

# Note that some people have received an error message that they are
# unable to connect to the correct port; for example:
  #   Error in postgresqlNewConnection(drv, ...) :
  #   RS-DBI driver: (could not connect public_bien@vegbiendev.nceas.ucsb.edu:5432 on dbname "public_vegbien": could not connect to server: Connection timed out (0x0000274C/10060)
  #                   Is the server running on host "vegbiendev.nceas.ucsb.edu" (128.111.85.31) and accepting
  #                   TCP/IP connections on port 5432?
# The vignette("BIEN") says this about the issue:
## "Database connection issues Some institution and computer programs
## (e.g. some antivirus programs) block the SQL connections that this package
## relies on. While we are exploring ways around this issue, at present the
## simplest method is to use the package on a computer/network that doesn’t
## block SQL connections."

# download occurrence data for target taxa
# this can take a few minutes or more if you have many target taxa
bien_raw <- BIEN_occurrence_species(taxon_names,all.taxonomy=T,native.status=T,
  natives.only=F,observation.type=T,collection.info=T,political.boundaries=T,
  cultivated=T)
nrow(bien_raw)
write.csv(bien_raw, file.path(main_dir,occ_dir,raw_occ,"BIEN",
                              "bien_R_download.csv"), row.names=FALSE)
# get citation info and write to a file
bien_citation <- BIEN_metadata_citation(dataframe = bien_raw)
write.csv(bien_citation, file.path(main_dir,occ_dir,raw_occ,"BIEN",
                                   "citation_info.csv"), row.names=FALSE)

# for some reason there are two date_collected columns... 
  # check that columns 25 and 35 are "date_collected"; if so, remove the 2nd one:
colnames(bien_raw)
bien_raw <- bien_raw[,c(1:34,36:42)]

# OPTIONAL: remove rows from GBIF and/or FIA if you are separately downloading
# those datasets
bien_raw <- bien_raw %>% filter(datasource != "FIA" & datasource != "GBIF")
nrow(bien_raw)

# split date collected column to just get year
# first remove extra date_collected column (no idea where this comes from)
bien_raw <- bien_raw[, !duplicated(colnames(bien_raw), fromLast = TRUE)]
bien_raw <- bien_raw %>% separate("date_collected","year",sep="-",remove=T)
sort(unique(bien_raw$year))
# keep only necessary columns & rename to fit standard
bien_raw <- bien_raw %>%
  select("name_matched","verbatim_scientific_name","verbatim_family",
    "identified_by","identification_remarks","date_identified",
    "latitude","longitude","observation_type","year","record_number",
    "locality","county","state_province","country",
    "custodial_institution_codes","dataset","datasource","dataowner",
    "recorded_by","is_cultivated_observation") %>%
  rename("taxon_name" = "name_matched",
         "scientificName" = "verbatim_scientific_name",
         "family" = "verbatim_family",
         "decimalLatitude" = "latitude",
         "decimalLongitude" = "longitude",
         "basisOfRecord" = "observation_type",
         "nativeDatabaseID" = "record_number",
         "stateProvince" = "state_province",
         "institutionCode" = "custodial_institution_codes",
         "datasetName" = "dataset",
         "publisher" = "datasource",
         "rightsHolder" = "dataowner",
         "recordedBy" = "recorded_by")
# add database column
bien_raw$database <- "BIEN"
# combine a few similar columns
bien_raw <- bien_raw %>% unite("taxonIdentificationNotes",
  identified_by:date_identified,na.rm=T, remove=T, sep=" | ")
  bien_raw$taxonIdentificationNotes <-
    gsub("^$", NA, bien_raw$taxonIdentificationNotes)
# fix taxon name notation for forms
bien_raw$taxon_name <- gsub(" fo. "," f. ",bien_raw$taxon_name)
bien_raw$taxon_name <- str_squish(bien_raw$taxon_name)
# create species_name column
bien_raw$species_name <- NA
bien_raw$species_name <- sapply(bien_raw$taxon_name, function(x)
  unlist(strsplit(x," var. | subsp. | f. "))[1])
# keep only target species
bien_raw2 <- bien_raw %>%
  filter(species_name %in% target_sp_names)
print("Species removed; if you want to keep any of these, add them to your target taxa list...")
unique(setdiff(bien_raw,bien_raw2)[,"taxon_name"])
bien_raw <- bien_raw2; rm(bien_raw2)
# check a few standards and recode if needed
  # basisOfRecord
sort(unique(bien_raw$basisOfRecord))
bien_raw <- bien_raw %>%
  mutate(basisOfRecord = recode(basisOfRecord,
    "literature" = "LITERATURE",
    "plot" = "OBSERVATION",
    "specimen" = "PRESERVED_SPECIMEN",
    "trait occurrence" = "OCCURRENCE",
    "occurrence" = "OCCURRENCE",
    "checklist occurrence" = "OCCURRENCE"
  ))
  # establishmentMeans
bien_raw$is_cultivated_observation <- as.character(bien_raw$is_cultivated_observation)
bien_raw <- bien_raw %>%
  mutate(establishmentMeans = recode(is_cultivated_observation,
    "1" = "MANAGED",
    "0" = "UNCERTAIN",
    .missing = "UNCERTAIN")) %>%
  select(-is_cultivated_observation)
# write file
write.csv(bien_raw, file.path(main_dir,occ_dir,standardized_occ,data_out,
                              "bien.csv"), row.names=FALSE)
rm(bien_raw)



###
###############
###############################################
# F) USDA Forest Service, Forest Inventory and Analysis (FIA)
#    data: https://apps.fs.usda.gov/fia/datamart/datamart.html
#    metadata doc: https://www.fia.fs.usda.gov/library/database-documentation/current/ver90/FIADB%20User%20Guide%20P2_9-0-1_final.pdf
#    Note that as of June 2023 data for District of Columbia is not available.
#    There is also an rFIA package (released 2020) that could be tried- I think
#    currently it might not working due to the recent FIA 2.0 release.
###############################################
###############
###

# create new folder if not already present
if(!dir.exists(file.path(main_dir,occ_dir,raw_occ,"FIA")))
  dir.create(file.path(main_dir,occ_dir,raw_occ,"FIA"), recursive=T)

# download and read in supplemental data tables
download.file("https://apps.fs.usda.gov/fia/datamart/CSV/FIADB_REFERENCE.zip",
  file.path(main_dir,occ_dir,raw_occ,"FIA","reference_tables"))
  # species codes
unzip(zipfile = file.path(main_dir,occ_dir,raw_occ,"FIA","reference_tables"),
      files = "REF_SPECIES.csv",
      exdir = file.path(main_dir,occ_dir,raw_occ,"FIA"))
sp_codes <- read.csv(file.path(main_dir,occ_dir,raw_occ,"FIA","REF_SPECIES.csv"))
  # create taxon name and select only the columns we need
subsp <- sp_codes %>% filter(SUBSPECIES != "")
subsp$taxon_name <- paste(subsp$GENUS,subsp$SPECIES,"subsp.",subsp$SUBSPECIES)
var <- sp_codes %>% filter(VARIETY != "")
var$taxon_name <- paste(var$GENUS,var$SPECIES,"var.",var$VARIETY)
spp <- sp_codes %>% filter(SUBSPECIES == "" & VARIETY == "")
spp$taxon_name <- paste(spp$GENUS,spp$SPECIES)
sp_codes <- Reduce(bind_rows,list(subsp,var,spp))
sp_codes <- sp_codes %>% select(SPCD,taxon_name)
  # get FIA species that are in our target taxa list
taxon_fia <- sp_codes[which(sp_codes$taxon_name %in% taxon_names),]
taxon_fia
species_codes <- sort(unique(taxon_fia$SPCD))

# set up list of states and territories to cycle through (57 total)
state_abb <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID",
  "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE",
  "NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN",
  "TX","UT","VT","WV","WA","VA","WI","WY"
    # comment these out if you don't want territories:
  ,"AS","FM","GU","MP","PW","PR","VI"
)

###
### OPTION 1: download FIA data manually (~6.65 GB) and run everything locally.
### this generally works better if you have enough disk space since it avoids 
### connection issues (but, see option 2 below if you don't have disk space), 
### though it does take time to get set up initially
###

# If you have access to The Morton Arboretum GTCP Shared drive and you don't
# need a more recent version of the data, you can use the files already
# downloaded to the "FIA_data_Nov2022" folder
# Otherwise, you'll need to download things from scratch :( ...
# Create a folder called "FIA_data" on your computer
# Download the full FIA dataset (6.65 GB !!) via this link:
#   https://apps.fs.usda.gov/fia/datamart/CSV/CSV_FIADB_ENTIRE.zip
#   this can take a few hours or more if you have a slow internet connection !
# Now unzip the file and move it to your FIA_data folder
# Next, go to the FIA DataMart and download each state tree file manually:
#   https://apps.fs.usda.gov/fia/datamart/datamart.html
# To do this, click the "Select States" dropdown arrow and click "Select all"
# Then, scroll through the list and click (download) every TREE file
#   (e.g. AL_TREE.csv)... I know this process is not fun. You're not alone.
# If not every state shows in the list, you may have to select them one-by-one
#   in the dropdown menu.
# Once you've done this for every state, pat yourself on the back and
#   move them to a folder called "FIA_TREE" and move it into your FIA_data folder

# Set the working directory for the location of your FIA_data folder (change for you)
fia_dir <- "/Users/emily/Library/CloudStorage/GoogleDrive-ebeckman@mortonarb.org/Shared drives/Global Tree Conservation Program/4. GTCP_Projects/Gap Analyses/FIA_data_Nov2022"

# read in metadata tables and select relevant columns
  # plot data (has lat-long information)
fia_plots <- read.csv(file.path(fia_dir,"CSV_FIADB_ENTIRE/ENTIRE_PLOT.csv"),
                      header = T, na.strings=c("","NA"), colClasses="character")
fia_plots <- fia_plots %>% select(INVYR,STATECD,UNITCD,COUNTYCD,PLOT,LAT,LON)
  # state and county codes and names
county_codes <- read.csv(file.path(fia_dir,"CSV_FIADB_ENTIRE/ENTIRE_COUNTY.csv"),
                         header = T, na.strings=c("","NA"), colClasses="character")
county_codes <- county_codes %>% select(STATECD,COUNTYCD,COUNTYNM)

# function to extract target species data from each state CSV,
# via a local copy of the data
extract_tree_data_local <- function(state_abb){
  data <- data.frame()
  # read in tree data, which lists all species and the plots they're in;
  # this takes longer for larger states
  cat("Reading in data for",state_abb)
  state_df <- read.csv(file.path(fia_dir,"FIA_TREE",
    paste0(state_abb,"_TREE.csv")), colClasses="character")
  state_df$SPCD <- as.numeric(state_df$SPCD)
  # cycle through list of target taxon codes and extract those rows from
  # the state CSV
  for (sp in 1:length(species_codes)){
    target_sp <- state_df[which(state_df$SPCD==species_codes[[sp]]),]
    data <- rbind(data, target_sp)
  }
  # remove state file to make space for reading in the next one
  rm(state_df)
  # take a look at how much data were pulled
  cat("--> ",nrow(data)," observations.<br>")
  # keep only necessary data columns
  data_sm <- data %>% select(SPCD,INVYR,UNITCD,COUNTYCD,PLOT,STATECD,STATUSCD)
  # return reults
  return(data_sm)
  rm(sp)
}

# loop through states and pull data
fia_outputs <- lapply(state_abb, extract_tree_data_local)
length(fia_outputs)
# stack state-by-state data extracted to create one dataframe
fia_raw <- data.frame()
for(file in seq_along(fia_outputs)){
  fia_raw <- rbind(fia_raw, fia_outputs[[file]])
}; nrow(fia_raw)
# join FIA data to supplemental tables
fia_raw <- left_join(fia_raw,sp_codes)
fia_raw <- left_join(fia_raw,fia_plots)
fia_raw <- left_join(fia_raw,county_codes)
# write file of raw data
write.csv(fia_raw,file.path(main_dir,occ_dir,raw_occ,"FIA",
  "fia_extracted_local.csv"),row.names=FALSE)

# !!! NOW SCROLL DOWN TO "STANDARDIZE THE DATA" SECTION !!!

###
### OPTION 2: read each file from the web then remove before moving to the next.
### this helps alleviate disk space concerns but requires stable internet
### connection for a few hours and can be even slower (or not work at all) if 
### you don't have a fast connection
###

# set location where you want each file to be temporarily downloaded while we 
#   extract data for the target taxa
local_dir <- "./Desktop"

# function to extract target species data from each state CSV,
#   via the online FIA DataMart
extract_tree_data_web <- function(state_abb){
  data <- data.frame()
  # read in tree data, which lists all species and the plots they're in;
  # this takes longer for larger states
  cat("Downloading data for",state_abb)
  download.file(
    paste0("https://apps.fs.usda.gov/fia/datamart/CSV/",state_abb,"_TREE.csv"),
    destfile = file.path(local_dir,"fia_state_file.csv"),
      # this option sets at what time the download aborts (in seconds), so 
      #   1200 = 20 minutes; if you get the error "Timeout of 1200 seconds was
      #   reached", you can make the time longer if desired
    timeout = options(timeout=1200)) 
  state_df <- read.csv(file.path(local_dir,"fia_state_file.csv"))
  unlink(file.path(local_dir,"fia_state_file.csv"))
  # cycle through list of target taxon codes and extract those rows from
  # the state CSV
  for (sp in 1:length(species_codes)){
    target_sp <- state_df[which(state_df$SPCD==species_codes[[sp]]),]
    data <- rbind(data, target_sp)
  }
  # remove state file to make space for reading in the next one
  rm(state_df)
  # take a look at how much data were pulled
  cat(state_abb,": ",nrow(data)," observations. ")
  # keep only necessary data columns
  data_sm <- data %>% select(SPCD,INVYR,UNITCD,COUNTYCD,PLOT,STATECD,STATUSCD)
  # read in metadata tables and select relevant columns
    # plot data (has lat-long information)
  fia_plots <- read.csv(url(paste0("https://apps.fs.usda.gov/fia/datamart/CSV/",
    state_abb,"_PLOT.csv")))
  fia_plots <- fia_plots %>% select(INVYR,STATECD,UNITCD,COUNTYCD,PLOT,LAT,LON)
    # state and county codes and names
  county_codes <- read.csv(url(paste0("https://apps.fs.usda.gov/fia/datamart/CSV/",
    state_abb,"_COUNTY.csv")))
  county_codes <- county_codes %>% select(STATECD,COUNTYCD,COUNTYNM)
  # join FIA data to supplemental tables
  data_sm <- left_join(data_sm,fia_plots)
  data_sm <- left_join(data_sm,county_codes)
  # return results
  return(data_sm)
  rm(sp)
}

# loop through states and pull data
fia_outputs <- lapply(state_abb, extract_tree_data_web)
length(fia_outputs)
# stack state-by-state data extracted to create one dataframe
fia_raw <- data.frame()
for(file in seq_along(fia_outputs)){
  fia_raw <- rbind(fia_raw, fia_outputs[[file]])
}; nrow(fia_raw)
# join FIA data to species codes
fia_raw <- left_join(fia_raw,sp_codes)
# write file of raw data
write.csv(fia_raw,file.path(main_dir,occ_dir,raw_occ,"FIA",
  "fia_extracted_web.csv"),row.names=FALSE)

###
### STANDARDIZE THE DATA
### Run this section no matter which option your chose for getting the data !!
###

# rename columns to fit standard & create ID column
fia_raw <- fia_raw %>%
  rename("decimalLatitude" = "LAT",
         "decimalLongitude" = "LON",
         "year" = "INVYR",
         "isAlive" = "STATUSCD",
         "county" = "COUNTYNM") %>%
  unite("nativeDatabaseID", c("year","UNITCD","COUNTYCD","PLOT","STATECD"),
      remove=F, sep="-")
 # add a few informational columns
fia_raw$database <- "FIA"
fia_raw$datasetName <- "Forest Inventory and Analysis Database"
fia_raw$rightsHolder <- "USDA Forest Service"
fia_raw$individualCount <- 1
# create species_name column
fia_raw$species_name <- NA
fia_raw$species_name <- sapply(fia_raw$taxon_name, function(x)
  unlist(strsplit(x," var. | subsp. | f. "))[1])
sort(unique(fia_raw$species_name))
  # check a few standards and recode if needed
    # establishmentMeans
fia_raw$isAlive <- as.character(fia_raw$isAlive)
fia_raw <- fia_raw %>%
  mutate(establishmentMeans = recode(isAlive,
    "1" = "UNCERTAIN",
    "2" = "DEAD", #CUT
    "3" = "DEAD",
    "0" = "UNCERTAIN"))
fia_raw <- fia_raw %>% select(-isAlive)
    # basisOfRecord
fia_raw$basisOfRecord <- "HUMAN_OBSERVATION"
    # year
fia_raw$year[which(fia_raw$year == 9999)] <- NA
  # keep only necessary columns
fia_raw <- fia_raw %>%
  select(nativeDatabaseID,year,taxon_name,county,decimalLatitude,
    decimalLongitude,database,species_name,establishmentMeans,
    basisOfRecord,datasetName,rightsHolder,individualCount)
# write file
write.csv(fia_raw, file.path(main_dir,occ_dir,standardized_occ,data_out,
                             "fia.csv"), row.names=FALSE)
rm(fia_raw)


###
###############
###############################################
# G) Ex situ accession-level data: wild collection locations
# These data are compiled in 2-compile_exsitu_data.R and here we are simply 
# formatting them for use in the occurrence point dataset
###############################################
###############
###

# read in ex situ data we saved in 2-compile_exsitu_data.R (edit to match your 
#   file names)
exsitu_raw1 <- read.csv(file.path(main_dir,occ_dir,raw_occ,"Ex_situ",
  "ExSitu_Compiled_Post-Geolocation_2023-06-19.csv"), colClasses = "character",
    na.strings=c("", "NA"), strip.white=T, fileEncoding="UTF-8")
exsitu_raw2 <- read.csv(file.path(main_dir,occ_dir,raw_occ,"Ex_situ",
  "ExSitu_Dead_2023-06-19.csv"), colClasses = "character",
  na.strings=c("", "NA"), strip.white=T, fileEncoding="UTF-8")
exsitu_raw <- bind_rows(exsitu_raw1,exsitu_raw2)
nrow(exsitu_raw)
rm(exsitu_raw1,exsitu_raw2)
# rename columns to fit standard
exsitu_raw <- exsitu_raw %>%
  rename("taxon_name" = "taxon_name",
         "scientificName" = "taxon_full_name_orig",
         "specificEpithet" = "species",
         "taxonRank" = "infra_rank",
         "infraspecificEpithet" = "infra_name",
         "taxonIdentificationNotes" = "taxon_verif",
         "year" = "coll_year",
         "nativeDatabaseID" = "UID",
         "publisher" = "data_source",
         "rightsHolder" = "inst_short",
         "references" = "acc_num",
         "issue" = "latlong_flag",
         "individualCount" = "num_indiv",
         "decimalLatitude" = "lat_dd",
         "decimalLongitude" = "long_dd",
         "coordinateUncertaintyInMeters" = "latlong_uncertainty",
         "verbatimLocality" = "all_locality",
         "stateProvince" = "state",
         "establishmentMeans" = "prov_type") %>%
  unite("recordedBy", c("coll_name","coll_num"), 
        remove=T, sep="; ", na.rm=T) %>%
  unite("geolocationNotes", c("latlong_det","geolocated_by","latlong_notes"), 
        remove=T, sep="; ", na.rm=T) %>%
  unite("locationNotes", c("germ_type","garden_loc"), 
        remove=T, sep="; ", na.rm=T)
# establishmentMeans recode
sort(unique(exsitu_raw$establishmentMeans))
exsitu_raw <- exsitu_raw %>%
  mutate(establishmentMeans = recode(establishmentMeans,
    "H" = "CULTIVATED",
    "H?" = "CULTIVATED",
    "N" = "NATIVE",
    "NG" = "UNCERTAIN",
    "U" = "UNCERTAIN",
    "W" = "NATIVE",
    "Z" = "NATIVE"
  ))
# add a few informational columns
exsitu_raw$database <- "Ex_situ"
exsitu_raw$basisOfRecord <- "HUMAN_OBSERVATION"
exsitu_raw$datasetName <- exsitu_raw$rightsHolder
# keep only necessary columns
exsitu_raw <- exsitu_raw %>%
  select(database,taxon_name,scientificName,genus,specificEpithet,taxonRank,
         infraspecificEpithet,taxonIdentificationNotes,year,basisOfRecord,
         nativeDatabaseID,datasetName,publisher,rightsHolder,references,
         issue,recordedBy,establishmentMeans,individualCount,decimalLatitude,
         decimalLongitude,coordinateUncertaintyInMeters,geolocationNotes,
         locality,verbatimLocality,locationNotes,municipality,county,
         stateProvince,country)
head(exsitu_raw)
# write file
write.csv(exsitu_raw, file.path(main_dir,occ_dir,standardized_occ,data_out,
                                "exsitu.csv"), row.names=FALSE)
rm(exsitu_raw)

