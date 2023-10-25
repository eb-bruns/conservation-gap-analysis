### 2-compile_exsitu_data.R
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
### Last Updated: June 2023 ; first written Dec 2019
### R version 4.3.0

### DESCRIPTION:
  ## This script takes a folder of CSV files representing accessions data from
  #   different botanical garden collections, combines them into one dataset, 
  #   and standardizes some important fields.
  ## !Please note! that significant user input and edits are needed, since every 
  #   ex situ dataset is unique. Go slowly step-by-step and continue checking 
  #   that everything is as expected. 

### INPUTS:
#
# 1. exsitu_standard_column_names (folder)
#    Accessions data from ex situ collections survey; CSV files whose column
#    names have been standardized by hand using instructions in the
#    "Processing ex situ data" tab in Gap-analysis-workflow_metadata workbook
#
# 2. respondent_institution_data_table.csv
#    Table with metadata for institutions who provided accessions data during
#    the ex situ collections survey; see example in the
#    "Processing ex situ data" tab in Gap-analysis-workflow_metadata workbook
#
# 3. target_taxa_with_synonyms.csv
#    List of target taxa and synonyms; see example in the "Target taxa list"
#    tab in Gap-analysis-workflow_metadata workbook
#
# 4. world_countries_10m.shp
#    Shapefile created in 1-prep_gis_layers.R script. It's the Natural Earth 
#    10m countries layer with the lakes cut out and some ISO_2A issues fixed.
#
# 5. (optionally) Accession-level data downloads from two international crop 
#    genebank databases; download instructions are provided within the script
# (a) Genesys [Global Crop Diversity Trust]:
#     https://www.genesys-pgr.org/a/overview
# (b) WIEWS [FAO's World information and early warning system on plant genetic
#     resources for food and agriculture]:
#     https://www.fao.org/wiews/data/ex-situ-sdg-251/search/en/?no_cache=1
# 
# 6. (optionally) ExSitu_Need_Geolocation_YYYY-MM-DD_Geolocated.csv
#    If you manually geolocate records, you'll use this to update your final 
#    dataset

### OUTPUTS:
  ## ExSitu_Unmatched_Species_YYYY_MM_DD.csv
  #   List of taxa in the ex situ accessions data that have no match in your
  #   target taxa list (target_taxa_with_synonyms.csv); these can be reviewed
  #   manually and added, as desired, to the synonyms in your taxa list
  ## Genera_Institutions_Summary_YYYY_MM_DD.csv
  #   Summary of which genera each institution reports:
  #     inst_short    genera 
  #     ARM005        Amygdalus; Cerasus; Diospyros; Prunus  
  #     HuntingtonBG  Asimina; Carya; Diospyros; Juglans; Prunus
  #     ...
  ## ExSitu_Dead_YYYY_MM_DD.csv
  #   All ex situ records with zero individuals (dead / removed accessions);
  #   we remove these from the ex situ data, but use them in the wild occurrence
  #   data, since it may provide an additional wild location; has all
  #   columns selected for your final export of ex situ data
  ## ExSitu_Compiled_No-Dups-Combined_YYYY_MM_DD.csv
  #   Version of final ex situ dataset right before combining duplicates at the
  #   accession level. Some institutions provided data at the individual level,
  #   but we are trying to standardize to the accession level. But, this
  #   pre-dup-removal version is saved for reference, just in case.
  ## All_ExSitu_Compiled_YYYY_MM_DD.csv
  #   If you're not manually geolocating (finding lat-long for records with 
  #   no coordinates but wild collection locality description), this is your
  #   final dataset. If you are geolocating, see the next outputs. For 
  #   descriptions of columns in the output, see the "Ex situ data output" tab
  #   in Gap-analysis-workflow_metadata 
  ## ExSitu_Geolocation_Needs_Summary_YYYY_MM_DD.csv
  #   Summary for each taxon of how many records may need to be manually 
  #   geolocated (NoCoords_YesLocality), the number of records that already 
  #   have lat-long (YesCoords), and a comparison (Percent_NeedGeo):
  #     taxon_name_accepted num_acc num_wild YesCoords NoCoords_YesLocality Percent_NeedGeo
  #     Asimina obovata     3       1        1         0                    0  
  #     Asimina parviflora  15      5        3         6                    66.7
  #     Asimina tetramera   9       3        0         9                    100
  #     ...
  ## ExSitu_Need_Geolocation_YYYY_MM_DD.csv
  #   This is the dataset you will go through and manually geolocate; 
  #   instructions for geolocating can be found here:
  #   https://docs.google.com/document/d/1RBUD6-ogLc7PRVkDJSIEKzkgvC6xekj9Q_kl1vzxhCs/edit?usp=share_link
  ## FINAL_ExSitu_Compiled_Post-Geolocation_YYYY_MM_DD.csv
  #   Final ex situ dataset, saved to both your ex situ output folder and your
  #   raw in situ input data folder (to use in mapping wild occurrences).
  #   For descriptions of columns in the output, see the "Ex situ data output"
  #   tab in Gap-analysis-workflow_metadata 

################################################################################
# Load libraries
################################################################################

# install measurements package if you don't have it yet
install.packages('measurements')

# load packages
my.packages <- c('tidyverse','textclean','CoordinateCleaner','terra')
  # versions I used (in the order listed above): 2.0.0, 0.9.3, 2.0-20, 1.7-29
# install.packages(my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

# be sure we're using dplyr when we want to
select <- dplyr::select
rename <- dplyr::rename
filter <- dplyr::filter
group_by <- dplyr::group_by
mutate <- dplyr::mutate
distinct <- dplyr::distinct

################################################################################
# Set working directory
################################################################################

# use 0-set_working_directory.R script:
  # update to your path
source("/Users/emily/Documents/GitHub/conservation-gap-analysis/spatial-analysis-workflow/0-set_working_directory.R")

################################################################################
# Load functions
################################################################################

# function to read in ex situ files from different folders/years and stack
read.exsitu.csv <- function(path,submission_year){
  # create list of paths to ex situ accessions CSV files in folder
  file_list <- list.files(path=path,pattern=".csv",full.names=TRUE)
  # read in each csv in path list to create list of data frames
  file_dfs <- lapply(file_list, read.csv, header=TRUE, fileEncoding="LATIN1",
    strip.white=TRUE, colClasses="character", na.strings=c("","NA","N/A"))
  print(paste0("Number of files: ",length(file_dfs)))
    #sapply(file_dfs, nrow) # can look at number of rows in each csv
  # for each file, make some edits...
  for(file in seq_along(file_dfs)){
    df <- file_dfs[[file]]
    # replace strange characters in column names (arise from saving?)
    names(df) <- gsub(x = names(df), pattern = "ï\\.\\.", replacement = "")
    # if there is no inst_short provided, add based on the file name
    if(length(df$inst_short) == 0){
      df$inst_short <- rep(mgsub(file_list[file],c(paste0(path,"/"),".csv"),""),nrow(df))
    }
    # add file name as column, to record the name of the file (often the same as
    #   inst_short, but different if from a 'parent R file' w/ mult. institutions)
    df$filename <- rep(mgsub(file_list[file],c(paste0(path,"/"),".csv"),""),nrow(df))
    # add year of submission
    df$submission_year <- submission_year
    # remove extra blank columns that may be present
    t <- grepl("^X",names(df))
    if(length(unique(t))>1){ df <- df[, -grep("^X", names(df))] }
    # add accession number if there isn't one
    if("acc_num" %in% names(df) & nrow(df[which(is.na(df$acc_num)),]) > 0){
      df[which(is.na(df$acc_num)),]$acc_num <- paste0("added",
        sprintf("%04d", 1:nrow(df[which(is.na(df$acc_num)),])))
    }  # if you have mis-named acc_num column(s) you need to address that...
          #} else if ("acc_no" %in% names(df) & nrow(df[which(is.na(df$acc_no)),]) > 0){
          #  df[which(is.na(df$acc_no)),]$acc_no <- paste0("added",
          #    sprintf("%04d", 1:nrow(df[which(is.na(df$acc_no)),])))
          #} else if (!("acc_num" %in% names(df)) & !("acc_no" %in% names(df))){
          #  df$acc_num <- paste0("added", sprintf("%04d", 1:nrow(df)))
          #} else {
          #  print(paste("NO ACC NUM EDITS:",df$filename[1]))
          #}
    # replace orig df with new edited df in your list
    file_dfs[[file]] <- df
    #print(head(file_dfs[[file]],n=2))
  }
  # stack all datasets using bind_rows, which keeps non-matching columns
  # and fills with NA; 'Reduce' iterates through and merges with previous;
  # this may take a few minutes if you have lots of data
  all_data <- Reduce(bind_rows, file_dfs)
    print(paste0("Number of rows: ",nrow(all_data)))
    print(paste0("Number of columns: ",ncol(all_data)))
  return(all_data)
}

################################################################################
# 1. Read in and stack all ex situ accessions data from survey of collections
################################################################################

# read in data from multiple surveys and stack, or just read in from one folder.
# this function also adds columns for 1) the file name [often equivalent to the
# "inst_short" institution nickname] 2) a submission year, 3) an accession
# number if one isn't given
### CHANGE BASED ON FOLDER(S) AND YEAR(S) YOU HAVE...
all_data <- read.exsitu.csv(file.path(main_dir, exsitu_dir, raw_exsitu,
                                      "exsitu_standard_column_names"), "2022")
# stack all data if you had multiple years:
#to_stack <- list(raw_2022,raw_2021,raw_2020,raw_2019,raw_2018,raw_2017)
#all_data <- Reduce(bind_rows, to_stack)

# look at columns
sort(colnames(all_data))
### IF NEEDED, EDIT COLUMNS (un-comment & update code chunks for your data)...
  ## combine multiple inst_short columns
#all_data <- tidyr::unite(all_data,"inst_short", c("inst_short","inst_short2"),
#  sep=";",remove=T,na.rm=T)
  ## combine multiple taxon name columns (we'll do the rest of the column 
  ## standardizing later; this is to get genus for a summary table right away)
#all_data <- tidyr::unite(all_data,"taxon_full_name",
#  c("taxon_full_name","taxon_name_full","taxon.full_name"),
#  sep=";",remove=T,na.rm=T)

# remove rows with no inst_short (shouldn't happen, but just in case)
all_data <- all_data[which(!is.na(all_data$inst_short)),]

# read in institution metadata file, for comparing list to data read in
inst_data <- read.csv(file.path(main_dir, exsitu_dir,
  "respondent_institution_data_table.csv"), stringsAsFactors = F)
## CHECK ALL INSTITUTIONS ARE PRESENT...
  ### INSTITUTIONS IN THE DATA YOU READ IN BUT NOT IN METADATA TABLE
  # this should be "character(0)"
unique(all_data$inst_short)[!(unique(all_data$inst_short) %in% 
                                unique(inst_data$inst_short))]
  ### INSTITUTIONS IN METADATA TABLE BUT NOT IN THE DATA YOU READ IN:
  # this should just be parent R files (have multiple institutions)
unique(inst_data$inst_short)[!(unique(inst_data$inst_short) %in% 
                               unique(all_data$inst_short))]

# fill genus column so we can see which datasets have our target genus
  # fill genus column if not already filled
all_data <- all_data %>% 
  separate("taxon_full_name","genus_temp", sep=" ", remove=F)
all_data[which(is.na(all_data$genus)),]$genus <- 
  all_data[which(is.na(all_data$genus)),]$genus_temp
  # standardize capitalization
all_data$genus <- str_to_title(all_data$genus)
  ### IF NEEDED, FIX GENUS MISSPELLINGS OR ABBREVIATIONS...
sort(unique(all_data$genus))
#all_data$genus <- mgsub(all_data$genus,
#  c("^Q$","^Q\\.$","Querces","Querucs","Querus","Qurercus","Cyclobalanopsis"),
#     "Quercus", fixed=F)
#all_data$taxon_full_name <- mgsub(all_data$taxon_full_name,
#  c("Q\\.","Cyclobalanopsis"), 
#     "Quercus", fixed=F)

### IF NEEDED, REMOVE DUPLICATE DATA - from previous years or networks...
### UPDATE THIS SECTION BASED ON YOUR SPECIFIC NEEDS; THIS WAS FOR QUERCUS 2022:
#nrow(all_data)
#all_data$file_inst_year <- paste(all_data$filename,all_data$inst_short,all_data$submission_year)
#
#  # 1) remove datasets that have no Quercus data (so we know they aren't dups)
#inst_genera <- all_data %>%
#  group_by(filename,inst_short,submission_year) %>%
#  mutate(genera = paste(unique(genus),collapse = ", ")) %>%
#  ungroup() %>% select(inst_short,filename,submission_year,genera)
#yes_Q <- unique(inst_genera[which(grepl("Quercus",inst_genera$genera)),]) %>%
#  mutate(file_inst_year = paste(filename,inst_short,submission_year))
#all_data <- all_data %>% filter(file_inst_year %in% yes_Q$file_inst_year)
#nrow(all_data)
#
#  # 2) remove old datasets if there is newer data from 2018-2020
#all_data$inst_and_year <- paste(all_data$inst_short,all_data$submission_year)
#find_dups <- all_data %>%
#  filter(submission_year != "2021" & submission_year != "2022") %>%
#  group_by(inst_short,submission_year) %>%
#  count() %>% ungroup() %>%
#  arrange(desc(submission_year)); find_dups
#remove_dups <- find_dups[which(duplicated(find_dups$inst_short)),]
#remove_dups <- paste(remove_dups$inst_short,remove_dups$submission_year); remove_dups
#all_data <- all_data %>% filter(!(inst_and_year %in% remove_dups))
#nrow(all_data)
#
#  # 3) if there is duplicate network data, keep only the data direct from garden
#network_data <- all_data %>% filter(inst_short != filename &
#  (submission_year != "2022" & submission_year != "2021"))
#own_data <- all_data %>% filter(inst_short == filename &
#  (submission_year != "2022" & submission_year != "2021"))
#remove <- sort(unique(network_data[which(
#  network_data$inst_short %in% own_data$inst_short),]$file_inst_year)); remove
#all_data <- all_data %>%
#  filter(!(file_inst_year %in% remove))
#nrow(all_data)

# remove columns that are completely empty
not_all_na <- function(x) any(!is.na(x))
all_data <- all_data %>% select(where(not_all_na))

# add required taxon name columns if they are not present, since we use them
#   later; the script will throw an error if they're not in the data
if(length(all_data$genus) == 0){ all_data$genus <- "" }
if(length(all_data$hybrid) == 0){ all_data$hybrid <- "" }
if(length(all_data$species) == 0){ all_data$species <- "" }
if(length(all_data$infra_rank) == 0){ all_data$infra_rank <- "" }
if(length(all_data$infra_name) == 0){ all_data$infra_name <- "" }
if(length(all_data$cultivar) == 0){ all_data$cultivar <- "" }

# check out column names
sort(colnames(all_data))
### IF NEEDED, SEPARATE SINGLE COLUMN INTO MULTIPLE...
#all_data <- all_data %>% separate("specific",
#  c("infra_rank_add","infra_name_add"), sep=" ", remove=T, fill="right")
### IF NEEDED, FIX MIS-NAMED COLUMNS (mistakes during manual processing)...
  # you can either see which datasets have the mis-named column & fix manually
  # in the data file:
#unique(all_data$filename[all_data$locality.1 != ""])
  # or you can simply merge similar columns like so (update for you!!)...
#all_data <- tidyr::unite(all_data,"cultivar",
#                         c("cultivar","cultivsr"), sep="; ", remove=T, na.rm=T)
#all_data <- tidyr::unite(all_data,"genus", 
#                         c("genus","gensu"), sep="; ", remove=T, na.rm=T)
#all_data <- tidyr::unite(all_data,"hybrid", c("hybrid","hyrbid"),
#                         sep="; ", remove=T, na.rm=T)
#all_data <- tidyr::unite(all_data,"notes", 
#                         c("notes","Notes","notes2","inst_short2"), sep="; ", remove=T, na.rm=T)

### SELECT ONLY THE COLUMNS YOU WANT TO KEEP, BASED ON YOUR OWN DATA & NEEDS...
keep_col <- c("acc_num","assoc_sp",#"author",
              "coll_name","coll_num","coll_year",#"condition",
              "country","county","cultivar",#"dataset_year",
              "filename","garden_loc","genus","germ_type",#"habitat",
              "hybrid","infra_name","infra_rank","inst_short","lin_num",
              "locality","municipality","notes","num_indiv","orig_lat",
              "orig_long","orig_source",#"private",
              "prov_type","rec_as","species","state","submission_year",
              "taxon_full_name","taxon_verif"#,"trade_name"
              )
all_data <- all_data[,keep_col]

# remove leading, trailing, and middle (e.g., double space) white space
all_data <- as.data.frame(lapply(all_data, function(x) str_squish(x)), stringsAsFactors=F)
# replace "" cells with NA in whole dataset
all_data[all_data == ""] <- NA
# add a general data source column
all_data$data_source <- "ex_situ_BG_survey"
# fix some common lat/long character issues before replacing non-ascii characters
all_data$orig_lat <- mgsub(all_data$orig_lat,
  c("\"","ç","d","'","°","º","Â","¼","،","¡","_","´","*","À","?","`")," ")
all_data$orig_long <- mgsub(all_data$orig_long,
  c("\"","ç","d","'","°","º","Â","¼","،","¡","_","´","*","À","?","`")," ")
# now replace all non-ascii characters
all_data <- as.data.frame(lapply(all_data,replace_non_ascii), stringsAsFactors=F)

# add additional necessary columns, some of which are in the optional data we 
#   add from public databases like Genesys
all_data$latlong_flag <- NA
all_data$latlong_det <- NA
all_data$latlong_uncertainty <- NA
all_data$geolocated_by <- NA
all_data$latlong_notes <- NA

################################################################################
# 2. [OPTIONAL] Compile Genesys data (genebanks)
################################################################################

### FIRST, download data from Genesys [Global Crop Diversity Trust]:
# Go to https://www.genesys-pgr.org/a/overview
# Search for each target genus/genera in the TAXONOMY tab on the left (remember
#   synonym genera, if applicable), then click "APPLY FILTERS" at the top left.
# Click the "ACCESSIONS" tab in the top bar, then click "DOWNLOAD ZIP" on the
#   right.
# Move the downloaded folder to your "exsitu_data > raw_exsitu_data" folder and 
#   rename to "genesys-accessions"
# You can view field metadata here: https://www.genesys-pgr.org/documentation/basics

# load data
  # collection site description
genPath <- file.path(main_dir, exsitu_dir, raw_exsitu, "genesys-accessions/coll.csv")
  # I think the "Found and resolved improper quoting" warning is ok
gen_col <- data.table::fread(file = genPath, header = TRUE, 
                             na.strings=c("NA",""))
  str(gen_col)
  nrow(gen_col) ; length(unique(gen_col$genesysId))
  # metadata about record
genPath <- file.path(main_dir, exsitu_dir, raw_exsitu, "genesys-accessions/core.csv")
gen_core <- data.table::fread(genPath, header = TRUE,
                              na.strings=c("NA",""))
str(gen_core)
  nrow(gen_core) ; length(unique(gen_core$genesysId))
  # spatial info
  #   for some reason the headers don't read in correctly with fread;
  #   will use read.csv just to get the headers and add them to the fread df
genPath <- file.path(main_dir, exsitu_dir, raw_exsitu, "genesys-accessions/geo.csv")
gen_geo <- data.table::fread(genPath, header = TRUE,
                             na.strings=c("NA",""))
  str(gen_geo)
  colnames(gen_geo)
gen_geo_head <- read.csv(genPath)
  colnames(gen_geo_head)
gen_geo <- gen_geo %>% select(-V8) # remove the last column (nothing in it)
colnames(gen_geo) <- colnames(gen_geo_head)
  str(gen_geo)
  nrow(gen_geo) ; length(unique(gen_geo$genesysId))

# combine all dataframes
genesys <- Reduce(full_join,list(gen_col,gen_core,gen_geo)) #,gen_names
str(genesys); nrow(genesys)
  # add 'Genesys' to ID
genesys$UID <- paste0("Genesys-",genesys$genesysId)
  # paste some similar columns together
genesys <- tidyr::unite(genesys,"coll_year",
  c("collDate","acqDate"), sep=";", remove=T, na.rm=T)

# rename columns to match ex situ data and select only those we need
genesys_sel <- genesys %>%
  rename(taxon_full_name = fullTaxa,
         coll_num = collNumb,
         locality = collSite,
         inst_short = instCode,
         acc_num = acceNumb,
         infra_rank = subtaxa,
         country = origCty,
         orig_lat = latitude,
         orig_long = longitude,
         latlong_notes = method,
         germ_type = storage,
         orig_source = collSrc,
         prov_type = sampStat,
         latlong_uncertainty = uncertainty) %>%
  select(UID,taxon_full_name,coll_num,coll_year,locality,
    inst_short,acc_num,infra_rank,country,orig_lat,orig_long,latlong_notes,
    germ_type,orig_source,prov_type,genus,species,latlong_uncertainty)

# add data source column
genesys_sel$data_source <- "Genesys"

# join to exsitu data
  # make all col character type first to join successfully
  genesys_sel <- genesys_sel %>% mutate(across(everything(), as.character))
all_data <- bind_rows(all_data,genesys_sel)
nrow(all_data)

### IF YOU HAVE DATA FROM THE USDA ARS NATIONAL PLANT GERMPLASM SYSTEM (GRIN)...
# I think all the US institutions are covered by GRIN, so we'll remove 
# these; pulled this list from CWR Gap Analysis 2020 (Khoury et al.)... may 
# need to be refined at some point?
#USDAcodes <- c("USA003" ,"USA004", "USA005" ,"USA016" ,"USA020",
#               "USA022", "USA026", "USA028", "USA029", "USA042" ,"USA047",
#               "USA049", "USA074", "USA108", "USA133", "USA148", "USA151", 
#               "USA167", "USA176", "USA390", "USA955", "USA956", "USA970", 
#               "USA971", "USA995")
#all_data <- all_data %>% filter(!(inst_short %in% USDAcodes))

nrow(all_data)

################################################################################
# 3. [OPTIONAL] Compile WIEWS data (genebanks)
################################################################################

### FIRST, download data from WIEWS [FAO's World information and early warning
#   system on plant genetic resources for food and agriculture]:
# Go to https://www.fao.org/wiews/data/ex-situ-sdg-251/search/en/?no_cache=1
# In the "Crop Wild Relatives" dropdown, select "Included".
# Type your target genus into the "Genus" box and press the "+" button on the
#   right. Add additional target genera like this as needed.
# Scroll down and press the magnifying glass icon on the bottom right.
# Now click the "Download Results" button on the top right.
# Move the downloaded folder to your "exsitu_data > raw_exsitu_data" folder and 
#   rename to "Wiews-Exsitu.csv"

# read in data
wiews <- read.csv(file.path(main_dir, exsitu_dir, raw_exsitu, "Wiews-Exsitu.csv"))
str(wiews)

# filter out Genesys data
wiews <- wiews %>%
  filter(Source.of.information != "Genesys (https://www.genesys-pgr.org)")
nrow(wiews)

# rename columns to match ex situ data and select only those we need
wiews_sel <- wiews %>%
  rename(inst_short = Holding.institute.code,
         acc_num = Accession.number,
         taxon_full_name = Taxon,
         genus = Genus,
         species = Species,
         rec_date = Acquisition.date..YYYY.MM.,
         country = Country.of.origin..ISO3.,
         prov_type = Biological.status,
         notes = Genebank.s..holding.safety.duplications...code,
         orig_lat = Latitude.of.collecting.site..decimal.degrees.format.,
         orig_long = Longitude.of.collecting.site..decimal.degrees.format.,
         orig_source = Collecting.acquisition.source,
         germ_type = Type.of.germplasm.storage) %>%
  select(inst_short,acc_num,taxon_full_name,genus,species,rec_date,country,
         prov_type,notes,orig_lat,orig_long,orig_source,germ_type)

### IF YOU HAVE DATA FROM THE USDA ARS NATIONAL PLANT GERMPLASM SYSTEM (GRIN)...
# I think all the US institutions are covered by GRIN, so we'll remove; I'm 
# not positive this is right... would need to do some digging
wiews_sel <- wiews_sel %>% filter(!grepl("USA",inst_short))
nrow(wiews_sel)

# add data source column
wiews_sel$data_source <- "FAO-WIEWS"

# join to exsitu data
  # make all col character type first to join successfully
  wiews_sel <- wiews_sel %>% mutate(across(everything(), as.character))
all_data <- bind_rows(all_data,wiews_sel)
nrow(all_data)

################################################################################
# 4. Save raw output of compiled data for target genera
#    (can be used to look for hybrids/cultivars, which are removed in next step)
################################################################################

# create new version of data before big edits, so we can more easily go back; 
# we do this throughout but I'll just mention it here; this does take more 
# computational power, so if you have a lot of data you may want to skip this 
# and edit the script to remove
all_data2 <- all_data

# read in target taxa list
taxon_list <- read.csv(file.path(main_dir, taxa_dir,"target_taxa_with_synonyms.csv"),
                       header=T, colClasses="character",na.strings=c("","NA"))
str(taxon_list)

# preserve original taxon name before we edit
all_data2$taxon_full_name_orig <- all_data2$taxon_full_name

# remove rows not in target genus/genera
target_genera <- unique(as.character(map(strsplit(taxon_list$taxon_name, split=" "), 1)))
all_data2 <- all_data2 %>% filter(genus %in% target_genera)
nrow(all_data); nrow(all_data2)

### CHECK OUT THE HYBRID COLUMN...
sort(unique(all_data2$hybrid))
## NOTE that this is only really necessary if you have a hybrid in your target
#   taxa list. If you don't, all you need to do is make sure there is nothing
#   in the hybrid column that does not mark the record as a hybrid (e.g. 
#   "species") and remove that text; the following will need edits !!!...
# if needed, standardize so only one hybrid symbol is used ( x )
all_data2$hybrid <- mgsub(all_data2$hybrid,
  c(" A ","^A ","^A$"," X ","^X"," _ ","^_ ","^_$","^1$","\\*","^H$","^hyb$","Hybrid"),
  " x ", fixed=F)
# add "x" at beginning then remove duplicates
all_data2$hybrid[!is.na(all_data2$hybrid)] <- paste0("x ",all_data2$hybrid[!is.na(all_data2$hybrid)])
all_data2$hybrid <- str_squish(all_data2$hybrid)
all_data2$hybrid <- str_to_lower(all_data2$hybrid)
all_data2$hybrid <- mgsub(all_data2$hybrid,c("x x","x h$"),"x",fixed=F)
sort(unique(all_data2$hybrid))

# create concatenated taxon_full_name column
all_data2 <- tidyr::unite(all_data2, "taxon_full_name_concat",
  c(genus,hybrid,species,infra_rank,infra_name,cultivar), sep=" ", remove=F,
  na.rm=T)

# when blank, fill taxon_full_name column with concatenated full name
all_data2[is.na(all_data2$taxon_full_name),]$taxon_full_name <-
  all_data2[is.na(all_data2$taxon_full_name),]$taxon_full_name_concat
unique(all_data2$taxon_full_name)

# standardize common hybrid signifiers in taxon_full_name; you will need to 
#   check this works as expected and doesn't leave out an important one
all_data2$taxon_full_name <- mgsub(all_data2$taxon_full_name,
  c(" A ","_"," X ","\\*","×"," \\? ")," x ",fixed=F)
# make sure there is a space b/w the author and taxon name in taxon_full_name
all_data2$taxon_full_name <- gsub("\\("," (",all_data2$taxon_full_name)
all_data2$taxon_full_name <- str_squish(all_data2$taxon_full_name)
unique(all_data2$taxon_full_name)

# add space after periods in taxon_full_name
all_data2$taxon_full_name <- gsub(".",". ",all_data2$taxon_full_name,fixed=T)
all_data2$taxon_full_name <- str_squish(all_data2$taxon_full_name)

################################################################################
# 5. Further standardize taxon name, then keep data for target taxa only
################################################################################

### manually fix taxon name issues you notice...
#all_data2$taxon_full_name <- mgsub(all_data2$taxon_full_name,
#  c("Corylus Acolurnoides","Prunus Ayedoensis","Pyrus (Malus) fusca"),
#  c("Corylus x colurnoides","Prunus x yedoensis","Malus fusca"))

# first change hybrid notation temporarily: remove space so it stays together
all_data2$taxon_full_name <- gsub(" x "," +",all_data2$taxon_full_name)

# separate out taxon full name and trim white space again
    # this warning is ok: "Expected 9 pieces. Additional pieces discarded..."
all_data2 <- all_data2 %>% separate("taxon_full_name",
  c("genus_new","species_new","extra1","extra2",
    "extra3","extra4","extra5","extra6","extra7"), sep=" ", extra="warn",
    remove=F, fill="right")
all_data2 <- as.data.frame(lapply(all_data2, str_squish), stringsAsFactors=F)
# replace genus_new with genus, since we fixed that up in the previous section
all_data2$genus_new <- all_data2$genus

## REMOVE RECORDS WITHOUT SPECIFIC EPITHET

# remove records with no/non-standard specific epithet (mostly cultivars or
#   'sp.' or questionable '?') by looking in species name column;
# if you WANT cultivars, you may need to edit this!
all_data3 <- all_data2 %>%
  filter(!grepl("\"",species_new) &
           !grepl("\'",species_new) &
           !grepl("\\[",species_new) &
           !grepl("\\(",species_new) &
           !grepl("\\.",species_new) &
           !grepl("[A-Z]",species_new) &
           !grepl("[0-9]",species_new) &
           !grepl("\\?",species_new) &
           !is.na(species_new))
nrow(all_data3)
# see names for records removed; can add anything you want to fix to the
#   "manually fix taxon name issues you notice" section above and rerun 
#   starting where you first created all_data2
sort(unique(suppressMessages(anti_join(all_data2,all_data3))$taxon_full_name))

## FIND INFRATAXA

# look for infrataxa key words
  # make data in all "extra" columns lower case
sp_col <- grep("^species_new$", colnames(all_data3))
all_data3[,sp_col:(sp_col+7)] <- as.data.frame(sapply(
  all_data3[,sp_col:(sp_col+7)], tolower), stringsAsFactors=F)
  # create matrix of all "extra" species name columns, to search for
  #   infraspecific key words
search.col <- matrix(cbind(all_data3$extra1,all_data3$extra2,all_data3$extra3,
  all_data3$extra4,all_data3$extra5,all_data3$extra6,all_data3$extra7),
    nrow=nrow(all_data3))
  # search the "extra" column matrix for matches to infraspecific key words
matches_i <- which(search.col=="variety"|search.col=="var"|search.col=="var."|
                     search.col=="v"|search.col=="v."|
                   search.col=="subspecies"|search.col=="subsp"|
                     search.col=="subsp."|search.col=="ssp"|search.col=="ssp."|
                     search.col=="subs."|search.col=="spp."|search.col=="sub."|
                     search.col=="infra"|
                   search.col=="forma"|search.col=="form"|search.col=="fma"|
                     search.col=="fo"|search.col=="fo."|search.col=="f"|
                     search.col=="f.",arr.ind=T)
matches_i[,2] <- matches_i[,2]+sp_col
# create new infra_rank column and fill with "extra" contents that matched
#   infraspecific key words
all_data3$infra_rank_new <- NA
all_data3[matches_i[,1],"infra_rank_new"] <- all_data3[matches_i]
unique(all_data3$infra_rank_new) # check results

# create new infra_name column and fill with next column over from "extra"
#   contents that matched infraspecific key word
all_data3$infra_name_new <- NA
matches_i[,2] <- matches_i[,2]+1
all_data3[matches_i[,1],"infra_name_new"] <- all_data3[matches_i]

# standardize infraspecific rank names
all_data3$infra_rank_new <- replace(all_data3$infra_rank_new,
  grep("^variety$|^var$|^v$|^v.$",all_data3$infra_rank_new), "var.")
all_data3$infra_rank_new <- replace(all_data3$infra_rank_new,
  grep("^subspecies$|^subsp$|^ssp$|^ssp.$|^subs.$|^spp.$|^sub.$|^infra$",
    all_data3$infra_rank_new), "subsp.")
all_data3$infra_rank_new <- replace(all_data3$infra_rank_new,
 grep("^forma$|^form$|^fma$|^fo$|^fo.$|^f$",all_data3$infra_rank_new), "f.")
  # names we identified as infraspecific 
unique(all_data3[which(!is.na(all_data3$infra_rank_new)),"taxon_full_name"])

## CREATE FINAL TAXON FULL NAME FOR FILTERING

# create new taxon full name column
all_data3$taxon_full_name <- NA
  # select rows with infraspecific name and concatenate
yes_infra <- which(!is.na(all_data3$infra_rank_new) &
  !is.na(all_data3$infra_name_new))
all_data3$taxon_full_name[yes_infra] <- paste(all_data3$genus_new[yes_infra],
  all_data3$species_new[yes_infra], all_data3$infra_rank_new[yes_infra],
  all_data3$infra_name_new[yes_infra],sep=" ")
  # select rows without infraspecific name and concatenate
all_data3$taxon_full_name[-yes_infra] <- paste(all_data3$genus_new[-yes_infra],
  all_data3$species_new[-yes_infra],sep=" ")
# switch hybrid symbol back to " x "
all_data3$taxon_full_name <- gsub(" \\+"," x ",all_data3$taxon_full_name)
sort(unique(all_data3$taxon_full_name))

## FILTER OUT NON-TARGET TAXA

# rename some taxon name columns to preserve originals
all_data3 <- all_data3 %>%
  rename(taxon_name = taxon_full_name,
                genus_orig = genus,
                species_orig = species,
                infra_rank_orig = infra_rank,
                infra_name_orig = infra_name)
all_data3 <- all_data3 %>%
  rename(genus = genus_new,
                species = species_new,
                infra_rank = infra_rank_new,
                infra_name = infra_name_new)

# join dataset to taxa list
### ROUND 1: match full taxon name
  # add genus_species column to taxon list
taxon_list$genus_species <- NA
taxon_list$genus_species <- sapply(taxon_list$taxon_name, function(x)
  unlist(strsplit(x," var. | subsp. | f. "))[1])
  # join by taxon name
all_data4 <- full_join(all_data3,taxon_list)
table(all_data4$taxon_name_status) 
### ROUND 2: match just by species name
need_match2 <- all_data4[which(is.na(all_data4$taxon_name_status)),]
nrow(need_match2)
    # remove columns from first taxon name match
need_match2 <- need_match2[,1:(ncol(all_data4)-ncol(taxon_list)+1)]
    # remove taxon_name col from taxon data so it doesn't match
taxon_list_sp <- taxon_list %>% 
  arrange(taxon_name_status) %>%
  distinct(genus_species,.keep_all=T) %>%
  select(-taxon_name)
  # create genus_species column
need_match2$genus_species <- paste(need_match2$genus,need_match2$species)
    # new join by genus_species
need_match2 <- left_join(need_match2,taxon_list_sp); nrow(need_match2)
  # bind together new matches and previously matched
matched1 <- all_data4[which(!is.na(all_data4$taxon_name_status)),]
all_data4 <- rbind(matched1,need_match2)
  table(all_data4$taxon_name_status)
head(all_data4)

### CHECK UNMATCHED SPECIES; ADD TO SYNONYM LIST AS NECESSARY ###
check <- all_data4 %>% filter(is.na(taxon_name_status))
check <- data.frame(taxon_name = sort(unique(check$taxon_name)))
nrow(check); check
# write file for checking, as desired
  # IF YOU FIND MISSPELLINGS AND/OR ADDITIONAL SYNONYMS, YOU CAN ADD THEM TO
  #   YOUR TARGET TAXA LIST AND GO BACK TO THE START OF SECTION 4 AND RUN AGAIN 
  #   FROM THERE
write.csv(check, file.path(main_dir, exsitu_dir,standardized_exsitu,
                           paste0("ExSitu_Unmatched_Species_", Sys.Date(), 
                                  ".csv")),row.names = F)

# keep only matched names
all_data5 <- all_data4 %>% 
  filter(!is.na(taxon_name_status) & !is.na(inst_short))
nrow(all_data5)
# see target taxa with no data
unique(taxon_list$taxon_name_accepted)[
  !(unique(taxon_list$taxon_name_accepted) %in% (unique(all_data5$taxon_name_accepted)))]
# "Asimina incana" "Asimina manasota" "Asimina pygmaea" "Asimina reticulata"
# "Asimina x nashii" "Juglans major var. major" "Juglans microcarpa var. microcarpa" 

# see any names with an x in the full name but not the accepted name
unique(all_data5[which(grepl(" x ",all_data5$taxon_full_name_orig) &
                         !grepl(" x ",all_data5$taxon_name_accepted)),
                 c("taxon_full_name_orig","taxon_name_accepted")])
# these are complicated hybrids we don't want; remove
all_data5 <- all_data5 %>%
  filter(!(grepl(" x ",all_data5$taxon_full_name_orig) &
           !grepl(" x ",all_data5$taxon_name_accepted)))
nrow(all_data5)

# final part for removing duplicate data - from previous years or networks
all_data6 <- all_data5
### UPDATE THIS SECTION BASED ON YOUR SPECIFIC NEEDS; THIS IS FOR QUERCUS 2022:
# for 2021 and 2022 data, only remove older data for species provided
#dup_2022 <- unique(all_data6[which(all_data6$submission_year=="2022"),
#  c("taxon_name","inst_short")]); dup_2022
#for(i in 1:nrow(dup_2022)){
#  all_data6 <- all_data6 %>%
#   filter(!(taxon_name == dup_2022[i,1] &
#            inst_short == dup_2022[i,2] &
#            submission_year != "2022"))
#}; nrow(all_data6)
#dup_2021 <- unique(all_data6[which(all_data6$submission_year=="2021"),
#  c("taxon_name","inst_short")]); dup_2021
#for(i in 1:nrow(dup_2021)){
#  all_data6 <- all_data6 %>%
#  filter(!(taxon_name == dup_2021[i,1] &
#           inst_short == dup_2021[i,2] &
#           submission_year != "2021"))
#}; nrow(all_data6)

# summary of genera for each institution
gen_summary <- all_data6 %>%
  arrange(genus) %>%
  rename(genera = genus) %>%
  group_by(inst_short) %>%
  mutate(
    genera = paste(unique(genera), collapse = '; ')) %>%
  ungroup() %>%
  distinct(inst_short,genera)
gen_summary
# write file
write.csv(gen_summary, 
          file.path(main_dir, exsitu_dir, standardized_exsitu,
                    paste0("Genera_Institutions_Summary_", Sys.Date(), ".csv")),
          row.names = F)

################################################################################
# 6. Standardize important columns
################################################################################

# add institution metadata
str(inst_data)
  # SELECT COLUMNS YOU WANT ADDED TO ALL DATA:
inst_data <- inst_data %>%
  select(inst_short,inst_country,inst_lat,inst_long,inst_type) %>%
  arrange(inst_lat) %>%
  distinct(inst_short,.keep_all=T) %>%
  arrange(inst_short); inst_data
all_data7 <- left_join(all_data6,inst_data)
str(all_data7)

######################
## A) Provenance type
######################

# save original version of column for reference
all_data7$orig_prov_type <- all_data7$prov_type
all_data7$prov_type <- str_to_lower(all_data7$prov_type)

## IF NEEDED: transfer contents of one column to another column, if data
#   needs to be preserved but is in the wrong place
  # these are used in Genesys; try each and comment out if error is thrown...
#all_data7[which(all_data7$prov_type=="130"|grepl("^130) ",all_data7$prov_type)),]$notes <- "Semi-natural/sown"
all_data7[which(all_data7$prov_type=="410"|grepl("^410) ",all_data7$prov_type)),]$notes <- "Breeding/research material: Breeder's line"
#all_data7[which(all_data7$prov_type=="300"|grepl("^300) ",all_data7$prov_type)),]$notes <- "Traditional cultivar/landrace"
all_data7[which(all_data7$prov_type=="400"|grepl("^400) ",all_data7$prov_type)),]$notes <- "Breeding/research material"
all_data7[which(all_data7$prov_type=="500"|grepl("^500) ",all_data7$prov_type)),]$notes <- "Advanced or improved cultivar (conventional breeding methods)"

### look at column contents and CHANGE THE SECTIONS BELOW AS NEEDED...
sort(unique(all_data7$prov_type))
# standardize the column by searching for keywords & replacing with standard value
  # first remove any confusing words/phrases
all_data7$prov_type <- mgsub(all_data7$prov_type,
  c("not of known wild origin","could be cultivated","not of wild source",
    "wildsourceunsure","cultivated plant of known "), "")
  # ex wild (Z)
all_data7$prov_type <- ifelse(grepl(paste(
  c("indirect","ex wild","^z$","cultivated from wild","c ex w","cw",
    "g from w plant","g ex w","cultivatedpropagatedfromwildsource",
    "(indirect) wild origin"),
  collapse = "|"), all_data7$prov_type),"Z",all_data7$prov_type)
  # wild (W)
all_data7$prov_type <- ifelse(grepl(paste(
  c("wild","wld","collect","^w$","^w\\*","^\\(w\\)$","wd","w\\?","genetic",
    "100","110","130"),
  collapse = "|"), all_data7$prov_type),"W",all_data7$prov_type)
  # native to site (N)
all_data7$prov_type <- ifelse(grepl(paste(
  c("original to site","spontaneous","^n$","existing on site","native"),
  collapse = "|"), all_data7$prov_type),"N",all_data7$prov_type)
  # unknown (U)
all_data7$prov_type <- ifelse(grepl(paste(
  c("^\\(u\\)$","^u$","^u ","unsure","insufficient data","unknown","\\?","un",
    "need to confirm","breeding","400","410","416","donated","999","purchased"),
  collapse = "|"), all_data7$prov_type),"U",all_data7$prov_type)
  # cultivated (H)
all_data7$prov_type <- ifelse(grepl(paste(
  c("cultiva","garden","^c$","^g$","^g ","^h$","horticult","landrace","clone",
    "300","500","^g\\."),
  collapse = "|"), all_data7$prov_type),"H",all_data7$prov_type)
## check one last time to be sure you got everything
sort(unique(all_data7$prov_type))
  # not given (NG) ; everything else
all_data7$prov_type <- ifelse(all_data7$prov_type!= "W" &
  all_data7$prov_type != "Z" & all_data7$prov_type != "H" &
  all_data7$prov_type != "N" & all_data7$prov_type != "U",
  "NG",all_data7$prov_type)
all_data7$prov_type[which(is.na(all_data7$prov_type))] <- "NG"

# check results
table(all_data7$prov_type)

############################
## B) Number of Individuals
############################

## IF NEEDED: change # of individuals for rows that say dead/removed, etc., if
# you have a 'condition' column
#sort(unique(all_data7$condition))
  ## CHANGE BASED ON PHRASE(S) IN YOUR DATA...
#all_data7[which(all_data7$condition=="Dead"),]$num_indiv <- "0"
#all_data7[which(all_data7$num_indiv=="all dead"),]$num_indiv <- "0"

# make everything lowercase; see what you have
all_data7$num_indiv <- str_to_lower(all_data7$num_indiv)
sort(unique(all_data7$num_indiv))

## IF NEEDED: replace unwanted characters
all_data7$num_indiv <- mgsub(all_data7$num_indiv,
    c("\\?",","," plants"," pieces","ca\\.","alive","\\+",
      " in terra",";[0-9][0-9]",";1",";2",";3",";4",";5",";7",";0",";"),
      c(""), fixed=F)
all_data7$num_indiv <- str_squish(all_data7$num_indiv)
sort(unique(all_data7$num_indiv))

# save version where we identify which didn't have # of individuals provided
all_data7$num_indiv[which(!grepl("^[0-9]+$",all_data7$num_indiv))] <- "Unknown"
all_data7$orig_num_indiv <- all_data7$num_indiv

# now we change the type to numeric and replace NA with 1, so we can use this
#   in calculations and when combining duplicate records later
# "NAs introduced by coercion" warning ok
all_data7$num_indiv <- as.numeric(all_data7$num_indiv)
all_data7$num_indiv[which(is.na(all_data7$num_indiv))] <- 1

# check results
sort(unique(all_data7$num_indiv))

# remove records with no individuals (first save as separate file)
no_indiv <- all_data7[which(all_data7$num_indiv == 0),]
nrow(no_indiv)
write.csv(no_indiv, file.path(main_dir, exsitu_dir, standardized_exsitu,
  paste0("ExSitu_Dead_", Sys.Date(), ".csv")),row.names = F)
  # save to in situ data folder as well
if(!dir.exists(file.path(main_dir,occ_dir,raw_occ,"Ex_situ")))
  dir.create(file.path(main_dir,occ_dir,raw_occ,"Ex_situ"),
  recursive=T)
write.csv(no_indiv, file.path(main_dir,occ_dir,raw_occ,"Ex_situ",
  paste0("ExSitu_Dead_", Sys.Date(), ".csv")),row.names = F)
  # remove records with no individuals
all_data7 <- all_data7[which(all_data7$num_indiv > 0),]
nrow(all_data7)

#############################
## C) Latitude and Longitude
#############################

all_data8 <- all_data7

# create temporary ID col for use in this section
all_data8$temp_id <- seq.int(nrow(all_data8))

# preserve original lat and long columns
all_data8$lat_dd <- all_data8$orig_lat
all_data8$long_dd <- all_data8$orig_long

# replace comma with decimal (European notation)
all_data8$lat_dd <- mgsub(all_data8$lat_dd, c(","), ".")
all_data8$long_dd <- mgsub(all_data8$long_dd, c(","), ".")
# separate values if lat and long both ended up in the lat column
all_data8[which(grepl("\\. ",all_data8$lat_dd)),] <-
  separate(all_data8[which(grepl("\\. ",all_data8$lat_dd)),], col = lat_dd,
           into = c("lat_dd","long_dd"), sep = "\\. ", remove = FALSE)

# replace unwanted characters
  ## latitude
  # replace unnecessary characters so we just have numbers
all_data8$lat_dd <- mgsub(all_data8$lat_dd,
  c("N","\\","/","M","A",": ","E","AZ","R","d","a"," \\."," W")," ")
    # remove leading zero
all_data8$lat_dd[which(grepl("^ *[0][1-9]+",all_data8$lat_dd))] <- gsub(
  "^ *[0]","",all_data8$lat_dd[which(grepl("^ *[0][1-9]+",all_data8$lat_dd))])
all_data8$lat_dd[which(grepl("^S *[0][1-9]+",all_data8$lat_dd))] <- gsub(
  "^S *[0]","-",all_data8$lat_dd[which(grepl("^S *[0][1-9]+",all_data8$lat_dd))])
    # add negative sign if south and remove "S"
all_data8$lat_dd[grep("S",all_data8$lat_dd,ignore.case=T)] <-
  paste("-",all_data8$lat_dd[grep("S",all_data8$lat_dd,ignore.case=T)],sep="")
all_data8$lat_dd <- gsub("S","",all_data8$lat_dd)
all_data8$lat_dd <- gsub("--","-",all_data8$lat_dd)
    # remove double spaces or leading/trailing whitespace
all_data8$lat_dd <- str_squish(all_data8$lat_dd)
all_data8$lat_dd[all_data8$lat_dd==""] <- NA
sort(unique(all_data8$lat_dd))
  # can check source of specific values that aren't formatted correctly
#all_data8[which(all_data8$lat_dd == "422538"),]
  ## longitude
  # replace unnecessary characters so we just have numbers
all_data8$long_dd <- mgsub(all_data8$long_dd,
  c("E","\\","/","NR","d","A","a"," .","o","O")," ")
  # remove leading zero
all_data8$long_dd[which(grepl("^ *[0][1-9]+",all_data8$long_dd))] <- gsub(
  "^ *[0]","",all_data8$long_dd[which(grepl("^ *[0][1-9]+",all_data8$long_dd))])
  # add negative sign if west and remove "W"
all_data8$long_dd[which(grepl("^W *[0][1-9]+",all_data8$long_dd))] <- gsub(
  "^W *[0]","-",all_data8$long_dd[which(grepl("^W *[0][1-9]+",
    all_data8$long_dd))])
all_data8$long_dd[grep("W",all_data8$long_dd,ignore.case=T)] <-
  paste("-",all_data8$long_dd[grep("W",all_data8$long_dd,ignore.case=T)],sep="")
all_data8$long_dd <- gsub("W","",all_data8$long_dd)
all_data8$long_dd <- mgsub(all_data8$long_dd,c("--","- "),"-")
  # remove double spaces or leading/trailing whitespace
all_data8$long_dd <- str_squish(all_data8$long_dd)
all_data8$long_dd[all_data8$long_dd==""] <- NA
sort(unique(all_data8$long_dd))

# convert decimal-minutes-seconds (dms) to decimal degrees (dd)
#   [d, m, and s must be in the same cell, with 1 space between each value]
#   format = ## ## ## (DMS) OR ## ##.### (DM)
  # mark rows that need to be converted
convert <- all_data8 %>% filter(grepl(" ",lat_dd) | grepl(" ",long_dd))
  nrow(convert)
no_convert <- anti_join(all_data8, convert)
  # separate by dec_min_sec and deg_dec_min then convert to decimal degrees
    # latitude
unique(convert$lat_dd)
dms <- convert[which(str_count(convert$lat_dd," ") == 2),]; nrow(dms)
ddm <- convert[which(str_count(convert$lat_dd," ") == 1),]; nrow(ddm)
other <- convert[which((str_count(convert$lat_dd," ") != 1 &
  str_count(convert$lat_dd," ") != 2) | is.na(str_count(convert$lat_dd," "))),]
  nrow(other)
dms$lat_dd = measurements::conv_unit(dms$lat_dd, from = 'deg_min_sec', to = 'dec_deg')
ddm$lat_dd = measurements::conv_unit(ddm$lat_dd, from = 'deg_dec_min', to = 'dec_deg')
convert <- rbind(dms,ddm,other); nrow(convert)
    # longitude
unique(convert$long_dd)
dms <- convert[which(str_count(convert$long_dd," ") == 2),]; nrow(dms)
ddm <- convert[which(str_count(convert$long_dd," ") == 1),]; nrow(ddm)
other <- convert[which((str_count(convert$long_dd," ") != 1 &
  str_count(convert$long_dd," ") != 2) | is.na(str_count(convert$long_dd," "))),]
  nrow(other)
dms$long_dd = measurements::conv_unit(dms$long_dd, from = 'deg_min_sec', to = 'dec_deg')
ddm$long_dd = measurements::conv_unit(ddm$long_dd, from = 'deg_dec_min', to = 'dec_deg')
convert <- rbind(dms,ddm,other); nrow(convert)
  # join everything back together
all_data8 <- rbind(no_convert,convert); nrow(all_data8)

# check validity of lat and long
all_data8$lat_dd <- as.numeric(all_data8$lat_dd)
all_data8$long_dd <- as.numeric(all_data8$long_dd)
  # if coords are both 0, set to NA
zero <- which(all_data8$lat_dd == 0 & all_data8$long_dd == 0)
all_data8$lat_dd[zero] <- NA; all_data8$long_dd[zero] <- NA
  # flag non-numeric, not available, and invalid (lat > 90 | lat < -90 |
  #   lon > 180 | lon < -180) coordinates 
coord_test <- cc_val(all_data8, lon = "long_dd",lat = "lat_dd",
  value = "flagged", verbose = TRUE)
  # try switching lat and long for invalid points and check validity again
all_data8[!coord_test,c("lat_dd","long_dd")] <-
  all_data8[!coord_test,c("long_dd","lat_dd")]
coord_test <- cc_val(all_data8,lon = "long_dd",lat = "lat_dd",
  value = "flagged",verbose = TRUE)

# IF the taxon is native to N/S America, make longitude value negative (you need
#   a taxon_region column for this, or could update code to work with countries!)...
#unique(all_data8$taxon_region)
#all_data8 <- all_data8 %>% 
#  mutate(long_dd = if_else(long_dd > 0 & 
#                             # edit these regions to match yours in the Americas:
#                             (taxon_region == "North America" | 
#                              taxon_region == "Central America" |
#                              taxon_region == "South America"),
#                           as.numeric(paste0("-",as.character(long_dd))), long_dd))
# OR, IF ALL YOUR TARGET TAXA ARE NATIVE TO THE AMERICAS, make all longitudes negative:
#   'NAs introduced by coercion' warning message is ok
all_data8 <- all_data8 %>% 
  mutate(long_dd = if_else(long_dd > 0, as.numeric(paste0("-",as.character(long_dd))), long_dd))
         
# make coords NA if they are still flagged
coord_test <- cc_val(all_data8, lon = "long_dd", lat = "lat_dd",
  value = "flagged", verbose = TRUE)
all_data8[!coord_test,"lat_dd"] <- NA
all_data8[!coord_test,"long_dd"] <- NA
nrow(all_data8)

### now we'll work just with the geolocated points for a bit
all_data8$latlong_flag <- ""
have_coord <- all_data8 %>% filter(!is.na(lat_dd) & !is.na(long_dd))
nrow(have_coord)
no_coord <- anti_join(all_data8,have_coord)
no_coord$latlong_country <- ""
# add country-level information to fix potentially-switched lat/longs
  # turn occurrence point data into a spatial object
geo_pts_spatial <- vect(cbind(have_coord$long_dd, have_coord$lat_dd),
  atts=have_coord, crs="+proj=longlat +datum=WGS84")
# read in world countries layer created in 1-prep_gis_layers.R
world_polygons <- vect(file.path(main_dir,gis_dir,"world_countries_10m",
                                 "world_countries_10m.shp"))
# select just the country name column we need
world_polygons <- world_polygons[,"admin"]
# add country polygon data to each point based on lat-long location
geo_pts <- terra::intersect(geo_pts_spatial,world_polygons)
# try switching lat and long for points in Antarctica
on_land <- as.data.frame(geo_pts); nrow(on_land)
on_land[which(on_land$admin == "Antarctica"),c("lat_dd","long_dd")] <-
  on_land[which(on_land$admin == "Antarctica"),c("long_dd","lat_dd")]
head(on_land)
# remove columns from first join and join to countries again
on_land <- on_land %>% select(-admin)
geo_pts_spatial <- vect(cbind(on_land$long_dd,on_land$lat_dd),
  atts=on_land, crs="+proj=longlat +datum=WGS84")
on_land <- as.data.frame(terra::intersect(geo_pts_spatial,world_polygons))
on_land <- on_land %>% rename(latlong_country = admin)
nrow(on_land)
# check if points are in water and mark
  # get points that have coords but didn't fall in a country;
  # these are in the water
land_id <- unique(on_land$temp_id)
in_water <- have_coord %>% filter(!(temp_id %in% land_id))
in_water <- as.data.frame(in_water)
if(nrow(in_water >0)){
  in_water$latlong_flag <- "Given lat-long is in the water" 
  in_water$latlong_country <- "" }
nrow(in_water)
# bind all the points back together
all_data9 <- rbind(on_land,in_water,no_coord)
nrow(all_data9)

# mark lat-long for records with same inst lat-long and wild lat-long
all_data9$lat_round <- round(all_data9$lat_dd,digits=1)
all_data9$long_round <- round(all_data9$long_dd,digits=1)
all_data9$inst_lat_round <- round(all_data9$inst_lat,digits=1)
all_data9$inst_long_round <- round(all_data9$inst_long,digits=1)
garden_latlong <- all_data9 %>% filter(lat_round == inst_lat_round &
  long_round == inst_long_round & prov_type != "N")
unique(garden_latlong$inst_short)
nrow(garden_latlong)
all_data9[which(all_data9$temp_id %in% garden_latlong$temp_id),]$latlong_flag <-
  "Given lat-long is at institution, use only if native to grounds"
  # you can make lat-long NA for these without checking them first, if desired:
#all_data9[all_data9$UID %in% garden_latlong$UID,]$lat_dd <- NA
#all_data9[all_data9$UID %in% garden_latlong$UID,]$long_dd <- NA
table(all_data9$latlong_flag) 

# add latlong_det (latlong determination) column
all_data9$latlong_det[which(all_data9$prov_type == "H")] <- "N/A (horticultural)"
all_data9$latlong_det[which(!is.na(all_data9$lat_dd) &
  !is.na(all_data9$long_dd))] <- "Given in original record"
all_data9$latlong_det[which(all_data9$latlong_det == "")] <- NA
table(all_data9$latlong_det)

# where prov_type is "N/A (horticultural)" but lat-long is given, change to "H?"
  # create new prov type column
all_data9$prov_type[which(all_data9$latlong_det == "Given in original record" &
  all_data9$prov_type == "H")] <- "H?"
table(all_data9$prov_type)

#######################
## D) Collection year
#######################

# this is not usually vital and takes some effort depending on your dataset;
# if you decide it's not necessary to standardize collection year, just comment 
# out this section; if you do want to standardize, you'll need to play with 
# updating the following section for your data

sort(unique(all_data9$coll_year))
# separate additional years
all_data9 <- all_data9 %>% 
  separate("coll_year","coll_year",sep="; |;",remove=F)

## IF NEEDED: replace non-year words/characters
all_data9$coll_year <- mgsub(all_data9$coll_year,
  c("----","about ","ca.","Unknown","original","Estate","estate","<",
    "NEAR ","PRE "),"")
all_data9$coll_year[all_data9$coll_year == ""] <- NA
  # change all month/day separators to a slash
all_data9$coll_year <- gsub("-|\\.","/",all_data9$coll_year)
sort(unique(all_data9$coll_year))

# remove extra elements so its just year
## YOU NEED TO BE CAREFUL HERE AND LOOK AT YOUR SPECIFIC DATA
  # remove 2-digit month and day when year is first
all_data9$coll_year <- gsub("/[0-9][0-9]/[0-9][0-9]$","",all_data9$coll_year)
  # remove 2-digit month and day when year is last
all_data9$coll_year <- gsub("^[0-9][0-9]/[0-9][0-9]/","",all_data9$coll_year)
  # etc.....
#all_data9$coll_year <- gsub("^[0-9]/[0-9][0-9]/","",all_data9$coll_year)
#all_data9$coll_year <- gsub("^[0-9][0-9]/[0-9]/","",all_data9$coll_year)
#all_data9$coll_year <- gsub("^[0-9]/[0-9]/","",all_data9$coll_year)
#all_data9$coll_year <- gsub("^[0-9][0-9]/[A-Z][a-z][a-z]/","",all_data9$coll_year)
#all_data9$coll_year <- gsub("^[0-9]/[A-Z][a-z][a-z]/","",all_data9$coll_year)
#all_data9$coll_year <- gsub("^[0-9][0-9]/","",all_data9$coll_year)
#all_data9$coll_year <- gsub("/[0-9][0-9]$","",all_data9$coll_year)
#all_data9$coll_year <- gsub("^[0-9][0-9] ","",all_data9$coll_year)
#all_data9$coll_year <- gsub("^[0-9] ","",all_data9$coll_year)
  # keep only the first four characters
  #   1 = first character to keep; 4 = last chracter to keep
all_data9$coll_year <- substr(all_data9$coll_year, 1, 4)

# make column numeric
all_data9$coll_year <- as.numeric(all_data9$coll_year)

## IF NEEDED: add first two numbers in year [this cannot be fully accurate]
#  # assume 2000s if values is less than 23
#all_data9$coll_year[which(all_data9$coll_year < 10)] <-
#  paste0("200",as.character(all_data9$coll_year[which(all_data9$coll_year < 10)]))
#all_data9$coll_year <- as.numeric(all_data9$coll_year)
#all_data9$coll_year[which(all_data9$coll_year < 23)] <-
#  paste0("20",as.character(all_data9$coll_year[which(all_data9$coll_year < 23)]))
#all_data9$coll_year <- as.numeric(all_data9$coll_year)
#  # assume 1900s if values is greater than or equal to 23
#all_data9$coll_year[which(all_data9$coll_year < 100)] <-
#  paste0("19",as.character(all_data9$coll_year[which(all_data9$coll_year < 100)]))
#all_data9$coll_year <- as.numeric(all_data9$coll_year)
sort(unique(all_data9$coll_year))

#####################
## E) Lineage number
#####################

# remove lin_num when same as acc_num
all_data9[which(all_data9$acc_num == all_data9$lin_num),]$lin_num <- NA

###########################
## F) Locality description
###########################

# create all_locality column (concatenating all locality data into one column 
#   for easy reference, especially when geolocating)
all_data9$latitude <- round(all_data9$lat_dd,digits=4)
all_data9$longitude <- round(all_data9$long_dd,digits=4)
all_data9 <- unite(all_data9, "all_locality",
  c(locality,municipality,county,state,country,orig_source,
    lin_num,coll_num,coll_name,coll_year,
    latitude,longitude,notes),sep = " | ",remove = F)
# remove NA in concatenated locality column
all_data9$all_locality <- gsub("NA","",all_data9$all_locality)
# if no locality info at all, make it NA
all_data9$all_locality[which(all_data9$all_locality ==
  " |  |  |  |  |  |  |  |  |  |  |  | ")] <- NA
head(all_data9$all_locality)

#######################
## G) Institution type
#######################

# add inst_type for gene bank data
all_data9$inst_type[which(is.na(all_data9$inst_type))] <- "Gene/Seed Bank"
unique(all_data9$inst_type)
table(all_data9$inst_type)

# check data_source column too
table(all_data9$data_source)

##################################################################
## H) Combine individuals (same institution and accession number)
##      so that everything is (hopefully) at the accession-level
##################################################################

## some institutions provided data at the accession level and some provided it
## at the individual level; we want everything at the accession level for
## later analyses, so we will try to combine individuals from the same 
## accession; this can be tricky...

# preserve original acc_num before removing individual signifiers in a minute
all_data9$orig_acc_num <- all_data9$acc_num

## KEEP ONLY NECESSARY COLUMNS
#   change this as needed based on the columns you want;
keep_col <- c(
  # key data
  "UID","inst_short","taxon_name_accepted","acc_num","prov_type",
  # locality
  "lat_dd","long_dd","latlong_flag","latlong_det","latlong_uncertainty",
  "geolocated_by","latlong_notes",
  "all_locality","locality","municipality","county","state","country",
  "latlong_country","assoc_sp",#"habitat",
  # source
  "orig_source","lin_num","coll_name","coll_num","coll_year",
  # material info
  "num_indiv","germ_type","garden_loc","rec_as",
  # other metadata
  "notes","filename","submission_year","data_source",#"condition","dataset_year","private",
  # taxon name details
  "taxon_name","taxon_full_name_orig","taxon_full_name_concat",
  "genus","species","infra_rank","infra_name","hybrid","cultivar",
  "taxon_name_status","taxon_verif",#"author","trade_name",
  # institution metadata
  "inst_country","inst_lat","inst_long","inst_type",
  # original versions of columns, for reference
  "orig_prov_type","orig_acc_num","orig_num_indiv","orig_lat","orig_long",
  # OPTIONAL additional taxon metadata
  "rl_category","ns_rank","elevation_range"#,"taxon_region"
)
all_data9 <- all_data9[,keep_col]

# save version without duplicates combined, in cases needed for reference
write.csv(all_data9, file.path(main_dir, exsitu_dir, standardized_exsitu,
                              paste0("ExSitu_Compiled_No-Dups-Combined_", 
                                     Sys.Date(), ".csv")),row.names = F)

# fill any spaces in acc_num with dashes
all_data9$acc_num <- gsub(" ","-",all_data9$acc_num)

# combine duplicates (same acc_num) - this shouldn't happen but can
all_data9 <- all_data9 %>%
  group_by(inst_short,acc_num,taxon_name_accepted) %>%
  mutate(num_indiv = sum(as.numeric(num_indiv)),
         orig_num_indiv = paste(unique(orig_num_indiv),collapse="; "),
         germ_type = paste(unique(germ_type),collapse="; "),
         garden_loc = paste(unique(garden_loc),collapse="; "),
         orig_acc_num = paste(unique(orig_acc_num),collapse="; "),
         all_locality = paste(unique(all_locality),collapse="; ")) %>%
  ungroup() %>%
  distinct(inst_short,acc_num,taxon_name_accepted,.keep_all=T)
nrow(all_data9)

# now we will try to find patterns that identify accession numbers
# with individual identifiers - we want everything to be at the
# accession level, not at the individual level for our analyses

# function to remove individual-specific identifiers (to combine dup accessions)
# "Expected 1 pieces" warning here is fine
combine_acc_dups <- function(df,char_cutoff,pattern){
  # create subset of records with acc_num greater than or equal to X characters
  check_accnum <- df[which(nchar(df$acc_num)>=char_cutoff),]
  print(nrow(check_accnum))
  no_check_accnum <- setdiff(df,check_accnum)
  print(nrow(no_check_accnum))
  # combine duplicates using the pattern provided to cut off characters
  check_accnum <- check_accnum %>%
    separate("acc_num","acc_num",sep=pattern,remove=F) %>%
    group_by(inst_short,acc_num,taxon_name_accepted) %>%
    mutate(num_indiv = sum(as.numeric(num_indiv)),
           orig_num_indiv = paste(unique(orig_num_indiv),collapse="; "),
           germ_type = paste(unique(germ_type),collapse="; "),
           garden_loc = paste(unique(garden_loc),collapse="; "),
           orig_acc_num = paste(unique(orig_acc_num),collapse="; "),
           all_locality = paste(unique(all_locality),collapse="; ")) %>%
    ungroup() %>%
    distinct(inst_short,acc_num,taxon_name_accepted,.keep_all=T)
  # create and return full data frame of checked and unchecked rows
  df_return <- full_join(check_accnum,no_check_accnum)
  return(df_return)
}

### !! THE NEXT SECTION NEEDS TO BE THOROUGHLY REVEIWED AND 
###  EDITED TO FIT YOUR DATASET !!
### Otherwise you will combine records that do not belong together

# first check the pattern you're using to identify the part of the acc_num that 
#   represents an individual; for example, the row below should show accession
#   numbers with a star, which often comes before an individual identifier, 
#   e.g., 1987-275*A  2021-56783-11*B  --> these are good examples; the pattern
#   may be "bad" (remove parts of the acc_num that we need), e.g., 23*1988
#   if you spot something that doesn't work, you need to skip or edit your pattern
all_data9[which(grepl("\\*",all_data9$acc_num)),]$acc_num
# now combine duplicates using the pattern you just checked...
#   the second input in the function is the minimum number of characters the 
#   acc_num needs for the pattern to be followed and dups combined; this is 
#   helpful because usually the longer accession numbers have individual
#   identifiers; for example, if you had 1987-275*A and 23*1988 show up when you
#   ran the line above, then you would want to choose 8 as the second input, so
#   that everything after the * is removed in 1987-275*A and 23*1988 is left
#   as-is since it does not have an individual identifier
all_data9 <- combine_acc_dups(all_data9,0,"\\*")
# now we'll repeat this (checking what will be combined then combining) for 
#   additional patterns and cutoffs
sort(all_data9[which(grepl("[A-F]$",all_data9$acc_num)),]$acc_num)
  all_data9 <- combine_acc_dups(all_data9,9,"[A-F]$"); nrow(all_data9)
sort(all_data9[which(grepl("-[1-9]$",all_data9$acc_num)),]$acc_num)
  all_data9 <- combine_acc_dups(all_data9,9,"-[1-9]$"); nrow(all_data9)
sort(all_data9[which(grepl("-[0-9][0-9]$",all_data9$acc_num)),]$acc_num)
  all_data9 <- combine_acc_dups(all_data9,9,"-[0-9][0-9]$"); nrow(all_data9)
sort(all_data9[which(grepl("-[0][0][0-9]$",all_data9$acc_num)),]$acc_num)
 all_data9 <- combine_acc_dups(all_data9,9,"-[0][0][0-9]$"); nrow(all_data9)
sort(all_data9[which(grepl("/[0-9]$",all_data9$acc_num)),]$acc_num)
 all_data9 <- combine_acc_dups(all_data9,0,"/[0-9]$"); nrow(all_data9)
sort(all_data9[which(grepl("/[0-9][0-9]$",all_data9$acc_num)),]$acc_num)
  all_data9 <- combine_acc_dups(all_data9,9,"/[0-9][0-9]$"); nrow(all_data9)
sort(all_data9[which(grepl("\\.[0-9]$",all_data9$acc_num)),]$acc_num)
  all_data9 <- combine_acc_dups(all_data9,0,"\\.[0-9]$"); nrow(all_data9)
sort(all_data9[which(grepl("\\.[0-9][0-9]$",all_data9$acc_num)),]$acc_num)
  all_data9 <- combine_acc_dups(all_data9,9,"\\.[0-9][0-9]$"); nrow(all_data9)
sort(all_data9[which(grepl("\\.[0-9][0-9][0-9]$",all_data9$acc_num)),]$acc_num)
  all_data9 <- combine_acc_dups(all_data9,9,"\\.[0-9][0-9][0-9]$"); nrow(all_data9)
# etc...[add additional patterns if needed]
  
# remove ending separation characters 
all_data9$acc_num <- mgsub(all_data9$acc_num,
                              c("/$","-$","\\.$"),"",fixed=F)
unique(all_data9$acc_num)
nrow(all_data9)

##############################
## I) Add Universal ID column
##############################

# create UID with institution name, accession number, provenance type, and 
# taxon name; also remove duplicates based on new UID and sums individuals
  # keep native UIDs from Genesys
need_id <- all_data9[which(is.na(all_data9$UID)),]
dont_need_id <- all_data9[which(!is.na(all_data9$UID)),]
nms <- names(need_id)
nrow(need_id)
  # create UID
need_id <- need_id %>%
  arrange(orig_lat,locality) %>%
  mutate(UID = paste(inst_short,acc_num,prov_type,taxon_name_accepted,sep="~")) %>%
  group_by(UID) %>%
  mutate(num_indiv = sum(as.numeric(num_indiv))) %>%
  distinct(UID,.keep_all=T) %>%
  ungroup() %>%
  select(c("UID",all_of(nms)))
all_data9 <- rbind(need_id,dont_need_id)
nrow(all_data9)

################################################################################
# 7. Save output
################################################################################

## SELECT ONLY THE COLUMNS YOU WANT one last time (see keep_col object created 
##   above)
data_sel <- all_data9[,keep_col]

# write file; if you don't want to geolocate or check flagged records, this
#   will be your final dataset
write.csv(data_sel, file.path(main_dir, exsitu_dir,standardized_exsitu,
  paste0("All_ExSitu_Compiled_", Sys.Date(), ".csv")),row.names = F)

# [OPTIONAL] regional subsets for easier use if lots of data:
#meso <- data_sel %>% filter(grepl("Mesoamerica",taxon_region))
#nrow(meso) #6514
#write.csv(meso, file.path(main_dir, exsitu_dir,standardized_exsitu,
#  paste0("Mesoamerican-spp_ExSitu_Compiled_", Sys.Date(), ".csv")),
#  row.names = F)


################################################################################
# 8. [OPTIONAL] Explore georeferencing needs and save file for geolocation
################################################################################

# if desired, you can try to manually find the latitude and longitude for 
# records with wild collection locality descriptions; the remaining sections
# help with this process

### explore georeferencing needs
# table with...
#   species name
#   num wild acc
#   num non-H acc w/ coords
#   num non-H acc with no coords & yes locality info
geo_needs <- data_sel %>%
  group_by(taxon_name_accepted) %>%
  summarize(
    num_acc = sum(!is.na(taxon_name_accepted)),
    num_wild = sum(prov_type == "W"),
    NotH_YesCoords = sum(!is.na(lat_dd) & prov_type != "H"),
    NotH_NoCoords_YesLocality = sum(is.na(lat_dd) & 
                                      !is.na(all_locality) & 
                                      prov_type != "H"),
    Percent_NonH_NeedGeo = (sum(is.na(lat_dd) & 
                                  !is.na(all_locality) & 
                                  prov_type != "H") 
                            / sum(prov_type != "H")*100)
  )
head(geo_needs,n=20)
# write file
write.csv(geo_needs, file.path(main_dir, exsitu_dir,standardized_exsitu,
  paste0("ExSitu_Geolocation_Needs_Summary_", Sys.Date(), ".csv")),
  row.names = F)

# select records that may need geolocation
#   (no lat-long, yes locality, prov type not H)
#   (also add flagged records: water or at institution)
need_geo <- data_sel %>%
  filter((is.na(lat_dd) & prov_type != "H" &
            !is.na(all_locality) & all_locality != "NA") |
         latlong_flag!="")
nrow(need_geo)

# optionally,
# condense all_locality duplicates so you only need to geolocate each locality 
#   once; you can skip this if it's too confusing (does temporarily combine 
#   records that could be for different taxa)
need_geo <- need_geo %>%
  group_by(prov_type,lat_dd,long_dd,latlong_det,all_locality) %>%
  mutate(UID = paste0(UID,collapse=" | "),
         inst_short = paste0(unique(inst_short),collapse=" | "),
         taxon_name_accepted = paste0(unique(taxon_name_accepted),
                                      collapse=" | ")) %>%
  ungroup() %>%
  distinct(UID,inst_short,taxon_name_accepted,prov_type,lat_dd,long_dd,
           latlong_uncertainty,latlong_det,geolocated_by,latlong_notes,
           all_locality,county,state,country,latlong_flag)

# select just the columns we need for geolocating
need_geo <- need_geo %>%
  select(UID,inst_short,taxon_name_accepted,prov_type,lat_dd,long_dd,
         latlong_uncertainty,latlong_det,geolocated_by,latlong_notes,
         all_locality,county,state,country,latlong_flag)
nrow(need_geo)
head(need_geo)

# optionally, flag records for taxa that are highest priority for geolocating;
#   e.g., threatened and/or have less than 15 wild accessions
#   (you can choose whatever threshold(s) you want)
  # thresholds
rl_threat <- c("Critically Endangered","Endangered","Vulnerable")
ns_threat <- c("G1 (Critically Imperiled)","G2 (Imperiled)","G3 (Vulerable)")
few_wild <- geo_needs[geo_needs$num_wild<15,]$taxon_name_accepted
  # get list of priority taxa
priority_taxa <- taxon_list %>%
  filter(rl_category %in% rl_threat |
         ns_rank %in% ns_threat |
         taxon_name %in% few_wild) %>%
  distinct(taxon_name_accepted)
priority_taxa <- priority_taxa[,1]
priority_taxa <- paste(priority_taxa, collapse="|")
  # flag priority taxa
need_geo$priority <- NA
need_geo[which(grepl(priority_taxa,need_geo$taxon_name_accepted)),]$priority <- "Priority"
table(need_geo$priority)

# replace NA with "" for easier viewing when geolocating
need_geo$lat_dd <- as.character(need_geo$lat_dd)
need_geo$long_dd <- as.character(need_geo$long_dd)
need_geo[is.na(need_geo)] <- ""

# write file
write.csv(need_geo, file.path(main_dir, exsitu_dir,standardized_exsitu,
  paste0("ExSitu_Need_Geolocation_", Sys.Date(), ".csv")),row.names = F)

### NOW MANUALLY GEOLOCATE !
### INSTRUCTIONS FOR GEOLOCATING:
### https://docs.google.com/document/d/1RBUD6-ogLc7PRVkDJSIEKzkgvC6xekj9Q_kl1vzxhCs/edit?usp=share_link

### When you're done geolocating, save your file with the same name but add 
### "_Geolocated" to the end
### We will read this in next!

################################################################################
# 9. (If you geolocated) Add geolocated data, after manual geolocation;
#    save final output file
################################################################################

# read in all compiled ex situ data (exported above)
exsitu <- read.csv(file.path(main_dir, exsitu_dir, standardized_exsitu,
  "All_ExSitu_Compiled_2023-07-10.csv"), #change this to your version!
  header = T, colClasses="character", na.strings = c("NA",""))

# read in geolocated dataset
geo_raw <- read.csv(file.path(main_dir, exsitu_dir, standardized_exsitu,
  "ExSitu_Need_Geolocation_2023-07-10_Geolocated.csv"), #change this to your version!
  header = T, colClasses="character", na.strings = c("NA",""))
head(geo_raw)
  # check this is just NA, meaning no "priority" records are not geolocated
unique(geo_raw[which(is.na(geo_raw$latlong_det)),"priority"])

# add geolocated coordinates to ex situ data
  # separate UID row
geolocated <- separate_rows(geo_raw, UID, sep=" \\| ")
  # keep only edited columns and records that have latlong_det filled in
geolocated <- geolocated %>%
  select(UID,prov_type,lat_dd,long_dd,latlong_uncertainty,latlong_det,
         geolocated_by,latlong_notes) %>%
  filter(!is.na(latlong_det))
head(geolocated)
table(geolocated$latlong_det)
  # select geolocated rows in full dataset and remove cols we want to add
exsitu_geo <- exsitu %>%
  filter(UID %in% geolocated$UID) %>%
  select(-prov_type,-lat_dd,-long_dd,-latlong_uncertainty,-latlong_det,
         -geolocated_by,-latlong_notes)
    # these two values should be the same:
nrow(exsitu_geo)
nrow(geolocated)
  # add geolocation data; should just print "Joining with `by = join_by(UID)`"
exsitu_geo <- full_join(exsitu_geo,geolocated)
  # join geolocated rows with rest of ex situ rows
exsitu_no_geo <- exsitu %>%
  filter(!(UID %in% exsitu_geo$UID))
nrow(exsitu_no_geo)
exsitu_all <- bind_rows(exsitu_no_geo,exsitu_geo)
nrow(exsitu_all)
table(exsitu_all$latlong_det)

## FINAL VERSION: SELECT ONLY THE COLUMNS YOU WANT
# this is the same as the version created above in step 6H, just copying here
#   for easy access if not running the whole script at once
keep_col <- c(
  # key data
  "UID","inst_short","taxon_name_accepted","acc_num","prov_type",
  # locality
  "lat_dd","long_dd","latlong_flag","latlong_det","latlong_uncertainty",
  "geolocated_by","latlong_notes",
  "all_locality","locality","municipality","county","state","country",
  "latlong_country","assoc_sp",#"habitat",
  # source
  "orig_source","lin_num","coll_name","coll_num","coll_year",
  # material info
  "num_indiv","germ_type","garden_loc","rec_as",
  # other metadata
  "notes","filename","submission_year","data_source",#"condition","dataset_year","private",
  # taxon name details
  "taxon_name","taxon_full_name_orig","taxon_full_name_concat",
  "genus","species","infra_rank","infra_name","hybrid","cultivar",
  "taxon_name_status","taxon_verif",#"author","trade_name",
  # institution metadata
  "inst_country","inst_lat","inst_long","inst_type",
  # original versions of columns, for reference
  "orig_prov_type","orig_acc_num","orig_num_indiv","orig_lat","orig_long",
  # OPTIONAL additional taxon metadata
  "rl_category","ns_rank","elevation_range"#,"taxon_region"
)
exsitu_all <- exsitu_all[,keep_col]

# write final file
write.csv(exsitu_all, file.path(main_dir, exsitu_dir, standardized_exsitu,
  paste0("FINAL_ExSitu_Compiled_Post-Geolocation_", Sys.Date(), ".csv")), 
  row.names = F)
# write to in situ folder also
write.csv(exsitu_all, file.path(main_dir, occ_dir, raw_occ, "Ex_situ",
  paste0("ExSitu_Compiled_Post-Geolocation_", Sys.Date(), ".csv")), 
  row.names = F)











### THIS SECTION IS OLD AND HAS NOT BEEN UPDATED SINCE ~2021 !!!
### it goes between step 8 and 9 above, if using

################################################################################
# 8.5 (Optionally) Create file(s) for GEOLocate (https://www.geo-locate.org),
#     which needs specific column headers
################################################################################

# read in data in again, if you didn't just run the whole script
#need_geo <- read.csv(file.path(main_dir,"Ex-situ_data","OUTPUTS_FROM_R",
#  "ExSitu_Need_Geolocation_2022-10-14.csv"), header = T, colClasses="character")

# add GEOLocate standard columns
need_geo$correction.status <- NA
need_geo$precision <- NA
need_geo$error.polygon <- NA
need_geo$multiple.results <- NA
need_geo$latitude <- NA
need_geo$longitude <- NA

# update column name and order for GEOLocate requirements
geolocate <- need_geo %>%
  # rename to GEOLocate standard columns
  rename(locality.string = all_locality) %>%
  # order records alphabetically
  arrange(taxon_name_acc,locality.string) %>%
  # reorder columns
  select(
    ## GeoLocate
    locality.string,country,state,county,latitude,longitude,
    correction.status,precision,error.polygon,multiple.results,uncertainty,
    ## metadata to include also
    taxon_name_acc,inst_short,prov_type,latlong_det,geolocated_by,latlong_notes,UID) %>%
  # replace NA with "" to make simpler to view in GEOLocate
  replace(., is.na(.), "")
head(geolocate)

# remove dot in column names (replace with space) for GEOLocate
names(geolocate) <- gsub(x = names(geolocate),pattern = "\\.",replacement = " ")
str(geolocate)
head(as.data.frame(geolocate))

# split by species, so each species has separate file;
#   this step makes it easier to use the GEOLocate tool because it doesn't
#   handle lots of records very well

# create one CSV for each target species
# !!! this doesn't work well if you combined species above to remove locality duplicates !!!
sp_split <- split(geolocate, as.factor(geolocate$taxon_name_acc))
names(sp_split) <- gsub(" ","_",names(sp_split))

# write files
if(!dir.exists(file.path(main_dir,"Ex-situ_data","OUTPUTS_FROM_R","files_for_GEOLocate")))
  dir.create(file.path(main_dir,"Ex-situ_data","OUTPUTS_FROM_R","files_for_GEOLocate"),recursive=T)
lapply(seq_along(sp_split), function(i) write.csv(sp_split[[i]],
  file.path(main_dir,"Ex-situ_data","OUTPUTS_FROM_R","files_for_GEOLocate",
  paste0(names(sp_split)[[i]],".csv")),row.names = F))


### !!!
### ! NOW GO GEOLOCATE !
### !!!

## YOU'LL ALSO NEED TO EDIT SECION 9 TO FIT YOUR GeoLocate OUTPUT !!








# Other random old code chunk:

### TESTING GEONAMES PACKAGE ###

# read in compiled ex situ data
df <- read.csv(file.path(main_dir,"outputs",
  "exsitu_compiled_standardized_2021-02-11_firstpasslatlongDet.csv"),
  header = T, colClasses="character")
str(df)
unique(df$latlong_det)
tail(df[which(df$latlong_det == "L"),])

# create login: https://www.geonames.org/login
# https://www.geonames.org/manageaccount
#   the account is not yet enabled to use the free web services. Click here to enable.
usethis::edit_r_environ()
devtools::install_github("ropensci/geonames")
library(geonames)

# https://rstudio-pubs-static.s3.amazonaws.com/489236_0259d8532a354ad6945d818bc4c052f1.html

login <- read_lines(log_loc)
username  <- login[4]
options(geonamesUsername = username)
head(GNsearch(name = "Chicago"))
#  adminCode1       lng geonameId               toponymName countryId fcl population countryCode                      name           fclName adminCodes1.ISO3166_2   countryName                                      fcodeName adminName1      lat fcode
#1         IL -87.65005   4887398                   Chicago   6252001   P    2720546          US                   Chicago city, village,...                    IL United States seat of a second-order administrative division   Illinois 41.85003 PPLA2
#2         IL -87.89062   6955104 Chicago-Naperville-Joliet   6252001   L    9569624          US Chicago-Naperville-Joliet   parks,area, ...                    IL United States                                economic region   Illinois 41.70778  RGNE
#3         IL  -87.6979  12070033 Chicago metropolitan area   6252001   L    9472676          US Chicago metropolitan area   parks,area, ...                    IL United States                                         region   Illinois  41.8902   RGN
#4         OH -82.72629   5176830                   Willard   6252001   P       6063          US                   Willard city, village,...                    OH United States                                populated place       Ohio 41.05311   PPL
#5         IL -87.69644   4887463              Chicago Lawn   6252001   P      55551          US              Chicago Lawn city, village,...                    IL United States                     section of populated place   Illinois 41.77503  PPLX
#6         IL -87.55425   4911863             South Chicago   6252001   P      28095          US             South Chicago city, village,...                    IL United States                                populated place   Illinois 41.73977   PPL

lanc_coords <- lanc_df[1, c("lng", "lat")]
