### 1-get_taxa_metadata.R
### Authors: Emily Beckman Bruns & Shannon M Still
### Supporting institutions: The Morton Arboretum, Botanic Gardens Conservation 
#   International-US, United States Botanic Garden, San Diego Botanic Garden,
#   Missouri Botanical Garden, UC Davis Arboretum & Botanic Garden
### Funding: Base script funded by the Institute of Museum and Library 
#   Services (IMLS MFA program grant MA-30-18-0273-18 to The Morton Arboretum).
#   Moderate edits were added with funding from a cooperative agreement
#   between the United States Botanic Garden and San Diego Botanic Garden
#   (subcontracted to The Morton Arboretum), and NSF ABI grant #1759759
### Last Updated: June 2023 ; first written Dec 2020
### R version 4.3.0

### DESCRIPTION:
  ## This script gets IUCN Red List and BGCI GlobalTreeSearch native countries 
  # of occurrence for taxa in your target taxa list. These are used later in 
  # the workflow to filter points.
  ## The IUCN Red List threat category is also matched to your target taxa list

### INPUTS:
  ## target_taxa_with_synonyms.csv
  #   List of target taxa and synonyms; see example in the "Target taxa list"
  #   tab in Gap-analysis-workflow_metadata workbook; Required columns include: 
  #   taxon_name, taxon_name_accepted, and taxon_name_status (Accepted/Synonym)
  ## IUCN Red List (RL) threat categories & native country data
  #   See script below for download instructions
  ## BGCI GlobalTreeSearch (GTS) native country data
  #   See script below for download instructions

### OUTPUTS:
  ## target_taxa_with_synonyms.csv
  #   This script adds 13 columns to your input target_taxa_with_synonyms.csv;
  #   See details in the "Target taxa list" tab in 
  #   Gap-analysis-workflow_metadata workbook

################################################################################
# Load libraries
################################################################################

my.packages <- c('tidyverse','countrycode','textclean')
  # versions I used (in the order listed above): 2.0.0, 1.5.0, 0.9.3
#install.packages(my.packages) # turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
  rm(my.packages)

################################################################################
# Set working directory
################################################################################

# use 0-set_working_directory.R script:
source("/Users/emily/Documents/GitHub/conservation-gap-analysis/spatial-analysis-workflow/0-set_working_directory.R")
  
################################################################################
# Read in target taxa and create folder for outputs
################################################################################

# read in taxa list
taxon_list <- read.csv(file.path(main_dir,taxa_dir,"target_taxa_with_synonyms.csv"), 
                       header=T, colClasses="character",na.strings=c("","NA"))
nrow(taxon_list)
# see target genus/genera name(s) - you'll use these in a minute
unique(separate(taxon_list,taxon_name,into="genus",extra="drop")[1])

# in case you're running this again, just keep the original columns:
taxon_list <- taxon_list %>% 
    select(taxon_name,taxon_name_accepted,taxon_name_status,
           ns_rank, ns_taxon_name #add any other manually-added columns here
           ) 

# create folder for files used in / created by this script
output_dir <- "taxa_metadata"
if(!dir.exists(file.path(main_dir, taxa_dir, output_dir)))
  dir.create(file.path(main_dir, taxa_dir, output_dir), recursive=T)

################################################################################
# Get IUCN Red List (RL) category and native country data
################################################################################

# FIRST, download raw data
  # Go to https://www.iucnredlist.org/search
  # Click "Login/Register" in top bar; create an account if you don't have one,
  #   then log in to your account
  # Open the "Taxonomy" tab in the left bar
  #   Either search for your target genus/genera or simply check "Plantae" to 
  #   download data for all plant species that have assessments globally (~60MB)
  #   You can also search by Family/families if preferred.
  #   You can limit the search further using the other tabs, as desired,
  #   but further refinement can sometimes exclude assessments you want.
  # When you're ready, on the right click "Download" then "Search Results"
  # You will receive an email when your download is ready
  # Next, go to your account (https://www.iucnredlist.org/account)
  #   Under "Saved downloads" click "Download" for your recent search
  # Move the downloaded folder to your "taxa_metadata" folder and rename it
  #   simply "redlist_species_data"
## Note that there is also an API you can use via the rredlist package, that 
#   has a 'rl_occ_country' function to get the occurrence countries; I had 
#   various issues with the API so decided to do it manually for now, but you
#   could test the function if desired.

## RED LIST CATEGORY

# read in downloaded RL data that has threat categories
category <- read.csv(file.path(main_dir,taxa_dir,output_dir,
                               "redlist_species_data","simple_summary.csv"),
                     colClasses="character",na.strings=c("","NA"),strip.white=T)
# keep just the threat category data
category <- category %>% 
  select(scientificName,redlistCategory) %>%
  rename(taxon = scientificName, rl_category = redlistCategory)

## COUNTRIES OF OCCURRENCE

# read in downloaded RL data for country-level species distribution
countries <- read.csv(file.path(main_dir,taxa_dir,output_dir,
                                "redlist_species_data","countries.csv"),
                      colClasses="character",na.strings=c("","NA"),strip.white=T)

# condense output so its one entry per species
countries_c <- countries %>%
  filter(presence != "Extinct Post-1500") %>%
  rename(taxon = scientificName) %>%
  arrange(code) %>%
  group_by(taxon,origin) %>%
  mutate(
    rl_native_dist_iso2 = paste(code, collapse = '; '),
    rl_native_dist = paste(name, collapse = '; ')) %>%
  ungroup() %>%
  select(taxon,origin,rl_native_dist_iso2,rl_native_dist) %>%
  distinct(taxon,origin,.keep_all=T)

# separate native dist countries from introduced dist countries
rl_native <- countries_c %>% filter(origin == "Native")
rl_native$rl_taxon_name <- rl_native$taxon
rl_introduced <- countries_c %>% filter(origin == "Introduced")
names(rl_introduced)[3] <- "rl_introduced_dist_iso2"
names(rl_introduced)[4] <- "rl_introduced_dist"
rl_introduced$rl_taxon_name <- rl_introduced$taxon
# join both native and introduced together
rl_list <- full_join(rl_native[,c(1,4,3)],rl_introduced[,c(1,4,3,5)])

# create RL dataframe for joining to our taxa list
rl_add <- rl_list %>%
  select(taxon,rl_native_dist,rl_native_dist_iso2,rl_introduced_dist,
         rl_introduced_dist_iso2,rl_taxon_name)
# join RL category and country datasets together
rl_add <- full_join(category,rl_add)
rl_add$rl_taxon_name <- rl_add$taxon
head(rl_add)

# see which of your target taxa have RL data or not
# match to taxon names
taxon_test <- left_join(taxon_list, rl_add, by=c("taxon_name" = "taxon"))
head(taxon_list)
  # see which of your accepted target taxa have no RL data
no_match <- taxon_test[
  which(is.na(taxon_test$rl_native_dist) &
          taxon_test$taxon_name_status == "Accepted"),]$taxon_name_accepted
no_match
  # see if any of your synonyms matched RL data
syn_match <- taxon_test[
  which(!is.na(taxon_test$rl_native_dist) &
          taxon_test$taxon_name_status == "Synonym"),]$taxon_name
syn_match # see note below about these

# finally, add RL data to your taxon list
taxon_list <- left_join(taxon_list, rl_add, 
                        by=c("taxon_name_accepted" = "taxon"))

# add Not Applicable RL category for any hybrids
taxon_list[which(grepl(" x ",taxon_list$taxon_name_accepted)),]$rl_category <- "Not Applicable"
# add Not Evaluated for any taxa with no RL category
taxon_list[which(is.na(taxon_list$rl_category)),]$rl_category <- "Not Evaluated"

# note that we don't automatically add the "syn_match" matches; for example, 
# if Querucs montana is your accepted name but there is only RL data for it's 
# synonym Quercus prinus, then the RL data will not be added to your taxon list
# (we only match to *accepted* names); if you'd like to add RL data found for 
# synonyms (first think about if it makes sense for that taxon!), you can do 
# it manually following this example:
#add <- taxon_test %>% 
#  filter(taxon_name == "Quercus prinus") %>%
#  select(rl_category,rl_native_dist,rl_native_dist_iso2,
#         rl_introduced_dist,rl_introduced_dist_iso2,rl_taxon_name)
#taxon_list[which(taxon_list$taxon_name_accepted == "Quercus montana"),
#           c("rl_category","rl_native_dist","rl_native_dist_iso2","
#             "rl_introduced_dist","rl_introduced_dist_iso2",
#             "rl_taxon_name")] <- add

################################################################################
# Get BGCI GlobalTreeSearch (GTS) native country data
################################################################################

# FIRST, download raw data
# Go to https://tools.bgci.org/global_tree_search.php
# Type your target genus name into the "Genus" box
# Click "Search" then scroll to the bottom and click "Download as CSV file"
# If you have more than one target genus, repeat the above steps for the
#   other genera
# Move all download(s) to your "taxa_metadata" folder (in target_taxa folder) 
#   and create a new folder called "globaltreesearch_data" to hold them all

# read in and compile GTS data
file_list <- list.files(path = file.path(main_dir,taxa_dir,output_dir,
                                         "globaltreesearch_data"),
                        pattern = "globaltreesearch_results", full.names = T)
file_dfs <- lapply(file_list, read.csv, colClasses = "character",
                   na.strings=c("","NA"),strip.white=T)
gts_list <- data.frame()
for(file in seq_along(file_dfs)){
  gts_list <- rbind(gts_list, file_dfs[[file]])
}
head(gts_list)

# split countries by delimiter
gts_all <- gts_list %>%
  rename(taxon_name = taxon) %>%
  mutate(native_distribution =
           strsplit(as.character(native_distribution), "; ")) %>%
  unnest(native_distribution) %>% mutate(native_distribution =
                                           str_trim(native_distribution, side="both"))

# can check countries list
spp_countries <- as.data.frame(sort(unique(str_trim(
  gts_all$native_distribution, side = c("both")))))
spp_countries

# use countrycode package to translate to country codes from the country names
country_set <- as.data.frame(sort(unique(gts_all$native_distribution))) %>%
  add_column(iso3c = countrycode(sort(unique(gts_all$native_distribution)),
                                 origin="country.name", destination="iso3c")) %>%
  add_column(iso2c = countrycode(sort(unique(gts_all$native_distribution)),
                                 origin="country.name", destination="iso2c")) %>%
  add_column(iso3n = countrycode(sort(unique(gts_all$native_distribution)),
                                 origin="country.name", destination="iso3n")) %>%
  add_column(fips = countrycode(sort(unique(gts_all$native_distribution)),
                                origin="country.name", destination="fips"))
names(country_set)[1] <- "country_name"
# a warning message may say "some values were not matched unambiguously"
# you can look at these if you'd like (examples below):
#country_set[which(country_set$country_name == "Bonaire, Sint Eustatius and Saba"),]
#country_set[which(country_set$country_name == "Curaçao"),]
#country_set[which(country_set$country_name == "South Sudan"),]

# add country codes to GTS native distribution data
gts_list$gts_native_dist_iso2 <- gts_list$native_distribution
gts_list$gts_native_dist_iso2 <- mgsub(gts_list$gts_native_dist_iso2,
                                        array(as.character(country_set$country_name)),
                                        array(as.character(country_set$iso2c)))
names(gts_list)[4] <- "gts_native_dist"

# create GTS dataframe for joining to our taxa list
gts_add <- gts_list %>%
  select(taxon,gts_native_dist,gts_native_dist_iso2)
gts_add$gts_taxon_name <- gts_add$taxon
head(gts_add)

# see which of your target taxa have GTS data or not
# match to taxon names
taxon_test <- left_join(taxon_list, gts_add, by=c("taxon_name" = "taxon"))
head(taxon_list)
# see which of your accepted target taxa have no GTS data
no_match <- taxon_test[
  which(is.na(taxon_test$gts_native_dist) &
          taxon_test$taxon_name_status == "Accepted"),]$taxon_name_accepted
no_match
# see if any of your synonyms matched GTS data
syn_match <- taxon_test[
  which(!is.na(taxon_test$gts_native_dist) &
          taxon_test$taxon_name_status == "Synonym"),]$taxon_name
syn_match # see note below about these

# finally, add GTS data to your taxon list
taxon_list <- left_join(taxon_list, gts_add, 
                        by=c("taxon_name_accepted" = "taxon"))

# note that we don't automatically add the "syn_match" matches; for example, 
# if Querucs montana is your accepted name but there is only GTS data for it's 
# synonym Quercus prinus, then the GTS data will not be added to your taxon list
# (we only match to *accepted* names); if you'd like to add GTS data found for 
# synonyms, you can do it manually following this example:
#add <- taxon_test %>% 
#  filter(taxon_name == "Quercus prinus") %>%
#  select(gts_native_dist,gts_native_dist_iso2,gts_taxon_name)
#taxon_list[which(taxon_list$taxon_name_accepted == "Quercus montana"),
#           c("gts_native_dist","gts_native_dist_iso2","gts_taxon_name")] <- add

################################################################################
# Combine GTS and RL results
################################################################################

# see what's been added to your taxon list so far
head(taxon_list)

# see which accepted target taxa have no distribution data matched
unique(taxon_list[is.na(taxon_list$gts_native_dist) &
           is.na(taxon_list$rl_native_dist),]$taxon_name_accepted)
# you can add data for these manually if you'd like; for example:
add_manually <- data.frame(
  taxon_name_accepted = c("Asimina incana", "Asimina longifolia",
                          "Asimina manasota", "Asimina pygmaea",
                          "Asimina reticulata", "Asimina x nashii",
                          "Juglans major var. major",
                          "Juglans microcarpa var. microcarpa"),
  manual_native_dist = c("United States","United States",
                         "United States","United States",
                         "United States","United States",
                         "Mexico; United States",
                         "United States"),
  manual_native_dist_iso2 = c("US","US",
                               "US","US",
                               "US","US",
                               "MX; US",
                               "US"))
taxon_list <- left_join(taxon_list,add_manually)
# if you don't add anything manually, you'll still need those columns, so add:
#taxon_list$manual_native_dist <- NA
#taxon_list$manual_native_dist_iso2 <- NA

# create columns that combine RL, GTS, and manually-added country data
  # full country names
taxon_list <- taxon_list %>%
  unite(all_native_dist, c(rl_native_dist,gts_native_dist,manual_native_dist),
        sep="; ", remove=F, na.rm=T)
taxon_list$all_native_dist <- sapply(strsplit(
  taxon_list$all_native_dist, "; "), function(x) paste(unique(x), collapse = "; "))
unique(taxon_list$all_native_dist)
  # ISO country code abbreviations
taxon_list <- taxon_list %>%
  unite(all_native_dist_iso2, c(rl_native_dist_iso2,gts_native_dist_iso2,manual_native_dist_iso2),
        sep="; ", remove=F, na.rm=T)
taxon_list$all_native_dist_iso2 <- sapply(strsplit(
  taxon_list$all_native_dist_iso2, "; "), function(x) paste(unique(x), collapse = "; "))
unique(taxon_list$all_native_dist_iso2)

################################################################################
# Write new target taxa file with new metadata added
################################################################################

# order columns
taxon_list <- taxon_list %>%
  select(taxon_name,taxon_name_accepted,taxon_name_status,
         ns_rank, ns_taxon_name, #add any other manually-added columns here 
         rl_category,all_native_dist,all_native_dist_iso2,
         rl_native_dist,rl_native_dist_iso2,
            rl_introduced_dist,rl_introduced_dist_iso2,rl_taxon_name,
         gts_native_dist,gts_native_dist_iso2,gts_taxon_name,
         manual_native_dist,manual_native_dist_iso2)

# replace NA with empty string, to be sure NA is not confused with a country code
taxon_list[is.na(taxon_list)] <- ""
head(taxon_list)

# write file
write.csv(taxon_list, file.path(main_dir, taxa_dir,
    "target_taxa_with_synonyms.csv"), row.names=F)
