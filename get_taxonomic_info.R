################################################################################

### get_taxonomic_info.R
### Authors: Emily Beckman Bruns & Shannon M Still 
### Written: 5/30/2020
### Last Updated: 2/26/2021

### DESCRIPTION:
  # This script takes a list of taxa and uses the taxize package and static
  #   backbones to pull taxonomic information from multiple databases.
  # Information pulled includes:
    # - Acceptance and authors from The Plant List (TPL); could not find good
    #   way to get synonyms
    # - Acceptance and authors plus synonyms from:
    #   - Tropicos (via taxize package)
    #   - Integrated Taxonomic Information Service (ITIS; via taxize package)
    #   - Plants of the World (POW; via taxize package)
    #   - World Checklist of Vascular Plants (WCVP; via static backbone)
    #   - World Flora Online (WFO; via static backbone)
    # - Synonyms, conservation category, and assessment year from IUCN Red List
    # - There is a section for pulling GBIF synonyms, but not used currently
    #   because sometimes pulled completely unrelated names such as insects
  # The output can either be used directly in following scripts
  #   or can be reviewed and revised by hand (recommended) based on your
  #   taxonomic viewpoint

### DATA IN:
  # target_taxa.csv (list of target taxa) or create list by hand in script
    # one column: "taxon_name_acc" (genus, species, infra rank, and infra name,
    # all separated by one space each; hybrid symbol should be " x ", rather
    # than "_" or "✕" and go between genus and species)

### DATA OUT:
  # target_taxa_with_syn_all.csv
  # target_taxa_with_syn_filtered.csv
  # more info here: https://docs.google.com/spreadsheets/d/1dllfDXaZBLvB1AsrY1wDS-sPceKAdOY681bqUbfoQAs/edit?usp=sharing

### PLEASE NOTE:
  # Some functions from taxize package require manual input so you must pause
  #   to answer queries before continuing with script; these areas are marked
  #   with "!!" to help you spot them.
  # See README file for more information

################################################################################
# Load libraries
################################################################################

# rm(list=ls())
my.packages <- c('plyr', 'tidyverse', 'rgbif', 'data.table', 'taxize',
  'anchors', 'batchtools', 'textclean', 'stringi', 'devtools','rredlist')
# install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

################################################################################
# Set working directory
################################################################################

# either set manually:
#main_dir <- "/Volumes/GoogleDrive/My Drive/Conservation Consortia/R Training/occurrence_points"
#script_dir <- "./Documents/GitHub/OccurrencePoints/scripts"

# or use 0-1_set_workingdirectory.R script:
 source("./Documents/GitHub/OccurrencePoints/scripts/0-1_set_workingdirectory.R")
#source('scripts/0-1_set_workingdirectory.R')

################################################################################
# Load functions
################################################################################
#source(file.path(script_dir,"0-2_load_IMLS_functions.R"))

# remove speices/taxa that did not have any synonyms, create data frame of
#   synonyms, and add column stating which database it came from
synonyms.compiled <- function(syn_output,db_name){
  found <- NA
  for(i in 1:length(syn_output)){
    if(length(syn_output[[i]])>1){
      if(syn_output[[i]][1,3]!="no syns found"){
        found <- c(found,i)
        syn_output[[i]]$taxon_name <- rep(names(syn_output[i]),
                                          nrow(syn_output[[i]]))
      }
    }
  }
  found <- found[-1]
  syn_output_df <- Reduce(rbind.fill, syn_output[found])
  syn_output_df$database <- db_name
  return(syn_output_df)
}

################################################################################
################################################################################
# 1. Load/create target taxa list
################################################################################

# CHANGE THIS LIST OF FAMILIES BASED ON TAXA YOURE LOOKING FOR:
#tpl_families() # list of families in database
families <- c("Fagaceae","Rosaceae","Ulmaceae","Malvaceae")
#families <- "Sapindaceae"
#families <- c("Juglandaceae","Fagaceae","Leguminosae","Lauraceae","Pinaceae","Taxaceae")
#families <- c("Fagaceae","Magnoliaceae")

# read in taxa list
taxa_list_acc <- read.csv(file.path(main_dir,"inputs","taxa_list",
  "target_taxa.csv"), header = T, colClasses="character")
nrow(taxa_list_acc) #191
# make sure there aren't extra spaces within species names
taxa_list_acc[,1] <- str_squish(taxa_list_acc[,1])
# create list of target taxa names
taxa_names <- taxa_list_acc[,1]
  # use this instead if you want to select names based on values in other col:
  #taxa_names_sel <- taxa_list_acc[which(taxa_list_acc$taxon_name_acc_type != "cultivar"),]
  #taxa_names <- taxa_names_sel[,1]
length(taxa_names) #191

## OR: you can create vector of taxa names here instead of reading in
#taxa_names <- c("Quercus fabrei","Quercus fabri","Magnolia albosericea",
#  "Quercus montana","Quercus palmeri")

# create list of target species names, with infraspecific taxa removed
species_names <- taxa_names[
  !grepl(" var. ",taxa_names) &
  !grepl(" subsp.",taxa_names) &
  !grepl(" f. ",taxa_names)]
length(species_names) #191

# create list of target species names only, with hybrids removed
species_only <- species_names[
  !grepl(" x ",species_names)]
length(species_only) #191

################################################################################
# 2. Find taxonomic status and synonyms for target taxa
################################################################################

###############
### A) Tropicos (from Missouri Botanical Garden)
### https://www.missouribotanicalgarden.org/media/fact-pages/tropicos.aspx
###############

# IF NEEDED: can save Tropicos API key as .txt and source from local drive
## check environment for the tropicos_key object
    ## "topicos_key.txt" should be a simple text file with only the key.
    ##    It should be stored in the local drive (local_dir) with file path set
    ##      in script 0-1_set_workingdirectory.R
if(file.exists(file.path(local_dir, "tropicos_key.txt"))){
  tpkey <- read_lines(file.path(local_dir, "tropicos_key.txt"))
  print("Good, you have your own dang Tropicos key!")
} else {print("Get your own dang Tropicos key!")}
  # or you can set API key in your R environment and restart R
    #taxize::use_tropicos() # get API
    #usethis::edit_r_environ() # set API
      # TROPICOS_KEY='________' # paste this in

# Tropicos does not search for infrataxa, so we will use species list

# replace characters to match Tropicos system
species_names <- gsub(" x "," × ",species_names,fixed=T)

## GET TAXONOMIC STATUS

tp_names_raw <- data.frame()
for(i in 1:length(species_names)){
  if(exists("tpkey")){
    output_new <- tp_search(species_names[[i]], key=tpkey)
  } else {
    output_new <- tp_search(species_names[[i]])
  }
  output_new$taxon_name_acc <- species_names[[i]]
  tp_names_raw <- rbind.fill(tp_names_raw,output_new)
  print(species_names[i])
}
head(tp_names_raw); class(tp_names_raw); names(tp_names_raw)
# standardize column names for joining later
tp_names <- tp_names_raw
setnames(tp_names,
  old = c("scientificname","nameid",
          "nomenclaturestatusname","scientificnamewithauthors"),
  new = c("taxon_name_match","match_id",
          "acceptance","match_name_with_authors"))
tp_names$database <- "tropicos"
# replace characters in taxa names
tp_names[] <- lapply(tp_names, function(x) gsub(" × "," x ", x))
tp_names[] <- lapply(tp_names, function(x) gsub(" fo. "," f. ", x))
# remove duplicates except those matching legitimate names
tp_names_noDup <- tp_names
  # remove rows with no match
tp_names_noDup <- tp_names_noDup[which(
  !is.na(tp_names_noDup$taxon_name_match)),]
  # OPTIONAL; IF NOT LOOKING FOR CHILDREN: remove subsp., var., and f.
tp_names_noDup <- tp_names_noDup %>%
  filter(!grepl("subsp.",taxon_name_match,fixed=T) &
         !grepl("var.",taxon_name_match,fixed=T) &
         !grepl("f.",taxon_name_match,fixed=T))
  # remove taxon_name_acc duplicates that aren't Legitimate
tp_names_noDup$dup <- c(duplicated(tp_names_noDup$taxon_name_acc,fromLast=T)
  | duplicated(tp_names_noDup$taxon_name_acc))
tp_names_noDup <- setdiff(tp_names_noDup,tp_names_noDup[which(
  tp_names_noDup$acceptance != "Legitimate" & tp_names_noDup$dup == T),])
# add column with authors
tp_names_noDup$match_name_with_authors <- paste(
  tp_names_noDup$taxon_name_match,tp_names_noDup$author)
# keep only necessary columns
tp_names_noDup <- tp_names_noDup[,c("taxon_name_acc","taxon_name_match",
  "match_id","acceptance","match_name_with_authors","database")]
tp_names_noDup$acceptance <- str_to_lower(tp_names_noDup$acceptance)

## GET SYNONYMS

tp_syn <- synonyms(species_names, db="tropicos")

# !!
# !! STOP BEFORE RUNNING NEXT SECTION -- YOU MAY HAVE TO ANSWER SOME PROMPTS
# !!

# remove species/taxa that did not have any synonyms,
#   create data frame of synonyms,
#   and add column stating which database it came from
tp_syn_df <- synonyms.compiled(tp_syn,"tropicos")
colnames(tp_syn_df)
# standardize column names for joining later
setnames(tp_syn_df,
  old = c("taxon_name","nameid","scientificname",
          "scientificnamewithauthors"),
  new = c("taxon_name_acc","match_id","taxon_name_match",
          "match_name_with_authors"))
tp_syn_df$acceptance <- "synonym"
# replace characters in taxa names
tp_syn_df[] <- lapply(tp_syn_df, function(x) gsub(" × "," x ", x))
tp_syn_df[] <- lapply(tp_syn_df, function(x) gsub(" fo. "," f. ", x))
# keep only necessary columns
tp_syn_df <- tp_syn_df[,c("taxon_name_acc","taxon_name_match",
  "match_id","acceptance","match_name_with_authors","database")]
tp_syn_df$acceptance <- str_to_lower(tp_syn_df$acceptance)

## BIND TOGETHER STATUS AND SYNONYMS

tp_all <- rbind.fill(tp_names_noDup,tp_syn_df)
head(tp_all)

###############
### B) Integrated Taxonomic Information Service (ITIS)
### https://www.itis.gov
###############

# replace characters to match ITIS system
taxa_names <- gsub(" x "," X ",taxa_names,fixed=T)
taxa_names <- gsub(" subsp. "," ssp. ",taxa_names)

## GET TAXONOMIC STATUS

# takes a while if lots of names
itis_names_raw <- itis_terms(taxa_names,what="scientific")
  itis_names_raw <- ldply(itis_names_raw, data.frame) # list to data frame
  itis_names_raw <- itis_names_raw[,c(1:2,4:6)]
head(itis_names_raw); class(itis_names_raw); names(itis_names_raw)
# standardize column names for joining later
itis_names <- itis_names_raw
setnames(itis_names,
  old = c(".id","scientificName","nameUsage","tsn"),
  new = c("taxon_name_acc","taxon_name_match","acceptance","match_id"))
itis_names$database <- "itis"
# replace characters in taxa names
itis_names[] <- lapply(itis_names, function(x) gsub(" X "," x ", x))
itis_names[] <- lapply(itis_names, function(x) gsub(" ssp. "," subsp. ", x))
# remove duplicates except those matching legitimate names
itis_names_noDup <- itis_names
  # remove rows with no match
itis_names_noDup <- itis_names_noDup[which(
  !is.na(itis_names_noDup$taxon_name_match)),]
  # OPTIONAL, IF NOT LOOKING FOR CHILDREN: remove subsp., var., and f.
itis_names_noDup <- itis_names_noDup %>%
  filter(!grepl("subsp.",taxon_name_match,fixed=T) &
         !grepl("var.",taxon_name_match,fixed=T) &
         !grepl("f.",taxon_name_match,fixed=T))
  # remove taxon_name_acc duplicates that aren't Legitimate
itis_names_noDup$dup <- c(duplicated(itis_names_noDup$taxon_name_acc,fromLast=T)
  | duplicated(itis_names_noDup$taxon_name_acc))
itis_names_noDup <- setdiff(itis_names_noDup,itis_names_noDup[which(
  itis_names_noDup$acceptance != "accepted" & itis_names_noDup$dup == T),])
# add column with authors
itis_names_noDup$match_name_with_authors <- paste(
  itis_names_noDup$taxon_name_match,itis_names_noDup$author)
# keep only necessary columns
itis_names_noDup <- itis_names_noDup[,c("taxon_name_acc","taxon_name_match",
  "match_id","acceptance","match_name_with_authors","database")]
itis_names_noDup$acceptance <- str_to_lower(itis_names_noDup$acceptance)

## GET SYNONYMS

itis_syn <- synonyms(taxa_names, db="itis", accepted = F)

# !!
# !! STOP BEFORE RUNNING NEXT SECTION -- YOU MAY HAVE TO ANSWER SOME PROMPTS
# !!

# remove species/taxa that did not have any synonyms,
#   create data frame of synonyms,
#   and add column stating which database it came from
itis_syn_df <- synonyms.compiled(itis_syn,"itis")
colnames(itis_syn_df)
# standardize column names for joining later
setnames(itis_syn_df,
  old = c("taxon_name","syn_name","syn_tsn","syn_author"),
  new = c("taxon_name_acc","taxon_name_match","match_id","author"))
# keep only necessary columns
itis_syn_df <- itis_syn_df[,c("taxon_name_acc","taxon_name_match","author",
  "match_id","database")]
itis_syn_df$acceptance <- "synonym"
# replace characters in taxa names
itis_syn_df[] <- lapply(itis_syn_df, function(x) gsub(" X "," x ", x))
itis_syn_df[] <- lapply(itis_syn_df, function(x) gsub(" ssp. "," subsp. ", x))
# add column with authors
itis_syn_df$match_name_with_authors <- paste(
  itis_syn_df$taxon_name_match,itis_syn_df$author)
# remove records where taxa name and syn name are the same
itis_syn_df <- itis_syn_df[which(itis_syn_df$taxon_name_acc !=
  itis_syn_df$taxon_name_match),]
# keep only necessary columns
itis_syn_df <- itis_syn_df[,c("taxon_name_acc","taxon_name_match",
  "match_id","acceptance","match_name_with_authors","database")]
itis_syn_df$acceptance <- str_to_lower(itis_syn_df$acceptance)

## BIND TOGETHER STATUS AND SYNONYMS

itis_all <- rbind.fill(itis_names_noDup,itis_syn_df)
head(itis_all)

###############
### C) Kew’s Plants of the World Online (POWO)
### http://www.plantsoftheworldonline.org
###############

# replace characters to match POW system
taxa_names <- gsub(" X "," x ",taxa_names,fixed=T)
taxa_names <- gsub(" ssp. "," subsp. ",taxa_names)

## GET TAXONOMIC STATUS AND SYNONYMS

pow_names <- data.frame()
pow_syn <- data.frame()
for(i in 1:length(taxa_names)){
  id <- get_pow(taxa_names[[i]])[1]
  if(!is.na(id)){
    output_new <- pow_lookup(id)
    if(length(output_new$meta$authors>0)){
      acc <- data.frame(
        "taxon_name_match" = output_new$meta$name,
        "match_id" = output_new$meta$fqId,
        "acceptance" = output_new$meta$taxonomicStatus,
        "author" = output_new$meta$authors,
        "taxon_name_acc" = taxa_names[[i]]
      )
    }else{
      acc <- data.frame(
        "taxon_name_match" = output_new$meta$name,
        "match_id" = output_new$meta$fqId,
        "acceptance" = output_new$meta$taxonomicStatus,
        #"author" = output_new$meta$authors,
        "taxon_name_acc" = taxa_names[[i]]
      )}
    not_acc <- data.frame(output_new$meta$accepted)
    if(length(not_acc)>0){
      acc <- data.frame(
        "taxon_name_match" = output_new$meta$accepted$name,
        "match_id" = output_new$meta$accepted$fqId,
        "acceptance" = output_new$meta$taxonomicStatus,
        "author" = output_new$meta$accepted$author,
        "taxon_name_acc" = taxa_names[[i]]
      )
    }
    syn <- data.frame(output_new$meta$synonyms)
    if(length(syn)>0){
      syn$taxon_name_acc <- taxa_names[[i]]
    }
    pow_names <- rbind.fill(pow_names,acc)
    pow_syn <- rbind.fill(pow_syn,syn)
  }
  Sys.sleep(5)
}

# !!
# !! STOP BEFORE RUNNING NEXT SECTION -- YOU MAY HAVE TO ANSWER SOME PROMPTS
# !!

# fix up acceptance df
  # add column stating which database it came from
pow_names$database <- "pow"
  # add column with authors
pow_names$match_name_with_authors <- paste(
  pow_names$taxon_name_match,pow_names$author)
  # remove duplicates except those matching legitimate names
pow_names_noDup <- pow_names
  # keep only necessary columns
pow_names_noDup <- pow_names_noDup[,c("taxon_name_acc","taxon_name_match",
  "match_id","database","acceptance","match_name_with_authors")]
pow_names_noDup$acceptance <- str_to_lower(pow_names_noDup$acceptance)
  # replace hybrid character
pow_names_noDup$taxon_name_acc <- gsub(" × "," x ",
  pow_names_noDup$taxon_name_acc,fixed=T)
pow_names_noDup$taxon_name_match <- gsub(" × "," x ",
  pow_names_noDup$taxon_name_match,fixed=T)
pow_names_noDup$match_name_with_authors <- gsub(" × "," x ",
  pow_names_noDup$match_name_with_authors,fixed=T)
  # OPTIONAL, IF NOT LOOKING FOR CHILDREN: remove subsp., var., and f.
pow_names_noDup <- pow_names_noDup %>%
  filter(!grepl("subsp.",taxon_name_match,fixed=T) &
         !grepl("var.",taxon_name_match,fixed=T) &
         !grepl("f.",taxon_name_match,fixed=T))

# fix up synonyms df
  # add column stating which database it came from
pow_syn_df <- pow_syn
pow_syn_df$database <- "pow"
  # standardize column names for joining later
setnames(pow_syn_df,
  old = c("name","fqId","taxonomicStatus"),
  new = c("taxon_name_match","match_id","acceptance"))
  # replace hybrid character
pow_syn_df$taxon_name_acc <- gsub(" × "," x ",
  pow_syn_df$taxon_name_acc,fixed=T)
pow_syn_df$taxon_name_match <- gsub(" × "," x ",
  pow_syn_df$taxon_name_match,fixed=T)
  # add column with authors
pow_syn_df$match_name_with_authors <- paste(
  pow_syn_df$taxon_name_match,pow_syn_df$author)
  # remove records where taxa name and syn name are the same
pow_syn_df <- pow_syn_df[which(pow_syn_df$taxon_name_acc !=
  pow_syn_df$taxon_name_match),]
  # keep only necessary columns
pow_syn_df <- pow_syn_df[,c("taxon_name_acc","taxon_name_match",
  "match_id","database","acceptance","match_name_with_authors")]
pow_syn_df$acceptance <- str_to_lower(pow_syn_df$acceptance)

## BIND TOGETHER STATUS AND SYNONYMS

pow_all <- rbind.fill(pow_names_noDup,pow_syn_df)
head(pow_all)

###############
### D) The Plant List (TPL)
### http://www.theplantlist.org
###############

# GET ALL DATA FOR TARGET FAMILIES
#   There is not an easy function for pulling synonyms from TPL :`(

tpl_names <- data.frame()
for(i in 1:length(families)){
  output_new <- tpl_get("files",family=families[i])
  output_new <- read.csv(paste("files/",families[i],".csv",sep=""), header = T,
    colClasses="character")
  tpl_names <- rbind.fill(tpl_names,output_new)
}
# standardize column names for joining later
setnames(tpl_names,
  old = c("ID","Taxonomic.status.in.TPL","Authorship"),
  new = c("match_id","acceptance","author"))
tpl_names$database <- "tpl"
# create concatenated taxon_name_acc col
tpl_names <- unite(tpl_names, "taxon_name_acc",
  c(Genus,Species.hybrid.marker,Species,Infraspecific.rank,
    Infraspecific.epithet), sep = " ", remove = F, na.rm = T)
# replace hybrid character
tpl_names$taxon_name_acc <- gsub(" × "," x ",
  tpl_names$taxon_name_acc,fixed=T)
# trim whitespace
tpl_names$taxon_name_acc <- str_squish(tpl_names$taxon_name_acc)
# fill other columns
tpl_names$taxon_name_match <- tpl_names$taxon_name_acc
tpl_names$match_name_with_authors <- paste(tpl_names$taxon_name_acc,
  tpl_names$author)
colnames(tpl_names)
# remove duplicates
tpl_names_noDup <- tpl_names
tpl_names_noDup$dup <- c(duplicated(tpl_names_noDup$taxon_name_acc,
  fromLast = TRUE) | duplicated(tpl_names_noDup$taxon_name_acc))
tpl_names_noDup <- setdiff(tpl_names_noDup,tpl_names_noDup[
  which(tpl_names_noDup$acceptance != "Accepted" & tpl_names_noDup$dup == T),])
# keep only necessary columns
tpl_names_noDup <- tpl_names_noDup[,c("taxon_name_acc","taxon_name_match",
  "match_id","database","acceptance","match_name_with_authors")]
tpl_names_noDup$acceptance <- str_to_lower(tpl_names_noDup$acceptance)
# join with taxa list and remove non-matches
tpl_all <- tpl_names_noDup %>% filter(tpl_names_noDup$taxon_name_acc %in%
  taxa_names)
head(tpl_all)

###############
### E) Global Biodiversity Information Facility (GBIF)
### https://www.gbif.org/species/search
###############

## commenting this section out for now; pulled a lot of synonyms that were
#   completely off (multiple insects...!?)

## GET SYNONYMS

#gbif_names <- data.frame()
#for(i in 1:length(taxa_names)){
#  output_new <- name_lookup(query=taxa_names[i],
#    status = c("synonym","homotypic_synonym"))$data
#  output_new$taxon_name_acc <- taxa_names[i]
#  output_new$database <- "gbif"
#  gbif_names <- rbind.fill(gbif_names,output_new)
#}
# keep only necessary columns and rename for joining later
#gbif_names <- gbif_names %>%
#  filter(rank == "SPECIES" | rank == "VARIETY" | is.na(rank) |
#    rank == "INFRASPECIFIC_NAME" | rank == "SUBSPECIES") %>%
#  dplyr::select(taxon_name_acc,database,scientificName,key,taxonomicStatus) %>%
#  rename(match_id = key,
#         acceptance = taxonomicStatus,
#         match_name_with_authors = scientificName) %>%
#  separate("match_name_with_authors",
#    c("genus_new","species_new","infra_rank","infra_name","extra"),sep=" ",
#    remove=F,fill="right")
#gbif_names$acceptance <- str_to_lower(gbif_names$acceptance)
#head(gbif_names)
# create standard taxon name column
#gbif_names$taxon_name_match <- NA
#for(i in 1:nrow(gbif_names)){
#  if(!is.na(gbif_names$infra_rank[i]) &
#    (gbif_names$infra_rank[i] == "var." | gbif_names$infra_rank[i] == "subsp.")){
#    gbif_names$taxon_name_match[i] <- paste(gbif_names$genus_new[i],
#      gbif_names$species_new[i],gbif_names$infra_rank[i],
#      gbif_names$infra_name[i],sep = " ")
#  } else if(!is.na(gbif_names$infra_rank[i]) & gbif_names$infra_rank[i] == "x"){
#    gbif_names$taxon_name_match[i] <- NA
#    #gbif_names$taxon_name_match[i] <- paste(gbif_names$genus_new[i],
#    #  gbif_names$species_new[i],gbif_names$infra_rank[i],
#    #  gbif_names$infra_name[i],gbif_names$extra[i],sep = " ")
#  } else {
#    gbif_names$taxon_name_match[i] <- paste(gbif_names$genus_new[i],
#      gbif_names$species_new[i],sep = " ")
#  }
#}
# remove duplicates and extra columns
#gbif_all <- gbif_names %>%
#  filter(!is.na(taxon_name_match)) %>%
#  filter(taxon_name_acc != taxon_name_match) %>%
#  distinct(taxon_name_match,taxon_name_acc,.keep_all=T) %>%
#  dplyr::select(taxon_name_acc,database,match_name_with_authors,match_id,
#    acceptance,taxon_name_match)
#head(gbif_all)

###############
### F) IUCN Red List
### https://www.iucnredlist.org
###############

## ! you first need an API key ! run the following line and fill out
##   the necessary online form to receive a key, then follow the instructions
##   to add to your R environment:
#rl_use_iucn()

## GET ASSESSMENT INFO

## use rredlist package to get assessment information
## can take a little while if lots of species
rl_names <- data.frame()
for(i in 1:length(species_only)){
	hist <- rl_history(species_only[[i]])
	name <- hist$name
	hist <- as.data.frame(hist$result)
	if(nrow(hist>0)){
    print(species_only[[i]])
		hist$genus_species <- rep(name)
		rl_names <- rbind(rl_names,hist)
	} else {
		print(paste(species_only[[i]],"no assessment"))
	}
}
# add col stating which database it came from
rl_names_df <- rl_names
rl_names_df$database <- "redlist"
colnames(rl_names_df)
# keep only necessary columns
rl_names_df <- rl_names_df[,c("year","code","genus_species","database")]
rl_names_df$acceptance <- "accepted"
# standardize column names for joining later
setnames(rl_names_df,
  old = c("genus_species","code","year"),
  new = c("taxon_name_acc","rl_category","rl_year"))
rl_names_df$taxon_name_match <- rl_names_df$taxon_name_acc
# keep only necessary columns
rl_names_df <- rl_names_df[,c("taxon_name_acc","taxon_name_match",
  "acceptance","database","rl_year","rl_category")]
# version for adding year and category at end of script
rl_assess <- rl_names_df %>%
  distinct(taxon_name_acc,.keep_all = T) %>%
  dplyr::select("taxon_name_acc","rl_year","rl_category")
# version for compiling acceptance with all other backbones
rl_names_df <- rl_names_df %>%
  distinct(taxon_name_acc,.keep_all = T) %>%
  dplyr::select("taxon_name_acc","taxon_name_match",
    "acceptance","database")

# GET SYNONYMS

## use rredlist package to get synonyms
## can take a little while if lots of species
rl_syn <- data.frame()
for(i in 1:length(species_only)){
	syn <- rl_synonyms(species_only[[i]])
	name <- syn$name
	syn <- as.data.frame(syn$result)
	if(nrow(syn>0)){
    print(species_only[[i]])
		syn$genus_species <- rep(name)
		rl_syn <- rbind(rl_syn,syn)
	} else {
		print(paste(species_only[[i]],"no synonyms"))
	}
}
# add col stating which database it came from
rl_syn_df <- rl_syn
rl_syn_df$database <- "redlist"
colnames(rl_syn_df)
# standardize column names for joining later
setnames(rl_syn_df,
  old = c("accepted_name","synonym","syn_authority"),
  new = c("taxon_name_acc","taxon_name_match","author"))
# keep only necessary columns
rl_syn_df <- rl_syn_df[,c("taxon_name_acc","taxon_name_match","author",
  "database")]
rl_syn_df$acceptance <- "synonym"
# add column with authors
rl_syn_df$match_name_with_authors <- paste(
  rl_syn_df$taxon_name_match,rl_syn_df$author)
# remove records where taxa name and syn name are the same
rl_syn_df <- rl_syn_df[which(rl_syn_df$taxon_name_acc !=
  rl_syn_df$taxon_name_match),]
# keep only necessary columns
rl_syn_df <- rl_syn_df[,c("taxon_name_acc","taxon_name_match",
  "acceptance","match_name_with_authors","database")]

## BIND TOGETHER STATUS AND SYNONYMS

rl_all <- rbind.fill(rl_names_df,rl_syn_df)
# remove names not in accepted taxa list
rl_all <- rl_all %>% filter(rl_all$taxon_name_acc %in%
  taxa_names)
head(rl_all)

###############
### G) World Checklist of Vascular Plants (WCVP) - static backbone
### https://wcvp.science.kew.org
###############

# DOWNLOAD MOST RECENT BACKBONE AT https://wcvp.science.kew.org AND
#   PLACE IN "taxa_list" FOLDER

# read in data
wcvp <- read.delim(file.path(main_dir, "inputs","taxa_list",
  "wcvp_v3_nov_2020.txt"),colClasses="character",sep="|")
head(wcvp)

## GET TAXONOMIC STATUS

# search for target taxa
wcvp_names <- data.frame()
for(i in 1:length(taxa_names)){
  matched <- wcvp[which(wcvp$taxon_name == taxa_names[i]),]
  if(length(matched) > 0){
    wcvp_names <- rbind(wcvp_names,matched)
  }
}
head(wcvp_names)
wcvp_names_df <- wcvp_names
colnames(wcvp_names_df)
# standardize column names for joining later
setnames(wcvp_names_df,
  old = c("taxon_name","kew_id","taxonomic_status"),
  new = c("taxon_name_acc","match_id","acceptance"))
wcvp_names_df$match_name_with_authors <-
  paste(wcvp_names_df$taxon_name_acc,wcvp_names_df$authors)
wcvp_names_df$taxon_name_match <- wcvp_names_df$taxon_name_acc
# keep only necessary columns
wcvp_names_df <- wcvp_names_df[,c("taxon_name_acc","taxon_name_match",
  "match_id","acceptance","match_name_with_authors")]
wcvp_names_df$acceptance <- str_to_lower(wcvp_names_df$acceptance)

## GET SYNONYMS

# search for target taxa
wcvp_syn <- data.frame()
for(i in 1:nrow(wcvp_names_df)){
  id <- wcvp_names_df$match_id[i]
  syn <- wcvp[which(wcvp$accepted_kew_id == id),]
  if(nrow(syn) > 0){
    wcvp_syn <- rbind(wcvp_syn,syn)
  }
}
head(wcvp_syn)
wcvp_syn_df <- wcvp_syn
colnames(wcvp_syn_df)
# standardize column names for joining later
setnames(wcvp_syn_df,
  old = c("taxon_name_acc","accepted_name"),
  new = c("taxon_name_match","taxon_name_acc"))
wcvp_syn_df$match_name_with_authors <-
  paste(wcvp_syn_df$taxon_name_match,wcvp_syn_df$authors)
wcvp_syn_df$acceptance <- "synonym"
# keep only necessary columns
wcvp_syn_df <- wcvp_syn_df[,c("taxon_name_acc","taxon_name_match",
  "match_id","acceptance","match_name_with_authors")]

## BIND TOGETHER STATUS AND SYNONYMS

wcvp_all <- rbind.fill(wcvp_names_df,wcvp_syn_df)
head(wcvp_all)
# add col stating which database it came from
wcvp_all$database <- "wcvp"

###############
### H) World Flora Online - static backbone
### http://www.worldfloraonline.org/downloadData
###############

# DOWNLOAD MOST RECENT BACKBONE AT http://www.worldfloraonline.org/downloadData
#   AND PLACE IN "taxa_list" FOLDER

# read in data
wfo <- read.delim(file.path(main_dir, "inputs","taxa_list",
  "WFO_Backbone_v.2019.05","classification.txt"),colClasses="character")
head(wfo)

## GET TAXONOMIC STATUS

# search for target taxa
wfo_names <- data.frame()
for(i in 1:length(taxa_names)){
  matched <- wfo[which(wfo$scientificName == taxa_names[i]),]
  if(length(matched) > 0){
    wfo_names <- rbind(wfo_names,matched)
  }
}
head(wfo_names)
wfo_names_df <- wfo_names
colnames(wfo_names_df)
# standardize column names for joining later
setnames(wfo_names_df,
  old = c("scientificName","taxonID","taxonomicStatus"),
  new = c("taxon_name_acc","match_id","acceptance"))
wfo_names_df$match_name_with_authors <-
  paste(wfo_names_df$taxon_name_acc,wfo_names_df$scientificNameAuthorship)
wfo_names_df$taxon_name_match <- wfo_names_df$taxon_name_acc
# keep only necessary columns
wfo_names_df <- wfo_names_df[,c("taxon_name_acc","taxon_name_match",
  "match_id","acceptance","match_name_with_authors")]
wfo_names_df$acceptance <- str_to_lower(wfo_names_df$acceptance)

## GET SYNONYMS

# search for target taxa
wfo_syn <- data.frame()
for(i in 1:nrow(wfo_names_df)){
  id <- wfo_names_df$match_id[i]
  syn <- wfo[which(wfo$acceptedNameUsageID == id),]
  if(nrow(syn) > 0){
    syn$accepted_name <- wfo_names_df$taxon_name_acc[i]
    wfo_syn <- rbind(wfo_syn,syn)
  }
}
head(wfo_syn)
wfo_syn_df <- wfo_syn
colnames(wfo_syn_df)
# standardize column names for joining later
setnames(wfo_syn_df,
  old = c("taxon_name_acc","accepted_name"),
  new = c("taxon_name_match","taxon_name_acc"))
wfo_syn_df$match_name_with_authors <-
  paste(wfo_syn_df$taxon_name_match,wfo_syn_df$scientificNameAuthorship)
wfo_syn_df$acceptance <- "synonym"
# keep only necessary columns
wfo_syn_df <- wfo_syn_df[,c("taxon_name_acc","taxon_name_match",
  "match_id","acceptance","match_name_with_authors")]

## BIND TOGETHER STATUS AND SYNONYMS

wfo_all <- rbind.fill(wfo_names_df,wfo_syn_df)
head(wfo_all)
# add col stating which database it came from
wfo_all$database <- "wfo"

################################################################################
# 3. Bind all taxonomic status info and synonyms together
################################################################################

## save taxonomic backbone query results for later reference, if needed
save(itis_all,pow_all,tpl_all,tp_all,wcvp_all,wfo_all,rl_all,
 file=file.path(main_dir, "inputs", "taxa_list", "raw_backbone_queries.RData"))

# create dataframe of all data found
  ## !!! change this list to reflect the sources you're using
datasets <- list(itis_all,pow_all,tpl_all,tp_all,wcvp_all,wfo_all,rl_all) #,gbif_all
all_data_raw <- Reduce(rbind.fill,datasets)
all_data <- all_data_raw
  names(all_data)
  str(all_data)

# add a space after every period and fix some other inconsistencies,
#  to standardize authors more
all_data$match_name_with_authors <- gsub(".",". ",
  all_data$match_name_with_authors,fixed=T)
all_data$match_name_with_authors <- str_squish(
  all_data$match_name_with_authors)
all_data$match_name_with_authors <- gsub(". )",".)",
  all_data$match_name_with_authors,fixed=T)
all_data$match_name_with_authors <- gsub("(pro sp.)","",
  all_data$match_name_with_authors,fixed=T)
# replace accented characters
all_data$match_name_with_authors <- stringi::stri_trans_general(
  all_data$match_name_with_authors, "Latin-ASCII")

# keep unique values and concatenate database, acceptance, id, authors
all_data <- all_data %>%
  dplyr::group_by(taxon_name_acc,taxon_name_match) %>%
  dplyr::summarize(
    database = paste(unique(database), collapse = ','),
    acceptance = paste(unique(acceptance),collapse = ','),
    ref_id = paste(unique(match_id),collapse = ' | '),
    match_name_with_authors = paste(unique(match_name_with_authors),collapse = ' | ')) %>%
  dplyr::ungroup()
unique(all_data$database)
unique(all_data$acceptance)
head(all_data$match_name_with_authors)
# add "database_count" column tallying number of items (databases) per taxon
all_data$database_count <- str_count(all_data$database, ',')+1
all_data[which(all_data$database == "NA"),]$database_count <- 0
str(all_data)
# replace NA in match name with authors column
all_data$match_name_with_authors <-
  gsub(" | NA","",all_data$match_name_with_authors,fixed=T)

# join with initial taxa list again
  # if using a manually-created list of target taxa:
#taxa_list_acc <- as.data.frame(taxa_names)
#taxa_list_acc <- taxa_list_acc %>% rename(taxon_name_acc = taxa_names)
  # either way:
taxa_list_acc$taxon_name_match <- taxa_list_acc$taxon_name_acc
all_data <- full_join(all_data,taxa_list_acc)
all_data[which(is.na(all_data$database)),]$acceptance <- "no match"
# separate out taxon_name_match
all_data <- all_data %>% separate("taxon_name_match",
  c("genus","species","infra_rank","infra_name"),sep=" ",extra="warn",
  remove=F,fill="right")
all_data$genus_species_match <- paste(all_data$genus,all_data$species,sep=" ")
# separate out taxon_name_acc to create genus_species column
all_data <- all_data %>% separate("taxon_name_acc",
  c("genus_acc","species_acc"),sep=" ",extra="warn",remove=F,fill="right")
all_data$genus_species_acc <- paste(all_data$genus_acc,all_data$species_acc,
  sep=" ")
# add column stating if synonym or desiderata
all_data$list <- "synonym"
all_data[which(all_data$taxon_name_acc == all_data$taxon_name_match),]$list <-
  "desiderata"
# remove var. and subsp. synonyms when species is already represented
#    (skip if you want children!)
all_data <- setdiff(all_data,all_data[which(
  all_data$genus_species_acc == all_data$genus_species_match &
  grepl("\\.",all_data$taxon_name_match)),])
all_data$syn_pair <- paste(all_data$genus_species_acc,
  all_data$genus_species_match,sep=";")
all_data$syn_pair2 <- paste(all_data$genus_species_acc,
  all_data$taxon_name_match,sep=";")
all_data <- setdiff(all_data,all_data[which(
  all_data$syn_pair %in% all_data$syn_pair2 &
  grepl("\\.",all_data$taxon_name_match)),]) %>%
  select(-syn_pair,-syn_pair2)
nrow(all_data)
# add flag if same syn match name matches more than 1 taxon_name_acc
all_data$dup_flag <- c(duplicated(all_data$taxon_name_match,fromLast=T)
  | duplicated(all_data$taxon_name_match))
# final ordering of names and column selection
all_data <- all_data %>%
  dplyr::arrange(taxon_name_acc,database_count) %>%
  dplyr::select(taxon_name_acc,genus_species_acc,
    taxon_name_match,genus,species,infra_rank,infra_name,
    match_name_with_authors,
    list,database,acceptance,ref_id,database_count,dup_flag
  ) %>%
  rename(species_name_acc = genus_species_acc,
         taxon_name = taxon_name_match)
# add RL assessment data
all_data_final <- left_join(all_data,rl_assess)
head(all_data_final)
# write file
write.csv(all_data_final,file.path(main_dir,"inputs","taxa_list",
  "target_taxa_with_syn_all.csv"),row.names=F)

################################################################################
# 4. Automatically pare down list, if desired
################################################################################

# THESE STEPS MAY REMOVE GOOD SYNONYMS, BUT HELP MAKE LIST HIGHER CONFIDENCE IF
#   YOU DON'T PLAN TO LOOK OVER MANUALLY

  ## remove forms
nrow(all_data)
all_data <- all_data[which(is.na(all_data$infra_rank) |
  all_data$infra_rank != "f."),]
nrow(all_data)
  ## remove records where same syn match name matches more than 1 taxon_name_acc
all_data <- setdiff(all_data,all_data[which(
  all_data$list == "synonym" & all_data$dup_flag == T),])
nrow(all_data)
  ## remove hybrids (naming format can be too variable to be useful)
  ##    and other strange names
#all_data <- all_data[which(!grepl(" x | unranked | group | subg\\.",
#  all_data$taxon_name_match)),]
#nrow(all_data)
  ## remove synonyms with less than two sources
  ##   CUTS DOWN SIGNIFICANTLY BUT MAY REMOVE IMPORTANT SYNONYMS!
#all_data <- all_data[which(all_data$database_count > 1 |
#  grepl("homotypic",all_data$acceptance) |
#  grepl("accepted",all_data$acceptance)),]
#nrow(all_data)

# final ordering of names and column selection
all_data_final <- all_data %>%
  dplyr::arrange(taxon_name_acc,database_count) %>%
  dplyr::select(taxon_name_match,genus,species,
    infra_rank,infra_name,list,taxon_name_acc,genus_species_acc,database,
    acceptance,database_count,match_name_with_authors
  ) %>%
  rename(species_name_acc = genus_species_acc,
         taxon_name = taxon_name_match)
head(all_data_final)
# write file
write.csv(all_data_final,file.path(main_dir,"inputs","taxa_list",
  "target_taxa_with_syn_filtered.csv"),row.names=F)
