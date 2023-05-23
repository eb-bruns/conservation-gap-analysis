### 0-set_working_directory.R
### Authors: Shannon M Still & Emily Beckman Bruns
### Supporting institutions: The Morton Arboretum, Botanic Gardens Conservation 
#   International-US, UC Davis Arboretum & Botanic Garden
### Funding: Institute of Museum and Library Services
#   (IMLS MFA program grant MA-30-18-0273-18 to The Morton Arboretum)
### Last Updated: May 2023 ; first written Dec 2020
### R version 4.2.2

### DESCRIPTION:
  ## This script sets the working environment for the computer on which you are
  # working. You can add your own computer and working directories by filling in 
  # the blanks (________) below! The first entry is an example for my computer.
  ## This script also creates the initial folder structure used for the
  # scripts in the spatial-analysis-workflow

################################################################################
# Set working environment depending on your computer
################################################################################

# Use the following line to check your "nodename"
#Sys.info()[4]

## For Emily Beckman Bruns:
if (Sys.info()[4] == "Africa.local") {
  # main working directory
  main_dir <- "/Users/emily/Library/CloudStorage/GoogleDrive-ebeckman@mortonarb.org/Shared drives/Global Tree Conservation Program/4. GTCP_Projects/Gap Analyses/User Guide/Supporting documents/Example_spatial-analysis-workflow"
  # [optional] location of cloned GapAnalysis repository; we use this in 
  #   7-run_GapAnalysis_package.R
  gap_dir <- "/Users/emily/Documents/GitHub/GapAnalysis/R"
  # set location for private login information (e.g., for GBIF); we use this
  #   in 3-get_occurrence_data.R
  log_loc <- "/Users/emily/Documents/gap-analysis_passwords.txt"
  # print computer name, to let you know you're in the right spot
  print(paste("Working from the lovely", Sys.info()[4]))

## For additional user or workstation (fill in ________ with your info):
} else if (Sys.info()[4] == "________") {
  # main working directory
  main_dir <- "________"
  # [optional] location of cloned GapAnalysis repository; we use this in 
  #   7-run_GapAnalysis_package.R
  gap_dir <- "________"
  # set location for private login information (e.g., for GBIF); we use this
  #   in 3-get_occurrence_data.R
  log_loc <- "________"
  # print computer name, to let you know you're in the right spot
  print(paste("Working from the lovely", Sys.info()[4]))
  
## can add as many additional "else if" sections as needed for other workstations

} else {
  # default, which sets the working directory as the folder from which you
  #   opened the scripts/project
  setwd(getwd())
  print("You should add your info to the 1-set_working_directory.R script so
    this line automatically sets up all the working directories you'll be using!")
}

################################################################################
# Initialize main folders used in spatial-analysis-workflow
################################################################################

# assign the main folders you'll reference in multiple scripts, and create
#   them if they're not already present

taxa_dir <- "target_taxa"
if(!dir.exists(file.path(main_dir, taxa_dir)))
  dir.create(file.path(main_dir, taxa_dir), recursive=T)
# now place your target_taxa_with_synonyms.csv file in this folder

gis_dir <- "gis_layers"
if(!dir.exists(file.path(main_dir, gis_dir)))
  dir.create(file.path(main_dir, gis_dir), recursive=T)
# you will add layers to this folder based on instructions provided in different 
#   scripts in the spatial-analysis-workflow sequence

exsitu_dir <- "exsitu_data"
if(!dir.exists(file.path(main_dir, exsitu_dir)))
  dir.create(file.path(main_dir, exsitu_dir), recursive=T)
# you will add folders and files here when you gather and process ex situ data

occ_dir <- "occurrence_data"
if(!dir.exists(file.path(main_dir, occ_dir)))
  dir.create(file.path(main_dir, occ_dir), recursive=T)
# you will add folders and files here as you get, compile, refine, and visualize
#   occurrence data throughout the spatial-analysis-workflow sequence



