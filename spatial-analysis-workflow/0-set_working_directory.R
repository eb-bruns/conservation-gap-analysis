### 0-set_working_directory.R
### Authors: Shannon M Still & Emily Beckman Bruns
### Supporting institutions: The Morton Arboretum, Botanic Gardens Conservation 
#   International-US, UC Davis Arboretum & Botanic Garden
### Funding: 
#   -- Institute of Museum and Library Services (IMLS MFA program grant
#        MA-30-18-0273-18 to The Morton Arboretum)
### Last Updated: June 2023 ; first written Dec 2020
### R version 4.3.0

### DESCRIPTION:
  ## This script sets the working environment for the computer on which you are
  #     working. You need to add your own computer and file paths by filling 
  #     in the blanks (________) below! The first entry is an example for my
  #     computer.
  ## This script also creates the initial folder structure used for the
  #     scripts in the spatial-analysis-workflow.

################################################################################
# Set working environment depending on your computer
################################################################################

# The following line gives your "nodename", which you will paste into the first
#   blank "________" (line 38)
Sys.info()[4]


## For Emily Bruns:
if (Sys.info()[4] == "Africa.local") {
  # I like my main directory in Google Drive for easy sharing; to do this, 
  #   you first need to install "Drive for desktop", see here:
  #   https://support.google.com/a/users/answer/13022292?hl=en
  # It's also totally great to keep everything local and not linked to the cloud!
  main_dir <- "/Users/emily/Library/CloudStorage/GoogleDrive-ebeckman@mortonarb.org/Shared drives/Global Tree Conservation Program/4. GTCP_Projects/Gap Analyses/User Guide ~ Conservation Gap Analysis/Example_spatial-analysis-workflow"
  log_loc <- "/Users/emily/Documents/gap-analysis_passwords.txt"
  gap_dir <- "/Users/emily/Documents/GitHub/GapAnalysis_EBB/R"
  print(paste("Working from the lovely", Sys.info()[4]))
  
  
## For additional user or workstation (fill in each ________ with your file path)
} else if (Sys.info()[4] == "________") {
  # main working directory (folder where you want most files to be)
  #   if you're not sure how to to find the path of a folder, you can right click
  #   the folder and do the following based on your operating system --
  #   For Mac: click "Get Info" and copy the text in the "Where:" field
  #   For Windows: click "Properties" and copy the text in the "Location:" field
  main_dir <- "________"
  # set location for log-in information text file (e.g., for GBIF)
  #   we use this in 3-get_occurrence_data.R
  log_loc <- "________"
  # [optional] location of cloned GapAnalysis repository; we use this in 
  #   7-run_GapAnalysis_package.R
  # Just leave this blank if you're not to this point in the workflow yet!
  gap_dir <- "________"
  # print computer name, to let you know you're in the right spot
  print(paste("Working from the lovely", Sys.info()[4]))
  
  
## can add as many additional "else if" sections as needed for other workstations

} else {
  # default, which sets the working directory as the folder from which you
  #   opened the scripts/project
  setwd(getwd())
  print("You should add your info to the 0-set_working_directory.R script so
    this line automatically sets up all the working directories you'll be using!")
}

################################################################################
# Initialize main folders used in spatial-analysis-workflow
################################################################################

# assign the main folders you'll reference in multiple scripts, and create
#   them if they're not already present; you don't need to change anything here

# place your target_taxa_with_synonyms.csv file in this folder
taxa_dir <- "target_taxa"
if(!dir.exists(file.path(main_dir, taxa_dir)))
  dir.create(file.path(main_dir, taxa_dir), recursive=T)

# you will add layers to this folder in 1-prep_gis_layers.R
gis_dir <- "gis_layers"
if(!dir.exists(file.path(main_dir, gis_dir)))
  dir.create(file.path(main_dir, gis_dir), recursive=T)

# you will add folders and files here when you gather and process ex situ data
exsitu_dir <- "exsitu_data"
if(!dir.exists(file.path(main_dir, exsitu_dir)))
  dir.create(file.path(main_dir, exsitu_dir), recursive=T)
raw_exsitu <- "raw_exsitu_data"
if(!dir.exists(file.path(main_dir, exsitu_dir, raw_exsitu)))
  dir.create(file.path(main_dir, exsitu_dir, raw_exsitu), recursive=T)
standardized_exsitu <- "standardized_exsitu_data"
if(!dir.exists(file.path(main_dir, exsitu_dir, standardized_exsitu)))
  dir.create(file.path(main_dir, exsitu_dir, standardized_exsitu), recursive=T)

# you will add folders and files here as you get, compile, refine, and visualize
#   occurrence data throughout the spatial-analysis-workflow sequence
occ_dir <- "occurrence_data"
if(!dir.exists(file.path(main_dir, occ_dir)))
  dir.create(file.path(main_dir, occ_dir), recursive=T)
raw_occ <- "raw_occurrence_data"
if(!dir.exists(file.path(main_dir, occ_dir, raw_occ)))
  dir.create(file.path(main_dir, occ_dir, raw_occ), recursive=T)
standardized_occ <- "standardized_occurrence_data"
if(!dir.exists(file.path(main_dir, occ_dir, standardized_occ)))
  dir.create(file.path(main_dir, occ_dir, standardized_occ), recursive=T)



