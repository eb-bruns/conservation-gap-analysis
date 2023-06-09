### 7-filter_occurrence_points.R
### Author: Emily Beckman Bruns
### Supporting institutions: The Morton Arboretum, Botanic Gardens Conservation 
#   International-US, United States Botanic Garden, San Diego Botanic Garden,
#   Missouri Botanical Garden
### Funding: 
#   -- United States Botanic Garden (cooperative agreement with San Diego
#        Botanic Garden)
#   -- NSF (award 1759759 to The Morton Arboretum)
### Last Updated: June 2023
### R version 4.3.0

### DESCRIPTION:
  ## This script does a final filter of occurrence points, based on the flagging
  #   columns (added in 5-flag_occurrence_points.R) you choose to use, and any
  #   manual edits provided through the manual_point_edits.R file (see INPUTS
  #   below for details).
  ## Note that currently the same flagging columns are used for every target 
  #   taxon; this makes it easier from a methods standpoint. But, if you want
  #   to use different flagging columns for different taxa, the script could
  #   be edited to do so. One option for this would be adding a column to the 
  #   manual_point_edits.csv that lists the filters you want to use... not a
  #   super simple or quick solution though.

### INPUTS:
  ## taxon_points_ready-to-vet (folder) 
  #   Occurrence data output from 5-flag_occurrence_points.R
  ## (optional) manual_point_edits.csv
  #   Instructions for using this file are in the "Manual point edits" tab of 
  #   Gap-analysis-workflow_metadata. The file has four vital columns we use in
  #   this script; these include:
  #     1. taxon_name_accepted ~ Accepted taxon name
  #     2. remove_id ~ UID of point(s) to remove, which aren't already removed by a
  #         flagging filter you're using. The UID for a point can be found by 
  #         clicking on the point in its interactive map (created in 
  #         6-visualize_occurrence_points.R). Multiple UIDs to remove are 
  #         separated by a semicolon.
  #     3. remove_bounding_box ~ Coordinates for bounding box(es) where all 
  #         points inside will be removed.Format for the bounding box is:
  #         top-left_lat, top-left_long, bottom-right_lat, bottom-right_long
  #         In other words, coordinates of the top-left corner followed by 
  #         coordinates of the bottom-right corner of the bounding box. Multiple
  #         bounding boxes are separated by a semicolon. Note that your 
  #         bounding box cannot cross the 180/-180 line (near the international 
  #         date line); should very rarely be a concern.
  #     4. keep_id ~ UID of point(s) to keep, which would otherwise be removed 
  #         by a flagging filter you're using. The UID for a point can be found  
  #         by clicking on the point in its interactive map (created in 
  #         6-visualize_occurrence_points.R). Multiple UIDs to keep are 
  #         separated by a semicolon.
  
### OUTPUTS:
  ## taxon_points_final (folder)
  #   For each taxon in your target taxa list, a CSV of filtered occurrence 
  #   records (e.g., Asimina_triloba.csv)

################################################################################
# Load libraries
################################################################################

# load packages
my.packages <- c('tidyverse','textclean','tools')
  # versions I used (in the order listed above): 2.0.0, 0.9.3, 4.3.0
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
data_out <- "taxon_points_final"
if(!dir.exists(file.path(main_dir,occ_dir,standardized_occ,data_out)))
  dir.create(file.path(main_dir,occ_dir,standardized_occ,data_out), 
             recursive=T)

# assign folder where you have input data (saved in 5-flag_occurrence_points.R)
data_in <- "taxon_points_ready-to-vet"

################################################################################
# Filter occurrence points by flags from script #5 and any manual edits in
#   manual_point_edits.R file
################################################################################

# read in manual point edits file
manual_edits <- read.csv(file.path(main_dir,occ_dir,standardized_occ,
                               "manual_point_edits.csv"),
                       header=T, colClasses="character", na.strings=c("","NA"))
# remove all spaces in the manual edits, to standardize in case manual mistakes
manual_edits <- manual_edits %>%
  mutate(across(remove_id:keep_id, ~
                  str_remove_all(.x, pattern = fixed(" "))))
manual_edits

# list of taxon files to iterate through
taxon_files <- list.files(path=file.path(main_dir,occ_dir,standardized_occ,data_in), 
                          ignore.case=FALSE, full.names=FALSE, recursive=TRUE)
target_taxa <- file_path_sans_ext(taxon_files)

# start a table to add summary of results for each species
summary_tbl <- data.frame(taxon_name_accepted = "start", final_pts = "start")


# cycle through each target taxon to remove flagged points and save new version
for (i in 1:length(target_taxa)){
  
  taxon_file <- target_taxa[i]
  taxon_nm <- mgsub(taxon_file, c("_","_var_","_subsp_"), 
                    c(" "," var. "," subsp. "))
  
  cat("Starting ", taxon_nm, ", ", i, " of ", length(target_taxa), "\n", sep="")
  
  ## read in records
  taxon_now <- read.csv(file.path(main_dir,occ_dir,standardized_occ,data_in,
                                  paste0(taxon_file, ".csv")))
  orig_num_pts <- nrow(taxon_now)
  # make sure all the T/F columns are logical type
  taxon_now <- taxon_now %>% mutate(across(.cen:.yrna, as.logical))
  
  
  ## filter occurrence data based on filter columns created in 5-flag_occurrence_points.R
  taxon_now <- taxon_now %>%
    filter(
  # we keep all ex situ points even if flagged, so they must be manually
  #   selected for removal if bad
      database == "Ex_situ" |
  # choose which filters you want to use: comment out those you want to skip
        .cen & 
        .inst & 
        .outl &
        .con & 
        #.urb & 
        .nativectry &
        .yr1950 & 
         #.yr1980 & 
         #.yrna &
         #.unc &
         .elev &
         .rec
    )
  cat(paste0("--Removed ",orig_num_pts-nrow(taxon_now)," points based on flagging colums\n"))
  
  
  ## check document with manual point edits to see if anything needs to be
  ##    removed or added back in
  # get manual edits row for the current target taxon
  taxon_edits <- manual_edits[which(
    manual_edits$taxon_name_accepted == taxon_nm),]
  # remove if ID listed in remove_id
  if(!is.na(taxon_edits$remove_id)){
    remove <- unlist(strsplit(taxon_edits$remove_id,";"))
    taxon_now <- taxon_now %>% filter(!(UID %in% remove))
    cat(paste0("--Removed ",length(remove)," points based on IDs to remove\n"))
  }
  # remove if inside remove_bounding_box
  if(!is.na(taxon_edits$remove_bounding_box)){
    boxes <- unlist(strsplit(taxon_edits$remove_bounding_box,";"))
    for(j in 1:length(boxes)){
      bounds <- unlist(strsplit(boxes[j],","))
      # note that if your bounding box crosses longitude 180/-180, which is near
      #   the international date line, then the longitude comparison here won't 
      #   work! - the filter would need to be edited to catch that exception
      remove <- taxon_now %>%
        filter(decimalLatitude < as.numeric(bounds[1]) & 
               decimalLongitude > as.numeric(bounds[2]) &
               decimalLatitude > as.numeric(bounds[3]) &
               decimalLongitude < as.numeric(bounds[4]))
      taxon_now <- taxon_now %>%
        filter(!(UID %in% unique(remove$UID)))
      cat(paste0("--Removed ",nrow(remove)," points based on bounding box ", j,"\n"))
    }
  }
  # add back if ID listed in keep_id
  if(!is.na(taxon_edits$keep)){
    keep <- unlist(strsplit(taxon_edits$keep,";"))
    add <- taxon_now %>% filter(UID %in% keep)
    taxon_now <- suppressMessages(full_join(taxon_now,add))
    cat(paste0("--Added back ",length(keep)," points based on IDs to keep\n"))
  }
  
  ## write final occurrence point file
  write.csv(taxon_now, file.path(main_dir,occ_dir,standardized_occ,data_out,
                                 paste0(taxon_file,".csv")), 
            row.names=FALSE)
  
  # add data to summary table
  summary_add <- data.frame(
    taxon_name_accepted = taxon_nm,
    final_pts = nrow(taxon_now))
  summary_tbl[i,] <- summary_add
  
  ## cat update
  cat("Original points: ", orig_num_pts, "\n", sep="")
  cat("Final points: ", nrow(taxon_now), "\n\n", sep="")

}

# add summary of points to summary we created in 5-compile_occurrence_points.R
file_nm <- list.files(path = file.path(main_dir,occ_dir,standardized_occ),
                      pattern = "summary_of_occurrences", full.names = T)
orig_summary <- read.csv(file_nm, colClasses = "character")
  # keep just the columns from script 5 output, in case you're running this script a second time
orig_summary <- orig_summary %>% select(taxon_name_accepted:.rec)
summary_tbl2 <- full_join(orig_summary,summary_tbl,by="taxon_name_accepted")
summary_tbl2

# write summary table
write.csv(summary_tbl2, file.path(main_dir,occ_dir,standardized_occ,
                                  paste0("summary_of_occurrences_", Sys.Date(), ".csv")),row.names = F)
