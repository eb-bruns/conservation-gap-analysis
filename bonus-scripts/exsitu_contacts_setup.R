### exsitu_contacts_setup.R
### Authors: Emily Beckman Bruns
### Supporting institutions: The Morton Arboretum, United States Botanic Garden,
#   Botanic Gardens Conservation International-US, San Diego Botanic Garden,
#   Missouri Botanical Garden
### Last Updated: Jan 2022

### DESCRIPTION:
  # Format BGCI PlantSearch & GardenSearch data for use in an ex situ survey 
  # data call, via a mail merge. **The script will need edits** based on who you 
  # want to contact and how you format your mail merge message (e.g., are you 
  # asking for specific taxa for each institution or the full genus). See
  # mail merge instructions and example here: 
  # https://drive.google.com/drive/folders/1X13TYt9cE3GmciS6H_hxZaiGvZ4UF52d?usp=share_link

### INPUTS:
  # Export from BGCI PlantSearch (taxa & institutions) & GardenSearch (contacts)
  #   databases, which are not publicly available. Find the databases and 
  #   request data here: https://www.bgci.org/resources/bgci-databases/
  # (optional) Target taxa list for your project

### OUTPUT:
  # CSV with columns used in a mail merge message, including: Institution, 
  #   contact_name, target_taxa, and contact_email. Additional optional 
  #   metatdata can be included, like InstitutionType, Latitude, Longitude, 
  #   and/or contact_type (Primary, Director, Curator, Plant Records Officer)

################################################################################

#################
### LIBRARIES ###
#################

my.packages <- c('plyr','xlsx','tidyverse','stringr','textclean','data.table')
#'readxl','leaflet','raster','sp','rgeos','rgdal','RColorBrewer','ggplot2','maptools')
lapply(my.packages, require, character.only=TRUE)
#  install.packages(my.packages) #Turn on to install current versions

select <- dplyr::select

#################
### FUNCTIONS ###
#################


#########################
### WORKING DIRECTORY ###
#########################

main_dir <- "/Volumes/GoogleDrive-103729429307302508433/My Drive/BGCI-US/CWR North America (2021-2022)/Ex situ survey"

###############################################################################
# Load data
###############################################################################

# read in target taxa list
target_sp <- read.csv(file.path(main_dir,
  "North America CWR taxa list with synonyms.csv"),
  header = T, na.strings=c("","NA"), colClasses="character")
head(target_sp)
# Taxon.accepted      Taxon.including.synonyms
# Asimina incana	    Annona incana
# Asimina incana	    Asimina incana
# Asimina longifolia	Asimina angustifolia
# Asimina longifolia	Asimina longifolia
# ...
nrow(target_sp) #266
length(unique(target_sp$Taxon.accepted)) #90

# read in PlantSearch data
ps <- read.csv(file.path(main_dir, "CWR PS match 01.13.22.csv"),
  header = T, na.strings=c("","NA"), colClasses="character")
head(ps) # note that the emails and names in the example below have been removed
         # to protect their privacy
# Taxon.accepted	    Taxon.including.synonyms	Institution	                    PrimaryEmail  DirectorName  DirectorEmail CuratorName	CuratorEmail	PlantRecordsOfficerName	PlantRecordsOfficerEmail	InstitutionType
# Asimina longifolia	Asimina angustifolia      North Carolina Botanical Garden	email@org.org Name          email@org.org Name        email@org.org Name                    email@org.org     	      Botanic Garden
# Asimina longifolia	Asimina angustifolia	    JC Raulston Arboretum	US        email@org.org Name          email@org.org Name        email@org.org Name                    email@org.org     	      Botanic Garden
# Asimina longifolia	Asimina longifolia	      National Plant Germplasm System	email@org.org Name          email@org.org Name        email@org.org Name                    email@org.org     	      Network
# Asimina obovata	    Asimina obovata	          Missouri Botanical Garden	      email@org.org Name          email@org.org Name        email@org.org Name                    email@org.org     	      Botanic Garden
# ...
nrow(ps) #3576

# read in GardenSearch data
# this is only needed if you want to send a message, for example, to all gardens
# in a specific region, versus only contacting gardens that have your target 
# taxa; for the latter, you will only need the PlantSearch export above
gs <- read.csv(file.path(main_dir, "All-GSgardens_Jan25-2022.csv"),
  header = T, na.strings=c("","NA"), colClasses="character")
head(gs) # note that the emails and names in the example below have been removed
         # to protect their privacy
# Institution	                    CountryCode	  PrimaryEmail	DirectorName	DirectorEmail	CuratorName	CuratorEmail	PlantRecordsOfficerName	PlantRecordsOfficerEmail	InstitutionType
# North Carolina Botanical Garden	US            email@org.org Name          email@org.org Name        email@org.org Name                    email@org.org     	      Botanic Garden
# JC Raulston Arboretum           US            email@org.org Name          email@org.org Name        email@org.org Name                    email@org.org     	      Botanic Garden
# National Plant Germplasm System	US            email@org.org Name          email@org.org Name        email@org.org Name                    email@org.org     	      Network
# Missouri Botanical Garden	      US            email@org.org Name          email@org.org Name        email@org.org Name                    email@org.org     	      Botanic Garden
# ...
nrow(gs) #3736

###############################################################################
# Create summary table with taxa at each instittuion (for mail merge)
###############################################################################

# replace strange characters in taxon names
ps$Taxon.including.synonyms <- gsub(".var\\.."," var. ",ps$Taxon.including.synonyms)
ps$Taxon.including.synonyms <- gsub(".subsp\\.."," subsp. ",ps$Taxon.including.synonyms)
ps$Taxon.including.synonyms <- gsub(".f\\.."," f. ",ps$Taxon.including.synonyms)

# summary table with species at each institution
ps_pivot <- ps %>%
  arrange(Taxon.including.synonyms) %>%
  distinct(Institution,Taxon.including.synonyms,.keep_all=T) %>%
  group_by(Institution) %>%
  mutate(target_taxa = paste0(Taxon.including.synonyms,collapse = ", ")) %>%
  mutate(accepted_taxa = paste0(Taxon.accepted,collapse = ", ")) %>%
  add_tally() %>%
  ungroup() %>%
  distinct(Institution,.keep_all=T) %>%
  select(-Taxon.accepted,-Taxon.including.synonyms,
    -PlantID,-GenHyb,-Gen,-SpecHyb,-Spec,-infRank,-InfraEPI,-Cultivar) %>%
  arrange(Institution)
head(as.data.frame(ps_pivot)); nrow(ps_pivot) #422

# fix institution names
ps_pivot <- ps_pivot %>%
  separate("Institution","Institution",sep=" / \\?|, The",remove=T)
unique(ps_pivot$Institution)

# remove titles after contact names
ps_pivot <- ps_pivot %>%
  separate("DirectorName","DirectorName",sep=", |~|\\(",remove=T) %>%
  separate("CuratorName","CuratorName",sep=", |~|\\(",remove=T) %>%
  separate("PlantRecordsOfficerName","PlantRecordsOfficerName",sep=", |~|\\(",remove=T)

# clean up contact name and email columns
  # if curator and plant records emails are the same, remove plant records email
ps_pivot[which(ps_pivot$PlantRecordsOfficerEmail ==
  ps_pivot$CuratorEmail),]$PlantRecordsOfficerEmail <- NA
  # if primary and director emails are the same, remove primary email
ps_pivot[which(ps_pivot$DirectorEmail ==
  ps_pivot$PrimaryEmail),]$PrimaryEmail <- NA
  # if director name but no email, remove name
ps_pivot[which(!is.na(ps_pivot$DirectorName) &
  is.na(ps_pivot$DirectorEmail)),]$DirectorName <- NA
  # if curator email but no name, add "Curator" as name
ps_pivot[which(!is.na(ps_pivot$CuratorEmail) &
  is.na(ps_pivot$CuratorName)),]$CuratorName <- "Curator"
  # if curator name but no email, remove name
ps_pivot[which(!is.na(ps_pivot$CuratorName) &
  is.na(ps_pivot$CuratorEmail)),]$CuratorName <- NA
  # if plant records email but no name, add "Plant Records Officer" as name
ps_pivot[which(!is.na(ps_pivot$PlantRecordsOfficerEmail) &
  is.na(ps_pivot$PlantRecordsOfficerName)),]$PlantRecordsOfficerName <- "Plant Records Officer"
  # if plant records name but no email, remove name
ps_pivot[which(!is.na(ps_pivot$PlantRecordsOfficerName) &
  is.na(ps_pivot$PlantRecordsOfficerEmail)),]$PlantRecordsOfficerName <- NA

# fill contact email and name columns

  ## Curator and Plant Records Officer names
ps_pivot$PlantRecordsOfficerName[which(is.na(ps_pivot$PlantRecordsOfficerName))] <- ""
ps_pivot$CuratorName[which(is.na(ps_pivot$CuratorName))] <- ""
ps_pivot$contact_name <- NA
for(i in 1:nrow(ps_pivot)){
  if(ps_pivot$PlantRecordsOfficerName[i] == ""){
    ps_pivot$contact_name[i] <- str_trim(str_to_title(ps_pivot$CuratorName[i]))
  } else if(ps_pivot$CuratorName[i] == ""){
    ps_pivot$contact_name[i] <- str_trim(str_to_title(ps_pivot$PlantRecordsOfficerName[i]))
  } else if (str_trim(str_to_title(ps_pivot$PlantRecordsOfficerName[i])) !=
      str_trim(str_to_title(ps_pivot$CuratorName[i]))){
    ps_pivot$contact_name[i] <- paste0(
      str_trim(str_to_title(ps_pivot$CuratorName[i]))," (",ps_pivot$CuratorEmail[i],") and ",
      str_trim(str_to_title(ps_pivot$PlantRecordsOfficerName[i]))," (",ps_pivot$PlantRecordsOfficerEmail[i],")")
  } else {
    ps_pivot$contact_name[i] <- str_trim(str_to_title(ps_pivot$CuratorName[i]))
  }
}
ps_pivot$contact_name[which(ps_pivot$contact_name == "")] <- NA
  # check out the results
sort(unique(ps_pivot$contact_name))
  # replace anything strange
ps_pivot$contact_name <- gsub("Secretary   : ","",ps_pivot$contact_name,)
str(ps_pivot)
  # create email column
plantrecords <- ps_pivot
plantrecords$contact_email <- plantrecords$PlantRecordsOfficerEmail
plantrecords <- plantrecords[which(!is.na(plantrecords$contact_email)),]
plantrecords$contact_type <- "Plant Records Officer"
nrow(plantrecords) #186
curator <- ps_pivot
curator$contact_email <- curator$CuratorEmail
curator <- curator[which(!is.na(curator$contact_email)),]
curator$contact_type <- "Curator"
nrow(curator) #325
ps_join <- full_join(plantrecords,curator)
nrow(ps_join) #511

  # only add director if no other contact, primary as last resort
add <- ps_pivot[which(is.na(ps_pivot$contact_name)),]
  ## Director names
add$contact_name <- str_trim(str_to_title(add$DirectorName))
add$contact_name[which(is.na(add$contact_name))] <- "Colleague"
add$contact_name <- gsub(" And "," and ",add$contact_name)
add$contact_name <- gsub("^Xx$","Colleague",add$contact_name)
add$contact_name <- gsub("^-$","Colleague",add$contact_name)
add$contact_type <- "Director"
unique(add$contact_name)
  # create email column
add$contact_email <- add$DirectorEmail
add[which(is.na(add$DirectorEmail)),]$contact_email <-
  add[which(is.na(add$DirectorEmail)),]$PrimaryEmail
add[which(is.na(add$DirectorEmail) & !is.na(add$contact_email)),]$contact_type <- "Primary"
add[which(is.na(add$contact_email)),]
  # join everything together
ps_join2 <- full_join(ps_join,add)
ps_join2[which(is.na(ps_join2$contact_email)),]$contact_type <- "X NO CONTACT INFO"
  # arrange by number of species
ps_join2 <- ps_join2 %>% arrange(desc(n),contact_type)
nrow(ps_join2) #566
str(ps_join2)

# final edits

### !! GARDENS WITH NO CONTACT INFO & NUMBER OF TARGET TAXA AT EACH !!
ps_join2[which(is.na(ps_join2$contact_email)),c(1,15)]
  # add contacts found online
ps_join2[which(ps_join2$Institution == "Institution name"),16:18] <-
 list("Contact Name","name@email.com","Director")
ps_join2[which(ps_join2$Institution == "Institution name"),16:18] <-
 list("Contact Name","name@email.com","Curator")
ps_join2[which(ps_join2$Institution == "Institution name"),16:18] <-
 list("Colleague","name@email.com","Primary")

### !! NETWORKS !!
ps_join2[which(ps_join2$InstitutionType == "Network"),1:2]
  # remove netwoks that overlap with other institutions we are contacting
ps_final <- ps_join2[which(ps_join2$Institution != "Center for Plant Conservation (USA)" &
                           ps_join2$Institution != "Bishop Museum - Checklist of Cultivated Plants of Hawai'i"),]
nrow(ps_final) #563

# set up dummy row as first row, with longest inputs in each column,
#   to be sure nothing gets trimmed during mail merge
instl <- ps_final[nchar(ps_final$Institution)==max(nchar(ps_final$Institution)),]$Institution[1]
namel <- ps_final[nchar(ps_final$contact_name)==max(nchar(ps_final$contact_name)),]$contact_name[1]
taxal <- ps_final[nchar(ps_final$target_taxa)==max(nchar(ps_final$target_taxa)),]$target_taxa[1]
dummy_row <- data.frame(Institution=instl, contact_name=namel, target_taxa=taxal,
  contact_email="my-email@org.org")
ps_final <- full_join(dummy_row,ps_final)

# write file
write.xlsx(ps_final, file.path(main_dir,"Mail Merge",
  paste0("NA-CWR_ExSituSurvey_DirectEmail_Contacts_",Sys.Date(), ".xlsx")),
  col.names = T, row.names=F, append = F)

###############################################################################
# Create list of U.S. and Canada institutions not already contacted (for secondary mail merge)
###############################################################################

# remove institutions already contacted
gs_nam <- gs %>% filter(CountryCode == "US" | CountryCode == "CA")
nrow(gs_nam) #1156
direct_contact <- unique(ps$Institution)
gs_contact <- gs_nam %>% filter(!(Institution %in% direct_contact))
nrow(gs_contact) #942
unique(gs_contact$InstitutionType)

# look at networks
gs_contact[which(gs_contact$InstitutionType == "Network"),]
# remove networks
gs_contact <- gs_contact %>% filter(InstitutionType != "Network")
nrow(gs_contact) #935

# fix institution names
gs_contact <- gs_contact %>%
  separate("Institution","Institution",sep=" / \\?|, The",remove=T)

# remove titles after contact names
gs_contact <- gs_contact %>%
  separate("DirectorName","DirectorName",sep=", |~|\\(",remove=T) %>%
  separate("CuratorName","CuratorName",sep=", |~|\\(",remove=T) %>%
  separate("PlantRecordsOfficerName","PlantRecordsOfficerName",sep=", |~|\\(",remove=T)

# clean up contact name and email columns
  # if curator and plant records emails are the same, remove plant records email
gs_contact[which(gs_contact$PlantRecordsOfficerEmail ==
  gs_contact$CuratorEmail),]$PlantRecordsOfficerEmail <- NA
  # if primary and director emails are the same, remove primary email
gs_contact[which(gs_contact$DirectorEmail ==
  gs_contact$PrimaryEmail),]$PrimaryEmail <- NA
  # if director name but no email, remove name
gs_contact[which(!is.na(gs_contact$DirectorName) &
  is.na(gs_contact$DirectorEmail)),]$DirectorName <- NA
  # if curator email but no name, add "Curator" as name
gs_contact[which(!is.na(gs_contact$CuratorEmail) &
  is.na(gs_contact$CuratorName)),]$CuratorName <- "Curator"
  # if curator name but no email, remove name
gs_contact[which(!is.na(gs_contact$CuratorName) &
  is.na(gs_contact$CuratorEmail)),]$CuratorName <- NA
  # if plant records email but no name, add "Plant Records Officer" as name
gs_contact[which(!is.na(gs_contact$PlantRecordsOfficerEmail) &
  is.na(gs_contact$PlantRecordsOfficerName)),]$PlantRecordsOfficerName <- "Plant Records Officer"
  # if plant records name but no email, remove name
gs_contact[which(!is.na(gs_contact$PlantRecordsOfficerName) &
  is.na(gs_contact$PlantRecordsOfficerEmail)),]$PlantRecordsOfficerName <- NA

# fill contact email and name columns

  ## Curator and Plant Records Officer names
gs_contact$PlantRecordsOfficerName[which(is.na(gs_contact$PlantRecordsOfficerName))] <- ""
gs_contact$CuratorName[which(is.na(gs_contact$CuratorName))] <- ""
gs_contact$contact_name <- NA
for(i in 1:nrow(gs_contact)){
  if(gs_contact$PlantRecordsOfficerName[i] == ""){
    gs_contact$contact_name[i] <- str_trim(str_to_title(gs_contact$CuratorName[i]))
  } else if(gs_contact$CuratorName[i] == ""){
    gs_contact$contact_name[i] <- str_trim(str_to_title(gs_contact$PlantRecordsOfficerName[i]))
  } else if (str_trim(str_to_title(gs_contact$PlantRecordsOfficerName[i])) !=
      str_trim(str_to_title(gs_contact$CuratorName[i]))){
    gs_contact$contact_name[i] <- paste0(
      str_trim(str_to_title(gs_contact$CuratorName[i]))," (",gs_contact$CuratorEmail[i],") and ",
      str_trim(str_to_title(gs_contact$PlantRecordsOfficerName[i]))," (",gs_contact$PlantRecordsOfficerEmail[i],")")
  } else {
    gs_contact$contact_name[i] <- str_trim(str_to_title(gs_contact$CuratorName[i]))
  }
}
gs_contact$contact_name[which(gs_contact$contact_name == "")] <- NA
  # check out the results
sort(unique(gs_contact$contact_name))
  # create email column
plantrecords <- gs_contact
plantrecords$contact_email <- plantrecords$PlantRecordsOfficerEmail
plantrecords <- plantrecords[which(!is.na(plantrecords$contact_email)),]
plantrecords$contact_type <- "Plant Records Officer"
nrow(plantrecords) #96
curator <- gs_contact
curator$contact_email <- curator$CuratorEmail
curator <- curator[which(!is.na(curator$contact_email)),]
curator$contact_type <- "Curator"
nrow(curator) #398
gs_join <- full_join(plantrecords,curator)
nrow(gs_join) #494

  # only add director if no other contact, primary as last resort
add <- gs_contact[which(is.na(gs_contact$contact_name)),]
  ## Director names
add$contact_name <- str_trim(str_to_title(add$DirectorName))
add$contact_name[which(is.na(add$contact_name))] <- "Colleague"
add$contact_name <- gsub(" And "," and ",add$contact_name)
add$contact_name <- gsub("^Xx$","Colleague",add$contact_name)
add$contact_name <- gsub("^-$","Colleague",add$contact_name)
add$contact_type <- "Director"
unique(add$contact_name)
  # create email column
add$contact_email <- add$DirectorEmail
add[which(is.na(add$DirectorEmail)),]$contact_email <-
  add[which(is.na(add$DirectorEmail)),]$PrimaryEmail
add[which(is.na(add$DirectorEmail) & !is.na(add$contact_email)),]$contact_type <- "Primary"
add[which(is.na(add$contact_email)),]
  # join everything together
gs_join2 <- full_join(gs_join,add)
gs_join2[which(is.na(gs_join2$contact_email)),]$contact_type <- "X NO CONTACT INFO"
nrow(gs_join2) #1011
str(gs_join2)

# final edits

### !! GARDENS WITH NO CONTACT INFO !!
gs_join2[which(is.na(gs_join2$contact_email)),2]

# set up dummy row as first row, with longest inputs in each column,
#   to be sure nothing gets trimmed during mail merge
instl <- gs_join2[nchar(gs_join2$Institution)==max(nchar(gs_join2$Institution)),]$Institution[1]
namel <- gs_join2[nchar(gs_join2$contact_name)==max(nchar(gs_join2$contact_name)),]$contact_name[1]
dummy_row <- data.frame(Institution=instl, contact_name=namel,
  contact_email="my-email@org.org")
gs_final <- full_join(dummy_row,gs_join2)

# add entry that sends to a colleague, so they can forward along
add_row <- data.frame(Institution="your institution", contact_name="Colleague",
  contact_email="my-email@org.org")
gs_final <- full_join(gs_final,add_row)

# write file
write.xlsx(gs_final, file.path(main_dir,"Mail Merge",
  paste0("NA-CWR_ExSituSurvey_BroaderEmail_Contacts_",Sys.Date(), ".xlsx")),
  col.names = T, row.names=F, append = F)
