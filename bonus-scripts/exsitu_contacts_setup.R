### exsitu_contacts_setup.R
### Author: Emily Beckman Bruns
### Supporting institutions: The Morton Arboretum, United States Botanic Garden,
#   Botanic Gardens Conservation International-US, San Diego Botanic Garden,
#   Missouri Botanical Garden
### Last Updated: Jan 2022

### DESCRIPTION:
  # Format BGCI PlantSearch & GardenSearch data for use in an ex situ survey 
  # data call, via a mail merge. **The script will need edits** based on who you 
  # want to contact and how you format your mail merge message (e.g., are you 
  # asking for specific taxa for each institution or the full genus). See
  # mail merge instructions and example here (restricted access): 
  # https://drive.google.com/drive/folders/1X13TYt9cE3GmciS6H_hxZaiGvZ4UF52d?usp=share_link

### INPUTS:
  ## Export from BGCI PlantSearch (taxa & institutions) & GardenSearch (contacts)
  #   databases, which are not publicly available. Find the databases and 
  #   request data here: https://www.bgci.org/resources/bgci-databases/
  #     *Here the required columns you need in the export:
  #     GardenID, Institution, PrimaryEmail, DirectorName, DirectorEmail, 
  #     CuratorName, CuratorEmail, PlantRecordsOfficerName, PlantRecordsOfficerEmail
  #     *Optionally you may need a 'Taxon' column (or split out into Gen, Spec, 
  #     infRank, etc.) if you want the mail merge to list the taxa the 
  #     the institution reports to PlantSearch
  ## [optional] Target taxa list for your project

### OUTPUT:
  ## CSV with columns used in a mail merge message, including: 
  #   contact_name, contact_email, contact_type (Primary, Director, 
  #   Curator, Plant Records Officer), and [optionally] target_taxa and 
  #   Institution (name of institution). 

################################################################################

#################
### LIBRARIES ###
#################

my.packages <- c('tidyverse','xlsx')
lapply(my.packages, require, character.only=TRUE)
#  install.packages(my.packages) #Turn on to install current versions

#########################
### WORKING DIRECTORY ###
#########################

# change this to the location of your contact list (and taxon list if using)
main_dir <- "/Users/emily/Desktop"

# add your email address here for creating a test mail merge row later:
my_email <- "my_email@institution.org"

###############################################################################
# 1. Load data
###############################################################################

# read in PlantSearch data (change file name to yours)
ps <- read.csv(file.path(main_dir, "BGCI_PlantSearch_export.csv"),
               header = T, na.strings=c("","NA"), colClasses="character")
head(ps) # note that the emails and names in the example below have been removed
# to protect their privacy
# Taxon.accepted	    Taxon.including.synonyms	Institution	                    PrimaryEmail  DirectorName  DirectorEmail CuratorName	CuratorEmail	PlantRecordsOfficerName	PlantRecordsOfficerEmail	InstitutionType
# Asimina longifolia	Asimina angustifolia      North Carolina Botanical Garden	email@org.org Name          email@org.org Name        email@org.org Name                    email@org.org     	      Botanic Garden
# Asimina longifolia	Asimina angustifolia	    JC Raulston Arboretum	US        email@org.org Name          email@org.org Name        email@org.org Name                    email@org.org     	      Botanic Garden
# Asimina longifolia	Asimina longifolia	      National Plant Germplasm System	email@org.org Name          email@org.org Name        email@org.org Name                    email@org.org     	      Network
# Asimina obovata	    Asimina obovata	          Missouri Botanical Garden	      email@org.org Name          email@org.org Name        email@org.org Name                    email@org.org     	      Botanic Garden
# ...
nrow(ps)

###############################################################################
## ONLY RUN THIS SECTION IF YOU'RE LISTING INSTITUTION-SPECIFIC TARGET TAXA IN 
##    YOUR DATA REQUEST !
# 2. Create summary table with taxa at each institution (for mail merge)
###############################################################################

# read in target taxa list if you're using one
target_sp <- read.csv(file.path(main_dir,"target_taxa_list_plus_synonyms.csv"),
                      header = T, na.strings=c("","NA"), colClasses="character")
head(target_sp)
# Taxon.accepted      Taxon.including.synonyms
# Asimina incana	    Annona incana
# Asimina incana	    Asimina incana
# Asimina longifolia	Asimina angustifolia
# Asimina longifolia	Asimina longifolia
# ...
nrow(target_sp)
length(unique(target_sp$Taxon.accepted))

# if needed, replace strange characters in taxon names
ps$Taxon.including.synonyms <- gsub(".var\\.."," var. ",ps$Taxon.including.synonyms)
ps$Taxon.including.synonyms <- gsub(".subsp\\.."," subsp. ",ps$Taxon.including.synonyms)
ps$Taxon.including.synonyms <- gsub(".f\\.."," f. ",ps$Taxon.including.synonyms)

# create summary table with species at each institution
ps <- ps %>%
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
head(as.data.frame(ps)); nrow(ps)

###############################################################################
# 3. Organize table for mail merge
###############################################################################

# if needed, fix institution names
ps <- ps %>%
  separate("Institution","Institution",sep=" / \\?|, The",remove=T)
unique(ps$Institution)

# remove titles after contact names
ps <- ps %>%
  separate("DirectorName","DirectorName",sep=", |~|\\(",remove=T) %>%
  separate("CuratorName","CuratorName",sep=", |~|\\(",remove=T) %>%
  separate("PlantRecordsOfficerName","PlantRecordsOfficerName",sep=", |~|\\(",remove=T)

# clean up contact name and email columns
  # if curator and plant records emails are the same, remove plant records email
ps[which(ps$PlantRecordsOfficerEmail ==
  ps$CuratorEmail),]$PlantRecordsOfficerEmail <- NA
  # if primary and director emails are the same, remove primary email
ps[which(ps$DirectorEmail ==
  ps$PrimaryEmail),]$PrimaryEmail <- NA
  # if director name but no email, remove name
ps[which(!is.na(ps$DirectorName) &
  is.na(ps$DirectorEmail)),]$DirectorName <- NA
  # if curator email but no name, add "Curator" as name
ps[which(!is.na(ps$CuratorEmail) &
  is.na(ps$CuratorName)),]$CuratorName <- "Curator"
  # if curator name but no email, remove name
ps[which(!is.na(ps$CuratorName) &
  is.na(ps$CuratorEmail)),]$CuratorName <- NA
  # if plant records email but no name, add "Plant Records Officer" as name
ps[which(!is.na(ps$PlantRecordsOfficerEmail) &
  is.na(ps$PlantRecordsOfficerName)),]$PlantRecordsOfficerName <- "Plant Records Officer"
  # if plant records name but no email, remove name
ps[which(!is.na(ps$PlantRecordsOfficerName) &
  is.na(ps$PlantRecordsOfficerEmail)),]$PlantRecordsOfficerName <- NA

### fill contact email and name columns...

  ## Curator and Plant Records Officer names
ps$PlantRecordsOfficerName[which(is.na(ps$PlantRecordsOfficerName))] <- ""
ps$CuratorName[which(is.na(ps$CuratorName))] <- ""
ps$contact_name <- NA
for(i in 1:nrow(ps)){
  if(ps$PlantRecordsOfficerName[i] == ""){
    ps$contact_name[i] <- str_trim(str_to_title(ps$CuratorName[i]))
  } else if(ps$CuratorName[i] == ""){
    ps$contact_name[i] <- str_trim(str_to_title(ps$PlantRecordsOfficerName[i]))
  } else if (str_trim(str_to_title(ps$PlantRecordsOfficerName[i])) !=
      str_trim(str_to_title(ps$CuratorName[i]))){
    ps$contact_name[i] <- paste0(
      str_trim(str_to_title(ps$CuratorName[i]))," (",ps$CuratorEmail[i],") and ",
      str_trim(str_to_title(ps$PlantRecordsOfficerName[i]))," (",ps$PlantRecordsOfficerEmail[i],")")
  } else {
    ps$contact_name[i] <- str_trim(str_to_title(ps$CuratorName[i]))
  }
}
ps$contact_name[which(ps$contact_name == "")] <- NA
  # check out the results
sort(unique(ps$contact_name))
  # replace anything strange
ps$contact_name <- gsub("Secretary   : ","",ps$contact_name,)
str(ps)
  # create email column
plantrecords <- ps
plantrecords$contact_email <- plantrecords$PlantRecordsOfficerEmail
plantrecords <- plantrecords[which(!is.na(plantrecords$contact_email)),]
plantrecords$contact_type <- "Plant Records Officer"
nrow(plantrecords)
curator <- ps
curator$contact_email <- curator$CuratorEmail
curator <- curator[which(!is.na(curator$contact_email)),]
curator$contact_type <- "Curator"
nrow(curator)
ps_join <- full_join(plantrecords,curator)
nrow(ps_join)

  # only add director if no other contact, primary as last resort
add <- ps[which(is.na(ps$contact_name)),]
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

## arrange by number of target taxa, if you ran section 2
if("n" %in% colnames(ps_join2)){
  ps_join2 <- ps_join2 %>% arrange(desc(n),contact_type)
}

nrow(ps_join2)
str(ps_join2)

### final edits...

### !! GARDENS WITH NO CONTACT INFO
ps_join2[which(is.na(ps_join2$contact_email)),1:2]
  # if desired, can add contacts found online; example formatting:
ps_join2[which(ps_join2$Institution == "Institution name"),
         c("contact_name","contact_email","contact_type")] <-
         list("Contact Name","name@email.com","Director")
  # now we'll remove anything that's still without contact info
ps_join2 <- ps_join2 %>% filter(!is.na(contact_email))

### !! NETWORKS !!
ps_join2[which(ps_join2$InstitutionType == "Network"),1:2]
  # remove specific networks that overlap with other institutions we are contacting
ps_join2 <- ps_join2[which(ps_join2$Institution != "Catalogue of Medicinal Plants of Ukrainian Botanic Gardens and Parks" &
                           ps_join2$Institution != "Bishop Museum - Checklist of Cultivated Plants of Hawai'i"),]

nrow(ps_join2)

# set up dummy row as first row, with longest inputs in each column and
#   your own email, to be sure none of the fields get trimmed during mail merge
ps_final <- ps_join2
instl <- ps_final[nchar(ps_final$Institution)==max(nchar(ps_final$Institution)),]$Institution[1]
namel <- ps_final[nchar(ps_final$contact_name)==max(nchar(ps_final$contact_name)),]$contact_name[1]
if("target_taxa" %in% colnames(ps_final)){
  taxal <- ps_final[nchar(ps_final$target_taxa)==max(nchar(ps_final$target_taxa)),]$target_taxa[1]
  dummy_row <- data.frame(Institution=instl, contact_name=namel, target_taxa=taxal, contact_email=my_email) 
} else {
  dummy_row <- data.frame(Institution=instl, contact_name=namel, contact_email=my_email) 
}
ps_final <- full_join(dummy_row,ps_final)
nrow(ps_final)

### *IF* YOU'RE NOT INCLUDING THE INSTITUTION NAME IN YOUR EMAIL MESSAGE:
#   remove full duplicates of contact name and email (so no one gets two exact 
#   copies of the email)
dup_test <- paste(ps_final$contact_name,ps_final$contact_email)
unique(ps_final[duplicated(dup_test) | duplicated(dup_test, fromLast=TRUE), ])
ps_final <- ps_final[!duplicated(dup_test), ]
nrow(ps_final)

# write file
write.xlsx(ps_final, file.path(main_dir,
  paste0("ExSituSurvey_DirectEmail_Contacts_",Sys.Date(), ".xlsx")),
  col.names = T, row.names=F, append = F)










# OPTIONAL; only possible with a full GardenSearch export...

###############################################################################
# Create list of all GardenSearch U.S. and Canada institutions not already 
#   contacted (for secondary mail merge)
###############################################################################

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
nrow(gs)

# remove institutions already contacted
  # first select countries you're interested in
gs_nam <- gs %>% filter(CountryCode == "US" | CountryCode == "CA")
nrow(gs_nam)
direct_contact <- unique(ps$Institution)
gs_contact <- gs_nam %>% filter(!(Institution %in% direct_contact))
nrow(gs_contact)
unique(gs_contact$InstitutionType)

# look at networks
gs_contact[which(gs_contact$InstitutionType == "Network"),]
# remove networks
gs_contact <- gs_contact %>% filter(InstitutionType != "Network")
nrow(gs_contact)

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
nrow(plantrecords)
curator <- gs_contact
curator$contact_email <- curator$CuratorEmail
curator <- curator[which(!is.na(curator$contact_email)),]
curator$contact_type <- "Curator"
nrow(curator)
gs_join <- full_join(plantrecords,curator)
nrow(gs_join)

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
nrow(gs_join2)
str(gs_join2)

# final edits

### !! GARDENS WITH NO CONTACT INFO !!
gs_join2[which(is.na(gs_join2$contact_email)),2]

# set up dummy row as first row, with longest inputs in each column and
#   your own email, to be sure none of the fields get trimmed during mail merge
gs_final <- gs_join2
instl <- gs_final[nchar(gs_final$Institution)==max(nchar(gs_final$Institution)),]$Institution[1]
namel <- gs_final[nchar(gs_final$contact_name)==max(nchar(gs_final$contact_name)),]$contact_name[1]
if("target_taxa" %in% colnames(ps_final)){
  taxal <- gs_final[nchar(gs_final$target_taxa)==max(nchar(gs_final$target_taxa)),]$target_taxa[1]
  dummy_row <- data.frame(Institution=instl, contact_name=namel, target_taxa=taxal, contact_email=my_email) 
} else {
  dummy_row <- data.frame(Institution=instl, contact_name=namel, contact_email=my_email) 
}
gs_final <- full_join(dummy_row,gs_final)
nrow(gs_final)

# add entry that sends to a colleague, so they can forward along
add_row <- data.frame(Institution="your institution", contact_name="Colleague",
  contact_email="my-email@org.org")
gs_final <- full_join(gs_final,add_row)

### *IF* YOU'RE NOT INCLUDING THE INSTITUTION NAME IN YOUR EMAIL MESSAGE:
#   remove full duplicates of contact name and email (so no one gets two exact 
#   copies of the email)
dup_test <- paste(gs_final$contact_name,gs_final$contact_email)
unique(gs_final[duplicated(dup_test) | duplicated(dup_test, fromLast=TRUE), ])
gs_final <- gs_final[!duplicated(dup_test), ]
nrow(gs_final)

# write file
write.xlsx(gs_final, file.path(main_dir,
  paste0("ExSituSurvey_BroaderEmail_Contacts_",Sys.Date(), ".xlsx")),
  col.names = T, row.names=F, append = F)
