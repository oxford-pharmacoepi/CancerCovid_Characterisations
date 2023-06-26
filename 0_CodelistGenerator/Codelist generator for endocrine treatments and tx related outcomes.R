# ============================================================================ #
#                       Codelist generator to identify                         #
#                  concepts relevant to endocrine treatments                   #
#                           and treatment related outcomes                     #
#                              Nicola Barclay                                  #
#                                01-06-2023                                    #
# ============================================================================ #



#install.packages("remotes")
#library(remotes)
# remotes::install_github("darwin-eu/CodelistGenerator")

# LOAD LIBRARIES

library(DBI)
library(dplyr)
library(dbplyr)
library(CodelistGenerator)
library(CDMConnector)
library(RPostgres)
library(here)

# CONNECT TO YOUR DATABASE (THE DBI WAY)

# example with postgres database connection details
server_dbi  <-  Sys.getenv("DB_SERVER_DBI_cdm_gold_202201")
server      <-  Sys.getenv("DB_SERVER_cdm_gold_202201")
user       <- Sys.getenv("DB_USER")
password   <- Sys.getenv("DB_PASSWORD")
port       <- Sys.getenv("DB_PORT")
host       <- Sys.getenv("DB_HOST")

db <- DBI::dbConnect(RPostgres::Postgres(),
                     dbname = server_dbi,
                     port = port,
                     host = host,
                     user = user,
                     password = password)

# CREATE YOUR CONNECTION TO THE DATABASE

cdm <- cdm_from_con(
  con = db,
  cdm_schema = "public",
  cdm_tables = tidyselect::all_of(c(
    "concept", "concept_relationship",
    "concept_ancestor",
    "concept_synonym",
    "vocabulary"
  ))
)


# -----------------------------------------------------------------------------#
#                          1. Aromatase inhibitors including    
#                          anastrozole, letrozole, exemestane
#
# -----------------------------------------------------------------------------#

# INPUT YOUR KEY WORDS AND DOMAINS TO SEARCH (ONCE THIS HAS RUN YOU WILL SEE A 'PROPOSED_LIST' IN YOUR ENVIRONMENT. CLICK THIS AND 
# IT WILL SHOW ALL THE CONCEPTS. YOU CAN THEN FILTER THESE OUT IN DISCUSSION WITH A CLINICIAN/COLLEAGUES. THERE MAY BE SOME THAT YOU DO NOT WANT
# TO INCLUDE

aromatase_inhibitors <- getCandidateCodes(
  keywords = "aromatase inhibitor",
  domains = "Drug",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  fuzzyMatch = TRUE,
  maxDistanceCost = 0.2,
  cdm = cdm
)

aromatase_inhibitors

write.csv(aromatase_inhibitors, file=here("Results", db.name, "CodelistGenerator", "aromatase_inhibitors.csv"))

# -----------------------------------------------------------------------------#
#                          2. Referral to mammography clinic                   #
# -----------------------------------------------------------------------------#

Referral_mamm_clinic <- getCandidateCodes(
  keywords = "referral to mammogra",
  domains = "Observation",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  fuzzyMatch = TRUE,
  maxDistanceCost = 0.2,
  cdm = cdm
)

Referral_mamm_clinic

write.csv(Referral_mamm_clinic, file=here("Results", db.name, "CodelistGenerator", "Referral_mamm_clinic.csv"))


# -----------------------------------------------------------------------------#
#             3. Fasttrack referral for suspected breast cancer                #
# -----------------------------------------------------------------------------#

Fast_track_breast <- getCandidateCodes(
  keywords = "Fasttrack referral for suspected breast cancer",
  domains = "Observation",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  fuzzyMatch = TRUE,
  maxDistanceCost = 0.2,
  cdm = cdm
)

Fast_track_breast

write.csv(Fast_track_breast, file=here("Results", db.name, "CodelistGenerator", "Fast_track_breast.csv"))


# -----------------------------------------------------------------------------#
#                        4. Referral to breast surgeon                         #
# -----------------------------------------------------------------------------#

Referral_breast_surgeon <- getCandidateCodes(
  keywords = "referral to breast surgeon",
  domains = "Observation",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  fuzzyMatch = TRUE,
  maxDistanceCost = 0.2,
  cdm = cdm
)

Referral_breast_surgeon

write.csv(Referral_breast_surgeon, file=here("Results", db.name, "CodelistGenerator", "Referral_breast_surgeon.csv"))



# -----------------------------------------------------------------------------#
#                        5. Seen in breast clinic                              #
# -----------------------------------------------------------------------------#

Seen_breast_clinic <- getCandidateCodes(
  keywords = "seen in breast clinic",
  domains = "Observation",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
 # fuzzyMatch = TRUE,
#  maxDistanceCost = 0.2,
  standardConcept = "Standard",
  cdm = cdm
)

Seen_breast_clinic

write.csv(Seen_breast_clinic, file=here("Results", db.name, "CodelistGenerator", "Seen_breast_clinic.csv"))



# -----------------------------------------------------------------------------#
#                        6. Seen by breast surgeon                             #
# -----------------------------------------------------------------------------#

Seen_breast_surgeon <- getCandidateCodes(
  keywords = "seen by breast surgeon",
  domains = "Observation",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
 # fuzzyMatch = TRUE,
#  maxDistanceCost = 0.2,
  cdm = cdm
)

Seen_breast_surgeon

write.csv(Seen_breast_surgeon, file=here("Results", db.name, "CodelistGenerator", "Seen_breast_surgeon.csv"))



# -----------------------------------------------------------------------------#
#                 7. Fast track referral for lung cancer                       #
# -----------------------------------------------------------------------------#

Fast_track_referral_lung <- getCandidateCodes(
  keywords = "Fast track referral for lung cancer",
  domains = "Observation",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

Fast_track_referral_lung

write.csv(Fast_track_referral_lung, file=here("Results", db.name, "CodelistGenerator", "Fast_track_referral_lung.csv"))



# -----------------------------------------------------------------------------#
#                8.  PSA monitoring                                            #
# -----------------------------------------------------------------------------#

PSA_mon <- getCandidateCodes(
  keywords = "prostate specific antigen monitoring",
  domains = "Observation",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

PSA_mon

write.csv(PSA_mon, file=here("Results", db.name, "CodelistGenerator", "PSA_mon.csv"))


# -----------------------------------------------------------------------------#
#                9a. Screening mammography - procedures                        #
# -----------------------------------------------------------------------------#

Screening_mammogram_proc <- getCandidateCodes(
  keywords = c("Screening mammography", "Screening mammogram"),
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  fuzzyMatch = TRUE,
  maxDistanceCost = 0.2,
  cdm = cdm
)

Screening_mammogram_proc


write.csv(Screening_mammogram_proc, file=here("Results", db.name, "CodelistGenerator", "Screening_mammogram_proc.csv"))


# -----------------------------------------------------------------------------#
#                9b. Screening mammography - measurements                      #
# -----------------------------------------------------------------------------#

Screening_mammogram_meas <- getCandidateCodes(
  keywords = c("Screening mammography", "Screening mammogram"),
  domains = "Measurement",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  fuzzyMatch = TRUE,
  maxDistanceCost = 0.2,
  cdm = cdm
)

Screening_mammogram_meas


write.csv(Screening_mammogram_meas, file=here("Results", db.name, "CodelistGenerator", "Screening_mammogram_meas.csv"))


# -----------------------------------------------------------------------------#
#                10. Diagnostic mammograms (mammography)                       #
# -----------------------------------------------------------------------------#

Diagnostic_mammogram <- getCandidateCodes(
  keywords = c("diagnostic mammography breast", "diagnostic mammogram breast"),
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  #fuzzyMatch = TRUE,
 # maxDistanceCost = 0.2,
  cdm = cdm
)

Diagnostic_mammogram


write.csv(Diagnostic_mammogram, file=here("Results", db.name, "CodelistGenerator", "Diagnostic_mammogram.csv"))

# up to here

# -----------------------------------------------------------------------------#
#                11. Biopsy of breast                                          #
# -----------------------------------------------------------------------------#

Biopsy_breast <- getCandidateCodes(
  keywords = c("breast biopsy", "biopsy of breast"),
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  #fuzzyMatch = TRUE,
  #maxDistanceCost = 0.2,
  cdm = cdm
)

Biopsy_breast


write.csv(Biopsy_breast, file=here("Results", db.name, "CodelistGenerator", "Biopsy_breast.csv"))


# -----------------------------------------------------------------------------#
#                12. Stereotactically guided core needle biopsy of breast      #
# -----------------------------------------------------------------------------#

Stereo_breast <- getCandidateCodes(
  keywords = "Stereotactically guided needle biopsy of breast",
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  fuzzyMatch = TRUE,
  maxDistanceCost = 0.2,
  cdm = cdm
)

Stereo_breast


write.csv(Stereo_breast, file=here("Results", db.name, "CodelistGenerator", "Stereo_breast.csv"))


# -----------------------------------------------------------------------------#
#                13. Percutaneous needle biopsy of breast                      #
# -----------------------------------------------------------------------------#

Percutaneous_breast <- getCandidateCodes(
  keywords = "Percutaneous needle biopsy of breast",
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  fuzzyMatch = TRUE,
  maxDistanceCost = 0.2,
  cdm = cdm
)

Percutaneous_breast


write.csv(Percutaneous_breast, file=here("Results", db.name, "CodelistGenerator", "Percutaneous_breast.csv"))



# -----------------------------------------------------------------------------#
#                14. Fine needle aspiration of breast                          #
# -----------------------------------------------------------------------------#

Fine_needle_breast <- getCandidateCodes(
  keywords = " Fine needle aspiration of breast",
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  #fuzzyMatch = TRUE,
 # maxDistanceCost = 0.2,
  cdm = cdm
)

Fine_needle_breast


write.csv(Fine_needle_breast, file=here("Results", db.name, "CodelistGenerator", "Fine_needle_breast.csv"))



# -----------------------------------------------------------------------------#
#                15. Wire guided local excision of breast lump                 #
# -----------------------------------------------------------------------------#

Wire_guided_excision_breast <- getCandidateCodes(
  keywords = "Wire guided local excision of breast lump",
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
 # fuzzyMatch = TRUE,
#  maxDistanceCost = 0.2,
  cdm = cdm
)

Wire_guided_excision_breast


write.csv(Wire_guided_excision_breast, file=here("Results", db.name, "CodelistGenerator", "Wire_guided_excision_breast.csv"))


# -----------------------------------------------------------------------------#
#                16. Excision of mammary duct                                  #
# -----------------------------------------------------------------------------#

Excision_mammary_duct <- getCandidateCodes(
  keywords = "Excision of mammary duct",
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

Excision_mammary_duct


write.csv(Excision_mammary_duct, file=here("Results", db.name, "CodelistGenerator", "Excision_mammary_duct.csv"))



# -----------------------------------------------------------------------------#
#                17. Wide local excision of breast lesion                      #
# -----------------------------------------------------------------------------#

Wide_local_ex_breast_lesion <- getCandidateCodes(
  keywords = "Wide local excision of breast lesion",
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

Wide_local_ex_breast_lesion


write.csv(Wide_local_ex_breast_lesion, file=here("Results", db.name, "CodelistGenerator", "Wide_local_ex_breast_lesion.csv"))



# -----------------------------------------------------------------------------#
#                19. Excision of breast tissue                                 #
# -----------------------------------------------------------------------------#

Excision_breast_tissue <- getCandidateCodes(
  keywords = "Excision of breast tissue",
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

Excision_breast_tissue


write.csv(Excision_breast_tissue, file=here("Results", db.name, "CodelistGenerator", "Excision_breast_tissue.csv"))




# -----------------------------------------------------------------------------#
#                20. Ultrasonography of intestine                                #
# -----------------------------------------------------------------------------#

Ultrason_intestine <- getCandidateCodes(
  keywords = "Ultrasonography of intestine",
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

Ultrason_intestine


write.csv(Ultrason_intestine, file=here("Results", db.name, "CodelistGenerator", "Ultrason_intestine.csv"))




# -----------------------------------------------------------------------------#
#                21. Colonoscopies                                             #
# -----------------------------------------------------------------------------#

Colonoscopies <- getCandidateCodes(
  keywords = c("Colonoscopies","Colonoscopy"),
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

Colonoscopies


write.csv(Colonoscopies, file=here("Results", db.name, "CodelistGenerator", "Colonoscopies.csv"))




# -----------------------------------------------------------------------------#
#                21. Colonoscopies                                             #
# -----------------------------------------------------------------------------#

Colonoscopies <- getCandidateCodes(
  keywords = c("Colonoscopies","Colonoscopy"),
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

Colonoscopies


write.csv(Colonoscopies, file=here("Results", db.name, "CodelistGenerator", "Colonoscopies.csv"))




# -----------------------------------------------------------------------------#
#                22. Sigmoidoscopy                                             #
# -----------------------------------------------------------------------------#

Sigmoidoscopy <- getCandidateCodes(
  keywords = "Sigmoidoscopy",
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

Sigmoidoscopy


write.csv(Sigmoidoscopy, file=here("Results", db.name, "CodelistGenerator", "Sigmoidoscopy.csv"))



# -----------------------------------------------------------------------------#
#                23. Ultrasound of gastrointestinal tract                      #
# -----------------------------------------------------------------------------#

US_Gastro <- getCandidateCodes(
  keywords = "Ultrasound of gastrointestinal tract",
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

US_Gastro


write.csv(US_Gastro, file=here("Results", db.name, "CodelistGenerator", "US_Gastro.csv"))




# -----------------------------------------------------------------------------#
#                24. Ultrasonography of abdomen                                #
# -----------------------------------------------------------------------------#

US_abdomen <- getCandidateCodes(
  keywords = "Ultrasonography of abdomen",
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

US_abdomen


write.csv(US_abdomen, file=here("Results", db.name, "CodelistGenerator", "US_abdomen.csv"))


# -----------------------------------------------------------------------------#
#                25. Ultrasonography of rectum                                 #
# -----------------------------------------------------------------------------#

US_rectum <- getCandidateCodes(
  keywords = "Ultrasonography of rectum",
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

US_rectum


write.csv(US_rectum, file=here("Results", db.name, "CodelistGenerator", "US_rectum.csv"))




# -----------------------------------------------------------------------------#
#                26. Bronchoscopy                                              #
# -----------------------------------------------------------------------------#

Bronchoscopy <- getCandidateCodes(
  keywords = "Bronchoscopy",
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

Bronchoscopy


write.csv(Bronchoscopy, file=here("Results", db.name, "CodelistGenerator", "Bronchoscopy.csv"))



# -----------------------------------------------------------------------------#
# 27. Endobronchial ultrasonography guided transbronchial needle aspiration    #
# -----------------------------------------------------------------------------#

Endo <- getCandidateCodes(
  keywords = "Endobronchial ultrasonography guided transbronchial needle aspiration",
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

Endo


write.csv(Endo, file=here("Results", db.name, "CodelistGenerator", "Endo.csv"))



# -----------------------------------------------------------------------------#
#               28. Mediastinoscopy                                            #
# -----------------------------------------------------------------------------#

Mediastinoscopy  <- getCandidateCodes(
  keywords = "Mediastinoscopy",
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

Mediastinoscopy 


write.csv(Mediastinoscopy , file=here("Results", db.name, "CodelistGenerator", "Mediastinoscopy.csv"))



# -----------------------------------------------------------------------------#
#               29. CT and biopsy of chest                                     #
# -----------------------------------------------------------------------------#

CTbiopsy_chest  <- getCandidateCodes(
  keywords = "CT and biopsy of chest",
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

CTbiopsy_chest 


write.csv(CTbiopsy_chest, file=here("Results", db.name, "CodelistGenerator", "CTbiopsy_chest.csv"))


# -----------------------------------------------------------------------------#
#               30. US scan and biopsy of chest                                #
# -----------------------------------------------------------------------------#

USbiopsy_chest  <- getCandidateCodes(
  keywords = "US scan and biopsy of chest",
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

USbiopsy_chest 


write.csv(USbiopsy_chest, file=here("Results", db.name, "CodelistGenerator", "USbiopsy_chest.csv"))



# -----------------------------------------------------------------------------#
#   31. Diagnostic Radiology (Diagnostic Imaging) Procedures of the Chest      #
# -----------------------------------------------------------------------------#

Radio_chest  <- getCandidateCodes(
  keywords = "Diagnostic Radiology Procedures of the Chest",
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

Radio_chest 


write.csv(Radio_chest, file=here("Results", db.name, "CodelistGenerator", "Radio_chest.csv"))


# -----------------------------------------------------------------------------#
#                          32. MRI of chest                                    #
# -----------------------------------------------------------------------------#

MRI_chest  <- getCandidateCodes(
  keywords = "MRI of chest",
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

MRI_chest 


write.csv(MRI_chest, file=here("Results", db.name, "CodelistGenerator", "MRI_chest.csv"))


# -----------------------------------------------------------------------------#
#                          33. Biopsy of prostate                              #
# -----------------------------------------------------------------------------#

Biopsy_prostate  <- getCandidateCodes(
  keywords = "Biopsy of prostate",
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

Biopsy_prostate 


write.csv(Biopsy_prostate, file=here("Results", db.name, "CodelistGenerator", "Biopsy_prostate.csv"))


# -----------------------------------------------------------------------------#
#                      34. 35. Diagnostic mammogram and ultrasound-L           #
# -----------------------------------------------------------------------------#

Diagnostic_mamm_US  <- getCandidateCodes(
  keywords = "Diagnostic mammogram and ultrasound",
  domains = "Measurement",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

Diagnostic_mamm_US 


write.csv(Diagnostic_mamm_US, file=here("Results", db.name, "CodelistGenerator", "Diagnostic_mamm_US.csv"))


# -----------------------------------------------------------------------------#
#                      36. Bowel cancer screening programme                    #
# -----------------------------------------------------------------------------#

Bowel_screening <- getCandidateCodes(
  keywords = c("Bowel cancer screening","colorectal screening"),
  domains = "Measurement",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

Bowel_screening 


write.csv(Bowel_screening, file=here("Results", db.name, "CodelistGenerator", "Bowel_screening.csv"))



# -----------------------------------------------------------------------------#
#             37. Quantitative faecal immunochemical tests                     #
# -----------------------------------------------------------------------------#

QFIT <- getCandidateCodes(
  keywords = "faecal immunochemical tests",
  domains = "Measurement",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

QFIT 


write.csv(QFIT, file=here("Results", db.name, "CodelistGenerator", "QFIT.csv"))


# -----------------------------------------------------------------------------#
#             38. Prostate specific antigen measurement                        #
# -----------------------------------------------------------------------------#

PSA <- getCandidateCodes(
  keywords = "Prostate specific antigen",
  domains = "Measurement",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  # fuzzyMatch = TRUE,
  #  maxDistanceCost = 0.2,
  cdm = cdm
)

PSA 


write.csv(PSA, file=here("Results", db.name, "CodelistGenerator", "PSA.csv"))


# IN THIS LIST, IF YOU DO NOT WANT TO RETAIN CONCEPTS 1, 8 AND 12
# YOU CAN THEN SAVE THE CONCEPT SET (AND EXCLUDE THOSE THAT YOU DO NOT WANT IN THIS FOLLOWING CODE)

Breast_cancer_with_metastases <- proposed_list[-c(1,3,4,5,6,8,10,11,12,13,14,15,16,22,23,24,25,27,34,55,69,94,100,102,103,104,105,106,127,159,165,166),]

# Then create an exportable json of this concept set and save it, but first need to create a function to get the concept set and save as a json
# THIS IS THE FUNCTION

getJsonFileFromConceptList <- function(x,cdm){
  
  x <- cdm$concept %>%
    dplyr::semi_join(x, by = "concept_id", copy = TRUE) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      invalid_reason_caption = dplyr::if_else(is.na(.data$invalid_reason),
                                              "Valid", "Invalid"
      )
    ) %>%
    dplyr::mutate(invalid_reason = dplyr::if_else(
      is.na(.data$invalid_reason), "V", .data$invalid_reason
    )) %>%
    dplyr::mutate(standard_concept_caption = dplyr::if_else(
      is.na(.data$standard_concept), "Non-Standard", dplyr::if_else(
        .data$standard_concept == "C", "Classification", "Standard"
      )
    )) %>%
    dplyr::mutate(standard_concept = dplyr::if_else(
      is.na(.data$standard_concept), "N",
      .data$standard_concept
    ))
  emptyConcept <-'{
    "concept": {
      "CONCEPT_CLASS_ID": "#concept_class_id",
      "CONCEPT_CODE": "#concept_code",
      "CONCEPT_ID": #concept_id,
      "CONCEPT_NAME": "#concept_name",
      "DOMAIN_ID": "#domain_id",
      "INVALID_REASON": "#invalid_reason",
      "INVALID_REASON_CAPTION": "#invalid_reason_caption",
      "STANDARD_CONCEPT": "#standard_concept",
      "STANDARD_CONCEPT_CAPTION": "#standard_concept_caption",
      "VOCABULARY_ID": "#vocabulary_id",
      "VALID_START_DATE": "#valid_start_date",
      "VALID_END_DATE": "valid_end_date"
    },
    "isExcluded": false,
    "includeDescendants": false,
    "includeMapped": false
  }'
  
  for (k in 1:nrow(x)){
    concept <- stringr::str_replace(emptyConcept,"#concept_class_id",x$concept_class_id[k])
    concept <- stringr::str_replace(concept,"#concept_code",x$concept_code[k])
    concept <- stringr::str_replace(concept,"#concept_id",as.character(x$concept_id[k]))
    concept <- stringr::str_replace(concept,"#concept_name",x$concept_name[k])
    concept <- stringr::str_replace(concept,"#domain_id",x$domain_id[k])
    concept <- stringr::str_replace(concept,"#invalid_reason",x$invalid_reason[k])
    concept <- stringr::str_replace(concept,"#invalid_reason_caption",x$invalid_reason_caption[k])
    concept <- stringr::str_replace(concept,"#standard_concept",x$standard_concept[k])
    concept <- stringr::str_replace(concept,"#standard_concept_caption",x$standard_concept_caption[k])
    concept <- stringr::str_replace(concept,"#vocabulary_id",x$vocabulary_id[k])
    concept <- stringr::str_replace(concept,"#valid_start_date",as.character(x$valid_start_date[k]))
    concept <- stringr::str_replace(concept,"#valid_end_date",as.character(x$valid_end_date[k]))
    if (k == 1){
      concept_all <- concept
    } else {
      concept_all <- paste0(concept_all, ",\n", concept)
    }
  }
  
  jsonFile <- paste0('{
  "items": [', concept_all, '  ]
}')
  
  return(jsonFile)
  
}


# USE THIS FUNCTION TO SAVE THE CONCEPT SET as a json file. You can then import this into ATLAS by copying the json
# into a new concept set and click 'append concept set expression'

Breast_cancer_with_metastases <- getJsonFileFromConceptList(Breast_cancer_with_metastases, cdm)
writeChar(Breast_cancer_with_metastases, here::here("Concepts", "Breast_cancer_with_metastases.json"))

# get the concept set expression to copy into ATLAS (or you can copy it from the file itself)
fileName <- here::here("Concepts", "Breast_cancer_with_metastases.json")
concept <- readChar(fileName, nchars = file.info(fileName)$size)
cat(concept)
