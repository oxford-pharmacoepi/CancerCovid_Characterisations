# ============================================================================ #
#                       Codelist generator to identify                         #
#                  concepts relevant to key cancer screening                   #
#                           and diagnostic tests                               #
#                              Nicola Barclay                                  #
#                                14-03-2023                                    #
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
#                          1. REFERRAL TO BREAST CLINIC                        #
# -----------------------------------------------------------------------------#

# INPUT YOUR KEY WORDS AND DOMAINS TO SEARCH (ONCE THIS HAS RUN YOU WILL SEE A 'PROPOSED_LIST' IN YOUR ENVIRONMENT. CLICK THIS AND 
# IT WILL SHOW ALL THE CONCEPTS. YOU CAN THEN FILTER THESE OUT IN DISCUSSION WITH A CLINICIAN/COLLEAGUES. THERE MAY BE SOME THAT YOU DO NOT WANT
# TO INCLUDE

Referral_breast_clinic <- getCandidateCodes(
  keywords = "referral to breast clinic",
  domains = "Observation",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  fuzzyMatch = TRUE,
  maxDistanceCost = 0.2,
  cdm = cdm
)

Referral_breast_clinic

write.csv(Referral_breast_clinic, file=here("Results", db.name, "CodelistGenerator", "Referral_breast_clinic.csv"))

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
#                9. Screening mammography                                      #
# -----------------------------------------------------------------------------#

Screening_mammogram <- getCandidateCodes(
  keywords = c("Screening mammography", "Screening mammogram"),
  domains = "Procedure",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  fuzzyMatch = TRUE,
  maxDistanceCost = 0.2,
  cdm = cdm
)

Screening_mammogram


write.csv(Screening_mammogram, file=here("Results", db.name, "CodelistGenerator", "Screening_mammogram.csv"))




# up to here




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

# -----------------------------------------------------------------------------#
#                               COLORECTAL CANCER                              #
# -----------------------------------------------------------------------------#


# INPUT YOUR KEY WORDS AND DOMAINS TO SEARCH (ONCE THIS HAS RUN YOU WILL SEE A 'PROPOSED_LIST' IN YOUR ENVIRONMENT. CLICK THIS AND 
# IT WILL SHOW ALL THE CONCEPTS. YOU CAN THEN FILTER THESE OUT IN DISCUSSION WITH A CLINICIAN. THERE MAY BE SOME THAT YOU DO NOT WANT
# TO INCLUDE

proposed_list <- getCandidateCodes(
  keywords = "colorectal cancer",
  domains = "Condition",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  cdm = cdm
)

# IN THIS LIST, IF YOU DO NOT WANT TO RETAIN CONCEPTS 1, 8 AND 12
# YOU CAN THEN SAVE THE CONCEPT SET (AND EXCLUDE THOSE THAT YOU DO NOT WANT IN THIS FOLLOWING CODE)

# Colorectal_cancer_with_metastases <- proposed_list[-c(),]

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

Colorectal_cancer_with_metastases <- getJsonFileFromConceptList(proposed_list, cdm)
writeChar(Colorectal_cancer_with_metastases, here::here("Concepts", "Colorectal_cancer_with_metastases_from_CodelistGenerator.json"))

# get the concept set expression to copy into ATLAS (or you can copy it from the file itself)
fileName <- here::here("Concepts", "Breast_cancer_with_metastases.json")
concept <- readChar(fileName, nchars = file.info(fileName)$size)
cat(concept)

up to here ##################################

# -----------------------------------------------------------------------------#
#                             LUNG CANCER                                      #
# -----------------------------------------------------------------------------#



# INPUT YOUR KEY WORDS AND DOMAINS TO SEARCH (ONCE THIS HAS RUN YOU WILL SEE A 'PROPOSED_LIST' IN YOUR ENVIRONMENT. CLICK THIS AND 
# IT WILL SHOW ALL THE CONCEPTS. YOU CAN THEN FILTER THESE OUT IN DISCUSSION WITH A CLINICIAN. THERE MAY BE SOME THAT YOU DO NOT WANT
# TO INCLUDE

proposed_list <- getCandidateCodes(
  keywords = "lung cancer",
  domains = "Condition",
  includeDescendants = TRUE,
  searchViaSynonyms = TRUE,
  searchInSynonyms = TRUE,
  cdm = cdm
)


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

Lung_cancer_with_metastases <- getJsonFileFromConceptList(proposed_list, cdm)
writeChar(Lung_cancer_with_metastases, here::here("Concepts", "Lung_cancer_with_metastases_from_CodelistGenerator.json"))

# get the concept set expression to copy into ATLAS (or you can copy it from the file itself)
fileName <- here::here("Concepts", "Lung_cancer_with_metastases.json")
concept <- readChar(fileName, nchars = file.info(fileName)$size)
cat(concept)
