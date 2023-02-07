# output files ----
if (!file.exists(output.folder)){
  dir.create(output.folder, recursive = TRUE)}

if (!file.exists(output.folder1)){
  dir.create(output.folder1, recursive = TRUE)}

if (!file.exists(output.folder2)){
  dir.create(output.folder2, recursive = TRUE)}

if (!file.exists(output.folder3)){
  dir.create(output.folder3, recursive = TRUE)}

if (!file.exists(output.folder4)){
  dir.create(output.folder4, recursive = TRUE)}


# table names----
outcome_table_name_1 <- paste0(outcome_table_stem,"_cancers_before_after_lockdown") # this is the four cancers

start<-Sys.time()

# start log ----
log_file <- paste0(output.folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# instantiate study cohorts ----
info(logger, 'INSTANTIATING STUDY COHORTS')
source(here("1_InstantiateCohorts","InstantiateStudyCohorts.R"))
info(logger, 'GOT STUDY COHORTS')


# Run functions ----
info(logger, 'RUNNING FUNCTIONS FOR ALL CUSTOM CHARACTERISATIONS')
source(here("2_Analysis","forAllCharacterisations_functions.R"))
info(logger, 'FUNCTIONS FOR ALL CUSTOM CHARACTERISATIONS RAN')

# Run custom characterisations on denominator population ----
info(logger, 'RUNNING CUSTOM CHARACTERISATIONS OF DENOMINATOR POPULATION')
source(here("2_Analysis","DenominatorCharacterisations.R"))
info(logger, 'CUSTOM CHARACTERISATIONS OF DENOMINATOR POPULATION RAN')

# Run custom characterisations on breast cancer population ----
info(logger, 'RUNNING CUSTOM CHARACTERISATIONS OF BREAST CANCER POPULATION')
source(here("2_Analysis","BreastCharacterisations.R"))
info(logger, 'CUSTOM CHARACTERISATIONS OF BREAST CANCER POPULATION RAN')

# Run custom characterisations on colorectal cancer population ----
info(logger, 'RUNNING CUSTOM CHARACTERISATIONS OF COLORECTAL CANCER POPULATION')
source(here("2_Analysis","ColorectalCharacterisations.R"))
info(logger, 'CUSTOM CHARACTERISATIONS OF COLORECTAL CANCER POPULATION RAN')

# Run custom characterisations on lung cancer population ----
info(logger, 'RUNNING CUSTOM CHARACTERISATIONS OF LUNG CANCER POPULATION')
source(here("2_Analysis","LungCharacterisations.R"))
info(logger, 'CUSTOM CHARACTERISATIONS OF LUNG CANCER POPULATION RAN')

# Run custom characterisations on prostate cancer population ----
info(logger, 'RUNNING CUSTOM CHARACTERISATIONS OF PROSTATE CANCER POPULATION')
source(here("2_Analysis","ProstateCharacterisations.R"))
info(logger, 'CUSTOM CHARACTERISATIONS OF PROSTATE CANCER POPULATION RAN')

# Run feature extraction package on all cancer populations ----
info(logger, 'RUNNING FEATURE EXTRACTION OF ALL CANCERS')
source(here("2_Analysis","FeatureExtraction.R"))
info(logger, 'FEATURE EXTRACTION OF ALL CANCERS RAN')


print("Done!")
print("-- If all has worked, there should now be .csv files, data objects and 
      tables in the corresponding results folders for each popualtion to share")
print("-- Thank you for running the study!")
Sys.time()-start
readLines(log_file)