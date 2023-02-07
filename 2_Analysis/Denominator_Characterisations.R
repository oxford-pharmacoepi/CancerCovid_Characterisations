# ============================================================================ #
#                         Custom Characterisations                             #
#                           for the denominator                                #
#                   NO COHORT DEFINED - JUST CDM REFERENCE                     #
#                              Nicola Barclay                                  #
#                                02-02-2023                                    #
# ============================================================================ #


start<-Sys.time()

# start log ----
log_file <- paste0(output.folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"


print(paste0("- 1. RUNNING DENOMINATOR CUSTOM CHARACTERISATIONS"))
info(logger, "- 1. RUNNING DENOMINATOR CUSTOM CHARACTERISATIONS")

# ================== N IN DATABASE BEFORE AND AFTER LOCKDOWN ================= #

print(paste0("- Getting counts of people observed in the database before and after lockdown"))
info(logger, "- Getting counts of people observed in the database before and after lockdown")

# how many people are observed in the database before lockdown
N_total_before_lockdown <-cdm$observation_period %>%
  select(person_id, observation_period_start_date, observation_period_end_date) %>%
  filter(observation_period_end_date >="2017-01-01" & observation_period_start_date <="2020-03-22") %>%
  distinct() %>%
  collect() %>%
  tally() %>%
  print()

# observed n the data after lockdown
N_total_after_lockdown <-cdm$observation_period %>%
  select(person_id, observation_period_start_date, observation_period_end_date) %>%
  filter(observation_period_end_date >="2020-03-23" & observation_period_start_date <="2020-06-29") %>%
  distinct() %>%
  collect() %>%
  tally() %>%
  print()


# save RData objects
save(N_total_before_lockdown, N_total_before_lockdown, file = here("Results", db.name, "Denominator", "Total_N.RData"))


print(paste0("- Got counts of people observed in the database before and after lockdown"))
info(logger, "- Got counts of people observed in the database before and after lockdown")

# ================== COUNTS OF ALL CANCER COVARIATES ========================= #

print(paste0("- Getting counts of visits in the healthcare system before and after lockdown"))
info(logger, "- Getting counts of visits in the healthcare system before and after lockdown")

## VISITS - STANDALONE  CODE
## 1. VISITS IN THE HEALTHCARE SYSTEM ------------------------------------------
Visits_1 <- cdm$visit_occurrence %>%
  select(person_id,visit_start_date) %>% # note that this does not require any filtering 
  # by concept_id because we want any visit occurring in the visit occurrence table
  filter(visit_start_date >= "2017-01-01" & visit_start_date <="2020-03-22") %>%
  collect() %>%
  tally() %>%
  print()

Visits_2 <- cdm$visit_occurrence %>%
  select(person_id,visit_start_date) %>% # note that this does not require any filtering 
  # by concept_id because we want any visit occurring in the visit occurrence table
  filter(visit_start_date >= "2020-03-23" & visit_start_date <="2020-06-29") %>%
  collect() %>%
  tally() %>%
  print()

# create a table to put this info in
Counts_table  <- tibble(Covariate = "Visits in the healthcare system", "N_Before_Lockdown" = Visits_1[[1]], "N_After_Lockdown" = Visits_2[[1]])

# save RData objects
save(Visits_1, Visits_2, file = here("Results", db.name, "Denominator", "Visits.RData"))

print(paste0("- Got counts of visits in the healthcare system before and after lockdown"))
info(logger, "- Got counts of visits in the healthcare system before and after lockdown")


# ========== TEST LOOP ======================================================= #

# CONCEPT ID'S OF INTEREST
test_concepts <- c(4077697,4047494)
test_covariate_names <- c("Screening mammography", "Biopsy of Breast")  
test_acronym <- c("SM", "BOB")


# Loop to get counts of all procedure concepts of interest before and after lockdown

i <- 1

test_loop <- for (each_concept in test_concepts) {
  
  value_from_test_before <- cdm$procedure_occurrence %>%
    select(person_id, procedure_concept_id, procedure_date) %>%
    filter(procedure_concept_id == each_concept) %>% 
    filter(procedure_date >= "2017-01-01" & procedure_date <="2020-03-22") %>%
    collect()  %>%
    tally() 
  
  value_from_test_after <- cdm$procedure_occurrence %>%
    select(person_id, procedure_concept_id, procedure_date) %>%
    filter(procedure_concept_id == each_concept) %>% 
    filter(procedure_date >= "2020-03-22" & procedure_date <="2020-06-29") %>%
    collect() %>%
    tally() 
  
  # code to add additional rows
  Counts_table_test  <- rbind(Counts_table,c(test_covariate_names[i], value_from_test_before[[1]], value_from_test_after[[1]]))
  i<-i+1
}

# save RData objects
save(value_from_test_before, value_from_test_after, file = here("Results", db.name, "Denominator", "Test.RData"))


## 2. === Set up for creating a for loop to get all observations of interest == #

print(paste0("- Getting counts of observations before and after lockdown"))
info(logger, "- Getting counts of observations before and after lockdown")

# CONCEPT ID'S OF INTEREST
observation_concepts <- c(4197459,4086282,44791272,4141840,4089031,4136626,44791283,4215705 )
observation_covariate_names <- c("Referral to Breast clinic", "Referral to mammography clinic", "Fasttrack referral for suspected breast cancer",
                                 "Referral to breast surgeon", "Seen in breast clinic","Seen by breast surgeon","Fast track referral for lung cancer","PSA monitoring")
observation_acronym <- c("RBC","RMC","FTRSBC","RBS","SBC","SBS","FTRLC","PSAM")


# Loop to get counts of all observation concepts of interest before and after lockdown

i <- 1

observation_loop <- for (each_concept in observation_concepts) {
  
  value_from_obs_before <- cdm$observation %>%
    select(person_id, observation_concept_id, observation_date) %>%
    filter(observation_concept_id == each_concept) %>% 
    filter(observation_date >= "2017-01-01" & observation_date <="2020-03-22") %>%
    collect() %>%
    tally() 
  
  value_from_obs_after <- cdm$observation %>%
    select(person_id, observation_concept_id, observation_date) %>%
    filter(observation_concept_id == each_concept) %>% 
    filter(observation_date >= "2020-03-22" & observation_date <="2020-06-29") %>%
    collect() %>%
    tally() 
  
 
  
  # code to add additional rows to the counts table
  Counts_table  <- rbind(Counts_table,c(observation_covariate_names[i], value_from_obs_before[[1]], value_from_obs_after[[1]]))
  i<-i+1
}

# save RData objects
save(value_from_obs_before, value_from_obs_after, file = here("Results", db.name, "Denominator", "Observations.RData"))

print(paste0("- Got counts of observations before and after lockdown"))
info(logger, "- Got counts of observations before and after lockdown")


## 3 ==== Set up for creating a loop to get all procedures of interest ======= #

print(paste0("- Getting counts of procedures before and after lockdown"))
info(logger, "- Getting counts of procedures before and after lockdown")

# CONCEPT ID'S OF INTEREST
procedure_concepts <- c(4077697,4324693,4047494,4022932,4028790,4306207,4216180,4146780,4129190,4194124,4286804,4082528,4249893,4087381,4125529,
                        4261497,2787168,4032404,44809038,4128302,4304406,4167553,45889178,4246485,4278515)
procedure_covariate_names <- c("Screening mammography", "Diagnostic mammograms", "Biopsy of breast", "Stereotactically guided core needle biopsy of breast",
                               "Percutaneous needle biopsy of breast","Fine needle aspiration of breast","Wire guided local excision of breast lump", 
                               "Excision of mammary duct","Wide local excision of breast lesion","Excision of lesion of breast","Excision of breast tissue",
                               "Ultrasonography of intestine","Colonoscopies","Sigmoidoscopy","Ultrasound of gastrointestinal tract",
                               "Ultrasonography of abdomen","Ultrasonography of rectum","Bronchoscopy","Endobronchial ultrasonography guided transbronchial needle aspiration",
                               "Mediastinoscopy","CT and biopsy of chest","US scan and biopsy of chest","Diagnostic Radiology (Diagnostic Imaging) Procedures of the Chest",
                               "MRI of chest","Biopsy of prostate")  
procedure_acronym <- c("SM","DM","BB","SGCNB","PNBB","FNAB","WGLEBL","EMD","WLEBL","ELB","EBT","UOI","COL","SIG","UGT","UOA","UOR","BRONCH","ENDO","MED","CTBC","USBC","DRC","MRI","BP")



# Loop to get counts of all procedure concepts of interest before and after lockdown

i <- 1

procedure_loop <- for (each_concept in procedure_concepts) {
  
  value_from_proc_before <- cdm$procedure_occurrence %>%
    select(person_id, procedure_concept_id, procedure_date) %>%
    filter(procedure_concept_id == each_concept) %>% 
    filter(procedure_date >= "2017-01-01" & procedure_date <="2020-03-22") %>%
    collect() %>%
    tally() 
  
  value_from_proc_after <- cdm$procedure_occurrence %>%
    select(person_id, procedure_concept_id, procedure_date) %>%
    filter(procedure_concept_id == each_concept) %>% 
    filter(procedure_date >= "2020-03-22" & procedure_date <="2020-06-29") %>%
    collect() %>%
    tally() 
  
  # code to add additional rows
  Counts_table  <- rbind(Counts_table,c(procedure_covariate_names[i], value_from_proc_before[[1]], value_from_proc_after[[1]]))
  i<-i+1
}

# save RData objects
save(value_from_proc_before, value_from_proc_after, file = here("Results", db.name, "Denominator", "Procedures.RData"))

print(paste0("- Got counts of procedures before and after lockdown"))
info(logger, "- Got counts of observations before and after lockdown")



## 4 ==== Set up for creating a for loop to get all measurements of interest == #

print(paste0("- Getting counts of measurements before and after lockdown"))
info(logger, "- Getting counts of measurements before and after lockdown")

# CONCEPT ID'S OF INTEREST
measurement_concepts <- c(36203740,36203750,44791543,37395561,4272032)
measurement_covariate_names <- c("Diagnostic mammogram and ultrasound-L","Diagnostic mammogram and ultrasound-R","Bowel cancer screening programme",
                                 "Quantitative faecal immunochemical tests","Prostate specific antigen measurement")
measurement_acronym <- c("DMSUL","DMSUR","BCSP","QFIT","PSA")


# Loop to get counts of all measurement concepts of interest before and after lockdown

i <- 1

measurement_loop <- for (each_concept in measurement_concepts) {
  
  value_from_meas_before <- cdm$measurement %>%
    select(person_id, measurement_concept_id, measurement_date) %>%
    filter(measurement_concept_id == each_concept) %>% 
    filter(measurement_date >= "2017-01-01" & measurement_date <="2020-03-22") %>%
    collect() %>%
    tally() 
  
  value_from_meas_after <- cdm$measurement %>%
    select(person_id, measurement_concept_id, measurement_date) %>%
    filter(measurement_concept_id == each_concept) %>% 
    filter(measurement_date >= "2020-03-22" & measurement_date <="2020-06-29") %>%
    collect() %>%
    tally() 
  
  # code to add additional rows
  Counts_table  <- rbind(Counts_table,c(measurement_covariate_names[i], value_from_meas_before[[1]], value_from_meas_after[[1]]))
  i<-i+1
}

# save RData objects
save(value_from_meas_before, value_from_meas_after, file = here("Results", db.name, "Denominator", "Measurements.RData"))

print(paste0("- Got counts of measurements before and after lockdown"))
info(logger, "- Got counts of measurements before and after lockdown")



# ================== ADD PROPORTION TO THE COUNTS TABLE ====================== #

print(paste0("- Add proportions to counts table"))
info(logger, "- Add proportions to counts table")

# note that the percentage comes out above 100% because this is calculated as the number of
#   visits (not distinct people) as a proprtion of the number of distinct people

Counts_with_percent <- Counts_table %>% 
  mutate(N_Before_Lockdown = paste0(N_Before_Lockdown, " (", round(100*N_Before_Lockdown/N_total_before_lockdown[[1]],1), "%)")) %>%
  mutate(N_After_Lockdown = paste0(N_After_Lockdown, " (", round(100*N_After_Lockdown/N_total_after_lockdown[[1]],1), "%)")) 


print(paste0("- Added proportions to counts table"))
info(logger, "- Added proportions to counts table")






print(paste0("- 1. DENOMINATOR CUSTOM CHARACTERISATIONS DONE"))
info(logger, "- 1. DENOMINATOR CUSTOM CHARACTERISATIONS DONE")


