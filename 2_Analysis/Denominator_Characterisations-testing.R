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


info(logger, "- Getting counts of mammography tests - with filter")

# ================== N IN DATABASE BEFORE AND AFTER LOCKDOWN ================= #

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


# ================== COUNTS OF ALL CANCER COVARIATES ========================= #

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
Counts_table  <- tibble(Covariate = "Visits in the healthcare system", "N Before Lockdown" = Visits_1, "N After Lockdown" = Visits_2)



# ============ TEST LOOP ===================================================== #
Counts_table  <- tibble(Covariate = "Screening_Mammographies", "N Before Lockdown" = Mammography_test[[1]], "N After Lockdown" = Mammography_test_2[[1]])

test_concepts <- c(4197459,4086282)
test_covariate_name <- c("Referral to Breast clinic", "Referral to mammography clinic")
test_acronym <- c("RBC","RMC")
i <- 1

tibble(concept_id=test_concepts, Covariate_name=test_covariate_names, acronymn=test_acronym)

test_loop <- for (each_concept in test_concepts) {
  
  value_from_loop_before <- cdm$procedure_occurrence %>%
    select(person_id, procedure_concept_id, procedure_date) %>%
    filter(procedure_concept_id == each_concept) %>% 
    filter(procedure_date >= "2017-01-01" & procedure_date <="2020-03-22") %>%
    collect() %>%
    tally() 
  
  value_from_loop_after <- cdm$procedure_occurrence %>%
    select(person_id, procedure_concept_id, procedure_date) %>%
    filter(procedure_concept_id == each_concept) %>% 
    filter(procedure_date >= "2020-03-22" & procedure_date <="2020-06-29") %>%
    collect() %>%
    tally() 
  
  # code to add additional rows
  Counts_table  <- rbind(Counts_table,c(test_covariate_name[i], value_from_loop_before[[1]], value_from_loop_after[[1]]))
  i<-i+1
}




## 2. Set up for creating a for loop to get all observations of interest --------------------

# CONCEPT ID'S OF INTEREST
observation_concepts <- c(4197459,4086282,44791272,4141840,4089031,4136626,44791283,4215705 )
observation_covariate_names <- c("Referral to Breast clinic", "Referral to mammography clinic", "Fasttrack referral for suspected breast cancer",
                                 "Referral to breast surgeon", "Seen in breast clinic","Seen by breast surgeon","Fast track referral for lung cancer","PSA monitoring")
observation_acronym <- c("RBC","RMC","FTRSBC","RBS","SBC","SBS","FTRLC","PSAM")

test_concepts <- c(4197459,4086282)
test_covariate_names <- c("Referral to Breast clinic", "Referral to mammography clinic")
test_acronym <- c("RBC","RMC")

test_loop <- for (concept in observation_concepts) {

    cdm$procedure_occurrence %>%
      select(person_id, procedure_concept_id, procedure_date) %>%
      filter(procedure_concept_id ==observation_concepts) %>% 
     filter(procedure_date >= "2017-01-01" & procedure_date <="2020-03-22") %>%
      collect() %>%
      tally() %>%
  print()

}

# create a table containing covariate IDs, names and acronyms
observation_covariate_info  <- tibble(concept_id=observation_concepts, Covariate_name=observation_covariate_names, acronymn=observation_acronym)

## 3. Set up for creating a loop to get all procedures of interest ----------------------
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

# create a table containing covariate IDs, names and acronyms
procedure_covariate_info  <- tibble(concept_id=procedure_concepts, Covariate_name=procedure_covariate_names, acronymn=procedure_acronym)


## 4. Set up for creating a for loop to get all measurements of interest --------------------

measurement_concepts <- c(36203740,36203750,44791543,37395561,4272032)
measurement_covariate_names <- c("Diagnostic mammogram and ultrasound-L","Diagnostic mammogram and ultrasound-R","Bowel cancer screening programme",
                                 "Quantitative faecal immunochemical tests","Prostate specific antigen measurement")
measurement_acronym <- c("DMSUL","DMSUR","BCSP","QFIT","PSA")

# create a table containing covariate IDs, names and acronyms
measurement_covariate_info  <- tibble(concept_id=measurement_concepts, Covariate_name=measurement_covariate_names, acronymn=measurement_acronym)




# Get counts of mammography tests before lockdown

Mammography_1 <- cdm$procedure_occurrence %>%
  select(person_id, procedure_concept_id, procedure_date) %>%
  filter(procedure_concept_id ==4077697) %>% 
  filter(procedure_date >= "2017-01-01" & procedure_date <="2020-03-22") %>%
    collect() %>%
    tally() %>%
    print()

# create a table to put this info in
  
Counts_table  <- tibble(Covariate = "Screening_Mammographies", "N Before Lockdown" = Mammography_test, "N After Lockdown" = Mammography_test_2)

# Get counts of mammography tests after lockdown

Mammography_test_2 <- cdm$procedure_occurrence %>%
  select(person_id, procedure_concept_id, procedure_date) %>%
  filter(procedure_concept_id ==4077697) %>% 
  filter(procedure_date >= "2020-03-23" & procedure_date <="2020-06-29") %>%
    distinct() %>%
    collect() %>%
    tally() %>%
    print()
  

 



# ================== ADD PROPORTION TO THE COUNTS TABLE ====================== #
Counts_table %>% 
mutate(n_before_lockdown = paste0(N before Lockdown, " (", round(100*n_before_lockdown/sum(n_before_lockdown),1), "%)")) %>%
  mutate(n_after_lockdown = paste0(N_after_lockdown, " (", round(100*n_after_lockdown/sum(n_after_lockdown),1), "%)")) 

# ========================= INDIVIDUAL TABLES================================= # 

print(paste0("- Getting breast cancer individual covariate tables"))
info(logger, "- Getting breast cancer individual covariate tables")


# =============================== (SMD) ====================================== #







print(paste0("- 2. BREAST CANCER CUSTOM CHARACTERISATIONS DONE"))
info(logger, "- 2. BREAST CANCER CUSTOM CHARACTERISATIONS DONE")