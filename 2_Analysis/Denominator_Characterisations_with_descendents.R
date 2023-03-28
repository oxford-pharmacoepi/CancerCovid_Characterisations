# ============================================================================ #
#                         Custom Characterisations                             #
#                           for the denominator                                #
#                   NO COHORT DEFINED - JUST CDM REFERENCE                     #
#               with code to obtain all descendant concepts                    #
#            splitting counts pre-covid, lockdown and post-covid               #
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

# ================== N IN DATABASE before, during and after LOCKDOWN ================= #

print(paste0("- Getting counts of people observed in the database before, during and after lockdown"))
info(logger, "- Getting counts of people observed in the database before, during and after lockdown")

# how many people are observed in the database before lockdown
N_total_before_lockdown <-cdm$observation_period %>%
  select(person_id, observation_period_start_date, observation_period_end_date) %>%
  filter(observation_period_end_date >="2017-01-01" & observation_period_start_date <="2020-03-22") %>%
  distinct() %>%
  collect() %>%
  tally() %>%
  print()


# observed n the data during lockdown
N_total_during_lockdown <-cdm$observation_period %>%
  select(person_id, observation_period_start_date, observation_period_end_date) %>%
  filter(observation_period_end_date >="2020-03-23" & observation_period_start_date <="2020-07-03") %>%
  distinct() %>%
  collect() %>%
  tally() %>%
  print()

# observed n the data after lockdown
N_total_after_lockdown <-cdm$observation_period %>%
  select(person_id, observation_period_start_date, observation_period_end_date) %>%
  filter(observation_period_end_date >="2020-07-04" & observation_period_start_date <="2022-01-01") %>%
  distinct() %>%
  collect() %>%
  tally() %>%
  print()


# save RData objects
save(N_total_before_lockdown, N_total_during_lockdown, N_total_before_lockdown, file = here("Results", db.name, "Denominator", "Total_N.RData"))


print(paste0("- Got counts of people observed in the database before, during and after lockdown"))
info(logger, "- Got counts of people observed in the database before, during and after lockdown")

# ================== COUNTS OF ALL CANCER COVARIATES ========================= #

print(paste0("- Getting counts of visits in the healthcare system before, during and after lockdown"))
info(logger, "- Getting counts of visits in the healthcare system before, during and after lockdown")

## VISITS - STANDALONE  CODE
## 1. VISITS IN THE HEALTHCARE SYSTEM ------------------------------------------
Visits_1 <- cdm$visit_occurrence %>%
  select(person_id,visit_start_date) %>% # note that this does not require any filtering 
  # by concept_id because we want any visit occurring in the visit occurrence table
  filter(visit_start_date >= "2017-01-01" & visit_start_date <="2020-03-22") %>%
  tally() %>%
  collect() %>%
  print()

Visits_2 <- cdm$visit_occurrence %>%
  select(person_id,visit_start_date) %>% # note that this does not require any filtering 
  # by concept_id because we want any visit occurring in the visit occurrence table
  filter(visit_start_date >= "2020-03-23" & visit_start_date <="2020-07-03") %>%
  tally() %>%
  collect() %>%
  print()

Visits_3 <- cdm$visit_occurrence %>%
  select(person_id,visit_start_date) %>% # note that this does not require any filtering 
  # by concept_id because we want any visit occurring in the visit occurrence table
  filter(visit_start_date >= "2020-07-04" & visit_start_date <="2022-01-01") %>%
  tally() %>%
  collect() %>%
  print()

# create a table to put this info in
Counts_table  <- tibble(Covariate = "Visits in the healthcare system", "N_Before_Lockdown" = Visits_1[[1]], "N_During_Lockdown" = Visits_2[[1]], "N_After_Lockdown" = Visits_3[[1]])
# only run the following because i didn't collect the results above. this can be removed in the final version.
Counts_table  <- tibble(Covariate = "Visits in the healthcare system", "N_Before_Lockdown" = "184471700", "N_During_Lockdown" = "12108287", "N_After_Lockdown" = "74106599")

# save RData objects
save(Visits_1, Visits_2, Visits_3, file = here("Results", db.name, "Denominator", "Visits.RData"))

print(paste0("- Got counts of visits in the healthcare system before, during and after lockdown"))
info(logger, "- Got counts of visits in the healthcare system before, during and after lockdown")



## 2. === Set up for creating a for loop to get all observations of interest == #

print(paste0("- Getting counts of observations before, during and after lockdown"))
info(logger, "- Getting counts of observations before, during and after lockdown")


## FIRST CREATE TEMPORARY TABLES WITH FILTERING OF THE DATES IN THE OBSERVATION TABLE
# create temp table with the observation table already filtered by date so only have to do this once, then remove date filter from loops

cdm$observation_temp_pre <- cdm$observation %>% filter(observation_date >= "2017-01-01" & observation_date <="2020-03-22") %>% compute()
cdm$observation_temp_lock <- cdm$observation %>% filter(observation_date >= "2020-03-23" & observation_date <="2020-07-03") %>% compute()
cdm$observation_temp_post <- cdm$observation %>% filter(observation_date >= "2020-07-04" & observation_date <="2022-01-01") %>% compute()


# CONCEPT ID'S OF INTEREST
observation_concepts <- c(4197459,4086282,44791272,4141840,4089031,4136626,44791283,4215705,44791274 )
observation_covariate_names <- c("Referral to Breast clinic", "Referral to mammography clinic", "Fasttrack referral for suspected breast cancer",
                                 "Referral to breast surgeon", "Seen in breast clinic","Seen by breast surgeon","Fast track referral for suspected lung cancer","PSA monitoring","Fast track referral for suspected colorectal cancer")
observation_acronym <- c("RBC","RMC","FTRSBC","RBS","SBC","SBS","FTRLC","PSAM","FTRSCC")


############################## TEST #############################
# test for referral to breast clinic code with descendants - both these versions work
#referral_breast_clinic_desc_before <-  cdm$observation_temp_pre %>%
#    select(person_id, observation_concept_id, observation_date) %>%
#    inner_join(cdm$concept_ancestor %>%
#                 select("ancestor_concept_id","descendant_concept_id") %>%
#                 rename("observation_concept_id" = "descendant_concept_id")) %>%
#  filter(observation_concept_id == 4197459) %>%  
#  filter(observation_date >= "2017-01-01" & observation_date <="2020-03-22") %>%
#    tally() 

# or
#referral_breast_clinic_desc_before <-  cdm$observation_temp_pre %>%
#  select(person_id, observation_concept_id, observation_date) %>%
#  inner_join(cdm$concept_ancestor %>%
#               select("ancestor_concept_id","descendant_concept_id") %>%
#               filter(ancestor_concept_id == 4197459) %>%
#               select("observation_concept_id" = "descendant_concept_id")) %>%
#  filter(observation_date >= "2017-01-01" & observation_date <="2020-03-22") %>%
#  tally() 
  
################################################################################
  


# Loop to get counts of all observation concepts of interest before, during and after lockdown

i <- 1

observation_loop <- for (each_concept in observation_concepts) {
  
  value_from_obs_before <- cdm$observation_temp_pre %>%
    select(person_id, observation_concept_id, observation_date) %>%
    inner_join(cdm$concept_ancestor %>%
                 select("ancestor_concept_id","descendant_concept_id") %>%
                 filter(ancestor_concept_id == each_concept) %>%
                 select("observation_concept_id" = "descendant_concept_id")) %>%
    collect() %>%
    tally() 
  
  
  value_from_obs_during <- cdm$observation_temp_lock %>%
    select(person_id, observation_concept_id, observation_date) %>%
    inner_join(cdm$concept_ancestor %>%
                 select("ancestor_concept_id","descendant_concept_id") %>%
                 filter(ancestor_concept_id == each_concept) %>%
                 select("observation_concept_id" = "descendant_concept_id")) %>%
    collect() %>%
    tally() 
  
  value_from_obs_after <- cdm$observation_temp_post %>%
    select(person_id, observation_concept_id, observation_date) %>%
    inner_join(cdm$concept_ancestor %>%
                 select("ancestor_concept_id","descendant_concept_id") %>%
                 filter(ancestor_concept_id == each_concept) %>%
                 select("observation_concept_id" = "descendant_concept_id")) %>%
    collect() %>%
    tally() 
 
  
  # code to add additional rows to the counts table
  Counts_table  <- rbind(Counts_table,c(observation_covariate_names[i], value_from_obs_before[[1]], value_from_obs_during[[1]], value_from_obs_after[[1]]))
  i<-i+1
}

# save RData objects
save(value_from_obs_before, value_from_obs_during, value_from_obs_after, file = here("Results", db.name, "Denominator", "Observations.RData"))
rm(i)


print(paste0("- Got counts of observations before, during and after lockdown"))
info(logger, "- Got counts of observations before, during and after lockdown")


## 3 ==== Set up for creating a loop to get all procedures of interest ======= #

print(paste0("- Getting counts of procedures before, during and after lockdown"))
info(logger, "- Getting counts of procedures before, during and after lockdown")


## FIRST CREATE TEMPORARY TABLES WITH FILTERING OF THE DATES IN THE PROCEDURE TABLE
# create temp table with the procedure table already filtered by date so only have to do this once, then remove date filter from loops

cdm$procedure_occurrence_temp_pre <- cdm$procedure_occurrence %>% filter(procedure_date >= "2017-01-01" & procedure_date <="2020-03-22") %>% compute()
cdm$procedure_occurrence_temp_lock <- cdm$procedure_occurrence %>% filter(procedure_date >= "2020-03-23" & procedure_date <="2020-07-03") %>% compute()
cdm$procedure_occurrence_temp_post <- cdm$procedure_occurrence %>% filter(procedure_date >= "2020-07-04" & procedure_date <="2022-01-01") %>% compute()

# CONCEPT ID'S OF INTEREST
procedure_concepts <- c(4324693,4047494,4022932,4028790,4306207,4216180,4146780,4129190,4194124,4286804,4082528,4061134,4249893,4087381,4125529,
                        4261497,2787168,4032404,44809038,4128302,4070986,4304406,4167553,45889178,4246485,4278515,4073010)
procedure_covariate_names <- c("Diagnostic mammograms", "Biopsy of breast", "Stereotactically guided core needle biopsy of breast",
                               "Percutaneous needle biopsy of breast","Fine needle aspiration of breast","Wire guided local excision of breast lump", 
                               "Excision of mammary duct","Wide local excision of breast lesion","Excision of lesion of breast","Excision of breast tissue",
                               "Ultrasonography of intestine","Endoscopic ultrasound of upper gastrointestinal tract","Colonoscopies","Sigmoidoscopy","Ultrasound of gastrointestinal tract",
                               "Ultrasonography of abdomen","Ultrasonography of rectum","Bronchoscopy","Endobronchial ultrasonography guided transbronchial needle aspiration",
                               "Mediastinoscopy"," Mediastinoscopy - inspection only","CT and biopsy of chest","US scan and biopsy of chest","Diagnostic Radiology (Diagnostic Imaging) Procedures of the Chest",
                               "MRI of chest","Biopsy of prostate","Open biopsy of prostate")  
procedure_acronym <- c("DM","BB","SGCNB","PNBB","FNAB","WGLEBL","EMD","WLEBL","ELB","EBT","UOI","ENDOGAS","COL","SIG","UGT","UOA","UOR","BRONCH","ENDO","MED","MED-IN","CTBC","USBC","DRC","MRI","BP","OBP")


# Loop to get counts of all procedure concepts of interest before, during and after lockdown

i <- 1

procedure_loop <- for (each_concept in procedure_concepts) {
  

  value_from_proc_before <- cdm$procedure_occurrence_temp_pre %>%
    select(person_id, procedure_concept_id, procedure_date) %>%
    inner_join(cdm$concept_ancestor %>%
                 select("ancestor_concept_id","descendant_concept_id") %>%
                 filter(ancestor_concept_id == each_concept) %>%
                 select("procedure_concept_id" = "descendant_concept_id")) %>%
    collect() %>%
    tally() 
  
  
  value_from_proc_during <- cdm$procedure_occurrence_temp_lock %>%
    select(person_id, procedure_concept_id, procedure_date) %>%
    inner_join(cdm$concept_ancestor %>%
                 select("ancestor_concept_id","descendant_concept_id") %>%
                 filter(ancestor_concept_id == each_concept) %>%
                 select("procedure_concept_id" = "descendant_concept_id")) %>%
    collect() %>%
    tally() 
  
  value_from_proc_after <- cdm$procedure_occurrence_temp_post %>%
    select(person_id, procedure_concept_id, procedure_date) %>%
    inner_join(cdm$concept_ancestor %>%
                 select("ancestor_concept_id","descendant_concept_id") %>%
                 filter(ancestor_concept_id == each_concept) %>%
                 select("procedure_concept_id" = "descendant_concept_id")) %>%
    collect() %>%
    tally() 
  
  # code to add additional rows
  Counts_table  <- rbind(Counts_table,c(procedure_covariate_names[i], value_from_proc_before[[1]], value_from_proc_during[[1]], value_from_proc_after[[1]]))
  i<-i+1
}

# save RData objects
save(value_from_proc_before, value_from_proc_during, value_from_proc_after, file = here("Results", db.name, "Denominator", "Procedures.RData"))
rm(i)




print(paste0("- Got counts of procedures before, during and after lockdown"))
info(logger, "- Got counts of procedures before, during and after lockdown")



## 4 ==== Set up for creating a for loop to get all measurements of interest == #

print(paste0("- Getting counts of measurements before, during and after lockdown"))
info(logger, "- Getting counts of measurements before, during and after lockdown")

## FIRST CREATE TEMPORARY TABLES WITH FILTERING OF THE DATES IN THE MEASUREMENT TABLE
# create temp table with the measurement table already filtered by date so only have to do this once, then remove date filter from loops

cdm$measurement_temp_pre <- cdm$measurement %>% filter(measurement_date >= "2017-01-01" & measurement_date <="2020-03-22") %>% compute()
cdm$measurement_temp_lock <- cdm$measurement %>% filter(measurement_date >= "2020-03-23" & measurement_date <="2020-07-03") %>% compute()
cdm$measurement_temp_post <- cdm$measurement %>% filter(measurement_date >= "2020-07-04" & measurement_date <="2022-01-01") %>% compute()

# CONCEPT ID'S OF INTEREST
measurement_concepts <- c(4077697,36203740,36203750,44791543,37395561,1397752,40480729,4272032,37398806)
measurement_covariate_names <- c("Screening mammography", "Diagnostic mammogram and ultrasound-L","Diagnostic mammogram and ultrasound-R","Bowel cancer screening programme",
                                 "Quantitative faecal immunochemical tests","Quantitative faecal immunochemical test-READ","Screening colonoscopy","Prostate specific antigen measurement","PSA (prostate-specific antigen) level measurement")
measurement_acronym <- c("SM","DMSUL","DMSUR","BCSP","QFIT","QFIT-READ","SC","PSA","PSA-Level")


# Loop to get counts of all measurement concepts of interest before, during and after lockdown

i <- 1

measurement_loop <- for (each_concept in measurement_concepts) {
  
    value_from_meas_before <- cdm$measurement_temp_pre %>%
    select(person_id, measurement_concept_id, measurement_date) %>%
    inner_join(cdm$concept_ancestor %>%
                 select("ancestor_concept_id","descendant_concept_id") %>%
                 filter(ancestor_concept_id == each_concept) %>%
                 select("measurement_concept_id" = "descendant_concept_id")) %>%
    collect() %>%
    tally() 
  
    value_from_meas_during <- cdm$measurement_temp_lock %>%
      select(person_id, measurement_concept_id, measurement_date) %>%
      inner_join(cdm$concept_ancestor %>%
                   select("ancestor_concept_id","descendant_concept_id") %>%
                   filter(ancestor_concept_id == each_concept) %>%
                   select("measurement_concept_id" = "descendant_concept_id")) %>%
      collect() %>%
      tally() 
    
    
    value_from_meas_after <- cdm$measurement_temp_post %>%
      select(person_id, measurement_concept_id, measurement_date) %>%
      inner_join(cdm$concept_ancestor %>%
                   select("ancestor_concept_id","descendant_concept_id") %>%
                   filter(ancestor_concept_id == each_concept) %>%
                   select("measurement_concept_id" = "descendant_concept_id")) %>%
      collect() %>%
      tally() 
    
  # code to add additional rows
  Counts_table  <- rbind(Counts_table,c(measurement_covariate_names[i], value_from_meas_before[[1]], value_from_meas_during[[1]], value_from_meas_after[[1]]))
  i<-i+1
}

# save RData objects
save(value_from_meas_before, value_from_meas_during, value_from_meas_after, file = here("Results", db.name, "Denominator", "Measurements.RData"))
rm(i)

print(paste0("- Got counts of measurements before, during and after lockdown"))
info(logger, "- Got counts of measurements before, during and after lockdown")


# = REORDER THE ROWS VARIABLES IN THE COUNTS TABLE SO THEY ARE CANCER SPECIFIC #

Counts_table_reorder <- Counts_table[c(1, 2, 3, 4, 5, 6, 7, 11, 38, 39, 40, 12, 13, 14, 15, 16, 17, 18, 19, 20, 
                                       10, 41, 42, 43, 44, 23,
                                       21, 22, 25, 26, 27, 8, 24, 28, 29, 30, 31, 32, 33, 34, 35, 
                                       9, 45, 46, 36, 37),]

# ================== RENAME COLUMNS IN COUNTS TABLE ========================== #

Counts_table_final <- Counts_table_reorder %>% rename("Screening/Diagnostic Test" = "Covariate",
                                                      "N Pre-COVID (Jan 2017-Feb 2020)" = "N_Before_Lockdown",
                                                      "N During Lockdown (March 2020-June 2020)" = "N_During_Lockdown",
                                                      "Post-lockdown (July 2020-Dec 2021)" = "N_After_Lockdown")

# ================== ADD PROPORTION TO THE COUNTS TABLE ====================== #

print(paste0("- Add proportions to counts table"))
info(logger, "- Add proportions to counts table")

# note that the percentage comes out above 100% because this is calculated as the number of
#   visits (not distinct people) as a proprtion of the number of distinct people

Counts_with_percent <- Counts_table %>% 
  mutate(N_Before_Lockdown = paste0(N_Before_Lockdown, " (", round(100*N_Before_Lockdown/N_total_before_lockdown[[1]],1), "%)")) %>%
  mutate(N_During_Lockdown = paste0(N_During_Lockdown, " (", round(100*N_During_Lockdown/N_total_during_lockdown[[1]],1), "%)")) %>%
  mutate(N_After_Lockdown = paste0(N_After_Lockdown, " (", round(100*N_After_Lockdown/N_total_after_lockdown[[1]],1), "%)")) 


print(paste0("- Added proportions to counts table"))
info(logger, "- Added proportions to counts table")


# save RData objects
save(Counts_table, file = here("Results", db.name, "Denominator", "Counts_table.RData"))
write.csv(Counts_table, file=here("Results", db.name, "Denominator", "Counts_table.csv"))
save(Counts_table_final, file = here("Results", db.name, "Denominator", "Counts_table_final.RData"))
write.csv(Counts_table_final, file=here("Results", db.name, "Denominator", "Counts_table_final.csv"))

Pretty_Counts_table <- flextable(Counts_table_final) %>% theme_vanilla() %>% 
  set_caption(caption = "Counts of healthcare visits, screening and diagnostic procedures, before, during and after lockdown") %>% 
  width(width = 3.5)

Pretty_Counts_table <- Pretty_Counts_table %>% set_table_properties(layout = "autofit")

save_as_docx('Denominator_counts_table' = Pretty_Counts_table, path="~/R/CancerCovid_Characterisations/Results/CPRDGold_202201/Denominator/Denominator_Counts.docx")
                                                                      
print(paste0("- 1. DENOMINATOR CUSTOM CHARACTERISATIONS DONE"))
info(logger, "- 1. DENOMINATOR CUSTOM CHARACTERISATIONS DONE")

# Create table of covariate names and concept ids
Concept_ids_table  <- rbind(observation_concepts, observation_covariate_names)
Concept_ids_table


# join all covariate concept ids and names into one dataframe
Observation_ids_table  <- cbind(observation_concepts, observation_covariate_names)
Procedure_ids_table  <- cbind(procedure_concepts, procedure_covariate_names)
Measurement_ids_table  <- cbind(measurement_concepts, measurement_covariate_names)
Concept_ids_table <- rbind(Observation_ids_table, Procedure_ids_table, Measurement_ids_table)
as.data.frame(Concept_ids_table)


write.csv(Concept_ids_table, file=here("Results", db.name, "Denominator", "Concept_ids_table.csv"))
save_as_docx('Concept_ids_table' = Concept_ids_table, path="~/R/CancerCovid_Characterisations/Results/CPRDGold_202201/Denominator/Concept_ids_table.docx")
