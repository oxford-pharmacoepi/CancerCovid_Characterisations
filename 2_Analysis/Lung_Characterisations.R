# ============================================================================ #
#                 Custom Characterisations - over 3 time periods               #
#                             Lung Cancer                                #
#                              Nicola Barclay                                  #
#                                12-12-2022                                    #
# ============================================================================ #


print(paste0("- 3. Lung CANCER CUSTOM CHARACTERISATIONS"))
info(logger, "- 3. Lung CANCER CUSTOM CHARACTERISATIONS")

print(paste0("- Set up for Lung cancer characterisations"))
info(logger, "- Set up for Lung cancer characterisations")

## ------------------------------- VARIABLES -------------------------------- ##
# Cohorts_ID
LungCancer_BeforeCOVID       <- 7
LungCancer_DuringLockdown      <- 8
LungCancer_AfterCOVID   <- 9



# List of individuals 
individuals_id <- cohorts_db %>% 
  filter(cohort_definition_id %in% c(LungCancer_BeforeCOVID,LungCancer_DuringLockdown,LungCancer_AfterCOVID)) %>%
  select(subject_id, cohort_start_date) %>%
  rename("person_id" = "subject_id", "index_date" = "cohort_start_date" ) %>%
  compute()
list_id <- individuals_id %>%
  select(person_id) %>%
  compute()
individuals_id <- individuals_id %>% collect()

# uncollect individuals_id to calculate index_date for age below
individuals_id_for_age <- cohorts_db %>% 
  filter(cohort_definition_id %in% c(LungCancer_BeforeCOVID,LungCancer_DuringLockdown,LungCancer_AfterCOVID)) %>%
  select(subject_id, cohort_start_date) %>%
  rename("person_id" = "subject_id", "index_date" = "cohort_start_date" ) %>%
  compute()

print(paste0("- Set up for Lung cancer characterisations done"))
info(logger, "- Set up for Lung cancer characterisations done")


# ======================== BASELINE CHARACTERISTICS=========================== #

print(paste0("- Running baseline characteristics - age and sex"))
info(logger, "- Running baseline characteristics - age and sex")


## AGE AT INDEX DATE IN Lung CANCER COHORTS ----------------------------
age_patients <- individuals_id_for_age %>%
  left_join(person_db) %>%
  select(person_id,year_of_birth,index_date) %>%
  collect() %>%
  mutate(month_of_birth = 1) %>%
  mutate(day_of_birth   = 1) %>%
  mutate(dob = as.Date(dmy(paste(day_of_birth,month_of_birth,year_of_birth,sep="-")))) %>%
  mutate(age = floor(as.numeric(difftime(index_date,dob,unit="days"))/365.25)) 

# AGE GROUP AT INDEX DATE IN Lung CANCER COHORT 

age_group <- age_patients %>%
  mutate(age_grouping = cut(age, c(0,10,20,30,40,50,60,70,80,90,100,110),
                            labels = c("0 to 9 years", "10 to 19 years","20 to 29 years","30 to 39 years","40 to 49 years","50 to 59 years","60 to 69 years","70 to 79 years","80 to 89 years","90 to 99 years","100+ years"),include.lowest = TRUE, right = FALSE, is.na = FALSE)) %>%
  mutate(agegid = as.numeric(age_grouping)) 

# -------- SMD OF AGE AT INDEX DATE BEFORE AND AFTER LOCKDOWN -----------------#

age_1 <- age_patients %>%
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==7) %>%
  filter(index_date==cohort_start_date) %>%
  collect() 

mean_age_1 <-  mean(age_1$age) %>% print()
var_age_1 <- var(age_1$age) %>% print()

age_2 <- age_patients %>%
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==8) %>%
  filter(index_date==cohort_start_date) %>%
  collect() 

mean_age_2 <-  mean(age_2$age) %>% print()
var_age_2 <- var(age_2$age) %>% print()

age_3 <- age_patients %>%
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==9) %>%
  filter(index_date==cohort_start_date) %>%
  collect() 

mean_age_3 <-  mean(age_3$age) %>% print()
var_age_3 <- var(age_3$age) %>% print()

age_table1  <- rbind(mean_age_1,var_age_1,mean_age_2,var_age_2,mean_age_3,var_age_3) %>% as.data.frame() %>% dplyr::mutate_if(is.numeric, round, digits = 2)

age_table_SMD  <- tibble(mean_age_1 = t(age_table1[1,])[,1], var_age_1 = t(age_table1[2,])[,1], 
                         mean_age_2 = t(age_table1[3,])[,1], var_age_2 = t(age_table1[4,])[,1],
                         mean_age_3 = t(age_table1[5,])[,1], var_age_3 = t(age_table1[6,])[,1]) %>%
  mutate("SMD Before lockdown vs. During lockdown" = abs(mean_age_1-mean_age_2)/sqrt(var_age_1+var_age_2)) %>% 
  mutate("SMD Before lockdown vs. After lockdown" = abs(mean_age_1-mean_age_3)/sqrt(var_age_1+var_age_3)) %>% print()


age_table_Lung_formatted <- age_table_SMD %>% dplyr::mutate_if(is.numeric, round, digits = 2) %>%  
  dplyr::mutate("Age at diagnosis before lockdown" = glue("{mean_age_1} ({var_age_1})")) %>%
  dplyr::mutate("Age at diagnosis during lockdown" = glue("{mean_age_2} ({var_age_2})")) %>% 
  dplyr::mutate("Age at diagnosis after lockdown" = glue("{mean_age_3} ({var_age_3})")) 

age_table_Lung_formatted <- age_table_Lung_formatted[-c(1:6)] #  remove superfluous columns
age_table_Lung_formatted <- age_table_Lung_formatted[, c(3, 4, 5, 1, 2)] # reorder the columns
age_table_Lung_formatted

Pretty_mean_age_table <- flextable(age_table_Lung_formatted) %>% theme_vanilla() %>% 
  set_caption(caption = "Mean (variance) of age at date of Lung cancer diagnosis before, during and after lockdown") %>% 
  width(width = 1.4) 

# save the table as a csv file
write.csv(age_table_Lung_formatted, here("Results", db.name, "Lung", "age_table_formatted_Lung.csv"), row.names = FALSE)
save(age_table_Lung_formatted, file = here("Results", db.name, "Lung", "age_table_Lung_formatted.RData"))

# save the table as docx
save_as_docx('Lung_mean_age_table' = Pretty_mean_age_table, path=here("Results", db.name, "Lung", "Lung_age_table_formatted.docx"))



# FREQUENCIES OF AGES AT INDEX DATE FOR Lung PATIENTS DIAGNOSED BEFORE LOCKDOWN ------
age_table_1 <- age_group %>% 
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==7) %>%
  filter(index_date==cohort_start_date) %>%
  group_by(age_grouping) %>%
  tally() %>% 
  print()

# FREQUENCIES OF AGES AT INDEX DATE FOR Lung PATIENTS DIAGNOSED DURING LOCKDOWN -----
age_table_2 <- age_group %>% 
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==8) %>%
  filter(index_date==cohort_start_date) %>%
  group_by(age_grouping) %>%
  tally() %>% 
  print()


# FREQUENCIES OF AGES AT INDEX DATE FOR Lung PATIENTS DIAGNOSED AFTER LOCKDOWN -----
age_table_3 <- age_group %>% 
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==9) %>%
  filter(index_date==cohort_start_date) %>%
  group_by(age_grouping) %>%
  tally() %>% 
  print()


# Make table of age by cohort
# first need to make a data frame of all the possible ages so that categories with zero counts are shown
age_group_labels <- c("0 to 9 years", "10 to 19 years","20 to 29 years","30 to 39 years","40 to 49 years","50 to 59 years",
                      "60 to 69 years","70 to 79 years","80 to 89 years","90 to 99 years","100+ years") %>% as.matrix() 

age_group_labels <- as.data.frame(age_group_labels) %>% rename("age_grouping" = "V1") %>% select("age_grouping")

age_table_1 <- age_table_1 %>% rename("N diagnosed before lockdown"="n")
age_table_2 <- age_table_2 %>% rename("N diagnosed during lockdown"="n")
age_table_3 <- age_table_3 %>% rename("N diagnosed after lockdown"="n")

# get the age grouping tables and join them with the full rows of categories and replace NAs with 0
age_table_1 <- age_group_labels  %>% left_join(age_table_1) %>% replace(is.na(.), 0) %>% print()
age_table_2 <- age_group_labels  %>% left_join(age_table_2) %>% replace(is.na(.), 0) %>% print()
age_table_3 <- age_group_labels  %>% left_join(age_table_3) %>% replace(is.na(.), 0) %>% print()

Age_table_all_Lung_cohorts <- age_table_1 %>% right_join(age_table_2) %>% right_join(age_table_3) %>% rename("Age Group" = "age_grouping") %>% print()

Pretty_age_group_table <- flextable(Age_table_all_Lung_cohorts) %>% theme_vanilla() %>% 
  set_caption(caption = "Age at date of Lung cancer diagnosis before, during and after lockdown") %>% 
  width(width = 1.4)  


# save the table as a csv file
write.csv(Age_table_all_Lung_cohorts, here("Results", db.name, "Lung", "Age_table_all_Lung_cohorts.csv"), row.names = FALSE)

# save the table as docx
save_as_docx('Lung_age_counts_table' = Pretty_age_group_table, path=here("Results", db.name, "Lung", "Lung_age_counts_table.docx"))


# save RData objects
save(Pretty_mean_age_table, Pretty_age_group_table, Age_table_all_Lung_cohorts, age_table_Lung_formatted, file = here("Results", db.name, "Lung", "LungAge.RData"))

print("Age done")



## GENDER Lung CANCER COHORT BEFORELOCKDOWN ----------------------------
gender_patients_1 <-  list_id %>% 
  left_join(person_db) %>%
  select(person_id,gender_concept_id) %>%
  rename("subject_id" ="person_id") %>%
  inner_join(cohorts_db, by = "subject_id") %>%
  filter(cohort_definition_id==7) %>%
  collect() %>%
  distinct() %>%
  mutate(value = if_else(gender_concept_id==8532,1,2)) %>%
  select(-gender_concept_id) %>%
  mutate(sex = if_else(value==1,"Female","Male")) %>%
  group_by(sex) %>%
  tally() %>%
  print()



## GENDER Lung CANCER COHORT DURING LOCKDOWN ----------------------------
gender_patients_2 <-  list_id %>% 
  left_join(person_db) %>%
  select(person_id,gender_concept_id) %>%
  rename("subject_id" ="person_id") %>%
  inner_join(cohorts_db, by = "subject_id") %>%
  filter(cohort_definition_id==8) %>%
  collect() %>%
  distinct() %>%
  mutate(value = if_else(gender_concept_id==8532,1,2)) %>%
  select(-gender_concept_id) %>%
  mutate(sex = if_else(value==1,"Female","Male")) %>%
  group_by(sex) %>%
  tally() %>%
  print()


## GENDER Lung CANCER COHORT AFTER LOCKDOWN ----------------------------
gender_patients_3 <-  list_id %>% 
  left_join(person_db) %>%
  select(person_id,gender_concept_id) %>%
  rename("subject_id" ="person_id") %>%
  inner_join(cohorts_db, by = "subject_id") %>%
  filter(cohort_definition_id==9) %>%
  collect() %>%
  distinct() %>%
  mutate(value = if_else(gender_concept_id==8532,1,2)) %>%
  select(-gender_concept_id) %>%
  mutate(sex = if_else(value==1,"Female","Male")) %>%
  group_by(sex) %>%
  tally() %>%
  print()


gender_table_1 <- gender_patients_1 %>% rename(n_diagnosed_before_lockdown = "n")
gender_table_2 <- gender_patients_2 %>% rename(n_diagnosed_during_lockdown = "n")
gender_table_3 <- gender_patients_3 %>% rename(n_diagnosed_after_lockdown = "n")

gender_table <- gender_table_1 %>% right_join(gender_table_2) %>% right_join(gender_table_3) %>% replace(is.na(.), 0)

gender_table_Lung <- gender_table %>%
  mutate("n diagnosed before lockdown" = paste0(n_diagnosed_before_lockdown, " (", round(100*n_diagnosed_before_lockdown/sum(n_diagnosed_before_lockdown),1), "%)")) %>%
  mutate("n diagnosed during lockdown" = paste0(n_diagnosed_during_lockdown, " (", round(100*n_diagnosed_during_lockdown/sum(n_diagnosed_during_lockdown),1), "%)")) %>%
  mutate("n diagnosed after lockdown" = paste0(n_diagnosed_after_lockdown, " (", round(100*n_diagnosed_after_lockdown/sum(n_diagnosed_after_lockdown),1), "%)"))


gender_table_Lung <- gender_table_Lung[-c(2:4)] #  remove superfluous columns

Pretty_gender_table <- flextable(gender_table_Lung) %>%
  set_caption(caption = "Gender of Lung cancer patients in groups before, during and after lockdown") %>% 
  width(width = 1.4)  


# save the table as a csv file
write.csv(gender_table_Lung, here("Results", db.name, "Lung", "Gender_table_all_Lung_cohorts.csv"), row.names = FALSE)


# save the table as docx
save_as_docx('Lung_gender_counts_table' = Pretty_gender_table, path=here("Results", db.name, "Lung", "Lung_gender_counts_table.docx"))


# save RData objects
save(gender_table_Lung, Pretty_gender_table, file = here("Results", db.name, "Lung", "LungGender.RData"))



print(paste0("- Baseline characteristics - age and sex done"))
info(logger, "- Baseline characteristics - age and sex done")

# ======================== COVARIATES OF INTEREST ============================ #

print(paste0("- Running lung cancer covariate counts"))
info(logger, "- Running lung cancer covariate counts")

## VISITS - STANDALONE  CODE
## 1. VISITS IN THE HEALTHCARE SYSTEM ------------------------------------------
VI_patients <- cdm$visit_occurrence %>%
  select(person_id,visit_start_date) %>% # note that this does not require any filtering 
  # by concept_id because we want any visit occurring in the visit occurrence table
  inner_join(list_id) %>% 
  distinct() %>%
  collect() %>%
  rename("Event_date"="visit_start_date") %>%
  mutate(FeatureExtractionId = 581477001) # the concept id here is purely for reference

VI_id <- tibble(FeatureExtractionId = 581477001,covariateId = 581477, 
                  covariateName = "Visits within healthcare system", AnalysisId = 1)

AnalysisRef  <- tibble(AnalysisId = 1, AnalysisName = "Visits_within_healthcare_system")

save(list = c("VI_patients","VI_id"), file = here("Results", db.name, "Lung", "Lung_covariates", "Visits.RData"))

print("Visits done")




## 2. FAST TRACK REFERRAL FOR SUSPECTED LUNG CANCER ------------------------------------------------
FTRSL_patients <- get_observations(44791283, 2)
FTRSL_id <- get_observations_id(44791283, 2, "Fast track referral for lung cancer")

AnalysisRef  <- rbind(AnalysisRef,c(2,"Fast track referral for lung cancer"))

save(list = c("FTRSL_patients","FTRSL_id"), file = here("Results", db.name, "Lung", "Lung_covariates", "FastTrackReferralSuspectedLung.RData"))

print("Fast track referral for lung cancer done")


## 3. BRONCHOSCOPY  ------------------------------------------------------------

BRONC_patients <- get_procedures(4032404, 3)
BRONC_id <- get_procedures_id(4032404, 3, "Bronchoscopy")

AnalysisRef  <- rbind(AnalysisRef,c(3,"Bronchoscopy"))

save(list = c("BRONC_patients","BRONC_id"), file = here("Results", db.name, "Lung", "Lung_covariates", "Bronchoscopy.RData"))

print("Bronchoscopy done")




## 4. ENDOBRONCHIAL ULTRASONOGRAPHY GUIDED TRANSBRONCHIAL NEEDLE ASPIRATION  -------
EUTNA_patients <- get_procedures(44809038, 4)
EUTNA_id <- get_procedures_id(44809038, 4, "Endobronchial ultrasonography guided transbronchial needle aspiration")

AnalysisRef  <- rbind(AnalysisRef,c(4,"Endobronchial ultrasonography guided transbronchial needle aspiration"))

save(list = c("EUTNA_patients","EUTNA_id"), file = here("Results", db.name, "Lung", "Lung_covariates", "EUTNA.RData"))

print("Endobronchial ultrasonography guided transbronchial needle aspiration done")




## 5. MEDIASTINOSCOPY ----------------------------------------------------
MEDIA_patients <- get_procedures(4128302, 5)
MEDIA_id <- get_procedures_id(4128302, 5, "Mediastinoscopy")

AnalysisRef  <- rbind(AnalysisRef,c(5,"Mediastinoscopy"))

save(list = c("MEDIA_patients","MEDIA_id"), file = here("Results", db.name, "Lung", "Lung_covariates", "Mediastinoscopy.RData"))

print("Mediastinoscopy done")


## 6. MEDIASTINOSCOPY - INSPECTION ONLY ------------------------------------------------
MED_IN_patients <- get_procedures(4070986, 6)
MED_IN_id <- get_procedures_id(4070986, 6, "Mediastinoscopy - inspection only")

AnalysisRef  <- rbind(AnalysisRef,c(6,"Mediastinoscopy - inspection only"))

save(list = c("MED_IN_patients","MED_IN_id"), file = here("Results", db.name, "Lung", "Lung_covariates", "Mediastinisciopy_ins.RData"))

print("Mediastinoscopy - inspection only done")

print(paste0("- Lung cancer covariate counts done"))
info(logger, "- Lung cancer covariate counts done")

## 7. CT AND BIOPSY OF CHEST ----------------------------------------------------
CTB_patients <- get_procedures(4304406, 7)
CTB_id <- get_procedures_id(4304406, 7, "CT and biopsy of chest")

AnalysisRef  <- rbind(AnalysisRef,c(7,"CT and biopsy of chest"))

save(list = c("CTB_patients","CTB_id"), file = here("Results", db.name, "Lung", "Lung_covariates", "CT_biopsy_chest.RData"))

print("CT and biopsy of chest done")



## 8. ULTRASOUND AND BIOPSY OF CHEST ----------------------------------------------------
USBC_patients <- get_procedures(4167553, 8)
USBC_id <- get_procedures_id(4167553, 8, "US scan and biopsy of chest")

AnalysisRef  <- rbind(AnalysisRef,c(8,"US scan and biopsy of chest"))

save(list = c("USBC_patients","USBC_id"), file = here("Results", db.name, "Lung", "Lung_covariates", "US_biopsy_chest.RData"))

print("US scan and biopsy of chest done")


## 9. DIAGNOSTIC RADIOLOGY PROCEDURES OF CHEST------------------------------------------------
DRP_patients <- get_procedures(45889178, 9)
DRP_id <- get_procedures_id(45889178, 9, "Diagnostic Radiology (Diagnostic Imaging) Procedures of the Chest")

AnalysisRef  <- rbind(AnalysisRef,c(9,"Diagnostic Radiology (Diagnostic Imaging) Procedures of the Chest"))

save(list = c("DRP_patients","DRP_id"), file = here("Results", db.name, "Lung", "Lung_covariates", "DiagnosticProceduresChest.RData"))

print("Diagnostic Radiology (Diagnostic Imaging) Procedures of the Chest done")


## 10. MRI OF CHEST ------------------------------------------------
MRI_patients <- get_procedures(4246485, 10)
MRI_id <- get_procedures_id(4246485, 10, "MRI of chest")

AnalysisRef  <- rbind(AnalysisRef,c(10,"MRI of chest"))

save(list = c("MRI_patients","MRI_id"), file = here("Results", db.name, "Lung", "Lung_covariates", "MRIchest.RData"))

print("MRI of chest done")


# ========================= INDIVIDUAL TABLES================================= # 


print(paste0("- Getting lung cancer individual covariate tables"))
info(logger, "- Getting lung cancer individual covariate tables")

# Get tables: person: id, covariate, value
VI_table        <- getIndividualTabs(VI_id, VI_patients, individuals_id, 3, FALSE)
FTRSL_table        <- getIndividualTabs(FTRSL_id, FTRSL_patients, individuals_id,  3, FALSE)
BRONC_table        <- getIndividualTabs(BRONC_id, BRONC_patients, individuals_id,  3, FALSE)
EUTNA_table      <- getIndividualTabs(EUTNA_id, EUTNA_patients, individuals_id, 3, FALSE)
MEDIA_table        <- getIndividualTabs(MEDIA_id, MEDIA_patients, individuals_id, 3, FALSE)
CTB_table        <- getIndividualTabs(CTB_id, CTB_patients, individuals_id,  3, FALSE)
USBC_table        <- getIndividualTabs(USBC_id, USBC_patients, individuals_id,  3, FALSE)
DRP_table        <- getIndividualTabs(DRP_id, DRP_patients, individuals_id, 3, FALSE)
MRI_table        <- getIndividualTabs(MRI_id, MRI_patients, individuals_id, 3, FALSE)
MED_IN_table        <- getIndividualTabs(MED_IN_id, MED_IN_patients, individuals_id, 3, FALSE)

# Join the tables
continuous_table <- VI_table %>% union_all(FTRSL_table) %>% union_all(BRONC_table) %>% union_all(EUTNA_table) %>% union_all(MEDIA_table) %>%
  union_all(MED_IN_table) %>% union_all(CTB_table) %>% union_all(USBC_table) %>% union_all(DRP_table) %>% union_all(MRI_table) %>% ungroup()

# Pivot the continuous table around, and rename person_id as subject_id. This
# is later used to run the SMD function
Continuous_table_pivot <- continuous_table %>% right_join(lung_covariate_names) %>%
  select(person_id, covariate, value) %>% 
  rename("subject_id" = "person_id") %>%
  tidyr::pivot_wider(names_from = covariate, values_from = value,values_fill = 0) 

continuous_table <- Continuous_table_pivot %>% tidyr::pivot_longer(2:31, names_to = "covariate", values_to = "value") 

# read all the covariate names from the 'forALLCharacterisations_with_functions.R
namt <- t(lung_covariate_names)

save(list = c("VI_table", "FTRSL_table", "BRONC_table", "EUTNA_table", "MEDIA_table", "CTB_table", "USBC_table", "DRP_table", "MRI_table", "MED_IN_table",
              "continuous_table", "Continuous_table_pivot", "namt"), file = here("Results", db.name, "Lung", "Lung_covariates", "LungIndividualTabs.Rdata"))

print(paste0("- Got lung cancer individual covariate tables"))
info(logger, "- Got lung cancer individual covariate tables")


# =================== AGGREGATED COUNTS OF COVARIATES ======================== # 

print(paste0("- Getting aggregated counts of Lung cancer covariate tables"))
info(logger, "- Getting aggregated counts of Lung cancer covariate tables")

# All tables joined together
# Cohort 1 - Lung cancer before lockdown
All_tables_counts1 <- continuous_table %>%  
  inner_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==7) %>%
  group_by(covariate) %>% 
  tally(value)%>% 
  print(n=30)


# cohort 2 - Lung cancer during lockdown
All_tables_counts2 <- continuous_table %>%  
  inner_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==8) %>%
  group_by(covariate) %>% 
  tally(value) %>% 
  print(n=30)


# cohort 3 - Lung cancer after lockdown
All_tables_counts3 <- continuous_table %>%  
  inner_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==9) %>%
  group_by(covariate) %>% 
  tally(value) %>% 
  print(n=30)

All_tables_counts1 <- All_tables_counts1 %>% rename("n before lockdown" = "n") %>% rename("Screening/diagnostic test" = "covariate")
All_tables_counts2 <- All_tables_counts2 %>% rename("n during lockdown" = "n") %>% rename("Screening/diagnostic test" = "covariate")
All_tables_counts3 <- All_tables_counts3 %>% rename("n after lockdown" = "n") %>% rename("Screening/diagnostic test" = "covariate")

All_count_joined <- All_tables_counts1 %>% right_join(All_tables_counts2) %>% right_join(All_tables_counts3) %>% print()

Pretty_counts_table <- flextable(All_count_joined) %>% set_caption(caption = 
                                                                     "Frequencies of visits, Lung cancer-related diagnostic/screening tests/visits during different time periods before, during and after lockdown") %>%
  set_table_properties(layout = "autofit")
Pretty_counts_table

# save the table as a csv file
write.csv(All_count_joined, here("Results", db.name, "Lung", "Lung_screening_diagnostic_counts_table.csv"), row.names = FALSE)


# save the table as docx
save_as_docx('Lung_counts_table' = Pretty_counts_table, path=here("Results", db.name, "Lung", "Lung_screening_diagnostic_counts_table.docx"))


# save RData objects
save(All_count_joined, Pretty_counts_table, file = here("Results", db.name, "Lung", "LungScreeningDiagnosticCounts.RData"))

print(paste0("- Got aggregated counts of Lung cancer covariate tables"))
info(logger, "- Got aggregated counts of Lung cancer covariate tables")


# =============================== (SMD) ====================================== #

print(paste0("- Getting SMD of Lung cancer covariate tables"))
info(logger, "- Getting SMD of Lung cancer covariate tables")

# Get all person level tables together and filter by cohort_definition_id_1
All_tables_cohort_1 <- individuals_id %>% select(person_id) %>%
  rename("subject_id"="person_id") %>%
  left_join(continuous_table) %>%
  select(subject_id, covariate, value) %>%
  inner_join(cohorts_db_df, by = "subject_id") %>%
  distinct() %>%
  filter(cohort_definition_id==7)

# Pivot the table so that all the covariates which were rows in the above code are now column headings
All_tables_cohort_1 <- All_tables_cohort_1 %>% select(subject_id, covariate, value, cohort_definition_id, cohort_start_date, cohort_end_date) %>% 
  tidyr::pivot_wider(names_from = covariate, values_from = value,values_fill = 0) # if this throws an error it's because there are duplicate records in the data somewhere. Look for it!


# Get all person level tables together and filter by cohort_definition_id_2
All_tables_cohort_2 <- individuals_id %>% select(person_id) %>%
  rename("subject_id"="person_id") %>%
  left_join(continuous_table) %>%
  select(subject_id, covariate, value) %>%
  inner_join(cohorts_db_df, by = "subject_id") %>%
  distinct() %>%
  filter(cohort_definition_id==8)

# Pivot the table so that all the covariates which were rows in the above code are now column headings
All_tables_cohort_2 <- All_tables_cohort_2 %>% select(subject_id, covariate, value, cohort_definition_id, cohort_start_date, cohort_end_date) %>% 
  tidyr::pivot_wider(names_from = covariate, values_from = value,values_fill = 0)


# Get all person level tables together and filter by cohort_definition_id_3
All_tables_cohort_3 <- individuals_id %>% select(person_id) %>%
  rename("subject_id"="person_id") %>%
  left_join(continuous_table) %>%
  select(subject_id, covariate, value) %>%
  inner_join(cohorts_db_df, by = "subject_id") %>%
  distinct() %>%
  filter(cohort_definition_id==9)

# Pivot the table so that all the covariates which were rows in the above code are now column headings
All_tables_cohort_3 <- All_tables_cohort_3 %>% select(subject_id, covariate, value, cohort_definition_id, cohort_start_date, cohort_end_date) %>% 
  tidyr::pivot_wider(names_from = covariate, values_from = value,values_fill = 0)

# Run SMD function to create table of all
All_SMD_Pre_lock <- compute_continuous_smd(All_tables_cohort_1, All_tables_cohort_2) 
All_SMD_Pre_post <- compute_continuous_smd(All_tables_cohort_1, All_tables_cohort_3) 

All_SMD_Pre_lock  <- All_SMD_Pre_lock  %>% rename("SMD Before lockdown vs. During lockdown" = "SMD") 
All_SMD_Pre_post  <- All_SMD_Pre_post  %>% rename("SMD Before lockdown vs. After lockdown" = "SMD") 

All_SMD_Pre_post  <- All_SMD_Pre_post  %>%  rename("mean3" = mean2) %>% rename("var3" = var2) 


All_SMD <- All_SMD_Pre_lock %>% full_join(All_SMD_Pre_post) %>% arrange(covariate)

All_SMD_formatted <- All_SMD %>% mutate("Mean (var) before lockdown"= paste0(paste(mean1)," (", paste(var1), ")")) %>%
  mutate("Mean (var) during lockdown"= paste0(paste(mean2)," (", paste(var2), ")")) %>%
  mutate("Mean(var) after lockdown"= paste0(paste(mean3)," (", paste(var3), ")")) %>%
  rename("Screening/Diagnostic Test" = "covariate") 

All_SMD_formatted <-  All_SMD_formatted[-c(2,3,4,5,7,8)]
All_SMD_formatted <-  All_SMD_formatted[c(1,4,5,6,2,3)]

Pretty_Lung_SMD_table <- flextable(All_SMD_formatted) %>% set_caption(caption = "Mean(var) frequencies of visits, Lung cancer-related screening/diagnostic tests during different time periods before and after lockdown") %>%
  set_table_properties(layout = "autofit")

Pretty_Lung_SMD_table

## ========================= Save all tables ================================ ##

# save RData objects
save(list = c("All_tables_counts1", "All_tables_counts2",  "All_tables_counts3", "All_count_joined", "Pretty_counts_table", "All_tables_cohort_1", "All_tables_cohort_2",
              "All_tables_cohort_3", "All_SMD", "All_SMD_formatted", "Pretty_Lung_SMD_table"), file = here("Results", db.name, "Lung", "Lung_covariates", "LungScreeningDiagnosticCountsSMDTabs.Rdata"))

# save the table as a csv file
write.csv(All_SMD_formatted, here("Results", db.name, "Lung", "LungScreeningDiagnosticSMD.csv"), row.names = FALSE)
write.csv(All_count_joined, here("Results", db.name, "Lung", "All_count_joined_Lung.csv"), row.names = FALSE)

# save the table as docx
save_as_docx('LungScreeningDiagnosticSMD' = Pretty_Lung_SMD_table, path=here("Results", db.name, "Lung", "LungScreeningDiagnosticSMD.docx"))

print(paste0("- Got SMD of Lung cancer covariate tables"))
info(logger, "- Got SMD of Lung cancer covariate tables")


print(paste0("- 2. Lung CANCER CUSTOM CHARACTERISATIONS DONE"))
info(logger, "- 2. Lung CANCER CUSTOM CHARACTERISATIONS DONE")
