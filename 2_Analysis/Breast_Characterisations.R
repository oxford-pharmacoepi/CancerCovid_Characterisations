# ============================================================================ #
#                Custom Characterisations - over 3 time periods                #
#                               Breast Cancer                                  #
#                              Nicola Barclay                                  #
#                                8-12-2022                                     #
# ============================================================================ #

print(paste0("- 2. BREAST CANCER CUSTOM CHARACTERISATIONS"))
info(logger, "- 2. BREAST CANCER CUSTOM CHARACTERISATIONS")

print(paste0("- Set up for breast cancer characterisations"))
info(logger, "- Set up for breast cancer characterisations")

## ---------- VARIABLES OF INTEREST BEFORE, DURING AND AFTER LOCKDOWN --------------- ##

# Read the cancer cohort table name and the cdm databases
cohorts_db        <- cdm[[outcome_table_name_2]]
cohorts_db_df <- as.data.frame(cohorts_db)

# Cohorts_ID
BreastCancer_BeforeCOVID       <- 1
BreastCancer_DuringLockdown      <- 2
BreastCancer_AfterCOVID   <- 3

# List of individuals 
individuals_id <- cohorts_db %>% 
  filter(cohort_definition_id %in% c(BreastCancer_BeforeCOVID,BreastCancer_DuringLockdown,BreastCancer_AfterCOVID)) %>%
  select(subject_id, cohort_start_date) %>%
  rename("person_id" = "subject_id", "index_date" = "cohort_start_date" ) %>%
  compute()
list_id <- individuals_id %>%
  select(person_id) %>%
  compute()
individuals_id <- individuals_id %>% collect()

# uncollect individuals_id to calculate index_date for age below
individuals_id_for_age <- cohorts_db %>% 
  filter(cohort_definition_id %in% c(BreastCancer_BeforeCOVID,BreastCancer_DuringLockdown,BreastCancer_AfterCOVID)) %>%
  select(subject_id, cohort_start_date) %>%
  rename("person_id" = "subject_id", "index_date" = "cohort_start_date" ) %>%
  compute()

print(paste0("- Set up for breast cancer characterisations done"))
info(logger, "- Set up for breast cancer characterisations done")


# ======================== BASELINE CHARACTERISTICS=========================== #

print(paste0("- Running baseline characteristics - age and sex"))
info(logger, "- Running baseline characteristics - age and sex")

# AGE AT INDEX DATE IN BREAST CANCER COHORTS ----------------------------
age_patients <- individuals_id_for_age %>%
  left_join(person_db) %>%
  select(person_id,year_of_birth,index_date) %>%
  collect() %>%
  mutate(month_of_birth = 1) %>%
  mutate(day_of_birth   = 1) %>%
  mutate(dob = as.Date(dmy(paste(day_of_birth,month_of_birth,year_of_birth,sep="-")))) %>%
  mutate(age = floor(as.numeric(difftime(index_date,dob,unit="days"))/365.25)) 

# AGE GROUP AT INDEX DATE IN BREAST CANCER COHORT 
age_group <- age_patients %>%
    mutate(age_grouping = cut(age, c(0,10,20,30,40,50,60,70,80,90,100,110),labels = c("0 to 9 years", "10 to 19 years","20 to 29 years","30 to 39 years","40 to 49 years","50 to 59 years","60 to 69 years","70 to 79 years","80 to 89 years","90 to 99 years","100+ years"),include.lowest = TRUE, right = FALSE, is.na = FALSE)) %>%
    mutate(agegid = as.numeric(age_grouping)) 
 
# -------- SMD OF AGE AT INDEX DATE BEFORE, DURING AND AFTER LOCKDOWN -----------------#

age_1 <- age_patients %>%
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==1) %>%
  filter(index_date==cohort_start_date) %>%
  collect() 

mean_age_1 <-  mean(age_1$age) %>% print()
var_age_1 <- var(age_1$age) %>% print()

age_2 <- age_patients %>%
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==2) %>%
  filter(index_date==cohort_start_date) %>%
  collect() 

mean_age_2 <-  mean(age_2$age) %>% print()
var_age_2 <- var(age_2$age) %>% print()

age_3 <- age_patients %>%
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==3) %>%
  filter(index_date==cohort_start_date) %>%
  collect() 

mean_age_3 <-  mean(age_3$age) %>% print()
var_age_3 <- var(age_3$age) %>% print()

  
age_table1  <- rbind(mean_age_1,var_age_1,mean_age_2,var_age_2,mean_age_3,var_age_3) %>% as.data.frame() %>% dplyr::mutate_if(is.numeric, round, digits = 2)

age_table_SMD  <- tibble(mean_age_1 = t(age_table1[1,])[,1], var_age_1 = t(age_table1[2,])[,1], 
                         mean_age_2 = t(age_table1[3,])[,1], var_age_2 = t(age_table1[4,])[,1],
                         mean_age_3 = t(age_table1[5,])[,1], var_age_3 = t(age_table1[6,])[,1]) %>%
  mutate("Pre/During SMD" = abs(mean_age_1-mean_age_2)/sqrt(var_age_1+var_age_2)) %>% 
  mutate("Pre/Post SMD" = abs(mean_age_1-mean_age_3)/sqrt(var_age_1+var_age_3)) %>% print()


age_table_formatted <- age_table_SMD %>% dplyr::mutate_if(is.numeric, round, digits = 2) %>%  
                       dplyr::mutate("Age at diagnosis before lockdown" = glue("{mean_age_1} ({var_age_1})")) %>%
                       dplyr::mutate("Age at diagnosis during lockdown" = glue("{mean_age_2} ({var_age_2})")) %>% 
                       dplyr::mutate("Age at diagnosis after lockdown" = glue("{mean_age_3} ({var_age_3})")) 

age_table_formatted <- age_table_formatted[-c(1:6)] #  remove superfluous columns
age_table_formatted <- age_table_formatted[, c(3, 4, 5, 1, 2)] # reorder the columns
age_table_formatted

Pretty_mean_age_table <- flextable(age_table_formatted) %>% theme_vanilla() %>% 
  set_caption(caption = "Mean (variance) of age at date of breast cancer diagnosis before, during and after lockdown") %>% 
  width(width = 1.4) 

# save the table as a csv file
write.csv(age_table_formatted, here("Results", db.name, "Breast", "age_table_formatted_breast.csv"), row.names = FALSE)
save(age_table_formatted, file = here("Results", db.name, "Breast", "age_table_formatted.RData"))

# save the table as docx
save_as_docx('Breast_mean_age_table' = Pretty_mean_age_table, path=here("Results", db.name, "Breast", "breast_age_table_formatted.docx"))


# FREQUENCIES OF AGES AT INDEX DATE FOR PATIENTS DIAGNOSED BEFORE LOCKDOWN ------
age_table_1 <- age_group %>% 
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==1) %>%
  filter(index_date==cohort_start_date) %>%
  group_by(age_grouping) %>%
  tally() %>% 
  print()

# FREQUENCIES OF AGES AT INDEX DATE FOR PATIENTS DIAGNOSED DURING LOCKDOWN -----
age_table_2 <- age_group %>% 
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==2) %>%
  filter(index_date==cohort_start_date) %>%
  group_by(age_grouping) %>%
  tally() %>% 
  print()


# FREQUENCIES OF AGES AT INDEX DATE FOR PATIENTS DIAGNOSED AFTER LOCKDOWN -----
age_table_3 <- age_group %>% 
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==3) %>%
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

Age_table_all_breast_cohorts <- age_table_1 %>% right_join(age_table_2) %>% right_join(age_table_3) %>% rename("Age Group" = "age_grouping") %>% print()

Pretty_age_group_table <- flextable(Age_table_all_breast_cohorts) %>% theme_vanilla() %>% 
  set_caption(caption = "Age at date of breast cancer diagnosis before, during and after lockdown") %>% 
  width(width = 1.4)  


# save the table as a csv file
write.csv(Age_table_all_breast_cohorts, here("Results", db.name, "Breast", "Age_table_all_breast_cohorts.csv"), row.names = FALSE)

# save the table as docx
save_as_docx('Breast_age_counts_table' = Pretty_age_group_table, path=here("Results", db.name, "Breast", "breast_age_counts_table.docx"))


# save RData objects
save(Pretty_mean_age_table, Pretty_age_group_table, Age_table_all_breast_cohorts, age_table_formatted, file = here("Results", db.name, "Breast", "BreastAge.RData"))

print("Age done")

# GENDER BREAST CANCER COHORT BEFORE LOCKDOWN ----------------------------
gender_patients_1 <-  list_id %>% 
  left_join(person_db) %>%
  select(person_id,gender_concept_id) %>%
  rename("subject_id" ="person_id") %>%
  inner_join(cohorts_db, by = "subject_id") %>%
  filter(cohort_definition_id==1) %>%
  collect() %>%
  distinct() %>%
  mutate(value = if_else(gender_concept_id==8532,1,2)) %>%
  select(-gender_concept_id) %>%
  mutate(sex = if_else(value==1,"Female","Male")) %>%
  group_by(sex) %>%
  tally() %>%
  print()



# GENDER BREAST CANCER COHORT DURING LOCKDOWN ----------------------------
gender_patients_2 <-  list_id %>% 
  left_join(person_db) %>%
  select(person_id,gender_concept_id) %>%
  rename("subject_id" ="person_id") %>%
  inner_join(cohorts_db, by = "subject_id") %>%
  filter(cohort_definition_id==2) %>%
  collect() %>%
  distinct() %>%
  mutate(value = if_else(gender_concept_id==8532,1,2)) %>%
  select(-gender_concept_id) %>%
  mutate(sex = if_else(value==1,"Female","Male")) %>%
  group_by(sex) %>%
  tally() %>%
  print()


# GENDER BREAST CANCER COHORT AFTER LOCKDOWN ----------------------------
gender_patients_3 <-  list_id %>% 
  left_join(person_db) %>%
  select(person_id,gender_concept_id) %>%
  rename("subject_id" ="person_id") %>%
  inner_join(cohorts_db, by = "subject_id") %>%
  filter(cohort_definition_id==3) %>%
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

gender_table <- gender_table %>%
  mutate("n diagnosed before lockdown" = paste0(n_diagnosed_before_lockdown, " (", round(100*n_diagnosed_before_lockdown/sum(n_diagnosed_before_lockdown),1), "%)")) %>%
  mutate("n diagnosed during lockdown" = paste0(n_diagnosed_during_lockdown, " (", round(100*n_diagnosed_during_lockdown/sum(n_diagnosed_during_lockdown),1), "%)")) %>%
  mutate("n diagnosed after lockdown" = paste0(n_diagnosed_after_lockdown, " (", round(100*n_diagnosed_after_lockdown/sum(n_diagnosed_after_lockdown),1), "%)"))

  
gender_table <- gender_table[-c(2:4)] #  remove superfluous columns

Pretty_gender_table <- flextable(gender_table) %>%
  set_caption(caption = "Gender of breast cancer patients in groups before, during and after lockdown") %>% 
  width(width = 1.4)  


# save the table as a csv file
write.csv(gender_table, here("Results", db.name, "Breast", "Gender_table_all_breast_cohorts.csv"), row.names = FALSE)


# save the table as docx
save_as_docx('Breast_gender_counts_table' = Pretty_gender_table, path=here("Results", db.name, "Breast", "breast_gender_counts_table.docx"))


# save RData objects
save(gender_table, Pretty_gender_table, file = here("Results", db.name, "Breast", "BreastGender.RData"))



print(paste0("- Baseline characteristics - age and sex done"))
info(logger, "- Baseline characteristics - age and sex done")


# ======================== COVARIATES OF INTEREST ============================ #

print(paste0("- Running breast cancer covariate counts"))
info(logger, "- Running breast cancer covariate counts")

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

save(list = c("VI_patients","VI_id"), file = here("Results", db.name, "Breast", "Breast_covariates", "Visits.RData"))
     
print("Visits done")




## 2. REFERRAL TO BREAST CLINIC ------------------------------------------------
RBC_patients <- get_observations(4197459, 2)
RBC_id <- get_observations_id(4197459, 2, "Referral_to_breast_clinic")

AnalysisRef  <- rbind(AnalysisRef,c(2,"Referral_to_breast_clinic"))

save(list = c("RBC_patients","RBC_id"), file = here("Results", db.name, "Breast", "Breast_covariates", "Referral_to_breast_clinic.RData"))

print("Referral to breast clinic done")


## 3. REFERRAL TO MAMMOGRAPHY CLINIC -------------------------------------------
RMC_patients <- get_observations(4086282, 3)
RMC_id <- get_observations_id(4086282, 3, "Referral_to_mammography_clinic")


AnalysisRef  <- rbind(AnalysisRef,c(3,"Referral_to_mammography_clinic"))

save(list = c("RMC_patients","RMC_id"), file = here("Results", db.name, "Breast", "Breast_covariates", "Referral_to_mammography_clinic.RData"))

print("Referral to mammography clinic done")


## 4. FAST TRACK REFERRAL FOR SUSPECTED BC -------------------------------------
FTRBC_patients <- get_observations(44791272, 4)
FTRBC_id <- get_observations_id(44791272, 4, "Fasttrack referral for suspected breast cancer")

AnalysisRef  <- rbind(AnalysisRef,c(4,"Fasttrack referral for suspected breast cancer"))

save(list = c("FTRBC_patients","FTRBC_id"), file = here("Results", db.name, "Breast", "Breast_covariates", "Fasttrack_referral.RData"))

print("Fast track referral for suspected breast cancer done")


## 5. REFERRAL TO BREAST SURGEON -----------------------------------------------
RBS_patients <- get_observations(4141840, 5)
RBS_id <- get_observations_id(4141840, 5, "Referral to breast surgeon")

AnalysisRef  <- rbind(AnalysisRef,c(5,"Referral to breast surgeon"))

save(list = c("RBS_patients","RBS_id"), file = here("Results", db.name, "Breast", "Breast_covariates", "Referral_breast_surgeon.RData"))


print("Referral to breast surgeon done")


## 6. SEEN IN BREAST CLINIC ----------------------------------------------------
SBC_patients <- get_observations(4089031, 6)
SBC_id <- get_observations_id(4089031, 6, "Seen in breast clinic")

AnalysisRef  <- rbind(AnalysisRef,c(6,"Seen in breast clinic"))

save(list = c("SBC_patients","SBC_id"), file = here("Results", db.name, "Breast", "Breast_covariates", "Seen_breast_clinic.RData"))

print("Seen in breast clinic done")


## 7. SEEN BY BREAST SURGEON --------------------------------------------------
SBS_patients <- get_observations(4136626, 7)
SBS_id <- get_observations_id(4136626, 7, "Seen by breast surgeon")

AnalysisRef  <- rbind(AnalysisRef,c(7,"Seen by breast surgeon"))

save(list = c("SBS_patients","SBS_id"), file = here("Results", db.name, "Breast", "Breast_covariates", "Seen_breast_surgeon.RData"))

print("Seen by breast surgeon done")




## 8. DIAGNOSTIC MAMMOGRAMS ----------------------------------------------------
DM_patients <- get_procedures(4324693, 7)
DM_id <- get_procedures_id(4324693, 7, "Diagnostic mammograms")

AnalysisRef  <- rbind(AnalysisRef,c(7,"Diagnostic mammograms"))

save(list = c("DM_patients","DM_id"), file = here("Results", db.name, "Breast", "Breast_covariates", "Diagnostic_mammograms.RData"))

print("Diagnostic mammograms done")


## 9. SCREENING MAMMOGRAMS -----------------------------------------------------
SM_patients <- cdm$measurement %>%
  select(person_id,measurement_concept_id, measurement_date) %>%
  inner_join(cdm$concept_ancestor %>% 
               filter(ancestor_concept_id == 4077697) %>%
               select("measurement_concept_id" = "descendant_concept_id")) %>%
  inner_join(list_id) %>% 
  distinct() %>%
  collect() %>%
  rename("Event_date"="measurement_date") %>%
  mutate(FeatureExtractionId = 4077697009)

SM_id <- tibble(FeatureExtractionId = 4077697009, covariateId = 4077697, covariateName = "Screening mammography", AnalysisId = 9)

AnalysisRef  <- rbind(AnalysisRef,c(9,"Screening mammography"))

save(list = c("SM_patients","SM_id"), file = here("Results", db.name, "Breast", "Breast_covariates", "Screening_mammography.RData"))

print("Screening mammograms done")


## 10. 11 DIAGNOSTIC MAMMOGRAM AND ULTRASOUND - MEASUREMENT - STANDALONE CODE ------
DMUS_patients <- cdm$measurement %>%
  select(person_id,measurement_concept_id, measurement_date) %>%
  inner_join(cdm$concept_ancestor %>% 
               filter(ancestor_concept_id == 36203740) %>%
               select("measurement_concept_id" = "descendant_concept_id")) %>%
  inner_join(list_id) %>% 
  distinct() %>%
  collect() %>%
  rename("Event_date"="measurement_date") %>%
  mutate(FeatureExtractionId = 36203740010)

DMUS_id <- tibble(FeatureExtractionId = 36203740010,covariateId = 36203740, covariateName = "Diagnostic mammogram and ultrasound", AnalysisId = 10)

AnalysisRef  <- rbind(AnalysisRef,c(10,"Diagnostic mammogram and ultrasound"))

save(list = c("DMUS_patients","DMUS_id"), file = here("Results", db.name, "Breast", "Breast_covariates", "Diagnostic_mammogram_ultrasound.RData"))

print("Diagnostic mammogram and ultrasound done")



## 12. BIOPSY OF BREAST --------------------------------------------------------
BB_patients <- get_procedures(4047494, 12)
BB_id <- get_procedures_id(4047494, 12, "Biopsy of breast")

AnalysisRef  <- rbind(AnalysisRef,c(12,"Biopsy of breast"))

save(list = c("BB_patients","BB_id"), file = here("Results", db.name, "Breast", "Breast_covariates", "Biopsy_breast.RData"))

print("Biopsy of breast done")


## 13. STEREOTACTICALLY GUIDED CORE NEEDLE BIOPSY OF BREAST --------------------
SNBB_patients <- get_procedures(4022932, 13)
SNBB_id <- get_procedures_id(4022932, 13, "Stereotactically guided core needle biopsy of breast")

AnalysisRef  <- rbind(AnalysisRef,c(13,"Stereotactically guided core needle biopsy of breast"))

save(list = c("SNBB_patients","SNBB_id"), file = here("Results", db.name, "Breast", "Breast_covariates", "Sterotactically_biopsy_breast.RData"))

print("Stereotactically guided core needle biopsy of breast done")


## 14. PERCUTANEOUS NEEDLE BIOPSY OF BREAST ------------------------------------
PNB_patients <- get_procedures(4028790, 14)
PNB_id <- get_procedures_id(4028790, 14, "Percutaneous needle biopsy of breast")

AnalysisRef  <- rbind(AnalysisRef,c(14,"Percutaneous needle biopsy of breast"))

save(list = c("PNB_patients","PNB_id"), file = here("Results", db.name, "Breast", "Breast_covariates", "Percutaneous_needle_biopsy.RData"))

print("Percutaneous needle biopsy of breast done")



## 13. FINE NEEDLE ASPIRATION OF BREAST ----------------------------------------
FNA_patients <- get_procedures(4306207, 15)
FNA_id <- get_procedures_id(4306207, 15, "Fine needle aspiration of breast")

AnalysisRef  <- rbind(AnalysisRef,c(15,"Fine needle aspiration of breast"))

save(list = c("FNA_patients","FNA_id"), file = here("Results", db.name, "Breast", "Breast_covariates", "Fine_needle_aspiration.RData"))

print("Fine needle aspiration of breast done")


## 16. WIRE GUIDED LOCAL EXCISION ----------------------------------------------
WGLE_patients <- get_procedures(4216180, 16)
WGLE_id <- get_procedures_id(4216180, 16, "Wire guided local excision of breast lump")

AnalysisRef  <- rbind(AnalysisRef,c(16,"Wire guided local excision of breast lump"))

save(list = c("WGLE_patients","WGLE_id"), file = here("Results", db.name, "Breast", "Breast_covariates", "Wire_guided_local_excision.RData"))


print("Wire guided local excision of breast lump done")


## 17. EXCISION OF MAMMARY DUCT ------------------------------------------------
EMD_patients <- get_procedures(4146780, 17)
EMD_id <- get_procedures_id(4146780, 17, "Excision of mammary duct")

AnalysisRef  <- rbind(AnalysisRef,c(17,"Excision of mammary duct"))

save(list = c("EMD_patients","EMD_id"), file = here("Results", db.name, "Breast", "Breast_covariates", "Excision_mammary_duct.RData"))

print("Excision of mammary duct done")


## 18. WIDE LOCAL EXCISION OF BREAST LESION ------------------------------------
WLEBL_patients <- get_procedures(4129190, 18)
WLEBL_id <- get_procedures_id(4129190, 18, "Wide local excision of breast lesion")

AnalysisRef  <- rbind(AnalysisRef,c(18,"Wide local excision of breast lesion"))

save(list = c("WLEBL_patients","WLEBL_id"), file = here("Results", db.name, "Breast", "Breast_covariates", "Wide_local_excision_breast_lesion.RData"))

print("Wide local excision of breast lesion done")


## 19. EXCISION OF LESION OF BREAST --------------------------------------------
ELB_patients <- get_procedures(4194124, 19)
ELB_id <- get_procedures_id(4194124, 19, "Excision of lesion of breast")

AnalysisRef  <- rbind(AnalysisRef,c(19,"Excision of lesion of breast"))

save(list = c("ELB_patients","ELB_id"), file = here("Results", db.name, "Breast", "Breast_covariates", "Excision_lesion_breast.RData"))

print("Excision of lesion of breast done")


## 20. EXCISION OF BREAST TISSUE -----------------------------------------------
EBT_patients <- get_procedures(4286804, 20)
EBT_id <- get_procedures_id(4286804, 20, "Excision of breast tissue")

AnalysisRef  <- rbind(AnalysisRef,c(20,"Excision of breast tissue"))

save(list = c("EBT_patients","EBT_id"), file = here("Results", db.name, "Breast", "Breast_covariates", "Excision_breast_tissue.RData"))

print("Excision of breast tissue done")




print(paste0("- Breast cancer covariate counts done"))
info(logger, "- Breast cancer covariate counts done")


# ========================= INDIVIDUAL TABLES================================= # 

print(paste0("- Getting breast cancer individual covariate tables"))
info(logger, "- Getting breast cancer individual covariate tables")

# Get tables: person: id, covariate, value
VI_table        <- getIndividualTabs(VI_id, VI_patients, individuals_id, 3, FALSE)
RBC_table        <- getIndividualTabs(RBC_id, RBC_patients, individuals_id, 3, FALSE)
RMC_table        <- getIndividualTabs(RMC_id, RMC_patients, individuals_id, 3, FALSE)
FTRBC_table      <- getIndividualTabs(FTRBC_id, FTRBC_patients, individuals_id,  3, FALSE)
RBS_table        <- getIndividualTabs(RBS_id, RBS_patients, individuals_id, 3, FALSE)
SBC_table        <- getIndividualTabs(SBC_id, SBC_patients, individuals_id, 3, FALSE)
SBS_table        <- getIndividualTabs(SBS_id, SBS_patients, individuals_id, 3, FALSE)
DM_table        <- getIndividualTabs(DM_id, DM_patients, individuals_id, 3, FALSE)
SM_table        <- getIndividualTabs(SM_id, SM_patients, individuals_id, 3, FALSE)
DMUS_table        <- getIndividualTabs(DMUS_id, DMUS_patients, individuals_id, 3, FALSE)
BB_table        <- getIndividualTabs(BB_id, BB_patients, individuals_id, 3, FALSE)
SNBB_table        <- getIndividualTabs(SNBB_id, SNBB_patients, individuals_id, 3, FALSE)
PNB_table        <- getIndividualTabs(PNB_id, PNB_patients, individuals_id, 3, FALSE)
FNA_table        <- getIndividualTabs(FNA_id, FNA_patients, individuals_id, 3, FALSE)
WGLE_table        <- getIndividualTabs(WGLE_id, WGLE_patients, individuals_id, 3, FALSE)
EMD_table        <- getIndividualTabs(EMD_id, EMD_patients, individuals_id, 3, FALSE)
WLEBL_table        <- getIndividualTabs(WLEBL_id, WLEBL_patients, individuals_id, 3, FALSE)
ELB_table        <- getIndividualTabs(ELB_id, ELB_patients, individuals_id, 3, FALSE)
EBT_table        <- getIndividualTabs(EBT_id, EBT_patients, individuals_id, 3, FALSE)



# Join the tables
continuous_table <- VI_table %>% union_all(RBC_table) %>% union_all(RMC_table) %>% union_all(FTRBC_table) %>% union_all(RBS_table) %>%
   union_all(SBC_table) %>% union_all(SBS_table) %>% union_all(DM_table) %>%   union_all(SM_table) %>% union_all(DMUS_table) %>% union_all(BB_table) %>% 
  union_all(SNBB_table) %>% union_all(PNB_table) %>% union_all(FNA_table) %>% union_all(WGLE_table) %>% union_all(EMD_table) %>% 
  union_all(WLEBL_table) %>% union_all(ELB_table) %>% union_all(EBT_table)  %>%ungroup()

save(list = c("VI_table", "RBC_table", "RMC_table", "FTRBC_table", "RBS_table", "SM_table", "SBC_table", "DM_table", "DMUS_table", 
              "BB_table", "SNBB_table", "PNB_table", "FNA_table", "WGLE_table", "EMD_table", "WLEBL_table", "ELB_table", "EBT_table", 
              "SBS_table", "continuous_table"),  
     file = here("Results", db.name, "Breast", "Breast_covariates", "BreastIndividualTabs.Rdata"))


# Pivot the continuous table around, and rename person_id as subject_id. This
# is later used to run the SMD function
Continuous_table_pivot <- continuous_table %>% inner_join(breast_covariate_names) %>%
  select(person_id, covariate, value) %>% 
  rename("subject_id" = "person_id") %>%
   tidyr::pivot_wider(names_from = covariate, values_from = value,values_fill = 0) 

continuous_table <- Continuous_table_pivot %>% tidyr::pivot_longer(2:58, names_to = "covariate", values_to = "value") 

# read all the covariate names from the 'forAllCharacterisations_with_functions.R
namt <- t(breast_covariate_names)



print(paste0("- Got breast cancer individual covariate tables"))
info(logger, "- Got breast cancer individual covariate tables")


# =================== AGGREGATED COUNTS OF COVARIATES ======================== # 

print(paste0("- Getting aggregated counts of breast cancer covariate tables"))
info(logger, "- Getting aggregated counts of breast cancer covariate tables")

# All tables joined together
# Cohort 1 - breast cancer before lockdown
All_tables_counts1 <- continuous_table %>%  
  inner_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==1) %>%
  group_by(covariate) %>% 
  tally(value)%>% 
    print(n=57)


# cohort 2 - breast cancer during lockdown
All_tables_counts2 <- continuous_table %>%  
  inner_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==2) %>%
  group_by(covariate) %>% 
  tally(value) %>% 
    print(n=57)


# cohort 3 - breast cancer after lockdown
All_tables_counts3 <- continuous_table %>%  
  inner_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==3) %>%
  group_by(covariate) %>% 
  tally(value) %>% 
  print(n=57)

All_tables_counts1 <- All_tables_counts1 %>% rename("n before lockdown" = "n") %>% rename("Screening/diagnostic test" = "covariate")
All_tables_counts2 <- All_tables_counts2 %>% rename("n during lockdown" = "n") %>% rename("Screening/diagnostic test" = "covariate")
All_tables_counts3 <- All_tables_counts3 %>% rename("n after lockdown" = "n") %>% rename("Screening/diagnostic test" = "covariate")

All_count_joined <- All_tables_counts1 %>% right_join(All_tables_counts2) %>% right_join(All_tables_counts3) %>% print()

Pretty_counts_table <- flextable(All_count_joined) %>% set_caption(caption = 
        "Frequencies of visits, breast cancer-related diagnostic/screening tests/visits during different time periods before, during and after lockdown") %>%
  set_table_properties(layout = "autofit")
Pretty_counts_table

# save the table as a csv file
write.csv(All_count_joined, here("Results", db.name, "Breast", "Breast_screening_diagnostic_counts_table.csv"), row.names = FALSE)


# save the table as docx
save_as_docx('Breast_counts_table' = Pretty_counts_table, path=here("Results", db.name, "Breast", "Breast_screening_diagnostic_counts_table.docx"))


# save RData objects
save(All_count_joined, Pretty_counts_table, file = here("Results", db.name, "Breast", "BreastScreeningDiagnosticCounts.RData"))

print(paste0("- Got aggregated counts of breast cancer covariate tables"))
info(logger, "- Got aggregated counts of breast cancer covariate tables")


# =============================== (SMD) ====================================== #

print(paste0("- Getting SMD of breast cancer covariate tables"))
info(logger, "- Getting SMD of breast cancer covariate tables")

# Get all person level tables together and filter by cohort_definition_id_1
All_tables_cohort_1 <- individuals_id %>% select(person_id) %>%
  rename("subject_id"="person_id") %>%
  left_join(continuous_table) %>%
  select(subject_id, covariate, value) %>%
  inner_join(cohorts_db_df, by = "subject_id") %>%
  distinct() %>%
  filter(cohort_definition_id==1)
 
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
  filter(cohort_definition_id==2)

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
  filter(cohort_definition_id==3)

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

Pretty_Breast_SMD_table <- flextable(All_SMD_formatted) %>% set_caption(caption = "Mean(var) frequencies of visits, breast cancer-related screening/diagnostic tests during different time periods before and after lockdown") %>%
  set_table_properties(layout = "autofit")

Pretty_Breast_SMD_table

## ========================= Save all tables ================================ ##

# save RData objects
save(list = c("All_tables_counts1", "All_tables_counts2",  "All_tables_counts3", "All_count_joined", "Pretty_counts_table", "All_tables_cohort_1", "All_tables_cohort_2",
              "All_tables_cohort_3", "All_SMD", "All_SMD_formatted", "Pretty_Breast_SMD_table"), file = here("Results", db.name, "Breast", "Breast_covariates", "BreastScreeningDiagnosticCountsSMDTabs.Rdata"))

# save the table as a csv file
write.csv(All_SMD_formatted, here("Results", db.name, "Breast", "BreastScreeningDiagnosticSMD.csv"), row.names = FALSE)
write.csv(All_count_joined, here("Results", db.name, "Breast", "All_count_joined_breast.csv"), row.names = FALSE)

# save the table as docx
save_as_docx('BreastScreeningDiagnosticSMD' = Pretty_Breast_SMD_table, path=here("Results", db.name, "Breast", "BreastScreeningDiagnosticSMD.docx"))

print(paste0("- Got SMD of breast cancer covariate tables"))
info(logger, "- Got SMD of breast cancer covariate tables")


print(paste0("- 2. BREAST CANCER CUSTOM CHARACTERISATIONS DONE"))
info(logger, "- 2. BREAST CANCER CUSTOM CHARACTERISATIONS DONE")
