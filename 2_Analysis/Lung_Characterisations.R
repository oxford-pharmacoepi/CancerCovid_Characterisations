# ============================================================================ #
#                         Custom Characterisations                             #
#                                Lung Cancer                                   #
#                              Nicola Barclay                                  #
#                                19-12-2022                                    #
# ============================================================================ #

## Connect to database
source("~/R/GOLD_CDM_connection.R")

## Packages, variables, tables and functions needed
source("~/R/CancerCovid/Custom Characterisations/Lung/forLungCharacterisations_with_functions.R")

## ------------------------------- VARIABLES -------------------------------- ##
# Cohorts_ID

Lung_after         <- 5
Lung_before        <- 6


cohort_id_groups   <- c(Lung_after,Lung_before)

cohorts_db_df <- as.data.frame(cohorts_db)

# List of individuals 
individuals_id <- cohorts_db %>% 
  filter(cohort_definition_id %in% c(Lung_after,Lung_before)) %>%
  select(subject_id, cohort_start_date) %>%
  rename("person_id" = "subject_id", "index_date" = "cohort_start_date" ) %>%
  compute()
list_id <- individuals_id %>%
  select(person_id) %>%
  compute()
individuals_id <- individuals_id %>% collect()

# uncollect individuals_id to calculate index_date for age below
individuals_id_for_age <- cohorts_db %>% 
  filter(cohort_definition_id %in% c(Lung_after,Lung_before)) %>%
  select(subject_id, cohort_start_date) %>%
  rename("person_id" = "subject_id", "index_date" = "cohort_start_date" ) %>%
  compute()


# ======================== BASELINE CHARACTERISTICS=========================== #

## 101a. AGE AT INDEX DATE IN Lung CANCER COHORTS ----------------------------
age_patients <- individuals_id_for_age %>%
  left_join(person_db) %>%
  select(person_id,year_of_birth,index_date) %>%
  collect() %>%
  mutate(month_of_birth = 1) %>%
  mutate(day_of_birth   = 1) %>%
  mutate(dob = as.Date(dmy(paste(day_of_birth,month_of_birth,year_of_birth,sep="-")))) %>%
  mutate(age = floor(as.numeric(difftime(index_date,dob,unit="days"))/365.25)) 

# 101b. AGE GROUP AT INDEX DATE IN Lung CANCER COHORT 

age_group <- age_patients %>%
    mutate(age_grouping = cut(age, c(0,10,20,30,40,50,60,70,80,90,100,110),labels = c("0 to 9 years", "10 to 19 years","20 to 29 years","30 to 39 years","40 to 49 years","50 to 59 years","60 to 69 years","70 to 79 years","80 to 89 years","90 to 99 years","100+ years"),include.lowest = TRUE, right = FALSE, is.na = FALSE)) %>%
    mutate(agegid = as.numeric(age_grouping)) 
 
# -------- SMD OF AGE AT INDEX DATE BEFORE AND AFTER LOCKDOWN -----------------#

age_1 <- age_patients %>%
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==5) %>%
  filter(index_date==cohort_start_date) %>%
  collect() 

mean_age_1 <-  mean(age_1$age) %>% print()
var_age_1 <- var(age_1$age) %>% print()

age_2 <- age_patients %>%
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==6) %>%
  filter(index_date==cohort_start_date) %>%
  collect() 

mean_age_2 <-  mean(age_2$age) %>% print()
var_age_2 <- var(age_2$age) %>% print()

  
age_table1  <- rbind(mean_age_1,var_age_1,mean_age_2,var_age_2) %>% as.data.frame() %>% dplyr::mutate_if(is.numeric, round, digits = 2)

age_table_SMD  <- tibble(mean_age_1 = t(age_table1[1,])[,1], var_age_1 = t(age_table1[2,])[,1], mean_age_2 = t(age_table1[3,])[,1], var_age_2 = t(age_table1[4,])[,1]) %>%
  mutate(smd = abs(mean_age_1-mean_age_2)/sqrt(var_age_1+var_age_2)) %>% print()


age_table_formatted <- age_table_SMD %>% dplyr::mutate_if(is.numeric, round, digits = 2) %>%  
                       dplyr::mutate("Lung Cancer Before Lockdown" = glue("{mean_age_2} ({var_age_2})")) %>%
                       dplyr::mutate("Lung Cancer After Lockdown" = glue("{mean_age_1} ({var_age_1})")) %>% 
                       rename( "Standardised Mean Difference" = "smd")
age_table_formatted <- age_table_formatted[-c(1:4)] #  remove superfluous columns
age_table_formatted <- age_table_formatted[, c(3, 2, 1)] # reorder the columns
age_table_formatted

Pretty_mean_age_table <- flextable(age_table_formatted) %>% theme_vanilla() %>% 
  set_caption(caption = "Table 1b. Mean (variance) of age at date of Lung cancer diagnosis before and after lockdown") %>% 
  width(width = 1.4)  %>% print()


# ----------------------------- FOR TABLE 1 ------------------------------------
# FREQUENCIES OF AGES AT INDEX DATE FOR Lung PATIENTS DIAGNOSED AFTER LOCKDOWN ------
age_table_1 <- age_group %>% 
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==5) %>%
  filter(index_date==cohort_start_date) %>%
  group_by(age_grouping) %>%
  tally() %>% 
  print()

# FREQUENCIES OF AGES AT INDEX DATE FOR Lung PATIENTS DIAGNOSED BEFORE LOCKDOWN -----
age_table_2 <- age_group %>% 
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==6) %>%
  filter(index_date==cohort_start_date) %>%
  group_by(age_grouping) %>%
  tally() %>% 
  print()

# Make table of age by cohort
# first need to make a data frame of all the possible ages so that categories with zero counts are shown
age_group_labels <- c("0 to 9 years", "10 to 19 years","20 to 29 years","30 to 39 years","40 to 49 years","50 to 59 years",
                      "60 to 69 years","70 to 79 years","80 to 89 years","90 to 99 years","100+ years") %>% as.matrix() 

age_group_labels <- as.data.frame(age_group_labels) %>% rename("age_grouping" = "V1") %>% select("age_grouping")

age_table_1 <- age_table_1 %>% rename("Lung Cancer after lockdown"="n")
age_table_2 <- age_table_2 %>% rename("Lung Cancer before lockdown"="n")

# get the age grouping tables and join them with the full rows of categories and replace NAs with 0
age_table_1 <- age_group_labels  %>% left_join(age_table_1) %>% replace(is.na(.), 0) %>% print()
age_table_2 <- age_group_labels  %>% left_join(age_table_2) %>% replace(is.na(.), 0) %>% print()

Age_table_both_lung_cohorts <- age_table_2 %>% left_join(age_table_1) %>% rename("Age Group" = "age_grouping") %>% print()

Pretty_age_group_table <- flextable(Age_table_both_lung_cohorts) %>% theme_vanilla() %>% 
  set_caption(caption = "Table 1a. Age at date of Lung cancer diagnosis before and after lockdown") %>% 
  width(width = 1.4)  %>% print()

print("Age done")


## 102a. GENDER Lung CANCER COHORT AFTER LOCKDOWN ----------------------------
gender_patients_1 <-  list_id %>% 
  left_join(person_db) %>%
  select(person_id,gender_concept_id) %>%
  rename("subject_id" ="person_id") %>%
  inner_join(cohorts_db, by = "subject_id") %>%
  filter(cohort_definition_id==5) %>%
  collect() %>%
  distinct() %>%
  mutate(value = if_else(gender_concept_id==8532,1,2)) %>%
  select(-gender_concept_id) %>%
  mutate(sex = if_else(value==1,"Female","Male")) %>%
  group_by(sex) %>%
  tally() %>%
  print()



## 102b. GENDER Lung CANCER COHORT BEFORE LOCKDOWN ----------------------------
gender_patients_2 <-  list_id %>% 
  left_join(person_db) %>%
  select(person_id,gender_concept_id) %>%
  rename("subject_id" ="person_id") %>%
  inner_join(cohorts_db, by = "subject_id") %>%
  filter(cohort_definition_id==6) %>%
  collect() %>%
  distinct() %>%
  mutate(value = if_else(gender_concept_id==8532,1,2)) %>%
  select(-gender_concept_id) %>%
  mutate(sex = if_else(value==1,"Female","Male")) %>%
  group_by(sex) %>%
  tally() %>%
  print()

gender_table_1 <- gender_patients_1 %>% rename("n_after_lockdown" = "n")
gender_table_2 <- gender_patients_2 %>% rename("n_before_lockdown" = "n")

gender_table <- gender_table_2 %>% left_join(gender_table_1) %>% replace(is.na(.), 0)

gender_table <- gender_table %>%
  mutate(n_before_lockdown = paste0(n_before_lockdown, " (", round(100*n_before_lockdown/sum(n_before_lockdown),1), "%)")) %>%
  mutate(n_after_lockdown = paste0(n_after_lockdown, " (", round(100*n_after_lockdown/sum(n_after_lockdown),1), "%)")) 
  
Pretty_gender_table <- flextable(gender_table) %>%
  set_caption(caption = "Table 1c. Gender of Lung cancer patients in groups before and after lockdown") %>% 
  width(width = 1.4)  %>% print()

print("Gender done")


## 103a. SMOKING HISTORY IN Lung CANCER COHORT AFTER LOCKDOWN ----------------

Smoker <- cdm$observation %>% 
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db, by = "subject_id") %>% 
  filter(cohort_definition_id ==1) %>%
  inner_join(cdm$concept_ancestor %>% 
               filter(ancestor_concept_id == 4298794) %>%
               select("observation_concept_id" = "descendant_concept_id")) %>%
  distinct() %>%
  collect() %>%
  print()


Never_smoked <- cdm$observation %>% 
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db, by = "subject_id") %>% 
  filter(cohort_definition_id ==1) %>%
  inner_join(cdm$concept_ancestor %>% 
               filter(ancestor_concept_id == 4144272) %>%
               select("observation_concept_id" = "descendant_concept_id")) %>%
  distinct() %>%
  collect() %>%
  print()



# now need this within the past year from cohort_start_date

## 103b. SMOKING HISTORY IN Lung CANCER COHORT BEFORE LOCKDOWN ---------------



## 104a. ALCOHOL HISTORY IN Lung CANCER COHORT AFTER LOCKDOWN ----------------



## 103b. ALCOHOL HISTORY IN Lung CANCER COHORT BEFORE LOCKDOWN ---------------



# ======================== COVARIATES OF INTEREST ============================ #

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

save(list = c("VI_patients","VI_id"), file = "~/R/CancerCovid/Custom Characterisations/Lung/Lung_covariates/Visits.RData")

print("Visits done")




## 2. FAST TRACK REFERRAL FOR SUSPECTED LUNG CANCER ------------------------------------------------
FTRSL_patients <- get_observations(44791283, 2)
FTRSL_id <- get_observations_id(44791283, 2, "Fast track referral for lung cancer")

AnalysisRef  <- rbind(AnalysisRef,c(2,"Fast track referral for lung cancer"))

save(list = c("FTRSL_patients","FTRSL_id"), file = "~/R/CancerCovid/Custom Characterisations/Lung/Lung_covariates/FastTrackReferralSuspectedLung.RData")

print("Fast track referral for lung cancer done")


## 3. BRONCHOSCOPY  ------------------------------------------------------------

BRONC_patients <- get_procedures(4032404, 3)
BRONC_id <- get_procedures_id(4032404, 3, "Bronchoscopy")

AnalysisRef  <- rbind(AnalysisRef,c(3,"Bronchoscopy"))

save(list = c("BRONC_patients","BRONC_id"), file = "~/R/CancerCovid/Custom Characterisations/Lung/Lung_covariates/Bronchoscopy.RData")

print("Bronchoscopy done")




## 4. ENDOBRONCHIAL ULTRASONOGRAPHY GUIDED TRANSBRONCHIAL NEEDLE ASPIRATION  -------
EUTNA_patients <- get_procedures(44809038, 4)
EUTNA_id <- get_procedures_id(44809038, 4, "Endobronchial ultrasonography guided transbronchial needle aspiration")

AnalysisRef  <- rbind(AnalysisRef,c(4,"Endobronchial ultrasonography guided transbronchial needle aspiration"))

save(list = c("EUTNA_patients","EUTNA_id"), file = "~/R/CancerCovid/Custom Characterisations/Lung/Lung_covariates/EUTNA.RData")

print("Endobronchial ultrasonography guided transbronchial needle aspiration done")




## 5. MEDIASTINOSCOPY ----------------------------------------------------
MEDIA_patients <- get_procedures(4128302, 5)
MEDIA_id <- get_procedures_id(4128302, 5, "Mediastinoscopy")

AnalysisRef  <- rbind(AnalysisRef,c(5,"Mediastinoscopy"))

save(list = c("MEDIA_patients","MEDIA_id"), file = "~/R/CancerCovid/Custom Characterisations/Lung/Lung_covariates/Mediastinoscopy.RData")

print("Mediastinoscopy done")



## 6. CT AND BIOPSY OF CHEST ----------------------------------------------------
CTB_patients <- get_procedures(4304406, 6)
CTB_id <- get_procedures_id(4304406, 6, "CT and biopsy of chest")

AnalysisRef  <- rbind(AnalysisRef,c(6,"CT and biopsy of chest"))

save(list = c("CTB_patients","CTB_id"), file = "~/R/CancerCovid/Custom Characterisations/Lung/Lung_covariates/CT_biopsy_chest.RData")

print("CT and biopsy of chest done")



## 7. ULTRASOUND AND BIOPSY OF CHEST ----------------------------------------------------
USBC_patients <- get_procedures(4167553, 7)
USBC_id <- get_procedures_id(4167553, 7, "US scan and biopsy of chest")

AnalysisRef  <- rbind(AnalysisRef,c(7,"US scan and biopsy of chest"))

save(list = c("USBC_patients","USBC_id"), file = "~/R/CancerCovid/Custom Characterisations/Lung/Lung_covariates/US_biopsy_chest.RData")

print("US scan and biopsy of chest done")


## 8. DIAGNOSTIC RADIOLOGY PROCEDURES OF CHEST------------------------------------------------
DRP_patients <- get_procedures(45889178, 8)
DRP_id <- get_procedures_id(45889178, 8, "Diagnostic Radiology (Diagnostic Imaging) Procedures of the Chest")

AnalysisRef  <- rbind(AnalysisRef,c(8,"Diagnostic Radiology (Diagnostic Imaging) Procedures of the Chest"))

save(list = c("DRP_patients","DRP_id"), file = "~/R/CancerCovid/Custom Characterisations/Lung/Lung_covariates/DiagnosticProceduresChest.RData")

print("Diagnostic Radiology (Diagnostic Imaging) Procedures of the Chest done")


## 9. MRI OF CHEST ------------------------------------------------
MRI_patients <- get_procedures(4246485, 9)
MRI_id <- get_procedures_id(4246485, 9, "MRI of chest")

AnalysisRef  <- rbind(AnalysisRef,c(9,"MRI of chest"))

save(list = c("MRI_patients","MRI_id"), file = "~/R/CancerCovid/Custom Characterisations/Lung/Lung_covariates/MRIchest.RData")

print("MRI of chest done")


# ========================= INDIVIDUAL TABLES================================= # 
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


# Join the tables
continuous_table <- VI_table %>% union_all(FTRSL_table) %>% union_all(BRONC_table) %>% union_all(EUTNA_table) %>% union_all(MEDIA_table) %>%
  union_all(CTB_table) %>% union_all(USBC_table) %>% union_all(DRP_table) %>% union_all(MRI_table) %>% ungroup()

# Pivot the continuous table around, and rename person_id as subject_id. This
# is later used to run the SMD function
Continuous_table_pivot <- continuous_table %>% right_join(lung_covariate_names) %>%
  select(person_id, covariate, value) %>% 
  rename("subject_id" = "person_id") %>%
  tidyr::pivot_wider(names_from = covariate, values_from = value,values_fill = 0) 

continuous_table <- Continuous_table_pivot %>% tidyr::pivot_longer(2:28, names_to = "covariate", values_to = "value") 

# read all the covariate names from the 'forALLCharacterisations_with_functions.R
namt <- t(lung_covariate_names)

save(list = c("VI_table", "FTRSL_table", "BRONC_table", "EUTNA_table", "MEDIA_table", "CTB_table", "USBC_table", "DRP_table", "MRI_table", 
              "continuous_table", "Continuous_table_pivot", "namt"), file = "~/R/CancerCovid/Custom Characterisations/Lung/LungIndividualTabs.Rdata")


# =================== AGGREGATED COUNTS OF COVARIATES ======================== # 


# All tables joined together
# Cohort 1 - Lung cancer after lockdown
All_tables_counts1 <- continuous_table %>%  
  inner_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==5) %>%
  group_by(covariate) %>% 
  tally(value) %>% 
    print()



# cohort 2 - Lung cancer before lockdown
All_tables_counts2 <- continuous_table %>%  
  inner_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==6) %>%
  group_by(covariate) %>% 
  tally(value) %>% 
    print()

All_tables_counts1 <- All_tables_counts1 %>% rename("n after lockdown" = "n")  %>% rename("Covariate" = "covariate")
All_tables_counts2 <- All_tables_counts2 %>% rename("n before lockdown" = "n")  %>% rename("Covariate" = "covariate")

All_count_joined <- All_tables_counts2 %>% inner_join(All_tables_counts1) %>% print()


Pretty_counts_table <- flextable(All_count_joined) %>% set_caption(caption = "Table 2. Frequencies of visits, lung cancer-related observations and procedures during different time periods before and after lockdown") 
Pretty_counts_table

# =============================== (SMD) ====================================== #

# Get all person level tables together and filter by cohort_definition_id_1
All_tables_cohort_1 <- individuals_id %>% select(person_id) %>%
  rename("subject_id"="person_id") %>%
  inner_join(continuous_table) %>%
  select(subject_id, covariate, value) %>%
    inner_join(cohorts_db_df, by = "subject_id") %>%
  distinct() %>%
  filter(cohort_definition_id==5)
 
# Pivot the table so that all the covariates which were rows in the above code are now column headings
All_tables_cohort_1 <- All_tables_cohort_1 %>% select(subject_id, covariate, value, cohort_definition_id, cohort_start_date, cohort_end_date) %>% 
  tidyr::pivot_wider(names_from = covariate, values_from = value,values_fill = 0) # if this throws an error it's because there are duplicate records in the data somewhere. Look for it!


# Get all person level tables together and filter by cohort_definition_id_2
All_tables_cohort_2 <- individuals_id %>% select(person_id) %>%
  rename("subject_id"="person_id") %>%
  inner_join(continuous_table) %>%
  select(subject_id, covariate, value) %>%
  inner_join(cohorts_db_df, by = "subject_id") %>%
  distinct() %>%
  filter(cohort_definition_id==6)

# Pivot the table so that all the covariates which were rows in the above code are now column headings
All_tables_cohort_2 <- All_tables_cohort_2 %>% select(subject_id, covariate, value, cohort_definition_id, cohort_start_date, cohort_end_date) %>% 
  tidyr::pivot_wider(names_from = covariate, values_from = value,values_fill = 0)


# Run SMD function to create table of all
All_SMD <- compute_continuous_smd(All_tables_cohort_1, All_tables_cohort_2) 

All_SMD <- All_SMD %>% rename("mean after lockdown" = "mean1") %>% rename("var after lockdown" = "var1") %>% 
  rename("mean before lockdown" = "mean2") %>% rename("var before lockdown" = "var2") %>%
  rename("Covariate" = "covariate") 
All_SMD <- All_SMD[,c(1,4,5,2,3,6)]

Pretty_SMD_table <- flextable(All_SMD) %>% set_caption(caption = "Table 3. Mean(var) frequencies of visits, lung cancer-related observations and procedures during different time periods before and after lockdown") 

Pretty_SMD_table
## ========================= Save all tables ================================ ##

save(list = c("All_tables_counts1", "All_tables_counts2", "All_count_joined", "Pretty_counts_table", "All_tables_cohort_1", "All_tables_cohort_2",
              "All_SMD", "Pretty_gender_table", "Pretty_SMD_table"), file = "~/R/CancerCovid/Custom Characterisations/Lung/CountsSMDTabs.Rdata")

write.csv(age_table_formatted, "~/R/CancerCovid/Custom Characterisations/Lung/age_table_formatted.csv", row.names = FALSE)
write.csv(Age_table_both_lung_cohorts, "~/R/CancerCovid/Custom Characterisations/Lung/Age_table_both_breast_cohorts.csv", row.names = FALSE)
write.csv(gender_table, "~/R/CancerCovid/Custom Characterisations/Lung/gender_table.csv", row.names = FALSE)
write.csv(All_count_joined, "~/R/CancerCovid/Custom Characterisations/Lung/All_count_joined.csv", row.names = FALSE)
write.csv(All_SMD, "~/R/CancerCovid/Custom Characterisations/Lung/All_SMD.csv", row.names = FALSE)
