# ============================================================================ #
#                         Custom Characterisations                             #
#                             Colorectal Cancer                                #
#                              Nicola Barclay                                  #
#                                12-12-2022                                    #
# ============================================================================ #


print(paste0("- 3. COLORECTAL CANCER CUSTOM CHARACTERISATIONS"))
info(logger, "- 3. COLORECTAL CANCER CUSTOM CHARACTERISATIONS")

print(paste0("- Set up for colorectal cancer characterisations"))
info(logger, "- Set up for colorectal cancer characterisations")

## ------------------------------- VARIABLES -------------------------------- ##
# Cohorts_ID
Colorectal_after   <- 3
Colorectal_before  <- 4



# List of individuals 
individuals_id <- cohorts_db %>% 
  filter(cohort_definition_id %in% c(Colorectal_after,Colorectal_before)) %>%
  select(subject_id, cohort_start_date) %>%
  rename("person_id" = "subject_id", "index_date" = "cohort_start_date" ) %>%
  compute()
list_id <- individuals_id %>%
  select(person_id) %>%
  compute()
individuals_id <- individuals_id %>% collect()

# uncollect individuals_id to calculate index_date for age below
individuals_id_for_age <- cohorts_db %>% 
  filter(cohort_definition_id %in% c(Colorectal_after,Colorectal_before)) %>%
  select(subject_id, cohort_start_date) %>%
  rename("person_id" = "subject_id", "index_date" = "cohort_start_date" ) %>%
  compute()

print(paste0("- Set up for colorectal cancer characterisations done"))
info(logger, "- Set up for colorectal cancer characterisations done")


# ======================== BASELINE CHARACTERISTICS=========================== #

print(paste0("- Running baseline characteristics - age and sex"))
info(logger, "- Running baseline characteristics - age and sex")


## AGE AT INDEX DATE IN COLORECTAL CANCER COHORTS ----------------------------
age_patients <- individuals_id_for_age %>%
  left_join(person_db) %>%
  select(person_id,year_of_birth,index_date) %>%
  collect() %>%
  mutate(month_of_birth = 1) %>%
  mutate(day_of_birth   = 1) %>%
  mutate(dob = as.Date(dmy(paste(day_of_birth,month_of_birth,year_of_birth,sep="-")))) %>%
  mutate(age = floor(as.numeric(difftime(index_date,dob,unit="days"))/365.25)) 

# AGE GROUP AT INDEX DATE IN COLORECTAL CANCER COHORT 

age_group <- age_patients %>%
    mutate(age_grouping = cut(age, c(0,10,20,30,40,50,60,70,80,90,100,110),
            labels = c("0 to 9 years", "10 to 19 years","20 to 29 years","30 to 39 years","40 to 49 years","50 to 59 years","60 to 69 years","70 to 79 years","80 to 89 years","90 to 99 years","100+ years"),include.lowest = TRUE, right = FALSE, is.na = FALSE)) %>%
    mutate(agegid = as.numeric(age_grouping)) 
 
# -------- SMD OF AGE AT INDEX DATE BEFORE AND AFTER LOCKDOWN -----------------#

age_1 <- age_patients %>%
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==3) %>%
  filter(index_date==cohort_start_date) %>%
  collect() 

mean_age_1 <-  mean(age_1$age) %>% print()
var_age_1 <- var(age_1$age) %>% print()

age_2 <- age_patients %>%
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==4) %>%
  filter(index_date==cohort_start_date) %>%
  collect() 

mean_age_2 <-  mean(age_2$age) %>% print()
var_age_2 <- var(age_2$age) %>% print()

  
age_table1  <- rbind(mean_age_1,var_age_1,mean_age_2,var_age_2) %>% as.data.frame() %>% dplyr::mutate_if(is.numeric, round, digits = 2)

age_table_SMD  <- tibble(mean_age_1 = t(age_table1[1,])[,1], var_age_1 = t(age_table1[2,])[,1], mean_age_2 = t(age_table1[3,])[,1], var_age_2 = t(age_table1[4,])[,1]) %>%
  mutate(smd = abs(mean_age_1-mean_age_2)/sqrt(var_age_1+var_age_2)) %>% print()


age_table_formatted <- age_table_SMD %>% dplyr::mutate_if(is.numeric, round, digits = 2) %>%  
                       dplyr::mutate("Colorectal Cancer Before Lockdown" = glue("{mean_age_2} ({var_age_2})")) %>%
                       dplyr::mutate("Colorectal Cancer After Lockdown" = glue("{mean_age_1} ({var_age_1})")) %>% 
                       rename( "Standardised Mean Difference" = "smd")
age_table_formatted <- age_table_formatted[-c(1:4)] #  remove superfluous columns
age_table_formatted <- age_table_formatted[, c(2, 3, 1)] # reorder the columns
age_table_formatted

Pretty_mean_age_table <- flextable(age_table_formatted) %>% theme_vanilla() %>% 
  set_caption(caption = "Mean (variance) of age at date of colorectal cancer diagnosis before and after lockdown") %>% 
  width(width = 1.4)  

# save the table as a csv file
write.csv(age_table_formatted, here("Results", db.name, "Colorectal", "age_table_formatted_colorectal.csv"), row.names = FALSE)


# save the table as pdf
analysis.name <- "Colorectal"
tablename <- paste0("mean_age_table", db.name, analysis.name, ".pdf")

save_as_image(Pretty_mean_age_table, here("Results", db.name , "Colorectal",tablename), 
              zoom=1, expand=100, webshot = "webshot")


# FREQUENCIES OF AGES AT INDEX DATE FOR COLORECTAL PATIENTS DIAGNOSED AFTER LOCKDOWN ------
age_table_1 <- age_group %>% 
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==3) %>%
  filter(index_date==cohort_start_date) %>%
  group_by(age_grouping) %>%
  tally() %>% 
  print()

# FREQUENCIES OF AGES AT INDEX DATE FOR COLORECTAL PATIENTS DIAGNOSED BEFORE LOCKDOWN -----
age_table_2 <- age_group %>% 
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==4) %>%
  filter(index_date==cohort_start_date) %>%
  group_by(age_grouping) %>%
  tally() %>% 
  print()

# Make table of age by cohort
# first need to make a data frame of all the possible ages so that categories with zero counts are shown
age_group_labels <- c("0 to 9 years", "10 to 19 years","20 to 29 years","30 to 39 years","40 to 49 years","50 to 59 years",
                      "60 to 69 years","70 to 79 years","80 to 89 years","90 to 99 years","100+ years") %>% as.matrix() 

age_group_labels <- as.data.frame(age_group_labels) %>% rename("age_grouping" = "V1") %>% select("age_grouping")

age_table_1 <- age_table_1 %>% rename("Colorectal Cancer after lockdown"="n")
age_table_2 <- age_table_2 %>% rename("Colorectal Cancer before lockdown"="n")

# get the age grouping tables and join them with the full rows of categories and replace NAs with 0
age_table_1 <- age_group_labels  %>% left_join(age_table_1) %>% replace(is.na(.), 0) %>% print()
age_table_2 <- age_group_labels  %>% left_join(age_table_2) %>% replace(is.na(.), 0) %>% print()

Age_table_both_colorectal_cohorts <- age_table_2 %>% left_join(age_table_1) %>% rename("Age Group" = "age_grouping") %>% print()

Pretty_age_group_table <- flextable(Age_table_both_colorectal_cohorts) %>% theme_vanilla() %>% 
  set_caption(caption = "Age at date of colorectal cancer diagnosis before and after lockdown") %>% 
  width(width = 1.4)


# save the table as a csv file
write.csv(Age_table_both_colorectal_cohorts, here("Results", db.name, "Colorectal", "Age_table_both_colorectal_cohorts.csv"), row.names = FALSE)

tablename <- paste0("age_group_table", db.name, analysis.name, ".pdf")

# save the table as pdf
save_as_image(Pretty_age_group_table, here("Results", db.name , "Colorectal",tablename), 
              zoom=1, expand=100, webshot = "webshot")

# save RData objects
save(Pretty_mean_age_table, Pretty_age_group_table, Age_table_both_colorectal_cohorts, age_table_formatted, file = here("Results", db.name, "Colorectal", "ColorectalAge.RData"))

print("Age done")

## GENDER COLORECTAL CANCER COHORT AFTER LOCKDOWN ----------------------------
gender_patients_1 <-  list_id %>% 
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



## GENDER COLORECTAL CANCER COHORT BEFORE LOCKDOWN ----------------------------
gender_patients_2 <-  list_id %>% 
  left_join(person_db) %>%
  select(person_id,gender_concept_id) %>%
  rename("subject_id" ="person_id") %>%
  inner_join(cohorts_db, by = "subject_id") %>%
  filter(cohort_definition_id==4) %>%
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
  set_caption(caption = "Gender of colorectal cancer patients in groups before and after lockdown") %>% 
  width(width = 1.4) 

# save the table as a csv file
write.csv(gender_table, here("Results", db.name, "Colorectal", "Gender_table_both_colorectal_cohorts.csv"), row.names = FALSE)



# create file name
tablename <- paste0("gender_table", db.name, analysis.name, ".pdf")

# save the table as pdf
save_as_image(Pretty_gender_table, here("Results", db.name , "Colorectal",tablename), 
              zoom=1, expand=100, webshot = "webshot")


# save RData objects
save(gender_table, Pretty_gender_table, file = here("Results", db.name, "Colorectal", "ColorectalGender.RData"))



print(paste0("- Baseline characteristics - age and sex done"))
info(logger, "- Baseline characteristics - age and sex done")


# ======================== COVARIATES OF INTEREST ============================ #

print(paste0("- Running colorectal cancer covariate counts"))
info(logger, "- Running colorectal cancer covariate counts")

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

save(list = c("VI_patients","VI_id"), file = here("Results", db.name, "Colorectal", "Colorectal_covariates", "Visits.RData"))

print("Visits done")




## 2. BOWEL CANCER SCREENING PROGRAMME   ------------------------------------------------
BCSP_patients <- cdm$measurement %>%
  select(person_id,measurement_concept_id, measurement_date) %>%
  filter(measurement_concept_id ==44791543) %>%
  inner_join(list_id) %>% 
  distinct() %>%
  collect() %>%
  rename("Event_date"="measurement_date") %>%
  mutate(FeatureExtractionId = 44791543002)

BCSP_id <- tibble(FeatureExtractionId = 44791543002,covariateId = 44791543, covariateName = "Bowel_cancer_screening_prog", AnalysisId = 2)

AnalysisRef  <- rbind(AnalysisRef,c(2,"Bowel_cancer_screening_prog"))

save(list = c("BCSP_patients","BCSP_id"), file = here("Results", db.name, "Colorectal", "Colorectal_covariates", "Bowel_cancer_screening_prog.RData"))

print("Bowel cancer screening programme done")



## 3. QUANTITATIVE FAECAL IMMUNOCHEMICAL TEST  -------------------------------------------

QFIT_patients <- cdm$measurement %>%
  select(person_id,measurement_concept_id, measurement_date) %>%
  filter(measurement_concept_id ==37395561) %>%
  inner_join(list_id) %>% 
  distinct() %>%
  collect() %>%
  rename("Event_date"="measurement_date") %>%
  mutate(FeatureExtractionId = 37395561003)

QFIT_id <- tibble(FeatureExtractionId = 37395561003,covariateId = 37395561, covariateName = "Quantitative_faecal_immunochemical_tests", AnalysisId = 3)


AnalysisRef  <- rbind(AnalysisRef,c(3,"Quantitative_faecal_immunochemical_tests"))

save(list = c("QFIT_patients","QFIT_id"), file = here("Results", db.name, "Colorectal", "Colorectal_covariates", "Quantitative_faecal_immunochemical_tests.RData"))


print("Quantitative faecal immunochemical tests done")



## 4. ULTRASONOGRAPHY OF INTESTINE -----------------------------------------------------
UOI_patients <- get_procedures(4082528, 4)
UOI_id <- get_procedures_id(4082528, 4, "Ultrasonography of intestine")

AnalysisRef  <- rbind(AnalysisRef,c(4,"Ultrasonography of intestine"))

save(list = c("UOI_patients","UOI_id"), file = here("Results", db.name, "Colorectal", "Colorectal_covariates", "Ultrasonography_intestine.RData"))

print("Ultrasonography of intestine done")




## 5. COLONOSCOPY ----------------------------------------------------
COLON_patients <- get_procedures(4249893, 5)
COLON_id <- get_procedures_id(4249893, 5, "Colonoscopies")

AnalysisRef  <- rbind(AnalysisRef,c(5,"Colonoscopies"))

save(list = c("COLON_patients","COLON_id"), file = here("Results", db.name, "Colorectal", "Colorectal_covariates", "Colonoscopies.RData"))

print("Colonoscopies done")



## 6. SIGMOIDOSCOPY ----------------------------------------------------
SIG_patients <- get_procedures(4087381, 6)
SIG_id <- get_procedures_id(4087381, 6, "Sigmoidoscopy")

AnalysisRef  <- rbind(AnalysisRef,c(6,"Sigmoidoscopy"))

save(list = c("SIG_patients","SIG_id"), file = here("Results", db.name, "Colorectal", "Colorectal_covariates", "Sigmoidoscopy.RData"))

print("Sigmoidoscopy done")



## 7. ULTRASOUND OF GASTROINTESTINAL TRACT----------------------------------------------------
USGI_patients <- get_procedures(4125529, 7)
USGI_id <- get_procedures_id(4125529, 7, "Ultrasound of gastrointestinal tract")

AnalysisRef  <- rbind(AnalysisRef,c(7,"Ultrasound of gastrointestinal tract"))

save(list = c("USGI_patients","USGI_id"), file = here("Results", db.name, "Colorectal", "Colorectal_covariates", "UltrasoundGastrointestinalTract.RData"))

print("Ultrasound of gastrointestinal tract done")


## 8. ULTRASONOGRAPHY OF ABDOMEN------------------------------------------------
USA_patients <- get_procedures(4261497, 8)
USA_id <- get_procedures_id(4261497, 8, "Ultrasonography of abdomen")

AnalysisRef  <- rbind(AnalysisRef,c(8,"Ultrasonography of abdomen"))

save(list = c("USA_patients","USA_id"), file = here("Results", db.name, "Colorectal", "Colorectal_covariates", "UltrasonographyAbdomen.RData"))

print("Ultrasonography of abdomen done")


## 9. ULTRASONOGRAPHY OF RECTUM ------------------------------------------------
USR_patients <- get_procedures(2787168, 9)
USR_id <- get_procedures_id(2787168, 9, "Ultrasonography of rectum")

AnalysisRef  <- rbind(AnalysisRef,c(9,"Ultrasonography of rectum"))

save(list = c("USR_patients","USR_id"), file = here("Results", db.name, "Colorectal", "Colorectal_covariates", "UltrasonographyRectum.RData"))

print("Ultrasonography of Rectum done")



## 10. SCREENING COLONOSCOPIES ------------------------------------------------
SC_patients <- cdm$measurement %>%
  select(person_id,measurement_concept_id, measurement_date) %>%
  filter(measurement_concept_id ==40480729) %>%
  inner_join(list_id) %>% 
  distinct() %>%
  collect() %>%
  rename("Event_date"="measurement_date") %>%
  mutate(FeatureExtractionId = 40480729010)

SC_id <- tibble(FeatureExtractionId = 40480729010,covariateId = 40480729, covariateName = "Screening Colonoscopies", AnalysisId = 10)


AnalysisRef  <- rbind(AnalysisRef,c(10,"Screening Colonoscopies"))

save(list = c("SC_patients","SC_id"), file = here("Results", db.name, "Colorectal", "Colorectal_covariates", "Screening Colonoscopies.RData"))


print("Screening Colonoscopies done")




## 11. ENDOSCOPIC ULTRASOUND OF UPPER GASTROINTESTINAL TRACT ------------------------------------------------
ENDO_GAS_patients <- get_procedures(4061134, 11)
ENDO_GAS_id <- get_procedures_id(4061134, 11, "Endoscopic ultrasound of upper gastrointestinal tract")

AnalysisRef  <- rbind(AnalysisRef,c(11,"Endoscopic ultrasound of upper gastrointestinal tract"))

save(list = c("ENDO_GAS_patients","ENDO_GAS_id"), file = here("Results", db.name, "Colorectal", "Colorectal_covariates", "Endoscopic ultrasound of upper gastrointestinal tract.RData"))

print("Endoscopic ultrasound of upper gastrointestinal tract done")



## 12. QUANTITATIVE FAECAL IMMUNOCHEMICAL TEST READ CODE ------------------------------------------------
QFIT_READ_patients <- cdm$measurement %>%
  select(person_id,measurement_concept_id, measurement_date) %>%
  filter(measurement_concept_id ==1397752) %>%
  inner_join(list_id) %>% 
  distinct() %>%
  collect() %>%
  rename("Event_date"="measurement_date") %>%
  mutate(FeatureExtractionId = 1397752012)

QFIT_READ_id <- tibble(FeatureExtractionId = 1397752012,covariateId = 1397752, covariateName = "Quantitative faecal immunochemical test-READ code", AnalysisId = 12)


AnalysisRef  <- rbind(AnalysisRef,c(12,"Quantitative faecal immunochemical test-READ code"))

save(list = c("QFIT_READ_patients","QFIT_READ_id"), file = here("Results", db.name, "Colorectal", "Colorectal_covariates", "Quantitative faecal immunochemical test-READ code.RData"))


print("Quantitative faecal immunochemical test-READ code done")


## 13. FAST TRACK REFERRAL FOR SUSPECTED COLORECTAL CANCER -----------------------------------------------------
FTRSCC_patients <- get_observations(44791274, 13)
FTRSCC_id <- get_procedures_id(44791274, 13, "Fast track referral for suspected colorectal cancer")

AnalysisRef  <- rbind(AnalysisRef,c(13,"Fast track referral for suspected colorectal cancer"))

save(list = c("FTRSCC_patients","FTRSCC_id"), file = here("Results", db.name, "Colorectal", "Colorectal_covariates", "FastTrackReferralColorectalCancer.RData"))

print("Fast track referral for suspected colorectal cancer done")


print(paste0("- Colorectal cancer covariate counts done"))
info(logger, "- Colorectal cancer covariate counts done")




# ========================= INDIVIDUAL TABLES================================= # 
print(paste0("- Getting colorectal cancer individual covariate tables"))
info(logger, "- Getting colorectal cancer individual covariate tables")

# Get tables: person: id, covariate, value
VI_table        <- getIndividualTabs(VI_id, VI_patients, individuals_id, 3, FALSE)
BCSP_table        <- getIndividualTabs(BCSP_id, BCSP_patients, individuals_id, 3, FALSE)
QFIT_table        <- getIndividualTabs(QFIT_id, QFIT_patients, individuals_id, 3, FALSE)
UOI_table      <- getIndividualTabs(UOI_id, UOI_patients, individuals_id, 3, FALSE)
COLON_table        <- getIndividualTabs(COLON_id, COLON_patients, individuals_id, 3, FALSE)
SIG_table        <- getIndividualTabs(SIG_id, SIG_patients, individuals_id, 3, FALSE)
USGI_table        <- getIndividualTabs(USGI_id, USGI_patients, individuals_id,  3, FALSE)
USA_table        <- getIndividualTabs(USA_id, USA_patients, individuals_id, 3, FALSE)
USR_table        <- getIndividualTabs(USR_id, USR_patients, individuals_id, 3, FALSE)
SC_table        <- getIndividualTabs(SC_id, SC_patients, individuals_id, 3, FALSE)
ENDO_GAS_table        <- getIndividualTabs(ENDO_GAS_id, ENDO_GAS_patients, individuals_id, 3, FALSE)
QFIT_READ_table        <- getIndividualTabs(QFIT_READ_id, QFIT_READ_patients, individuals_id, 3, FALSE)
FTRSCC_table        <- getIndividualTabs(FTRSCC_id, FTRSCC_patients, individuals_id, 3, FALSE)


# Join the tables
continuous_table <- VI_table %>% union_all(BCSP_table) %>% union_all(QFIT_table) %>% union_all(UOI_table) %>% union_all(COLON_table) %>%
  union_all(SIG_table) %>% union_all(USGI_table) %>% union_all(USA_table) %>% union_all(USR_table) %>% union_all(SC_table) %>% 
  union_all(ENDO_GAS_table) %>% union_all(QFIT_READ_table) %>% union_all(FTRSCC_table) %>% ungroup()

# Pivot the continuous table around, and rename person_id as subject_id. This
# is later used to run the SMD function
Continuous_table_pivot <- continuous_table %>% right_join(colorectal_covariate_names) %>%
  select(person_id, covariate, value) %>% 
  rename("subject_id" = "person_id") %>%
  tidyr::pivot_wider(names_from = covariate, values_from = value,values_fill = 0) 

continuous_table <- Continuous_table_pivot %>% tidyr::pivot_longer(2:40, names_to = "covariate", values_to = "value") 

# read all the covariate names from the 'forAllCharacterisations_with_functions.R
namt <- t(colorectal_covariate_names)

save(list = c("VI_table", "BCSP_table", "QFIT_table", "UOI_table", "COLON_table", "SIG_table", "USGI_table", "USA_table", "USR_table", "SC_table", "ENDO_GAS_table", 
              "QFIT_READ_table", "FTRSCC_table", "continuous_table", "Continuous_table_pivot", "namt"), file = here("Results", db.name, "Colorectal", "Colorectal_covariates", "ColorectalIndividualTabs.Rdata"))

print(paste0("- Got colorectal cancer individual covariate tables"))
info(logger, "- Got colorectal cancer individual covariate tables")


# =================== AGGREGATED COUNTS OF COVARIATES ======================== # 

print(paste0("- Getting aggregated counts of colorectal cancer covariate tables"))
info(logger, "- Getting aggregated counts of colorectal cancer covariate tables")


# All tables joined together
# Cohort 1 - colorectal cancer after lockdown
All_tables_counts1 <- continuous_table %>%  
    inner_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==3) %>%
  group_by(covariate) %>% 
  tally(value) %>% 
    print()



# cohort 2 - colorectal cancer before lockdown
All_tables_counts2 <- continuous_table %>%  
  inner_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==4) %>%
  group_by(covariate) %>% 
  tally(value) %>% 
    print()

All_tables_counts1 <- All_tables_counts1 %>% rename("n after lockdown" = "n") %>% rename("Covariate" = "covariate")
All_tables_counts2 <- All_tables_counts2 %>% rename("n before lockdown" = "n") %>% rename("Covariate" = "covariate")

All_count_joined <- All_tables_counts2 %>% inner_join(All_tables_counts1) %>% print()

Pretty_counts_table <- flextable(All_count_joined)%>% set_caption(caption = 
              "Frequencies of visits, colorectal cancer-related observations and procedures during different time periods before and after lockdown") 
Pretty_counts_table

tablename <- paste0("All_covariate_counts", db.name, analysis.name, ".pdf")

# save the table as pdf
save_as_image(Pretty_counts_table, here("Results", db.name , "Colorectal",tablename), 
              zoom=1, expand=100, webshot = "webshot")

print(paste0("- Got aggregated counts of colorectal cancer covariate tables"))
info(logger, "- Got aggregated counts of colorectal cancer covariate tables")



# =============================== (SMD) ====================================== #

print(paste0("- Getting SMD of colorectal cancer covariate tables"))
info(logger, "- Getting SMD of colorectal cancer covariate tables")

# Get all person level tables together and filter by cohort_definition_id_1
All_tables_cohort_1 <- individuals_id %>% select(person_id) %>%
  rename("subject_id"="person_id") %>%
  left_join(continuous_table) %>%
  select(subject_id, covariate, value) %>%
  inner_join(cohorts_db_df, by = "subject_id") %>%
  distinct() %>%
  filter(cohort_definition_id==3)
 
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
  filter(cohort_definition_id==4)

# Pivot the table so that all the covariates which were rows in the above code are now column headings
All_tables_cohort_2 <- All_tables_cohort_2 %>% select(subject_id, covariate, value, cohort_definition_id, cohort_start_date, cohort_end_date) %>% 
  tidyr::pivot_wider(names_from = covariate, values_from = value,values_fill = 0)

# Run SMD function to create table of all
All_SMD <- compute_continuous_smd(All_tables_cohort_1, All_tables_cohort_2) 

All_SMD <- All_SMD %>% rename("mean after lockdown" = "mean1") %>% rename("var after lockdown" = "var1") %>% 
  rename("mean before lockdown" = "mean2") %>% rename("var before lockdown" = "var2") %>%
  rename("Covariate" = "covariate") 
All_SMD <- All_SMD[,c(1,4,5,2,3,6)]

Pretty_SMD_table <- flextable(All_SMD) %>% set_caption(caption = "Mean(var) frequencies of visits, colorectal cancer-related observations and procedures during different time periods before and after lockdown") 
Pretty_SMD_table

tablename <- paste0("All_covariate_SMD", db.name, analysis.name, ".pdf")

# save the table as pdf
save_as_image(Pretty_SMD_table, here("Results", db.name , "Colorectal",tablename), 
              zoom=1, expand=100, webshot = "webshot")

## ========================= Save all tables ================================ ##

save(list = c("All_tables_counts1", "All_tables_counts2", "All_count_joined", "Pretty_counts_table", "All_tables_cohort_1", "All_tables_cohort_2",
              "All_SMD", "Pretty_gender_table", "Pretty_SMD_table"), file = here("Results", db.name, "Colorectal", "Colorectal_covariates", "CountsSMDTabs.Rdata"))

write.csv(All_count_joined, here("Results", db.name, "Colorectal", "All_count_joined_colorectal.csv"), row.names = FALSE)
write.csv(All_SMD, here("Results", db.name, "Colorectal", "All_SMD_colorectal.csv"), row.names = FALSE)


print(paste0("- Got SMD of colorectal cancer covariate tables"))
info(logger, "- Got SMD of colorectal cancer covariate tables")


print(paste0("- 3. COLORECTAL CANCER CUSTOM CHARACTERISATIONS DONE"))
info(logger, "- 3. COLORECTAL CANCER CUSTOM CHARACTERISATIONS DONE")
