# ============================================================================ #
#                         Custom Characterisations                             #
#                               Breast Cancer                                  #
#                              Nicola Barclay                                  #
#                                8-12-2022                                     #
# ============================================================================ #

print(paste0("- 2. BREAST CANCER CUSTOM CHARACTERISATIONS"))
info(logger, "- 2. BREAST CANCER CUSTOM CHARACTERISATIONS")

print(paste0("- Set up for breast cancer characterisations"))
info(logger, "- Set up for breast cancer characterisations")

## ---------- VARIABLES OF INTEREST BEFORE AND AFTER LOCKDOWN --------------- ##

# Read the cancer cohort table name
cohorts_db        <- cdm[[outcome_table_name_2]]
cohorts_db_df <- as.data.frame(cohorts_db)

# Cohorts_ID
Breast_after       <- 1
Breast_before      <- 2


# List of individuals 
individuals_id <- cohorts_db %>% 
  filter(cohort_definition_id %in% c(Breast_after,Breast_before)) %>%
  select(subject_id, cohort_start_date) %>%
  rename("person_id" = "subject_id", "index_date" = "cohort_start_date" ) %>%
  compute()
list_id <- individuals_id %>%
  select(person_id) %>%
  compute()
individuals_id <- individuals_id %>% collect()

# uncollect individuals_id to calculate index_date for age below
individuals_id_for_age <- cohorts_db %>% 
  filter(cohort_definition_id %in% c(Breast_after,Breast_before)) %>%
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
 
# -------- SMD OF AGE AT INDEX DATE BEFORE AND AFTER LOCKDOWN -----------------#

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

  
age_table1  <- rbind(mean_age_1,var_age_1,mean_age_2,var_age_2) %>% as.data.frame() %>% dplyr::mutate_if(is.numeric, round, digits = 2)

age_table_SMD  <- tibble(mean_age_1 = t(age_table1[1,])[,1], var_age_1 = t(age_table1[2,])[,1], mean_age_2 = t(age_table1[3,])[,1], var_age_2 = t(age_table1[4,])[,1]) %>%
  mutate(smd = abs(mean_age_1-mean_age_2)/sqrt(var_age_1+var_age_2)) %>% print()


age_table_formatted <- age_table_SMD %>% dplyr::mutate_if(is.numeric, round, digits = 2) %>%  
                       dplyr::mutate("Breast Cancer Before Lockdown" = glue("{mean_age_2} ({var_age_2})")) %>%
                       dplyr::mutate("Breast Cancer After Lockdown" = glue("{mean_age_1} ({var_age_1})")) %>% 
                       rename( "Standardised Mean Difference" = "smd")
age_table_formatted <- age_table_formatted[-c(1:4)] #  remove superfluous columns
age_table_formatted <- age_table_formatted[, c(3, 2, 1)] # reorder the columns
age_table_formatted

Pretty_mean_age_table <- flextable(age_table_formatted) %>% theme_vanilla() %>% 
  set_caption(caption = "Mean (variance) of age at date of breast cancer diagnosis before and after lockdown") %>% 
  width(width = 1.4)  %>% print()


# up to here =================================



# save the table as a csv file
write.csv(age_table_formatted, here("Results", db.name, "Breast", "age_table_formatted.csv"), row.names = FALSE)

# write.csv(age_table_formatted, "~/R/CancerCovid/Custom Characterisations/Results/Breast/age_table_formatted.csv", row.names = FALSE)


# save the table as pdf
analysis.name <- "Breast"
tablename <- paste0("mean_age_table", db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"Breast",plotname),
    width = 8, height = 8)
print(Pretty_mean_age_table, newpage = FALSE)
dev.off()




# FREQUENCIES OF AGES AT INDEX DATE FOR PATIENTS DIAGNOSED AFTER LOCKDOWN ------
age_table_1 <- age_group %>% 
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==1) %>%
  filter(index_date==cohort_start_date) %>%
  group_by(age_grouping) %>%
  tally() %>% 
  print()

# FREQUENCIES OF AGES AT INDEX DATE FOR PATIENTS DIAGNOSED BEFORE LOCKDOWN -----
age_table_2 <- age_group %>% 
  rename("subject_id"="person_id") %>% 
  left_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==2) %>%
  filter(index_date==cohort_start_date) %>%
  group_by(age_grouping) %>%
  tally() %>% 
  print()

# Make table of age by cohort
# first need to make a data frame of all the possible ages so that categories with zero counts are shown
age_group_labels <- c("0 to 9 years", "10 to 19 years","20 to 29 years","30 to 39 years","40 to 49 years","50 to 59 years",
                      "60 to 69 years","70 to 79 years","80 to 89 years","90 to 99 years","100+ years") %>% as.matrix() 

age_group_labels <- as.data.frame(age_group_labels) %>% rename("age_grouping" = "V1") %>% select("age_grouping")

age_table_1 <- age_table_1 %>% rename("Breast Cancer after lockdown"="n")
age_table_2 <- age_table_2 %>% rename("Breast Cancer before lockdown"="n")

# get the age grouping tables and join them with the full rows of categories and replace NAs with 0
age_table_1 <- age_group_labels  %>% left_join(age_table_1) %>% replace(is.na(.), 0) %>% print()
age_table_2 <- age_group_labels  %>% left_join(age_table_2) %>% replace(is.na(.), 0) %>% print()

Age_table_both_breast_cohorts <- age_table_2 %>% left_join(age_table_1) %>% rename("Age Group" = "age_grouping") %>% print()

Pretty_age_group_table <- flextable(Age_table_both_breast_cohorts) %>% theme_vanilla() %>% 
  set_caption(caption = "Age at date of breast cancer diagnosis before and after lockdown") %>% 
  width(width = 1.4)  %>% print()

print("Age done")

write.csv(Age_table_both_breast_cohorts, "~/R/CancerCovid/Custom Characterisations/Breast/Age_table_both_breast_cohorts.csv", row.names = FALSE)


save(Pretty_mean_age_table, Pretty_age_group_table, Age_table_both_breast_cohorts,  per_prev_yrs, file = here("Results", db.name, "1_Cancers", "prev.RData"))



# save the plot as pdf
plotname <- paste0("inc_yrs",db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"1_Cancers",plotname),
    width = 10, height = 8)
print(inc_yrs_plot, newpage = FALSE)
dev.off()



# GENDER BREAST CANCER COHORT AFTER LOCKDOWN ----------------------------
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



# GENDER BREAST CANCER COHORT BEFORE LOCKDOWN ----------------------------
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

gender_table_1 <- gender_patients_1 %>% rename("n_after_lockdown" = "n")
gender_table_2 <- gender_patients_2 %>% rename("n_before_lockdown" = "n")

gender_table <- gender_table_2 %>% left_join(gender_table_1) %>% replace(is.na(.), 0)

gender_table <- gender_table %>%
  mutate(n_before_lockdown = paste0(n_before_lockdown, " (", round(100*n_before_lockdown/sum(n_before_lockdown),1), "%)")) %>%
  mutate(n_after_lockdown = paste0(n_after_lockdown, " (", round(100*n_after_lockdown/sum(n_after_lockdown),1), "%)")) 
  
Pretty_gender_table <- flextable(gender_table) %>%
  set_caption(caption = "Table 1c. Gender of breast cancer patients in groups before and after lockdown") %>% 
  width(width = 1.4)  %>% print()

write.csv(gender_table, "~/R/CancerCovid/Custom Characterisations/Breast/gender_table.csv", row.names = FALSE)

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

save(list = c("VI_patients","VI_id"), file = here("Results", db.name, "1_Breast", "Breast_covariates", Visits.RData))
     
print("Visits done")




## 2. REFERRAL TO BREAST CLINIC ------------------------------------------------
RBC_patients <- get_observations(4197459, 2)
RBC_id <- get_observations_id(4197459, 2, "Referral_to_breast_clinic")

AnalysisRef  <- rbind(AnalysisRef,c(2,"Referral_to_breast_clinic"))

save(list = c("RBC_patients","RBC_id"), file = "~/R/CancerCovid/Custom Characterisations/Breast/Breast_covariates/Referral_to_breast_clinic.RData")

print("Referral to breast clinic done")


## 3. REFERRAL TO MAMMOGRAPHY CLINIC -------------------------------------------
RMC_patients <- get_observations(4086282, 3)
RMC_id <- get_observations_id(4086282, 3, "Referral_to_mammography_clinic")


AnalysisRef  <- rbind(AnalysisRef,c(3,"Referral_to_mammography_clinic"))

save(list = c("RMC_patients","RMC_id"), file = "~/R/CancerCovid/Custom Characterisations/Breast/Breast_covariates/Referral_to_mammography_clinic.RData")

print("Referral to mammography clinic done")


## 4. FAST TRACK REFERRAL FOR SUSPECTED BC -------------------------------------
FTRBC_patients <- get_observations(44791272, 4)
FTRBC_id <- get_observations_id(44791272, 4, "Fasttrack referral for suspected breast cancer")

AnalysisRef  <- rbind(AnalysisRef,c(4,"Fasttrack referral for suspected breast cancer"))

save(list = c("FTRBC_patients","FTRBC_id"), file = "~/R/CancerCovid/Custom Characterisations/Breast/Breast_covariates/Fasttrack_referral.RData")

print("Fast track referral for suspected breast cancer done")


## 5. REFERRAL TO BREAST SURGEON -----------------------------------------------
RBS_patients <- get_observations(4141840, 5)
RBS_id <- get_observations_id(4141840, 5, "Referral to breast surgeon")

AnalysisRef  <- rbind(AnalysisRef,c(5,"Referral to breast surgeon"))

save(list = c("RBS_patients","RBS_id"), file = "~/R/CancerCovid/Custom Characterisations/Breast/Breast_covariates/Referral_breast_surgeon.RData")


print("Referral to breast surgeon done")


## 6. SCREENING MAMMOGRAMS -----------------------------------------------------
SM_patients <- get_procedures(4077697, 6)
SM_id <- get_procedures_id(4077697, 6, "Screening mammography")

AnalysisRef  <- rbind(AnalysisRef,c(6,"Screening mammography"))

save(list = c("SM_patients","SM_id"), file = "~/R/CancerCovid/Custom Characterisations/Breast/Breast_covariates/Screening_mammography.RData")

print("Screening mammograms done")


## 7. SEEN IN BREAST CLINIC ----------------------------------------------------
SBC_patients <- get_observations(4089031, 7)
SBC_id <- get_observations_id(4089031, 7, "Seen in breast clinic")

AnalysisRef  <- rbind(AnalysisRef,c(7,"Seen in breast clinic"))

save(list = c("SBC_patients","SBC_id"), file = "~/R/CancerCovid/Custom Characterisations/Breast/Breast_covariates/Seen_breast_clinic.RData")

print("Seen in breast clinic done")


## 8. DIAGNOSTIC MAMMOGRAMS ----------------------------------------------------
DM_patients <- get_procedures(4324693, 8)
DM_id <- get_procedures_id(4324693, 8, "Diagnostic mammograms")

AnalysisRef  <- rbind(AnalysisRef,c(8,"Diagnostic mammograms"))

save(list = c("DM_patients","DM_id"), file = "~/R/CancerCovid/Custom Characterisations/Breast/Breast_covariates/Diagnostic_mammograms.RData")

print("Diagnostic mammograms done")


## 9. DIAGNOSTIC MAMMOGRAM AND ULTRASOUND - MEASUREMENT - STANDALONE CODE ------
DMUS_patients <- cdm$measurement %>%
  select(person_id,measurement_concept_id, measurement_date) %>%
  filter(measurement_concept_id ==c(36203740, 36203750)) %>%
  inner_join(list_id) %>% 
  distinct() %>%
  collect() %>%
  rename("Event_date"="measurement_date") %>%
  mutate(FeatureExtractionId = 36203740009)

DMUS_id <- tibble(FeatureExtractionId = 36203740009,covariateId = 36203740, covariateName = "Diagnostic mammogram and ultrasound", AnalysisId = 9)

AnalysisRef  <- rbind(AnalysisRef,c(9,"Diagnostic mammogram and ultrasound"))

save(list = c("DMUS_patients","DMUS_id"), file = "~/R/CancerCovid/Custom Characterisations/Breast/Breast_covariates/Diagnostic_mammogram_ultrasound.RData")

print("Diagnostic mammogram and ultrasound done")



## 10. BIOPSY OF BREAST --------------------------------------------------------
BB_patients <- get_procedures(4047494, 10)
BB_id <- get_procedures_id(4047494, 10, "Biopsy of breast")

AnalysisRef  <- rbind(AnalysisRef,c(10,"Biopsy of breast"))

save(list = c("BB_patients","BB_id"), file = "~/R/CancerCovid/Custom Characterisations/Breast/Breast_covariates/Biopsy_breast.RData")

print("Biopsy of breast done")


## 11. STEREOTACTICALLY GUIDED CORE NEEDLE BIOPSY OF BREAST --------------------
SNBB_patients <- get_procedures(4022932, 11)
SNBB_id <- get_procedures_id(4022932, 11, "Stereotactically guided core needle biopsy of breast")

AnalysisRef  <- rbind(AnalysisRef,c(11,"Stereotactically guided core needle biopsy of breast"))

save(list = c("SNBB_patients","SNBB_id"), file = "~/R/CancerCovid/Custom Characterisations/Breast/Breast_covariates/Sterotactically_biopsy_breast.RData")

print("Stereotactically guided core needle biopsy of breast done")


## 12. PERCUTANEOUS NEEDLE BIOPSY OF BREAST ------------------------------------
PNB_patients <- get_procedures(4028790, 12)
PNB_id <- get_procedures_id(4028790, 12, "Percutaneous needle biopsy of breast")

AnalysisRef  <- rbind(AnalysisRef,c(12,"Percutaneous needle biopsy of breast"))

save(list = c("PNB_patients","PNB_id"), file = "~/R/CancerCovid/Custom Characterisations/Breast/Breast_covariates/Percutaneous_needle_biopsy.RData")

print("Percutaneous needle biopsy of breast done")



## 13. FINE NEEDLE ASPIRATION OF BREAST ----------------------------------------
FNA_patients <- get_procedures(4306207, 13)
FNA_id <- get_procedures_id(4306207, 13, "Fine needle aspiration of breast")

AnalysisRef  <- rbind(AnalysisRef,c(13,"Fine needle aspiration of breast"))

save(list = c("FNA_patients","FNA_id"), file = "~/R/CancerCovid/Custom Characterisations/Breast/Breast_covariates/Fine_needle_aspiration.RData")

print("Fine needle aspiration of breast done")


## 14. WIRE GUIDED LOCAL EXCISION ----------------------------------------------
WGLE_patients <- get_procedures(4216180, 14)
WGLE_id <- get_procedures_id(4216180, 14, "Wire guided local excision of breast lump")

AnalysisRef  <- rbind(AnalysisRef,c(14,"Wire guided local excision of breast lump"))

save(list = c("WGLE_patients","WGLE_id"), file = "~/R/CancerCovid/Custom Characterisations/Breast/Breast_covariates/Wire_guided_local_excision.RData")


print("Wire guided local excision of breast lump done")


## 15. EXCISION OF MAMMARY DUCT ------------------------------------------------
EMD_patients <- get_procedures(4146780, 15)
EMD_id <- get_procedures_id(4146780, 15, "Excision of mammary duct")

AnalysisRef  <- rbind(AnalysisRef,c(15,"Excision of mammary duct"))

save(list = c("EMD_patients","EMD_id"), file = "~/R/CancerCovid/Custom Characterisations/Breast/Breast_covariates/Excision_mammary_duct.RData")

print("Excision of mammary duct done")


## 16. WIDE LOCAL EXCISION OF BREAST LESION ------------------------------------
WLEBL_patients <- get_procedures(4129190, 16)
WLEBL_id <- get_procedures_id(4129190, 16, "Wide local excision of breast lesion")

AnalysisRef  <- rbind(AnalysisRef,c(16,"Wide local excision of breast lesion"))

save(list = c("WLEBL_patients","WLEBL_id"), file = "~/R/CancerCovid/Custom Characterisations/Breast/Breast_covariates/Wide_local_excision_breast_lesion.RData")

print("Wide local excision of breast lesion done")


## 17. EXCISION OF LESION OF BREAST --------------------------------------------
ELB_patients <- get_procedures(4194124, 17)
ELB_id <- get_procedures_id(4194124, 17, "Excision of lesion of breast")

AnalysisRef  <- rbind(AnalysisRef,c(17,"Excision of lesion of breast"))

save(list = c("ELB_patients","ELB_id"), file = "~/R/CancerCovid/Custom Characterisations/Breast/Breast_covariates/Excision_lesion_breast.RData")

print("Excision of lesion of breast done")


## 18. EXCISION OF BREAST TISSUE -----------------------------------------------
EBT_patients <- get_procedures(4286804, 18)
EBT_id <- get_procedures_id(4286804, 18, "Excision of breast tissue")

AnalysisRef  <- rbind(AnalysisRef,c(18,"Excision of breast tissue"))

save(list = c("EBT_patients","EBT_id"), file = "~/R/CancerCovid/Custom Characterisations/Breast/Breast_covariates/Excision_breast_tissue.RData")

print("Excision of breast tissue done")


## 19. SEEN BY BREAST SURGEON --------------------------------------------------
SBS_patients <- get_observations(4136626, 19)
SBS_id <- get_observations_id(4136626, 19, "Seen by breast surgeon")

AnalysisRef  <- rbind(AnalysisRef,c(19,"Seen by breast surgeon"))

save(list = c("SBS_patients","SBS_id"), file = "~/R/CancerCovid/Custom Characterisations/Breast/Breast_covariates/Seen_breast_surgeon.RData")

print("Seen by breast surgeon done")


print(paste0("- Breast cancer covariate counts done"))
info(logger, "- Breast cancer covariate counts done")


# ========================= INDIVIDUAL TABLES================================= # 
# Get tables: person: id, covariate, value
VI_table        <- getIndividualTabs(VI_id, VI_patients, individuals_id, 3, FALSE)
RBC_table        <- getIndividualTabs(RBC_id, RBC_patients, individuals_id, 3, FALSE)
RMC_table        <- getIndividualTabs(RMC_id, RMC_patients, individuals_id, 3, FALSE)
FTRBC_table      <- getIndividualTabs(FTRBC_id, FTRBC_patients, individuals_id,  3, FALSE)
RBS_table        <- getIndividualTabs(RBS_id, RBS_patients, individuals_id, 3, FALSE)
SM_table        <- getIndividualTabs(SM_id, SM_patients, individuals_id, 3, FALSE)
SBC_table        <- getIndividualTabs(SBC_id, SBC_patients, individuals_id, 3, FALSE)
DM_table        <- getIndividualTabs(DM_id, DM_patients, individuals_id, 3, FALSE)
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
SBS_table        <- getIndividualTabs(SBS_id, SBS_patients, individuals_id, 3, FALSE)

# Join the tables
continuous_table <- VI_table %>% union_all(RBC_table) %>% union_all(RMC_table) %>% union_all(FTRBC_table) %>% union_all(RBS_table) %>%
  union_all(SM_table) %>% union_all(SBC_table) %>% union_all(DM_table) %>% union_all(DMUS_table) %>% union_all(BB_table) %>% 
  union_all(SNBB_table) %>% union_all(PNB_table) %>% union_all(FNA_table) %>% union_all(WGLE_table) %>% union_all(EMD_table) %>% 
  union_all(WLEBL_table) %>% union_all(ELB_table) %>% union_all(EBT_table) %>% union_all(SBS_table) %>%ungroup()

# Pivot the continuous table around, and rename person_id as subject_id. This
# is later used to run the SMD function
Continuous_table_pivot <- continuous_table %>% inner_join(breast_covariate_names) %>%
  select(person_id, covariate, value) %>% 
  rename("subject_id" = "person_id") %>%
   tidyr::pivot_wider(names_from = covariate, values_from = value,values_fill = 0) 

continuous_table <- Continuous_table_pivot %>% tidyr::pivot_longer(2:58, names_to = "covariate", values_to = "value") 

# read all the covariate names from the 'forALLCharacterisations_with_functions.R
namt <- t(breast_covariate_names)

save(list = c("VI_table", "RBC_table", "RMC_table", "FTRBC_table", "RBS_table", "SM_table", "SBC_table", "DM_table", "DMUS_table", 
              "BB_table", "SNBB_table", "PNB_table", "FNA_table", "WGLE_table", "EMD_table", "WLEBL_table", "ELB_table", "EBT_table", 
              "SBS_table", "continuous_table", "Continuous_table_pivot", "namt"), file = "~/R/CancerCovid/Custom Characterisations/Breast/BreastIndividualTabs.Rdata")


# =================== AGGREGATED COUNTS OF COVARIATES ======================== # 


# All tables joined together
# Cohort 1 - breast cancer after lockdown
All_tables_counts1 <- continuous_table %>%  
  inner_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==1) %>%
  group_by(covariate) %>% 
  tally(value) %>% 
    print(n=57)


# cohort 2 - breast cancer before lockdown
All_tables_counts2 <- continuous_table %>%  
  inner_join(cohorts_db_df, by = "subject_id") %>% 
  filter(cohort_definition_id ==2) %>%
  group_by(covariate) %>% 
  tally(value) %>% 
    print(n=57)

All_tables_counts1 <- All_tables_counts1 %>% rename("n after lockdown" = "n") %>% rename("Covariate" = "covariate")
All_tables_counts2 <- All_tables_counts2 %>% rename("n before lockdown" = "n") %>% rename("Covariate" = "covariate")

All_count_joined <- All_tables_counts2 %>% inner_join(All_tables_counts1) %>% print()

Pretty_counts_table <- flextable(All_count_joined) %>% set_caption(caption = "Table 2. Frequencies of visits, breast cancer-related observations and procedures during different time periods before and after lockdown") 
Pretty_counts_table



# =============================== (SMD) ====================================== #

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

# Run SMD function to create table of all
All_SMD <- compute_continuous_smd(All_tables_cohort_1, All_tables_cohort_2) 


All_SMD <- All_SMD %>% rename("mean after lockdown" = "mean1") %>% rename("var after lockdown" = "var1") %>% 
                                  rename("mean before lockdown" = "mean2") %>% rename("var before lockdown" = "var2") %>%
                                  rename("Covariate" = "covariate") 
All_SMD <- All_SMD[,c(1,4,5,2,3,6)]

Pretty_SMD_table <- flextable(All_SMD) %>% set_caption(caption = "Table 3. Mean(var) frequencies of visits, breast cancer-related observations and procedures during different time periods before and after lockdown") 

Pretty_SMD_table

## ========================= Save all tables ================================ ##

save(list = c("All_tables_counts1", "All_tables_counts2", "All_count_joined", "Pretty_counts_table", "All_tables_cohort_1", "All_tables_cohort_2",
              "All_SMD", "Pretty_SMD_table"), file = "~/R/CancerCovid/Custom Characterisations/Breast/CountsSMDTabs.Rdata")


write.csv(All_count_joined, "~/R/CancerCovid/Custom Characterisations/Breast/All_count_joined.csv", row.names = FALSE)
write.csv(All_SMD, "~/R/CancerCovid/Custom Characterisations/Breast/All_SMD.csv", row.names = FALSE)
     
