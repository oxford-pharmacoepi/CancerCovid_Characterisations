# ============================================================================ #
#                         Custom Characterisations                             #
#                     Functions for denominator Cohort                         #
#                              Nicola Barclay                                  #
#                                19-12-2022                                    #
# ============================================================================ #


## ----------------------------- LOAD PACKAGES ------------------------------ ##
library("lubridate")
library("tidyr")
library("glmnet")
library("readr")
library("survey")
library("tableone")
library("ggplot2")
library("here") 
library("dplyr")
library("dbplyr")
library("flextable")
library("glue")



## ------------------------------- FUNCTIONS -------------------------------- ##
## getIndividualTables
getIndividualTabs      <- function(tab_id, tab_patients, population_tab, numOfWindows, value1) {
  if (numOfWindows == 2) {
    aux_table <- tab_patients %>%
      inner_join(population_tab, by = "person_id") %>% 
      filter(index_date-Era_start_date <= days(180) & index_date-Era_start_date > days(0)) %>%
      mutate(window = if_else(index_date-Era_start_date >= days(1) & index_date-Era_start_date <= days(30), "1. 1to30", "2. 31to180")) %>%
      inner_join(tab_id, by = "FeatureExtractionId") %>%
      mutate(covariate = gsub(" ","_", paste(covariateName,window, sep = "_"))) %>%
      select(person_id, covariate) %>%
      distinct() %>%
      mutate(value = 1)
  } else if (numOfWindows == 3) {
    aux_table <- tab_patients %>%
      inner_join(population_tab, by = "person_id") %>% 
      filter(index_date-Event_date > days(0)) %>%
      mutate(window = NA)
    aux_table$window[aux_table$index_date-aux_table$Event_date >= days(1) & aux_table$index_date-aux_table$Event_date <= days(30)]    <- "1. 1to30"
    aux_table$window[aux_table$index_date-aux_table$Event_date >= days(31) & aux_table$index_date-aux_table$Event_date <= days(180)]  <- "2. 31_to_180"
    aux_table$window[aux_table$index_date-aux_table$Event_date >= days(181)] <- "3. +181"
    
    if (value1)
      aux_table <- aux_table %>%
      inner_join(tab_id, by = "FeatureExtractionId") %>%
      mutate(covariate = gsub(" ","_", paste(covariateName,window, sep = "_"))) %>%
      select(person_id, covariate) %>%
      distinct() %>%
      mutate(value = 1) 
    else {
      aux_table <- aux_table %>%
        inner_join(tab_id, by = "FeatureExtractionId") %>%
        mutate(covariate = gsub(" ","_", paste(covariateName,window, sep = "_"))) %>%
        select(person_id, covariate) %>%
        group_by(person_id,covariate) %>%
        count() %>%
        rename(value = n) # here i would like an ifelse statement to return a 0 if there are no counts in the column
    }
  } else if (numOfWindows == 0) {
    if (value1) {
      aux_table <- tab_patients %>%
        inner_join(population_tab, by = "person_id") %>% 
        inner_join(tab_id, by = c("FeatureExtractionId", "value")) %>%
        select(-value) %>%
        mutate(covariate = gsub(" ","_", paste(valueName, sep = "_")), value = 1) %>%
        select(person_id, covariate, value) %>%
        distinct()
      
      
    } else {
      aux_table <- tab_patients %>% inner_join(population_tab, by = "person_id") %>% filter(index_date-Event_date > days(0)) %>%
        inner_join(tab_id, by = "FeatureExtractionId") %>%
        mutate(covariate = gsub(" ","_", paste(covariateName,"allTime", sep = "_"))) %>% select(person_id, covariate) %>%
        group_by(person_id,covariate) %>% count() %>% rename(value = n)
    }
  }
  return(aux_table)
}





## GET OBSERVATIONS FROM THE OBSERVATION TABLE
# get observations for patients
get_observations <- function(yourconceptid, analysis_num){
  aux_table <- cdm$observation %>%
  select(person_id,observation_concept_id, observation_date) %>%
  inner_join(cdm$concept_ancestor %>% 
               filter(ancestor_concept_id == yourconceptid) %>%
               select("observation_concept_id" = "descendant_concept_id")) %>% # this part of the code will include all descendant concepts too
  inner_join(list_id) %>% 
  distinct() %>%
  collect() %>%
  rename("Event_date"="observation_date") %>%
  mutate(FeatureExtractionId = paste(yourconceptid,analysis_num, sep = "_"))
  
  return(aux_table)
  
}
  
# create an id for these observations for use in later tables
get_observations_id <- function(yourconceptid, analysis_num, covariateName){
  aux_table <- tibble(FeatureExtractionId = paste(yourconceptid,analysis_num, sep = "_"),covariateId = yourconceptid, covariateName = covariateName, AnalysisId = analysis_num)
  
  return(aux_table)
}





## GET OBSERVATIONS FROM THE PROCEDURE OCCURRENCE TABLE
# get procedures for patients
get_procedures <- function(yourconceptid, analysis_num){
  aux_table <- cdm$procedure_occurrence %>%
    select(person_id,procedure_concept_id, procedure_date) %>%
    inner_join(cdm$concept_ancestor %>% 
                 filter(ancestor_concept_id == yourconceptid) %>%
                 select("procedure_concept_id" = "descendant_concept_id")) %>% # this part of the code will include all descendant concepts too
    inner_join(list_id) %>% 
    distinct() %>%
    collect() %>%
    rename("Event_date"="procedure_date") %>%
    mutate(FeatureExtractionId = paste(yourconceptid,analysis_num, sep = "_"))
  
  return(aux_table)
  
}

# create an id for these observations for use in later tables
get_procedures_id <- function(yourconceptid, analysis_num, covariateName){
  aux_table <- tibble(FeatureExtractionId = paste(yourconceptid,analysis_num, sep = "_"),covariateId = yourconceptid, covariateName = covariateName, AnalysisId = analysis_num)
  
  return(aux_table)
}






## ============ SET UP FOR SMD CALCULATION ================================== ##
## This will only run once you have created the continuous table which is a union
## of the individual tables. Run separately once individual covariates and individual
## tables have run


all_covariate_names <- data.frame(c("Biopsy_of_breast_1._1to30", "Biopsy_of_breast_2._31_to_180", "Biopsy_of_breast_3._+181",                                          
                                        "Diagnostic_mammogram_and_ultrasound_1._1to30", "Diagnostic_mammogram_and_ultrasound_2._31_to_180", "Diagnostic_mammogram_and_ultrasound_3._+181",
                                        "Diagnostic_mammograms_1._1to30","Diagnostic_mammograms_2._31_to_180","Diagnostic_mammograms_3._+181",
                                        "Excision_of_breast_tissue_1._1to30","Excision_of_breast_tissue_2._31_to_180","Excision_of_breast_tissue_3._+181",
                                        "Excision_of_lesion_of_breast_1._1to30","Excision_of_lesion_of_breast_2._31_to_180","Excision_of_lesion_of_breast_3._+181",
                                        "Excision_of_mammary_duct_1._1to30","Excision_of_mammary_duct_2._31_to_180","Excision_of_mammary_duct_3._+181",
                                        "Fasttrack_referral_for_suspected_breast_cancer_1._1to30","Fasttrack_referral_for_suspected_breast_cancer_2._31_to_180","Fasttrack_referral_for_suspected_breast_cancer_3._+181",
                                        "Fine_needle_aspiration_of_breast_1._1to30","Fine_needle_aspiration_of_breast_2._31_to_180","Fine_needle_aspiration_of_breast_3._+181",
                                        "Percutaneous_needle_biopsy_of_breast_1._1to30","Percutaneous_needle_biopsy_of_breast_2._31_to_180","Percutaneous_needle_biopsy_of_breast_3._+181",
                                        "Referral_to_breast_clinic_1._1to30", "Referral_to_breast_clinic_2._31_to_180", "Referral_to_breast_clinic_3._+181",
                                        "Referral_to_breast_surgeon_1._1to30","Referral_to_breast_surgeon_2._31_to_180", "Referral_to_breast_surgeon_3._+181",
                                        "Referral_to_mammography_clinic_1._1to30","Referral_to_mammography_clinic_2._31_to_180","Referral_to_mammography_clinic_3._+181",
                                        "Screening_mammography_1._1to30", "Screening_mammography_2._31_to_180", "Screening_mammography_3._+181",
                                        "Seen_by_breast_surgeon_1._1to30","Seen_by_breast_surgeon_2._31_to_180","Seen_by_breast_surgeon_3._+181",
                                        "Seen_in_breast_clinic_1._1to30", "Seen_in_breast_clinic_2._31_to_180", "Seen_in_breast_clinic_3._+181",
                                        "Stereotactically_guided_core_needle_biopsy_of_breast_1._1to30","Stereotactically_guided_core_needle_biopsy_of_breast_2._31_to_180","Stereotactically_guided_core_needle_biopsy_of_breast_3._+181",
                                        "Visits_within_healthcare_system_1._1to30", "Visits_within_healthcare_system_2._31_to_180", "Visits_within_healthcare_system_3._+181",
                                        "Wide_local_excision_of_breast_lesion_1._1to30","Wide_local_excision_of_breast_lesion_2._31_to_180","Wide_local_excision_of_breast_lesion_3._+181",
                                        "Wire_guided_local_excision_of_breast_lump_1._1to30", "Wire_guided_local_excision_of_breast_lump_2._31_to_180", "Wire_guided_local_excision_of_breast_lump_3._+181",
                                        "Bowel_cancer_screening_prog_1._1to30", "Bowel_cancer_screening_prog_2._31_to_180", "Bowel_cancer_screening_prog_3._+181",                                          
                                        "Colonoscopies_1._1to30", "Colonoscopies_2._31_to_180", "Colonoscopies_3._+181",
                                        "Sigmoidoscopy_1._1to30","Sigmoidoscopy_2._31_to_180","Sigmoidoscopy_3._+181",
                                        "Quantitative_faecal_immunochemical_tests_1._1to30","Quantitative_faecal_immunochemical_tests_2._31_to_180","Quantitative_faecal_immunochemical_tests_3._+181",
                                        "Ultrasonography of abdomen_1._1to30","Ultrasonography of abdomen_2._31_to_180","Ultrasonography of abdomen_3._+181",
                                        "Ultrasonography of intestine_1._1to30","Ultrasonography of intestine_2._31_to_180","Ultrasonography of intestine_3._+181",
                                        "Ultrasonography of rectum_1._1to30","Ultrasonography of rectum_2._31_to_180","Ultrasonography of rectum_3._+181",
                                        "Ultrasound of gastrointestinal tract_1._1to30","Ultrasound of gastrointestinal tract_2._31_to_180","Ultrasound of gastrointestinal tract_3._+181",
                                        "Bronchoscopy_1._1to30", "Bronchoscopy_2._31_to_180", "Bronchoscopy_3._+181",   
                                        "CT and biopsy of chest_1._1to30","CT and biopsy of chest_2._31_to_180","CT and biopsy of chest_3._+181",
                                        "Diagnostic Radiology (Diagnostic Imaging) Procedures of the Chest_1._1to30","Diagnostic Radiology (Diagnostic Imaging) Procedures of the Chest_2._31_to_180","Diagnostic Radiology (Diagnostic Imaging) Procedures of the Chest_3._+181",
                                        "Endobronchial ultrasonography guided transbronchial needle aspiration_1._1to30","Endobronchial ultrasonography guided transbronchial needle aspiration_2._31_to_180","Endobronchial ultrasonography guided transbronchial needle aspiration_3._+181",
                                        "Fast track referral for lung cancer_1._1to30", "Fast track referral for lung cancer_2._31_to_180", "Fast track referral for lung cancer_3._+181",
                                        "Mediastinoscopy_1._1to30","Mediastinoscopy_2._31_to_180","Mediastinoscopy_3._+181",
                                        "MRI of chest_1._1to30","MRI of chest_2._31_to_180","MRI of chest_3._+181",
                                        "US scan and biopsy of chest_1._1to30","US scan and biopsy of chest_2._31_to_180","US scan and biopsy of chest_3._+181",
                                         "Biopsy of prostate_1._1to30","Biopsy of prostate_2._31_to_180","Biopsy of prostate_3._+181",
                                         "Prostate specific antigen measurement_1._1to30", "Prostate specific antigen measurement_2._31_to_180", "Prostate specific antigen measurement_3._+181",                                          
                                         "PSA monitoring_1._1to30", "PSA monitoring_2._31_to_180", "PSA monitoring_3._+181"))
                                      

colnames(all_covariate_names) <- c("covariate")

namt <- t(all_covariate_names)
 

## SMD for continuous variables MY WAY
compute_continuous_smd <- function(data_table_cohort_1, data_table_cohort_2){
  
    data_g1 <- data_table_cohort_1
  
    mean1   <- data_g1 %>% summarise_at(namt, mean) 
    var1    <- data_g1 %>% summarise_at(namt, var)
  
    data_g2 <- data_table_cohort_2 
  
    mean2   <- data_g2 %>% summarise_at(namt, mean) 
    var2    <- data_g2 %>% summarise_at(namt, var)
  
  table1  <- rbind(mean1,var1,mean2,var2) %>% as.data.frame()  
  
  ttable1  <- tibble(covariate = names(table1), mean1 = t(table1[1,])[,1], var1 = t(table1[2,])[,1], mean2 = t(table1[3,])[,1], var2 = t(table1[4,])[,1]) %>%
    mutate(SMD = abs(mean1-mean2)/sqrt(var1+var2)) %>% arrange(covariate) %>% mutate_if(is.numeric, round, digits = 3)
  

  return(ttable1) # return only works within a function, so this might work if you have a function
}

