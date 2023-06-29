# ============================================================================ #
#                 INSTANTIATE COHORTS FOR CANCER/COVID STUDY                   #
#                       FOR CUSTOM CHARACTERISATIONS                           #
#                                Nicola Barclay                                #
#                                 01-02-2023                                   #
# ============================================================================ #


# ============================================================================ #
#                    1.  CANCER DIAGNOSES BEFORE AND AFTER LOCKDOWN            #
# ============================================================================ #
info(logger, "- getting cancer populations before and after lockdown")

outcome_cohorts_1 <- readCohortSet(here("1_InstantiateCohorts", "CancerCohortsBeforeAfterLockdown"))

cdm <- generateCohortSet(cdm = cdm, 
                                       cohortSet = outcome_cohorts_1,
                                       name = outcome_table_name_1,
                                       overwrite = TRUE) 

cohortCount(cdm[[outcome_table_name_1]]) %>%  glimpse()

cohortAttrition(cdm[[outcome_table_name_1]]) %>%  glimpse()

cohortSet(cdm[[outcome_table_name_1]]) %>%  glimpse()

info(logger, "- got cancer populations before and after lockdown")

# ============================================================================ #


# ============================================================================ #
#           2.  CANCER DIAGNOSES BEFORE, DURING AND AFTER LOCKDOWN             #
# ============================================================================ #
info(logger, "- getting cancer populations before, during and after lockdown")

outcome_cohorts_2 <- readCohortSet(here("1_InstantiateCohorts", "CancerCohorts3TimePeriods"))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_2,
                         name = outcome_table_name_2,
                         overwrite = TRUE) 

cohortCount(cdm[[outcome_table_name_2]]) %>%  glimpse()

cohortAttrition(cdm[[outcome_table_name_2]]) %>%  glimpse()

cohortSet(cdm[[outcome_table_name_2]]) %>%  glimpse()

info(logger, "- got cancer populations before, during and after lockdown")

# ============================================================================ #


# ============================================================================ #
#           3.  DENOMINATOR BEFORE, DURING AND AFTER LOCKDOWN                  #
# ============================================================================ #
info(logger, "- getting denominator populations before, during and after lockdown")

outcome_cohorts_3 <- readCohortSet(here("1_InstantiateCohorts", "Denominator"))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_3,
                         name = outcome_table_name_3,
                         overwrite = TRUE) 

cohortCount(cdm[[outcome_table_name_3]]) %>%  glimpse()

cohortAttrition(cdm[[outcome_table_name_3]]) %>%  glimpse()

cohortSet(cdm[[outcome_table_name_3]]) %>%  glimpse()

info(logger, "- got denominator populations before, during and after lockdown")

# ============================================================================ #