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
                                       cohortTableName = outcome_table_name_1,
                                       overwrite = TRUE) 

cdm$cancers_before_after_lockdown %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

info(logger, "- got cancer populations before and after lockdown")

# ============================================================================ #