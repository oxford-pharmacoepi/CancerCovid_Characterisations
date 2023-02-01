# ============================================================================ #
#                 INSTANTIATE COHORTS FOR CANCER/COVID STUDY                   #
#                       FOR CUSTOM CHARACTERISATIONS                           #
#                                Nicola Barclay                                #
#                                 01-02-2023                                   #
# ============================================================================ #



# ============================================================================ #
#            1.  DENOMINATOR POPULATIONS BEFORE AND AFTER LOCKDOWN             #
# ============================================================================ #


# instantiate cancer outcome cohorts
info(logger, "- getting denominator populations before and after lockdown")

outcome_cohorts_1 <- CDMConnector::readCohortSet(here::here("1_InstantiateCohorts","DenominatorCohortsBeforeAfterLockdown"))




cdm <- CDMConnector::generateCohortSet(cdm = cdm, 
                                       cohortSet = outcome_cohorts_1,
                                       cohortTableName = outcome_table_name_1,
                                       overwrite = TRUE
)

cdm$DenominatorBeforeAfterLockdown %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

info(logger, "- got denominator populations before and after lockdown")



# ============================================================================ #
#                    2.  CANCER DIAGNOSES BEFORE AND AFTER LOCKDOWN               #
# ============================================================================ #
info(logger, "- getting cancer populations before and after lockdown")

outcome_cohorts_2 <- CDMConnector::readCohortSet(here("1_InstantiateCohorts", "CancerCohortsBeforeAfterLockdown"))

cdm <- CDMConnector::generateCohortSet(cdm = cdm, 
                                       cohortSet = outcome_cohorts_2,
                                       cohortTableName = outcome_table_name_2,
                                       overwrite = TRUE) 

cdm$CancersBeforeAfterLockdown %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

info(logger, "- got cancer populations before and after lockdown")

