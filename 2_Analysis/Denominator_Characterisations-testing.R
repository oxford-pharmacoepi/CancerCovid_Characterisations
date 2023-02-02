# ============================================================================ #
#                         Custom Characterisations                             #
#                           for the denominator                                #
#                   NO COHORT DEFINED - JUST CDM REFERENCE                     #
#                              Nicola Barclay                                  #
#                                02-02-2023                                    #
# ============================================================================ #


start<-Sys.time()

# start log ----
log_file <- paste0(output.folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"


info(logger, "- Getting counts of mammography tests - with filter")


# Get counts of mammography tests before lockdown

Mammography_test_1 <- cdm$procedure_occurrence %>%
  select(person_id, procedure_concept_id, procedure_date) %>%
  filter(procedure_concept_id ==4077697) %>% 
  filter(procedure_date >= "2017-01-01" & procedure_date <="2020-03-22") %>%
    distinct() %>%
    collect() %>%
    tally() %>%
    print()

info(logger, "- Got counts of mammography tests - with filter")


    
# Get counts of mammography tests after lockdown

Mammography_test_2 <- cdm$procedure_occurrence %>%
  select(person_id, procedure_concept_id, procedure_date) %>%
  filter(procedure_concept_id ==4077697) %>% 
  filter(procedure_date >= "2020-03-23" & procedure_date <="2020-06-29") %>%
    distinct() %>%
    collect() %>%
    tally() %>%
    print()
  
  
# create a table to put this info in
  
  AnalysisRef  <- tibble(AnalysisId = 1, AnalysisName = "Screening_Mammographies")
  
  
  ## UP TO HERE ##
  
  
  
# code to add additional rows
  AnalysisRef  <- rbind(AnalysisRef,c(5,"Referral to breast surgeon"))

# ========================= INDIVIDUAL TABLES================================= # 

print(paste0("- Getting breast cancer individual covariate tables"))
info(logger, "- Getting breast cancer individual covariate tables")

# Get tables: person: id, covariate, value

SM_table        <- getIndividualTabs(SM_id, SM_patients, individuals_id, 3, FALSE)


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

# read all the covariate names from the 'forAllCharacterisations_with_functions.R
namt <- t(breast_covariate_names)

save(list = c("VI_table", "RBC_table", "RMC_table", "FTRBC_table", "RBS_table", "SM_table", "SBC_table", "DM_table", "DMUS_table", 
              "BB_table", "SNBB_table", "PNB_table", "FNA_table", "WGLE_table", "EMD_table", "WLEBL_table", "ELB_table", "EBT_table", 
              "SBS_table", "continuous_table", "Continuous_table_pivot", "namt"),  
     file = here("Results", db.name, "Breast", "Breast_covariates", "BreastIndividualTabs.Rdata"))

print(paste0("- Got breast cancer individual covariate tables"))
info(logger, "- Got breast cancer individual covariate tables")


# =================== AGGREGATED COUNTS OF COVARIATES ======================== # 

print(paste0("- Getting aggregated counts of breast cancer covariate tables"))
info(logger, "- Getting aggregated counts of breast cancer covariate tables")

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

Pretty_counts_table <- flextable(All_count_joined) %>% set_caption(caption = 
        "Frequencies of visits, breast cancer-related observations and procedures during different time periods before and after lockdown") 
Pretty_counts_table

tablename <- paste0("All_covariate_counts", db.name, analysis.name, ".pdf")

# save the table as pdf
save_as_image(Pretty_counts_table, here("Results", db.name , "Breast",tablename), 
              zoom=1, expand=100, webshot = "webshot")

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

# Run SMD function to create table of all
All_SMD <- compute_continuous_smd(All_tables_cohort_1, All_tables_cohort_2) 


All_SMD <- All_SMD %>% rename("mean after lockdown" = "mean1") %>% rename("var after lockdown" = "var1") %>% 
                                  rename("mean before lockdown" = "mean2") %>% rename("var before lockdown" = "var2") %>%
                                  rename("Covariate" = "covariate") 
All_SMD <- All_SMD[,c(1,4,5,2,3,6)]

Pretty_SMD_table <- flextable(All_SMD) %>% set_caption(caption = "Mean(var) frequencies of visits, breast cancer-related observations and procedures during different time periods before and after lockdown") 

Pretty_SMD_table

tablename <- paste0("All_covariate_SMD", db.name, analysis.name, ".pdf")

# save the table as pdf
save_as_image(Pretty_SMD_table, here("Results", db.name , "Breast",tablename), 
              zoom=1, expand=100, webshot = "webshot")

## ========================= Save all tables ================================ ##

save(list = c("All_tables_counts1", "All_tables_counts2", "All_count_joined", "Pretty_counts_table", "All_tables_cohort_1", "All_tables_cohort_2",
              "All_SMD", "Pretty_SMD_table"), file = here("Results", db.name, "Breast", "Breast_covariates", "BreastCountsSMDTabs.Rdata"))

write.csv(All_count_joined, here("Results", db.name, "Breast", "All_count_joined_breast.csv"), row.names = FALSE)
write.csv(All_SMD, here("Results", db.name, "Breast", "All_SMD_breast.csv"), row.names = FALSE)


print(paste0("- Got SMD of breast cancer covariate tables"))
info(logger, "- Got SMD of breast cancer covariate tables")


print(paste0("- 2. BREAST CANCER CUSTOM CHARACTERISATIONS DONE"))
info(logger, "- 2. BREAST CANCER CUSTOM CHARACTERISATIONS DONE")