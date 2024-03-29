# ============================================================================ #
#                       STANDARD OHDSI FEATURE EXTRACTION                      #
#                   OF CANCER COHORTS - OVER 3 TIME PERIODS                    #
#                                Nicola Barclay                                #
#                                 07-02-2023                                   #
# ============================================================================ #


# The code will run the standardised feature extraction package comparing the 
# cancer cohorts before and after lockdown. In this script I use the cohort 
# definitions that are dissected at the date of lockdown.


print(paste0("- 6. Standardized Feature Extraction"))
info(logger, "- 6. Standardized Feature Extraction")


# CONNECT TO YOUR DATABASE (THE OHDSI WAY) - assuming you have instantiated your cohorts. 
# Note that your data base may require a different driver. See here for details:
# ??jdbcDrivers


connectionDetails <- DatabaseConnector::downloadJdbcDrivers("postgresql",
                                                            here::here())

connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server =server,
                                             user = user,
                                             password = password,
                                             port = port ,
                                             pathToDriver = here())


## 1. BREAST CANCER BASELINE CHARACTERISTICS FOR TABLE 1 FROM FEATURE EXTRACTION PACKAGE

print(paste0("- Feature extraction of breast cancers"))
info(logger, "- Feature extraction of breast cancers")

settings <- createTable1CovariateSettings()

covBreastBefore <- getDbCovariateData(connectionDetails = connectionDetails,
                                      cdmDatabaseSchema = cdm_database_schema,
                                      cohortDatabaseSchema = results_database_schema,
                                      cohortTable = outcome_table_name_2,
                                      cohortId = 1,
                                      covariateSettings = settings,
                                      aggregated = TRUE)

covBreastDuring <- getDbCovariateData(connectionDetails = connectionDetails,
                                      cdmDatabaseSchema = cdm_database_schema,
                                      cohortDatabaseSchema = results_database_schema,
                                      cohortTable = outcome_table_name_2,
                                      cohortId = 2,
                                      covariateSettings = settings,
                                      aggregated = TRUE)

covBreastAfter <- getDbCovariateData(connectionDetails = connectionDetails,
                                      cdmDatabaseSchema = cdm_database_schema,
                                      cohortDatabaseSchema = results_database_schema,
                                      cohortTable = outcome_table_name_2,
                                      cohortId = 3,
                                      covariateSettings = settings,
                                      aggregated = TRUE)


std1 <- computeStandardizedDifference(covBreastBefore, covBreastDuring)
std2 <- computeStandardizedDifference(covBreastBefore, covBreastAfter)

head(std1)
head(std2)

 # Breast Cancer Table 1 including standardised mean difference between the two cohorts
result1 <- createTable1(covBreastBefore, covBreastDuring, output = "one column", showCounts = TRUE, showPercent = TRUE, percentDigits = 1, valueDigits = 1, stdDiffDigits = 2)
print(result1, row.names = TRUE, right = FALSE)

result2 <- createTable1(covBreastBefore, covBreastAfter, output = "one column", showCounts = TRUE, showPercent = TRUE, percentDigits = 1, valueDigits = 1, stdDiffDigits = 2)
print(result2, row.names = TRUE, right = FALSE)

# save RData objects
save(result1, result2, covBreastBefore, covBreastDuring, covBreastAfter, file = here("Results", db.name, "Breast", "BreastFeatureExtraction.RData"))


# need to add columns for after lockdown to the dataframe for before and during before making a flextable.
Breast_combined_results <- cbind(result1, result2)
names(Breast_combined_results)[2] = "Count before lockdown" 
names(Breast_combined_results)[3] = "% (n=8,815) before lockdown"
names(Breast_combined_results)[4] = "Count during lockdown"
names(Breast_combined_results)[5] = "% (n=459) during lockdown"
names(Breast_combined_results)[6] = "Std. Diff Before vs. during lockdown"
names(Breast_combined_results)[10] = "Count after lockdown"
names(Breast_combined_results)[11] = "% (n=2,900) after lockdown"
names(Breast_combined_results)[12] = "Std. Diff Before vs. after lockdown"
Breast_combined_results <- Breast_combined_results[-c(7,8,9)]
Breast_combined_results <- Breast_combined_results[c(1,2,3,4,5,7,8,6,9)]

Pretty_breast_table <- flextable(Breast_combined_results) %>% theme_vanilla() %>% 
  set_caption(caption = "Feature extraction for breast cancer cohorts before, during and after lockdown") %>% 
  width(width = 1.4) 

save_as_docx(
  "Breast_features_table" = Pretty_breast_table, path = here("Results", db.name, "Breast", "Breast_feature_extraction_table.docx"))

# save the table as a csv file
write.csv(Breast_combined_results, here("Results", db.name, "Breast", "Breast_features_table.csv"), row.names = FALSE)


save_as_image(Pretty_breast_table, path = here("Results", db.name, "Breast", "Breast_feature_extraction_table.png"))

print(paste0("- Feature extraction of breast cancers done"))
info(logger, "- Feature extraction of breast cancers done")

## 2. COLORECTAL CANCER BASELINE CHARACTERISTICS FOR TABLE 1 FROM FEATURE EXTRACTION PACKAGE

print(paste0("- Feature extraction of colorectal cancers"))
info(logger, "- Feature extraction of colorectal cancers")

covColorectalBefore <- getDbCovariateData(connectionDetails = connectionDetails,
                                      cdmDatabaseSchema = cdm_database_schema,
                                      cohortDatabaseSchema = results_database_schema,
                                      cohortTable = outcome_table_name_2,
                                      cohortId = 4,
                                      covariateSettings = settings,
                                      aggregated = TRUE)

covColorectalDuring <- getDbCovariateData(connectionDetails = connectionDetails,
                                         cdmDatabaseSchema = cdm_database_schema,
                                         cohortDatabaseSchema = results_database_schema,
                                         cohortTable = outcome_table_name_2,
                                         cohortId = 5,
                                         covariateSettings = settings,
                                         aggregated = TRUE)

covColorectalAfter <- getDbCovariateData(connectionDetails = connectionDetails,
                                     cdmDatabaseSchema = cdm_database_schema,
                                     cohortDatabaseSchema = results_database_schema,
                                     cohortTable = outcome_table_name_2,
                                     cohortId = 6,
                                     covariateSettings = settings,
                                     aggregated = TRUE)

std1 <- computeStandardizedDifference(covColorectalBefore, covColorectalDuring)
std2 <- computeStandardizedDifference(covColorectalBefore, covColorectalAfter)

head(std1)
dim(std2)


# Colorectal Cancer Table 1 including standardised mean difference between the two cohorts
result3 <- createTable1(covColorectalBefore, covColorectalDuring, output = "one column", showCounts = TRUE, showPercent = TRUE, percentDigits = 1, valueDigits = 1, stdDiffDigits = 2)
print(result3, row.names = TRUE, right = FALSE)

result4 <- createTable1(covColorectalBefore, covColorectalAfter, output = "one column", showCounts = TRUE, showPercent = TRUE, percentDigits = 1, valueDigits = 1, stdDiffDigits = 2)
print(result4, row.names = TRUE, right = FALSE)

# save RData objects
save(result3, result4, covColorectalBefore, covColorectalDuring, covColorectalAfter, file = here("Results", db.name, "Colorectal", "ColorectalFeatureExtraction.RData"))


# need to add columns for after lockdown to the dataframe for before and during before making a flextable.
names(result3)[2] = "Count before lockdown"
names(result3)[3] = "% (n=6,025) before lockdown"
names(result3)[4] = "Count during lockdown"
names(result3)[5] = "% (n=349) during lockdown"
names(result3)[6] = "Std. Diff Before vs. during lockdown"
names(result4)[1] = "Characteristic repeat"
names(result4)[2] = "Count before lockdown"
names(result4)[3] = "% (n=6,025) before lockdown"
names(result4)[4] = "Count after lockdown"
names(result4)[5] = "% (n=2,234) after lockdown"
names(result4)[6] = "Std. Diff Before vs. after lockdown"

# because result3 doesn't have any data for age 15-19, it will not be possible to cbind result3 and 4 together as the number of rows don't match. Therefore
# add a row with this data in to result 3 and then re-order the rows so that this is in the same position as result 4. Then cbind.
result3_add_row <- result3
result3_add_row[nrow(result3_add_row) + 1,] = c("   15 -  19")
result3_add_row <- result3_add_row[c(1, 108, 2:107),]


Colorectal_combined_results <- cbind(result3_add_row, result4)

# remove superfluous columns
Colorectal_combined_results <- Colorectal_combined_results[-c(7,8,9)]
Colorectal_combined_results <- Colorectal_combined_results[c(1,2,3,4,5,7,8,6,9)]

Pretty_Colorectal_table <- flextable(Colorectal_combined_results) %>% theme_vanilla() %>% 
  set_caption(caption = "Feature extraction for Colorectal cancer cohorts before, during and after lockdown") %>% 
  width(width = 1.4) 

save_as_docx(
  "Colorectal_features_table" = Pretty_Colorectal_table, path = here("Results", db.name, "Colorectal", "Colorectal_feature_extraction_table.docx"))

# save the table as a csv file
write.csv(Colorectal_combined_results, here("Results", db.name, "Colorectal", "Colorectal_features_table.csv"), row.names = FALSE)

save_as_image(Pretty_Colorectal_table, path = here("Results", db.name, "Colorectal", "Colorectal_feature_extraction_table.png"))


print(paste0("- Feature extraction of colorectal cancer done"))
info(logger, "- Feature extraction of colorectal cancer done")


# LUNG CANCER BASELINE CHARACTERISTICS FOR TABLE 1 FROM FEATURE EXTRACTION PACKAGE

print(paste0("- Feature extraction of lung cancer"))
info(logger, "- Feature extraction of lung cancer")

covLungBefore <- getDbCovariateData(connectionDetails = connectionDetails,
                                          cdmDatabaseSchema = cdm_database_schema,
                                          cohortDatabaseSchema = results_database_schema,
                                          cohortTable = outcome_table_name_2,
                                          cohortId = 7,
                                          covariateSettings = settings,
                                          aggregated = TRUE)


covLungDuring <- getDbCovariateData(connectionDetails = connectionDetails,
                                   cdmDatabaseSchema = cdm_database_schema,
                                   cohortDatabaseSchema = results_database_schema,
                                   cohortTable = outcome_table_name_2,
                                   cohortId = 8,
                                   covariateSettings = settings,
                                   aggregated = TRUE)

covLungAfter <- getDbCovariateData(connectionDetails = connectionDetails,
                                         cdmDatabaseSchema = cdm_database_schema,
                                         cohortDatabaseSchema = results_database_schema,
                                         cohortTable = outcome_table_name_2,
                                         cohortId = 9,
                                         covariateSettings = settings,
                                         aggregated = TRUE)

std1 <- computeStandardizedDifference(covLungBefore, covLungDuring)
std2 <- computeStandardizedDifference(covLungBefore, covLungAfter)

head(std1)
head(std2)



# Lung Cancer Table 1 including standardised mean difference between the two cohorts
result5 <- createTable1(covLungBefore, covLungDuring, output = "one column", showCounts = TRUE, showPercent = TRUE, percentDigits = 1, valueDigits = 1, stdDiffDigits = 2)
print(result5, row.names = TRUE, right = FALSE)

result6 <- createTable1(covLungBefore, covLungAfter, output = "one column", showCounts = TRUE, showPercent = TRUE, percentDigits = 1, valueDigits = 1, stdDiffDigits = 2)
print(result6, row.names = TRUE, right = FALSE)

# save RData objects
save(result5, result6, covLungBefore, covLungDuring, covLungAfter, file = here("Results", db.name, "Lung", "LungFeatureExtraction.RData"))


# need to add columns for after lockdown to the dataframe for before and during before making a flextable.
names(result5)[2] = "Count before lockdown"
names(result5)[3] = "% (n=5,766) before lockdown"
names(result5)[4] = "Count during lockdown"
names(result5)[5] = "% (n=435) during lockdown"
names(result5)[6] = "Std. Diff Before vs. during lockdown"
names(result6)[1] = "Characteristic repeat"
names(result6)[2] = "Count before lockdown"
names(result6)[3] = "% (n=5,766) before lockdown"
names(result6)[4] = "Count after lockdown"
names(result6)[5] = "% (n=2,102) after lockdown"
names(result6)[6] = "Std. Diff Before vs. after lockdown"

# because result3 doesn't have any data for age 15-19, it will not be possible to cbind result3 and 4 together as the number of rows don't match. Therefore
# add a row with this data in to result 3 and then re-order the rows so that this is in the same position as result 4. Then cbind.
result5_add_row <- result5
result5_add_row[nrow(result5_add_row) + 1,] = c("   25 -  29")
result5_add_row <- result5_add_row[c(1:3, 108, 4:107),]


Lung_combined_results <- cbind(result5_add_row, result6)

# remove superfluous columns
Lung_combined_results <- Lung_combined_results[-c(7,8,9)]
Lung_combined_results <- Lung_combined_results[c(1,2,3,4,5,7,8,6,9)]


Pretty_Lung_table <- flextable(Lung_combined_results) %>% theme_vanilla() %>% 
  set_caption(caption = "Feature extraction for Lung cancer cohorts before, during and after lockdown") %>% 
  width(width = 1.4) 

save_as_docx(
  "Lung_features_table" = Pretty_Lung_table, path = here("Results", db.name, "Lung", "Lung_feature_extraction_table.docx"))

# save the table as a csv file
write.csv(Lung_combined_results, here("Results", db.name, "Lung", "Lung_features_table.csv"), row.names = FALSE)

save_as_image(Pretty_Lung_table, path = here("Results", db.name, "Lung", "Lung_feature_extraction_table.png"))


print(paste0("- Feature extraction of Lung cancer done"))
info(logger, "- Feature extraction of Lung cancer done")




# PROSTATE CANCER BASELINE CHARACTERISTICS FOR TABLE 1 FROM FEATURE EXTRACTION PACKAGE

print(paste0("- Feature extraction of prostate cancer"))
info(logger, "- Feature extraction of prostate cancer")

covProstateBefore <- getDbCovariateData(connectionDetails = connectionDetails,
                                    cdmDatabaseSchema = cdm_database_schema,
                                    cohortDatabaseSchema = results_database_schema,
                                    cohortTable = outcome_table_name_2,
                                    cohortId = 10,
                                    covariateSettings = settings,
                                    aggregated = TRUE)

covProstateDuring <- getDbCovariateData(connectionDetails = connectionDetails,
                                       cdmDatabaseSchema = cdm_database_schema,
                                       cohortDatabaseSchema = results_database_schema,
                                       cohortTable = outcome_table_name_2,
                                       cohortId = 11,
                                       covariateSettings = settings,
                                       aggregated = TRUE)

covProstateAfter <- getDbCovariateData(connectionDetails = connectionDetails,
                                   cdmDatabaseSchema = cdm_database_schema,
                                   cohortDatabaseSchema = results_database_schema,
                                   cohortTable = outcome_table_name_2,
                                   cohortId = 12,
                                   covariateSettings = settings,
                                   aggregated = TRUE)

std1 <- computeStandardizedDifference(covProstateBefore, covProstateDuring)
std2 <- computeStandardizedDifference(covProstateBefore, covProstateAfter)

head(std1)
head(std2)


# Prostate Cancer Table 1 including standardised mean difference between the two cohorts
result7 <- createTable1(covProstateBefore, covProstateDuring, output = "one column", showCounts = TRUE, showPercent = TRUE, percentDigits = 1, valueDigits = 1, stdDiffDigits = 2)
print(result7, row.names = TRUE, right = FALSE)

result8 <- createTable1(covProstateBefore, covProstateAfter, output = "one column", showCounts = TRUE, showPercent = TRUE, percentDigits = 1, valueDigits = 1, stdDiffDigits = 2)
print(result8, row.names = TRUE, right = FALSE)

# save RData objects
save(result7, result8, covProstateBefore, covProstateDuring, covProstateAfter, file = here("Results", db.name, "Prostate", "ProstateFeatureExtraction.RData"))

# need to add columns for after lockdown to the dataframe for before and during before making a flextable.
names(result7)[2] = "Count before lockdown"
names(result7)[3] = "% (n=8,103) before lockdown"
names(result7)[4] = "Count during lockdown"
names(result7)[5] = "% (n=427) during lockdown"
names(result7)[6] = "Std. Diff Before vs. during lockdown"
names(result8)[1] = "Characteristic repeat"
names(result8)[2] = "Count before lockdown"
names(result8)[3] = "% (n=8,103) before lockdown"
names(result8)[4] = "Count after lockdown"
names(result8)[5] = "% (n=2,477) after lockdown"
names(result8)[6] = "Std. Diff Before vs. after lockdown"

# because result3 doesn't have any data for age 15-19, it will not be possible to cbind result3 and 4 together as the number of rows don't match. Therefore
# add a row with this data in to result 3 and then re-order the rows so that this is in the same position as result 4. Then cbind.
result7_add_row <- result7
result7_add_row[nrow(result7_add_row) + 1,] = c("  Viral hepatitis C")
result7_add_row <- result7_add_row[c(1:41, 108, 42:107),]


Prostate_combined_results <- cbind(result7_add_row, result8)

# remove superfluous columns
Prostate_combined_results <- Prostate_combined_results[-c(7,8,9)]
Prostate_combined_results <- Prostate_combined_results[c(1,2,3,4,5,7,8,6,9)]

Pretty_Prostate_table <- flextable(Prostate_combined_results) %>% theme_vanilla() %>% 
  set_caption(caption = "Feature extraction for Prostate cancer cohorts before, during and after lockdown") %>% 
  width(width = 1.4) 

save_as_docx(
  "Prostate_features_table" = Pretty_Prostate_table, path = here("Results", db.name, "Prostate", "Prostate_feature_extraction_table.docx"))

# save the table as a csv file
write.csv(Prostate_combined_results, here("Results", db.name, "Prostate", "Prostate_features_table.csv"), row.names = FALSE)

save_as_image(Pretty_Prostate_table, path = here("Results", db.name, "Prostate", "Prostate_feature_extraction_table.png"))


print(paste0("- Feature extraction of Prostate cancer done"))
info(logger, "- Feature extraction of Prostate cancer done")



print(paste0("- 6. Standardized Feature Extraction done"))
info(logger, "- 6. Standardized Feature Extraction done")
