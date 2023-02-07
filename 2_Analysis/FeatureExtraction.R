# ============================================================================ #
#                       STANDARD OHDSI FEATURE EXTRACTION                      #
#                              OF CANCER COHORTS                               #
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
                                      cohortTable = outcome_table_name_1,
                                      cohortId = 2,
                                      covariateSettings = settings,
                                      aggregated = TRUE)

covBreastAfter <- getDbCovariateData(connectionDetails = connectionDetails,
                                      cdmDatabaseSchema = cdm_database_schema,
                                      cohortDatabaseSchema = results_database_schema,
                                      cohortTable = outcome_table_name_1,
                                      cohortId = 1,
                                      covariateSettings = settings,
                                      aggregated = TRUE)


std <- computeStandardizedDifference(covBreastBefore, covBreastAfter)

head(std)

 # Breast Cancer Table 1 including standardised mean difference between the two cohorts
result <- createTable1(covBreastBefore, covBreastAfter, output = "one column")
print(result, row.names = TRUE, right = FALSE)


Pretty_breast_table <- flextable(result) %>% theme_vanilla() %>% 
  set_caption(caption = "Feature extraction for breast cancer cohorts before and after lockdown") %>% 
  width(width = 1.4) 

save_as_docx(
  "Breast_features_table" = Pretty_breast_table, path = here("Results", db.name, "Breast", "Breast_feature_extraction_table.docx"))

save_as_image(Pretty_breast_table, path = here("Results", db.name, "Breast", "Breast_feature_extraction_table.png"))

print(paste0("- Feature extraction of breast cancers done"))
info(logger, "- Feature extraction of breast cancers done")

## 2. COLORECTAL CANCER BASELINE CHARACTERISTICS FOR TABLE 1 FROM FEATURE EXTRACTION PACKAGE

print(paste0("- Feature extraction of colorectal cancers"))
info(logger, "- Feature extraction of colorectal cancers")

covColorectalBefore <- getDbCovariateData(connectionDetails = connectionDetails,
                                      cdmDatabaseSchema = cdm_database_schema,
                                      cohortDatabaseSchema = results_database_schema,
                                      cohortTable = outcome_table_name_1,
                                      cohortId = 4,
                                      covariateSettings = settings,
                                      aggregated = TRUE)

covColorectalAfter <- getDbCovariateData(connectionDetails = connectionDetails,
                                     cdmDatabaseSchema = cdm_database_schema,
                                     cohortDatabaseSchema = results_database_schema,
                                     cohortTable = outcome_table_name_1,
                                     cohortId = 3,
                                     covariateSettings = settings,
                                     aggregated = TRUE)

std <- computeStandardizedDifference(covColorectalBefore, covColorectalAfter)

head(std)
dim(std)


# Colorectal Cancer Table 1 including standardised mean difference between the two cohorts
result2 <- createTable1(covColorectalBefore, covColorectalAfter, output = "one column")
print(result2, row.names = TRUE, right = FALSE)


Pretty_colorectal_table <- flextable(result2) %>% theme_vanilla() %>% 
  set_caption(caption = "Feature extraction for colorectal cancer cohorts before and after lockdown") %>% 
  width(width = 1.4) 

save_as_docx(
  "Colorectal_features_table" = Pretty_colorectal_table, path = here("Results", db.name, "Colorectal", "Colorectal_feature_extraction_table.docx"))

save_as_image(Pretty_colorectal_table, path = here("Results", db.name, "Colorectal", "Colorectal_feature_extraction_table.png"))


print(paste0("- Feature extraction of colorectal cancer done"))
info(logger, "- Feature extraction of colorectal cancer done")


# LUNG CANCER BASELINE CHARACTERISTICS FOR TABLE 1 FROM FEATURE EXTRACTION PACKAGE

print(paste0("- Feature extraction of lung cancer"))
info(logger, "- Feature extraction of lung cancer")

covLungBefore <- getDbCovariateData(connectionDetails = connectionDetails,
                                          cdmDatabaseSchema = cdm_database_schema,
                                          cohortDatabaseSchema = results_database_schema,
                                          cohortTable = outcome_table_name_1,
                                          cohortId = 6,
                                          covariateSettings = settings,
                                          aggregated = TRUE)

covLungAfter <- getDbCovariateData(connectionDetails = connectionDetails,
                                         cdmDatabaseSchema = cdm_database_schema,
                                         cohortDatabaseSchema = results_database_schema,
                                         cohortTable = outcome_table_name_1,
                                         cohortId = 5,
                                         covariateSettings = settings,
                                         aggregated = TRUE)

std <- computeStandardizedDifference(covLungBefore, covLungAfter)

head(std)
dim(std)



# Lung Cancer Table 1 including standardised mean difference between the two cohorts
result3 <- createTable1(covLungBefore, covLungAfter, output = "one column")
print(result3, row.names = TRUE, right = FALSE)


Pretty_lung_table <- flextable(result3) %>% theme_vanilla() %>% 
  set_caption(caption = "Feature extraction for lung cancer cohorts before and after lockdown") %>% 
  width(width = 1.4) 

save_as_docx(
  "Lung_features_table" = Pretty_lung_table, path = here("Results", db.name, "Lung", "Lung_feature_extraction_table.docx"))

save_as_image(Pretty_lung_table, path = here("Results", db.name, "Lung", "Lung_feature_extraction_table.png"))

print(paste0("- Feature extraction of lung cancer done"))
info(logger, "- Feature extraction of lung cancer done")


# PROSTATE CANCER BASELINE CHARACTERISTICS FOR TABLE 1 FROM FEATURE EXTRACTION PACKAGE

print(paste0("- Feature extraction of prostate cancer"))
info(logger, "- Feature extraction of prostate cancer")

covProstateBefore <- getDbCovariateData(connectionDetails = connectionDetails,
                                    cdmDatabaseSchema = cdm_database_schema,
                                    cohortDatabaseSchema = results_database_schema,
                                    cohortTable = outcome_table_name_1,
                                    cohortId = 8,
                                    covariateSettings = settings,
                                    aggregated = TRUE)

covProstateAfter <- getDbCovariateData(connectionDetails = connectionDetails,
                                   cdmDatabaseSchema = cdm_database_schema,
                                   cohortDatabaseSchema = results_database_schema,
                                   cohortTable = outcome_table_name_1,
                                   cohortId = 7,
                                   covariateSettings = settings,
                                   aggregated = TRUE)

std <- computeStandardizedDifference(covProstateBefore, covProstateAfter)

head(std)
dim(std)


# Prostate Cancer Table 1 including standardised mean difference between the two cohorts
result4 <- createTable1(covProstateBefore, covProstateAfter, output = "one column")
print(result4, row.names = TRUE, right = FALSE)


Pretty_prostate_table <- flextable(result4) %>% theme_vanilla() %>% 
  set_caption(caption = "Feature extraction for prostate cancer cohorts before and after lockdown") %>% 
  width(width = 1.4) 

save_as_docx(
  "Prostate_features_table" = Pretty_prostate_table, path = here("Results", db.name, "Prostate", "Prostate_feature_extraction_table.docx"))

save_as_image(Pretty_prostate_table, path = here("Results", db.name, "Prostate", "Prostate_feature_extraction_table.png"))

print(paste0("- Feature extraction of prostate cancer done"))
info(logger, "- Feature extraction of prostate cancer done")

print(paste0("- 6. Standardized Feature Extraction done"))
info(logger, "- 6. Standardized Feature Extraction done")
