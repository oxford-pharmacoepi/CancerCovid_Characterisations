# CODE FOR CHECKING COVARIATE IDS BETWEEN CPRD_GOLD AND CPRD_GOLD_012022
# THIS IS BECAUSE THE MAPPING HAS BEEN PERFORMED WITH A DIFFERENT VOCABULARY

original_concept_ids <- read_csv(here("extras", "Concept_ids_table.csv"))

original_concept_ids

# Connect to the original database to find the source code of the original concept ids
# Specify databaseConnector connection details -----
# database connection details
# connect to database
user        <-  Sys.getenv("DB_USER")
password    <-  Sys.getenv("DB_PASSWORD")
port        <-  Sys.getenv("DB_PORT") 
host        <-  Sys.getenv("DB_HOST") 
server_dbi  <-  Sys.getenv("DB_SERVER_DBI_cdmgold202007")
server      <-  Sys.getenv("DB_SERVER_cdmgold202007")

# Specify cdm_reference via DBI connection details -----
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below (see https://dbi.r-dbi.org/articles/dbi for more details)
# you may need to install another package for this (although RPostgres is included with renv in case you are using postgres)
db <- dbConnect(RPostgres::Postgres(),
                dbname = server_dbi,
                port = port,
                host = host, 
                user = user, 
                password = password)

# Set database details -----
# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema<-"public"

# The name of the schema that contains the vocabularies 
# (often this will be the same as cdm_database_schema)
vocabulary_database_schema<-cdm_database_schema

# The name of the schema where results tables will be created 
results_database_schema<-"results"


# create cdm reference ----
cdm <- CDMConnector::cdm_from_con(con = db, 
                                  cdm_schema = cdm_database_schema,
                                  write_schema = results_database_schema)


# to check whether the DBI connection is correct, 
# running the next line should give you a count of your person table
cdm$person %>% 
  tally()


# find the concept ids in the procedure table using the observation_source_concept_id / procedure_source_concept_id / procedure_source_concept_id 

# TEST referral to breast clinic
# Identify the source code of a concept_id - BUT NOTE THAT THIS WILL NOT INITIALL SHOW YOU ALL THE POSSIBLE CODES THE MAPPED TO THIS CONCEPT
referral_breast_clinic_check <- cdm$observation %>% 
  select(observation_concept_id, observation_source_value, observation_source_concept_id) %>%
  filter(observation_concept_id == 4197459)

# look for all the distinct values of the concept relationship table to identify the name of the value that either maps to or from a concept
cdm$concept_relationship %>% select(relationship_id) %>% distinct() %>% pull()


# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4197459)





#------------ 1. REFERRAL TO BREAST CLINIC -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4197459)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4197459)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4197459)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40317387)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40383207)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45522751)

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4197459)# change concept code



#------------ 2. Referral to mammography clinic -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4086282)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4086282)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4086282)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40317376)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45449357)


# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4086282)# change concept code


#------------ 3. Fasttrack referral for suspected breast cancer -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 44791272)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44791272)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 44791272)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45472960)



# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4086282)# change concept code



#------------ 4. Referral to breast surgeon -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4141840)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4141840)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4141840)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45436133)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45451276)


# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4086282)# change concept code

#------------ 5. Seen in breast clinic -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4089031)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4089031)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4089031)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40323769)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40378544)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45449441)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45462919)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45528623)

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4086282)# change concept code



#------------ 6. Seen by breast surgeon -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4136626)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4136626)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4136626)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45428053)


# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4086282)# change concept code



#------------ 7. Fast track referral for lung cancer -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 44791283)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44791283)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 44791283)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45469670)


# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4086282)# change concept code



#------------ 8. PSA monitoring -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4215705)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4215705)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4215705)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40563066)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40566211)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45489505)

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4086282)# change concept code


#------------ 9. Screening mammography -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4077697)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4077697)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4077697)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40309745)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40339178)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40360284)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 44823895)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 44829677)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 44836711)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 44836712)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45508743)
# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4086282)# change concept code


#------------ 10. Diagnostic mammograms -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4324693)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4324693)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4324693)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40315403)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45422834)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45465450)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45472213)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45482173)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45532789)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45563331)

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4086282)# change concept code


#------------ 11. Biopsy of breast -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4047494)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4047494)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4047494)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4066549)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4066551)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4068296)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45422335)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45435565)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45442167)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45455476)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45526548)

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4086282)# change concept code
