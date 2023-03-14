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
#server_dbi  <-  Sys.getenv("DB_SERVER_DBI_cdmgold202007")
#server      <-  Sys.getenv("DB_SERVER_cdmgold202007")

server_dbi  <-  Sys.getenv("DB_SERVER_DBI_cdm_gold_202201")
server      <-  Sys.getenv("DB_SERVER_cdm_gold_202201")

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
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4197459) %>% print(n=Inf)

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
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4086282) %>% print(n=Inf)

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
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 44791272) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44791272)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 44791272)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45472960)



# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44791272)# change concept code



#------------ 4. Referral to breast surgeon -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4141840) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4141840)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4141840)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45436133)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45451276)


# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4141840)# change concept code

#------------ 5. Seen in breast clinic -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4089031) %>% print(n=Inf)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4089031)# change concept code



#------------ 6. Seen by breast surgeon -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4136626) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4136626)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4136626)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45428053)


# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4136626)# change concept code



#------------ 7. Fast track referral for lung cancer -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 44791283) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44791283)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 44791283)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45469670)


# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44791283)# change concept code



#------------ 8. PSA monitoring -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4215705) %>% print(n=Inf)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4215705)# change concept code


#------------ 9. Screening mammography -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4077697) %>% print(n=Inf)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4077697)# change concept code


#------------ 10. Diagnostic mammograms -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4324693) %>% print(n=Inf)
 
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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4324693)# change concept code


#------------ 11. Biopsy of breast -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4047494) %>% print(n=Inf)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4047494)# change concept code


#------------ 12. Stereotactically guided core needle biopsy of breast -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4022932) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4022932)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4022932)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40587841)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45422334)

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4022932)# change concept code






#------------ 13. Percutaneous needle biopsy of breast -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4028790) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4028790)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 2006379)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4028790)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45422333)
# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4028790)# change concept code


#------------ 14. Fine needle aspiration of breast -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4306207) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4306207)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4306207)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40564303)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45428931)
# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4306207)# change concept code


#------------ 15. Wire guided local excision of breast lump -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4216180) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4216180)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4216180)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40566202)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45489000)
# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4216180)# change concept code


#------------ 16. Excision of mammary duct -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4146780) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4146780)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4144568)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4146780)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45502202)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45508791)
# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4146780)# change concept code



#------------ 17. Wide local excision of breast lesion -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4129190) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4129190)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4129190)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40335303)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45452115)

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4129190)# change concept code



#------------ 18. Excision of lesion of breast -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4194124) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4194124)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4194124)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40309819)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40616700)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45495546)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45522164)


# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4194124)# change concept code


#------------ 19. Excision of breast tissue -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4286804) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4286804)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 2006416)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4119576)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4286804)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40309829)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45448764)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45505486)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45515575)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45526545)


# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4286804)# change concept code



#------------ 20. Ultrasonography of intestine -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4082528) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4082528)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4082528)



# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4082528)# change concept code



#------------ 21. Colonoscopies -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4249893) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4249893)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 2002703)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4249893)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40299988)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40352176)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40538125)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40637497)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45530496)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45754739)


# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4249893)# change concept code




#------------ 22. Sigmoidoscopy -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4087381) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4087381)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4087381)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40299989 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40504911 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45482453 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45485757 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45527530 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45754738 )




# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4087381)# change concept code





#------------ 23. Ultrasound of gastrointestinal tract -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4125529) %>% print(n=Inf)
 
# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4125529)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4125529)

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4125529)# change concept code



#------------ 24. Ultrasonography of abdomen -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4261497) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4261497)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4261497)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40309065)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45419619 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45462095 )

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4261497)# change concept code




#------------ 25. Ultrasonography of rectum -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 2787168) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 2787168)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 0)


# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 2787168)# change concept code



#------------ 26. Bronchoscopy -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4032404) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4032404)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4032404 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4335465 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4337590 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4339604 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40272940 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40299977 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40340315 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45438921 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45452166 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45479086 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45529457 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45754741 )

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4032404)# change concept code



#------------ 27. Endobronchial ultrasonography guided transbronchial needle aspiration -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 44809038) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44809038)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 44809038 )

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44809038)# change concept code



#------------ 28. Mediastinoscopy -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4128302) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4128302) %>% print(n=Inf)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 2001401 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4069743)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4128302 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4138736)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4173652)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40296583)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40340831)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45435623)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45442224)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45482408)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45527592)
# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4128302)# change concept code



#------------ 29. CT and biopsy of chest -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4304406) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4304406) %>% print(n=Inf)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4304406 )

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4304406)# change concept code


#------------ 30. US scan and biopsy of chest -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4167553) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4167553) %>% print(n=Inf)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4167553 )

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4167553)# change concept code


#------------ 31. Diagnostic Radiology (Diagnostic Imaging) Procedures of the Chest -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 45889178) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 45889178) %>% print(n=Inf)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 0 )

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 45889178)# change concept code




#------------ 32. MRI of chest -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4246485) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4246485) %>% print(n=Inf)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4246485 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45426099 )
# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4246485)# change concept code



#------------ 33. Biopsy of prostate -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4278515) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4278515) %>% print(n=Inf)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4278515  )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45529498 )
# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4278515)# change concept code




#------------ 34. Diagnostic mammogram and ultrasound-L -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 36203740) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 36203740) %>% print(n=Inf)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 36203740)

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 36203740)# change concept code


#------------ 35. Diagnostic mammogram and ultrasound-R -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 36203750) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 36203750) %>% print(n=Inf)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 36203750)

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 36203750)# change concept code



#------------ 36. Bowel cancer screening programme -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 44791543) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44791543) %>% print(n=Inf)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 44791543)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45482304)
# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44791543)# change concept code


#------------ 37. Quantitative faecal immunochemical tests -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 37395561) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 37395561) %>% print(n=Inf)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 1397752)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 37395561 )
# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 37395561)# change concept code




#------------ 38. Prostate specific antigen measurement -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4272032) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4272032) %>% print(n=Inf)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4272032 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 40351156  )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 44835093 )
# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4272032)# change concept code























