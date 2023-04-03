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

cdm$concept %>% filter(concept_id == 4197459)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4197459)

# READ CODE SEARCH. SEARCH THE FOLLOWING AND SEE WHERE IT MAPS TO:
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 45522751)



#------------ 2. Referral to mammography clinic -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4086282)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4086282)

# READ CODE SEARCH. SEARCH THE FOLLOWING AND SEE WHERE IT MAPS TO:
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id ==  45449357)

# then in the new database you can check the counts of the concept ids and populate the table with these counts

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45449357)





#------------ 3. Fasttrack referral for suspected breast cancer -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 44791272)

cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 44791272) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44791272)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 44791272)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45472960)





#------------ 4. Referral to breast surgeon -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4141840)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4141840)




#------------ 5. Seen in breast clinic -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4089031)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4089031)


# codelist generator indicated possibility of including 44793351 Seen in fast track suspected breast cancer clinic. Check counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44793351)



#------------ 6. Seen by breast surgeon -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4136626)

cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4136626) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4136626)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4136626)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45428053)


# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4136626)


#------------ 7. Fast track referral for lung cancer -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 44791283)


cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 44791283) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44791283)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 44791283)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45469670)


# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44791283)



#------------ 8. PSA monitoring -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4215705)


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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4215705)


# READ CODE SEARCH. SEARCH THE FOLLOWING AND SEE WHERE IT MAPS TO:
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id ==  45483071 )

# then in the new database you can check the counts of the concept ids and populate the table with these counts

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45483071 )

cdm$concept %>% filter(concept_id == 45483071)


#------------ 9. Screening mammography -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4077697)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4077697)

# codelist generator indicated possibility of including 2211814, 40218347, 42627987, 42737560, 45888264,36713187,36713191,36713192
# - none of these are mapped

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 2211814)
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 40218347)
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 42627987)
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 42737560)
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 45888264)
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 36713187)
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 36713191)
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 36713192)


tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 45422834)


cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45422834)
#------------ 10. Diagnostic mammograms (mammography)-----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4324693)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4324693)

# codelist generator indicated possibility of including 2211813,2211814,42628028,42627940,42627987,40218348,40218349,40218347,42737560 

# - none of these are mapped

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 2211813)
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 2211814)
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 42628028)
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 42627940)
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 42627987)
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 40218348)
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 40218349)
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 40218347)
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 42737560)



#------------ 11. Biopsy of breast -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4047494)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4047494)


#------------ 12. Stereotactically guided core needle biopsy of breast -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4022932)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4022932)


# codelist generator indicated possibility of including 44791926,44791927,44792005,44792006

# - none of these are mapped

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44791926)
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44791927)
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44792005)
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44792006)




#------------ 13. Percutaneous needle biopsy of breast -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4028790)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4028790)


#------------ 14. Fine needle aspiration of breast -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4306207)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4306207)


#------------ 15. Wire guided local excision of breast lump -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4216180)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4216180)



# READ CODE SEARCH. SEARCH THE FOLLOWING AND SEE WHERE IT MAPS TO:
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id ==  45435560 )

# then in the new database you can check the counts of the concept ids and populate the table with these counts

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45435560 )

cdm$concept %>% filter(concept_id == 45435560)



#------------ 16. Excision of mammary duct -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4146780)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4146780)



#------------ 17. Wide local excision of breast lesion -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4129190)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4129190)



#------------ 18. Excision of lesion of breast -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4194124)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4194124)


#------------ 19. Excision of breast tissue -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept %>% filter(concept_id == 4286804)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4286804)



#------------ 20. Ultrasonography of intestine -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4082528)

cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4082528) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4082528)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4082528)



# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4082528)



#------------ 21. Colonoscopies -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept %>% filter(concept_id == 4249893)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4249893)




#------------ 22. Sigmoidoscopy -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4087381)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4087381)

# additional code identified from hierarchy on atlas
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4196378)


# READ CODE SEARCH. SEARCH THE FOLLOWING AND SEE WHERE IT MAPS TO:
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id ==  45882974 )

# then in the new database you can check the counts of the concept ids and populate the table with these counts

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45882974 )

cdm$concept %>% filter(concept_id == 45882974) 

# READ CODE SEARCH. SEARCH THE FOLLOWING AND SEE WHERE IT MAPS TO:
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id ==  4196387 )

# then in the new database you can check the counts of the concept ids and populate the table with these counts

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4196387 )

cdm$concept %>% filter(concept_id == 4196387) 

#------------ 23. Ultrasound of gastrointestinal tract -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:


cdm$concept %>% filter(concept_id == 4125529)

cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4125529) %>% print(n=Inf)
 
# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4125529)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4125529)

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4125529)


# READ CODE SEARCH. SEARCH THE FOLLOWING AND SEE WHERE IT MAPS TO:
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id ==  45418956  )

# then in the new database you can check the counts of the concept ids and populate the table with these counts

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45418956  )

cdm$concept %>% filter(concept_id == 45418956 )

cdm$concept %>% filter(concept_id == 4061134 )
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id ==  4061134  )

#------------ 24. Ultrasonography of abdomen -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:


cdm$concept %>% filter(concept_id == 4261497)
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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4261497)




#------------ 25. Ultrasonography of rectum -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 2787168)
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 2787168) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 2787168)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 0)


# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 2787168)


# codelist generator identified 40489305 which is a descendent of biopsy of rectum (4184601). See how many counts there are:
# none
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4184601)


#------------ 26. Bronchoscopy -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:


cdm$concept %>% filter(concept_id == 4032404)
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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4032404)

# READ CODE SEARCH. SEARCH THE FOLLOWING AND SEE WHERE IT MAPS TO:
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id ==  36309334   )

# then in the new database you can check the counts of the concept ids and populate the table with these counts

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 36309334   )

cdm$concept %>% filter(concept_id == 36309334  )

cdm$concept %>% filter(concept_id == 36309334  )
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id ==  4061134  )

#------------ 27. Endobronchial ultrasonography guided transbronchial needle aspiration -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 44809038)
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 44809038) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44809038)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 44809038 )

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44809038)



#------------ 28. Mediastinoscopy -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4128302)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4128302)

# READ CODE SEARCH. SEARCH THE FOLLOWING AND SEE WHERE IT MAPS TO:
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id ==  45485702  )

# then in the new database you can check the counts of the concept ids and populate the table with these counts

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45485702  )

cdm$concept %>% filter(concept_id == 45485702 )

# the above read code maps to this code
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4070986  )
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id ==  4070986  )
cdm$concept %>% filter(concept_id == 4070986 )
#------------ 29. CT and biopsy of chest -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4304406)

cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4304406) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4304406) %>% print(n=Inf)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4304406 )

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4304406)


#------------ 30. US scan and biopsy of chest -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4167553)

cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4167553) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4167553) %>% print(n=Inf)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4167553 )

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4167553)


#------------ 31. Diagnostic Radiology (Diagnostic Imaging) Procedures of the Chest -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 45889178)

cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 45889178) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 45889178) %>% print(n=Inf)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 0 )

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 45889178)




#------------ 32. MRI of chest -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4246485)

cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4246485) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4246485) %>% print(n=Inf)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4246485 )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45426099 )
# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4246485)



#------------ 33. Biopsy of prostate -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 4278515)
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 4278515) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4278515) %>% print(n=Inf)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 4278515  )
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45529498 )
# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4278515)

# READ CODE SEARCH. SEARCH THE FOLLOWING AND SEE WHERE IT MAPS TO:
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id ==  45515804   )

# then in the new database you can check the counts of the concept ids and populate the table with these counts

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45515804   )

cdm$concept %>% filter(concept_id == 45515804  )

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id ==  4061134  )

# this maps to:
cdm$concept %>% filter(concept_id == 4073010  )

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id ==  4073010  )



#------------ 34. Diagnostic mammogram and ultrasound-L -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 36203740)
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 36203740) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 36203740) %>% print(n=Inf)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 36203740)

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 36203740)


#------------ 35. Diagnostic mammogram and ultrasound-R -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 36203750)
cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 36203750) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 36203750) %>% print(n=Inf)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 36203750)

# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 36203750)



#------------ 36. Bowel cancer screening programme -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 44791543)

cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 44791543) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44791543) %>% print(n=Inf)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 44791543)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45482304)
# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 44791543)

# READ CODE SEARCH. SEARCH THE FOLLOWING AND SEE WHERE IT MAPS TO:
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id ==  45482304)

# then in the new database you can check the counts of the concept ids and populate the table with these counts

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45482304     )

cdm$concept %>% filter(concept_id == 45482304    )

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id ==  45482304    )



#------------ 37. Quantitative faecal immunochemical tests -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:

cdm$concept %>% filter(concept_id == 37395561)

cdm$concept_relationship %>% filter(relationship_id == "Mapped from") %>% filter(concept_id_1 == 37395561) %>% print(n=Inf)

# then in the original database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 37395561) %>% print(n=Inf)

# TO DO IN THE NEW DATABASE 012022

# copy these resulting concept IDs (source concept codes) into the word table, then in the 2022 database search for the new
# list of concept ids and select 'maps to' and insert the concept_id_2 into the word document to show the new concept_id

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 1397752)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 37395561 )
# then in the new database you can check the counts of the concept ids and populate the table with these counts

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 37395561)

#looking through the hierarchy brings up this code 1397752 which is the READ code equivalent for Quantitative faecal immunochemical test
# which appears to have two mappings in 202201 database, which 64760 subjects
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 1397752)
cdm$concept %>% filter(concept_id == 1397752)
cdm$concept %>% filter(concept_id == 37395561)
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 37395561)

1397752


#------------ 38. Prostate specific antigen measurement -----------

# TO DO IN THE ORIGINAL CPRD GOLD 0720 DATABASE

# to find the source values that the concept_id of interest was mapped from, run the following. The concept_id_2 column shows ALL the source concept
# codes that were mapped to your code of interest:
cdm$concept %>% filter(concept_id == 4272032)

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

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id == 4272032)

# READ CODE SEARCH. SEARCH THE FOLLOWING AND SEE WHERE IT MAPS TO:
tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id ==  45422026    )

# then in the new database you can check the counts of the concept ids and populate the table with these counts

cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 37398806    )

cdm$concept %>% filter(concept_id == 37398806   )

tbl(db, sql("SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")) %>% filter(concept_id ==  37398806   )







################### FROM COHORT DIAGNOSTICS, I WILL CHECK THE FOLLOWING ORPHAN CODE #################################

### BRONCHOSCOPY

# Bronchoscopy normal - READ CODE - THIS MAPS TO 4065516  BRONCHOSCOPY NORMAL (CONDITION)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45428535)

# Bronchoscopy abnormal - READ CODE - THIS MAPS TO 4065519  BRONCHOSCOPY ABNORMAL (CONDITION)
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45495175)

### BOWEL CANCER SCREENING PROGRAMME 

# Bowel cancer screening programme: faecal occult blood result - READ CODE - THIS MAPS TO 44792842
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45419060)


### COLONOSCOPY

# Colonoscopy Study observation - LOINCCODE - THIS MAPS TO 3022145
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 3022145)

# Diagnostic Colonoscopy - READ CODE - THIS MAPS TO 4068278
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45429037)

# Colonoscopy normal - READ CODE - THIS MAPS TO 4197456
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45435170)

# Colonoscopy ABnormal - READ CODE - THIS MAPS TO 4208962
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45428536)


### MAMMOGRAMS

# Breast neoplasm screen - READ CODE - THIS MAPS TO 4147961
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45485601)

# Mammography normal - READ CODE - THIS MAPS TO 4059048
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45492145)

# Breast neoplasm screen normal - READ CODE - THIS MAPS TO 4062647
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45492261)

# Breast neoplasm screen ABnormal - READ CODE - THIS MAPS TO 4062648
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45438831)

# Mammography attended - READ CODE - THIS MAPS TO 44813155
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45451970)

# [D]Mammogram abnormal - READ CODE - THIS MAPS TO 4059049
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45437276)



### PROSTATE SPECIFIC ANTIGEN

# Total prostate specific antigen level - READ CODE - THIS MAPS TO 37392634
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45482021)

# Serum prostate specific antigen level - READ CODE - THIS MAPS TO 37392151
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45518548)

# Prostate specific antigen normal - READ CODE - THIS MAPS TO 4013978 
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45455113)

# PSA - Serum prostate specific antigen level - READ CODE - THIS MAPS TO 37398806 
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45441837)

# PSA - Prostate specific antigen threshold for referral - READ CODE - THIS MAPS TO 45769987 
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45773811)

# Free prostate specific antigen level - READ CODE - THIS MAPS TO 37394310
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45478701)

# Serum free prostate specific antigen level - READ CODE - THIS MAPS TO 37392159
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45491972)

# Prostate specific antigen abnormal - READ CODE - THIS MAPS TO 439453
cdm$concept_relationship %>% filter(relationship_id == "Maps to") %>% filter(concept_id_1 == 45418775)
