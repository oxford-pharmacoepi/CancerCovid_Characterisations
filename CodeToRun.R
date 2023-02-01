# ============================================================================ #
#          CODE TO RUN FOR CANCER/COVID CUSTOM CHARACTERISATIONS               #
#                 GENERAL POPULATION AND CANCER POPULATIONS                    #
#                                Nicola Barclay                                #
#                                 01-02-2023                                   #
#                                                                              #
#             THIS SHOULD BE THE ONLY FILE YOU NEED TO INTERACT WITH           #
#                                                                              #
# ============================================================================ #



## ----------------------------- LOAD PACKAGES ------------------------------ ##

# load r packages
library(CirceR)
library(here)
library(DBI)
library(dbplyr)
library(dplyr)
library(readr)
library(log4r)
library(tidyr)
library(stringr)
library(CDMConnector)
library(ggplot2)
library(RPostgres)
library(lubridate)
library(flextable)
library(glue)
library(tableone)
library(glmnet)
library(survey)


# database metadata and connection details -----
# The name/ acronym for the database
db.name<-"CPRDGold"

# Set output folder locations -----
# the paths to the folders where the results from this analysis will be saved
output.folder1<-here("Results", db.name, "Breast")
output.folder2<-here("Results", db.name, "Colorectal")
output.folder3<-here("Results", db.name, "Lung")
output.folder4<-here("Results", db.name, "Prostate")

# Specify databaseConnector connection details -----
# database connection details
# connect to database
user        <-  Sys.getenv("DB_USER")
password    <-  Sys.getenv("DB_PASSWORD")
port        <-  Sys.getenv("DB_PORT") 
host        <-  Sys.getenv("DB_HOST") 
server_dbi  <-  Sys.getenv("DB_SERVER_DBI_cdmgold202007")

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

# Name of outcome and strata tables in the result table where the outcome and strata cohorts will be stored
# Note, if there is an existing table in your results schema with the same names
# it will be overwritten 

outcome_table_name_1 <- "DenominatorBeforeAfterLockdown" # this is the four cancers
outcome_table_name_2 <- "CancersBeforeAfterLockdown" # this is the table for the endocrine treatments


# create cdm reference ----
cdm <- CDMConnector::cdm_from_con(con = db, 
                                  cdm_schema = cdm_database_schema,
                                  write_schema = results_database_schema)


# to check whether the DBI connection is correct, 
# running the next line should give you a count of your person table
cdm$person %>% 
  tally()

# Run the study ------
source(here("RunStudy.R"))
# after the study is run you should have a zip folder in your output folder to share