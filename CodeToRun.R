# ============================================================================ #
#          CODE TO RUN FOR CANCER/COVID CUSTOM CHARACTERISATIONS               #
#                 GENERAL POPULATION AND CANCER POPULATIONS                    #
#                                Nicola Barclay                                #
#                                 01-02-2023                                   #
#                                                                              #
#             THIS SHOULD BE THE ONLY FILE YOU NEED TO INTERACT WITH           #
#                                                                              #
# ============================================================================ #

# Load packages ------
#renv::activate()
#renv::restore()

install.packages("remotes")
remotes::install_github("ohdsi/CirceR")

install.packages("drat")
drat::addRepo("OHDSI")
install.packages("FeatureExtraction")

# install.packages("devtools")
install.packages("devtools")
devtools::install_github("davidgohel/gdtools")
devtools::install_github("davidgohel/flextable")


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
library(lubridate)
library(flextable)
library(glue)
library(tableone)
library(glmnet)
library(survey)
library(webshot)
library(RPostgres)
library(FeatureExtraction)
library(DatabaseConnector)
library(Andromeda)
library(gt)
library(Hmisc)
library(CodelistGenerator)
library(readxl)
library(gdtools)

# database metadata and connection details -----
# The name/ acronym for the database
db.name<-"..."

# Set output folder locations -----
# the paths to the folders where the results from this analysis will be saved
output.folder<-here("Results", db.name, "Denominator")
output.folder1<-here("Results", db.name, "Breast")
output.folder2<-here("Results", db.name, "Colorectal")
output.folder3<-here("Results", db.name, "Lung")
output.folder4<-here("Results", db.name, "Prostate")

output.folder5<-here("Results", db.name, "Breast", "Breast_covariates")
output.folder6<-here("Results", db.name, "Colorectal", "Colorectal_covariates")
output.folder7<-here("Results", db.name, "Lung", "Lung_covariates")
output.folder8<-here("Results", db.name, "Prostate", "Prostate_covariates")

# Specify databaseConnector connection details -----
# database connection details
# connect to database
user        <-  "..."
password    <-  "..."
port        <-  "..." 
host        <-  "..." 
server_dbi  <-  "..."
server      <-  "..."

# Specify cdm_reference via DBI connection details -----
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below (see https://dbi.r-dbi.org/articles/dbi for more details)
# you may need to install another package for this (although RPostgres is included with renv in case you are using postgres)
# see here for details: https://odyosg.github.io/CDMConnector/articles/DBI_connection_examples.html
db <- dbConnect("...",
                dbname = server_dbi,
                port = port,
                host = host, 
                user = user, 
                password = password)

# Set database details -----
# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema<-"..."

# The name of the schema that contains the vocabularies 
# (often this will be the same as cdm_database_schema)
vocabulary_database_schema<-cdm_database_schema

# The name of the schema where results tables will be created 
results_database_schema<-"..."

# Name of outcome and strata tables in the result table where the outcome and strata cohorts will be stored
# Note, if there is an existing table in your results schema with the same names
# it will be overwritten 

outcome_table_stem<-"..."


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
# If all has worked, there should now be a zipped folder of all your results in your
# home directory to share