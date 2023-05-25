# Locating the location of the patients across time. CPRD has changed across the
# last few years, with more practices moving over to AURUM

# Connect to the database and then view the location table


# load r packages

library(here)
library(DBI)
library(dbplyr)
library(dplyr)
library(readr)
library(tidyr)
library(CDMConnector)
library(RPostgres)
library(lubridate)
library(Hmisc)
library(flextable)

# table names instantiated in cdm with cancer diagnoses
# "nb_cancer_covid_cancers_3_time_periods"

outcome_table_stem <- "nb_cancer_covid" # this will form the start of the table name where the cohorts are instantiated
# table names----
outcome_table_name_3 <- paste0(outcome_table_stem,"_denominator_3_time_periods") # this is the denominator before, during and after lockdown

db.name<-"CPRDGold_202201"


# connect to database
user        <-  Sys.getenv("DB_USER")
password    <-  Sys.getenv("DB_PASSWORD")
port        <-  Sys.getenv("DB_PORT") 
host        <-  Sys.getenv("DB_HOST") 
server_dbi  <-  Sys.getenv("DB_SERVER_DBI_cdm_gold_202201")
server      <-  Sys.getenv("DB_SERVER_cdm_gold_202201")

# Specify cdm_reference via DBI connection details -----
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below (see https://dbi.r-dbi.org/articles/dbi for more details)
# you may need to install another package for this (although RPostgres is included with renv in case you are using postgres)
# see here for details: https://odyosg.github.io/CDMConnector/articles/DBI_connection_examples.html
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


# create cdm reference for 1st ever cancer diagnoses---- 
cdm <- CDMConnector::cdm_from_con(con = db, 
                                  cdm_schema = cdm_database_schema,
                                  write_schema = results_database_schema,
                                  #cohort_tables = "nb_cancer_covid_cancers_3_time_periods")
                                  cohort_tables = "nb_cancer_covid_denominator_3_time_periods")

# count n in each cohort definition
#cdm$nb_cancer_covid_cancers_3_time_periods %>% group_by(cohort_definition_id) %>% tally() %>% print(n=Inf)
cdm$nb_cancer_covid_denominator_3_time_periods %>% group_by(cohort_definition_id) %>% tally() %>% print(n=Inf)


cdm$person %>% head()
cdm$nb_cancer_covid_denominator_3_time_periods %>% head()


# ============================================================================ #
#       Get age distribution for cancer cohorts in 3 time period            #
#          (make sure the correct cohort table is read into the cdm)           #
#                 cdm$nb_denominator_covid_cancers_3_time_periods                   #
# ============================================================================ #

## AGE AT COHORT START DATE IN DENOMINATOR COHORTS ----------------------------
age_distribution_denominator_cohorts <- cdm$nb_cancer_covid_denominator_3_time_periods %>% 
  rename("person_id" = subject_id) %>%
  left_join(cdm$person %>% select(person_id, year_of_birth), by = "person_id") %>%
  collect() %>%
  mutate(month_of_birth = 1) %>%
  mutate(day_of_birth   = 1) %>%
  mutate(dob = as.Date(dmy(paste(day_of_birth,month_of_birth,year_of_birth,sep="-")))) %>%
  mutate(age = floor(as.numeric(difftime(cohort_start_date,dob,unit="days"))/365.25)) 

# AGE at COHORT START DATE IN DENOMINATOR COHORTS

age_distribution_denominator_cohorts <- age_distribution_denominator_cohorts %>%
  mutate(age_grouping = cut(age, c(0,10,20,30,40,50,60,70,80,90,100,110),
                            labels = c("0 to 9 years", "10 to 19 years","20 to 29 years","30 to 39 years","40 to 49 years","50 to 59 years","60 to 69 years","70 to 79 years","80 to 89 years","90 to 99 years","100+ years"),include.lowest = TRUE, right = FALSE, is.na = FALSE)) %>%
  mutate(agegid = as.numeric(age_grouping)) 

Mean_age_before <- age_distribution_denominator_cohorts %>% filter(cohort_definition_id==1) 
Mean_age_before <- mean(Mean_age_before$age) %>% print()

Mean_age_during <- age_distribution_denominator_cohorts %>% filter(cohort_definition_id==2) 
Mean_age_during <- mean(Mean_age_during$age) %>% print()

Mean_age_after <- age_distribution_denominator_cohorts %>% filter(cohort_definition_id==3) 
Mean_age_after <- mean(Mean_age_after$age) %>% print()

  
# FREQUENCIES OF AGES AT COHORT START DATE FOR DENOMINATOR POP BEFORE LOCKDOWN ------
age_table_1 <- age_distribution_denominator_cohorts %>% 
  filter(cohort_definition_id ==1) %>%
  group_by(age_grouping) %>%
  tally() %>% 
  print()

age_table_1 <- age_table_1 %>% mutate(freq = (n/sum(n))*100) %>% rename(N_Before_Lockdown = n) %>% print()
age_table_1 <- age_table_1 %>% mutate(Proportion_before_lockdown = paste0(round(freq, digits=2), "%")) %>% print()

# FREQUENCIES OF AGES AT COHORT START DATE FOR DENOMINATOR POP DURING LOCKDOWN ------
age_table_2 <- age_distribution_denominator_cohorts %>% 
  filter(cohort_definition_id ==2) %>%
  group_by(age_grouping) %>%
  tally() %>% 
  print()

age_table_2 <- age_table_2 %>% mutate(freq = (n/sum(n))*100) %>% rename(N_During_Lockdown = n) %>% print()
age_table_2 <- age_table_2 %>% mutate(Proportion_during_lockdown = paste0(round(freq, digits=2), "%")) %>% print()


# FREQUENCIES OF AGES AT COHORT START DATE FOR DENOMINATOR POP AFTER LOCKDOWN ------
age_table_3 <- age_distribution_denominator_cohorts %>% 
  filter(cohort_definition_id ==3) %>%
  group_by(age_grouping) %>%
  tally() %>% 
  print()

age_table_3 <- age_table_3 %>% mutate(freq = (n/sum(n))*100) %>% rename(N_After_Lockdown = n) %>% print()
age_table_3 <- age_table_3 %>% mutate(Proportion_after_lockdown = paste0(round(freq, digits=2), "%")) %>% print()


# join age group tables together
AGE_GROUPS_full <- cbind(age_table_1, age_table_2, age_table_3)
AGE_GROUPS <- AGE_GROUPS_full[c(1,4,8,12)]



write.csv(AGE_GROUPS_full, file=here::here("Results", db.name, "Denominator", "age_distribution_denominator_cohorts.csv"))
save(AGE_GROUPS_full, age_table_1, age_table_2, age_table_3, AGE_GROUPS, file=here::here("Results", db.name, "Denominator", "age_distribution_denominator_cohorts.RData"))



Pretty_age_groups_table <- flextable(AGE_GROUPS) %>% theme_vanilla() %>% 
  set_caption(caption = "Proportion of source population in different age groups for those enrolled before, during and after lockdown") %>% 
  width(width = 3.5)


save_as_docx('Pretty_age_groups_table' = Pretty_age_groups_table, path="~/R/CancerCovid_Characterisations/Results/CPRDGold_202201/Denominator/Pretty_age_groups_table.docx")





## ======================== SEX ============================================= XX



## SEX DENOMINATOR COHORT BEFORE LOCKDOWN ----------------------------
sex_denominator_1 <-  cdm$nb_cancer_covid_denominator_3_time_periods %>% 
  rename("person_id" = subject_id) %>% 
  left_join(cdm$person) %>%
  filter(cohort_definition_id==1) %>%
  collect() %>%
  distinct() %>%
  mutate(value = if_else(gender_concept_id==8532,1,2)) %>%
  select(-gender_concept_id) %>%
  mutate(sex = if_else(value==1,"Female","Male")) %>%
  group_by(sex) %>%
  tally() %>%
  print()


## SEX DENOMINATOR COHORT DURING LOCKDOWN ----------------------------
sex_denominator_2 <-  cdm$nb_cancer_covid_denominator_3_time_periods %>% 
  rename("person_id" = subject_id) %>% 
  left_join(cdm$person) %>%
  filter(cohort_definition_id==2) %>%
  collect() %>%
  distinct() %>%
  mutate(value = if_else(gender_concept_id==8532,1,2)) %>%
  select(-gender_concept_id) %>%
  mutate(sex = if_else(value==1,"Female","Male")) %>%
  group_by(sex) %>%
  tally() %>%
  print()

## SEX DENOMINATOR COHORT AFTER LOCKDOWN ----------------------------
sex_denominator_3 <-  cdm$nb_cancer_covid_denominator_3_time_periods %>% 
  rename("person_id" = subject_id) %>% 
  left_join(cdm$person) %>%
  filter(cohort_definition_id==3) %>%
  collect() %>%
  distinct() %>%
  mutate(value = if_else(gender_concept_id==8532,1,2)) %>%
  select(-gender_concept_id) %>%
  mutate(sex = if_else(value==1,"Female","Male")) %>%
  group_by(sex) %>%
  tally() %>%
  print()


sex_table_1 <- sex_denominator_1 %>% rename(n_before_lockdown = "n")
sex_table_2 <- sex_denominator_2 %>% rename(n_during_lockdown = "n")
sex_table_3 <- sex_denominator_3 %>% rename(n_after_lockdown = "n")

sex_table <- sex_table_1 %>% right_join(sex_table_2) %>% right_join(sex_table_3) %>% replace(is.na(.), 0)

sex_table_denominator <- sex_table %>%
  mutate("n before lockdown" = paste0(n_before_lockdown, " (", round(100*n_before_lockdown/sum(n_before_lockdown),1), "%)")) %>%
  mutate("n during lockdown" = paste0(n_during_lockdown, " (", round(100*n_during_lockdown/sum(n_during_lockdown),1), "%)")) %>%
  mutate("n after lockdown" = paste0(n_after_lockdown, " (", round(100*n_after_lockdown/sum(n_after_lockdown),1), "%)"))


sex_table_denominator <- sex_table_denominator[-c(2:4)] #  remove superfluous columns

Pretty_sex_table <- flextable(sex_table_denominator) %>%
  set_caption(caption = "Sex of denominator population entering CPRD before, during and after lockdown") %>% 
  width(width = 1.4)  


# save the table as a csv file
write.csv(sex_table_denominator, here("Results", db.name, "Denominator", "sex_table_denominator.csv"), row.names = FALSE)


# save the table as docx
save_as_docx('sex_table_denominator' = Pretty_sex_table, path=here("Results", db.name, "Denominator", "sex_table_denominator.docx"))


# save RData objects
save(sex_table_denominator, Pretty_sex_table, file = here("Results", db.name, "Denominator", "Denominator_sex.RData"))


