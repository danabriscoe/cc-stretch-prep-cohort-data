# prep cc ssm data.R


## Load Libraries ----
library(tidyverse)
library(data.table)
library(glue)
library(here)


## Source helper functions ----
source(here::here("R", "00_prep_cohort_data_helper_functions.R"))
source(here::here("R", "pull_historic_metadata.R"))


## Prep input data set -------
tag_fpath <- file.path(here() %>% dirname(), 'cc-process-ssm', 'data', 'ssm_rw')
# flist = list.files(tag_fpath, pattern = ".csv",full.names = T, recursive = TRUE) # recursive grabs files from all sub directories (eg historic, cohort1)

# List only the 'stretch' and 'historic' folders (not others like 'stretch12')
subdirs <- list.dirs(tag_fpath, recursive = TRUE, full.names = TRUE)
target_dirs <- subdirs[basename(subdirs) %in% c("stretch", "historic")]

# Then, get all .csv files within those specific folders
flist <- unlist(lapply(target_dirs, function(dir) {
    list.files(dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
}))


## Read in tags -------
tag_list <-
    lapply(flist, function(i){
        read.csv(i, header=TRUE)
    })



## Convert to df and leave out unnecessary -------
tag_df = data.table::rbindlist(tag_list) %>% 
    as_tibble() %>% 
    mutate(dt = as.Date(date)) 

## Add Group -----
### Add in column to identify historic vs cohort X -----

tag_df <- tag_df %>%
    mutate(group = case_when(
        date < ymd_hms('2023-01-01 00:00:00')  ~ "historic", # assign dates before this time a value of NA, because they were still on ship.
        date >= ymd_hms('2023-07-01 00:00:00') & date < ymd_hms('2024-05-01 00:00:00') ~ "cohort1",
        date >= ymd_hms('2024-07-01 00:00:00') ~ "cohort2",
        TRUE ~ NA))


### Check stats -----
tag_df %>%
    group_by(group) %>%
    summarise(n_indivs = n_distinct(id))

# # A tibble: 3 × 2
# group    n_indivs
# <chr>       <int>
#   1 cohort1        25
#   2 cohort2        28
#   3 historic      282


## Split DF ------------ 
### Split by cohort num and historic ---
tag_df_split <- tag_df %>% split(f = as.factor(.$group))


### Check names are ordered correctly ----
names(tag_df_split) 


## Add Metadata -----------


### 1) STRETCH -----
#### Get list of metadata xlsx files -----
metadata_flist = list.files(file.path(
    here() %>% dirname(),
    'cc-stretch-prep-cohort-data',"utils"), full.names = T)


#### Cohort 1 -----
cohort1_data_w_names <- tag_df_split$cohort1 %>%
    attach_metadata(., assign_metadata(f = metadata_flist[grepl("2023", metadata_flist)])) %>% add_ymd()


#### Cohort 2 -----
cohort2_data_w_names <- tag_df_split$cohort2 %>%
    attach_metadata(., assign_metadata(f = metadata_flist[grepl("2024", metadata_flist)])) %>% add_ymd()


#### Cohorts 3 & 4 .....


### 2) Historic ----
# (do just for df consistency). have to run cohorts first in order to select same col names
historic_data_wo_names <- tag_df_split$historic %>%
    left_join(., pull_historic_metadata(), by = "id", relationship = "many-to-many") %>%
    dplyr::select(any_of(names(cohort1_data_w_names))) %>% add_ymd() %>%
    # add dummy cols to match stretch colnames
    mutate(
        turtle_num = id,
        turtle_name = id
    ) %>%
    relocate(turtle_num, turtle_name, .before = "scl_cm")
    


## Save indiv groups as .rds -----
### Historic (complete) ----
fname_historic <- here('data', 'processed', 'historic_cc_prepped_ssm_data_complete.rds')
saveRDS(historic_data_wo_names, file = fname_historic)


### Cohort 1 (complete) ----
fname_cohort1 <- here('data', 'processed', 'stretch_cc_prepped_ssm_data_cohort1_complete.rds')
saveRDS(cohort1_data_w_names, file = fname_cohort1)

### Cohort 2 (complete) ----
fname_cohort2 <- here('data', 'processed', 'stretch_cc_prepped_ssm_data_cohort2_ongoing.rds')
saveRDS(cohort2_data_w_names, file = fname_cohort2)


## Log record last date prepped & data saved ----
### Run log -----
library(logr)

# Open the log
log_open("cohort_save_timestamps.log")

# Print text to the log
log_print("Last cohort data prep & save:")

# Print a dataframe to the log
Sys.Date()
# Close the log
log_close()



## fin ----