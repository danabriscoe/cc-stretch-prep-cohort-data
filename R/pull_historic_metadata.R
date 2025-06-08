# pull_historic_metadata.R

# script to run plot turtle (cc) - env histograms & show results in ms northern lats - electronic supplemenal section

# author: dk briscoe
# date: 30 aug 2024


# load libraries
library(tidyverse)
library(readxl)
library(stringr)
library(magrittr)
library(glue)
library(here)

# 
# ## Source funcs ------
# source(file.path(
#     here() %>% dirname(),
#     'cc-stretch-northern-lats',"code", "00_northern_lats_helper_functions.R"))
# 
# 
# ## 1) Load data set used in ms models ----
# turtle_df_180_140 <- readRDS(here('data','processed','indiv_tags_df_sept1997_mar2024_180_140W_xtracted_env_ms.rds'))
# 
# ### Get monthly vals ----
# ## 1) Bind historic and stretch cohort 1 dfs
# turtle_df <- turtle_df_180_140 %>%
#     
#     mutate(lon = make180(lon)) %>% 
#     
#     mutate(year = year(dtime),
#            month = month(dtime),
#            day = day(dtime)) 
# 
# 
# 
# 
# 
# 
# #### END SKIP THIS -----------------------------------
# 
# cc_table_data <- turtle_df |>
#     
#     # below ids have been manually reviewed with ssm rs re-fits (Mar 2024)
#     filter(!(id == "19581_97")) |> 
#     filter(!(id == "19587_97")) |>
#     filter(!(id == "22131_98")) |>
#     filter(!(id == "50154_04")) |>
#     filter(!(id == "53760_04")) |>
#     filter(!(id == "24747_00"))
# 
# 
# length(unique(cc_table_data$id))


## 2) pull historic metadata ----
pull_historic_metadata <- function(){

    # Define the path to your Excel file
    metadata_files <- list(
        "~/Dropbox/RESEARCH/PROJECTS/NPAC_Turtles/data/backup_data/original_data_from_DP/NorthPacificCC_Metadata1997-2009.xlsx",
        "~/Dropbox/RESEARCH/PROJECTS/NPAC_Turtles/data/backup_data/original_data_from_DP/NorthPacificCC_Metadata2010-2011.xlsx",
        "~/Dropbox/RESEARCH/PROJECTS/NPAC_Turtles/data/backup_data/original_data_from_DP/NorthPacificCC_Metadata2011-2012.xlsx",
        "~/Dropbox/RESEARCH/PROJECTS/NPAC_Turtles/data/backup_data/original_data_from_DP/NorthPacificCC_Metadata2012-2013.xlsx",
        "~/Dropbox/RESEARCH/PROJECTS/NPAC_Turtles/data/data_tracks_for_analysis/metadata/2003-2007PNPA_Info_formatted.xls", "~/Dropbox/RESEARCH/PROJECTS/NPAC_Turtles/data/data_tracks_for_analysis/metadata/LonglineLogggerhead_Info_formatted.xls")
    
    # Read all sheets into a list and assign names
    df.list <- lapply(metadata_files, read_excel)
    
    # Deal with excel dates for last 2 metadata files
    df.list[[5]]$`Date deployed` <- openxlsx::convertToDate(df.list[[5]]$`Date deployed`)
    # df.list[[6]]$`Date deployed` <- openxlsx::convertToDate(df.list[[6]]$`Date deployed`)
    
    ### Double check all metadata df's have the same column names ----------
    # names(df.list[[1]]) == names(df.list[[2]])
    # names(df.list[[3]]) == names(df.list[[4]])
    # names(df.list[[5]]) == names(df.list[[6]])
    
    col_names = colnames(df.list[[1]]) %>% tolower(.) %>% gsub(" ", "_", .) %>% 
        
        gsub("(", "", ., fixed=TRUE) %>% gsub(")", "", ., fixed=TRUE) %>% 
        gsub(".", "", ., fixed=TRUE) %>%
        gsub("________", "_", .) %>%
        gsub("__", "_", .) 
    
    # custom function to format
    format_df_cols <- function(x){
        ret <- x |>
            dplyr::select(c(id_code, date_deployed,
                            deployment_location, #date_terminated, 
                            transmitting_days, 
                            distance_traveled, 
                            scl_cm,
                            capture_status)) %>%
            mutate(date_deployed = as.Date(date_deployed),
                   transmitting_days = as.numeric(transmitting_days) 
                   # date_terminated = as.Date(date_terminated)
            )
        return(ret)
    }
    
    ### Rename df cols -----
    # df_list <- lapply(df.list, setNames, col_names) # this works
    df_list <- purrr::map(df.list, ~ rename_with(., ~ col_names)) # this also works! but need to specify purrr library (otherwise thinks its animotum)
    
    ### Merge meta dfs ----
    merged_meta_list <- lapply(df_list, format_df_cols) 
    
    merged_meta_dfs <- do.call("rbind", merged_meta_list) 
    
    ### Format metda df info -----
    formatted_merged_meta_dfs <- merged_meta_dfs |>
        mutate(year = year(date_deployed) %>% substr(., 3,4)) |>
        mutate(id = paste0(id_code,"_", year)) %>%
        mutate(scl_cm = case_when(
            grepl(pattern = "\\**", x = .$scl_cm) ~ gsub("\\**", "", .$scl_cm),
            grepl(pattern = "--", x = .$scl_cm) ~ NA
        ) %>% {suppressWarnings(as.numeric(.))} 
            ) %>%
        mutate(capture_status = tolower(capture_status),
               capture_status = 
                   case_when(
                       startsWith(capture_status, 
                                  "cap") ~ "captive",
                       TRUE ~ capture_status
                   )) |>
        drop_na(capture_status)
    
    
    ret <- formatted_merged_meta_dfs

    return(ret)
}

# ## 3) Find which ids in ms historic data set -----
# 
# ms_historic_ids <- unique(cc_table_data$id)
# full_historic_ids <- unique(formatted_merged_meta_dfs$id)
# 
# 
# id_idx <- which(full_historic_ids %in% ms_historic_ids)
# length(id_idx) # [1] 167 Historic + 25 Cohort 1
# 
# ms_historic_meta_dfs <- formatted_merged_meta_dfs[id_idx,]
# 
# test = c(setdiff(full_historic_ids, ms_historic_ids), setdiff(ms_historic_ids, full_historic_ids)) %>% as.data.frame()
# which( ms_historic_ids %in% full_historic_ids)
# 
# # 65417_10
# # 50135_11
# 
# 
