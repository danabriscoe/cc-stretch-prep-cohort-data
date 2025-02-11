# R functions
## Source functions ----

## assign metadata
assign_metadata <- function(f){
    
    ret <- tryCatch( 
        {
            
            # load_metadata_xls("../utils/2023_Turtle_Info.xlsx")
            meta_cohort <- load_metadata_xls(f)
            
            # merge dfs to align ID and Turtle Names
            release_yr <- str_split(f |> basename(), "_") %>%
                purrr::map_chr(~ pluck(., 1)) %>% substr(., start=3, stop=4)
            
            meta_cohort %>%
                mutate(id = glue("{id}_{release_yr}"))
            
            
        },
        error = function(e) {
            # load_metadata_xls("./utils/2023_Turtle_Info.xlsx")
            print('check file path!')
        }
    )
    return(ret)   
}


## Attach metadata to df
attach_metadata <- function(dat_df, meta_df, by="id"){
    merged_df <- 
        dat_df %>%
        merge(., meta_df, by="id", all.x = TRUE)
    
    return(merged_df)
}

## Calc avg daily location
calc_avg_daily_loc <- function(x, ...) {
    # check date attr
    if(!is.Date(x$date)){
        x <- x %>%
            mutate(date = as.Date(date))
    } else {
        print('error: fix date attribute')
    }
    
    # vars to group
    vars1 <- syms(...) # must be in quotes
    
    # calc daily avg by groups 
    ret <- x %>%
        group_by(!!!vars1) %>%
        summarise(lat = mean(lat), 
                  lon = mean(lon),
                  scl_cm = mean(scl_cm), 
                  .groups = 'drop')
    return(ret)
}

## Load raw data (from wc downloads)
load_wc_downloads <- function(wc_files){
    ret <- rbindlist(lapply(wc_files, fread)) %>%
        dplyr::select('Platform ID No.', 'Latitude', 'Longitude', 'Loc. quality', 'Loc. date') %>%
        dplyr::rename(id = 1,
                      lat = 2, 
                      lon = 3, 
                      loc_quality = 4, 
                      date = 5
        ) %>%
        mutate(date = as.POSIXct(date, format= "%m/%d/%Y %H:%M:%S", tz="UTC")) %>%
        as_tibble()
    
    return(ret)
}

## Load metadata files
load_metadata_xls <- function(meta_files){
    ret <- readxl::read_excel(path = meta_files) %>%
        dplyr::select(1,3,6,7) %>%
        dplyr::rename("turtle_num"=1, "id"=2, "turtle_name"=3, "scl_cm"=4)
    return(ret)
}    

