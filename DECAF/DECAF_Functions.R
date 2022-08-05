DECAF_Nback_loop <- function(path) {
  require(here)
  require(tidyverse)
  library(here)
  library(tidyverse)
  
  #define DECAF_Nback function
  DECAF_Nback <- function(csvname) {
    
    #check if dataframe "df" exists
    if (exists("df") && is.data.frame(get("df"))) {
      #do nothing if data frame "df" exists
    } else {
      #if data frame "df" doesn't exists then create an empty data frame "df"
      df <- data.frame()
    }
    
    #load CSV as "datafile"
    datafile <- read_csv(csvname)
    
    
    #loop through every row of "datafile" 
    for (i in 1:nrow(datafile)) {
      
      #check if row "i" column "NBACK_TASK_NBACK" is NOT empty (i.e., is not NA).
      #if row "i" column "NBACK_TASK_NBACK" is NOT empty (i.e., is not NA), then print that the row has data and extract that cell of       N back data
      if (!is.na(datafile$NBACK_TASK_NBACK[i])) { 
        
        #print "Row i has data"
        print(paste("Row",as.character(i),"has data")) 
        
        #Extract N Back response data and save as "n_back"
        n_back <- datafile$NBACK_TASK_NBACK[i]
        
        #Extract column/variable names from n_back response data
        col_head <- word(n_back,1,sep = "~") %>% 
          str_split(pattern = ",") %>% 
          unlist()
        
        
        #reformat n_back response data into useable data frame
        #create new data frame for n_back data from row "i"
        n_back_df <- as.data.frame(n_back,stringsAsFactors = FALSE) %>%  
          #create new rows for n_back response data based on separator in data (i.e., ~)
          separate_rows(., n_back, sep = "~", convert = TRUE) %>% 
          .[-c(1), ] %>% 
          separate(n_back, 
                   into = col_head,
                   sep = ",") %>% 
          mutate_all(na_if, "") %>% 
          #add subject_id and global_date variables to first two columns of n_back_df
          mutate(subject_id = datafile$subject_id[i], global_date = datafile$global_date[i]) %>% 
          relocate(subject_id, global_date, .before = Response)
        
        #add newly extracted n_back_df dataframe to the bottoms of df
        df <- rbind(df, n_back_df)
        
      } 
      #Else, if row "i" column "NBACK_TASK_NBACK" of "datafile is empty (i.e., is NA) then print that the row "is an empty cell" and       create a row of NA data for that "subject_id"'s "global_date"
      else {
        #create a list of column headers for the empty row
        col_head_empty <- c("subject_id",
                            "global_date",
                            "Response",
                            "Response_Time",
                            "Response_TS",
                            "Tapped",
                            "Tap_Count",
                            "Extra_Taps",
                            "Is_Target",
                            "Is_Lure",
                            "Show_Cross_TS",
                            "Hide_Cross_TS",
                            "Show_Stimulus_TS",
                            "Hide_Stimulus_TS",
                            "Stimulus_ID",
                            "Stimulus",
                            "Card_ID")
        
        print(paste("Row",as.character(i), "is an empty cell"))
        
        #create empty data frame "n_back_df" with columns named according to "col_head_empty"
        n_back_df <- data.frame(matrix(ncol = length(col_head_empty)))
        colnames(n_back_df) <- col_head_empty
        #add subject_id and global_date variables to first two columns of n_back_df
        n_back_df <- n_back_df %>% 
          mutate(subject_id = datafile$subject_id[i], global_date = datafile$global_date[i]) %>% 
          relocate(subject_id, global_date, .before = Response)
        #add new row of empty n back data "n_back_df" to larger dataset "df"
        df <- rbind(df,n_back_df)
      }  
    }
    #change the data format for various n back data columns
    df <- df %>% 
      mutate(Response = as.factor(Response),
             Response_Time = as.numeric(Response_Time),
             Response_TS = as.numeric(Response_TS),
             Tap_Count = as.numeric(Tap_Count),
             Show_Cross_TS = as.numeric(Show_Cross_TS),
             Hide_Cross_TS = as.numeric(Hide_Cross_TS),
             Show_Stimulus_TS = as.numeric(Show_Stimulus_TS),
             Hide_Stimulus_TS = as.numeric(Hide_Stimulus_TS)
      )
    
    return(df)
  }
  
  
  
  #create a list "file_list" of all ".csv" files in the directory "path"
  file_list = list.files(path = here(data),
                         pattern="*.csv")
  
  #if file_list is empty, print message that there are no .csv files in "path"
  if (length(file_list) == 0) {
    writeLines(paste0("There are no .csv files in the specified directory --> ",path, "\n\nPlease choose another directory."))
  } else {
    
    #create empty n_back_output_full data frame
    n_back_output_full <- data.frame()
    
    
    #Else, if file_list contains .csv files, then loop through all .csv files and apply DECAF_Nback function
    for (i in file_list) {
      print(i)
      #run DECAF_Nback function on .csv file "i" from "file_list"
      n_back_output_partial<- DECAF_Nback(here(data, i))
      #add new rows extracted into "n_back_output_partial" from .csv file "i" to the bottom of "n_back_output_full"
      n_back_output_full <- rbind(n_back_output_partial, n_back_output_full)
    }
    
    #Calculate hit rate and similar metrics for N Back data
    n_back_output_summary <- n_back_output_full %>% 
      group_by(subject_id, global_date) %>% 
      summarise(NB_mean_rt = mean(Response_Time),
                NB_sd_rt = sd(Response_Time),
                NB_Hits = sum(Response == 'Correct Hit'),
                NB_Correct_Reject = sum(Response == 'Correct Rejection'),
                NB_False_Alarm = sum(Response == 'False Alarm'),
                NB_Miss = sum(Response == 'Omission'),
                NB_No_Response = sum(Response == "No Response"),
                NB_N_Stimuli = sum(Response == 'Correct Hit' | Response == 'Correct Rejection' | Response == 'False Alarm' | Response == 'Omission' | Response == "No Response")) %>% 
      mutate(NB_hit_rate = (NB_Hits/(NB_Hits+NB_Miss)),
             NB_false_alarm_rate = (NB_False_Alarm/(NB_False_Alarm+NB_Correct_Reject))) %>% 
      ungroup()
    
    out <- list()
    out$raw <- n_back_output_full
    out$summary <- n_back_output_summary
    
  }
  out
}

DECAF_SpatialSpan_loop <- function(path) {
  require(here)
  require(tidyverse)
  library(here)
  library(tidyverse)
  #define DECAF_Nback function
  DECAF_SpatialSpan <- function(csvname) {
    
    if (exists("df_SpatialSpan_master") && is.data.frame(get("df_SpatialSpan_master"))) {
      #if df_SpatialSpan_master exists and is a dataframe, then continue
    } else {
      #else if df_SpatialSpan_master doesn't exist or isn't a data frame, create it!
      df_SpatialSpan_master <- data.frame()
    }
    
    datafile <- read_csv(csvname) #read in the raw datafile to R
    
    #for each row in "datafile"
    for (i in 1:nrow(datafile)) {
      #if SPACIAL_SPAN_TASK_SSP cell in row i is NOT empty
      if (!is.na(datafile$SPACIAL_SPAN_TASK_SSP[i])) {
        print(paste("Row",as.character(i),"has data"))
        #Extract n_back response data
        SpatialSpan <- datafile$SPACIAL_SPAN_TASK_SSP[i]
        
        #Extract column/variable names from n_back response data
        col_head <- word(SpatialSpan,1,sep = "~") %>% 
          str_split(pattern = ",") %>% 
          unlist()
        
        if (length(col_head) == 0) {
          df_SpatialSpan <- data.frame(matrix(ncol = length(col_head)))
          colnames(df_SpatialSpan) <- col_head
          
          df_SpatialSpan <- df_SpatialSpan %>% 
            mutate(subject_id = datafile$subject_id[i], global_date = datafile$global_date[i]) %>% 
            relocate(subject_id, global_date, .before = col_head[1])
          
          df_SpatialSpan_master <- rbind(df_SpatialSpan_master,df_SpatialSpan)
        } else {
          
          #reformat n_back response data into useable dataframe
          df_SpatialSpan <- as.data.frame(SpatialSpan,stringsAsFactors = FALSE) %>%  #create new data frame for row i's n_back data
            separate_rows(., SpatialSpan, sep = "~", convert = TRUE) %>% #create new rows for n_back response data based on separator in data (i.e., ~)
            .[-c(1), ] %>% 
            separate(SpatialSpan, 
                     into = col_head,
                     sep = ",") %>% 
            mutate_all(na_if, "") %>% 
            mutate(subject_id = datafile$subject_id[i], global_date = datafile$global_date[i]) %>% 
            relocate(subject_id, global_date, .before = col_head[1])
          
          #add newly extracked n_back_df dataframe to the bottoms of df
          df_SpatialSpan_master <- rbind(df_SpatialSpan_master,df_SpatialSpan)
        }
      }
      else {
        
        col_head_empty <- c("name",
                            "correct",
                            "correctSequence",
                            "tappedSequence",
                            "startTime",
                            "endTime",
                            "responseStartTime",
                            "responseEndTime",
                            "responseTime",
                            "responseTotal",
                            "targetLog",
                            "interuptions")  
        
        print(paste("Row",as.character(i), "is an empty cell"))
        df_SpatialSpan <- data.frame(matrix(ncol = length(col_head_empty)))
        colnames(df_SpatialSpan) <- col_head_empty
        
        df_SpatialSpan <- df_SpatialSpan %>% 
          mutate(subject_id = datafile$subject_id[i], global_date = datafile$global_date[i]) %>% 
          relocate(subject_id, global_date, .before = col_head_empty[1])
        
        df_SpatialSpan_master <- rbind(df_SpatialSpan_master,df_SpatialSpan)
      }  
    }
    df_SpatialSpan_master <- df_SpatialSpan_master %>%
      mutate(correct = as.factor(correct),
             startTime = as.numeric(startTime),
             endTime = as.numeric(endTime),
             responseStartTime = as.numeric(responseStartTime),
             responseEndTime = as.numeric(responseEndTime),
             responseTime = as.numeric(responseTime))
    return(df_SpatialSpan_master)
  }
  
  file_list = list.files(path = path,
                         pattern="*.csv")
  if (length(file_list) == 0) {
    writeLines(paste0("There are no .csv files in the specified directory --> ",path, "\n\nPlease choose another directory."))
  }
  SpatialSpan_output_full <- data.frame()
  
  for (i in file_list) {
    print(i)
    SpatialSpan_output_partial<- DECAF_SpatialSpan(here(data, i))
    SpatialSpan_output_full <- rbind(SpatialSpan_output_partial, SpatialSpan_output_full)
  }
  
  
  #Score spatial span responses
  print("Scoring spatial span responsess...")
  for (i in 1:nrow(SpatialSpan_output_full)) {
    correct_string <- str_trim(SpatialSpan_output_full$correctSequence[i])%>% strsplit(., "-")
    tapped_string <- str_trim(SpatialSpan_output_full$tappedSequence[i])%>% strsplit(., "-")
    
    if (is.na(SpatialSpan_output_full$correct[i])) {
      SpatialSpan_output_full$Response[i] = NA
    } else if (SpatialSpan_output_full$correct[i] == "true") {
      SpatialSpan_output_full$Response[i] = "True Hit"
    } else {
      if (identical(sort(correct_string[[1]]), sort(tapped_string[[1]]))) {
        SpatialSpan_output_full$Response[i] = "True - Wrong order"
      } else {
        SpatialSpan_output_full$Response[i] = "Incorrect"
      }
    }
  }
  Spatial_Span_Summary <- SpatialSpan_output_full %>% 
    mutate(newCol = str_detect(correct, "true")) %>% 
    group_by(subject_id, global_date) %>% 
    summarise(SpatialSpan_TotalCorrect = sum(Response == 'True Hit'), 
              SpatialSpan_PartialCorrect = sum(Response == 'True - Wrong order'),
              SpatialSpan_TotalIncorrect = sum(Response == 'Incorrect'),
              SpatialSpan_n_Trials = sum(!is.na(correct))) %>% 
    ungroup()
  
  
  out <- list()
  out$raw <- SpatialSpan_output_full
  out$summary <- Spatial_Span_Summary
  
  out
}

DECAF_EMA_SR <- function(path) {
  #create a list "file_list" of all ".csv" files in the directory "path"
  file_list = list.files(path = here(data),
                         pattern="*.csv")
  
  #if file_list is empty, print message that there are no .csv files in "path"
  if (length(file_list) == 0) {
    writeLines(paste0("There are no .csv files in the specified directory --> ",path, "\n\nPlease choose another directory."))
  } else {
    
    #create empty EMA_SR_output_full data frame
    EMA_SR_output_full <- data.frame()
    
    #Else, if file_list contains .csv files, then loop through all .csv files and extract EMA_SR data
    for (i in file_list) {
      print(i)
      
      #extract self-reported data for each row (i.e., timepoint) of .csv file "i" and save to dataframe "EMA_SR_output_partial"
      EMA_SR_output_partial <- read_csv(here(data,i))%>% 
        select(subject_id, 
               global_date, 
               DAILY_ACTIVITIES_LOCATION:AFF_GLT, 
               PRCVD_COG_P,
               PRCVD_DIST,
               PRCVD_COG_P_1,
               PRCVD_DIST_1) %>% 
        mutate(PRCVD_N_Back_COG_P = PRCVD_COG_P,
               PRCVD_N_Back_DIST = PRCVD_DIST,
               PRCVD_SPATIALSPAN_COG_P = PRCVD_COG_P_1,
               PRCVD_SPATIALSPAN_Dist = PRCVD_DIST_1)
      
      #add new rows extracted into "EMA_SR_output_partial" from .csv file "i" to the bottom of "EMA_SR_output_full"
      EMA_SR_output_full <- rbind(EMA_SR_output_partial, EMA_SR_output_full)
    }
  }
  EMA_SR_output_full
}

DECAF_Extract <- function(path) {
  N_Back <- DECAF_Nback_loop(path)
  Spatial_Span <- DECAF_SpatialSpan_loop(path)
  EMA_SR <- DECAF_EMA_SR(path)
  
  out <- list()
  out$raw_n_back <- N_Back$raw  
  out$summary_n_back <- N_Back$summary
  out$raw_EMA_SR <- EMA_SR
  out$raw_spatial_span <- Spatial_Span$raw
  out$summary_spatial_span <- Spatial_Span$summary
  out$DECAF_all <- left_join(N_Back$summary, Spatial_Span$summary) %>% 
    left_join(., EMA_SR)
  
  out
}