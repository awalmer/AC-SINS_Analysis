#------------------------------------------------------------------------------------------------------
# AC-SINS R Shiny App 
# Version 5 for Deployment 
# New Combined Function Script - Tab and Prep Functions
# Refactored Code
# January 31st, 2019 // Auralee Walmer
#------------------------------------------------------------------------------------------------------


##-------- PACKAGE SET-UP --------##

## Function to load multiple packages:
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

## Load packages:
my_packages <- c("ggplot2","ggrepel","readxl","reshape2","plyr","dplyr","stringr","data.table","tibble","gtools", 
                 "purrr","DT","assertthat","devtools","utils", "gapminder", "jsonlite", "parsedate", "xlsx", "tidyr", "datasets", "openxlsx")
ipak(my_packages)


##-------- GENERIC FUNCTIONS: DO NOT REQUIRE AC-SINS DATA --------##

#Split the string by the space. Keep first part of string:
splitspace <- function(string) {
  string <- as.character(string) # added this because was getting an error "non-character argument" after .csv update
  return(strsplit(string, " ")[[1]][1])
}

## Build blank sample name data frame:
build_blank_names <- function() {
  return(data.frame(matrix("",ncol = 12, nrow = 8)))
}

#Build blank sample names for goodness of fit table:
build_blank_names_gfit <- function() {
  blanknames <- data.frame(matrix("",ncol = 2, nrow = 384))
  colnames(blanknames)[c(1,2)] <- c('Well','Sample Name')
  blanknames['Well'] <- process_wellonly()
  return(blanknames)
}


## Read and modify .csv files -- put in right format to make consistent with reading excel files

read_modify_acsins_csv <- function(datapath_acsinsnames) { # This is geared specifically towards reading the acsins names file if .csv format
  df <- read.csv(datapath_acsinsnames)
  df[] <- lapply(df[], as.character) #change factors to characters
  colnames(df) <- c('Plate','Well','Sample Name','Buffer Name','Control Name','ID')
  #Now need to change the character "NA" values in Control Name to actual 'NA' values
  for (a in 1:nrow(df)) {
    if (df$`Control Name`[a] == "") {
      df$`Control Name`[a] <- NA
    }
  }
  return(df)
}


#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------

##-------- IMPORT AND CLEAN RAW AC-SINS DATA --------##

## CLEAN RAW DATA:
## Import Excel Spreadsheet of raw ac-sins data, and identify starting data position by 'Wavel." value:
clean_acsins_excel_new <- function(fileName) {
  acsins_data <- suppressMessages(read_excel(fileName)) #Import Data
  acsins_data[] <- lapply(acsins_data, as.character) #factor to character
  wavel <- which(acsins_data[1] == 'Wavel.') #Find index of cell containing 'Wavel.' (Varies based on partial vs. full plate read)
  colnames(acsins_data) <- acsins_data[wavel, ] #Reposition headers
  colnames(acsins_data)[1] <- "wavelength" #Rename wavelegth var for future use
  acsins_data <- acsins_data[(wavel+1):(wavel+151), ] #matrix notation, capture all columns for 151 nm values
  acsins_data[] <- lapply(acsins_data, function(x) as.numeric(as.character(x))) #Make all values numeric for calculations
  
  return(acsins_data)
}

## CLEAN RAW DATA: Numeric
clean_acsins_excel_num <- function(fileName, for_display) { # for_display = 1 if numeric but visually abridged. for_display = 0 if numeric for calculations only.
  acsins_data <- clean_acsins_excel_new(fileName)
  acsins_data[] <- lapply(acsins_data, function(x) as.numeric(as.character(x))) #Make all values numeric for calculations
  if (for_display==1) {
    acsins_data<- lapply(acsins_data, sprintf, fmt="%3.6f")
    acsins_data$wavelength<-as.integer(acsins_data$wavelength)
  }
  return(acsins_data)
}


##-------- DATE & TIME from RAW AC-SINS DATA --------##

## Extract Date and Time from Raw Data:
extract_date_time <- function(fileName) {
  acsins_data <- read_excel(fileName)
  daterow <- which(acsins_data[1] == 'Start Time:')
  return(as.character(acsins_data[daterow,2]))
}

## Extract date/time and put into DATA FRAME format: 
# (purpose = insert into downloadable concatenated archive file)
extract_date_time_df <- function(fileName) {
  date_string <- extract_date_time(fileName)
  date_df <- data.frame(matrix(ncol = 1, nrow = 0),stringsAsFactors=FALSE)
  colnames(date_df)[1] <- 'Data Acquisition Date and Time: '
  date_df[1,1] <- date_string
  return(date_df)
}



#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------

##-------- WELL ID FORMAT MANIPULATION --------##

## Extract teh column headers of AC-SINS clean data as a list of strings:
extract_column_list <- function(clean_acsins_data) {
  return(as.list(colnames(clean_acsins_data)[2:ncol(clean_acsins_data)]))
}

## Process ONLY 384 Well IDs (No data):
process_wellonly <- function() {
  output <- data.frame(matrix(ncol = 1, nrow = 0)) #Cannot skip this step, otherwise produces 1x1 matrix containing P24
  for (a in 65:80) {
    for (b in 1:24) {
      str <- paste(intToUtf8(a),b,sep='') # For each letter A-P and for each number 1-24, pasting!
      output<-rbind(output, data.frame("Well"=str)) } } # Binds into column of data frame
  return(output)
}

## Process ONLY 384 Well IDs (WITH DATA) - VARIABLE NUMBER OF COLUMNS:
process_wellonly_wdata <- function(clean_acsins_data, keepNA) { # Binary indicator: If keepNA==1, insert NA values where missing (total index=384). If keepNA==0, skip NA values (total<384)
  output <- data.frame(matrix(ncol = 1, nrow = 0)) #Cannot skip this step, otherwise produces 1x1 matrix containing P24
  for (a in extract_column_list(clean_acsins_data) ) {
    output<-rbind(output, data.frame("Well"=a))
    output[] <- lapply(output, as.character)
  }
  if (keepNA == 1) { #  (Purpose = when there are <384 columns in raw data, need to be able to have NA place holders so that the 96-well format is mapped correctly.)
    output$dup <- output$Well #need a duplicate of the limited Well column
    output <- join(process_wellonly(),output, by="Well") #Join the partial well column with the full 384 well column
    output$Well <- NULL 
    colnames(output)[1] <- 'Well' #Rename duplicated one as well. Contains NAs where missing wells were.
  }
  return(output)
}


##Sort Well IDs indirectly (not factorized correctly, so break down):
sort_well_ids <- function(table) {
  letter<-as.character(substr(table$Well, 1,1))  #Extract the letter from Well ID
  number<-as.numeric(substr(table$Well,2,3))     #Extract the number from Well ID
  return(table[order(letter, number),]) #Order by letter then number
}


#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------

##-------- IMPORT AND CLEAN AC-SINS SAMPLE NAME DATA --------##

# Update as of February, 2019: The Sample name data file will now be generated through the Assay Development Toolkit.
# Rather than a set of matrices, the sample names will be compiled in a spreadsheet, where each layer is a column.

capture_fullname_df_96 <- function(assaydev_name_flatfile_96) {  ## EXPECTS 96-WELL FORMAT OF FLAT FILE
  #names96 <- read_excel(assaydev_name_flatfile_96)
  names96 <- assaydev_name_flatfile_96[-c(assaydev_name_flatfile_96$Plate)]
  return(names96)
}


capture_fullname_df_384 <- function(assaydev_name_flatfile_96) {
  names96 <- assaydev_name_flatfile_96
  names96$Well_Position <- seq.int(nrow(names96))
  names384 <- process_wellonly()
  
  names96_temp <- data.frame(matrix(ncol = 4, nrow = 0))
  for (x in seq(1,384,48)) {
    for (n in seq(x,x+23,2)) {
      Reading_1 <- as.character(names384$Well[n])
      Reading_2 <- as.character(names384$Well[n+1])
      Reading_3 <- as.character(names384$Well[n+24])
      Reading_4 <- as.character(names384$Well[n+25])
      names96_temp<-rbind(names96_temp, data.frame("Reading_1"=Reading_1, "Reading_2"=Reading_2,"Reading_3"=Reading_3,"Reading_4"=Reading_4))
    }
  }
  
  names96_temp$Well_Position <- seq.int(nrow(names96_temp))
  names96 <- join(names96, names96_temp, by="Well_Position")
  #Change all factors to strings before the melt:
  i <- sapply(names96, is.factor)
  names96[i] <- lapply(names96[i], as.character)
  #Reshape:
  melt_temp <- melt(names96[c("Well_Position","Reading_1","Reading_2","Reading_3","Reading_4")], id.vars="Well_Position", factorsAsStrings=F)
  #Now joing melted data frame with sample names
  df <- join(melt_temp, names96, by="Well_Position")
  df <- df[, c("value","Sample Name","Buffer Name","Control Name","ID")]
  #Change all factors to strings after the melt:
  j <- sapply(df, is.factor)
  df[j] <- lapply(df[j], as.character)
  colnames(df)[1] <- 'Well' #Rename
  df <- sort_well_ids(df) #Order by Well
  
  return(df)
}


convert_names_384_to_96 <- function(assaydev_name_flatfile_384) {
  
  names384 <- assaydev_name_flatfile_384
  names384 <- names384[c('Sample Name','Buffer Name','Control Name','ID')]
  
  df96 <- data.frame(matrix(ncol = 0, nrow = 0)) #Create new data frame for the avg peak values (96 wells)
  #Loop to calculate average peak values across 4 readings:
  for (x in seq(1,384,48)) {
    for (n in seq(x,x+23,2)) {
      s <- names384$`Sample Name`[n]
      b <- names384$`Buffer Name`[n]
      c <- names384$`Control Name`[n]
      i <- names384$`ID`[n]
      
      df96 <- rbind(df96, data.frame("Sample_Name" = s, "Buffer_Name"=b,"Control_Name"=c,"ID"=i))
    }
  }
  colnames(df96) <- c('Sample Name','Buffer Name','Control Name','ID')
  
  #Convert all data to characters (currently in factor format)
  df96[] <- lapply(df96, as.character)
  
  return(df96)
}

isolate_names_384 <- function(assaydev_name_flatfile_384) {
  #names384 <- read_excel(assaydev_name_flatfile_384)
  # names384 <- assaydev_name_flatfile_384[-c(assaydev_name_flatfile_384$Plate)]  ## removing becasue giving problems with csv file
  names384 <- assaydev_name_flatfile_384[, -match("Plate", colnames(assaydev_name_flatfile_384))]
  return(names384)
}


#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------

##-------- MISCELLANEOUS CLEANING --------##

## If the Sample Name data is partial, need to adjust the summary table so that the empty Sample Name rows get removed. 
## Function to remove blank sample name rows from summary table
drop_NAsamplenames_rows_table <- function(table_w_names) {
  table_w_names <- table_w_names[!is.na(table_w_names$Sample_Name), ]
  table_w_names <- table_w_names[which(table_w_names$Sample_Name!=''), ]
  return(table_w_names)
}


## Update drop down menus after sample names are uploaded (in case of NA sample names):
abridge_well <- function(clean_acsins_names_384) {
  wells <- clean_acsins_names_384[clean_acsins_names_384$`Sample Name`!='',]
  #wells <- clean_acsins_names_384[!is.na(clean_acsins_names_384$Well),]
  return(as.list(wells$Well))
}
abridge_names <- function(clean_acsins_names_384) {
  samplenames <- clean_acsins_names_384[clean_acsins_names_384$`Sample Name`!='',]
  #samplenames <- clean_acsins_names_384[!is.na(clean_acsins_names_384$`Sample Name`),]
  return(as.list(samplenames$`Sample Name`))
}
abridge_buffers <- function(clean_acsins_names_384) {
  buffernames <- clean_acsins_names_384[clean_acsins_names_384$`Sample Name`!='',]
  #buffernames <- clean_acsins_names_384[!is.na(clean_acsins_names_384$`Buffer Name`),]
  return(as.list(buffernames$`Buffer Name`))
}


## Function for "Manual Exclusion" feature: Drop manually selected rows/wells

exclude_selected_columns_rawdata <- function(rawdata, selected_well_list) {  ## well list format e.g. [1] "A2" "A5" "A7"
  drop <- c(selected_well_list)
  rawdata <- rawdata[ , !(names(rawdata) %in% drop)] ## Updated Dec 2019 to fix the exclusion. Do it all at once instead of iteratively (was loop before)
  return(rawdata)
}

## Have to update reactve data in the app so that everything refreshes. Automatic reactive not happening in R Shiny. 

#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------

##-------- AC-SINS ANALYSIS --------##

## VARIABLES OF INTEREST / LOESS
## Generate table will all variables of interest (Loess Method):
processing_loop <- function(clean_acsins_data) {
  
  clean_acsins_data[] <- lapply(clean_acsins_data, function(x) as.numeric(as.character(x)))
  output_vars <- data.frame(matrix(ncol = 11, nrow = 0))
  
  for (str in extract_column_list(clean_acsins_data) ) {
    y <- clean_acsins_data[str]
    y <- sapply(y, as.numeric) #change y to proper format
    df <- data.frame(x=clean_acsins_data$wavelength, y)
    loessmod <- loess(y~x,data=df,span=0.5)
    smoothed <- predict(loessmod)
    peak_i <- which.max(loessmod$fitted)
    peak <- loessmod$x[peak_i]
    rmse <- sqrt((sum(loessmod$fitted-y)**2)/151) #RMSE = root mean squared error
    se <- loessmod$s
    se2 <- ((loessmod$s)*2)
    check <- abs(loessmod$fitted-df[str])
    inrange <- as.numeric(check<rmse) #Using RMSE instead of se2
    inrange_perct <- sum(inrange)/151
    sd <- sd(loessmod$y)
    p50 <- quantile(loessmod$y, c(.5))
    sse <- ((sum(abs(loessmod$residuals)))**2)/151
    nres <- sum(abs(y-loessmod$fitted)/loessmod$fitted)/151
    
    
    output_vars<-rbind(output_vars, data.frame("Well"=str,"Min"=min(y),"Max"=max(y),"Plasmon_Wavelength_Index"=peak_i, 
                                               "Plasmon_Wavelength"=peak, "Standard_Error"=se, "Standard_Error_Doubled"=se2, 
                                               "Percentile50_Absorbance"=p50,"Percent_In2SE_Range"=inrange_perct, 
                                               "Sum_Squared_Errors"=sse, "Scatter_Measure"=nres, "Root_Mean_Squared_Error"=rmse))
    
  }
  return(output_vars)
}


## SMALLER LOOP (FOR PLOTS)
# (Purpose = only process what is necessary to produce Loess plots)
processing_loop_plots <- function(clean_acsins_data, wellname) {
  clean_acsins_data[] <- lapply(clean_acsins_data, function(x) as.numeric(as.character(x)))
  y <- clean_acsins_data[wellname]
  y <- sapply(y, as.numeric) #change y to proper format
  df <- data.frame(x=clean_acsins_data$wavelength, y)
  loessmod <- loess(y~x,data=df,span=0.5)
  smoothed <- predict(loessmod)
  peak_i <- which.max(loessmod$fitted)
  peak <- loessmod$x[peak_i]
  
  return(list("y"=y,"df"=df,"loessmod"=loessmod,"smoothed"=smoothed,"peak_i"=peak_i,"peak"=peak))
}


##-------- LOESS CALCULATIONS --------##

# Smoothed Loess Curve Function:
get_loess_smoothed <- function(df, selected_well) { # format needs to be "df$A1" etc.
  y <- selected_well
  y <- sapply(y, as.numeric)
  x<-df[1]
  x<-sapply(x, as.numeric)
  n_df <- data.frame(x, y)
  loessmod <- loess(y~x,data=n_df,span=0.5)
  smoothedcurve <- predict(loessmod)
  
  return(smoothedcurve)
}

# Find Plasmon Wavelength value:
find_pw <- function(data) {
  pw_result <- (data
                %>% group_by( variable )
                %>% slice( which.max( value ) )
                %>% as.data.frame)
  return(pw_result)
}

# Find Plasmon Wavelength value: ALTERNATE


#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------

##-------- AVERAGE BUFFER VALUES & DELTA PLASMON WAVELENGTH --------##

# Generate table containing the mean plasmon wavelength of 'Buffer' values for each distict Buffer Name group:
calc_meanbuffers_table <- function(any_table_w_names) { 
  any_table_w_names$Plasmon_Wavelength <- as.numeric(as.character(any_table_w_names$Plasmon_Wavelength))
  
  temp <- subset(any_table_w_names, select=c(Sample_Name, Buffer_Name, Plasmon_Wavelength))
  # test debug
  #temp <- filter(temp, Sample_Name=='Buffer') ## commenting this out because it seems to be causing errors in deployment 4/10/2019
  temp <- temp[ which(temp$Sample_Name=='Buffer'),]
  temp <- ddply(temp,~Buffer_Name,summarise,buffer_meanPW=mean(Plasmon_Wavelength))
  return(temp)
}

#Integrate delta plasmon wavelength into summary table:
calc_deltapw_table <- function(acsins_any_table) {
  meanPW_byBuffer <- calc_meanbuffers_table(acsins_any_table)
  
  for (a in 1:nrow(acsins_any_table)) {
    for (b in 1:nrow(meanPW_byBuffer)) {
      if (isTRUE(acsins_any_table$Buffer_Name[a] == meanPW_byBuffer$Buffer_Name[b])) {
        acsins_any_table$Delta_PWL[a] <- acsins_any_table$Plasmon_Wavelength[a]-meanPW_byBuffer$buffer_meanPW[b]
      }
    }
  }
  return(acsins_any_table)
}

#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------


##-------- GENERATE CONTROL-ONLY TABLE --------##

## Added feature: scientists want a static view of the control names and their corresponding plasmon wavelengths and delta plasmon wavelengths for references
## To be tacked on top of the 96-well table. 

create_control_table <- function(clean_acsins_data, clean_acsins_names_96) {
  df <- build_96_well_table(clean_acsins_data, clean_acsins_names_96)[c('Buffer_Name','Control_Name','Plasmon_Wavelength','Delta_PWL')]
  df <- df[ which(!is.na(df$Control_Name)), ]
  
  # in the that there is a duplicated buffer/control pair, drop one. generally the user should not upload data of this kind.
  df <- df[!duplicated(df[1:2]), ]
  
  df_pw <- dcast(df, Control_Name ~ Buffer_Name, value.var="Plasmon_Wavelength")
  colnames(df_pw)[2:ncol(df_pw)] <- paste(colnames(df_pw)[2:ncol(df_pw)], "pwl", sep = "_")
  df_dpw <- dcast(df, Control_Name ~ Buffer_Name, value.var="Delta_PWL")
  colnames(df_dpw)[2:ncol(df_dpw)] <- paste(colnames(df_dpw)[2:ncol(df_dpw)], "delta_pwl", sep = "_")
  
  df <- join(df_pw, df_dpw, by="Control_Name")
  
  return(df)
}


#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------


##-------- RAW DATA TAB --------##

# For the raw data tab, implement "clean_acsins_excel_num(fileName, for_display = 1)"

##-------- SUMMARY TABLE TAB --------##

build_summary_table <- function(clean_acsins_data, clean_acsins_names_384) { # Clean names in 384-format should be reactive value: capture_fullname_df_384("[excel file]")
  l_out <- processing_loop(clean_acsins_data) # ac-sins analysis
  #width <- find_50p_width(clean_acsins_data) # 50% curve width
  #In case of partial read, join with 384-format names:
  l_out <- join(processing_loop(clean_acsins_data), process_wellonly(), type="right", by="Well")
  summary_table <- data.frame("Index"="", "Well" = l_out$Well, "Sample_Name"=clean_acsins_names_384$`Sample Name`, "ID" = "", "Buffer_Name"=clean_acsins_names_384$`Buffer Name`, 
                              "Control_Name"=clean_acsins_names_384$`Control Name`,
                              "Plasmon_Wavelength"=sprintf("%3.0f", as.numeric(l_out$Plasmon_Wavelength)), "Delta_PWL"="", 
                              "Standard_Error"=sprintf("%.6f", as.numeric(l_out$Standard_Error)),
                              "Min"=sprintf("%.6f", as.numeric(l_out$Min)), "Max"=sprintf("%.6f", as.numeric(l_out$Max))) # Build initial data frame
  summary_table$Index <- seq.int(nrow(summary_table)) # Index id var
  
  #Molecule ID:
  summary_table$ID <- clean_acsins_names_384$ID
  
  # Convert factors to correct character and numeric values:
  summary_table[] <- lapply(summary_table[], as.character)
  j <- c('Min','Max','Plasmon_Wavelength','Delta_PWL','Standard_Error') # Isolate those that should be numeric
  summary_table[j] <- suppressWarnings(lapply(summary_table[j], as.numeric))
  
  #Drop rows for which Sample Name is NA:
  summary_table <- drop_NAsamplenames_rows_table(summary_table)
  
  #Integrate Delta PW column to summary table:
  summary_table <- calc_deltapw_table(summary_table)
  
  #reset index id:
  summary_table$Index <- seq.int(nrow(summary_table))
  
  return(summary_table)
}


#flatfile example: "~/assay_dev_example.xlsx"
#Summary table: build_summary_table(cleandata, capture_fullname_df_384("~/assay_dev_example.xlsx"))

##-------- 96-WELL TABLE TAB --------##

build_96_well_table <- function(clean_acsins_data, clean_acsins_names_96) {
  l_out <- processing_loop(clean_acsins_data)
  df <- data.frame("Well" = l_out$Well, "Plasmon_Wavelength"=as.numeric(as.character(l_out$Plasmon_Wavelength)))
  df <- join(df, process_wellonly(), type="right",by="Well") # Partial plate read insurance (type="right" means keep values in right side of join)
  
  df96 <- data.frame(matrix(ncol = 12, nrow = 0)) #Create new data frame for the avg peak values (96 wells)
  #Loop to calculate average peak values across 4 readings:
  for (x in seq(1,384,48)) {
    for (n in seq(x,x+23,2)) {
      a <- df$Plasmon_Wavelength[n]
      b <- df$Plasmon_Wavelength[n+1]
      c <- df$Plasmon_Wavelength[n+24]
      d <- df$Plasmon_Wavelength[n+25]
      mean_pw <- mean(c(a,b,c,d), na.rm=TRUE)
      sd <- sd(c(a,b,c,d), na.rm=TRUE)
      Reading_1 <- df$Well[n]
      Reading_2 <- df$Well[n+1]
      Reading_3 <- df$Well[n+24]
      Reading_4 <- df$Well[n+25]
      toString(c(Reading_1, Reading_2, Reading_3, Reading_4))
      
      df96 <- rbind(df96, data.frame("Index"="","Sample_Name"="", "ID"="", "Buffer_Name"="", "Control_Name"="", 
                                     "Plasmon_Wavelength"=mean_pw, "Standard_Deviation"=sprintf("%.4f", as.numeric(sd)), "Delta_PWL"="",  ## Keep as "Plasmon_Wavelength for compatability with calc_deltapw_table function
                                     "Well1"=Reading_1,"PW1"=sprintf("%.0f", as.numeric(a)), 
                                     "Well2"=Reading_2, "PW2"=sprintf("%.0f", as.numeric(b)), 
                                     "Well3"=Reading_3, "PW3"=sprintf("%.0f", as.numeric(c)), 
                                     "Well4"=Reading_4, "PW4"=sprintf("%.0f", as.numeric(d)))) ## rbind add layers to the data frame with each iteration
    }
  }
  
  df96[c("Index","Sample_Name","ID", "Buffer_Name","Control_Name")] <- c(seq.int(nrow(df96)),clean_acsins_names_96$`Sample Name`,
                                                                         clean_acsins_names_96$`ID`,
                                                                         clean_acsins_names_96$`Buffer Name`,clean_acsins_names_96$`Control Name`)
  df96 <- drop_NAsamplenames_rows_table(df96)
  # Convert factors to correct character and numeric values:
  df96[] <- lapply(df96[], as.character)
  j <- c('Plasmon_Wavelength','Delta_PWL','Standard_Deviation','PW1','PW2','PW3','PW4') # Isolate those that should be numeric
  df96[j] <- suppressWarnings(lapply(df96[j], as.numeric))
  #Delta PW:
  df96<- calc_deltapw_table(df96)
  
  #reset Index:
  df96$Index <- seq.int(nrow(df96))
  
  return(df96)
}


## New function to accomodate duplicates, not just quadruplicates
build_96_well_table_new <- function(clean_acsins_data, clean_acsins_names_384) { ## EXPECTS 384 NAME FORMAT!!
  l_out <- processing_loop(clean_acsins_data)
  df <- data.frame("Well" = l_out$Well, "Plasmon_Wavelength"=as.numeric(as.character(l_out$Plasmon_Wavelength)))
  ## make PWs blank where no name! ----------------------------
  namedf <- clean_acsins_names_384[c('Well','Sample Name')]
  df <- join(namedf, df, by="Well")
  for (x in 1:nrow(df)) {
    if (as.character(df$`Sample Name`[x])=='') {
      df$Plasmon_Wavelength[x] <- NA
    } 
  }
  df <- df[c('Well','Plasmon_Wavelength')]
  #continue ----------------------------------------------------
  df <- join(df, process_wellonly(), type="right",by="Well") # Partial plate read insurance (type="right" means keep values in right side of join)
  
  df96 <- data.frame(matrix(ncol = 12, nrow = 0)) #Create new data frame for the avg peak values (96 wells)
  #Loop to calculate average peak values across 4 readings:
  for (x in seq(1,384,48)) {
    for (n in seq(x,x+23,2)) {
      a <- df$Plasmon_Wavelength[n]
      b <- df$Plasmon_Wavelength[n+1]
      c <- df$Plasmon_Wavelength[n+24]
      d <- df$Plasmon_Wavelength[n+25]
      mean_pw <- mean(c(a,b,c,d), na.rm=TRUE)
      sd <- sd(c(a,b,c,d), na.rm=TRUE)
      Reading_1 <- df$Well[n]
      Reading_2 <- df$Well[n+1]
      Reading_3 <- df$Well[n+24]
      Reading_4 <- df$Well[n+25]
      toString(c(Reading_1, Reading_2, Reading_3, Reading_4))
      
      df96 <- rbind(df96, data.frame("Index"="","Sample_Name"="", "ID"="", "Buffer_Name"="", "Control_Name"="", 
                                     "Plasmon_Wavelength"=mean_pw, "Standard_Deviation"=sprintf("%.4f", as.numeric(sd)), "Delta_PWL"="",  ## Keep as "Plasmon_Wavelength for compatability with calc_deltapw_table function
                                     "Well1"=Reading_1,"PW1"=sprintf("%.0f", as.numeric(a)), 
                                     "Well2"=Reading_2, "PW2"=sprintf("%.0f", as.numeric(b)), 
                                     "Well3"=Reading_3, "PW3"=sprintf("%.0f", as.numeric(c)), 
                                     "Well4"=Reading_4, "PW4"=sprintf("%.0f", as.numeric(d)))) ## rbind add layers to the data frame with each iteration
    }
  }
  #--convert 384 to 96
  clean_acsins_names_96 <- convert_names_384_to_96(clean_acsins_names_384)
  #--
  df96[c("Index","Sample_Name","ID", "Buffer_Name","Control_Name")] <- c(seq.int(nrow(df96)),clean_acsins_names_96$`Sample Name`,
                                                                         clean_acsins_names_96$`ID`,
                                                                         clean_acsins_names_96$`Buffer Name`,clean_acsins_names_96$`Control Name`)
  df96 <- drop_NAsamplenames_rows_table(df96)
  # Convert factors to correct character and numeric values:
  df96[] <- lapply(df96[], as.character)
  j <- c('Plasmon_Wavelength','Delta_PWL','Standard_Deviation','PW1','PW2','PW3','PW4') # Isolate those that should be numeric
  df96[j] <- suppressWarnings(lapply(df96[j], as.numeric))
  #Delta PW:
  df96<- calc_deltapw_table(df96)
  
  #reset Index:
  df96$Index <- seq.int(nrow(df96))
  
  return(df96)
}

##-------- 96-WELL OPTIMIZED TABLE TAB --------##

build_96_well_optimized_table <- function(clean_acsins_data, clean_acsins_names_384, threshold) { ### UPDATED DEC.2019
  normal96 <- build_96_well_table_new(clean_acsins_data, clean_acsins_names_384)
  columns <- c("PW1","PW2","PW3","PW4")
  duplicate <- normal96
  #Boolean indicator of change:
  normal96$`NA?` <- ""
  
  for (row in 1:nrow(normal96)) {
    avg <- duplicate[row, "Plasmon_Wavelength"] # Average equal to Avg PW for each row
    stand_dev <- duplicate[row, "Standard_Deviation"] # Standard Deviation for the original set of PWs
    #Loop through PW values and assign NA dependent on distance from threshold value:
    for (x in columns) {
      if(isTRUE(stand_dev!=0) & isTRUE(abs(avg-normal96[row, x]) >= (stand_dev*threshold) )) {
        normal96[row, x] <- NA #If the difference between a given PW and the average for that row is greater than the threshold value, assign NA (i.e. EXCLUDE IT)
        if (is.na(normal96[row, x])) {
          normal96$`NA?`[row] <- "*"  # If any of the PW values are NA, then edit the boolean indicator
        }
      }
    }
    #Calculate the new average PW based on what gets excluded. "na.omit" key here:
    list<-na.omit(c(normal96[row, "PW1"],normal96[row, "PW2"],normal96[row, "PW3"],normal96[row, "PW4"]))
    normal96[row, "Avg_PW"] <- mean(list)
    normal96[row, "Stand_Dev"] <- sd(list)
  }
  
  optim96 <- normal96 # Transition to optimized table
  optim96[c('Plasmon_Wavelength','Standard_Deviation')] <- optim96[c('Avg_PW','Stand_Dev')] # Rename
  optim96[c('Avg_PW','Stand_Dev')] <- list(NULL) # Remove old comparison vars
  
  #Delta_PW adjustmnet:
  optim96 <- calc_deltapw_table(optim96)
  
  #Prep display of integers:
  optim96[c('Plasmon_Wavelength','Standard_Deviation','Delta_PWL')] <- lapply(optim96[c('Plasmon_Wavelength','Standard_Deviation','Delta_PWL')], sprintf, fmt="%3.3f")
  #optim96[columns] <- lapply(optim96[columns], as.character)
  
  # Make it so the actual text "NA" appears in the cells
  for (i in 1:nrow(optim96)) {
    for (j in columns) {
      if (is.na(optim96[i,j])) {
        optim96[i,j] <- "NA"
      }
    }
  }
  
  #reset Index:
  optim96$Index <- seq.int(nrow(optim96))
  
  return(optim96)
}


##-------- GOODNESS OF FIT TABLE TAB --------##

## GOODNESS OF FIT TABLE, with ordering options
table_fit_categories <- function(clean_acsins_data, clean_acsins_names_384, sortby) { # nameindicator =1 if sample names uploaded. # "sortby" argument should either take on "percent" or "rmse"
  l_out <- processing_loop(clean_acsins_data) #Produce variables of interest from initial processing loop
  l_out <- join(l_out, clean_acsins_names_384[c('Well','Sample Name')], by="Well") 
  
  # Build data frame containing relevant variables: 
  output_fit <- data.frame("Well"=l_out$Well, "Sample_Name"=l_out$`Sample Name`, "Percent_InRange"=as.numeric(l_out$Percent_In2SE_Range),
                           "Root_Mean_Squared_Error"=as.numeric(l_out$Root_Mean_Squared_Error))
  
  #Ordering:
  if (sortby == "percent") {
    output_fit <- output_fit[order(output_fit$Percent_InRange),]
  } else if (sortby == "rmse") {
    output_fit <- output_fit[order(-output_fit$Root_Mean_Squared_Error),] # order RMSE descending because we want biggest RMSE first
  }
  
  #Convert factors to characters:
  output_fit[c('Well','Sample_Name')] <- lapply(output_fit[c('Well','Sample_Name')], as.character)
  
  # only keep rows for which Sample Names are 
  output_fit <- output_fit[which(output_fit$Sample_Name!=''),]
  output_fit <- output_fit[which(!is.na(output_fit$Sample_Name)),]
  
  output_fit[3:4] <- lapply(output_fit[3:4],formatC,digits=4,format="f") # adjusts the digits so that they are limited to 4 places after the decimal. 
  
  return(output_fit)
}


##-------- INDIVIDUAL PLOT TAB --------##

## SINGLE PLOT 
loessplot_acsins_names <- function(clean_acsins_data, wellname, clean_acsins_names_384) {
  p <- processing_loop_plots(clean_acsins_data, splitspace(wellname)) #Variables needed for plot
  #Sample Names:
  samplename <- clean_acsins_names_384$`Sample Name`[clean_acsins_names_384$Well==splitspace(wellname)] # Capture the Sample Name that corresponds to the selected Well ID
  
  plot_output <- {
    plot(p$y,x=clean_acsins_data$wavelength, type="p", main=paste0(splitspace(wellname)," Loess Model: ",samplename), xlab="Wavelength (nm)", ylab="Absorbance (Au)", pch=20, cex=c(0.6))
    lines(p$smoothed, x=clean_acsins_data$wavelength, col="red")
    points(p$peak, p$loessmod$y[p$peak_i], type="p", pch=19, col="red", cex=c(0.9))
    text(p$peak, p$loessmod$y[p$peak_i], labels=p$peak, pos=1, col="red")
  }
  
  return(plot_output)
}

## Alternative NON-NAME plot addition since incorporating partial plate reads:
# (If a partial plate is uploaded, there are issues with only uploading the raw data and not the sample name data)
loessplot_acsins_noname <- function(clean_acsins_data, wellname) {
  p <- processing_loop_plots(clean_acsins_data, splitspace(wellname)) #Variables needed for plot
  
  plot_output <- {
    plot(p$y,x=clean_acsins_data$wavelength, type="p", main=paste0(splitspace(wellname)," Loess Model"), xlab="Wavelength (nm)", ylab="Absorbance (Au)", pch=20, cex=c(0.6))
    lines(p$smoothed, x=clean_acsins_data$wavelength, col="red")
    points(p$peak, p$loessmod$y[p$peak_i], type="p", pch=19, col="red", cex=c(0.9))
    text(p$peak, p$loessmod$y[p$peak_i], labels=p$peak, pos=1, col="red")
  }
  
  return(plot_output)
}


##-------- PLOT GRID TABS (CURVE FIT AND RMSE) --------##

build_plot_grid <- function(clean_acsins_data, clean_acsins_names_384_gfit, nameindicator, sortby) { # sortby="percent" or "rmse" # name data required
  outputfit_wells <- table_fit_categories(clean_acsins_data, clean_acsins_names_384_gfit, sortby=sortby)[c('Well','Sample_Name')]
  
  if (nameindicator == 1) { outputfit_wells <- drop_NAsamplenames_rows_table(outputfit_wells) } # if sample names uploaded, drop rows where sample name blank
  
  plot_grid <- {
    par(mfrow=c(4,2))
    for (x in 1:8) {
      str<-outputfit_wells$Well[x]
      loessplot_acsins_names(clean_acsins_data, str, clean_acsins_names_384_gfit)
    }
  }
  return(plot_grid)
}


##-------- MULTI-CURVE PLOT TAB --------##

# Build the Multi-curve plot:
plot_loess_multiple <- function(clean_acsins_data, selected_well1, selected_well2, selected_well3, selected_well4, selected_well5, selected_well6, selected_well7, selected_well8, checkbox) { # if checkbox == 1, adds vertical line to plot
  
  #Add a null column to the original clean data
  clean_acsins_data$No_Plot <- NA
  #Create list of selections for later use:
  selectionlist <- c(selected_well1, selected_well2, selected_well3, selected_well4, selected_well5, selected_well6, selected_well7, selected_well8)
  #Temporary data frame:
  dftemp <- data.frame(clean_acsins_data[1], clean_acsins_data[splitspace(selected_well1)],clean_acsins_data[splitspace(selected_well2)],
                       clean_acsins_data[splitspace(selected_well3)], clean_acsins_data[splitspace(selected_well4)], 
                       clean_acsins_data[splitspace(selected_well5)], clean_acsins_data[splitspace(selected_well6)],
                       clean_acsins_data[splitspace(selected_well7)], clean_acsins_data[splitspace(selected_well8)])
  #Create the new Loess variables: 
  # (ONLY for non-NA selections in drop-down menu)
  if (any(is.na(dftemp[2]))) {
    dftemp$loess1 <- ""} else {
      dftemp$loess1 <- get_loess_smoothed(dftemp, dftemp[2])}
  if (any(is.na(dftemp[3]))) {
    dftemp$loess2 <- ""} else {
      dftemp$loess2 <- get_loess_smoothed(dftemp, dftemp[3])}
  if (any(is.na(dftemp[4]))) {
    dftemp$loess3 <- ""} else {
      dftemp$loess3 <- get_loess_smoothed(dftemp, dftemp[4])}
  if (any(is.na(dftemp[5]))) {
    dftemp$loess4 <- ""} else {
      dftemp$loess4 <- get_loess_smoothed(dftemp, dftemp[5])}
  if (any(is.na(dftemp[6]))) {
    dftemp$loess5 <- ""} else {
      dftemp$loess5 <- get_loess_smoothed(dftemp, dftemp[6])}
  if (any(is.na(dftemp[7]))) {
    dftemp$loess6 <- ""} else {
      dftemp$loess6 <- get_loess_smoothed(dftemp, dftemp[7])}
  if (any(is.na(dftemp[8]))) {
    dftemp$loess7 <- ""} else {
      dftemp$loess7 <- get_loess_smoothed(dftemp, dftemp[8])}
  if (any(is.na(dftemp[9]))) {
    dftemp$loess8 <- ""} else {
      dftemp$loess8 <- get_loess_smoothed(dftemp, dftemp[9])}
  
  
  
  #Remove original data columns
  dftemp <- dftemp[-c(2:9)]
  #for however many number of columns in the data frame, name them for each selection
  for (i in 2:ncol(dftemp)) {
    colnames(dftemp)[i] <- selectionlist[i-1]
  }
  #melt/reshape the data frame:
  dftemp<-melt(dftemp, id.vars='wavelength', factorsAsStrings=F) #--deprecated
  #dftemp <-gather(test, "variable","value",-wavelength)
  
  #Remove rows for which 'variable' equals 'No_Plot'
  dftemp <- subset(dftemp, variable!='No_Plot')
  
  #calculate plasmon wavelength on loess curve
  pw_result<-find_pw(dftemp) # Utilize "find pw" function to calculate plasmon wavelength for this DF (by group; i.e. by curve)
  setnames(dftemp, old=c("variable"), new=c("Legend")) # Change so that the groups are named Legend in plot
  dftemp$value <- as.numeric(as.character(dftemp$value))
  dftemp$wavelength <- as.numeric(as.character(dftemp$wavelength))
  
  p <- ggplot(dftemp, aes(wavelength, value, group=Legend, color=Legend)) + geom_line() + xlab("Wavelength (nm)") + 
    ylab("Absorbance (Au)") + ggtitle("Loess Curves by Well Selection") + geom_point(data=pw_result, aes(x=as.numeric(pw_result$wavelength), y=as.numeric(pw_result$value))) +
    geom_text_repel(data=filter(pw_result), aes(label=pw_result$wavelength, x=as.numeric(pw_result$wavelength), y=as.numeric(pw_result$value)), show.legend = F)
  
  if (checkbox == 1) p <- p + geom_vline(data=filter(pw_result), aes(xintercept = pw_result$wavelength), color = 'dark grey') # If user clicks checkbox, vertical lines will be added to plot
  
  return(p)
  
} 


#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------


##-------- DOWNLOADABLE --------##

generate_report <- function(clean_acsins_data, clean_acsins_names_384, datetime) {
  df <- build_96_well_table_new(clean_acsins_data, clean_acsins_names_384)[c('Sample_Name','ID','Delta_PWL','Plasmon_Wavelength','Standard_Deviation','Buffer_Name', 'Control_Name')] ## Update Dec. 2019
  # order by control (Oct. 2019 addition)
  # removing order by and putting it into the app script instead
  
  colnames(df) <- c('Variant','ID', 'Delta pwl', 'Ave pwl', 'Std Dev','Buffer', 'Control')
  df$`Acq Date/Time` <- datetime
  #Remove any possible special characters:
  df$Variant <- str_replace_all(df$Variant, "[^[:alnum:]]", " ") ##
  
  return(df)
}

generate_report_opt <- function(clean_acsins_data, clean_acsins_names_384, threshold, datetime) {
  df <- build_96_well_optimized_table(clean_acsins_data, clean_acsins_names_384, threshold)[c('Sample_Name','ID','Delta_PWL','Plasmon_Wavelength','Standard_Deviation','Buffer_Name', 'Control_Name')]
  # order by control (Oct. 2019 addition)
  # removing order by and putting it into the app script instead
  
  colnames(df) <- c('Variant','ID', 'Delta pwl', 'Ave pwl', 'Std Dev','Buffer', 'Control')
  df$`Acq Date/Time` <- datetime
  #Change appropriate variables to numeric:
  df$`Delta pwl` <- suppressWarnings(as.numeric(df$`Delta pwl`))
  df$`Ave pwl` <- suppressWarnings(as.numeric(df$`Ave pwl`))
  df$`Std Dev` <- suppressWarnings(as.numeric(df$`Std Dev`))
  #Remove any possible special characters:
  df$Variant <- str_replace_all(df$Variant, "[^[:alnum:]]", " ")
  
  return(df)
}


#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------


##-------- DATABASE PREPARATION --------##

# For converting the date format to API format:
convert_date_format_for_API <- function(acquisition_date_time) { # Format to start is "12/12/2018 6:48:36 PM" -- convert to "2019-01-14T23:57:06.158Z"
  return(format_iso_8601(parse_iso_8601(parse_date(acquisition_date_time))))
}

#Capture title of excel sheet for experiment_name:
capture_excel_title <- function(raw_acsins_file) {
  
}


##-------- DATA FRAME FOR DATABASE --------##

build_results_df <- function(clean_acsins_data, clean_acsins_names_96, threshold) {
  l_out <- processing_loop(clean_acsins_data)
  df <- data.frame("Well" = l_out$Well, "Plasmon_Wavelength"=as.numeric(as.character(l_out$Plasmon_Wavelength)))
  df <- join(df, process_wellonly(), type="right",by="Well") # Partial plate read insurance (type="right" means keep values in right side of join)
  
  df96 <- data.frame(matrix(ncol = 11, nrow = 0)) #Create new data frame for the avg peak values (96 wells)
  #Loop to calculate average peak values across 4 readings:
  for (x in seq(1,384,48)) {
    for (n in seq(x,x+23,2)) {
      a <- df$Plasmon_Wavelength[n]
      b <- df$Plasmon_Wavelength[n+1]
      c <- df$Plasmon_Wavelength[n+24]
      d <- df$Plasmon_Wavelength[n+25]
      mean_pw <- mean(c(a,b,c,d), na.rm=TRUE)
      sd <- sd(c(a,b,c,d), na.rm=TRUE)
      Reading_1 <- df$Well[n]
      Reading_2 <- df$Well[n+1]
      Reading_3 <- df$Well[n+24]
      Reading_4 <- df$Well[n+25]
      toString(c(Reading_1, Reading_2, Reading_3, Reading_4))
      
      df96 <- rbind(df96, data.frame("Index"="","Sample_Name"="", "Buffer_Name"="", "Control_Name"="", "ID"="", "Well_Position"="",
                                     "Plasmon_Wavelength"=mean_pw, "Standard_Deviation"=sprintf("%.4f", as.numeric(sd)), "Delta_PWL"="",  ## Keep as "Plasmon_Wavelength for compatability with calc_deltapw_table function
                                     "Well1"=Reading_1,"PW1"=sprintf("%.0f", as.numeric(a)), 
                                     "Well2"=Reading_2, "PW2"=sprintf("%.0f", as.numeric(b)), 
                                     "Well3"=Reading_3, "PW3"=sprintf("%.0f", as.numeric(c)), 
                                     "Well4"=Reading_4, "PW4"=sprintf("%.0f", as.numeric(d)))) ## rbind add layers to the data frame with each iteration
    }
  }
  
  df96[c("Index","Sample_Name","Buffer_Name","Control_Name","ID")] <- c(seq.int(nrow(df96)),clean_acsins_names_96$`Sample Name`,
                                                                        clean_acsins_names_96$`Buffer Name`,clean_acsins_names_96$`Control Name`,
                                                                        clean_acsins_names_96$`ID`)
  
  ## Formatting: 
  df96 <- drop_NAsamplenames_rows_table(df96)
  # Convert factors to correct character and numeric values:
  df96[] <- lapply(df96[], as.character)
  columns <- c("PW1","PW2","PW3","PW4")
  wellcolumns <- c("Well1","Well2","Well3","Well4")
  j <- c('Plasmon_Wavelength','Standard_Deviation','Delta_PWL',columns) # Isolate those that should be numeric
  df96[j] <- suppressWarnings(lapply(df96[j], as.numeric))
  
  
  ## Update the PW and SD based on threshold
  duplicate <- df96
  
  for (row in 1:nrow(df96)) {
    avg <- duplicate[row, "Plasmon_Wavelength"] # Average equal to Avg PW for each row
    stand_dev <- duplicate[row, "Standard_Deviation"] # Standard Deviation for the original set of PWs
    for (x in columns) {
      if(isTRUE(stand_dev!=0) & isTRUE(abs(avg-df96[row, x]) >= (stand_dev*threshold) )) {
        df96[row, x] <- NA #If the difference between a given PW and the average for that row is greater than the threshold value, assign NA (i.e. EXCLUDE IT)
      }
    }
    #Calculate the new average PW based on what gets excluded. "na.omit" key here:
    list<-na.omit(c(df96[row, "PW1"],df96[row, "PW2"],df96[row, "PW3"],df96[row, "PW4"]))
    df96[row, "Plasmon_Wavelength"] <- mean(list)
    df96[row, "Standard_Deviation"] <- sd(list)
  }
  
  #Make Well names NA also:
  for (r in 1:nrow(df96)) {
    for (m in 1:4) {
      if (is.na(df96[r,columns[m]])) {
        df96[r,wellcolumns[m]] <- NA
      }
    }
  }
  
  
  #Delta PW:
  df96<- calc_deltapw_table(df96)
  # Well Position variable:
  for (m in 1:nrow(df96)) { df96$Well_Position[m] <- paste0(df96$Well1[m],", ",df96$Well2[m],", ",df96$Well3[m],", ",df96$Well4[m]) }
  # Filter and rename:
  df96 <- df96[c('Sample_Name','Buffer_Name','Control_Name','Well_Position','ID','Plasmon_Wavelength','Standard_Deviation','Delta_PWL')]
  colnames(df96)[] <- c('Sample Name','Buffer Name','Control Name','Well Position','ID','mean plasmon wavelength','stdev plasmon wavelength','delta plasmon wavelength')
  
  ## Edit Buffer names so that the format is "Buffer_[buffer name]"
  for (m in 1:nrow(df96)) {
    if (isTRUE(df96[m,'Sample Name']=='Buffer')) {
      bn <- df96[m,'Buffer Name']
      sn <- df96[m,'Sample Name']
      df96[m,'Sample Name'] <- paste0(sn,"_",bn)
    }
  }
  
  return(df96)
}


## Reshaping the data frame df for the Results API:
build_results_df_api <- function(clean_acsins_data, clean_acsins_names_96, threshold, experiment_id, sample_id) {
  df <- build_results_df(clean_acsins_data, clean_acsins_names_96, threshold) # Start with full df
  results_df <- df[c('Sample Name','mean plasmon wavelength','stdev plasmon wavelength','delta plasmon wavelength')] # Extract only vars of interest
  colnames(results_df)[3] <- c('standard deviation plasmon wavelength') # change wording for string extraction
  results_df$index <- seq.int(nrow(results_df)) # add index id to reorder after reshaping data
  results_df <- melt(results_df, id.vars=c("Sample Name", "index"), factorsAsStrings=F) # Reshape the data
  results_df <- results_df[order(results_df$index),] #
  results_df$result_type <- "plasmon wavelength"
  results_df[c('Sample Name','variable')] <- lapply(results_df[c('Sample Name','variable')], as.character)
  results_df$stats_modifier <- str_remove(results_df$variable, " plasmon wavelength")
  results_df$value_numeric <- results_df$value
  results_df$experiment_sample_display_order <- 0
  for (i in 1:nrow(results_df)) { ## Manually insert the repeating values for display order variable (index starts at 0)
    if (results_df$stats_modifier[i]=='mean') { results_df$display_order[i] <- 0 }
    if (results_df$stats_modifier[i]=='standard deviation') { results_df$display_order[i] <- 1 }
    if (results_df$stats_modifier[i]=='delta') { results_df$display_order[i] <- 2 }
  }
  
  results_df$experiment_id <- experiment_id
  results_df$sample_id <- sample_id
  
  results_df_api <- results_df[c('Sample Name','result_type','display_order','value_numeric','experiment_sample_display_order','experiment_id','sample_id')]
  #context_df_api <- results_df[c('experiment_sample_display_order','')]
  
  return(results_df_api)
}


#-------------------------------
# DOWNLOAD FILE FOR BENCHLING
#-------------------------------

order_by_control <- function(report) {
  report$control <- 0
  report$control[which(report$Control_Name!='')] <- 1
  report <- report[order(report$control, decreasing = TRUE),]
  report$control <- NULL
  return(report)
}

capture_date_for_benchling <- function(raw_file) {
  acq_date <- extract_date_time(raw_file)
  acq_date <- strsplit(acq_date, " ")[[1]][1]
  acq_date <- as.character(as.Date(acq_date, "%m/%d/%Y")) ## actually appears yyyy-mm-dd
  return(acq_date)
}

create_benchling_table <- function(cleaned_384_data, cleaned_384_names, acquisition_date) {
  report <- build_96_well_table_new(cleaned_384_data, cleaned_384_names)
  report <- order_by_control(report)
  # Additional columns needed for Benchling result schema:
  report$assay <- "AC-SINS"
  report$`well reference` <- paste(report$Well1,report$Well2, report$Well3, report$Well4, sep = ",")
  report$`acquisition date` <- acquisition_date
  report <- report[c('ID','assay','acquisition date','well reference','Buffer_Name','Control_Name','Delta_PWL','Plasmon_Wavelength',
                     'Standard_Deviation','PW1','PW2','PW3','PW4')]
  colnames(report) <- c('Sample ID','assay','acquisition date','well reference','buffer','control',
                        'delta plasmon wavelength','average plasmon wavelength',
                        'standard deviation','quadrant 1 plasmon wavelength','quadrant 2 plasmon wavelength',
                        'quadrant 3 plasmon wavelength','quadrant 4 plasmon wavelength')
  for (x in c(6,10:13)) {
    report[x][is.na(report[x])] <- ""
  } # replace NA values with blanks for upload prep
  # round decimal places: for onset and IC-50, 2. for rest of numeric variables, 4. 
  
  return(report)
  
}