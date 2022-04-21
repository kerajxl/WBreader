if (!require("tidyverse")) install.packages("dplyr")
library(dplyr)
if (!require("readxl")) install.packages("readxl")
library(readxl)
if (!require("devtools")) install.packages("devtools")
library(devtools)
if (!require('countrycode')) install.packages('countrycode')
library(countrycode)
options(scipen=99)




#path <- 'Data_Extract_From_World_Development_Indicators.xlsx'

wb_read_excel <- function(path, extension = 'xlsx', keep_NA = FALSE, preview = FALSE,
                          iso_coding = '2c', prettyValue = TRUE) {

  if(extension == 'xlsx'){
    df <- read_excel(path)
  } else if (extension == 'csv'){
    df <- read.csv2(path)
  }

  df <- df %>%
    select(-`Series Code`) %>%
    drop_na(`Country Code`)
  df <- df %>%
    gather(year_x, value, 4:ncol(df)) %>%
    #spread(`Series Name`, value)  %>%
    mutate(year = str_extract(year_x, "[0-9]{4}")) %>%
    select(-year_x)
  df <- df[,c(1,2,ncol(df),4:ncol(df)-1)]
  for (col in colnames(df)){
    df[,col] <- replace(df[,col], df[,col] == '..', NA)
  }
  if(keep_NA == FALSE){
    df <- na.omit(df)
  }
 
  if(iso_coding == '2c'){
    df$'Country Code' <- countrycode(df$'Country Name', "country.name", "iso2c")
    df$'Country Code' <- tolower(df$'Country Code')
  }
  df$year <- as.integer(df$year)
  
  df$value <- as.numeric(df$value)
  
  if(prettyValue == TRUE){
    df <- df %>% 
      mutate(prettyValue = ifelse(grepl('%',`Series Name`, fixed = TRUE)==TRUE, paste0(round(value*100,2),'%'), 
                                  format(round(value,0),nsmall = 0, big.mark="'")))
  }
   if(preview == TRUE){
    View(df)
  }
  df$year <- as.integer(df$year)
  
  df$value <- as.numeric(df$value)
 
  
  return(df)
}
