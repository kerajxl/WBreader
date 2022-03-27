
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if (!require("readxl")) install.packages("readxl")
library(readxl)
if (!require("devtools")) install.packages("devtools")
library(devtools)
options(scipen=99)

#path <- 'Data_Extract_From_World_Development_Indicators.xlsx'

wb_read_excel <- function(path, extension = 'xlsx', keep_NA = FALSE) {
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
    spread(`Series Name`, value)  %>%
    mutate(year = str_extract(year_x, "[0-9]{4}")) %>%
    select(-year_x)
  df <- df[,c(1,2,ncol(df),4:ncol(df)-1)]
  for (col in colnames(df)){
    df[,col] <- replace(df[,col], df[,col] == '..', NA)
  }
  if(keep_NA == FALSE){
    df <- na.omit(df)
  }

  View(df)
  return(df)
}


