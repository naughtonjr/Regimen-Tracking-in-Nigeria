#####################
## -- Libraries -- ##
#####################

library(openxlsx)
library(tidyverse)
library(Hmisc)
library(data.table)
library(lubridate)
library(gridExtra)
library(scales)
library(grid)

# --- Directory --- #

path1 <- ("./Historical Linelists/full line list - 2020-07-01.csv")
path2 <- ("./Historical Linelists/full line list - 2020-06-01.csv")
path3 <- ("./Historical Linelists/Full Line List - 2020-08-02.csv")
path4 <- ("./Historical Linelists/full line list - 2020-09-06.csv")
path5 <- ("./Historical Linelists/full line list - 2020-10-05.csv")
path6 <- ("./Historical Linelists/full line list - 2020-11-04.csv")
path7 <- ("./Historical Linelists/full line list - 2020-12-07.csv")
path8 <- ("./Historical Linelists/full line list - 2021-01-11.csv")
path9 <- ("./Historical Linelists/full line list - 2021-02-08.csv")
path10 <- ("./Historical Linelists/full line list - 2021-04-06.csv")
path11 <- ("./Historical Linelists/full line list - 2021-05-05.csv")
path12 <- ("./Historical Linelists/full line list - 2021-06-01.csv")
path13 <- ("./Historical Linelists/full line list - 2021-07-12.csv")
path14 <- ("./Historical Linelists/full line list - 2021-08-10.csv")
path15 <- ("./Historical Linelists/full line list - 2021-09-06.csv")



pathlist <- c(path4,
              path5,
              path6,
              path7,
              path8,
              path9,
              path10,
              path11,
              path12,
              path13,
              path14,
              path15)

############################
## -- Import functions -- ##
############################

source("./TLD analysis/RegimenAnalysis_Func_JRN.R")


FullregAnalysisGO <- function(LLpath){
  ############################
  ## -- Import Linelist  -- ##
  ############################
  
  linelistfilterGO(path10)
  
  ################################################
  ## --  Important Global Variables / Lists  -- ##
  ################################################

  importlistGO(df.full_line)
  
  ##################################
  ######## 0 - 9 age cohort ########
  ##################################
  
  zero9GO(df.zero9_full)
  
  ####################################
  ######## 10 - 19 age cohort ########
  ####################################
  
  ten19GO(df.ten19_full)
  
  ################################
  ######## 15+ age cohort ########
  ################################
  
  fifteenplusGO(df.15plus_full)
  
  ### Write data to folders
  writeGO()
  
  #### TIME SERIES ANALYSIS ####
  TS.zero9prepGO("All")
  # TS.zero9prepGO("CDC")
  regOPTdfs()
  
  TS.ten19prepGO("All")
  # TS.ten19prepGO("CDC")
  
  TS.15plusprepGO("All")
  # TS.15plusprepGO("CDC")
  
  ## Write workbook, enter "[monthyear]" into function
  writeWB_ALLGO("September2021")
  # writeWB_CDCGO("September2021")
}

ptm <- proc.time()
FullregAnalysisGO(path15)
proc.time() - ptm

# FullregAnalysisGO(path1)
# for(path in pathlist){
#   FullregAnalysisGO(path)
# }
