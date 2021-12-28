#### Nigeria Regimen Analysis Script
#### Functions are used in Regimen analysis to create excel regimen analysis workbook
#### AUTHOR: Jeff Naughton (Jeffnaughton2@gmail.com)
#### CREATION DATE: 9/10/2021

# Searches dataframe (DF) state list to find missing states
search <- function(x){
  statefull[statefull %nin% x]
}

# Searches DF state list to find missing CDC partner states
searchcdc <- function(x){
  statefullCDC[statefullCDC %nin% x]
}

# Adds entries of states missing from DF
FillmissingGO <- function(tableX, colX, Ncols, partnertype){
  ifelse(partnertype == "cdc", 
    missing <- data.frame(searchcdc(colX)), 
    missing <- data.frame(search(colX))
        )
  col_names <- names(tableX)
  df.tmp <- data.frame()
  for(i in seq_len(nrow(missing))){
    tmp <- cbind(missing[i,1], matrix(0, ncol = Ncols - 1, nrow = 1))
    df.tmp <- rbind(df.tmp,tmp)
  }
  colnames(df.tmp) <- col_names
  rbind(tableX, df.tmp)
}

# Common lists needed throughout script
importlistGO <- function(linelist){
  cdc_ip <<- list("IHVN", "APIN", "CCFN", "CIHP")
  globalfund_ip <<- list("ABIA", "TARABA")
  
  usaid_ip <<- list("Chemonics_SHARP_TO3",
                   "JHPIEGO",
                   "SFH",
                   "FHI360",
                   "HAN",
                   "FHI360_GF",
                   "FHI360_SHARP_TO2",
                   "Chemonics",
                   "FHI360")
  statefullCDC <<- as.list(str_sort(unique(linelist$state_name[linelist$ip %in% cdc_ip]), numeric = F))
  statefull <<- as.list(str_sort(unique(linelist$state_name), numeric = F))
}

# Filters out transferred/dead patients from linelist
linelistfilterGO <- function(linelistpath){
  df.full_line <<- read.csv(linelistpath, na.strings = "NULL")
  #### Remove died and transferred #########
  df.died <- subset(df.full_line, !is.na(patient_deceased_date) | patient_has_died==1)
  df.transfer <- subset(df.full_line, !is.na(transferred_out_date) | patient_transferred_out==1)
  test_tsfr <- subset(df.transfer, as.Date(last_drug_pickup_date) < as.Date(transferred_out_date))
  df2.full_line <- subset(df.full_line, is.na(patient_deceased_date) & (patient_has_died==0 | is.na(patient_has_died)))
  df3.full_line <- subset(df2.full_line, is.na(transferred_out_date) & (patient_transferred_out==0 | is.na(patient_transferred_out)))
  #### create age cohort dfs ######
  df.zero9_full <<- subset(df3.full_line, current_age <= 9)
  df.ten19_full <<- subset(df3.full_line, current_age >= 10 & current_age <= 19)
  df.15plus_full <<- subset(df3.full_line, current_age >= 15)
  current_date <<- as.Date(df.zero9_full$created_at[1])
  mnth <<- format(current_date, "%b")
  yr <<- format(current_date, "%y")
}

# Create DF for 0-9 age cohort
zero9_indivGO <- function(df_input, wt.cat){
  df <- df_input
  # LPVr or ATVr generalized
  zero9lpatvr_gen <- unique(grep(".LPV/r|.ATV/r", df$last_drug_regimen, value = TRUE))
  # TLD generalized
  zero9dtg_gen <- unique(grep(".DTG", df$last_drug_regimen, value = TRUE))
  # NNRTI generalized
  zero9nnrti_gen <- unique(grep(".NVP|.EFV", df$last_drug_regimen, value = TRUE))
  # False regimens generalized
  zero9false_gen <- unique(grep(" ", df$last_drug_regimen, value = TRUE))
  # Other regimens generalized
  zero9other_gen <- unique(df$last_drug_regimen[!is.na(df$last_drug_regimen) &
                                                                      df$last_drug_regimen %nin% zero9lpatvr_gen &
                                                                      df$last_drug_regimen %nin% zero9nnrti_gen &
                                                                      df$last_drug_regimen %nin% zero9dtg_gen &
                                                                      df$last_drug_regimen %nin% zero9false_gen])
  
  # Check for any missed regimens (result should only be NA)
  unique(df$last_drug_regimen[df$last_drug_regimen %nin% zero9lpatvr_gen &
                                               df$last_drug_regimen %nin% zero9nnrti_gen &
                                               df$last_drug_regimen %nin% zero9dtg_gen &
                                               df$last_drug_regimen %nin% zero9false_gen &
                                               df$last_drug_regimen %nin% zero9other_gen])
  
  
  # Categorize and aggregate regimens
  # LPV/r and ATV/r variable
  df <- df %>% 
    mutate(LPV_ATV = ifelse(last_drug_regimen %in% zero9lpatvr_gen, 1, 0))
  ## %VLS variable
  df <- df %>% 
    mutate(LPV_ATV_VLS = if_else(
                          (last_drug_regimen %in% zero9lpatvr_gen & current_viral_load < 1000), 1, 0)) %>%
    replace_na(list(LPV_ATV_VLS = 0))
  
  # DTG variable
  df <- df %>% 
    mutate(DTG = if_else(last_drug_regimen %in% zero9dtg_gen, 1, 0))
  
  ## %VLS variable
  df <- df %>% 
    mutate(DTG_VLS = if_else(
                      (last_drug_regimen %in% zero9dtg_gen & current_viral_load < 1000), 1, 0)) %>%
    replace_na(list(DTG_VLS = 0))
  
  # NNRTI regimen [EFV | NVP]
  df <- df %>% 
    mutate(NNRTI = if_else(last_drug_regimen %in% zero9nnrti_gen, 1, 0))
  
  ## %VLS variable
  df <- df %>%
    mutate(NNRTI_VLS = if_else(
                        (last_drug_regimen %in% zero9nnrti_gen & current_viral_load < 1000), 1, 0)) %>%
    replace_na(list(NNRTI_VLS = 0))
  
  # Other regimens
  df <- df %>%
    mutate(other = if_else(last_drug_regimen %in% zero9other_gen, 1, 0))
  
  ## %VLS variable
  df <- df %>%
    mutate(other_VLS = if_else(
                        (last_drug_regimen %in% zero9other_gen & current_viral_load < 1000), 1, 0)) %>%
    replace_na(list(other_VLS = 0))
  
  # No regimen documented
  df <- df %>%
    mutate(No_reg = if_else(is.na(last_drug_regimen) | last_drug_regimen %in% zero9false_gen, 1, 0))
           
  ## %VLS variable
  df <- df %>%
    mutate(No_reg_VLS = if_else(
                          (last_drug_regimen %in% zero9false_gen & current_viral_load < 1000), 1, 0)) %>%
    replace_na(list(No_reg_VLS = 0))
  
  ##------------------##
  ##     Aggregate    ##
  ##------------------##
  
  #### ALL Partners ####
  
  #aggregate CDC + nonCDC partners
  df_agg <- aggregate(list(LPVr_ATVr = df$LPV_ATV,
                           LPVr_ATVr_vls = df$LPV_ATV_VLS,
                           DTG = df$DTG,
                           DTG_vls = df$DTG_VLS,
                           NNRTI = df$NNRTI,
                           NNRTI_vls = df$NNRTI_VLS,
                           Other = df$other,
                           Other_vls = df$other_VLS,
                           No_regimen_documented= df$No_reg,
                           No_regimen_documented_vls= df$No_reg_VLS),
                      by = list(State = df$state_name), sum)

  
  # add in VLS%
  df_agg <- df_agg %>%
    mutate(
      LPVr_ATVr_VLSperc = case_when(
        LPVr_ATVr == 0 ~ "0%",
        LPVr_ATVr != 0 ~ paste(round(100*(LPVr_ATVr_vls/ LPVr_ATVr)),"%", sep = "")),
      DTG_VLSperc = case_when(
        DTG == 0 ~ "0%",
        DTG != 0 ~ paste(round(100*(DTG_vls/ DTG)),"%", sep = "")),
      NNRTI_VLSperc = case_when(
        NNRTI == 0 ~ "0%",
        NNRTI != 0 ~ paste(round(100*(NNRTI_vls/ NNRTI)),"%", sep = "")),
      Other_VLSperc = case_when(
        Other == 0 ~ "0%",
        Other != 0 ~ paste(round(100*(Other_vls/ Other)),"%", sep = "")),
      No_regimen_documented_VLSperc = case_when(
        No_regimen_documented == 0 ~ "0%",
        No_regimen_documented != 0 ~ paste(round(100*(No_regimen_documented_vls/ No_regimen_documented)),"%", sep = ""))
    )

  # Fill in missing states
  if(length(df_agg$State) < length(statefull)){
    n = length(statefull) - length(df_agg$State)
    for(i in seq(1,n)){
      df_agg <- FillmissingGO(df_agg, df_agg$State, length(df_agg), "noncdc")
      df_agg <- arrange(df_agg, State)
    }
  }


  # Add Analysis Date
  df_agg$datapull.date <- current_date
  # Add Weight category
  df_agg$Wt.Cat <- wt.cat

  # rearrange
  df_agg_final <- df_agg[, c("State", "LPVr_ATVr", "LPVr_ATVr_vls", "LPVr_ATVr_VLSperc", 
                              "DTG", "DTG_vls", "DTG_VLSperc", 
                              "NNRTI", "NNRTI_vls", "NNRTI_VLSperc",
                              "Other", "Other_vls", "Other_VLSperc", 
                              "No_regimen_documented", "No_regimen_documented_vls", "No_regimen_documented_VLSperc",
                              "datapull.date", "Wt.Cat")]
  # Rename
  assign(paste("df_agg_", wt.cat, sep = ""), df_agg_final, envir = .GlobalEnv)
  
  #### CDC Partners ####
  
  # Subset for CDC / non-CDC partners
  df_CDC <- subset(df, ip %in% cdc_ip)
  #aggregate CDC partners
  df_CDCagg <- aggregate(list(LPVr_ATVr = df_CDC$LPV_ATV,
                              LPVr_ATVr_vls = df_CDC$LPV_ATV_VLS,
                              DTG = df_CDC$DTG,
                              DTG_vls = df_CDC$DTG_VLS,
                              NNRTI = df_CDC$NNRTI,
                              NNRTI_vls = df_CDC$NNRTI_VLS,
                              Other = df_CDC$other,
                              Other_vls = df_CDC$other_VLS,
                              No_regimen_documented= df_CDC$No_reg,
                              No_regimen_documented_vls= df_CDC$No_reg_VLS),
                          by = list(State = df_CDC$state_name), sum)

  # add in VLS%
  df_CDCagg <- df_CDCagg %>%
    mutate(
      LPVr_ATVr_VLSperc = case_when(
        LPVr_ATVr == 0 ~ "0%",
        LPVr_ATVr != 0 ~ paste(round(100*(LPVr_ATVr_vls/ LPVr_ATVr)),"%", sep = "")),
      DTG_VLSperc = case_when(
        DTG == 0 ~ "0%",
        DTG != 0 ~ paste(round(100*(DTG_vls/ DTG)),"%", sep = "")),
      NNRTI_VLSperc = case_when(
        NNRTI == 0 ~ "0%",
        NNRTI != 0 ~ paste(round(100*(NNRTI_vls/ NNRTI)),"%", sep = "")),
      Other_VLSperc = case_when(
        Other == 0 ~ "0%",
        Other != 0 ~ paste(round(100*(Other_vls/ Other)),"%", sep = "")),
      No_regimen_documented_VLSperc = case_when(
        No_regimen_documented == 0 ~ "0%",
        No_regimen_documented != 0 ~ paste(round(100*(No_regimen_documented_vls/ No_regimen_documented)),"%", sep = ""))
    )

  # Fill in missing states
  if(length(df_CDCagg$State) < length(statefullCDC)){
    n = length(statefullCDC) - length(df_CDCagg$State)
    for(i in seq(1,n)){
      df_CDCagg <- FillmissingGO(df_CDCagg, df_CDCagg$State, length(df_CDCagg), "cdc")
      df_CDCagg <- arrange(df_CDCagg, State)
    }
  }

  # Add Analysis Date
  df_CDCagg$datapull.date <- current_date
  # Add Weight category
  df_CDCagg$Wt.Cat <- wt.cat

  # rearrange
  df_CDCagg_final <- df_CDCagg[, c("State", "LPVr_ATVr", "LPVr_ATVr_vls", "LPVr_ATVr_VLSperc", 
                              "DTG", "DTG_vls", "DTG_VLSperc", 
                              "NNRTI", "NNRTI_vls", "NNRTI_VLSperc",
                              "Other", "Other_vls", "Other_VLSperc", 
                              "No_regimen_documented", "No_regimen_documented_vls", "No_regimen_documented_VLSperc",
                              "datapull.date", "Wt.Cat")]
  # Rename
  assign(paste("df_CDCagg_", wt.cat, sep = ""), df_CDCagg_final, envir = .GlobalEnv)
  
}

# This function creates DFs used in time series analysis and used to fill table in workbook
zero9GO <- function(df.zero9_full){
  # Filter for Active patients, Q3 / Q4 added for recent data analysis
  df.zero9_full_act <- subset(df.zero9_full, ((as.Date(last_drug_pickup_date) + days_of_arv_refill + 28) > current_date) |
                                currentStatus_28_Q3 == "Active" | currentStatus_28_Q4 == "Active"
                              )
  
  # separate by weight
  df.zero9_act_LT20 <- subset(df.zero9_full_act, last_recorded_weight < 20)
  df.zero9_act_20_30 <- subset(df.zero9_full_act, last_recorded_weight >= 20 & last_recorded_weight < 30)
  df.zero9_act_GT30 <- subset(df.zero9_full_act, last_recorded_weight >= 30)
  # find no weight no regimen
  df.zero9_act_noweight <- subset(df.zero9_full_act, is.na(last_recorded_weight))
  df.zero9_act_noweightcdc <- subset(df.zero9_full_act, is.na(last_recorded_weight) & ip %in% cdc_ip)
  noweight <<- length(df.zero9_act_noweight$pid)
  noweightCDC <<- length(df.zero9_act_noweightcdc$pid)
  noweightnoreg <<- sum(is.na(df.zero9_act_noweight$last_drug_regimen))
  noweightnoregCDC <<- sum(is.na(df.zero9_act_noweightcdc$last_drug_regimen))
  
  ##------------------------##
  ##    Less than 20 kg     ##
  ##------------------------##
  zero9_indivGO(df.zero9_act_LT20, "LT20")
  df.LT20reg <<- as.data.frame(getRegGO(df.zero9_act_LT20))

  ##------------------------##
  ##      20 to 30 kg       ##
  ##------------------------##
  zero9_indivGO(df.zero9_act_20_30, "20_30")
  df.20_30reg <<- as.data.frame(getRegGO(df.zero9_act_20_30))

  ##--------------------------------##
  ##    Greater than than 30 kg     ##
  ##--------------------------------##
  zero9_indivGO(df.zero9_act_GT30, "GT30")
  df.GT30reg <<- as.data.frame(getRegGO(df.zero9_act_GT30))
}

# This function creates DFs used in time series analysis and used to fill table in workbook
teenAdultGO <- function(df_input, age.cat){
  # Set DF
  df2 <- df_input
  ## Group Regimens
  # TLD generalized
  df2_tld_gen <- unique(grep(".DTG", df2$last_drug_regimen, value = TRUE))
  # false reg generalized
  df2_falsereg_gen <- unique(grep(" ", df2$last_drug_regimen, value = TRUE))
  # non-TLD (TLD eligible) generalized
  df2_tldelig_gen <- unique(df2$last_drug_regimen[!is.na(df2$last_drug_regimen) &
                                                         df2$last_drug_regimen %nin% df2_tld_gen &
                                                         df2$last_drug_regimen %nin% df2_falsereg_gen])
  # check for new regimens
  regimenCHECK <- unique(df2$last_drug_regimen[df2$last_drug_regimen %nin% df2_falsereg_gen & 
                                               df2$last_drug_regimen %nin% df2_tld_gen &
                                               df2$last_drug_regimen %nin% df2_tldelig_gen])
  print(regimenCHECK)
                            
  ##------------------##
  ##       Flag       ##
  ##------------------##
  
  df2 <- df2 %>%
    mutate(
      # Find patients on TLD 
      on_tld = if_else(last_drug_regimen %in% df2_tld_gen, 1,0),
      # Find patients TLD eligible
      tld_eligible = if_else(last_drug_regimen %in% df2_tldelig_gen, 1, 0),
      # Find patients with No reg documented
      No_reg = if_else(is.na(last_drug_regimen) | last_drug_regimen %in% df2_falsereg_gen, 1, 0))
          
  ##------------------##
  ##     Aggregate    ##
  ##------------------##
  
  #### ALL Partners ####
  df2_agg <- aggregate(list(No_reg_doc = df2$No_reg,
                            tld_elig = df2$tld_eligible,
                            on_tld = df2$on_tld),
                       by = list(State = df2$state_name), sum)

   # Add data pull date
  df2_agg$datapull.date <- current_date
  # Add age category
  df2_agg$age.cat <- age.cat
  # Rename
  assign(paste("df_agg_", age.cat, sep = ""), df2_agg, envir = .GlobalEnv)
    
  
  #### CDC Partners ####
  # Subset for CDC partners
  df2_CDC <- subset(df2, ip %in% cdc_ip) 
  #aggregate (CDC partners)
  df2_CDCagg <- aggregate(list(No_reg_doc = df2_CDC$No_reg,
                               tld_elig = df2_CDC$tld_eligible,
                               on_tld = df2_CDC$on_tld),
                          by = list(State = df2_CDC$state_name), sum)

   # Add data pull Date
  df2_CDCagg$datapull.date <- current_date
  # Add age category
  df2_CDCagg$age.cat <- age.cat
  # Rename
  assign(paste("df_CDCagg_", age.cat, sep = ""), df2_CDCagg, envir = .GlobalEnv)
  
}

# Currently this is most generous "active requirement", This might need to be changed to reflect active/ quarter
ten19GO <- function(df.ten19_full){
  # Active 
  df.ten19_full_act <- subset(df.ten19_full, ((as.Date(last_drug_pickup_date) + days_of_arv_refill + 28) > current_date |
                                                currentStatus_28 == "Active")
                              )
  teenAdultGO(df.ten19_full_act,"ten19")
  df.ten19reg <<- as.data.frame(getAdultRegGO(df.ten19_full_act))
}

fifteenplusGO <- function(df.15plus_full){
  # Filter for Active patients
  df.15plus_full_act <- subset(df.15plus_full, (((as.Date(last_drug_pickup_date) + days_of_arv_refill + 28) > current_date) |
                                 currentStatus_28 == "Active")
  )
  teenAdultGO(df.15plus_full_act,"ovr15")
  df.15plusreg <<- as.data.frame(getAdultRegGO(df.15plus_full_act))
}

# Write DFs to CDC and All Partner folders to be used in Time series development
writeGO <- function(){
  ### write files CDC+non-CDC files
  write.csv(df_agg_ten19, paste("./TLD analysis/Time series/All/ten19/df_agg_ten19_", mnth,yr,".csv", sep=""), na = "", row.names = FALSE)
  write.csv(df_agg_ovr15, paste("./TLD analysis/Time series/All/ovr15/df_agg_ovr15_", mnth,yr,".csv", sep=""), na = "", row.names = FALSE)
  write.csv(df_agg_LT20, paste("./TLD analysis/Time series/All/zero9/df_agg_LT20_", mnth,yr,".csv", sep=""), na = "", row.names = FALSE)
  write.csv(df_agg_20_30, paste("./TLD analysis/Time series/All/zero9/df_agg_20_30_", mnth,yr,".csv", sep=""), na = "", row.names = FALSE)
  write.csv(df_agg_GT30, paste("./TLD analysis/Time series/All/zero9/df_agg_GT30_", mnth,yr,".csv", sep=""), na = "", row.names = FALSE)
  ### write files CDC files
  write.csv(df_CDCagg_ten19, paste("./TLD analysis/Time series/CDC/ten19/df_CDCagg_ten19_", mnth, yr, ".csv", sep=""), na = "", row.names = FALSE)
  write.csv(df_CDCagg_ovr15, paste("./TLD analysis/Time series/CDC/ovr15/df_CDCagg_ovr15_", mnth,yr,".csv", sep=""), na = "", row.names = FALSE)
  write.csv(df_CDCagg_LT20, paste("./TLD analysis/Time series/CDC/zero9/df_CDCagg_LT20_", mnth,yr,".csv", sep=""), na = "", row.names = FALSE)
  write.csv(df_CDCagg_20_30, paste("./TLD analysis/Time series/CDC/zero9/df_CDCagg_20_30_", mnth,yr,".csv", sep=""), na = "", row.names = FALSE)
  write.csv(df_CDCagg_GT30, paste("./TLD analysis/Time series/CDC/zero9/df_CDCagg_GT30_", mnth,yr,".csv", sep=""), na = "", row.names = FALSE)
}

# Creates DF of 0-9 age cohort Regimen lists to be used in writing workbook
getRegGO <- function(df){
  # LPVr or ATVr generalized
  lpatvr_gen <- unique(grep(".LPV/r|.ATV/r", df$last_drug_regimen, value = TRUE))
  # TLD generalized
  dtg_gen <- unique(grep(".DTG", df$last_drug_regimen, value = TRUE))
  # NNRTI generalized
  nnrti_gen <- unique(grep(".NVP|.EFV", df$last_drug_regimen, value = TRUE))
  # False regimens generalized
  false_gen <- unique(grep(" ", df$last_drug_regimen, value = TRUE))
  # Other regimens generalized
  other_gen <- unique(df$last_drug_regimen[!is.na(df$last_drug_regimen) &
                                          df$last_drug_regimen %nin% lpatvr_gen &
                                          df$last_drug_regimen %nin% nnrti_gen &
                                          df$last_drug_regimen %nin% dtg_gen &
                                          df$last_drug_regimen %nin% false_gen])
  TX_CurrReg <- c(lpatvr_gen, dtg_gen, nnrti_gen, false_gen, other_gen)
  # To create DF, vectors need to be the same length before using cbind
  n <- length(TX_CurrReg)
  length(lpatvr_gen) <- n
  length(dtg_gen) <- n
  length(nnrti_gen) <- n 
  length(false_gen) <- n 
  length(other_gen) <- n

  df.regimen = cbind(lpatvr_gen, NA, dtg_gen, NA, nnrti_gen, NA, false_gen, NA, other_gen, NA, TX_CurrReg)

  return(df.regimen)
}

# Creates DF of Adolescent and Adult Regimen lists to be used in writing workbook
getAdultRegGO <- function(df2){
  # TLD generalized
  df2_tld_gen <- unique(grep(".DTG", df2$last_drug_regimen, value = TRUE))
  # false reg generalized
  df2_falsereg_gen <- unique(grep(" ", df2$last_drug_regimen, value = TRUE))
  # non-TLD (TLD eligible) generalized
  df2_tldelig_gen <- unique(df2$last_drug_regimen[!is.na(df2$last_drug_regimen) &
                                                         df2$last_drug_regimen %nin% df2_tld_gen &
                                                         df2$last_drug_regimen %nin% df2_falsereg_gen])

  TX_CurrReg <- c(df2_tld_gen, df2_falsereg_gen, df2_tldelig_gen)
  # To create DF, vectors need to be the same length before using cbind
  n <- length(TX_CurrReg)
  length(df2_tld_gen) <- n
  length(df2_falsereg_gen) <- n
  length(df2_tldelig_gen) <- n 
  df.regimen = cbind(TX_CurrReg, df2_falsereg_gen, df2_tldelig_gen, df2_tld_gen)

  return(df.regimen)
}


#######################################
#### Time Series Data Functions #######
#######################################


TS.zero9prepGO <- function(partnertype){
  # import files and separate into individual data frames
  ifelse(partnertype == "All",
  # All Partners
  filelist <<- list.files(path = "./TLD analysis/Time series/All/zero9/", pattern = "*.csv", full.names = TRUE),
  # CDC Partners
  filelist <<- list.files(path = "./TLD analysis/Time series/CDC/zero9/", pattern = "*.csv", full.names = TRUE))

  dflist <<- lapply(filelist, read.csv, na.strings = "NULL")
  df.alldata <<- do.call(rbind, lapply(filelist, function(path) {
    df <- read.csv(path, na.strings = "NULL")
    df[["source"]] <- rep(path, nrow(df))
    df}))
  dflist_tmp <- list ()
  split <- length(dflist)/3

  df.20_30 <<- do.call("rbind", dflist[1:split])
  df.GT30 <<- do.call("rbind", dflist[(split+1):(2*split)])
  df.LT20 <<- do.call("rbind", dflist[((2*split)+1):length(dflist)])

  # df.alldata$datapull.date <- sapply(df.alldata$datapull.date, function(x) gsub("02/08/2021", "2021-02-08", x))

  # Replacing above section in case future datapull.date imports have different formats
  mdy <- mdy(df.alldata$datapull.date)
  ymd <- ymd(df.alldata$datapull.date)
  mdy[is.na(mdy)] <- ymd[is.na(mdy)]
  df.alldata$datapull.date <- mdy

  # Set weight categories 
  df.alldata$Wt.Cat = factor(df.alldata$Wt.Cat, 
                             levels = c("LT20", "20_30", "GT30"), 
                             ordered = TRUE)
  #Change weight categories
  df.alldata$Wt.Cat <- case_when(
                      df.alldata$Wt.Cat == "LT20" ~ "< 20 kg",
                      df.alldata$Wt.Cat == "20_30" ~ "20 to 30 kg",
                      df.alldata$Wt.Cat == "GT30" ~ "> 30 kg"
    )

  # Split datapull.date and create TX_Curr variable
  df.alldata2 <<- df.alldata %>%
                  mutate(year = year(as.Date(df.alldata$datapull.date)),
                         mnth = month(as.Date(df.alldata$datapull.date), label = TRUE, abbr = TRUE),
                         TX_Curr = (as.numeric(df.alldata$LPVr_ATVr)+ 
                                as.numeric(df.alldata$DTG)+
                                as.numeric(df.alldata$NNRTI)+
                                as.numeric(df.alldata$Other)+
                                as.numeric(df.alldata$No_regimen_documented))
                        ) %>%
                  arrange(Wt.Cat, year, mnth)

  df.alldata2$datapull.date <- factor(df.alldata2$datapull.date)
}

regOPTdfs <- function(){
  df.LPVr_ATVr <<- regOPT_GO("LPVr_ATVr") %>%
                  gather(Category, N, LPVr_ATVr, DTG, NNRTI, Other, No_regimen_documented, factor_key = TRUE)

  df.DTG <<- regOPT_GO("DTG") %>%
                  gather(Category, N, DTG, LPVr_ATVr, NNRTI, Other, No_regimen_documented, factor_key = TRUE)

  df.NNRTI <<- regOPT_GO("NNRTI") %>%
                  gather(Category, N, NNRTI, LPVr_ATVr, DTG, Other, No_regimen_documented, factor_key = TRUE)

  df.Other <<- regOPT_GO("Other") %>%
                  gather(Category, N, Other, LPVr_ATVr, DTG, NNRTI, No_regimen_documented, factor_key = TRUE)

  df.No_reg <<- regOPT_GO("No_regimen_documented") %>%
                  gather(Category, N,  No_regimen_documented, LPVr_ATVr, DTG, NNRTI, Other, factor_key = TRUE)

  reg_dflist <<- list(df.LPVr_ATVr, df.DTG, df.NNRTI, df.Other, df.No_reg)
}


TS.ten19prepGO <- function(partnertype){
  # import files and separate into individual data frames
  ifelse(partnertype == "All",
  # All Partners
  filelist.2 <<- list.files(path = "./TLD analysis/Time series/All/ten19/", pattern = "*.csv", full.names = TRUE),
  # CDC Partners
  filelist.2 <<- list.files(path = "./TLD analysis/Time series/CDC/ten19/", pattern = "*.csv", full.names = TRUE))

  dflist.2 <<- lapply(filelist.2, read.csv, na.strings = "NULL")
  df2.alldata <<- do.call(rbind, lapply(filelist.2, function(path) {
    df <- read.csv(path, na.strings = "NULL")
    df[["source"]] <- rep(path, nrow(df))
    df}))
  dflist_tmp <- list ()

  # df2.alldata$datapull.date <- sapply(df2.alldata$datapull.date, function(x) gsub("6/1/2020", "2020-06-01", x))
  # df2.alldata$datapull.date <- sapply(df2.alldata$datapull.date, function(x) gsub("7/1/2020", "2020-07-01", x))
  
  # Replacing above section in case future datapull.date imports have different formats
  mdy <- mdy(df2.alldata$datapull.date)
  ymd <- ymd(df2.alldata$datapull.date)
  mdy[is.na(mdy)] <- ymd[is.na(mdy)]
  df2.alldata$datapull.date <- mdy

  # Split datapull.date and create TX_Curr variable
  df2.alldata2 <<- df2.alldata %>%
                  mutate(year = year(as.Date(df2.alldata$datapull.date)),
                         mnth = month(as.Date(df2.alldata$datapull.date), label = TRUE, abbr = TRUE),
                         TX_Curr = (as.numeric(df2.alldata$No_reg_doc)+ 
                                as.numeric(df2.alldata$tld_elig)+
                                as.numeric(df2.alldata$on_tld))
                        ) %>%
                  arrange(year, mnth)

  df2.alldata2$datapull.date <- factor(df2.alldata2$datapull.date)

  df.ten19 <<- regOPT_GO.2(df2.alldata2)
}

TS.15plusprepGO <- function(partnertype){
  # import files and separate into individual data frames
  ifelse(partnertype == "All",
  # All Partners
  filelist.3 <<- list.files(path = "./TLD analysis/Time series/All/ovr15/", pattern = "*.csv", full.names = TRUE),
  # CDC Partners
  filelist.3 <<- list.files(path = "./TLD analysis/Time series/CDC/ovr15/", pattern = "*.csv", full.names = TRUE))

  dflist.3 <<- lapply(filelist.3, read.csv, na.strings = "NULL")
  df3.alldata <<- do.call(rbind, lapply(filelist.3, function(path) {
    df <- read.csv(path, na.strings = "NULL")
    df[["source"]] <- rep(path, nrow(df))
    df}))
  dflist_tmp <- list ()

  # df3.alldata$datapull.date <- sapply(df3.alldata$datapull.date, function(x) gsub("6/1/2020", "2020-06-01", x))
  # df3.alldata$datapull.date <- sapply(df3.alldata$datapull.date, function(x) gsub("7/1/2020", "2020-07-01", x))

  # Replacing above section in case future datapull.date imports have different formats
  mdy <- mdy(df3.alldata$datapull.date)
  ymd <- ymd(df3.alldata$datapull.date)
  mdy[is.na(mdy)] <- ymd[is.na(mdy)]
  df3.alldata$datapull.date <- mdy

  # Split datapull.date and create TX_Curr variable
  df3.alldata2 <<- df3.alldata %>%
                  mutate(year = year(as.Date(df3.alldata$datapull.date)),
                         mnth = month(as.Date(df3.alldata$datapull.date), label = TRUE, abbr = TRUE),
                         TX_Curr = (as.numeric(df3.alldata$No_reg_doc)+ 
                                as.numeric(df3.alldata$tld_elig)+
                                as.numeric(df3.alldata$on_tld))
                        ) %>%
                  arrange(year, mnth)

  df3.alldata2$datapull.date <- factor(df3.alldata2$datapull.date)
  df.ovr15 <<- regOPT_GO.2(df3.alldata2)
}

# Enter regimen from above, make sure regimen is in quotes
regOPT_GO <- function(regimen){
  colnames <- c("State", "LPVr_ATVr", "DTG", "NNRTI", "Other", "No_regimen_documented","Optimization",
                "MoM", "MoMpos","MoMneg", "TX_Curr", "datapull.date", "year", "mnth", "Wt.Cat", "Optreg")
  df1 <- df.alldata2
  colnum <- which(colnames(df1) == regimen)
  df1$Optimization <- round(100*as.numeric(df1[, colnum])/(df1$TX_Curr))
  df1$Optimization[df1$Optimization == "NaN"] <- 0 
  df2 <- df1 %>% 
          mutate(MoM = as.numeric((Optimization - lag(Optimization, 37))),
                MoMpos = as.numeric(ifelse(is.na(MoM) | (MoM < 0) | (MoM == 0), NA, MoM)),
                MoMneg = as.numeric(ifelse(is.na(MoM) | (MoM > 0) | (MoM == 0), NA, MoM)),
                )

  df2[24:26] <- lapply(df2[24:26], function(x) sprintf("%+3d%%", x))
  df2[24:26] <- lapply(df2[24:26], function(x) gsub("NA%", "", x))
  # Line 68 creates errors in month of June 2020 MoM for 20-30 and > 30 weight classes
  df2$MoM[df2$datapull.date == "2020-06-01"]  <- "" 
  df2$MoMpos[df2$datapull.date == "2020-06-01"]  <- ""
  df2$MoMneg[df2$datapull.date == "2020-06-01"]  <- ""
  # Add col identifying regimen 
  df2$Optreg <- regimen
  # Strip data frame cols needed for plots
  df3 <- df2[,colnames]
  return(df3)
}

regOPT_GO.2 <- function(df_name){
  df1 <- df_name
  colnames <- c("State", "on_tld", "tld_elig", "No_reg_doc","Optimization",
                "MoM", "MoMpos","MoMneg", "TX_Curr", "datapull.date", "year", 
                "mnth", "age.cat")
  colnum <- which(colnames(df1) == "on_tld")
  df1$Optimization <- round(100*as.numeric(df1[, colnum])/(df1$TX_Curr))
  df1$Optimization[df1$Optimization == "NaN"] <- 0
  df2 <- df1 %>% 
          mutate(MoM = as.numeric((Optimization - lag(Optimization, 37))),
                MoMpos = as.numeric(ifelse(is.na(MoM) | (MoM < 0) | (MoM == 0), NA, MoM)),
                MoMneg = as.numeric(ifelse(is.na(MoM) | (MoM > 0) | (MoM == 0), NA, MoM)),
                )
  MoMcol <- which(colnames(df2) == "MoM")
  MoMnegcol <- which(colnames(df2) == "MoMneg")
  df2[MoMcol:MoMnegcol] <- lapply(df2[MoMcol:MoMnegcol], function(x) sprintf("%+3d%%", x))
  df2[MoMcol:MoMnegcol] <- lapply(df2[MoMcol:MoMnegcol], function(x) gsub("NA%", "", x))
  # Leaving this in for DQ check 
  df2$MoM[df2$datapull.date == "2020-06-01"]  <- "" 
  df2$MoMpos[df2$datapull.date == "2020-06-01"]  <- ""
  df2$MoMneg[df2$datapull.date == "2020-06-01"]  <- ""
  # Strip data frame cols needed for plots
  df3 <- df2[,colnames] %>%
         rename(c("TLD" = "on_tld",  "Other" = "tld_elig", "No_regimen_documented" = "No_reg_doc")) %>%
         gather(Category, N, TLD, No_regimen_documented, Other, factor_key = TRUE)
  return(df3)
}



###########################################
#### Time Series Plotting functions #######
###########################################

t_col <- function(color, percent) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100)
  
  ## Save the color
  invisible(t.col)
}

plotGO <- function(stategrp, df_regimen, wt.cohort){
  #Colors used in plot with added transparancy
  red1 <- t_col("#d11141", 65)
  col2 <- t_col("#DFC27D", 85)
  col3 <- t_col("#A6611A", 85)
  col4 <- t_col("#E66101", 85)
  col5 <- t_col("#999999", 65) #grey
  regimen <- unique(df_regimen$Optreg)
  df.plot <- gather(df_regimen, key, value, Optimization, TX_Curr)
  df <- subset(df.plot, State == stategrp & Wt.Cat == wt.cohort)
  colsopt <- ("Optimization" = "black") 
  colsfill <- c("#006633", #Dark Green
                col2, col3, col4, col5, red1)
  max.TX <- max(df$value[df$key == "TX_Curr"])
  min.TX <- min(df$value[df$key == "TX_Curr"])

  # By removing 0's to make the plot less cluttered a conditional is required here
  # in order to avoid an error generated by an empy dataframe in some of the geom calls 
  ifelse(sum(df$value[df$key == "Optimization" & df$value > 0]) > 0,
        # Optimization Line plot when dataframe != 0
        p1 <- ggplot(df[df$key == "Optimization",], 
                     aes(x=mnth, y = value/100, group = 1))+
          geom_line(data = df[df$key == "Optimization",], 
                    aes(y = value/100, group = 1, colour = "Optimization %"), 
                    stat = 'identity', size = 1)+
          geom_point(data = df[df$key == "Optimization",], 
                     aes(y = value/100, group = 1, colour = "Optimization %"),
                     stat = 'identity', size = 3)+
          annotate(geom = "rect", xmin=-Inf, xmax=Inf, ymin=1, ymax=1.25, 
                    fill = "grey", alpha = 0.2)+
          annotate(geom = "rect", xmin=-Inf, xmax=Inf, ymin=-0.25, ymax=0, 
                    fill = "grey", alpha = 0.2)+
          geom_text(data = df[df$key == "Optimization" & df$value > 0,],
                   aes(label = paste(as.character(value), "%", sep = "")),
                   vjust= -1.4, hjust= 0.45, colour = "black", size = 4)+
          geom_text(data = df[df$key == "Optimization" & df$value > 0,],
                  aes(label = paste(as.character(value), "%", sep = "")),
                  vjust= -1.4, hjust= 0.45, colour = "black", size = 4)+
          geom_text(data = df[df$key == "Optimization",], 
                    aes(label = MoMneg),
                    vjust= 1.8, hjust= 0.45, colour = "red", size = 4)+
          facet_grid(key~year, scale="free", switch="y")+
          scale_colour_manual(name = paste(regimen, " Optimization"), values = colsopt)+
          scale_y_continuous(position="right", 
                             breaks = c(0,0.2,0.4,0.6,0.8,1),
                             limits = c(-0.25, 1.25),
                             expand= c(0,0))+
          ylab("Regimen Optimization (%)")+
          theme_bw()+
          ggtitle(paste("Age cohort: 0-9   ", "Weight Category: ", wt.cohort))+
          theme(plot.title = element_text(size = 14, face = "bold.italic", hjust = 0.5),
                strip.text.x = element_text(face = "bold"),
                strip.text.y = element_text(face = "bold"),
                axis.title.y = element_blank(),
                axis.title.x = element_blank(),
                axis.ticks.x = element_blank(),
                legend.title = element_text(face = "bold", size = 12),
                legend.text = element_text(size = 12),
                panel.background=element_blank(),
                panel.spacing.x = unit(0, "lines"),
                axis.text.x = element_blank(),
                axis.text.y = element_text(face="bold",size=10)),

          # Optimization Line plot when dataframe = 0
        p1 <- ggplot(df[df$key == "Optimization",], 
                     aes(x=mnth, y = value/100, group = 1))+
          geom_line(data = df[df$key == "Optimization",], 
                    aes(y = value/100, group = 1, colour = "Optimization %"), 
                    stat = 'identity', size = 1)+
          geom_point(data = df[df$key == "Optimization",], 
                     aes(y = value/100, group = 1, colour = "Optimization %"),
                     stat = 'identity', size = 3)+
          annotate(geom = "rect", xmin=-Inf, xmax=Inf, ymin=1, ymax=1.25, 
                    fill = "grey", alpha = 0.2)+
          annotate(geom = "rect", xmin=-Inf, xmax=Inf, ymin=-0.25, ymax=0, 
                    fill = "grey", alpha = 0.2)+
          # geom_text(data = df[df$key == "Optimization" & df$value > 0,],
          #          aes(label = paste(as.character(value), "%", sep = "")),
          #          vjust= -1.4, hjust= 0.45, colour = "black", size = 4)+
          # geom_text(data = df[df$key == "Optimization" & df$value > 0,],
          #         aes(label = paste(as.character(value), "%", sep = "")),
          #         vjust= -1.4, hjust= 0.45, colour = "black", size = 4)+
          geom_text(data = df[df$key == "Optimization",], 
                    aes(label = MoMneg),
                    vjust= 1.8, hjust= 0.45, colour = "red", size = 4)+
          facet_grid(key~year, scale="free", switch="y")+
          scale_colour_manual(name = paste(regimen, " Optimization"), values = colsopt)+
          scale_y_continuous(position="right", 
                             breaks = c(0,0.2,0.4,0.6,0.8,1),
                             limits = c(-0.25, 1.25),
                             expand= c(0,0))+
          ylab("Regimen Optimization (%)")+
          theme_bw()+
          ggtitle(paste("Age cohort: 0-9   ", "Weight Category: ", wt.cohort))+
          theme(plot.title = element_text(size = 14, face = "bold.italic", hjust = 0.5),
                strip.text.x = element_text(face = "bold"),
                strip.text.y = element_text(face = "bold"),
                axis.title.y = element_blank(),
                axis.title.x = element_blank(),
                axis.ticks.x = element_blank(),
                legend.title = element_text(face = "bold", size = 12),
                legend.text = element_text(size = 12),
                panel.background=element_blank(),
                panel.spacing.x = unit(0, "lines"),
                axis.text.x = element_blank(),
                axis.text.y = element_text(face="bold",size=10))
      )
  

  
  ifelse(sum(df$value[df$key == "TX_Curr" & df$Category == regimen & df$N > 0]) > 0,
        # TX_Curr Bar plot when df$N != 0
        p2 <- ggplot(df[df$key == "TX_Curr",], 
                     aes(x=mnth, y= value, group = 1))+
          geom_bar(data = df[df$key == "TX_Curr",],
                   aes(x = mnth, y = N, fill= Category),
                   position = "stack", stat="identity", colour = "grey")+
          # TX_Curr for all regimens
          geom_point(data = df[df$key == "TX_Curr",], 
                     stat = 'identity', size = 3, colour = "black")+
          geom_text(data = df[df$key == "TX_Curr",], 
                    aes(label = value),vjust= -1 , colour = "black", size = 4)+
          # TX_Curr for Indiv regimen
          annotate(geom = "rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=0, 
                   fill = "grey", alpha = 0.2)+
          geom_point(data = df[df$key == "TX_Curr" & df$Category == regimen & df$N > 0,], 
                     aes(y = 0), 
                     shape = "triangle down filled", colour = "black", fill = "white", 
                     stat = 'identity', size = 2, stroke = 1.5, position = position_nudge(y = -0.15))+
          geom_text(data = df[df$key == "TX_Curr" & df$Category == regimen & df$N > 0,], 
                    aes(y = 0, label = paste("(",N,")",sep="")),
                    vjust= 2, hjust= 0.45, colour = "black", fontface = "bold", size = 4)+
          facet_grid(key~year, scale="free", switch="y")+
          scale_fill_manual(name= "Total TX_Curr\n\u25CF
                            \n\u26DB\n(Regimen TX_Curr)", 
                            values = colsfill,
                            # reverse legend order
                            guide = guide_legend(reverse = TRUE))+
          scale_y_continuous(position="right")+
          expand_limits(y = c(-(max.TX/6), 1.1*max.TX))+
          xlab("Analysis Month")+
          theme_bw()+
          theme(plot.title = element_text(size = 14, face = "bold.italic", hjust = 0.5),
                strip.text.x = element_blank(),
                strip.text.y = element_text(face = "bold"),
                axis.title.y = element_blank(),
                axis.title.x = element_text(size = 15),
                axis.ticks.y = element_blank(),
                legend.title = element_text(face = "bold",size = 12),
                legend.title.align = 0.5,
                legend.text = element_text(size = 12),
                panel.background = element_blank(),
                panel.spacing.x = unit(0, "lines"),
                axis.text.x = element_text(face="bold", size=10, hjust = 0.8),
                axis.text.y = element_text(face="bold",size=10, color = "white")),

        # TX_Curr Bar plot when df$N == 0
        p2 <- ggplot(df[df$key == "TX_Curr",], 
                     aes(x=mnth, y= value, group = 1))+
          geom_bar(data = df[df$key == "TX_Curr",],
                   aes(x = mnth, y = N, fill= Category),
                   position = "stack", stat="identity", colour = "grey")+
          # TX_Curr for all regimens
          geom_point(data = df[df$key == "TX_Curr",], 
                     stat = 'identity', size = 3, colour = "black")+
          geom_text(data = df[df$key == "TX_Curr",], 
                    aes(label = value),vjust= -1 , colour = "black", size = 4)+
          # TX_Curr for Indiv regimen
          annotate(geom = "rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=0, 
                   fill = "grey", alpha = 0.2)+
          # geom_point(data = df[df$key == "TX_Curr" & df$Category == regimen & df$N > 0,], 
          #            aes(y = 0), 
          #            shape = "triangle down filled", colour = "black", fill = "white", 
          #            stat = 'identity', size = 2, stroke = 1.5, position = position_nudge(y = -0.15))+
          # geom_text(data = df[df$key == "TX_Curr" & df$Category == regimen & df$N > 0,], 
          #           aes(y = 0, label = paste("(",N,")",sep="")),
          #           vjust= 2, hjust= 0.45, colour = "black", fontface = "bold", size = 4)+
          facet_grid(key~year, scale="free", switch="y")+
          scale_fill_manual(name= "Total TX_Curr\n\u25CF
                            \n\u26DB\n(Regimen TX_Curr)", 
                            values = colsfill,
                            # reverse legend order
                            guide = guide_legend(reverse = TRUE))+
          scale_y_continuous(position="right")+
          expand_limits(y = c(-(max.TX/6), 1.1*max.TX))+
          xlab("Analysis Month")+
          theme_bw()+
          theme(plot.title = element_text(size = 14, face = "bold.italic", hjust = 0.5),
                strip.text.x = element_blank(),
                strip.text.y = element_text(face = "bold"),
                axis.title.y = element_blank(),
                axis.title.x = element_text(size = 15),
                axis.ticks.y = element_blank(),
                legend.title = element_text(face = "bold",size = 12),
                legend.title.align = 0.5,
                legend.text = element_text(size = 12),
                panel.background = element_blank(),
                panel.spacing.x = unit(0, "lines"),
                axis.text.x = element_text(face="bold", size=10, hjust = 0.8),
                axis.text.y = element_text(face="bold",size=10, color = "white"))
        )
  
  gP1 <- ggplotGrob(p1)
  gP2 <- ggplotGrob(p2)
  grid.newpage()
  grid.draw(rbind(gP1,gP2))
  # grid.arrange(p1,p2) 
}

## old  version ###
# plotGO <- function(stategrp, df_regimen, wt.cohort){
#   red1 <- t_col("#d11141", 65)
#   col2 <- t_col("#DFC27D", 85)
#   col3 <- t_col("#A6611A", 85)
#   col4 <- t_col("#E66101", 85)
#   col5 <- t_col("#999999", 65) #grey
#   regimen <- unique(df_regimen$Optreg)
#   df.plot <- gather(df_regimen, key, value, Optimization, TX_Curr)
#   df <- subset(df.plot, State == stategrp & Wt.Cat == wt.cohort)
#   colsopt <- ("Optimization" = "black") 
#   colsfill <- c("#006633", #Dark Green
#                 col2, col3, col4, col5, red1)
#   maxopt <- max(df$value[df$key == "Optimization"])
#   minopt <- min(df$value[df$key == "Optimization"])
  
#   p1 <- ggplot(df[df$key == "Optimization",], 
#                aes(x=mnth, y = value, group = 1))+
#     geom_line(data = df[df$key == "Optimization",], 
#               aes(y = value, group = 1, colour = "Optimization %"), 
#               stat = 'identity', size = 1)+
#     geom_point(data = df[df$key == "Optimization",], 
#                aes(y = value, group = 1, colour = "Optimization %"),
#                stat = 'identity', size = 3)+
#     geom_text(data = df[df$key == "Optimization",], 
#               aes(label = MoMpos),vjust= -1.5, hjust= 0.45, colour = "#339900", size = 4)+
#     geom_text(data = df[df$key == "Optimization",], 
#               aes(label = MoMneg),vjust= -1.5, hjust= 0.45, colour = "red", size = 4)+
#     facet_grid(key~year, scale="free", switch="y")+
#     scale_colour_manual(name = paste(regimen, " Optimization"), values = colsopt)+
#     scale_y_continuous(position="right", 
#                        breaks = pretty_breaks(),
#                        limits = c(minopt, 1.2*maxopt))+
#     ylab("Regimen Optimization (%)")+
#     theme_bw()+
#     ggtitle(paste("Age cohort: 0-9   ", "Weight Category: ", wt.cohort))+
#     theme(plot.title = element_text(size = 14, face = "bold.italic", hjust = 0.5),
#           strip.text.x = element_text(face = "bold"),
#           strip.text.y = element_text(face = "bold"),
#           axis.title.y = element_blank(),
#           axis.title.x = element_blank(),
#           axis.ticks.x = element_blank(),
#           legend.title = element_text(face = "bold", size = 12),
#           legend.text = element_text(size = 12),
#           panel.background=element_blank(),
#           panel.spacing.x = unit(0, "lines"),
#           axis.text.x = element_blank(),
#           axis.text.y = element_text(face="bold",size=10))
  
#   p2 <- ggplot(df[df$key == "TX_Curr",], 
#                aes(x=mnth, y= value, group = 1))+
#     geom_bar(data = df[df$key == "TX_Curr",],
#              aes(x = mnth, y = N, fill= Category),
#              position = "stack", stat="identity", colour = "grey")+
#     geom_point(data = df[df$key == "TX_Curr",], 
#                stat = 'identity', size = 3, colour = "black")+
#     geom_text(data = df[df$key == "TX_Curr",], 
#               aes(label = value),vjust= 1.5 , colour = "black", size = 4)+
#     facet_grid(key~year, scale="free", switch="y")+
#     scale_fill_manual(name= "TX_Curr", 
#                       values = colsfill,
#                       # reverse legend order
#                       guide = guide_legend(reverse = TRUE))+
#     scale_y_continuous(position="right")+
#     xlab("Analysis Month")+
#     theme_bw()+
#     theme(plot.title = element_text(size = 14, face = "bold.italic", hjust = 0.5),
#           strip.text.x = element_blank(),
#           strip.text.y = element_text(face = "bold"),
#           axis.title.y = element_blank(),
#           axis.title.x = element_text(size = 15),
#           axis.ticks.y = element_blank(),
#           legend.title = element_text(face = "bold",size = 12),
#           legend.text = element_text(size = 12),
#           panel.background = element_blank(),
#           panel.spacing.x = unit(0, "lines"),
#           axis.text.x = element_text(face="bold", size=10, hjust = 0.8),
#           axis.text.y = element_text(face="bold",size=10, color = "white"))

#   gP1 <- ggplotGrob(p1)
#   gP2 <- ggplotGrob(p2)
#   grid.newpage()
#   grid.draw(rbind(gP1,gP2))
#   # grid.arrange(p1,p2) 
# }

### Adolescent and Adult cohorts 
plotGO.2 <- function(df_name, stategrp, age.cohort){
  red1 <- t_col("#d11141", 65)
  col2 <- t_col("#DFC27D", 85)
  col3 <- t_col("#A6611A", 85)
  col4 <- t_col("#E66101", 85)
  col5 <- t_col("#999999", 65) #grey
  df.plot <- gather(df_name, key, value, Optimization, TX_Curr)
  df <- subset(df.plot, State == stategrp)
  colsopt <- ("Optimization" = "black") 
  colsfill.2 <- c("#006633", #Dark Green
                col2, col5)
  max.TX <- max(df$value[df$key == "TX_Curr"])
  min.TX <- min(df$value[df$key == "TX_Curr"])
  
  p1 <- ggplot(df[df$key == "Optimization",], 
               aes(x=mnth, y = value/100, group = 1))+
    geom_line(data = df[df$key == "Optimization",], 
              aes(group = 1, colour = "Optimization %"), 
              stat = 'identity', size = 1)+
    geom_point(data = df[df$key == "Optimization",], 
               aes(group = 1, colour = "Optimization %"),
               stat = 'identity', size = 3)+
    annotate(geom = "rect", xmin=-Inf, xmax=Inf, ymin=1, ymax=1.25, 
              fill = "grey", alpha = 0.2)+
    annotate(geom = "rect", xmin=-Inf, xmax=Inf, ymin=-0.25, ymax=0, 
              fill = "grey", alpha = 0.2)+
    geom_text(data = df[df$key == "Optimization" & df$value > 0,], 
              aes(label = paste(as.character(value), "%", sep = "")),
              vjust= -1.4, hjust= 0.45, colour = "black", size = 4)+
    geom_text(data = df[df$key == "Optimization",], 
              aes(label = MoMpos),
              vjust= 1.8, hjust= 0.45, colour = "#339900", size = 4)+
    geom_text(data = df[df$key == "Optimization",], 
              aes(label = MoMneg),
              vjust= 1.8, hjust= 0.45, colour = "red", size = 4)+
    facet_grid(key~year, scale="free", switch="y")+
    scale_colour_manual(name = "TLD Optimization", values = colsopt)+
    scale_y_continuous(position="right", 
                       breaks = c(0,0.2,0.4,0.6,0.8,1),
                       limits = c(-0.25, 1.25),
                       expand= c(0,0))+
    theme_bw()+
    ggtitle(paste("Age cohort: ", age.cohort))+
    theme(plot.title = element_text(size = 14, face = "bold.italic", hjust = 0.5),
          strip.text.x = element_text(face = "bold"),
          strip.text.y = element_text(face = "bold"),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.title = element_text(face = "bold", size = 12),
          legend.text = element_text(size = 12),
          panel.background=element_blank(),
          panel.spacing.x = unit(0, "lines"),
          axis.text.x = element_blank(),
          axis.text.y = element_text(face="bold",size=10))
  
  p2 <- ggplot(df[df$key == "TX_Curr",], 
               aes(x=mnth, y= value, group = 1))+
    geom_bar(data = df[df$key == "TX_Curr",],
             aes(x = mnth, y = N, fill= Category),
             position = "stack", stat="identity", colour = "grey")+
    # TX_Curr for all regimens
    geom_point(data = df[df$key == "TX_Curr",], 
               stat = 'identity', size = 3, colour = "black")+
    geom_text(data = df[df$key == "TX_Curr",], 
              aes(label = value),vjust= -1 , colour = "black", size = 4)+
    # TX_Curr for Indiv regimen
    annotate(geom = "rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=0, 
             fill = "grey", alpha = 0.2)+
    geom_point(data = df[df$key == "TX_Curr" & df$Category == "TLD" & df$N > 0,], 
               aes(y = 0), 
               shape = "triangle down filled", colour = "black", fill = "white", 
               stat = 'identity', size = 2, stroke = 1.5, position = position_nudge(y = -0.15))+
    geom_text(data = df[df$key == "TX_Curr" & df$Category == "TLD" & df$N > 0,], 
              aes(y = 0, label = paste("(",N,")",sep="")),
              vjust= 2, hjust= 0.45, colour = "black", fontface = "bold", size = 4)+
    facet_grid(key~year, scale="free", switch="y")+
    scale_fill_manual(name= "Total TX_Curr\n\u25CF
                      \n\u26DB\n(TLD TX_Curr)", 
                      values = colsfill.2,
                      # reverse legend order
                      guide = guide_legend(reverse = TRUE))+
    scale_y_continuous(position="right")+
    expand_limits(y = c(-(max.TX/6), 1.15*max.TX))+
    xlab("Analysis Month")+
    theme_bw()+
    theme(plot.title = element_text(size = 14, face = "bold.italic", hjust = 0.5),
          strip.text.x = element_blank(),
          strip.text.y = element_text(face = "bold"),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 15),
          axis.ticks.y = element_blank(),
          legend.title = element_text(face = "bold",size = 12),
          legend.title.align = 0.5,
          legend.text = element_text(size = 12),
          panel.background = element_blank(),
          panel.spacing.x = unit(0, "lines"),
          axis.text.x = element_text(face="bold", size=10, hjust = 0.8),
          axis.text.y = element_text(face="bold",size=10, color = "white"))

  gP1 <- ggplotGrob(p1)
  gP2 <- ggplotGrob(p2)
  grid.newpage()
  grid.draw(rbind(gP1,gP2))
  # grid.arrange(p1,p2)   
}


# plotGO.2 <- function(df_name, stategrp, age.cohort){
#   red1 <- t_col("#d11141", 65)
#   col2 <- t_col("#DFC27D", 85)
#   col3 <- t_col("#A6611A", 85)
#   col4 <- t_col("#E66101", 85)
#   col5 <- t_col("#999999", 65) #grey
#   df.plot <- gather(df_name, key, value, Optimization, TX_Curr)
#   df <- subset(df.plot, State == stategrp)
#   colsopt <- ("Optimization" = "black") 
#   colsfill.2 <- c("#006633", #Dark Green
#                 col2, col5)
#   maxopt <- max(df$value[df$key == "Optimization"])
#   minopt <- min(df$value[df$key == "Optimization"])
  
#   p1 <- ggplot(df[df$key == "Optimization",], 
#                aes(x=mnth, y = value, group = 1))+
#     geom_line(data = df[df$key == "Optimization",], 
#               aes(y = value, group = 1, colour = "Optimization %"), 
#               stat = 'identity', size = 1)+
#     geom_point(data = df[df$key == "Optimization",], 
#                aes(y = value, group = 1, colour = "Optimization %"),
#                stat = 'identity', size = 3)+
#     geom_text(data = df[df$key == "Optimization",], 
#               aes(label = MoMpos),vjust= -1.5, hjust= 0.45, colour = "#339900", size = 4)+
#     geom_text(data = df[df$key == "Optimization",], 
#               aes(label = MoMneg),vjust= -1.5, hjust= 0.45, colour = "red", size = 4)+
#     facet_grid(key~year, scale="free", switch="y")+
#     scale_colour_manual(name = "TLD Optimization", values = colsopt)+
#     scale_y_continuous(position="right", 
#                        breaks = pretty_breaks(),
#                        limits = c(minopt, 1.2*maxopt))+
#     theme_bw()+
#     ggtitle(paste("Age cohort: ", age.cohort))+
#     theme(plot.title = element_text(size = 14, face = "bold.italic", hjust = 0.5),
#           strip.text.x = element_text(face = "bold"),
#           strip.text.y = element_text(face = "bold"),
#           axis.title.y = element_blank(),
#           axis.title.x = element_blank(),
#           axis.ticks.x = element_blank(),
#           legend.title = element_text(face = "bold", size = 12),
#           legend.text = element_text(size = 12),
#           panel.background=element_blank(),
#           panel.spacing.x = unit(0, "lines"),
#           axis.text.x = element_blank(),
#           axis.text.y = element_text(face="bold",size=10))
  
#   p2 <- ggplot(df[df$key == "TX_Curr",], 
#                aes(x=mnth, y= value, group = 1))+
#     geom_bar(data = df[df$key == "TX_Curr",],
#              aes(x = mnth, y = N, fill= Category),
#              position = "stack", stat="identity", colour = "grey")+
#     geom_point(data = df[df$key == "TX_Curr",], 
#                stat = 'identity', size = 3, colour = "black")+
#     geom_text(data = df[df$key == "TX_Curr",], 
#               aes(label = value),vjust= 1.5 , colour = "black", size = 4)+
#     facet_grid(key~year, scale="free", switch="y")+
#     scale_fill_manual(name= "TX_Curr", 
#                       values = colsfill.2,
#                       # reverse legend order
#                       guide = guide_legend(reverse = TRUE))+
#     scale_y_continuous(position="right")+
#     xlab("Analysis Month")+
#     theme_bw()+
#     theme(plot.title = element_text(size = 14, face = "bold.italic", hjust = 0.5),
#           strip.text.x = element_blank(),
#           strip.text.y = element_text(face = "bold"),
#           axis.title.y = element_blank(),
#           axis.title.x = element_text(size = 15),
#           axis.ticks.y = element_blank(),
#           legend.title = element_text(face = "bold",size = 12),
#           legend.text = element_text(size = 12),
#           panel.background = element_blank(),
#           panel.spacing.x = unit(0, "lines"),
#           axis.text.x = element_text(face="bold", size=10, hjust = 0.8),
#           axis.text.y = element_text(face="bold",size=10, color = "white"))

#   gP1 <- ggplotGrob(p1)
#   gP2 <- ggplotGrob(p2)
#   grid.newpage()
#   grid.draw(rbind(gP1,gP2))
#   # grid.arrange(p1,p2)   
# }

####### WRITE WORKBOOK SECTION ########

writeWB_ALLGO <- function(partnertype, iteration){
  # Import Excel Tool Template
  wb <- loadWorkbook(file = "./TLD analysis/Regimen Analysis_allpartners_template.xlsx")

  #Adjust age 0-9 data frames to fit template
  Childcols <- c("LPVr_ATVr", "LPVr_ATVr_VLSperc" , "DTG", "DTG_VLSperc", "NNRTI", "NNRTI_VLSperc", "Other", "Other_VLSperc",                
                  "No_regimen_documented" , "No_regimen_documented_VLSperc")
  df.statefull <- as.data.frame(statefull)
  df.statefull <- df.statefull %>% t() %>% sort()

  df.LT20_refit <- df_agg_LT20[,Childcols]
  df.20_30_refit <- df_agg_20_30[,Childcols]
  df.GT30_refit <- df_agg_GT30[,Childcols]
  # write state column 
  writeData(wb, 
            "Summary_All partners", 
            as.data.frame(df.statefull),
            startCol = "G",
            startRow = 5,
            colNames = FALSE,
            rowNames = FALSE)
  # write LT20 regimen data
  writeData(wb, 
           "Summary_All partners", 
           df.LT20_refit,
           startCol = "H",
           startRow = 5,
           colNames = FALSE,
           rowNames = FALSE)
  # write regimen lists
  writeData(wb, 
            "Summary_All partners", 
            df.LT20reg,
            startCol = "H",
            startRow = 47,
            colNames = FALSE,
            rowNames = FALSE)

  # write 20_30 regimen data
  writeData(wb, 
            "Summary_All partners", 
            df.20_30_refit,
            startCol = "S",
            startRow = 5,
            colNames = FALSE,
            rowNames = FALSE)
  # write regimen lists
  writeData(wb, 
            "Summary_All partners", 
            df.20_30reg,
            startCol = "S",
            startRow = 47,
            colNames = FALSE,
            rowNames = FALSE)

  # write GT30 regimen data
  writeData(wb, 
            "Summary_All partners", 
            df.GT30_refit,
            startCol = "AD",
            startRow = 5,
            colNames = FALSE,
            rowNames = FALSE)
  # write GT30 regimen lists
  writeData(wb, 
            "Summary_All partners", 
            df.GT30reg,
            startCol = "AD",
            startRow = 47,
            colNames = FALSE,
            rowNames = FALSE)

  #Adjust age 10-19 data frame to fit template

  teenAdultcols <- c("No_reg_doc", "tld_elig", "on_tld")
  df.Ten19_refit <- df_agg_ten19[, teenAdultcols]
  # write state column 
  writeData(wb, 
            "Summary_All partners", 
            as.data.frame(df.statefull),
            startCol = "AP",
            startRow = 5,
            colNames = FALSE,
            rowNames = FALSE)
  #write regimen data 
  writeData(wb, 
           "Summary_All partners", 
           df.Ten19_refit,
           startCol = "AR",
           startRow = 5,
           colNames = FALSE,
           rowNames = FALSE)
  # write regimen lists
  writeData(wb, 
            "Summary_All partners", 
            df.ten19reg,
            startCol = "AQ",
            startRow = 47,
            colNames = FALSE,
            rowNames = FALSE)

  #Adjust age ovr15 data frame to fit template
  # write state column 
  writeData(wb, 
            "Summary_All partners", 
            as.data.frame(df.statefull),
            startCol = "AW",
            startRow = 5,
            colNames = FALSE,
            rowNames = FALSE)
  df.ovr15_refit <- df_agg_ovr15[, teenAdultcols]
  # write regimen data
  writeData(wb, 
           "Summary_All partners", 
           df.ovr15_refit,
           startCol = "AY",
           startRow = 5,
           colNames = FALSE,
           rowNames = FALSE)
  # write regimen lists
  writeData(wb, 
            "Summary_All partners", 
            df.15plusreg,
            startCol = "AX",
            startRow = 47,
            colNames = FALSE,
            rowNames = FALSE)

  # write no weight data
  df.noweight <- as.data.frame(c(noweight, noweightnoreg)) %>% t()

  writeData(wb, 
          "Summary_All partners", 
          as.data.frame(df.noweight),
          startCol = "C",
          startRow = 8,
          colNames = FALSE,
          rowNames = FALSE)

  # write linelist
  writeData(wb, 
            "All partners 0-9 linelist", 
            df.zero9_full[,-1],
            startCol = 1,
            startRow = 1,
            colNames = TRUE,
            rowNames = F)

 # title_style1 <- createStyle(halign = "center", valign = "center", wrapText = TRUE, fgFill = "#D9E1F2",
 #                             borderStyle = getOption("openxlsx.borderStyle", "double"), 
 #                             border = "TopBottom",
 #                             borderColour = "black"))
 # addStyle(wb, 
 #         "Summary_All partners", 
 #         title_style1, 
 #         rows = c(4,46), 
 #         cols = 7:40,
 #         gridExpand = T)

 # Write time series plots
 ## Excel cell default width = 0.64" (64 px), height 0.2" (20 px)
 num.months <- length(filelist.2)
 wdth <- num.months
 hght <- 5.34
 weightcats <- c("< 20 kg", "20 to 30 kg", "> 30 kg")

  for(counter in statefull){
    addWorksheet(wb, counter, tabColour = "red")
    n_rowstrt <- 1
    for(reg in reg_dflist){
      for(weight in weightcats){
        plotGO(counter, reg, weight)
        insertPlot(wb,
                   counter,
                   width = wdth,
                   height = hght,
                   startRow = n_rowstrt,
                   startCol = 1,
                   fileType = "png",
                   units = "in",       # in, cm, or px
                   dpi = 300)
        n_rowstrt = n_rowstrt + 29
      }
    }
    plotGO.2(df.ten19, counter, "10 - 19")
    insertPlot(wb,
                 counter,
                 width = wdth,
                 height = hght,
                 startRow = n_rowstrt,
                 startCol = 1,
                 fileType = "png",
                 units = "in",       # in, cm, or px
                 dpi = 300)
    plotGO.2(df.ovr15, counter, "15 +")
    insertPlot(wb,
                 counter,
                 width = wdth,
                 height = hght,
                 startRow = n_rowstrt + 29,
                 startCol = 1,
                 fileType = "png",
                 units = "in",       # in, cm, or px
                 dpi = 300)
  }
  # Save Workbook and Export
  saveWorkbook(wb, paste("./TLD Analysis/Regimen Analysis_", iteration, ".xlsx", sep = "") , overwrite = TRUE)

}

writeWB_CDCGO <- function(iteration){
  # Import Excel Tool Template
  wb <- loadWorkbook(file = "./TLD analysis/Regimen Analysis_CDCpartners_template.xlsx")

  #Adjust age 0-9 data frames to fit template

  Childcols <- c("LPVr_ATVr", "LPVr_ATVr_VLSperc" , "DTG", "DTG_VLSperc", "NNRTI", "NNRTI_VLSperc", "Other", "Other_VLSperc",                
                  "No_regimen_documented" , "No_regimen_documented_VLSperc")
  df.statefull <- as.data.frame(statefullCDC)
  df.statefull <- df.statefull %>% t() %>% sort()


  df.LT20_refit <- df_CDCagg_LT20[,Childcols]
  df.20_30_refit <- df_CDCagg_20_30[,Childcols]
  df.GT30_refit <- df_CDCagg_GT30[,Childcols]
  # write state column 
  writeData(wb, 
            "Summary_All partners", 
            as.data.frame(df.statefull),
            startCol = "G",
            startRow = 5,
            colNames = FALSE,
            rowNames = FALSE)
  # write LT20 regimen data
  writeData(wb, 
           "Summary_All partners", 
           df.LT20_refit,
           startCol = "H",
           startRow = 5,
           colNames = FALSE,
           rowNames = FALSE)
  # write regimen lists
  writeData(wb, 
            "Summary_All partners", 
            df.LT20reg,
            startCol = "H",
            startRow = 29,
            colNames = FALSE,
            rowNames = FALSE)

  # write 20_30 regimen data
  writeData(wb, 
            "Summary_All partners", 
            df.20_30_refit,
            startCol = "S",
            startRow = 5,
            colNames = FALSE,
            rowNames = FALSE)
  # write regimen lists
  writeData(wb, 
            "Summary_All partners", 
            df.20_30reg,
            startCol = "S",
            startRow = 29,
            colNames = FALSE,
            rowNames = FALSE)

  # write GT30 regimen data
  writeData(wb, 
            "Summary_All partners", 
            df.GT30_refit,
            startCol = "AD",
            startRow = 5,
            colNames = FALSE,
            rowNames = FALSE)
  # write GT30 regimen lists
  writeData(wb, 
            "Summary_All partners", 
            df.GT30reg,
            startCol = "AD",
            startRow = 29,
            colNames = FALSE,
            rowNames = FALSE)

  #Adjust age 10-19 data frame to fit template

  teenAdultcols <- c("No_reg_doc", "tld_elig", "on_tld")
  df.Ten19_refit <- df_CDCagg_ten19[, teenAdultcols]
  # write state column 
  writeData(wb, 
            "Summary_All partners", 
            as.data.frame(df.statefull),
            startCol = "AP",
            startRow = 5,
            colNames = FALSE,
            rowNames = FALSE)
  #write regimen data 
  writeData(wb, 
           "Summary_All partners", 
           df.Ten19_refit,
           startCol = "AR",
           startRow = 5,
           colNames = FALSE,
           rowNames = FALSE)
  # write regimen lists
  writeData(wb, 
            "Summary_All partners", 
            df.ten19reg,
            startCol = "AQ",
            startRow = 29,
            colNames = FALSE,
            rowNames = FALSE)

  #Adjust age ovr15 data frame to fit template
  # write state column 
  writeData(wb, 
            "Summary_All partners", 
            as.data.frame(df.statefull),
            startCol = "AW",
            startRow = 5,
            colNames = FALSE,
            rowNames = FALSE)
  df.ovr15_refit <- df_CDCagg_ovr15[, teenAdultcols]
  # write regimen data
  writeData(wb, 
           "Summary_All partners", 
           df.ovr15_refit,
           startCol = "AY",
           startRow = 5,
           colNames = FALSE,
           rowNames = FALSE)
  # write regimen lists
  writeData(wb, 
            "Summary_All partners", 
            df.15plusreg,
            startCol = "AX",
            startRow = 29,
            colNames = FALSE,
            rowNames = FALSE)

  # write no weight data
  df.noweight <- as.data.frame(c(noweight, noweightnoreg)) %>% t()

  writeData(wb, 
          "Summary_All partners", 
          as.data.frame(df.noweight),
          startCol = "C",
          startRow = 8,
          colNames = FALSE,
          rowNames = FALSE)

  # write linelist
  writeData(wb, 
            "All partners 0-9 linelist", 
            df.zero9_CDCfull2[,-1],
            startCol = 1,
            startRow = 1,
            colNames = TRUE,
            rowNames = F)

 # title_style1 <- createStyle(halign = "center", valign = "center", wrapText = TRUE, fgFill = "#D9E1F2",
 #                             borderStyle = getOption("openxlsx.borderStyle", "double"), 
 #                             border = "TopBottom",
 #                             borderColour = "black"))
 # addStyle(wb, 
 #         "Summary_All partners", 
 #         title_style1, 
 #         rows = c(4,46), 
 #         cols = 7:40,
 #         gridExpand = T)

  # Save Workbook and Export
  saveWorkbook(wb, paste("./TLD Analysis/Regimen AnalysisCDC_", iteration, ".xlsx", sep = "") , overwrite = TRUE)

}
