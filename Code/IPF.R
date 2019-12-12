# Not in use ---------------
#temp = list.files(pattern="*.dta")
#myfiles = lapply(temp, read_dta)

#LCF: Derived Total Weekly Expenditure by Item for Each Adult Spender Data, 2017-2018
#diary_2018 <- myfiles[[3]]
#rawperson_2018 <- myfiles[[4]]
#person_dv_2018 <- myfiles[[1]]
#survey_hh_2018 <- myfiles[[2]]
function(notinuse){
  #Cons_london <- fread("lsoa-data-old-boundaries-DataSheet.csv")
  #So for Individual information -> We have the following variables 
  #Individual ID 
  #Sex 
  #Age category 
  #Age and Sex category 
  #Ethnicity 
  #Annual weight 
  #Geographical Regions 
  #Employment Status (Binary)
  #Student Status(Binary)
  #Economic Position
  #Annual gross wage
  #puchasing amount £ for Smoking products 
  
  #Import relevant constraints data -> Merge all of them into one big table 
  
  #Baseline population  -> Ok 
  #Age Category  -> Ok
  #Data_AGE_UNIT <- read_csv("Data_AGE_UNIT.csv",  skip = 1)
  
  #Sex
  #Data_SEX_UNIT <- read_csv("Data_SEX_UNIT.csv",skip = 1)
  
  #Ethnicity  -> ok
  #Data_ETHGRP_UNIT <- read_csv("Data_ETHGRP_UNIT.csv",skip=1)
  
  #Employment Status (Binary) -> Ok
  
  #Student Status(Binary) 
  
  #Economic Position -> ok 
  
  #Annual gross wage ->  OK 
  
  #puchasing amount £ for Smoking products
}


function(notuseful){
  format_rrcpl <- melt(rrcpl, id.vars=c("exp_reg"))
  format_rrcpl <- plyr::rename(format_rrcpl, c("variable" = "cat", "value" = "val_rrcpl"))
  diary <- merge(diary, format_rrcpl, by = c("exp_reg", "cat"))
  #Scaled to UK average 
  diary$exp_uk_std <- diary$pdamount/(diary$val_rrcpl/100.0)
  diary_out <- diary
  diary <- dcast(diary, id_hh_pers + Perstyp2~COI_PLUS, value.var = "exp_uk_std") ##cast diary - col per item, row per person
  diary[is.na(diary)] <- 0 # Set expenditure to 0 if NA
  for (i in reg_list) {
    reg_msim <- msim[which(msim[reg_col_name] == i),]
    print(paste("Zone ID = ", i))
    print(head(diary))
    print(head(reg_msim))
    reg_msim <- merge(reg_msim, diary[c(food_drink_tob_all_list, "id_hh_pers")], by= "id_hh_pers", all.x = TRUE) 
    print(dim(reg_msim))
    exp_reg_msim <- as.character(unique(reg_tab[which(reg_tab[reg_col_name] == i), "Region.name"]))
    print(paste("expenditure region = ", exp_reg_msim))
    reg_msim[food_drink_tob_all_list][is.na(reg_msim[food_drink_tob_all_list])] <- 0
    reg_msim[food_drink_tob_all_list][which(reg_msim$age_ori < 7)] <- 0
    reg_msim[food_drink_tob_all_list] <- reg_msim[food_drink_tob_all_list] * reg_msim$freq
    reg_msim <- merge(reg_msim, adult_child_exp_reg[,c("id_hh_pers", "plus_18", "age_ori")], by = "id_hh_pers", all.x = TRUE)
    reg_msim[which(reg_msim$age_ori < 7),food_drink_tob_all_list] <- 0 
    aggregate_sum_mnemonic <- aggregate(reg_msim[(c(food_drink_tob_all_list, "freq"))], list(reg_msim[,reg_col_name]),FUN= sum)
    rm(reg_msim)
    aggregate_sum_mnemonic[food_drink_tob_all_list] <- aggregate_sum_mnemonic[food_drink_tob_all_list] / aggregate_sum_mnemonic$freq
    food_non_alc_hh_rrcpl <- rrcpl[which(rrcpl$exp_reg == exp_reg_msim), "food_non_alc_hh"]
    alc_tob_rrcpl <- rrcpl[which(rrcpl$exp_reg == exp_reg_msim), "alc_tob_hh"]
    rest_hotel_rrcpl <- rrcpl[which(rrcpl$exp_reg == exp_reg_msim), "rest_hotel"]
    rec_cult_rrcpl <- rrcpl[which(rrcpl$exp_reg == exp_reg_msim), "rec_cult"]
    aggregate_sum_mnemonic[,which(colnames(aggregate_sum_mnemonic) %in% food_non_alc_hh_list)] <- aggregate_sum_mnemonic[,which(colnames(aggregate_sum_mnemonic) %in% food_non_alc_hh_list)] * (food_non_alc_hh_rrcpl/100.0)
    aggregate_sum_mnemonic[,which(colnames(aggregate_sum_mnemonic) %in% alc_tob_hh_list)] <- aggregate_sum_mnemonic[,which(colnames(aggregate_sum_mnemonic) %in% alc_tob_hh_list)] * (alc_tob_rrcpl/100.0)
    aggregate_sum_mnemonic[,which(colnames(aggregate_sum_mnemonic) %in% rest_hotel_list)] <- aggregate_sum_mnemonic[,which(colnames(aggregate_sum_mnemonic) %in% rest_hotel_list)] * (rest_hotel_rrcpl/100.0)
    aggregate_sum_mnemonic[,which(colnames(aggregate_sum_mnemonic) %in% rec_cult_list)] <- aggregate_sum_mnemonic[,which(colnames(aggregate_sum_mnemonic) %in% rec_cult_list)] * (rec_cult_rrcpl/100.0)
    final_mnemonic <-  rbind(aggregate_sum_mnemonic, final_mnemonic)
    rm(aggregate_sum_mnemonic)}
  return(final_mnemonic)
}

format_diary_smoking <- function(diary,list){
  
  #COI_PLUS: Coicop code 
  #pdamount: expenditure amount 
  #tobacoo COICOP code 02.2.0
  #final_mnemonic <- NULL
  #diary <- merge(diary, adult_child_exp_reg[,c("id_hh_pers", "exp_reg")]) ## FOR PRODUCT LEVEL RRCPL
  #print(head(diary))
  diary$id_hh_pers <- as.numeric(paste(diary$case, diary$Person, sep="."))
  # Convert . to _ e.g. 1.1.1.1.1 to 1_1_1_1_1 
  diary$COI_PLUS <- gsub(".", '_', diary$COI_PLUS, fixed = TRUE)
  # Convert 1_1_1_1_1  to X1_1_1_1_1 
  diary$COI_PLUS <- paste("X", diary$COI_PLUS, sep = "" )
  
  
  diary <-  diary[diary$COI_PLUS %in% list,]
  diary <- aggregate(. ~id_hh_pers + COI_PLUS ,diary, FUN = function(x) mean(as.numeric(as.character(x))))
  
  return(diary)
}


# Import Library--------
library(haven)
library(plyr)
library(mipfp) 
library(Hmisc)
library(R.utils)
library(abind)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(ipfp)
library(stringr)
library("ggplot2")
library(readr)
library(readxl)


# Step 1 Import the data from LCF-----------
#I am using four datasets from 2011 
#Using 2011 data from the LCF 
setwd("~/Desktop/Data/LCF/UKDA-7272-stata9_se/stata9_se")
#Derived total weekly expenditure by item for each adult 
diary_2011 <- read_dta("2011_dv_set89_ukanon.dta") 
#Raw household data
raw_person_2011 <- read_dta("2011_rawper_ukanon.dta")
#Derived household members data
person_dv_2011 <- read_dta("2011_dvper_ukanon_v2.dta")
#Derived household characteristics data
survey_hh_2011 <- read_dta("2011_dvhh_ukanon.dta") 

# Step 2 Reformat Individual Data------- 

#Colnames: ID, Region, Age&Sex, Ethnicity, unemployment status, student, gross wage, employment type 

#Data I will be using: raw_person_2011,person_dv_2011, survey_hh_2011


#Function 1: Format the data from raw_person_2011 

## Generate table of all those 16+ with variables of gender, ethnicity, age (grouped), age (original) and age/gender

format_raw_person <- function(raw_person){
  
  #Extract Ethnicity column names
  raw_person_cols_all <- c("Eth01p", "EthEp", "EthWp", "EthSp", "EthNIp")
  
  #Colnames description:
  
  #EthEp - Ethnic origin grouped - England
  #EthWp - Ethnic origin grouped - Wales
  #EthSp - Ethnic origin grouped - Scotland
  #EthNIp - Ethnic origin grouped - Northern Ireland
  
  #Ethnicity Coding/labels 
  
  #  Labels:
  #  value  label
  #  1      White
  #  2      Mixed race
  #  3      Asian
  #  4      Black
  #  5      Other
  
  #case - Case Number
  
  #person - Person ID 
  
  #Sex - Gender 
  
  # Labels: 
  # Value Label 
  #   1    Male 
  #   2    Female  
  
  
  
  
  #Create empty column names 
  raw_person_cols <- c()
  
  #if our dataframe has the assigned ethnicity column, then paste the ethnicity column with case, person, sex, age
  for (col in raw_person_cols_all){
    if(col %in% colnames(raw_person)){
      raw_person_cols <- c(raw_person_cols, col)}}
  #Subset dataframe based on the selected columns 
  raw_person <- raw_person[,c(raw_person_cols,"case", "Person", "Sex", "dvage_p", "INA011")]
  #Create Composite ID 
  raw_person$id_hh_pers <- as.numeric(paste(raw_person$case, raw_person$Person, sep="."))
  
  
  #Create age category
  raw_person$age_ori <- raw_person$dvage_p
  brks <- c(0,4, 14, 24, 44, 69, 89,  200)
  labs <- c("0_4","5_14","15_24","25_44","45_64", "65_89", "90+")
  raw_person$age <- cut(as.numeric(raw_person$age_ori), breaks = brks, labels = labs) # bin the age variable 
  raw_person$age <- as.character(raw_person$age)
  raw_person$age_gen <- as.character(paste(raw_person$Sex, raw_person$age, sep='_'))
  
  #Create Ethnicity category 
  raw_person$Eth_ori  <- do.call(paste, c(raw_person[raw_person_cols], sep=""))
  raw_person$Eth_ori <- gsub(" ","",raw_person$Eth_ori)
  raw_person$Eth_ori <- readr::parse_number(raw_person$Eth_ori) 
  
  #Select the column that I need 
  raw_person <- raw_person[,c("case","id_hh_pers", "Sex", "age", "age_gen","Eth_ori")]
  
  
  return(raw_person)
}

#format_raw_person(raw_person_2011)

#Function 2: Merge survey table to the previous table - add variables of weight, region, country, expenditure region and 'over 18'

prod_person <- function(format_raw_person, survey_hh){
  ##survey_hh$Gorx <- revalue(survey_hh$Gorx, c("North West and Merseyside" = "North West & Merseyside"))
  prod_person <- inner_join(format_raw_person, survey_hh[,c("case","weighta", "Gorx")], by="case")
  prod_person <- prod_person[,c("case","id_hh_pers", "Sex", "age", "age_gen","Eth_ori","weighta", "Gorx")]
  return(prod_person)
}

#prod_person(format_raw_person(raw_person_2011),survey_hh_2011)

#Function 3: Format and merge PERSON LEVEL table to the table - add variables of unemployment status (binary), student status (binary), Employment type. 

##FORMAT DERIVED PERSON LEVEL SURVEY TABLE
format_dv_person <- function(person_dv){
  ##Generate unique ID for each 'person' - concatanate case (household number) and person within household
  person_dv$id_hh_pers <- as.numeric(paste(person_dv$case, person_dv$Person, sep="."))
  #Select column 
  person_dv <- person_dv[ ,c("id_hh_pers","A200","A015",  "A206", "P007p")]
  #Rename column 
  person_dv <- plyr::rename(person_dv, c("A200"="Employment_status", "A015" = "student",  "A206" = "Economic_pos", "P007p" = "gross_wage"))
  #Subset dataset that the varaible is labelled as not recorded 
  person_dv <- person_dv[!(person_dv$Employment_status == 0 | person_dv$student == 0),]
  #unemployment status 0 = unemployed, 1 = employed 
  person_dv$unEmployment_status <- ifelse(person_dv$Employment_status == 1,1,0)
  #Economic position 
  person_dv$Employment_type <- as.factor(person_dv$Economic_pos)
  #student status 0 = not student 1 = student 
  person_dv$student <- ifelse(person_dv$student == 3,1,0)
  #Gross rate 
  person_dv$gross_wage <- as.numeric(person_dv$gross_wage)*1000
  brks <- c(25000,35000,450000,550000 , 2000000)
  labs <- c("25_35K","35_45K","45_55K","55K+")
  person_dv$gross_wage <- cut(as.numeric(person_dv$gross_wage), breaks = brks, labels = labs) # bin the age variable 
  return(person_dv)
}

#format_dv_person(person_dv_2011)

#Function 4: Join the previous tables to gether to get individual characteristics estimates 

person_char <- function(prod_person, format_dv_person){
  #Join PERSON LEVEL table, survey table and raw people table
  prod_person <- inner_join(prod_person, format_dv_person, by="id_hh_pers")
  return(prod_person)
}

#person_char(prod_person,format_dv_person)


#Function 5: Format diary data -> Estimate the consumption of tobacoo expenditure -> Main focus 

format_diary_code <- function(diary){
  
  #COI_PLUS: Coicop code 
  #pdamount: expenditure amount 
  #tobacoo COICOP code 02.2.0
  #final_mnemonic <- NULL
  #diary <- merge(diary, adult_child_exp_reg[,c("id_hh_pers", "exp_reg")]) ## FOR PRODUCT LEVEL RRCPL
  #print(head(diary))
  diary$id_hh_pers <- as.numeric(paste(diary$case, diary$Person, sep="."))
  # Convert . to _ e.g. 1.1.1.1.1 to 1_1_1_1_1 
  diary$COI_PLUS <- gsub(".", '_', diary$COI_PLUS, fixed = TRUE)
  # Convert 1_1_1_1_1  to X1_1_1_1_1 
  diary$COI_PLUS <- paste("X", diary$COI_PLUS, sep = "" )
  

  smoke_list <- c("X2_2_1_1_1", "X2_2_1_2_1", "X2_2_1_3_1")
  alc_hh_list <- c("X2_1_1_1_1", "X2_1_2_1_1", "X2_1_2_1_2", "X2_1_2_1_3", "X2_1_2_1_4", "X2_1_2_2_1", "X2_1_3_1_1")
  alc_tob_hh_list <- c(smoke_list,alc_hh_list)
  #Convert X1_1_1_1_1 to X1 d
  diary$div_code <- gsub("[_].*$", "", diary$COI_PLUS)
  #Factorise division code 
  diary$div_code <- as.factor(diary$div_code)
  diary$s2 <- substring(diary$COI_PLUS, 1, 3)
  diary$s3 <- substring(diary$COI_PLUS, 1, 4)
  diary$s4 <- substring(diary$COI_PLUS, 1, 5)
  diary$s5 <- substring(diary$COI_PLUS, 1, 6)
  diary_all_list <- unique(as.vector(diary$COI_PLUS))
  food_drink_tob_all_list <- c(unique(as.vector(diary[which(diary$div_code == 'X1' | diary$s4 == 'X11_1' | diary$s3 == 'X2_1' | diary$s3 == 'X2_2') ,c("COI_PLUS")])), 'X3_11_3',  'X3_11_8',  "X20_5_2_1_3")
  food_drink_tob_all_list <-  c(unique(as.vector(diary[which(diary$div_code == 'X1' | diary$s4 == 'X11_1' | diary$s3 == 'X2_1' | diary$s3 == 'X2_2' | diary$COI_PLUS == 'X3_11_3' | diary$COI_PLUS == 'X3_11_8' |  diary$COI_PLUS == "X20_5_2_1_3") ,c("COI_PLUS")])))
  food_drink_tob_all_list <- c(unique(as.vector(diary[which(diary$div_code == 'X1' | diary$s4 == 'X11_1' | diary$s3 == 'X2_1' | diary$s3 == 'X2_2') ,c("COI_PLUS")])))
  #print (paste("FOOD DRINK TOB ALL LIST", food_drink_tob_all_list))
  #print(length(food_drink_tob_all_list))
  food_non_alc_hh_list <- c(unique(as.vector(diary[which(diary$div_code == 'X1'),c("COI_PLUS")])), 'X3_11_3',  'X3_11_8',  "X20_5_2_1_3")
  food_non_alc_hh_list <- c(unique(as.vector(diary[which(diary$div_code == 'X1'),c("COI_PLUS")])))
  rest_hotel_list <- unique(as.vector(diary[which(diary$s2 == 'X11'),c("COI_PLUS")]))
  rec_cult_list <- unique(as.vector(diary[which(diary$div_code == 'X9') ,c("COI_PLUS")]))
  clot_foot_list <- unique(as.vector(diary[which(diary$div_code == 'X3'),c("COI_PLUS")]))
  hh_services_list <- unique(as.vector(diary[which(diary$div_code == 'X4'),c("COI_PLUS")]))
  furniture_hh_goods_list <- unique(as.vector(diary[which(diary$div_code == 'X5'),c("COI_PLUS")]))
  transport_list <- unique(as.vector(diary[which(diary$div_code == 'X7'),c("COI_PLUS")]))
  comms_list <- unique(as.vector(diary[which(diary$div_code == 'X8'),c("COI_PLUS")]))
  misc_goods_serv_list <- unique(as.vector(diary[which(diary$s2 == 'X12'),c("COI_PLUS")]))
  diary$cat <- NA
  diary$cat[diary$COI_PLUS %in% food_non_alc_hh_list$COI_PLUS] <- "food_non_alc_hh"
  diary$cat[diary$COI_PLUS %in% hh_services_list$COI_PLUS] <- "hh_services"
  diary$cat[diary$COI_PLUS %in% clot_foot_list$COI_PLUS] <- "clot_foot"
  diary$cat[diary$COI_PLUS %in% furniture_hh_goods_list$COI_PLUS] <- "furniture_hh_goods"
  diary$cat[diary$COI_PLUS %in% transport_list$COI_PLUS] <- "transport"
  diary$cat[diary$COI_PLUS %in% comms_list$COI_PLUS] <- "comms"
  diary$cat[diary$COI_PLUS %in% rec_cult_list$COI_PLUS] <- "rec_cult"
  diary$cat[diary$COI_PLUS %in% rest_hotel_list$COI_PLUS] <- "rest_hotel"
  diary$cat[diary$COI_PLUS %in% misc_goods_serv_list$COI_PLUS] <- "misc_goods_serv"
  diary$cat[diary$COI_PLUS %in% smoke_list] <- "Smoke"
  diary$cat[diary$COI_PLUS %in% alc_hh_list] <- "alcohol"
  diary$cat[is.na(diary$cat)] <- "other"
  #diary <- aggregate(. ~id_hh_pers + COI_PLUS ,diary, FUN = function(x) mean(as.numeric(as.character(x))))

  return(diary)
  
}

#format_diary_code(diary_2011) 

format_diary <- function(diary,category){
  diary <- format_diary_code(diary)
  diary <-  diary[diary$cat %in% category,]
  diary <- aggregate(. ~id_hh_pers + COI_PLUS ,diary, FUN = function(x) mean(as.numeric(as.character(x))))
  
}
#format_diary(diary_2011,"alcohol") 





#Function 6: Join the previous tables to gether to get individual and consumption characteristics for smoking

person_char2 <- function(All_Ind, format_diary){
  #Join PERSON LEVEL table, survey table and raw people table
  ind <- inner_join(All_Ind, format_diary, by="id_hh_pers")
  ind <- ind[,c("id_hh_pers", "Sex", "age", "age_gen","Eth_ori"
                ,"weighta", "Gorx","unEmployment_status","student","Economic_pos"
                , "gross_wage","pdamount","cat")]
  return(ind)
}


Ind_format <- function(diary,raw_person,person_dv,survey_hh,category){
  ind <- format_raw_person(raw_person)
  ind <- prod_person(ind,survey_hh)
  ind_dv <- format_dv_person(person_dv)
  ind <- person_char(ind,ind_dv)
  ind_diary <- format_diary(diary,category)
  ind <- person_char2(ind,ind_diary)
  return(ind)
}

Ind_all <- function(diary,raw_person,person_dv,survey_hh){
  ind <- format_raw_person(raw_person)
  ind <- prod_person(ind,survey_hh)
  ind_dv <- format_dv_person(person_dv)
  ind <- person_char(ind,ind_dv)
  ind_diary <- format_diary_code(diary)
  ind <- person_char2(ind,ind_diary)
  return(ind)
}



# Step 3 Reformat constraints data -----

#Using Composite datasets from different data sources MSOA 
setwd("~/Desktop/Data/Constraints")


Geodemo <- read_excel("2011censusdata_tcm77-409596-2.xls", skip = 5)

Population <- read_excel("Copy of ukmidyearestimates20182019ladcodes.xls", sheet = "MYE 5", skip = 3)

wage <- read_excel("1smallareaincomeestimatesdata.xls",sheet = "Total annual income", skip = 3)

#Constraint format 
const_char <- function(Geodemo, Population,wage_dat){
  #Northern Ireland is excluded 
  
  #As well as NA -> I will refine this 
  
  #Population <- plyr::rename(Population,replace = c("`Estimated Population mid-2011`" = "Pop_2011"))
  const <- inner_join(Geodemo, Population, by = "Code")
  wage_dat <- aggregate(x = wage_dat$"Total annual income (£)" , by = list(wage_dat$"Local authority code"), FUN = function(x) mean(as.numeric(as.character(x))))
  colnames(wage_dat) <- c("Code","income")
  brks <- c(25000,35000,450000,550000 , 2000000)
  labs <- c("25_35K","35_45K","45_55K","55K+")
  wage_dat$income <- cut(as.numeric(wage_dat$income), breaks = brks, labels = labs) # bin the age variable 
  
  const <- inner_join(const, wage_dat, by = "Code")
  
  #Age Category  -> Not Ok Age group 15_24 is missing 
  const$"% Persons aged 15-24" <- 100 - const$`% Persons aged 90+` - const$`% Persons aged 65–89` - const$`% Persons aged 45–64` - const$`% Persons aged 25–44` - const$`% Persons aged 5–14` - const$`% Persons aged 0–4` 
  
  agecat_list <- c("% Persons aged 0–4", 
                   "% Persons aged 5–14",
                   "% Persons aged 15-24",
                   "% Persons aged 25–44",                                                                                                            
                   "% Persons aged 45–64" ,                                                                                                           
                   "% Persons aged 65–89",                                                                                                            
                   "% Persons aged 90+")
  #Ethnicity  -> Not ok -> 2 asian group 
  
  const$"% Persons who are Asian/Asian British" <- const$"% Persons who are Asian/Asian British: Indian/Pakistani/Bangladeshi" +
    const$"% Persons who are Asian/Asian British: Chinese and Other"
  
  Ethnicity_list <- c( "% Persons who are white",                                                                                                         
                       "% Persons who have mixed ethnicity or are from multiple ethnic groups",                                                           
                       "% Persons who are Asian/Asian British",                                                              
                       "% Persons who are Black/African/Caribbean/Black British",                                                                         
                       "% Persons who are Arab or are from another ethnic group"   )
  #Employment Status (Binary) -> Ok
  unemployment <- c("% Persons aged 16–74 who are unemployed")
  #Student Status (Binary) -> Ok
  student <- c('% Households with full-time students')
  #Income -> Ok
  wage  <- c("income")
  #Percentage List  
  Per_list <- c(agecat_list,Ethnicity_list,unemployment,student)
  for(i in Per_list){
    const[[i]] <- (const[[i]]*const$`Estimated Population mid-2011`)/100
  }
  const$emplyment <- const$`Estimated Population mid-2011` - const$"% Persons aged 16–74 who are unemployed"
  const$nonstudent <- const$`Estimated Population mid-2011` - const$'% Households with full-time students'
  
  const <- const[,c("Code","Region/Country" ,"Supergroup code" ,"Group code","Subgroup code", Per_list,wage,"emplyment","nonstudent")]
  
  
  #Recode the column name 
  colnames(const) <- c("Code","Region/Country" ,"Supergroup code" ,"Group code","Subgroup code","0_4","5_14","15_24","25_44","45_64","65_89", "90+","White","Mixed","Asian","Black", "Other","unemployed","student",
                       "income","emplyment","nonstudent")
  return(const)
}

#Cons

# Step 4 Reformat both data ----------------
# create new age/sex variable

#Individual data all
ind <- Ind_all(diary_2011,raw_person_2011,person_dv_2011,survey_hh_2011)


#Individual Data 
ind_alcohol <- Ind_format(diary_2011,raw_person_2011,person_dv_2011,survey_hh_2011,"alcohol")
ind_alcohol_london <- ind_alcohol[ind_alcohol$Gorx == 7,]

ind_smoke <- Ind_format(diary_2011,raw_person_2011,person_dv_2011,survey_hh_2011,"Smoke")
ind_smoke_london <- ind_smoke[ind_smoke$Gorx == 7,]

ind_misc_goods_serv<- Ind_format(diary_2011,raw_person_2011,person_dv_2011,survey_hh_2011,"misc_goods_serv")
ind_misc_goods_serv_london <- ind_misc_goods_serv[ind_misc_goods_serv$Gorx == 7,]

#Constraint data 
cons <- const_char(Geodemo,Population,wage)

cons <- cons[cons$`Region/Country` == "London",]
ind_cat <- function(ind){
  #Constraint based on age 
  Age <- paste0(ind$age)
  unique(Age)
  
  # matrix for constraint 1 - age
  m1 <- model.matrix(~Age-1)
  colnames(m1) <- names(cons)[8:11]
  
  # matrix for con2 (Ethnicity)
  ind$Eth_ori <- as.character(ind$Eth_ori)
  m2 <- model.matrix(~ind$Eth_ori-1)
  colnames(m2) <- names(cons)[13:17]
  
  
  # matrix for con3 (Unemployment)
  ind$unEmployment_status <- as.character(ind$unEmployment_status)
  m3 <- model.matrix(~ind$unEmployment_status-1)
  colnames(m3) <- names(cons)[c(18,21)]
  
  # matrix for con4 (student)
  ind$student <- as.character(ind$student)
  m4 <- model.matrix(~ind$student-1)
  colnames(m4) <- names(cons)[c(22,19)]
  
  #Income is a bit tricky -> there is no population statistics on different income groups 
  
  #Create individual category 
  ind.cat <- data.frame(cbind(m1, m2, m3,m4))
  
  return(ind.cat)
}

ind_alcohol_london.cat <- ind_cat(ind_alcohol_london)
ind_smoke_london.cat <- ind_cat(ind_smoke_london)
ind_misc_goods_serv_london.cat <-ind_cat(ind_misc_goods_serv_london)
#Setting up constraints 

con1 <- cons[8:11]
con2 <- cons[13:17]
con3 <- cons[c(18,21)]
con4 <- cons[c(22,19)]

# check constraint totals - should be true
sum(ind.cat[,1:ncol(con1)]) == nrow(ind) # is the number in each category correct?
sum(ind.cat[,ncol(con1)+1:ncol(con2)]) == nrow(ind) 

# Step 5 Create weight constraint---------------

#First iteration all weight equals to 1 for each individual in all regions (in this case region 1 and 2) 
#I only need the constraint varaibles 

num.cons <- 4 # n. constraints 
num.its <- num.cons+1 # how many iterations will we run?

#We only keep 
cons_new <- cons[,c(8:11,13:19,21,22)]

#Rearrange cons_new to ensure the column name is matched with ind.cat
cons_new <- cons_new[,c(colnames(cons_new)[1:10],"emplyment","nonstudent","student")]

# create weights in 3D matrix (individuals, areas, iteration)
weights <- array(dim=c(nrow(ind),nrow(cons_new), 4)) 
weights[,,1] <- 1 # sets all initial weights to 1
weights1 <- data.frame(weights[,,1])

# convert survey data into aggregates to compare with census (3D matix)

#Ahhh fair -> Good -> ind.agg represents the initial number of population with weighting of 1 in each geographical regions based on survey data
ind.agg <- array(dim=c(nrow(cons_new),ncol(cons_new),5))
for (i in 1:nrow((cons_new))){
  ind.agg[i,,1]   <- colSums(ind.cat) * weights[1,i,1]
  #Colsums is the number of individual in each category 
  #weights[1,i,1] is the first iteration of the 
}
colnames(ind.agg) <- colnames(cons_new)

# Step 6 rewighting:IPF with mipfp-----
weights2 <- data.frame(weights[,,1])
for (j in 1:nrow(cons_new)){
  for(i in 1:ncol(con1)){
    weights2[which(ind.cat[,i] == 1),j]<- cons_new[j,i]*weights[which(ind.cat[[i]] == 1),j,1]/ ind.agg[j,i,1]
  }
}


for (i in 1:nrow(cons_new)){ # convert con1 weights back into aggregates
  ind.agg[i,,2]   <- colSums(ind.cat * weights[,i,1] * weights2[,i])
}
# test results for first row (not necessary for model)
ind.agg[1,1,2] - cons_new[1,1] # should be zero for age/sex

# second constraint
weights3 <- weights2

for (j in 1:nrow(cons_new)){
  for(i in (1+ncol(con1)):(ncol(con2) + ncol(con1))){
    weights3[which(ind.cat[,i] == 1),j] <- cons_new[j,i]/ ind.agg[j,i,2]
  }
}  
for (i in 1:nrow(cons_new)){ # convert con2 back into aggregate
  ind.agg[i,,3] <- colSums(ind.cat * weights[,i,1] * weights2[,i] * weights3[,i])
}

ind.agg[1,,3] - cons_new[1,] # should be close to zero for new constraint

# third constraint
weights4 <- weights3

for (j in 1:nrow(cons_new)){
  for(i in (1+ncol(con1)+ncol(con2)):(ncol(con3)+ncol(con2) + ncol(con1))){
    weights4[which(ind.cat[,i] == 1),j] <- cons_new[j,i]/ ind.agg[j,i,3]
  }
}  
for (i in 1:nrow(cons_new)){ # convert con2 back into aggregate
  ind.agg[i,,4] <- colSums(ind.cat * weights[,i,1] * weights2[,i] * weights3[,i]* weights4[,i])
}

ind.agg[1:3,,4] - cons_new[1:3,] # test the result

# fourth constraint
weights5 <- weights4

for (j in 1:nrow(cons_new)){
  for(i in (1+ncol(con1)+ncol(con2)+ncol(con3)):(ncol(con4)+ncol(con3)+ncol(con2) + ncol(con1))){
    weights5[which(ind.cat[,i] == 1),j] <- cons_new[j,i]/ ind.agg[j,i,4]
  }
}  
for (i in 1:nrow(cons_new)){ # convert con2 back into aggregate
  ind.agg[i,,5] <- colSums(ind.cat * weights[,i,1] * weights2[,i] * weights3[,i]* weights4[,i]*weights5[,i])
}

ind.agg[1:3,,5] - cons_new[1:3,] # test the result

fw <- weights1 * weights2 * weights3* weights4*weights5# save final weights (not done in 'e2.R')


# Step 7 Interisation & Expansion--------


# 'Proportional probabilities' (PP) method of integerisation
# (see http://www.sciencedirect.com/science/article/pii/S0198971513000240):
int_pp <- function(x){
  xv <- as.vector(x)
  xint <- rep(0, length(x))
  xs <- sample(length(x), size = round(sum(x)), prob = x, replace = T)
  xsumm <- summary(as.factor(xs))
  topup <- as.numeric(names(xsumm))
  xint[topup] <- xsumm
  dim(xint) <- dim(x)
  xint
}

# 'Truncate, replicate, sample' (TRS) method of integerisation
# (see http://www.sciencedirect.com/science/article/pii/S0198971513000240):
int_trs <- function(x){
  xv <- as.vector(x)
  xint <- floor(xv)
  r <- xv - xint
  def <- round(sum(r)) # the deficit population
  # the weights be 'topped up' (+ 1 applied)
  topup <- sample(length(x), size = def, prob = r)
  xint[topup] <- xint[topup] + 1
  dim(xint) <- dim(x)
  dimnames(xint) <- dimnames(x)
  xint
}

int_expand_vector <- function(x){
  index <- 1:length(x)
  rep(index, round(x))
}

int_expand_array <- function(x){
  # Transform the array into a dataframe
  count_data <- as.data.frame.table(x)
  # Store the indices of categories for the final population
  indices <- rep(1:nrow(count_data), count_data$Freq)
  # Create the final individuals
  ind_data <- count_data[indices,]
  ind_data
}



# Total absolute error
tae <- function(observed, simulated){
  obs_vec <- as.numeric(observed)
  sim_vec <- as.numeric(simulated)
  sum(abs(obs_vec - sim_vec))
}

# Number of times each unique matrix row appears
umat_count <- function(x) {
  xp <- apply(x, 1, paste0, collapse = "") # "pasted" version of constraints
  freq <- table(xp) # frequency of occurence of each individual
  xu <- unique(x) # save only unique individuals
  rns <- as.integer(row.names(xu)) # save the row names of unique values of ind
  xpu <- xp[rns]
  o <- order(xpu, decreasing = TRUE) # the order of the output (to rectify table)
  cbind(xu, data.frame(ind_num = freq[o], rns = rns)) # output
}

# Generates list of outputs - requires dplyr
umat_count_dplyr <- function(x){
  x$p <- apply(x, 1, paste0, collapse = "")
  up <- data.frame(p = unique(x$p)) # unique values in order they appeared
  y <- dplyr::count(x, p) # fast freq table
  umat <- inner_join(up, y) # quite fast
  umat <- join(umat, x, match = "first")
  list(u = umat, p = x$p) # return unique individuals and attributes
}

# set up the objects we'll be using subsequently
intall <- ints <- as.list(1:nrow(cons_new)) # Names of integer indices (ints), and integer populations (intall) in ordered list
intagg <- cons_new * 0 # Aggregate area stats - set to 0 to avoid confusion
Estimate <- data.frame(avCake = rep(0,nrow(cons_new)), sdCake = rep(0,nrow(cons_new)))


# Method 1: using a for loop
Estimate <- data.frame(code = NA, av = rep(0,nrow(cons_new)), sd = rep(0,nrow(cons_new)))
set.seed(42)
colnames(fw) <- cons$Code
#So for each regions, the weiight for each individual for each regioins will be interger and expand in the correponding regions to estimate the total population 
for(i in 1:nrow(cons_new)){
  # Integerise and expand
  #Since i ranges from 1 to 324 which is the total number of local authority level   
  #fw[,1] represents individual weighting in region 1 
  ints[[i]] <- int_expand_vector(int_trs(fw[, i]))
  # Take the right individuals 
  data_frame <- data.frame(ind[ints[[i]],])
  Estimate$code[i] <- cons$Code[i]
  Estimate$av[i] <- mean(data_frame$pdamount)
  Estimate$sd[i] <- sd(data_frame$pdamount)
}

#Through the process of expansion, the integer weight matrix produced by integerisation can be converted into the final spatial microdata output stored in ‘long’ format represented in the right-hand box of Figure~5.1. 
#Thus the combined processes of integerisation and expansion allow weight matrices to be translated into the same output format that combinatorial optimisation algorithms produce directly. In other words fractional weighting is interchangeable with combinatorial optimisation approaches for population synthesis.

#Estimate



# Step 8 Plotting the estimates result --------
plot(Estimate$av)
setwd("~/Desktop/Data/Local_Authority_Districts_April_2019_Boundaries_UK_BUC")
library(rgdal)
library(tmap)
library(ggplot2)
LA <- readOGR("Local_Authority_Districts_April_2019_Boundaries_UK_BUC.shp")
LA@data <- merge( LA@data,Estimate,by.y = "code",by.x = "lad19cd", all = TRUE)
LA@data[is.na(LA@data$av),] <- mean(LA@data$av,na.rm = T)
tmap_mode("View")

  tm1 <- tm_shape(LA) + tm_polygons("av", convert2density = FALSE,style = "quantile",
                                  palette = "BuPu") + 	
  tm_layout(main.title = "Simulated average expenditure on tobacco per week",
                                                                 main.title.position = "center",
                                                                 main.title.color = "black",
                                                                 main.title.size = 0.7)

tm2 <- tm_shape(LA) + 
  tm_polygons("sd", convert2density = FALSE,style = "quantile") + 
  tm_layout(main.title = "Simulated standard deviation expenditure on tobacco per week",
                                                                                                main.title.position = "center",
                                                                                                main.title.color = "black",
                                                                                                main.title.size = 0.7)

tmap_arrange(tm1, tm2)



# Step 9 Validation -------------------
# Bonus: Using python to automate IPF and generate new weighting -----

# 'Truncate, replicate, sample' (TRS) method of integerisation
# (see http://www.sciencedirect.com/science/article/pii/S0198971513000240):

area_weight_alc <- read_csv("~/Desktop/IterativeProportionalFitting/OutputData/area_weight_alc.csv")
area_weight_smoke <- read_csv("~/Desktop/IterativeProportionalFitting/OutputData/area_weight_smoke.csv")
area_weight_misc <- read_csv("~/Desktop/IterativeProportionalFitting/OutputData/area_weight_misc.csv")
TAE_alc <- read_csv("~/Desktop/IterativeProportionalFitting/OutputData/TAE_alc.csv")
TAE_smoke <- read_csv("~/Desktop/IterativeProportionalFitting/OutputData/TAE_smoke.csv")
TAE_misc <- read_csv("~/Desktop/IterativeProportionalFitting/OutputData/TAE_misc.csv")

area_weight_alc <- area_weight_alc[2:length(area_weight_alc)]
area_weight_smoke <- area_weight_smoke[2:length(area_weight_smoke)]
area_weight_misc <- area_weight_misc[2:length(area_weight_misc)]
TAE_alc <- TAE_alc[2:length(TAE_alc)]
TAE_smoke <- TAE_smoke[2:length(TAE_smoke)]
TAE_misc <- TAE_misc[2:length(TAE_misc)]


int_trs <- function(x){
  xv <- as.vector(x)
  xint <- floor(xv)
  r <- xv - xint
  def <- round(sum(r)) # the deficit population
  # the weights be 'topped up' (+ 1 applied)
  topup <- sample(length(x), size = def, prob = r)
  xint[topup] <- xint[topup] + 1
  dim(xint) <- dim(x)
  dimnames(xint) <- dimnames(x)
  xint
}

int_expand_vector <- function(x){
  index <- 1:length(x)
  rep(index, round(x))
}


# Method 1: using a for loop
Estimate_smoke <- data.frame(code = NA, skav = rep(0,nrow(area_weight_smoke)), sksd = rep(0,nrow(area_weight_smoke)))
Estimate_alc <- data.frame(code = NA, alav = rep(0,nrow(area_weight_alc)), alsd = rep(0,nrow(area_weight_alc)))
Estimate_misc <- data.frame(code = NA, miav = rep(0,nrow(area_weight_alc)), misd = rep(0,nrow(area_weight_alc)))

set.seed(42)
#So for each regions, the weiight for each individual for each regioins will be interger and expand in the correponding regions to estimate the total population 
for(i in 1:(nrow(area_weight_smoke))){
  # Integerise and expand
  #Since i ranges from 1 to 324 which is the total number of local authority level   
  #fw[,1] represents individual weighting in region 1 
  ints[[i]] <- int_expand_vector(int_trs(area_weight_smoke[i,]))
  # Take the right individuals 
  data_frame <- data.frame(ind_smoke_london[ints[[i]],])
  Estimate_smoke$code[i] <- cons$Code[i]
  Estimate_smoke$skav[i] <- mean(data_frame$pdamount)
  Estimate_smoke$sksd[i] <- sd(data_frame$pdamount)
}


for(i in 1:(nrow(area_weight_alc))){
  # Integerise and expand
  #Since i ranges from 1 to 324 which is the total number of local authority level   
  #fw[,1] represents individual weighting in region 1 
  ints[[i]] <- int_expand_vector(int_trs(area_weight_alc[i,]))
  # Take the right individuals 
  data_frame <- data.frame(ind_alcohol_london[ints[[i]],])
  Estimate_alc$code[i] <- cons$Code[i]
  Estimate_alc$alav[i] <- mean(data_frame$pdamount)
  Estimate_alc$alsd[i] <- sd(data_frame$pdamount)
}

for(i in 1:(nrow(area_weight_misc))){
  # Integerise and expand
  #Since i ranges from 1 to 324 which is the total number of local authority level   
  #fw[,1] represents individual weighting in region 1 
  ints[[i]] <- int_expand_vector(int_trs(area_weight_misc[i,]))
  # Take the right individuals 
  data_frame <- data.frame(ind_misc_goods_serv_london[ints[[i]],])
  Estimate_misc$code[i] <- cons$Code[i]
  Estimate_misc$miav[i] <- mean(data_frame$pdamount)
  Estimate_misc$misd[i] <- sd(data_frame$pdamount)
}

TAE_alc <- data.frame(code = cons$Code,TAE_alc = TAE_alc$`0`)
TAE_smoke <- data.frame(code = cons$Code,TAE_smoke = TAE_smoke$`0`)
TAE_misc <- data.frame(code = cons$Code,TAE_misc = TAE_misc$`0`)

#Through the process of expansion, the integer weight matrix produced by integerisation can be converted into the final spatial microdata output stored in ‘long’ format represented in the right-hand box of Figure~5.1. 
#Thus the combined processes of integerisation and expansion allow weight matrices to be translated into the same output format that combinatorial optimisation algorithms produce directly. In other words fractional weighting is interchangeable with combinatorial optimisation approaches for population synthesis.

#Estimate



setwd("~/Desktop/IterativeProportionalFitting/statistical-gis-boundaries-london/ESRI")
library(rgdal)
library(tmap)
library(ggplot2)
LA <- readOGR("London_Borough_Excluding_MHW.shp")
LA@data$id <- 1:nrow(LA@data)
LA@data <- merge(LA@data,Estimate_smoke,by.x = "GSS_CODE",by.y = "code",all.x = TRUE)
LA@data <- merge(LA@data,Estimate_alc,by.x = "GSS_CODE",by.y = "code",all.x = TRUE)
LA@data <- merge(LA@data,Estimate_misc,by.x = "GSS_CODE",by.y = "code",all.x = TRUE)

LA@data <- merge(LA@data,TAE_alc,by.x = "GSS_CODE",by.y = "code",all.x = TRUE)
LA@data <- merge(LA@data,TAE_smoke,by.x = "GSS_CODE",by.y = "code",all.x = TRUE)
LA@data <- merge(LA@data,TAE_misc,by.x = "GSS_CODE",by.y = "code",all.x = TRUE)

LA@data <- LA@data[order(LA@data$id),]
#LA@data[is.na(LA@data$av),] <- mean(LA@data$av,na.rm = T)
#LA@data <- LA@data[complete.cases(LA@data$av),]
tmap_mode("view")
tmap_options(limits = c(facets.view = 6))

tm1_smoke <- tm_shape(LA) + tm_polygons("skav", convert2density = FALSE,
                                  palette = "BuPu") + 	
  tm_layout(main.title = "Simulated average expenditure on tobacco per week",
            main.title.position = "center",
            main.title.color = "black",
            main.title.size = 0.7)

tm1_alc <- tm_shape(LA) + tm_polygons("alav", convert2density = FALSE) + 	
  tm_layout(main.title = "Simulated average expenditure on tobacco per week",
            main.title.position = "center",
            main.title.color = "black",
            main.title.size = 0.7)

tm1_misc <- tm_shape(LA) + tm_polygons("miav", convert2density = FALSE) + 	
  tm_layout(main.title = "Simulated average expenditure on tobacco per week",
            main.title.position = "center",
            main.title.color = "black",
            main.title.size = 0.7)

tm1_smoke_TAE <- tm_shape(LA) + tm_polygons("TAE_smoke", convert2density = FALSE,
                                        palette = "BuPu") + 	
  tm_layout(main.title = "Simulated average expenditure on tobacco per week",
            main.title.position = "center",
            main.title.color = "black",
            main.title.size = 0.7)

tm1_alc_TAE <- tm_shape(LA) + tm_polygons("TAE_alc", convert2density = FALSE) + 	
  tm_layout(main.title = "Simulated average expenditure on tobacco per week",
            main.title.position = "center",
            main.title.color = "black",
            main.title.size = 0.7)

tm1_misc_TAE <- tm_shape(LA) + tm_polygons("TAE_misc", convert2density = FALSE) + 	
  tm_layout(main.title = "Simulated average expenditure on tobacco per week",
            main.title.position = "center",
            main.title.color = "black",
            main.title.size = 0.7)


tmap_arrange(tm1_smoke, tm1_alc,tm1_misc,tm1_smoke_TAE,tm1_alc_TAE,tm1_misc_TAE)



tm2 <- tm_shape(LA) + 
  tm_polygons("sd", convert2density = FALSE,style = "quantile") + 
  tm_layout(main.title = "Simulated standard deviation expenditure on tobacco per week",
            main.title.position = "center",
            main.title.color = "black",
            main.title.size = 0.7)


