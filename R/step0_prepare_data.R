## Set environment -------------------------------------------------------------
library(tidyverse)
library(data.table)
library(furrr)
# library(purrr)
options(future.globals.maxSize= 89128960000)
plan(multisession, workers = 30)

path = "/home/jccit_caltech_edu/SoS/"
setwd(path)

data_path = "data/"
zip_path = "data/zip/"
txt_path = "data/txt/"
raw_path = "data/raw/"
summary_path = "data/summary/"
clean_path = "data/clean/"
subset_path = "data/subset/initials/"
exact_path = "data/result/exact/"
stats_path = "data/result/stats/"
prob_path = "data/result/prob/"
match_path = "data/result/matches/"

var_list = c("LastName", "FirstName", "MiddleName", "Suffix", "NamePrefix",
             "Address", "MailingAddress", "NonStandardAddress", "PlaceOfBirth",
             "City", "State", "Zip", "Phone1", "Phone2", "Email", "Language",
             "DOB", "Gender", "PartyCode", "Status", "VbmVoterType")

var_exclude = c("CountyCode", "RegistrantID", "Phone3Area", "Phone3Exchange",
                "Phone3NumberPart", "EmsVoterId", "PrecinctId")

## Functions -------------------------------------------------------------------
process_one_match = function(data, stats, var_list, var_exclude){
  dfA = data$id_match_A %>%
    select(-all_of(var_exclude))
  dfB = data$id_match_B[match(data$id_match_B$RegistrantID,
                              data$id_match_A$RegistrantID),] %>% 
    select(-all_of(var_exclude))
  dfA[is.na(dfA)] = ""
  dfB[is.na(dfB)] = ""
  changed = (dfA != dfB) %>% 
    as.data.frame 
  changed = changed %>% 
    mutate(
      CountyCode = data$id_match_A$CountyCode,
      Address = AddressNumber | HouseFractionNumber | AddressNumberSuffix |
        StreetDirPrefix | StreetName | StreetType | StreetDirSuffix |
        UnitType | UnitNumber,
      Phone1 = Phone1Area | Phone1Exchange | Phone1NumberPart,
      Phone2 = Phone2Area | Phone2Exchange | Phone2NumberPart,
      MailingAddress = MailingAddressLine1 | MailingAddressLine2 | 
        MailingAddressLine3 | MailingCity | MailingState |
        MailingZip5 | MailingCountry
    )
  
  output = data.frame(CountyCode = stats$by_group$CountyCode$CountyCode %>%
                        unique %>% sort)
  
  output$Date = paste0(data$id_match_A$ExtractDate[1], " | ",
                       data$id_match_B$ExtractDate[1])
  
  output = merge(output,
                 stats$by_group$CountyCode[c("CountyCode", "County", "Count")] %>% 
                   `colnames<-`(c("CountyCode", "County", "VoterCount")),
                 by = "CountyCode", all.x = TRUE)
  
  for (var in var_list) {
    agg = aggregate(changed[var],
                    by=list(CountyCode=changed$CountyCode),FUN=sum)
    output = merge(output, agg, by = "CountyCode", all.x = TRUE)
  }
  
  added = table(data$only_B$CountyCode) %>% 
    as.data.frame %>% 
    `colnames<-`(c("CountyCode", "Added"))
  dropped = table(data$only_A$CountyCode) %>% 
    as.data.frame %>% 
    `colnames<-`(c("CountyCode", "Dropped"))
  
  output = merge(output, added, by = "CountyCode", all.x = TRUE)
  output = merge(output, dropped, by = "CountyCode", all.x = TRUE)
  
  output[is.na(output)] = 0
  
  for (i in 5:ncol(output)) {
    output[i] = output[i] / output$VoterCount
  }
  
  return(output)
}

## Read data -------------------------------------------------------------------
match_list = list.files(path = match_path,
                       pattern = "matches.Rds",
                       all.files = TRUE) %>% sort

stats_list = list.files(path = stats_path,
                        pattern = ".Rds",
                        all.files = TRUE) %>% sort

data = future_map2(
  match_list %>% as.list,
  stats_list[2:length(stats_list)] %>% as.list,
  ~{
    filename_match = .x
    filename_stats = .y
    matches = readRDS(paste0(path, match_path, filename_match))
    stats = readRDS(paste0(path, stats_path, filename_stats))
    output = process_one_match(matches, stats, var_list, var_exclude)
    output
  }
) %>% 
  do.call("rbind", .)

saveRDS(data, file = paste0(path, "bayesian_analysis/data/data_for_bayesian.Rds"))

