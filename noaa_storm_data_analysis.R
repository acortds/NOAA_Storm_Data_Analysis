# Init
rm(list = ls())

# Functions
getfile <- function(source_file, dest_file, file_type) {
      pwd <- getwd()
      
      if (dir.exists("workdir")) {
            unlink("workdir", recursive = TRUE)
      }
      
      dir.create("workdir")
      setwd("workdir")
      
      fileURL <- source_file
      download.file(fileURL, destfile = dest_file,method = "curl")
      
      if (file_type == "zip") {
            unzip(dest_file)
      } else if (file_type == "bz2") {
            system(paste("/usr/bin/bunzip2 ", dest_file))
      }
      
      setwd(pwd)
      
      list.files("workdir")
}

get_amount <- function(quantity, expression){
      if (expression == "K") {
            return(quantity*1000)
      } else if (expression == "M") {
            return(quantity*1000000)
      } else if (expression == "B") {
            return(quantity*1000000000)
      } else { 
            return(quantity)
      }
}

simplify_amt <- function(quantity, mny_unit = "M", precision = 3){
      K<-1000
      M<-1000000
      B<-1000000000
      if (mny_unit == "K") {
            return(paste0(round(quantity/K,digits = precision),"K"))
      } else if (mny_unit == "M") {
            return(paste0(round(quantity/M,digits = precision),"M"))
      } else if (mny_unit == "B") {
            return(paste0(round(quantity/B,digits = precision),"B"))
      }
}

# Get file
files <- getfile("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
                 "stormdata.bz2", 
                 "bz2")

# Read file
storm_data <- read.csv(paste0("workdir/",files[1]),
                       na.strings = "NA", 
                       stringsAsFactors = T)

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Across the United States, which types of events (as indicated in the EVTYPE 
# variable) are most harmful with respect to population health?
health_data_gt0 <- storm_data %>%
      select (EVTYPE, 
              FATALITIES, 
              INJURIES) %>%
      mutate(event_type=EVTYPE) %>%
      group_by(event_type) %>%
      summarise(fatalities = sum(FATALITIES), 
                injuries = sum(INJURIES), 
                total_event=fatalities+injuries,
                pct_fatalities = round(fatalities/total_event,2),
                pct_injuries = round(injuries/total_event,2)) %>%
      filter(total_event > 0) %>%
      arrange(desc(pct_fatalities)) %>%
      top_n(20,fatalities )

# Across the United States, which types of events have the greatest economic 
# consequences?
econo_data_gt0 <- storm_data %>%
      select (EVTYPE, 
              PROPDMG,
              PROPDMGEXP,
              CROPDMG,
              CROPDMGEXP,
              WFO) %>%
      mutate(event_type=EVTYPE) %>%
      group_by(event_type) %>%
      mutate (propdmgdll = get_amount(PROPDMG, PROPDMGEXP), 
              cropdmgdll = get_amount(CROPDMG, CROPDMGEXP)) %>%
      summarise(total_prop_dmg = sum(propdmgdll),
                total_crop_dmg = sum(cropdmgdll),
                total_dmg = total_prop_dmg+total_crop_dmg) %>%
      mutate (total_prop_dmg_m=simplify_amt(total_prop_dmg),
              total_crop_dmg_m=simplify_amt(total_crop_dmg),
              total_dmg_m=simplify_amt(total_dmg)) %>%
      filter(total_dmg > 0) %>%
      arrange(desc(total_dmg)) %>%
      top_n(20)
