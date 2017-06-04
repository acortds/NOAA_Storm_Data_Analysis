#
# Author: Adrian Cortinas
# Date:  June 4th, 2017
# Description: Program to analyse NOAA Storm Data for Coursera's Reproducible
#                 Research course in John Hopkin's Data Science Track
# This program is free. You may use part or all of it.
# 

# Init - Let's remove any variable that is already created in the environment
rm(list = ls())

# Functions

# Function to download a file from a URL. The function will create or replace 
# a working directory (workdir) in the local directory. The files will be 
# downloaded to workdir dir. If the file is a zip or bz2 file,
# it will uncompress/expand the file. The function will return the list of files
# that are contained in the workdir.
# example: files <- getfile("http://site.com/file.zip", "myfile.zip","zip")
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

# Function to translate numbers given in thousand's, million's and billion's
# to their corresponding natural number
# example: get_amount(1,"K") will return 1000
get_amount <- function(quantity, expression){
      K<-1e+3
      M<-1e+6
      B<-1e+9
      if (expression == "K") {
            return(quantity*K)
      } else if (expression == "M") {
            return(quantity*M)
      } else if (expression == "B") {
            return(quantity*B)
      } else { 
            return(quantity)
      }
}

# Function to translate natural numbers to their correspoding in K (thousand's)
# M (million's) or B (Billion's), 
# example: simplify_amt(1000, "K") will return 1K 
simplify_amt <- function(quantity, mny_unit = "M", precision = 3){
      K<-1e+3
      M<-1e+6
      B<-1e+9
      if (mny_unit == "K") {
            return(paste0(round(quantity/K,
                                digits = precision),
                          "K"))
      } else if (mny_unit == "M") {
            return(paste0(round(quantity/M,
                                digits = precision),
                          "M"))
      } else if (mny_unit == "B") {
            return(paste0(round(quantity/B,
                                digits = precision),
                          "B"))
      }
}

# Get file
files <- getfile("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
                 "stormdata.csv.bz2", 
                 "bz2")

# Read file
storm_data <- read.csv(paste0("workdir/",files[1]),
                       na.strings = "NA", 
                       stringsAsFactors = T)

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Clean Data
event_list <- storm_data %>%
      select (1:37) %>%
      mutate (EVTYPE = toupper(EVTYPE)) %>%
      group_by(EVTYPE) %>%
      count() %>%
      distinct() %>%
      arrange(EVTYPE)

clean_storm_data <- storm_data %>%
      mutate (EVTYPE = toupper(EVTYPE))

clean_storm_data$EVTYPE <- gsub("EROSIN", "EROSION", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("AVALANCE", "AVALANCHE", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("BRUSH FIRES", "BRUSH FIRE", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("BLOW-OUT TIDES", "BLOW-OUT TIDE", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("BLOWING SNOW- EXTREME WIND CHI", "BLOWING SNOW & EXTREME WIND CH", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("BLOWING SNOW/EXTREME WIND CHIL", "BLOWING SNOW & EXTREME WIND CH", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("COASTAL FLOODING", "COASTAL FLOOD", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("COLD AIR FUNNELS", "COLD AIR FUNNEL", clean_storm_data$EVTYPE)

clean_storm_data$EVTYPE <- gsub("COASTALFLOOD", "COASTAL FLOOD", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("COASTALSTORM", "COASTAL STORM", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("COLD AIR FUNNELS", "COLD AIR FUNNEL", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("COLD TEMPERATURES", "COLD TEMPERATURE", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("DUST DEVEL", "DUST DEVIL", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("DUSTSTORM", "DUST STORM", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("EXTREME WIND CHILLS", "EXTREME WIND CHILL", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("FLASH FLOOD/", "FLASH FLOOD", clean_storm_data$EVTYPE)

clean_storm_data$EVTYPE <- gsub("FLASH FLOODS", "FLASH FLOOD", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("FLASH FLOOODING", "FLASH FLOOD", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("FROST\FREEZE", "FROST/FREEZE", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("FUNNEL CLOUD.", "FUNNEL CLOUD", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("FUNNEL CLOUDS", "FUNNEL CLOUD", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("GRADIENT WINDS", "GRADIENT WIND", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("GUSTY WINDS", "GUSTY WIND", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("HAILSTORMS", "HAILSTORM", clean_storm_data$EVTYPE)

clean_storm_data$EVTYPE <- gsub("HEAT WAVES", "HEAT WAVE", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("HEAVY PRECIPATATION", "HEAVY PRECIPITATION", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("HEAVY SNOW & ICE", "HEAVY SNOW AND ICE", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("HEAVY SNOW-SQUALLS", "HEAVY SNOW SQUALLS", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("HEAVY RAINS", "HEAVY RAIN", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("HVY RAIN", "HEAVY RAIN", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("LANDSLIDES", "LANDSLIDE", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("LIGHTING", "LIGHTNING", clean_storm_data$EVTYPE)

clean_storm_data$EVTYPE <- gsub("WINDSS", "WINDS", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("LIGHTNING.", "LIGHTNING", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("LIGNTNING", "LIGHTNING", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("MUD SLIDES", "MUD SLIDE", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("RECORD COOL", "RECORD COLD", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("RIP CURRENTS", "RIP CURRENT", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("SEVERE THUNDERSTORMS", "SEVERE THUNDERSTORM", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("SNOW SQUALLS", "SNOW SQUALL", clean_storm_data$EVTYPE)

clean_storm_data$EVTYPE <- gsub("STRONG WINDS", "STRONG WIND", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("WINDS53", "WINDS 53", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("WINS", "WIND", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("WINDS", "WIND", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("WATERSPOUT", "WATER SPOUT", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("WATERSPOUTS", "WATER SPOUT", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("WAYTERSPOUT", "WATER SPOUT", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("MICOBURST", "MICROBURST", clean_storm_data$EVTYPE)

clean_storm_data$EVTYPE <- gsub("WILD FIRES", "WILDFIRE", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("WILDFIRES", "WILDFIRE", clean_storm_data$EVTYPE)
clean_storm_data$EVTYPE <- gsub("WND", "WIND", clean_storm_data$EVTYPE)

# Across the United States, which types of events (as indicated in the EVTYPE 
# variable) are most harmful with respect to population health?

health_data_t20 <- clean_storm_data %>%
      select (EVTYPE, 
              FATALITIES, 
              INJURIES) %>%
      mutate(event_type=toupper(EVTYPE)) %>%
      group_by(event_type) %>%
      summarise(fatalities = sum(FATALITIES), 
                injuries = sum(INJURIES), 
                total_event=fatalities+injuries,
                pct_fatalities = round(fatalities/total_event,3)*100,
                pct_injuries = round(injuries/total_event,3)*100) %>%
      filter(total_event > 0) %>%
      arrange(desc(pct_fatalities)) %>%
      top_n(20,fatalities )

g <- ggplot(health_data_t20, 
            aes(x = reorder(event_type,
                            order(pct_fatalities, 
                                  decreasing=TRUE)), 
                y = pct_fatalities))

g + geom_point() + 
      ggtitle("Top 20 Fatal Natural Events") +
      labs(x="Natural Event", 
           y="Fatality Percent") +
      theme_bw(base_size = 10) +
      theme(plot.title = element_text(hjust = 0.5),
            #legend.position = "none", 
            axis.text.x = element_text(angle = 90, 
                                       hjust =1))

# Across the United States, which types of events have the greatest economic 
# consequences?

econo_data_t20 <- clean_storm_data %>%
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
                total_event_dmg = total_prop_dmg+total_crop_dmg) %>%
      mutate (total_prop_dmg_m=simplify_amt(total_prop_dmg,"M"),
              total_crop_dmg_m=simplify_amt(total_crop_dmg,"M"),
              total_dmg_m=simplify_amt(total_event_dmg,"M"),
              pct_prop_dmg=round(total_prop_dmg/total_event_dmg,3)*100,
              pct_crop_dmg=round(total_crop_dmg/total_event_dmg,3)*100) %>%
      filter(total_event_dmg > 0) %>%
      arrange(desc(total_event_dmg)) %>%
      top_n(20,total_event_dmg)

g <- ggplot(econo_data_t20, 
            aes(x = reorder(event_type,
                            order(total_event_dmg, 
                                  decreasing=TRUE)), 
                y = total_event_dmg))

g + geom_point() + 
      ggtitle("Top 20 Economic Catastrophe of Natural Events") +
      labs(x="Natural Event", 
           y="Cost (Million's)") +
      scale_y_log10(limits = c(1e+8, 2e+14)) +
      theme_bw(base_size = 10) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none", 
            axis.text.x = element_text(angle = 90, 
                                       hjust = 1))