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
storm_data_gt0 <- storm_data %>%
      mutate(event_type=EVTYPE) %>%
      group_by(event_type) %>%
      summarise(fatalities = sum(FATALITIES), injuries = sum(INJURIES), total_event=fatalities+injuries) %>%
      filter(total_event > 0) %>%
      arrange(desc(total_event), desc(fatalities))

# Across the United States, which types of events have the greatest economic 
# consequences?

