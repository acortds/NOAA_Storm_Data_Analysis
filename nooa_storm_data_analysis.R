rm(list = ls())

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
}

