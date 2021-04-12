
## Download script for the 02323 and 02402 Introduction to Statistics material

## WARNING: THIS SCRIPTS OVERWRITES the files, therefore if you did notes in the pdfs etc. this will be lost!

## First set the working directory (run "?getwd" in the console to learn about it):
## In RStudio: open the script, take "Session->Set Working Directory->To Source File Location"
## and then replace the line below with the line from the console (this is a good trick for all scripts)
setwd("PUT PATH TO THE SCRIPT FOLDER HERE")

## The file names of the chapters
chapternames <- c("chapter1-DataVisualization",
                  "chapter2-ProbabilitySimulation",
                  "chapter3-StatisticsNormalAssumption",
                  "chapter4-SimulationBasedStatistics",
                  "chapter5-SimpleLinearRegression",
                  "chapter6-MultipleLinearRegression",
                  "chapter7-ProportionsFrequencyTables",
                  "chapter8-StatisticsMultigroupANOVA",
                  "chapterA-formulas")
## The two urls used
urlBook <- "https://02323.compute.dtu.dk/filemanager/02323/sharelatex-public/"
urlSlides <- "http://www2.compute.dtu.dk/courses/introstat/"

## Function for downloading
download <- function(sourceurl, destdir, files){
  ## First create the directory
  dir.create(paste0("introstat/",destdir), showWarnings=FALSE, recursive=TRUE)
  ## Download the files to the directory
  for(i in 1:length(files)){
    print(paste("Now downloading file",i,"of",length(files)))
    print(url <- paste0(sourceurl,files[i]))
    print(destfile <- paste0("introstat/",destdir,files[i]))
    tryCatch(download.file(url=url,
                           destfile=destfile,
                           quiet=TRUE,
                           mode='wb'),
             error=function(e){NA})
  }
}

## Download the book and chapters download(urlBook, "",
download(urlBook, "", c("book-IntroStatistics.pdf", paste0(chapternames,".pdf")))

## Download the chapter R scripts
download(paste0(urlBook,"files/"), "book-scripts/", paste0(chapternames,".R")[-length(chapternames)])
download(paste0(urlSlides,"data/"), "book-scripts/", "studentheights.csv")

## Download the chapter exercise solutions
download(paste0(urlBook,"files/"), "book-solutions/", paste0("solutions-chapter",1:8,".pdf"))

## Download the 02323 slides
destdir <- "slides02323/"
url <- paste0(urlSlides,destdir)
download(url, destdir, paste0("week",1:13,".pdf"))
download(url, destdir, paste0("merged.pdf"))
download(url, destdir, paste0("week",1:13,"HA.pdf"))
download(url, destdir, paste0("week",1:13,".R"))
download(url, destdir, c("overviewSlide_02323.pdf","overblikSlide_02323.pdf"))

## Download the 02402 slides
destdir <- "slides02402/"
url <- paste0(urlSlides,destdir)
download(url, destdir, paste0("week",1:13,".pdf"))
download(url, destdir, paste0("merged.pdf"))
download(url, destdir, paste0("week",1:13,"HA.pdf"))
download(url, destdir, paste0("week",1:13,".R"))
download(url, destdir, "overblikSlide_02402.pdf")

## Download the 02402 English slides used in the podcasts
destdir <- "slides02402en/"
url <- paste0(urlSlides,destdir)
download(url, destdir, paste0("week",1:13,".pdf"))
download(url, destdir, paste0("week",1:13,"HA.pdf"))
download(url, destdir, paste0("week",1:13,".R"))
download(url, destdir, "overviewSlide_02402.pdf")

## Download the 02402 Danish slides used in the podcasts
destdir <- "slides02402da/"
url <- paste0(urlSlides,destdir)
download(url, destdir, paste0("uge",1:13,".pdf"))
download(url, destdir, paste0("uge",1:13,"HA.pdf"))
download(url, destdir, "overviewSlide_02402.pdf")

## Upload this script
## source("functions/introstatUploadScript.R")
## introstatUploadScript("downloadMaterial.R","")