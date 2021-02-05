# Rstudio Cheatsheet:---------------------

## https://github.com/rstudio/cheatsheets/raw/master/data-import.pdf

# Base R: ------------------------------

childs <- read.csv("Orginal Data/Childs_2020_Habitat.csv")

covidData <- read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv')

head(covidData)
str(covidData)

## Useful arguments in read.csv:
###  header = 
###  na.strings = ''
###  skip = 



# Packages: -------------------------------
library(readr) # Default package for loading with tidyverse
library(readxl) # used to read excel files

## Add Code:

readxl::read_excel("example.xls")


# Custom functions: ----------------------
## from: https://www.r-bloggers.com/2013/02/copying-data-from-excel-to-r-and-back/

#read from clipboard:
read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

dat <- read.excel()

#write to clipboard:

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(dat)

## This may have been fully superceded by library(copypasta) . I haven't used
##  this package yet but it probably does your clipboard to data pipeline now.
## LETS TRY IT!
library(datapasta)

datapasta::df_paste()

# RStudio User interface:-----------------------------------

### 1: Upper right panel, under "Environment", click "Import Dataset"
### 2: Lower right panel, under "Files", navigate to file and click.


