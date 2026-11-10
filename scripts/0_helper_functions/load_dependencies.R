if (!require("readxl")) {
  install.packages("readxl")
  library(readxl)
}

if (!require("rineq")) {
  if (!require("devtools")){
    install.packages("devtools")
  }
  devtools::install_github("kdevkdev/rineq")
  library(rineq)
}

if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require("countrycode")) {
  install.packages("countrycode")
  library(countrycode)
}

if (!require("writexl")) {
  install.packages("writexl")
  library(writexl)
}

if (!require("grid")) {
  install.packages("grid")
  library(grid)
}

if (!require("econocharts")) {
  if (!require("devtools")){
    install.packages("devtools")
  }
  devtools::install_github("R-CoderDotCom/econocharts")
  library(econocharts)
}

