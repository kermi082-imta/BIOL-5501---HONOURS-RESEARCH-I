library(tidyverse)
library(readxl)
library("readr")
library(ggplot2)
library(dplyr)

top30 <- read_csv("updatedtop30journals.csv")
# Download data set of the top 30 journals from overton database with their Journal Impact Factors (jif) into R. 