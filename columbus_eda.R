# columbus_eda.R
## EDA on zip code data comparisons for Columbus, OH.
## Goal is to identify zip codes with differing characteristics
## and compare their COVID numbers

source("Scripts/functions.R")
library(readxl)

######################################
# Retrieve zip code to county dataset
######################################

zip_code <- read_csv("Data/zip_county.csv")
zip_code_col <- zip_code %>% filter(primary_city == "Columbus", state == "OH")
                                      
# This has 47 zip codes, on the excel file we have 51. 

######################################
# Retrieve ZIP geometry data
######################################

geometry_zip <- read_sf("Data/All_zips.shp", type = 6)
geometry_zip_col <- filter(geometry_zip, GEOID %in% zip_code_col$zip)

# The geometry zip only has 30 observations while zip_code_col has 47.

######################################
# Retrieve COVID case data for Columbus
# collected on 9/17
#####################################

col_covid <- read_excel("Data/columbus_covid.xlsx")

