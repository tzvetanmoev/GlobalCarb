############ GLOBAL CARBOON FOOTPRINT DISTRIBUTION GRAPH #######################

# rm(list = ls()) 	
options("scipen"=100, "digits"=4)

######## LOAD USEFUL PACKAGES ############

library(epade)
# install.packages("epade")
library(plotly)
# install.packages("plotly")
library(reshape)
# install.packages("reshape")
library(readxl)
# install.packages("dplyr")
library(dplyr)

######## LOAD RAW DATA #######################

# set to directory containing WID data
setwd("C:/Users/tzvet/Pictures/Kings MA/CORE RA/WIDcarbon") # set to github dir
WIDraw <- read.csv("wid_car_footprint_usd19_wide.csv")

# setwd("C:\Users\tzvet\Pictures\Kings MA\CORE RA\Intro app")
WIDraw <- WIDraw %>% 
  select(-age, -variable, -pop, -mean_income) %>%
  select(-total_population, total_population) %>%
  select(-country_code, country_code) %>%
  filter(year > 1989)
colnames(WIDraw) <- c("country", "year" ,"carb_incdec1", "carb_incdec2", 
                      "carb_incdec3", "carb_incdec4", "carb_incdec5", 
                      "carb_incdec6", "carb_incdec7", "carb_incdec8",
                      "carb_incdec9", "carb_incdec10", 
                      "population", "country_code")

# Merge income data for ranking of countries
WID_income <- read_xlsx("wid_pretax_usd21_wide.xlsx", col_names = TRUE)
WID_income <- WID_income %>% 
  select(full_name, year, mean_income) %>% 
  dplyr::rename(country = full_name, mean = mean_income)
WIDraw <- WIDraw %>%
  left_join(WID_income, by = c("country",  "year"))

# change ordering to what it was before
WIDraw <- WIDraw[,c(1:12, 15, 13, 14)]
WIDraw$country[WIDraw$country== "United Kingdom"] <- "UK"
WIDraw$country[WIDraw$country== "Russian Federation"] <- "Russia"
full <- WIDraw


######## EXCLUDE VARIOUS COUNTRIES ###########
# Drop all countries with less than 750,000 population - this is done year by 
# year so countries can 'grow into' the chart
full <- full[which(full$population>=750000),]

# Drop countries that have missing values
countries_to_drop <-  full %>% 
  group_by(country) %>% 
  count(country) %>% 
  filter(n < 26) %>% 
  select(-n)
countries_to_drop <- as.matrix(countries_to_drop)
#countries_to_drop[(length(countries_to_drop)+1):
#                  (length(countries_to_drop)+3)] <- c("United Arab Emirates")
full <-  full[ !  full$country %in% countries_to_drop, ]
# jack <- full

######## SET UP COUNTRY LABELS ####################

# Set which countries to label
count_label <- c("China", "India", "Nigeria", "Norway", "Russia",
                 "USA","UK", "Venezuela", "Indonesia", "Brazil", 
                 "South Africa")
# Countries to label: 
# BRICS: Brazil, Russia, India, China, South Agrica

# Assign labels
full$lab <- rep("")
for (i in 1:dim(full)[1]){
  if (is.element(full$country[i], count_label))
    full$lab[i] <- as.character(full$country[i])
}

# Set label colours
#full$labcol <-"#000000"
#for (i in 1:dim(full)[1]) {
#  if (full$lab[i] == "")
#    full$labcol[i] <- "FFFFFF"
#  }


####### SET UP COLOUR CODES WITH COUNTRIES ###########
#http://www.strangeplanet.fr/work/gradient-generator/index.php is a useful tool


### Option 1: background colours with a few countries highlighted

# Set which countries to highlight in a different colour
#count_high <- c("United States","United Kingdom","China","India")

# Non-highlighted country colour:
#col_back <- "#B9DFED"

# Non-highlighted country transparency (0-1):
#trans <- 0.1

# Highlighted country colour:
#col_high <- "F67F6A"

# Assign colours to countries
#full$col <- rep(col_back)
#for (i in 1:dim(full)[1]){
#  if (is.element(full$country[i], count_high))
#    full$col[i] <- col_high
#}

# Assign transparency 

#full$trans <- 1
#for (i in 1:dim(full)[1]){
#  if (full$col[i] == col_back)
#    full$trans[i] <- trans
#}

### Option 2: Spectrum of colours based on 1995 ranking

col_codes <- "#A60000 #A70300 #A80700 #A90A00 #AA0E00 #AB1200 #AC1500 #AD1900 #AE1D00 #AF2000 #B02400 #B12800 #B32B00 #B42F00 #B53300 #B63600 #B73A00 #B83D00 #B94100 #BA4500 #BB4800 #BC4C00 #BD5000 #BE5300 #C05700 #C15B00 #C25E00 #C36200 #C46600 #C56900 #C66D00 #C77000 #C87400 #C97800 #CA7B00 #CC7F00 #CD8300 #CE8600 #CF8A00 #D08E00 #D19100 #D29500 #D39900 #D49C00 #D5A000 #D6A300 #D7A700 #D9AB00 #DAAE00 #DBB200 #DCB600 #DDB900 #DEBD00 #DFC100 #E0C400 #E1C800 #E2CC00 #E3CF00 #E4D300 #E6D600 #E7DA00 #E8DE00 #E9E100 #EAE500 #EBE900 #ECEC00 #EDF000 #EEF400 #EFF700 #F0FB00 #F2FF00 #EEFD00 #EBFB00 #E7F900 #E4F700 #E0F500 #DDF300 #D9F200 #D6F000 #D2EE00 #CFEC00 #CCEA00 #C8E800 #C5E700 #C1E500 #BEE300 #BAE100 #B7DF00 #B3DD00 #B0DC00 #ADDA00 #A9D800 #A6D600 #A2D400 #9FD200 #9BD000 #98CF00 #94CD00 #91CB00 #8DC900 #8AC700 #87C500 #83C400 #80C200 #7CC000 #79BE00 #75BC00 #72BA00 #6EB900 #6BB700 #68B500 #64B300 #61B100 #5DAF00 #5AAE00 #56AC00 #53AA00 #4FA800 #4CA600 #48A400 #45A200 #42A100 #3E9F00 #3B9D00 #379B00 #349900 #309700 #2D9600 #299400 #269200 #239000 #1F8E00 #1C8C00 #188B00 #158900 #118700 #0E8500 #0A8300 #078100 #048000 #048000 #048000 #048000 #048000 #048000 #048000 #048000 #048000 #048000"
col_codes <- strsplit(col_codes, " ")
order_year <- full[which(full$year=="1995"), ] # Dataset for a single year
attach(order_year)
order_year <- order_year[order(mean),]
detach(order_year)
for (i in 1:dim(order_year)[[1]]){
  order_year$col[i] <- col_codes[[1]][i]
}
col_codes <- as.vector(col_codes[[1]])

vars <- c("country","col")
country_col <- order_year[vars] # this provides a dataframe of country names and associated colour

new_count_colname <- list() # these are used later in assigning colours to countries that are not present in the 1995 chart
new_count_col <- list()

####### CONVERT MONTHLY carb_incdec TO ANNUAL #############

# full$carb_incdec1 <- 12*full$carb_incdec1
# full$carb_incdec2 <- 12*full$carb_incdec2
# full$carb_incdec3 <- 12*full$carb_incdec3
# full$carb_incdec4 <- 12*full$carb_incdec4
# full$carb_incdec5 <- 12*full$carb_incdec5
# full$carb_incdec6 <- 12*full$carb_incdec6
# full$carb_incdec7 <- 12*full$carb_incdec7
# full$carb_incdec8 <- 12*full$carb_incdec8
# full$carb_incdec9 <- 12*full$carb_incdec9
# full$carb_incdec10 <- 12*full$carb_incdec10

####### SET UP TO LOOP OVER YEARS ##################
# Could also just list individual years, but need to alter export section at end of this script accordingly

st_year <- 1990 # first year
end_year <- 2019 # last year