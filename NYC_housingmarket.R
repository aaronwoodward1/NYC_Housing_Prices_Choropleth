#Preliminary code

library("tidyverse")
library("dplyr")
library("tidyr")
library("ggplot2")
library("rvest")
library("janitor")
library("xml2")

manhattan_homesales_roll <- download.file(url = "https://www1.nyc.gov/assets/finance/downloads/pdf/rolling_sales/neighborhood_sales/2021/2021_manhattan_sales_prices.xlsx", destfile = "~Desktop", mode="wb")

head(manhattan_homesales_roll)


####Data cleaning###

temp = tempfile(fileext = ".xlsx")
dataURL <- "http://www1.nyc.gov/assets/finance/downloads/pdf/rolling_sales/neighborhood_sales/2021/2021_manhattan_sales_prices.xlsx"
download.file(dataURL, destfile=temp, mode='wb')
test <- readxl::read_excel(temp, sheet =1)
head(test)

manhattan_homesales_roll <- janitor::row_to_names(manhattan_homesales_roll, 6, remove_rows_above = TRUE) 
head(manhattan_homesales_roll)

manhattan_homesales_roll <- na.omit(manhattan_homesales_roll)
head(manhattan_homesales_roll)

########
manhattan_homesales_roll <- read.csv("rollingsales_manhattan.csv")
head(manhattan_homesales_roll)

manhattan_homesales_roll <- janitor::row_to_names(manhattan_homesales_roll, 4, remove_rows_above = TRUE) 
head(manhattan_homesales_roll)

summary(manhattan_homesales_roll)

#  as.Date('SALE DATE')

#######
#Need to install plot.ly and Socrata
df <- read.socrata(
  "https://data.cityofnewyork.us/resource/5ebm-myj7.json",
  app_token = "SAVvPY9DDYV1TmGl9fqF4bvIR",
  email     = "aaron.woodward1@gmail.com",
  password  = "Hudson@27")


devtools::install_github('rstudio/leaflet')
devtools::install_github('bhaskarvk/leaflet.extras')


library("tidyverse")
library("dplyr")
library("tidyr")
library("ggplot2")
library("rvest")
library("janitor")
library("xml2")
library("plotly")
library("RSocrata")
library("hablar")
library("reshape2")
library("geojsonsf")
library("sf")
library("leaflet")
library("sp")
library("maps")
library("rgdal")
library("geojsonio")
library("ggmap")
library("knitr")
library("osmdata")
library("rmarkdown")
library("testthat")
library("tidygeocoder")
library("nominatimlite")
library("leaflet.extras")




 
head(df)

df <- df %>% mutate_at(c('number_of_sales','lowest_sale_price','average_sale_price','median_sale_price',
                   'highest_sale_price'), as.numeric)

#EDA
summary(df) 

ggplot(data = melt(df), mapping = aes(x = value)) + 
  geom_histogram(bins=10) + facet_wrap(~variable, scales = 'free_x')

df_hoods <- read.socrata(
  "https://data.cityofnewyork.us/resource/q2z5-ai38.json",
  app_token = "SAVvPY9DDYV1TmGl9fqF4bvIR",
  email     = "aaron.woodward1@gmail.com",
  password  = "Hudson@27")



#library(devtools)
#install_github("edzer/sfr")

options(timeout = 1200) 
install.packages("sf")

PackageNames <- c("knitr","osmdata","rmarkdown","testthat","tibble","tidygeocoder","nominatimlite")

for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

#https://data.cityofnewyork.us/resource/q2z5-ai38.json

#NYC Neighborhood Polygons GeoJSON
#https://data-beta-nyc-files.s3.amazonaws.com/resources/35dd04fb-81b3-479b-a074-a27a37888ce7/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson?Signature=ePFks%2FsnyrtML3XTKKAVZWI2g54%3D&Expires=1650765481&AWSAccessKeyId=AKIAWM5UKMRH2KITC3QA
install.packages('geojsonR')
library(geojsonR)

nyc_hood_gjson_url_path <-"https://data-beta-nyc-files.s3.amazonaws.com/resources/35dd04fb-81b3-479b-a074-a27a37888ce7/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson?Signature=ePFks%2FsnyrtML3XTKKAVZWI2g54%3D&Expires=1650765481&AWSAccessKeyId=AKIAWM5UKMRH2KITC3QA"
#nyc_hood_boundaries <-FROM_GeoJson(url_file_string = nyc_hood_gjson_url_path)
nyc_hoods <- geojsonio::geojson_read("https://data-beta-nyc-files.s3.amazonaws.com/resources/35dd04fb-81b3-479b-a074-a27a37888ce7/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson?Signature=ePFks%2FsnyrtML3XTKKAVZWI2g54%3D&Expires=1650765481&AWSAccessKeyId=AKIAWM5UKMRH2KITC3QA", what = "sp")
nyc_hoods <- readOGR("https://data-beta-nyc-files.s3.amazonaws.com/resources/35dd04fb-81b3-479b-a074-a27a37888ce7/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson?Signature=ePFks%2FsnyrtML3XTKKAVZWI2g54%3D&Expires=1650765481&AWSAccessKeyId=AKIAWM5UKMRH2KITC3QA") 

#url <- "https://rstudio.github.io/leaflet/json/nycounties.geojson"
#nycounties <- geojson_read(url, parse = TRUE, what = "sp")
#nycounties <- geojson_read(url)

nycounties <- readOGR("https://rstudio.github.io/leaflet/json/nycounties.geojson") 
pal <- colorNumeric("viridis", NULL)

leaflet(nycounties) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(log10(pop)),
              label = ~paste0(county, ": ", formatC(pop, big.mark = ","))) %>%
  addLegend(pal = pal, values = ~log10(pop), opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x)))

########




#Reading in the .csv files for each borough
roll_sales_manhattan <- read.csv("rollingsales_manhattan.csv")
roll_sales_bronx <- read.csv("rollingsales_bronx.csv")
roll_sales_brooklyn <- read.csv("rollingsales_brooklyn.csv")
roll_sales_queens <- read.csv("rollingsales_queens.csv")
roll_sales_statenisland <- read.csv("rollingsales_statenisland.csv")


manhattan_df <- row_to_names(roll_sales_manhattan, 4, remove_rows_above = TRUE) 
bronx_df <- row_to_names(roll_sales_bronx, 4, remove_rows_above = TRUE) 
brooklyn_df <- row_to_names(roll_sales_brooklyn, 4, remove_rows_above = TRUE) 
queens_df <- row_to_names(roll_sales_queens, 4, remove_rows_above = TRUE) 
statenisland_df <- row_to_names(roll_sales_statenisland, 4, remove_rows_above = TRUE) 

df_list <- list(manhattan_df,bronx_df,brooklyn_df,queens_df,statenisland_df)
nyc_homesales_df <- rbind(manhattan_df,bronx_df,brooklyn_df,queens_df,statenisland_df)
tail(nyc_homesales_df)

nyc_homesales_df$BOROUGH[nyc_homesales_df$BOROUGH=="1"] <- "MANHATTAN"
nyc_homesales_df$BOROUGH[nyc_homesales_df$BOROUGH=="2"] <- "BRONX"
nyc_homesales_df$BOROUGH[nyc_homesales_df$BOROUGH=="3"] <- "BROOKLYN"
nyc_homesales_df$BOROUGH[nyc_homesales_df$BOROUGH=="4"] <- "QUEENS"
nyc_homesales_df$BOROUGH[nyc_homesales_df$BOROUGH=="5"] <- "STATEN ISLAND"





#manhattan_df <- row_to_names(roll_sales_manhattan, 4, remove_rows_above = TRUE) 

nyc_homesales_df$`SALE PRICE` <- as.numeric(gsub(",", "", nyc_homesales_df$`SALE PRICE`))
nyc_homesales_df$`RESIDENTIAL UNITS` <- as.numeric(gsub(",", "", nyc_homesales_df$`RESIDENTIAL UNITS`))
nyc_homesales_df$`COMMERCIAL UNITS` <- as.numeric(gsub(",", "", nyc_homesales_df$`COMMERCIAL UNITS`))
nyc_homesales_df$`TOTAL UNITS` <- as.numeric(gsub(",", "", nyc_homesales_df$`TOTAL UNITS`))
nyc_homesales_df$`LAND SQUARE FEET` <- as.numeric(gsub(",", "", nyc_homesales_df$`LAND SQUARE FEET`))
nyc_homesales_df$`GROSS SQUARE FEET` <- as.numeric(gsub(",", "", nyc_homesales_df$`GROSS SQUARE FEET`))

######Data Cleaning
#Removing Sale Price that are below $100,000
nyc_homesales_df <- nyc_homesales_df[!(nyc_homesales_df$`SALE PRICE`<100000),]
summary(nyc_homesales_df)
view(nyc_homesales_df)

#Removing non-residential sales
nyc_homesales_df <- nyc_homesales_df[!(nyc_homesales_df$`BUILDING CLASS CATEGORY`=="21 OFFICE BUILDINGS"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="22 STORE BUILDINGS"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="26 OTHER HOTELS"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="28 COMMERCIAL CONDOS"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="29 COMMERCIAL GARAGES"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="30 WAREHOUSES"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="31 COMMERCIAL VACANT LAND"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="33 EDUCATIONAL FACILITIES"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="43 CONDO OFFICE BUILDINGS"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="44 CONDO PARKING"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="46 CONDO STORE BUILDINGS"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="49 CONDO WAREHOUSES/FACTORY/INDUS"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="11 SPECIAL CONDO BILLING LOTS"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="32 HOSPITAL AND HEALTH FACILITIES"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="42 CONDO CULTURAL/MEDICAL/EDUCATIONAL/ETC"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="25 LUXURY HOTELS"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="45 CONDO HOTELS"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="48 CONDO TERRACES/GARDENS/CABANAS"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="27 FACTORIES"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="36 OUTDOOR RECREATIONAL FACILITIES"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="37 RELIGIOUS FACILITIES"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="38 ASYLUMS AND HOMES"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="41 TAX CLASS 4 - OTHER"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="34 THEATRES"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="40 SELECTED GOVERNMENTAL FACILITIES"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="05 TAX CLASS 1 VACANT LAND"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="06 TAX CLASS 1 - OTHER"|
                                         nyc_homesales_df$`BUILDING CLASS CATEGORY`=="35 INDOOR PUBLIC AND CULTURAL FACILITIES"),]
  
summary(nyc_homesales_df)
unique(nyc_homesales_df$`BUILDING CLASS CATEGORY`)
nyc_homesales_df <- nyc_homesales_df[!(nyc_homesales_df$`BUILDING CLASS CATEGORY`=="39 TRANSPORTATION FACILITIES"),]

#Removing apartment numbers from ADDRESS COLUMN
nyc_homesales_df$ADDRESS <- gsub(",.*","",nyc_homesales_df$ADDRESS)
view(nyc_homesales_df)

write_csv(nyc_homesales_df,"~..\\aaronwoodward\\Desktop\\nyc_homesales_df")

#Removing apartmen numbers from ADDRESS COLUMN
nyc_homesales_df$ADDRESS <- gsub(",.*","",nyc_homesales_df$ADDRESS)

#converting streets in an ordinal or sequential naming conventions
nyc_homesales_df$ADDRESS <- gsub("0 STREET","0TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("0 ST","0TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("1 STREET","1ST STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("1 ST","1ST STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("2 STREET","2ND STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("2 ST","2ND STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("3 STREET","3RD STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("3 ST","3RD STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("4 STREET","4TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("4 ST","4TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("5 STREET","5TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("5 ST","5TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("6 STREET","6TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("6 ST","6TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("7 STREET","7TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("7 ST","7TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("8 STREET","8TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("8 ST","8TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("9 STREET","9TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("9 ST","9TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("11 STREET","11TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("11 ST","11TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("12 STREET","12TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("12 ST","12TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("13 STREET","13TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("13 ST","13TH STREET",nyc_homesales_df$ADDRESS)


nyc_homesales_df$ADDRESS <- gsub("FIRST","1ST",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("SECOND","2ND",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("THIRD","3RD",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("FOURTH","4TH",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("FIFTH","5TH",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("SIXTH","6TH",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("SEVENTH","7TH",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("EIGHTH","8TH",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("NINTH","9TH",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("TENTH","10TH",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("ELEVENTH","11TH",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("TWELVETH","12TH",nyc_homesales_df$ADDRESS)

nyc_homesales_df$ADDRESS <- gsub("512 E 11ST STREET","512 E 11TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("408 WEST 25","408 WEST 25TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("201 WEST 16","201 WEST 16TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("532 WEST 22","532 WEST 22ND STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("555 WEST 23","555 WEST 23RD STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("133 WEST 22","133 WEST 22ND STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("360 WEST 22","360 WEST 22ND STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("47TH STREET MARKS PLACE","47 ST. MARK'S PLACE",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("27TH STREET MARKS PLACE","27 ST. MARK'S PLACE",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("107TH STREET MARKS PLACE","107 ST. MARK'S PLACE",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("87 SAINT MARKS PLACE","87 ST. MARK'S PLACE",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("12ND STREET","12TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("11ST STREET","11TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("15TH STREET MARKS PLACE","15 ST. MARK'S PLACE",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("WALL STREEET COURT","WALL STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("16 WEST 16","16 WEST 16TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("AVENUE OF THE AMER","AVENUE OF THE AMERICAS",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("10 MADISON SQUARE WEST","1107 BROADWAY",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("7 LEXINGTON AVENU","7 LEXINGTON AVENUE",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("N/A EAST 23RD STREET","10 EAST 23RD STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("225 EAST19TH","225 EAST 19TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("27 EAST 11ST STREET","27 EAST 11TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("BLEEKER","BLEECKER",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("ONE 5TH AVENUE","1 5TH AVENUE",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("LA GUARDIA","LAGUARDIA",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("13RD STREET","13TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("290 W 11TH","290 WEST 11TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("MORTON SQUARE","MORTON STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("13RD STREET","13TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("11-15TH STREET NICHOLAS AVENUE","11-15 ST. NICHOLAS AVENUE",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("FREDRICK DOUGLASS BL","FREDERICK DOUGLASS BLVD",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("FREDRICK DOUGLASS BL","FREDERICK DOUGLASS BLVD",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("FREDERICK DOUGLAS BLVD","FREDERICK DOUGLASS BLVD",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("112ND STREET","112TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("118 WEST 112","118 WEST 112TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("ADAM C POWELL BLVD","ADAM CLAYTON POWELL JR BLVD",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("118 WEST 112","118 WEST 112TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("258TH STREET NICHOLAS AVENUE","258 ST. NICHOLAS AVENUE",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("362ND STREET. NICHOLAS AVENUE","362 ST. NICHOLAS AVENUE",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("157 WEST 111ST STREET","157 WEST 111TH STREET",nyc_homesales_df$ADDRESS)
nyc_homesales_df$ADDRESS <- gsub("FREDERICK DOUGLASS B","FREDERICK DOUGLASS BLVD",nyc_homesales_df$ADDRESS)



#Retrieving geospatial coordinates for addresses 
nyc_address_coords <- geo_lite(paste0(nyc_homesales_df$ADDRESS,", ",nyc_homesales_df$BOROUGH,", NY ",nyc_homesales_df$`ZIP CODE`), lat = "lat", long = "lon", limit = 1)

head(nyc_address_coords)

geo_lite("47 ST. MARK'S PLACE, MANHATTAN, NY 10003")

#r <- GET('https://data-beta-nyc-files.s3.amazonaws.com/resources/35dd04fb-81b3-479b-a074-a27a37888ce7/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson?Signature=3N%2ByC8%2BAq59PIiO%2F4w%2BZYb3%2Fiqg%3D&Expires=1650859374&AWSAccessKeyId=AKIAWM5UKMRH2KITC3QA')
nyc_neighborhoods <- readOGR("https://data-beta-nyc-files.s3.amazonaws.com/resources/35dd04fb-81b3-479b-a074-a27a37888ce7/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson?Signature=xvf1EjzzGA7wuDBVldYCqgtTgdk%3D&Expires=1650859640&AWSAccessKeyId=AKIAWM5UKMRH2KITC3QA") 
summary(nyc_neighborhoods)

leaflet(nyc_neighborhoods) %>%
  addTiles() %>% 
  addPolygons(popup = ~neighborhood) %>%
  addProviderTiles("CartoDB.Positron")

nyc_address_coords <- geo_lite(nyc_homesales_df$ADDRESS, lat = "lat", long = "lon", limit = 1)












manhattan_df <- manhattan_df %>% mutate_at(c('RESIDENTIAL UNITS','COMMERCIAL UNITS','TOTAL UNITS','LAND SQUARE FEET',
                                             'GROSS SQUARE FEET', 'SALE PRICE'), as.numeric)

#manhattan_df$sale_date <- manhattan_df$`SALE DATE`
manhattan_df$`SALE DATE` <- as.Date(manhattan_df$`SALE DATE`, format = "%Y/%m/%d")

head(manhattan_df)
summary(manhattan_df)

nyc_nta <-read.socrata(
    "https://data.cityofnewyork.us/resource/q2z5-ai38.json",
    app_token = "SAVvPY9DDYV1TmGl9fqF4bvIR",
    email     = "aaron.woodward1@gmail.com",
    password  = "Hudson@27")

head(nyc_nta)

leaflet(nyc_nta) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              label = ~paste0(name, ": "))





nyc_hoods <- readOGR("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/Neighborhood_Names/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson") 
pal <- colorNumeric("viridis", NULL)

leaflet(nycounties) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(log10(pop)),
              label = ~paste0(Name, ": ", formatC(pop, big.mark = ","))) %>%
  addLegend(pal = pal, values = ~log10(pop), opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x)))

##############################
#Brooklyn

library(sf)
library(tidyverse)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(ggplot2)
library(rvest)
library(lubridate)
library(rjson)
library(stringr)
library(janitor)
library(plotly)
library(RSocrata)
library(rNYCclean)
library(reshape2)
library(splitstackshape)
library(tidygeocoder)


devtools::install_github("gmculp/rNYCclean", force = TRUE)


nyc_housing_mkt_df <- read.socrata(
  "https://data.cityofnewyork.us/resource/5ebm-myj7.json",
  app_token = "SAVvPY9DDYV1TmGl9fqF4bvIR",
  email     = "aaron.woodward1@gmail.com",
  password  = "Hudson@27")

#Set working directory
setwd("~aaronwoodward/Desktop")

#Loading property sales data
brooklyn_df <- read.csv("rollingsales_brooklyn.csv")

#Cleaning property sales data
brooklyn_df <- janitor::row_to_names(brooklyn_df, 4, remove_rows_above = TRUE) 
head(brooklyn_df)

#Renaming variables:
brooklyn_df <- brooklyn_df %>% 
  rename(SALE_PRICE = `SALE PRICE`, 
         RESIDENTIAL_UNITS = `RESIDENTIAL UNITS`,
         COMMERCIAL_UNITS = `COMMERCIAL UNITS`,
         TOTAL_UNITS = `TOTAL UNITS`,
         LAND_SQ_FT = `LAND SQUARE FEET`,
         GROSS_SQ_FT = `GROSS SQUARE FEET`,
         SALE_DATE = `SALE DATE`,
         APT_NUMBER = `APARTMENT NUMBER`,
         ZIP_CODE = `ZIP CODE`,
         YEAR_BUILT = `YEAR BUILT`,
         TAX_CLASS_AT_PRESENT = `TAX CLASS AT PRESENT`,
         TAX_CLASS_AT_TOS = `TAX CLASS AT TIME OF SALE`,
         BUILDING_CLASS_AT_PRESENT = `BUILDING CLASS AT PRESENT`,
         BUILDING_CLASS_AT_TOS = `BUILDING CLASS AT TIME OF SALE`,
         BUILDING_CLASS_CATEGORY = `BUILDING CLASS CATEGORY`,
         BOROUGH_CODE = BOROUGH)

brooklyn_df <- brooklyn_df %>% 
  mutate(BOROUGH = "BROOKLYN")

colnames(brooklyn_df)

#Changing Data types
brooklyn_df$SALE_PRICE <- as.numeric(gsub(",", "", brooklyn_df$SALE_PRICE))
brooklyn_df$RESIDENTIAL_UNITS <- as.numeric(gsub(",", "", brooklyn_df$RESIDENTIAL_UNITS))
brooklyn_df$COMMERCIAL_UNITS <- as.numeric(gsub(",", "", brooklyn_df$COMMERCIAL_UNITS))
brooklyn_df$TOTAL_UNITS <- as.numeric(gsub(",", "", brooklyn_df$TOTAL_UNITS))
brooklyn_df$LAND_SQ_FT <- as.numeric(gsub(",", "", brooklyn_df$LAND_SQ_FT))
brooklyn_df$GROSS_SQ_FT <- as.numeric(gsub(",", "", brooklyn_df$GROSS_SQ_FT))
brooklyn_df$SALE_DATE <- mdy(brooklyn_df$SALE_DATE)
#brooklyn_df$SALE_MONTH <- as.Date(brooklyn_df$SALE_MONTH,"%m/%d/%y")

str(brooklyn_df)

#Removing records that have sale prices <$100,000
brooklyn_df <- brooklyn_df[!(brooklyn_df$SALE_PRICE<50000),]

summary(brooklyn_df)

#Removing non-residential and rental property sales by subsetting BUILDING_CLASS_CATEGORY
brooklyn_home_sales_df <- subset(brooklyn_df, BUILDING_CLASS_CATEGORY == "09 COOPS - WALKUP APARTMENTS" | 
                                   BUILDING_CLASS_CATEGORY == "10 COOPS - ELEVATOR APARTMENTS" | 
                                   BUILDING_CLASS_CATEGORY == "12 CONDOS - WALKUP APARTMENTS" | 
                                   BUILDING_CLASS_CATEGORY == "13 CONDOS - ELEVATOR APARTMENTS" | 
                                   BUILDING_CLASS_CATEGORY =="17 CONDO COOPS" | BUILDING_CLASS_CATEGORY=="01 ONE FAMILY DWELLINGS" )




#Cleaning Addresses
# Separating the apartment numbers from addresses
brooklyn_home_sales_df$ADDRESS <- gsub(",.*","",brooklyn_home_sales_df$ADDRESS)

# get version of DCP PAD used to build rNYCclean package data
rNYCclean::pad_version

system.time({brooklyn_home_sales_df <- pad_addr(brooklyn_home_sales_df,"ADDR.pad","ADDRESS","BOROUGH_CODE","boro_code")})

#Getting clean addresses
brooklyn_home_sales_df <- brooklyn_home_sales_df %>%
  mutate(ADDRESS_CLEAN = ifelse(is.na(ADDR.pad),ADDRESS,ADDR.pad))


#Putting together full addresses
brooklyn_home_sales_df <- brooklyn_home_sales_df %>%
  mutate(FULL_ADDR = paste0(ADDRESS_CLEAN,", ",BOROUGH,", NY, ",ZIP_CODE))

###Geocoding using the tidygeocoder R package to get latitude and longitude for all addresses
install.packages('tidygeocoder')

brooklyn_home_sales_df <- brooklyn_home_sales_df %>% 
  geocode(FULL_ADDR, method = 'osm', lat = latitude , long = longitude)

sum(is.na(brooklyn_home_sales_df$latitude))

#Missing lat/lon coordinates for 412 (9%) addresses

#Creating a dataframe of addresses that do not have coords.

bklyn_addr_na <- brooklyn_home_sales_df[is.na(brooklyn_home_sales_df$latitude),]

#Cleaning addresses that the tidygeocoder did not return coords.
bklyn_addr_na$FULL_ADDR <- gsub("4 AVENUE","4TH AVENUE",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("1030 E 32","1030 EAST 32ND STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 3 ST","EAST 3RD STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("57 STREET","57TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("29 STREET","29TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("13 STREET","13TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("8 STREET","8TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("42 STREET","42ND STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("HYMEN","HYMAN",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("78 STREET","78TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 10 ST","EAST 10TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("98 STREET","98TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("23 STREET","23RD STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("54 STREET","54TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("31 STREET","31ST STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("32 STREET","32ND STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("49 STREET","49TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 46 STREET","EAST 46TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 53 STREET","EAST 53RD STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("77 STREET","77TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("7 STREET","7TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("28 STREET","28TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("104 STREET","104TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("45 STREET","45TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("100 STREET","100TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("19 STREET","19TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("101 STREET","101ST STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("10 STREET","10TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("102 STREET","102ND STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("35 STREET","35TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("51 STREET","51ST STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("56 STREET","56TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("103 STREET","103RD STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("4 STREET","4TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("5 STREET","5TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 102ND STREET","EAST 102ND STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("11 STREET","11TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("PROSPECT PARK S.W.","PROSPECT PARK SW",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("76 STREET","76TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("46 STREET","46TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("2 STREET","2ND STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("54 STREET","54TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("39 STREET","39TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("70 STREET","70TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("10 LANE","10TH LANE",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("48 STREET","48TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("18 STREET","18TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("81 STREET","81ST STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("22 STREET","22ND STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("1 STREET","1ST STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("66 STREET","66TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("15 STREET","15TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("STRET","STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 22ND","EAST 22ND STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("37 STREET","37TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 53RD STREET","EAST 53RD STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("14 STREET","14TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("21 STREET","21ST STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("12 STREET","12TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("36 STREET","36TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("34 STREET","34TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("82 STREET","82ND STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("3 STREET","3RD STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 12 STREET","EAST 12TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("1913 49TH ROAD","866 DAHILL ROAD",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("1925 49TH ROAD","866 DAHILL ROAD",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("1935 49TH ROAD","866 DAHILL ROAD",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("33 STREET","33RD STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("65 STREET","65TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("1 NORTH PIER","4 NORTH 5TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("1 NORTHSIDE PIERS","4 NORTH 5TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 8TH STREET","EAST 8TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E58TH ST","EAST 58TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("24 STREET","24TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("26 STREET","26TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("38 STREET","38TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("N 6 STREET","NORTH 6TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("N 6TH","NORTH 6TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("72 STREET","72ND STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("71 STREET","71ST STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("62 STREET","62ND STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("69 STREET","69TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("16 STREET","16TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 7TH STREET","EAST 7TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("5 LANE","5TH LANE",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("13 STREET","13TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 27TH ST","EAST 27TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 28TH ST","EAST 28TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("ST. JOHN'S PLACE","ST. JOHNS PLACE",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("96 STREET","96TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("17 STREET","17TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("MC KINLEY","MCKINLEY",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("374-76 SOUTH 2ND STREET","374 SOUTH 2ND STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("39 NORTH 4TH PLACE","178 KENT AVENUE",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("FIRST ROAD","1ST ROAD",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("1 ROAD","1ST ROAD",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("5116-5120 TILDEN AVE","5116 TILDEN AVENUE",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("CELESTE","CELEST",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("MAC DONOUGH","MACDONOUGH",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("6 AVENUE","6TH AVENUE",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 17TH ST","EAST 17TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("6 WYTHE LANE","51 SOUTH 4TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("7019 RIDGE CREST TERRACE","6924 RIDGE BOULEVARD",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 40 STREET","EAST 40TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("15 AVENUE","15TH AVENUE",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("5 AVENUE","5TH AVENUE",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 55 ST","EAST 55TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("13 AVENUE","13TH AVENUE",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("85-101 N 3RD","85 NORTH 3RD STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("BAY16TH STREET","BAY 16TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("18 AVENUE","18TH AVENUE",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("8 STREET","8TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("88 STREET","88TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("94 STREET","94TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("N/A PROSPECT PLACE","906 PROSPECT PLACE",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("9 STREET","9TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("40 STREET","40TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 46 STREET","EAST 46TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("12ND STREET","12TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("BRIGHTONORTH 6TH STREET","BRIGHTON 6TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("SHORE PKWAY","SHORE PARKWAY",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("99 STREET","99TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("9 AVENUE","9TH AVENUE",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("8 AVENUE","8TH AVENUE",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("3 COURT","3RD COURT",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("EAST 17TH STREETREET","EAST 17TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 7TH STREET","EAST 7TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 8 ST","EAST 8TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 28 ST","EAST 28TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 17 ST","EAST 17TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 27 ST","EAST 27TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 46TH","EAST 46TH STREET",bklyn_addr_na$FULL_ADDR)
bklyn_addr_na$FULL_ADDR <- gsub("E 24 ST","EAST 24TH STREET",bklyn_addr_na$FULL_ADDR)

bklyn_addr_na <- bklyn_addr_na %>% 
  geocode(FULL_ADDR, method = 'osm', lat = latitude , long = longitude)

bklyn_addr_na <- subset(bklyn_addr_na, select = -c(latitude...28,longitude...29))
bklyn_addr_na <- bklyn_addr_na %>%
  rename(latitude = latitude...30,
         longitude = longitude...31)


sum(is.na(bklyn_addr_na$latitude))

#Need to only clean 6 more addresses, update in both dataframes
bklyn_addr_na_2 <- bklyn_addr_na[is.na(bklyn_addr_na$latitude),]

bklyn_addr_na_2$FULL_ADDR <- gsub("4 AVENUE","4TH AVENUE",bklyn_addr_na_2$FULL_ADDR)

bklyn_addr_na_2 <- bklyn_addr_na_2 %>% 
  geocode(FULL_ADDR, method = 'osm', lat = latitude , long = longitude)

bklyn_addr_na_2 <- subset(bklyn_addr_na_2, select = -c(latitude...28,longitude...29))
bklyn_addr_na_2 <- bklyn_addr_na_2 %>%
  rename(latitude = latitude...30,
         longitude = longitude...31)

#Joining the dataframes that had no coords
bklyn_addr_na_joined1 <- full_join(bklyn_addr_na, bklyn_addr_na_2)
bklyn_addr_na_joined1 <- bklyn_addr_na_joined1 %>%
  filter(!is.na(latitude), !is.na(longitude))

sum(is.na(bklyn_addr_na_joined1$latitude))

##Joining datasets
#bklyn_addr_na_joined1 <- full_join(bklyn_addr_na, bklyn_addr_na_2)

brooklyn_df <- full_join(brooklyn_home_sales_df, bklyn_addr_na_joined1)
sum(is.na(merged_df$latitude))

brooklyn_df <- merged_df %>%
  filter(!is.na(latitude), !is.na(longitude))

sum(is.na(brooklyn_df$latitude))

#All addresses now have coords, we need to convert these coords into geometries.
#merged_df1 <- merged_df %>%
#  mutate(GEOMETRY = st_as_sf(merged_df, coords = c("longitude", "latitude"), crs=4326, 
#                             agr = "constant"))

brooklyn_df <- st_as_sf(brooklyn_df, coords = c(x = "longitude", y = "latitude"), crs = 4326)
nyc_nta_sf <- st_read(dsn  = "./2020 Neighborhood Tabulation Areas (NTAs) - Tabular/geo_export_1183c9a2-33e9-4c9c-9c60-b600cbed4e05.shp")
nyc_nta_sf <- st_transform(nyc_nta_sf, 4326)


#nyc_neighborhoods_sf <- as_Spatial(nyc_nta_df$the_geom.coordinates)

#nyc_neighborhoods_sf <- geojson_sf(nyc_nta_df$the_geom.coordinates, expand_geometries = FALSE, crs = 4326)
#nyc_neighborhoods_sf <- st_sf(nyc_nta_df, sf_column_name = "the_geom.coordinates", crs = 4236)
#merged_df1 <- st_sfc(st_point(merged_df, coords = c("longitude", "latitude"), crs=4326))

brooklyn_sf <- st_join(brooklyn_df, nyc_nta_sf["ntaname"])
str(brooklyn_sf)

####
#Aggregating Monthly Home Sales Data by Neighborhood

brooklyn_sf$SALES_YEAR <- strftime(brooklyn_sf$SALE_DATE, "%Y")
brooklyn_sf$SALES_MONTH <- strftime(brooklyn_sf$SALE_DATE, "%m")


#Creating value metric "price_per_sqft" - price per square footage.
brooklyn_sf$PRICE_PER_SQFT <- brooklyn_sf$SALE_PRICE / brooklyn_sf$GROSS_SQ_FT


#Aggregate sale metrics by month for each neighborhood "ntaname"

#Using lubridate package to pair sales with month and year of sale
brooklyn_sf$SALE_YEAR_MONTH <- floor_date(brooklyn_sf$SALE_DATE, "month")

brooklyn_monthly_medsales <- brooklyn_sf[,c("BOROUGH","ntaname", "SALE_YEAR_MONTH", "SALE_PRICE", "PRICE_PER_SQFT")] 

#brooklyn_monthly_medsales$MEDIAN_SALE_PRICE <- aggregate(SALE_PRICE ~ SALE_YEAR_MONTH + ntaname, data = brooklyn_monthly_medsales, FUN = median)

brooklyn_monthly_medsales <- brooklyn_monthly_medsales %>% 
  group_by(ntaname, SALE_YEAR_MONTH) %>%
  summarise(MEDIAN_SALE_PRICE = median(SALE_PRICE),
            MEDIAN_PRICE_PER_SQFT = median(PRICE_PER_SQFT),
            MAX_SALE_PRICE = max(SALE_PRICE),
            MIN_SALE_PRICE = min(SALE_PRICE)
  )



#########################

#Loading required libraries
library(tidyverse)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(ggplot2)
library(rvest)
library(lubridate)
library(rjson)
library(stringr)
library(janitor)
library(plotly)
library(RSocrata)
library(rNYCclean)
library(reshape2)
library(splitstackshape)
library(tidygeocoder)
library(sf)


#Manhattan

#Set working directory
setwd("~aaronwoodward/Desktop")

#Loading property sales data
manhattan_df <- read.csv("rollingsales_manhattan.csv")

#Cleaning property sales data
manhattan_df <- janitor::row_to_names(manhattan_df, 4, remove_rows_above = TRUE) 
head(manhattan_df)
colnames(manhattan_df)

#Renaming variables:
manhattan_df <- manhattan_df %>% 
  rename(SALE_PRICE = ` SALE PRICE `, 
         RESIDENTIAL_UNITS = `RESIDENTIAL UNITS`,
         COMMERCIAL_UNITS = `COMMERCIAL UNITS`,
         TOTAL_UNITS = `TOTAL UNITS`,
         LAND_SQ_FT = `LAND SQUARE FEET`,
         GROSS_SQ_FT = `GROSS SQUARE FEET`,
         SALE_DATE = `SALE DATE`,
         APT_NUMBER = `APARTMENT NUMBER`,
         ZIP_CODE = `ZIP CODE`,
         YEAR_BUILT = `YEAR BUILT`,
         TAX_CLASS_AT_PRESENT = `TAX CLASS AT PRESENT`,
         TAX_CLASS_AT_TOS = `TAX CLASS AT TIME OF SALE`,
         BUILDING_CLASS_AT_PRESENT = `BUILDING CLASS AT PRESENT`,
         BUILDING_CLASS_AT_TOS = `BUILDING CLASS AT TIME OF SALE`,
         BUILDING_CLASS_CATEGORY = `BUILDING CLASS CATEGORY`,
         BOROUGH_CODE = BOROUGH)

manhattan_df <- manhattan_df %>% 
  mutate(BOROUGH = "MANHATTAN")

#Changing Data types
manhattan_df$SALE_PRICE <- as.numeric(gsub(",", "", manhattan_df$SALE_PRICE))
manhattan_df$RESIDENTIAL_UNITS <- as.numeric(gsub(",", "", manhattan_df$RESIDENTIAL_UNITS))
manhattan_df$COMMERCIAL_UNITS <- as.numeric(gsub(",", "", manhattan_df$COMMERCIAL_UNITS))
manhattan_df$TOTAL_UNITS <- as.numeric(gsub(",", "", manhattan_df$TOTAL_UNITS))
manhattan_df$LAND_SQ_FT <- as.numeric(gsub(",", "", manhattan_df$LAND_SQ_FT))
manhattan_df$GROSS_SQ_FT <- as.numeric(gsub(",", "", manhattan_df$GROSS_SQ_FT))
manhattan_df$SALE_DATE <- mdy(manhattan_df$SALE_DATE)
#manhattan_df$SALE_MONTH <- as.Date(manhattan_df$SALE_MONTH,"%m/%d/%y")

str(manhattan_df)

#Removing records that have sale prices <$100,000
manhattan_df <- manhattan_df[!(manhattan_df$SALE_PRICE<150000),]

summary(manhattan_df)

#Removing non-residential and rental property sales by subsetting BUILDING_CLASS_CATEGORY
manhattan_df <- subset(manhattan_df, BUILDING_CLASS_CATEGORY == "09 COOPS - WALKUP APARTMENTS" | 
                                   BUILDING_CLASS_CATEGORY == "10 COOPS - ELEVATOR APARTMENTS" | 
                                   BUILDING_CLASS_CATEGORY == "12 CONDOS - WALKUP APARTMENTS" | 
                                   BUILDING_CLASS_CATEGORY == "13 CONDOS - ELEVATOR APARTMENTS" | 
                                   BUILDING_CLASS_CATEGORY =="17 CONDO COOPS" | BUILDING_CLASS_CATEGORY=="01 ONE FAMILY DWELLINGS" )

#Cleaning Addresses
# Separating the apartment numbers from addresses
manhattan_df$ADDRESS <- gsub(",.*","",manhattan_df$ADDRESS)

# get version of DCP PAD used to build rNYCclean package data
rNYCclean::pad_version

system.time({manhattan_df <- pad_addr(manhattan_df,"ADDR.pad","ADDRESS","BOROUGH_CODE","boro_code")})

#Getting clean addresses
manhattan_df <- manhattan_df %>%
  mutate(ADDRESS_CLEAN = ifelse(is.na(ADDR.pad),ADDRESS,ADDR.pad))


#Fixing street and avenue names of addresses that are in rNYCclean 
manhattan_df$ADDRESS_CLEAN <- gsub("5 AVENUE","5TH AVENUE",manhattan_df$ADDRESS_CLEAN)
manhattan_df$ADDRESS_CLEAN <- gsub("5TH AV","5TH AVENUE",manhattan_df$ADDRESS_CLEAN)
manhattan_df$ADDRESS_CLEAN <- gsub("10 AVENUE","10TH AVENUE",manhattan_df$ADDRESS_CLEAN)
manhattan_df$ADDRESS_CLEAN <- gsub("45-53 WEST 110TH STREET","53 WEST 110TH STREET",manhattan_df$ADDRESS_CLEAN)
manhattan_df$ADDRESS_CLEAN <- gsub("W 77","WEST 77TH STREET",manhattan_df$ADDRESS_CLEAN)
manhattan_df$ADDRESS_CLEAN <- gsub("N. MOORE STREET","NORTH MOORE STREET",manhattan_df$ADDRESS_CLEAN)
manhattan_df$ADDRESS_CLEAN <- gsub("N MOORE STREET","NORTH MOORE STREET",manhattan_df$ADDRESS_CLEAN)
manhattan_df$ADDRESS_CLEAN <- gsub("137 FRANKLIN STREET","25 NORTH MOORE STREET",manhattan_df$ADDRESS_CLEAN)
manhattan_df$ADDRESS_CLEAN <- gsub("123-25 EAST 102ND STREET","125 EAST 102ND STREET",manhattan_df$ADDRESS_CLEAN)

#Putting together full addresses
manhattan_df <- manhattan_df %>%
  mutate(FULL_ADDR = paste0(ADDRESS_CLEAN,", ",BOROUGH,", NY, ",ZIP_CODE))

#W 77,N. MOORE STREET
###Geocoding using the tidygeocoder R package to get latitude and longitude for all addresses
#install.packages('tidygeocoder')

manhattan_df <- manhattan_df %>% 
  geocode(FULL_ADDR, method = 'osm', lat = latitude , long = longitude)

sum(is.na(manhattan_df$latitude))

#Missing lat/lon coordinates for 412 (9%) addresses

#Creating a dataframe of addresses that do not have coords.

manh_addr_na <- manhattan_df[is.na(manhattan_df$latitude),]
manh_addr_na_unique <- data.frame(unique(manh_addr_na$FULL_ADDR))

#Cleaning addresses that the tidygeocoder did not return coords.
manh_addr_na$FULL_ADDR <- gsub("15 STREET","15TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("117 STREET","117TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("75 STREET","75TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("122 STREET","122ND STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("91TH STREET","91ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("138 STREET","138TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("10 MADISON SQUARE WEST","1107 BROADWAY",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("66 STREET","66TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 30TH ST","EAST 30TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("113 STREET","113TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("83 STREET","83RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("72 STREET","72ND STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("68 STREET","68TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("123 STREET","123RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("60 STREET","60TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("71 STREET","71ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("112 STREET","112TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("29 STREET","29TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("W 89","WEST 89TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("104 STREET","104TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("79 STREET","79TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("81 STREET","81ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("W 58","WEST 58TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("20 STREET","20TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("82 STREET","82ND STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("37 STREET","37TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("63 STREET","63RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("STRET","STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("96 STREET","96TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("138 STREET","138TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("17 STREET","17TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("W.67 STREET","WEST 67TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("83 STREET","83RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("28 STREET","28TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("35 STREET","35TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("WEST 22","WEST 22ND STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("22 STREET","22ND STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("52 STREET","52ND STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("138-140 WEST 124 STREET","138 WEST 124TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("40 STREET","40TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("3 STREET","3RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("13 STREET","13TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 16TH ST","EAST 16TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("84 STREET","84TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("48 STREET","48TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("23 STREET","23RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("4 STREET","4TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 69 STREET","EAST 69TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("27 STREET","27TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("61 STREET","61ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("77 STREET","77TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("93 STREET","93RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("73 STREET","73RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("W 71","WEST 71ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("11 STREET","11TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("126 STREET","126TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("118 STREET","118TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("53 STREET","53RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 26 STREET","EAST 26TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("30 STREET","30TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("26 STREET","26TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("91 STREET","91ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("110 STREET","110TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("W 15 STREET","WEST 15TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("133 STREET","133RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("32 STREET","32ND STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 67 ST","EAST 67TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("87 STREET","87TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("90 STREET","90TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("W 23 ST","WEST 23RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("131 STREET","131ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("74 STREET","74TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("79 STREET","79TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("13 STREET","13TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("80 STREET","80TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("54 STREET","54TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("ADAM C. POWELL BOULEVARD","ADAM CLAYTON POWELL JR BOULEVARD",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("1867-69 7TH AVENUE","1867 7TH AVENUE",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("70 STREET","70TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("76 STREET","76TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("101 STREET","101ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 75 ST","EAST 75TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("1 MORTON SQUARE","1 MORTON STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("1 WALL STREET COURT","1 WALL STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 89 STREET","EAST 89TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("W 86","WEST 86TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("109 STREET","109TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("21 STREET","21ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("W 21 STREET","WEST 21ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("89 STREET","89TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 22 STREET","EAST 22ND STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 63 ST","EAST 63RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 78 STREET","EAST 78TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("124 STREET","124TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("120 STREET","120TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("51 STREET","51ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("W 64 ST","WEST 64TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 35TH ST","EAST 35TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 80 STREET","EAST 80TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("24 STREET","24TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("105 STREET","105TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("57 STREET","57TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("111 STREET","111TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("148 STREET","148TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("50 STREET","50TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("14 STREET","14TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("7 STREET","7TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("EAST19TH","EAST 19TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("34 STREET","34TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("36 STREET","36TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("25 STREET","25TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 28 STREET","EAST 28TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 29 ST","EAST 29TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("W 105 ST","WEST 105TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 6 ST","EAST 6TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("86 STREET","86TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("108 STREET","108TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 14 ST","EAST 14TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("21 STREET`","21ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 22 ST","EAST 22ND STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 30 STREET","EAST 30TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("10 STREET","10TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("46 STREET","46TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("47 STREET","47TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 93 ST","EAST 93RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("51TH STREET","51ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("123 STREET","123RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("64 STREET","64TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("112 STREET","112TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("17 STREET","17TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("107 STREET","107TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("FIFTH AVENUE","5TH AVENUE",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("137 STREET","137TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 74 ST","EAST 74TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("59 STREET","59TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("85 STREET","85TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("110 STREET","110TH STREET",manh_addr_na$FULL_ADDR)
#manh_addr_na$FULL_ADDR <- gsub("122 STREET","122ND STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("135 STREET","135TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("145 STREET","145TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("45 STREET","45TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("62 STREET","62ND STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("78 STREET","78TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("118 STREET","118TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("88 STREET","88TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("W 66 ST","WEST 66TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("66 STREET","66TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("304-324 EAST 41ST STREET","304 EAST 41ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("65 STREET","65TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("150 STREET","150TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("49 STREET","49TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("52 STREET","52ND STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("100 STREET","100TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E.56 ST","EAST 56TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 79 ST","EAST 79TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("68 STREET","68TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("70 STREET","70TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("18 STREET","18TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("42D STREET","42ND STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("42 STREET","42ND STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("12 STREET","12TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 57","EAST 57TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("38 STREET","38TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("43 STREET","43RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("55 STREET","55TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("EAST55TH STREET","EAST 55TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("91 STREET","91ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("58 STREET","58TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E. 93 ST","EAST 93RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E93ST","EAST 93RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("119 STREET","119TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("44 STREET","44TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("89 STREET","89TH STREET",manh_addr_na$FULL_ADDR)
#manh_addr_na$FULL_ADDR <- gsub("W 23 ST","WEST 23RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("9 STREET","9TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("W 21 ST","WEST 21ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("123RD FLOOR","123RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("EAST72ND STREET","EAST 72ND STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("W 49 ST","WEST 49TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("56 STREET","56TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 90 ST","EAST 90TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("4-10 WEST 101 STREET","4 WEST 101ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 53 STREET","EAST 53RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 57 ST","EAST 57TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 59 ST","EAST 59TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 51 ST","EAST 51ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("51 STREET","51ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("141 STREET","141ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("W 37 ST","WEST 37TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("FRANKLIN D ROOSEVELT DRIVE","FDR DRIVE",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("152 STREET","152ND STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("167 STREET","167TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("132 STREET","132ND STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("LA GUARDIA PLACE","LAGUARDIA PLACE",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("4 MORTON SQUARE","100 MORTON STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("W 123","WEST 123RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 89 ST","EAST 89TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 11 ST","EAST 11TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("158 STREET","158TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("W 42 STREET","WEST 42ND STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("147 STREET","147TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("131 STREET","131ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("159 STREET","159TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("W 114","WEST 114TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("187 STREET","187TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("W114TH STREET","WEST 114TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("114 STREET","114TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("207 STREET","207TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("106 STREET","106TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("MAC DOUGAL","MACDOUGAL",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("W 138","WEST 138TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("181 STREET","181ST STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 35 ST","EAST 35TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("ST MARKS PLACE,","ST. MARK'S PLACE",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("ST. MARKS PLACE","ST. MARK'S PLACE",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("E 96 STREET","EAST 96TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("190 STREET","190TH STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("N/A EAST 23RD STREET","100 EAST 23RD STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("ONE FIFTH AVENUE","1 5TH AVENUE",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("ONE WALL STREET COURT","1 WALL STREET",manh_addr_na$FULL_ADDR)
manh_addr_na$FULL_ADDR <- gsub("AVENUEENUE","AVENUE",manh_addr_na$FULL_ADDR)


manh_addr_na <- manh_addr_na %>% 
  geocode(FULL_ADDR, method = 'osm', lat = latitude , long = longitude)

manh_addr_na <- subset(manh_addr_na, select = -c(latitude...28,longitude...29))
manh_addr_na <- manh_addr_na %>%
  rename(latitude = latitude...30,
         longitude = longitude...31)


sum(is.na(manh_addr_na$latitude))



#Need to only clean 6 more addresses, update in both dataframes
manh_addr_na_2 <- manh_addr_na[is.na(manh_addr_na$latitude),]

manh_addr_na_2$FULL_ADDR <- gsub("E 30 ST","EAST 30TH STREET",manh_addr_na_2$FULL_ADDR)
manh_addr_na_2$FULL_ADDR <- gsub("STREET ST","STREET",manh_addr_na_2$FULL_ADDR)
manh_addr_na_2$FULL_ADDR <- gsub("W. 67","WEST 67TH STREET",manh_addr_na_2$FULL_ADDR)
manh_addr_na_2$FULL_ADDR <- gsub("STREET STREET","STREET",manh_addr_na_2$FULL_ADDR)
manh_addr_na_2$FULL_ADDR <- gsub("13RD STREET","13TH STREET",manh_addr_na_2$FULL_ADDR)
manh_addr_na_2$FULL_ADDR <- gsub("16 STREET","16TH STREET",manh_addr_na_2$FULL_ADDR)
manh_addr_na_2$FULL_ADDR <- gsub("E 89","EAST 89TH STREET",manh_addr_na_2$FULL_ADDR)
manh_addr_na_2$FULL_ADDR <- gsub("W 60 ST","WEST 60TH STREET",manh_addr_na_2$FULL_ADDR)
manh_addr_na_2$FULL_ADDR <- gsub("W 111 ST","WEST 111TH STREET",manh_addr_na_2$FULL_ADDR)
manh_addr_na_2$FULL_ADDR <- gsub("E 80 ST","EAST 80TH STREET",manh_addr_na_2$FULL_ADDR)
manh_addr_na_2$FULL_ADDR <- gsub("E 72 ST","EAST 72ND STREET",manh_addr_na_2$FULL_ADDR)
manh_addr_na_2$FULL_ADDR <- gsub("ONE 5TH AVENUE","1 5TH AVENUE",manh_addr_na_2$FULL_ADDR)
manh_addr_na_2$FULL_ADDR <- gsub("STREETREET,","STREET",manh_addr_na_2$FULL_ADDR)
manh_addr_na_2$FULL_ADDR <- gsub("E 16 ST","EAST 16TH STREET",manh_addr_na_2$FULL_ADDR)
manh_addr_na_2$FULL_ADDR <- gsub("W 15 ST","EAST 15TH STREET",manh_addr_na_2$FULL_ADDR)


manh_addr_na_2 <- manh_addr_na_2 %>% 
  geocode(FULL_ADDR, method = 'osm', lat = latitude , long = longitude)

manh_addr_na_2 <- subset(manh_addr_na_2, select = -c(latitude...28,longitude...29))
manh_addr_na_2 <- manh_addr_na_2 %>%
  rename(latitude = latitude...30,
         longitude = longitude...31)






#Joining the dataframes that had no coords
manh_addr_na_joined1 <- full_join(manh_addr_na, manh_addr_na_2)
manh_addr_na_joined1 <- manh_addr_na_joined1 %>%
  filter(!is.na(latitude), !is.na(longitude))

sum(is.na(manh_addr_na_joined1$latitude))
sum(is.na(manh_addr_na_joined1$latitude))

##Joining datasets
#manh_addr_na_joined1 <- full_join(manh_addr_na, manh_addr_na_2)

manhattan_df <- full_join(manhattan_df, manh_addr_na_joined1)
sum(is.na(manhattan_df$latitude))

manhattan_df <- manhattan_df %>%
  filter(!is.na(latitude), !is.na(longitude))

sum(is.na(manhattan_df$latitude))


#Need to only clean 1 more addresses, update in both dataframes
#manh_addr_na_3 <- manh_addr_na[is.na(manh_addr_na$latitude),]

#manh_addr_na_3$FULL_ADDR <- gsub("5 AVENUE","5TH AVENUE",manh_addr_na_3$FULL_ADDR)


#manh_addr_na_3 <- manh_addr_na_3 %>% 
#  geocode(FULL_ADDR, method = 'osm', lat = latitude , long = longitude)

#manh_addr_na_3 <- subset(manh_addr_na_3, select = -c(latitude...28,longitude...29))
#manh_addr_na_3 <- manh_addr_na_3 %>%
#  rename(latitude = latitude...30,
#         longitude = longitude...31)

#Joining the dataframes that had no coords
#manh_addr_na_joined1 <- full_join(manh_addr_na, manh_addr_na_2)
#manh_addr_na_joined2 <- full_join(manh_addr_na_joined1, manh_addr_na_3)
#manh_addr_na_joined2 <- manh_addr_na_joined2 %>%
#  filter(!is.na(latitude), !is.na(longitude))

#sum(is.na(manh_addr_na_joined2$latitude))
#sum(is.na(manh_addr_na_joined2$latitude))

##Joining datasets
#manh_addr_na_joined1 <- full_join(manh_addr_na, manh_addr_na_2)

#manhattan_df <- full_join(manhattan_df, manh_addr_na_joined2)
#sum(is.na(manhattan_df$latitude))

#manhattan_df <- manhattan_df %>%
#  filter(!is.na(latitude), !is.na(longitude))

#sum(is.na(manhattan_df$latitude))

#Eliminate Central Park and Randall's Island from NTA dataset
#nyc_nta_sf <- gsub("Randall's Island","Randalls Island",nyc_nta_sf$ntaname)


nyc_nta_sf <- nyc_nta_sf %>% 
  filter(!(ntaname =="Randall's Island" | ntaname =="Central Park"))
#brooklyn_df[!(brooklyn_df$SALE_PRICE<50000),]
#All addresses now have coords, we need to convert these coords into geometries.
#library(geojsonsf)
#merged_df1 <- merged_df %>%
#  mutate(GEOMETRY = st_as_sf(merged_df, coords = c("longitude", "latitude"), crs=4326, 
#                             agr = "constant"))

manhattan_df <- st_as_sf(manhattan_df, coords = c(x = "longitude", y = "latitude"), crs = 4326)
nyc_nta_sf <- st_read(dsn  = "./2020 Neighborhood Tabulation Areas (NTAs) - Tabular/geo_export_1183c9a2-33e9-4c9c-9c60-b600cbed4e05.shp")
nyc_nta_sf <- st_transform(nyc_nta_sf, 4326)


#nyc_neighborhoods_sf <- as_Spatial(nyc_nta_df$the_geom.coordinates)

#nyc_neighborhoods_sf <- geojson_sf(nyc_nta_df$the_geom.coordinates, expand_geometries = FALSE, crs = 4326)
#nyc_neighborhoods_sf <- st_sf(nyc_nta_df, sf_column_name = "the_geom.coordinates", crs = 4236)
#merged_df1 <- st_sfc(st_point(merged_df, coords = c("longitude", "latitude"), crs=4326))

manhattan_sf <- st_join(manhattan_df, nyc_nta_sf["ntaname"])

manhattan_sf <- manhattan_sf %>%
  filter(!is.na(ntaname))
####
#Aggregating Monthly Home Sales Data by Neighborhood

manhattan_sf$SALES_YEAR <- strftime(manhattan_sf$SALE_DATE, "%Y")
manhattan_sf$SALES_MONTH <- strftime(manhattan_sf$SALE_DATE, "%m")


#Creating value metric "price_per_sqft" - price per square footage.
manhattan_sf$PRICE_PER_SQFT <- manhattan_sf$SALE_PRICE / manhattan_sf$GROSS_SQ_FT


#Aggregate sale metrics by month for each neighborhood "ntaname"

#Using lubridate package to pair sales with month and year of sale
manhattan_sf$SALE_YEAR_MONTH <- floor_date(manhattan_sf$SALE_DATE, "month")

manhattan_monthly_medsales <- manhattan_sf[,c("BOROUGH","ntaname", "SALE_YEAR_MONTH", "SALE_PRICE", "PRICE_PER_SQFT")] 

#manhattan_monthly_medsales$MEDIAN_SALE_PRICE <- aggregate(SALE_PRICE ~ SALE_YEAR_MONTH + ntaname, data = manhattan_monthly_medsales, FUN = median)

manhattan_monthly_medsales <- manhattan_monthly_medsales %>% 
  group_by(ntaname, SALE_YEAR_MONTH) %>%
  summarise(MEDIAN_SALE_PRICE = median(SALE_PRICE),
            MEDIAN_PRICE_PER_SQFT = median(PRICE_PER_SQFT),
            MAX_SALE_PRICE = max(SALE_PRICE),
            MIN_SALE_PRICE = min(SALE_PRICE)
  )


#manhattan_homesales_month_df = sf_to_da


#Queens

#Set working directory
setwd("~aaronwoodward/Desktop")

#Loading property sales data
queens_df <- read.csv("rollingsales_queens.csv")

#Cleaning property sales data
queens_df <- janitor::row_to_names(queens_df, 4, remove_rows_above = TRUE) 
head(queens_df)
colnames(queens_df)

#Renaming variables:
queens_df <- queens_df %>% 
  rename(SALE_PRICE = `SALE PRICE`, 
         RESIDENTIAL_UNITS = `RESIDENTIAL UNITS`,
         COMMERCIAL_UNITS = `COMMERCIAL UNITS`,
         TOTAL_UNITS = `TOTAL UNITS`,
         LAND_SQ_FT = `LAND SQUARE FEET`,
         GROSS_SQ_FT = `GROSS SQUARE FEET`,
         SALE_DATE = `SALE DATE`,
         APT_NUMBER = `APARTMENT NUMBER`,
         ZIP_CODE = `ZIP CODE`,
         YEAR_BUILT = `YEAR BUILT`,
         TAX_CLASS_AT_PRESENT = `TAX CLASS AT PRESENT`,
         TAX_CLASS_AT_TOS = `TAX CLASS AT TIME OF SALE`,
         BUILDING_CLASS_AT_PRESENT = `BUILDING CLASS AT PRESENT`,
         BUILDING_CLASS_AT_TOS = `BUILDING CLASS AT TIME OF SALE`,
         BUILDING_CLASS_CATEGORY = `BUILDING CLASS CATEGORY`,
         BOROUGH_CODE = BOROUGH)

queens_df <- queens_df %>% 
  mutate(BOROUGH = "QUEENS")

#Changing Data types
queens_df$SALE_PRICE <- as.numeric(gsub(",", "", queens_df$SALE_PRICE))
queens_df$RESIDENTIAL_UNITS <- as.numeric(gsub(",", "", queens_df$RESIDENTIAL_UNITS))
queens_df$COMMERCIAL_UNITS <- as.numeric(gsub(",", "", queens_df$COMMERCIAL_UNITS))
queens_df$TOTAL_UNITS <- as.numeric(gsub(",", "", queens_df$TOTAL_UNITS))
queens_df$LAND_SQ_FT <- as.numeric(gsub(",", "", queens_df$LAND_SQ_FT))
queens_df$GROSS_SQ_FT <- as.numeric(gsub(",", "", queens_df$GROSS_SQ_FT))
queens_df$SALE_DATE <- mdy(queens_df$SALE_DATE)
#queens_df$SALE_MONTH <- as.Date(queens_df$SALE_MONTH,"%m/%d/%y")

str(queens_df)

#Removing records that have sale prices <$100,000
queens_df <- queens_df[!(queens_df$SALE_PRICE<50000),]

summary(queens_df)

#Removing non-residential and rental property sales by subsetting BUILDING_CLASS_CATEGORY
queens_df <- subset(queens_df, BUILDING_CLASS_CATEGORY == "09 COOPS - WALKUP APARTMENTS" | 
                         BUILDING_CLASS_CATEGORY == "10 COOPS - ELEVATOR APARTMENTS" | 
                         BUILDING_CLASS_CATEGORY == "12 CONDOS - WALKUP APARTMENTS" | 
                         BUILDING_CLASS_CATEGORY == "13 CONDOS - ELEVATOR APARTMENTS" | 
                         BUILDING_CLASS_CATEGORY =="17 CONDO COOPS" | BUILDING_CLASS_CATEGORY=="01 ONE FAMILY DWELLINGS" )

#Cleaning Addresses
# Separating the apartment numbers and dashes from addresses
queens_df$ADDRESS <- gsub(",.*","",queens_df$ADDRESS)
queens_df$ADDRESS <- gsub("-","",queens_df$ADDRESS)

# get version of DCP PAD used to build rNYCclean package data
rNYCclean::pad_version

system.time({queens_df <- pad_addr(queens_df,"ADDR.pad","ADDRESS","BOROUGH_CODE","boro_code")})

#Getting clean addresses
queens_df <- queens_df %>%
  mutate(ADDRESS_CLEAN = ifelse(is.na(ADDR.pad),ADDRESS,ADDR.pad))

#Removing dashes from ADDRESS_CLEAN
queens_df$ADDRESS_CLEAN <- gsub("-","",queens_df$ADDRESS_CLEAN)

#Putting together full addresses
queens_df <- queens_df %>%
  mutate(FULL_ADDR = paste0(ADDRESS_CLEAN,", ",BOROUGH,", NY, ",ZIP_CODE))

###Geocoding using the tidygeocoder R package to get latitude and longitude for all addresses
#install.packages('tidygeocoder')

queens_df <- queens_df %>% 
  geocode(FULL_ADDR, method = 'osm', lat = latitude , long = longitude)

sum(is.na(queens_df$latitude))

#Missing lat/lon coordinates for 412 (9%) addresses

#Creating a dataframe of addresses that do not have coords.

queens_addr_na <- queens_df[is.na(queens_df$latitude),]
queens_addr_na_unique <- data.frame(unique(queens_addr_na$FULL_ADDR))

write.csv(queens_addr_na_unique, "~aaronwoodward/Desktop/QUEENS_ADDR_NA")
#Cleaning addresses that the tidygeocoder did not return coords.
queens_addr_na$FULL_ADDR <- gsub("194 STREET","194TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("133 STREET","133RD STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("149 STREET","149TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("160 AVENUE","160TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("111 STREET","111TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("125 STREET","125TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("WHITESTONE EXPRESSWAY SR EAST","WHITESTONE EXPRESSWAY",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("162 STREET","162ND STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("10211 REMSON PLACE","10210 159TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("65 AVENUE","65TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("22 STREET","22ND STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("62 DRIVE","62ND DRIVE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("166 STREET","166TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("67 DRIVE","67TH DRIVE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("6 ROAD","6TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("171 STREET","171ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("177 STREET","177TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("24 STREET","24TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("216 STREET","216TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("199 STREET","199TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("68 DRIVE","68TH DRIVE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("191 STREET","191ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("198 STREET","198TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("126 STREET","126TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("66 ROAD","66TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("233 STREET","233RD STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("63 DRIVE","63RD DRIVE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("STRET","STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("66 AVENUE","66TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("133 STREET","133RD STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("86 STREET","86TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("64 AVENUE","64TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("171 PLACE","171ST PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("142 STREET","142ND STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("110 STREET","110TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("221 STREET","221ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("120 STREET","120TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("90 STREET","90TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("105 STREET","105TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("116 STREET","116TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("127 STREET","127TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("117 STREET","117TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("123 STREET","123RD STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("126 STREET","126TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("220 STREET","220TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("171 STREET","171ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("64 ROAD","64TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("70 ROAD","70TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("63 AVENUE","63RD AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("86 AVENUE","86TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("103 AVENUE","103RD AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("142 STREET","142ND STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("130 STREET","130TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("196 STREET","196TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("131TH STREET","131ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("126 STREET","126TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("118 STREET","118TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("71 AVENUE","71ST AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("73 ROAD","73RD ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("72 ROAD","72ND ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("176 STREET","176TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("208 STREET","208TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("71 ROAD","71ST ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("173 STREET","173RD STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("141 STREET","141ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("134 STREET","134TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("168 STREET","168TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("143 STREET","143RD STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("98 STREET","98TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("128 STREET","128TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("145 STREET","145TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("205 STREET","205TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("146 STREET","146TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("41 AVENUE","41ST AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("109 AVENUE","109TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("97 STREET","97TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("34 AVENUE","34TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("72 DRIVE","72ND DRIVE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("24 AVENUE","24TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("208 STREET","208TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("114116 231ST STREET","11416 231ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("114122 230TH STREET","11422 230TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("169 STREET","169TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("158 STREET","158TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("135 STREET","135TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("226 STREET","226TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("211 STREET","211TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("115102 217TH STREET","11920 217TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("115111 219TH STREET","11519 219TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("115120 226TH STREET","11520 226TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("202 STREET","202ND STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("229 STREET","229TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("148 STREET","148TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("227 STREET","227TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("224 STREET","224TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("140 STREET","140TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("201 PLACE","201ST PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("201 STREET","201ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("193 STREET","193RD STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("9 AVENUE","9TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("84 AVENUE","84TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("223 STREET","223RD STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("152 STREET","152ND STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("234 STREET","234TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("154 STREET","154TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("164 STREET","164TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("6 AVENUE","6TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("195 STREET","195TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("197 STREET","197TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("97 AVENUE","97TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("82 AVENUE","82ND AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("5 AVENUE","5TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("107 AVENUE","107TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("103 AVENUE","103RD AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("135 PLACE","135TH PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("218 STREET","218TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("228 STREET","228TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("121 STREET","121ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("40 ROAD","40TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("137 STREET","137TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("83 STREET","83RD STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("120 STREET","120TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("94 PLACE","94TH PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("96 STREET","96TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("157 STREET","157TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("175 STREET","175TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("241 STREET","241ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("76 STREET","76TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("62 ROAD","62ND ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("31 ROAD","31ST ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("242 STREET","242ND STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("59 AVENUE","59TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("131 AVENUE","131ST AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("174 STREET","174TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("220 PLACE","220TH PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("135 AVENUE","135TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("45 AVENUE","45TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("222 STREET","222ND STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("230 PLACE","230TH PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("122 PLACE","122ND PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("85 DRIVE","85TH DRIVE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("84 DRIVE","84TH DRIVE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("35 AVENUE","35TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("159 STREET","159TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("79 AVENUE","79TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("71 AVENUE","71ST AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("78 AVENUE","78TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("78 ROAD","78TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("84 ROAD","84TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("160 STREET","160TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("13 AVENUE","13TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("26 AVENUE","26TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("120 AVENUE","120TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("14221/27 37TH AVENUE","14221 37TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("116 AVENUE","116TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("105 AVENUE","105TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("161 STREET","161ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("256 STREET","256TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("230 STREET","230TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("145100 178TH PLACE","145106 178TH PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("119 AVENUE","119TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("20 ROAD","20TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("212 STREET","212TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("7 AVENUE","7TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("72 AVENUE","72ND AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("2 AVENUE","2ND AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("14719F ROOSEVELT AVENUE","14719 ROOSEVELT AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("38 AVENUE","38TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("8 AVENUE","8TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("PKWAY","PARKWAY",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("87 AVENUE","87TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("60 AVENUE","60TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("88 STREET","88TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("15002A 78TH RD","15002 78TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("116 ROAD","116TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("84 STREET","84TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("10 AVENUE","10TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("109 ROAD","109TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("118 AVENUE","118TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("11 AVENUE","11TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("163 AVENUE","163RD AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("26 AVENUE","26TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("79 STREET","79TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("81 STREET","81ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("12 ROAD","12TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("90 STREET","90TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("17 AVENUE","17TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("129 AVENUE","129TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("32 AVENUE","32ND AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("24 ROAD","24TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("200 STREET","200TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("92 AVENUE","92ND AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("115 AVENUE","115TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("22 AVENUE","22ND AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("12 AVENUE","12TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("144 TERRACE","144TH TERRACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("110 AVENUE","110TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("100 STREET","100TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("144 AVENUE","144TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("91 STREET","91ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("91 AVENUE","91ST AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("90 AVENUE","90TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("147 STREET","147TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("73 AVENUE","73RD AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("88 AVENUE","88TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("24 STREET","24TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("130 AVENUE","130TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("80 ROAD","80TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("93 AVENUE","93RD AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("136 AVENUE","136TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("215 STREET","215TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("18312B 58TH AVENUE","18312 58TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("144 STREET","144TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("111 AVENUE","111TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("115 DRIVE","115TH DRIVE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("115 ROAD","115TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("48 AVENUE","48TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("37 AVENUE","37TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("85 ROAD","85TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("110 ROAD","110TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("SREET","STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("112 ROAD","112TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("MC LAUGHLIN","MCLAUGHLIN",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("69 AVENUE","69TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("67 AVENUE","67TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("104 AVENUE","104TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("29 AVENUE","29TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("22 AVENUE","22ND AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("46 ROAD","46TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("43 STREET","43RD STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("47 STREET","47TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("36 AVENUE","36TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("18 AVENUE","18TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("111 ROAD","111TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("23 AVENUE","23RD AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("113 AVENUE","113TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("75 AVENUE","75TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("28 AVENUE","28TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("33 ROAD","33RD ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("2111 NAMEOKE","22 NAMEOKE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("33 STREET","33RD STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("21210A 69TH AVENUE","21210 69TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("21324B 69TH AVENUE","21324 69TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("21401A HILLSIDE AVENUE","21401 HILLSIDE AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("21419B HILLSIDE AVENUE","21419 HILLSIDE AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("21451A HILLSIDE AVENUE","21451 HILLSIDE AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("138 STREET","138TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("117 ROAD","117TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("114 AVENUE","114TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("43 AVENUE","43RD AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("77 AVENUE","77TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("132 AVENUE","132ND AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("68 AVENUE","68TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("102 AVENUE","102ND AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("22123B 67TH AVENUE","22123 67TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("80 STREET","80TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("22233B 69TH AVENUE","22233 69TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("114 ROAD","114TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("145 STREET","145TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("22410B HORACE HARDING EXPRESSWAY","22410 HORACE HARDING EXPRESSWAY",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("22468A HORACE HARDING EXPWY","22468 HORACE HARDING EXPRESSWAY",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("93 ROAD","93RD ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("76 AVENUE","76TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("133 AVENUE","133RD AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("31 AVENUE","31ST AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("23910 HILLSIDE AVENUE BELLEROSE","23910 HILLSIDE AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("145 AVENUE","145TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("85 AVENUE","85TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("24503B UNION TURNPIKE","24503 UNION TURNPIKE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("38 STREET","38TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("24531B 77TH CRESCENT","24531 77TH CRESCENT",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("62 AVENUE","62ND AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("24609A UNION TURNPIKE","24609 UNION TURNPIKE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("24611B UNION TURNPIKE","24611 UNION TURNPIKE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("57 DRIVE","57TH DRIVE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("24711 77 CRESCENT","24711 77TH CRESCENT",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("88 DRIVE","88TH DRIVE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("24778A 77TH CRESCENT","24778 77TH CRESCENT",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("148 ROAD","148TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("52 AVENUE","52ND AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("147 ROAD","147TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("87 DRIVE","87TH DRIVE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("61 AVENUE","61ST AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("147 AVENUE","147TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("87 ROAD","87TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("148 AVENUE","148TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("25 CIRCLE ROAD","25 240TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("92 AVENUE","92ND AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("150 STREET","150TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("21 STREET","21ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("89 STREET","89TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("87 STREET","87TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("48 STREET","48TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("94 STREET","94TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("101 STREET","101ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("78 STREET","78TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("11 ROAD","11TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("147 PLACE","147TH PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("77 STREET","77TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("92 STREET","92ND STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("204 STREET","204TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("203 STREET","203RD STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("104 STREET","104TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("82 STREET","82ND STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("13 STREET","13TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("14 STREET","14TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("21 STREET","21ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("12 STREET","12TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("40 STREET","40TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("45 STREET","45TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("93 STREET","93RD STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("208 STREET","208TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("35 STREET","35TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("68 STREET","68TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("30 STREET","30TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("44 STREET","44TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("214 STREET","214TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("SEA BREEZE","SEABREEZE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("54 STREET","54TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("37 STREET","37TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("62 STREET","62ND STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("163 STREET","163RD STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("47 AVENUE","47TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("51 STREET","51ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("209 AVENUE","209TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("156 STREET","156TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("46 STREET","46TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("247 STREET","247TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("64 STREET","64TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("139 STREET","139TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("158 STREET","158TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("196 PLACE","196TH PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("171 PLACE","171ST PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("39 AVENUE","39TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("187 STREET","187TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("215 PLACE","215TH PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("216 STREET","216TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("42 STREET","42ND STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("206 STREET","206TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("212 STREET","212TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("2 STREET","2ND STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("245 STREET","245TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("244 STREET","244TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("207 STREET","207TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("231 STREET","231ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("126 STREET","126TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("65 PLACE","65TH PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("71 STREET","71ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("131 STREET","131ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("108 STREET","108TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("63 PLACE","63RD PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("65 STREET","65TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("5622A 175TH PLACE","5622 175TH PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("5645B UTOPIA PARKWAY","5645 UTOPIA PARKWAY",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("225 STREET","225TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("246 CRESCENT","246TH CRESCENT",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("213 STREET","213TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("5814A 184TH STREET","5814 184TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("251 STREET","251ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("58 DRIVE","58TH DRIVE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("60 STREET","60TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("67 STREET","67TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("255 STREET","255TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("146 PLACE","146TH PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("66 STREET","66TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("53 AVENUE","53RD AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("72 STREET","72ND STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("73 PLACE","73RD PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("6524A 224TH STREET","6524 224TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("6529A 223RD PLACE","6529 223RD PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("165 STREET","165TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("6535B 223RD PLACE","6535 223RD PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("53 ROAD","53RD ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("6714A 230TH STREET","6714 230TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("182 STREET","182ND STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("223 PLACE","223RD PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("6757A 223RD PLACE","6757 223RD PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("6778A 223RD PLACE","6778 223RD PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("6783B 223RD PL","6783 223RD PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("136 STREET","136TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("6796A 223RD PLACE","6796 223RD PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("6823B SPRINGFIELD BOULEVARD","6823 SPRINGFIELD BOULEVARD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("6925A 210TH STREET","6925 210TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("6926A BELL BOULEVARD","6926 BELL BOULEVARD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("267 STREET","267TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("6933A 213TH STREET","6933 213TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("6936B 215TH STREET","6936 215TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("183 STREET","183RD STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("6951A PARK DRIVE EAST","6951 PARK DRIVE EAST",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("6951B PARK DRIVE EAST","6951 PARK DRIVE EAST",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("6961B PARK DRIVE EAST","6961 PARK DRIVE EAST",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("6978B PARK DRIVE EAST","6978 PARK DRIVE EAST",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("69 STREET","69TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("7061A PARK DRIVE EAST","7061 PARK DRIVE EAST",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("252 STREET","252ND STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("112 STREET","112TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("260 STREET","260TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("113 STREET","113TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("179 STREET","179TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("263 STREET","263RD STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("192 STREET","192ND STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("217 STREET","217TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("210 STREET","210TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("167 STREET","167TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("186 STREET","186TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("180 STREET","180TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("7714A 247TH STREET","7714 247TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("265 STREET","265TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("268 STREET","268TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("78 STREET","78TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("264 STREET","264TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("226 STREET","226TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("270 STREET","270TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("79 PLACE","79TH PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("232 STREET","232ND STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("246 STREET","246TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("243D STREET","243RD STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("249 STREET","249TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("156 AVENUE","156TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("133 AVENUE","133RD AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("261 STREET","261ST STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("61 ROAD","61ST ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("164 AVENUE","164TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("159 AVENUE","159TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("253 STREET","253RD STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("51 AVENUE","51ST AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("8431 VAN WYCK EXPRESSWAY SR EAST","8431 VAN WYCK EXPRESSWAY",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("8431 VAN WYCK EXPWY SR E","8431 VAN WYCK EXPRESSWAY",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("151 AVENUE","151ST AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("259 STREET","259TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("155 AVENUE","155TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("8634 235 COURT","8634 235TH COURT",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("149 AVENUE","149TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("57 ROAD","57TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("81 ROAD","81ST ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("129 STREET","129TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("212 PLACE","212TH PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("89 AVENUE","89TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("189 STREET","189TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("218 PLACE","218TH PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("185 STREET","185TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("86 ROAD","86TH ROAD",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("221 PLACE","221ST PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("56 AVENUE","56TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("215 PLACE","215TH PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("219 PLACE","219TH PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("184 STREET","184TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("219 STREET","219TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("85 STREET","85TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("124 STREET","124TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("107 STREET","107TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("162 AVENUE","162ND AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("9526A 125TH STREET","9526 125TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("115 STREET","115TH STREET",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("134 AVENUE","134TH AVENUE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("205 PLACE","205TH PLACE",queens_addr_na$FULL_ADDR)
queens_addr_na$FULL_ADDR <- gsub("67 ROAD","67TH ROAD",queens_addr_na$FULL_ADDR)


queens_addr_na <- queens_addr_na %>% 
  geocode(FULL_ADDR, method = 'osm', lat = latitude , long = longitude)

queens_addr_na <- subset(queens_addr_na, select = -c(latitude...28,longitude...29))
queens_addr_na <- queens_addr_na %>%
  rename(latitude = latitude...30,
         longitude = longitude...31)


sum(is.na(queens_addr_na$latitude))

# Dropping the Queens addresses that don't have coords.
queens_addr_na <- queens_addr_na %>%
  filter(!is.na(latitude), !is.na(longitude))

#sum(is.na(queens_df$latitude))

#queens_addr_na_2 <- queens_addr_na_2 %>% 
#  geocode(FULL_ADDR, method = 'osm', lat = latitude , long = longitude)

#queens_addr_na_2 <- subset(queens_addr_na_2, select = -c(latitude...28,longitude...29))
#queens_addr_na_2 <- queens_addr_na_2 %>%
#  rename(latitude = latitude...30,
#         longitude = longitude...31)

#Joining the dataframes that had no coords
#queens_addr_na_joined1 <- full_join(queens_addr_na, queens_addr_na_2)
#queens_addr_na_joined1 <- queens_addr_na_joined1 %>%
#  filter(!is.na(latitude), !is.na(longitude))

#sum(is.na(queens_addr_na_joined1$latitude))
#sum(is.na(queens_addr_na_joined1$latitude))

##Joining datasets
#queens_addr_na_joined1 <- full_join(queens_addr_na, queens_addr_na_2)

queens_df <- full_join(queens_df, queens_addr_na)
sum(is.na(queens_df$latitude))

#Checking for duplicates
#queens_df[duplicated(queens_df),]


queens_df <- queens_df %>%
  filter(!is.na(latitude), !is.na(longitude))

sum(is.na(queens_df$latitude))

#All addresses now have coords, we need to convert these coords into geometries.

#library(geojsonsf)
#merged_df1 <- merged_df %>%
#  mutate(GEOMETRY = st_as_sf(merged_df, coords = c("longitude", "latitude"), crs=4326, 
#                             agr = "constant"))

queens_df <- st_as_sf(queens_df, coords = c(x = "longitude", y = "latitude"), crs = 4326)
nyc_nta_sf <- st_read(dsn  = "./2020 Neighborhood Tabulation Areas (NTAs) - Tabular/geo_export_1183c9a2-33e9-4c9c-9c60-b600cbed4e05.shp")
nyc_nta_sf <- st_transform(nyc_nta_sf, 4326)


#nyc_neighborhoods_sf <- as_Spatial(nyc_nta_df$the_geom.coordinates)

#nyc_neighborhoods_sf <- geojson_sf(nyc_nta_df$the_geom.coordinates, expand_geometries = FALSE, crs = 4326)
#nyc_neighborhoods_sf <- st_sf(nyc_nta_df, sf_column_name = "the_geom.coordinates", crs = 4236)
#merged_df1 <- st_sfc(st_point(merged_df, coords = c("longitude", "latitude"), crs=4326))

queens_sf <- st_join(queens_df, nyc_nta_sf["ntaname"])


####
#Aggregating Monthly Home Sales Data by Neighborhood

#SALES_MONTH <- month(queens_sf$SALE_DATE)
#SALES_YEAR <- year(queens_sf$SALE_DATE)

#SALES_YEAR_MONTH <- paste0(SALES_YEAR,"-",SALES_MONTH)
#SALES_YEAR_MONTH <- ym(SALES_YEAR_MONTH)

queens_sf$SALES_YEAR <- strftime(queens_sf$SALE_DATE, "%Y")
queens_sf$SALES_MONTH <- strftime(queens_sf$SALE_DATE, "%m")


#Creating value metric "price_per_sqft" - price per square footage.
queens_sf$PRICE_PER_SQFT <- queens_sf$SALE_PRICE / queens_sf$GROSS_SQ_FT


#Aggregate sale metrics by month for each neighborhood "ntaname"

#Using lubridate package to pair sales with month and year of sale
queens_sf$SALE_YEAR_MONTH <- floor_date(queens_sf$SALE_DATE, "month")

queens_monthly_medsales <- queens_sf[,c("BOROUGH","ntaname", "SALE_YEAR_MONTH", "SALE_PRICE", "PRICE_PER_SQFT")] 

#queens_monthly_medsales$MEDIAN_SALE_PRICE <- aggregate(SALE_PRICE ~ SALE_YEAR_MONTH + ntaname, data = queens_monthly_medsales, FUN = median)

queens_monthly_medsales <- queens_monthly_medsales %>% 
  group_by(ntaname, SALE_YEAR_MONTH) %>%
  summarise(MEDIAN_SALE_PRICE = median(SALE_PRICE),
            MEDIAN_PRICE_PER_SQFT = median(PRICE_PER_SQFT),
            MAX_SALE_PRICE = max(SALE_PRICE),
            MIN_SALE_PRICE = min(SALE_PRICE)
            )
  
#THE BRONX

#Set working directory
setwd("~aaronwoodward/Desktop")

#Loading property sales data
bronx_df <- read.csv("rollingsales_bronx.csv")

#Cleaning property sales data
bronx_df <- janitor::row_to_names(bronx_df, 4, remove_rows_above = TRUE) 
head(bronx_df)
colnames(bronx_df)

#Renaming variables:
bronx_df <- bronx_df %>% 
  rename(SALE_PRICE = `SALE PRICE`, 
         RESIDENTIAL_UNITS = `RESIDENTIAL UNITS`,
         COMMERCIAL_UNITS = `COMMERCIAL UNITS`,
         TOTAL_UNITS = `TOTAL UNITS`,
         LAND_SQ_FT = `LAND SQUARE FEET`,
         GROSS_SQ_FT = `GROSS SQUARE FEET`,
         SALE_DATE = `SALE DATE`,
         APT_NUMBER = `APARTMENT NUMBER`,
         ZIP_CODE = `ZIP CODE`,
         YEAR_BUILT = `YEAR BUILT`,
         TAX_CLASS_AT_PRESENT = `TAX CLASS AT PRESENT`,
         TAX_CLASS_AT_TOS = `TAX CLASS AT TIME OF SALE`,
         BUILDING_CLASS_AT_PRESENT = `BUILDING CLASS AT PRESENT`,
         BUILDING_CLASS_AT_TOS = `BUILDING CLASS AT TIME OF SALE`,
         BUILDING_CLASS_CATEGORY = `BUILDING CLASS CATEGORY`,
         BOROUGH_CODE = BOROUGH)

bronx_df <- bronx_df %>% 
  mutate(BOROUGH = "THE BRONX")

#Changing Data types
bronx_df$SALE_PRICE <- as.numeric(gsub(",", "", bronx_df$SALE_PRICE))
bronx_df$RESIDENTIAL_UNITS <- as.numeric(gsub(",", "", bronx_df$RESIDENTIAL_UNITS))
bronx_df$COMMERCIAL_UNITS <- as.numeric(gsub(",", "", bronx_df$COMMERCIAL_UNITS))
bronx_df$TOTAL_UNITS <- as.numeric(gsub(",", "", bronx_df$TOTAL_UNITS))
bronx_df$LAND_SQ_FT <- as.numeric(gsub(",", "", bronx_df$LAND_SQ_FT))
bronx_df$GROSS_SQ_FT <- as.numeric(gsub(",", "", bronx_df$GROSS_SQ_FT))
bronx_df$SALE_DATE <- mdy(bronx_df$SALE_DATE)
#bronx_df$SALE_MONTH <- as.Date(bronx_df$SALE_MONTH,"%m/%d/%y")

str(bronx_df)

#Removing records that have sale prices <$100,000
bronx_df <- bronx_df[!(bronx_df$SALE_PRICE<50000),]

summary(bronx_df)

#Removing non-residential and rental property sales by subsetting BUILDING_CLASS_CATEGORY
bronx_df <- subset(bronx_df, BUILDING_CLASS_CATEGORY == "09 COOPS - WALKUP APARTMENTS" | 
                      BUILDING_CLASS_CATEGORY == "10 COOPS - ELEVATOR APARTMENTS" | 
                      BUILDING_CLASS_CATEGORY == "12 CONDOS - WALKUP APARTMENTS" | 
                      BUILDING_CLASS_CATEGORY == "13 CONDOS - ELEVATOR APARTMENTS" | 
                      BUILDING_CLASS_CATEGORY =="17 CONDO COOPS" | BUILDING_CLASS_CATEGORY=="01 ONE FAMILY DWELLINGS" )

#Cleaning Addresses
# Separating the apartment numbers and dashes from addresses
bronx_df$ADDRESS <- gsub(",.*","",bronx_df$ADDRESS)
bronx_df$ADDRESS <- gsub("-","",bronx_df$ADDRESS)

# get version of DCP PAD used to build rNYCclean package data
rNYCclean::pad_version

system.time({bronx_df <- pad_addr(bronx_df,"ADDR.pad","ADDRESS","BOROUGH_CODE","boro_code")})

#Getting clean addresses
bronx_df <- bronx_df %>%
  mutate(ADDRESS_CLEAN = ifelse(is.na(ADDR.pad),ADDRESS,ADDR.pad))

#Removing dashes from ADDRESS_CLEAN
bronx_df$ADDRESS_CLEAN <- gsub("-","",bronx_df$ADDRESS_CLEAN)

#Putting together full addresses
bronx_df <- bronx_df %>%
  mutate(FULL_ADDR = paste0(ADDRESS_CLEAN,", ",BOROUGH,", NY, ",ZIP_CODE))

###Geocoding using the tidygeocoder R package to get latitude and longitude for all addresses
#install.packages('tidygeocoder')

bronx_df <- bronx_df %>% 
  geocode(FULL_ADDR, method = 'osm', lat = latitude , long = longitude)

sum(is.na(bronx_df$latitude))

#Missing lat/lon coordinates for 412 (9%) addresses

#Creating a dataframe of addresses that do not have coords.

bronx_addr_na <- bronx_df[is.na(bronx_df$latitude),]
bronx_addr_na_unique <- data.frame(unique(bronx_addr_na$FULL_ADDR))

write.csv(bronx_addr_na_unique, "~aaronwoodward/Desktop/bronx_ADDR_NA")
#Cleaning addresses that the tidygeocoder did not return coords.
bronx_addr_na$FULL_ADDR <- gsub("EXPWY","EXPRESSWAY",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("232 STREET","232ND STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("229 STREET","229TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("CAROLL","CARROLL",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("237 STREET","237TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("215 STREET","215TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("219 STREET","219TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("230 STREET","230TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("213 STREET","213TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("235 STREET","235TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("178 STREET","178TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("11B EDGEWATER PARK","11 EDGEWATER PARK",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("224 STREET","224TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("122C EDGEWATER PARK","122 EDGEWATER PARK",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("222 STREET","222ND STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("167 STREET","167TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("184 STREET","184TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("131B EDGEWATER PARK","131 EDGEWATER PARK",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("132B EDGEWATER PARK","132 EDGEWATER PARK",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("133D EDGEWATER PARK","133 EDGEWATER PARK",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("136D EDGEWATER PARK","136 EDGEWATER PARK",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("BAY VIEW","BAYVIEW",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("16B EDGEWATER PARK","16 EDGEWATER PARK",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("ST RAYMOND AVENUE","ST. RAYMOND AVENUE",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("179 STREET","179TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("202 STREET","202ND STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("239 STREET","239TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("260 STREET","260TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("AQUEDUCT AVENUE EAST","AQUEDUCT AVENUE",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("236 STREET","236TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("261 STREET","261ST STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("26A EDGEWATER PARK","26 EDGEWATER PARK",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("163 STREET","163RD STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("197 STREET","197TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("196 STREET","196TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("140 STREET","140TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("169 STREET","169TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("201 STREET","201ST STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("240 STREET","240TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("245 STREET","245TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("3A EDGEWATER PARK","3 EDGEWATER PARK",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("141 STREET","141ST STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("136 STREET","136TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("139 STREET","139TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("143 STREET","143RD STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("238 STREET","238TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("144 STREET","144TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("43B EDGEWATER PARK","43 EDGEWATER PARK",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("180 STREET","180TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("142 STREET","142ND STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("158 STREET","158TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("46A EDGEWATER PARK","46 EDGEWATER PARK",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("W 232 ST","WEST 232ND STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("W 236 ST","WEST 236TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("159 STREET","159TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("190 STREET","190TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("249 STREET","249TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("61E EDGEWATER PARK","61 EDGEWATER PARK",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("223 STREET","223RD STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("182 STREET","182ND STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("231 STREET","231ST STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("226 STREET","226TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("W 239 ST","WEST 239TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("217 STREET","217TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("218 STREET","218TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("E 231 ST","EAST 231ST STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("231 ST","231ST STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("75B EDGEWATER PARK","75 EDGEWATER PARK",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("W. 238 ST","WEST 238TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("EXPWY SR","EXPRESSWAY",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("82D EDGEWATER PARK","82 EDGEWATER PARK",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("216 STREET","216TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("228 STREET","228TH STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("85D EDGEWATER PARK","85 EDGEWATER PARK",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("241 STREET","241ST STREET",bronx_addr_na$FULL_ADDR)
bronx_addr_na$FULL_ADDR <- gsub("EXPRESSWAY SR","EXPRESSWAY",bronx_addr_na$FULL_ADDR)


bronx_addr_na <- bronx_addr_na %>% 
  geocode(FULL_ADDR, method = 'osm', lat = latitude , long = longitude)

bronx_addr_na <- subset(bronx_addr_na, select = -c(latitude...28,longitude...29))
bronx_addr_na <- bronx_addr_na %>%
  rename(latitude = latitude...30,
         longitude = longitude...31)


sum(is.na(bronx_addr_na$latitude))

# Dropping the bronx addresses that don't have coords.
bronx_addr_na <- bronx_addr_na %>%
  filter(!is.na(latitude), !is.na(longitude))

#sum(is.na(bronx_df$latitude))

#bronx_addr_na_2 <- bronx_addr_na_2 %>% 
#  geocode(FULL_ADDR, method = 'osm', lat = latitude , long = longitude)

#bronx_addr_na_2 <- subset(bronx_addr_na_2, select = -c(latitude...28,longitude...29))
#bronx_addr_na_2 <- bronx_addr_na_2 %>%
#  rename(latitude = latitude...30,
#         longitude = longitude...31)

#Joining the dataframes that had no coords
#bronx_addr_na_joined1 <- full_join(bronx_addr_na, bronx_addr_na_2)
#bronx_addr_na_joined1 <- bronx_addr_na_joined1 %>%
#  filter(!is.na(latitude), !is.na(longitude))

#sum(is.na(bronx_addr_na_joined1$latitude))
#sum(is.na(bronx_addr_na_joined1$latitude))

##Joining datasets
#bronx_addr_na_joined1 <- full_join(bronx_addr_na, bronx_addr_na_2)

bronx_df <- full_join(bronx_df, bronx_addr_na)
sum(is.na(bronx_df$latitude))

#Checking for duplicates
#bronx_df[duplicated(bronx_df),]


bronx_df <- bronx_df %>%
  filter(!is.na(latitude), !is.na(longitude))

sum(is.na(bronx_df$latitude))

#All addresses now have coords, we need to convert these coords into geometries.

#library(geojsonsf)
#merged_df1 <- merged_df %>%
#  mutate(GEOMETRY = st_as_sf(merged_df, coords = c("longitude", "latitude"), crs=4326, 
#                             agr = "constant"))

bronx_df <- st_as_sf(bronx_df, coords = c(x = "longitude", y = "latitude"), crs = 4326)
nyc_nta_sf <- st_read(dsn  = "./2020 Neighborhood Tabulation Areas (NTAs) - Tabular/geo_export_1183c9a2-33e9-4c9c-9c60-b600cbed4e05.shp")
nyc_nta_sf <- st_transform(nyc_nta_sf, 4326)


#nyc_neighborhoods_sf <- as_Spatial(nyc_nta_df$the_geom.coordinates)

#nyc_neighborhoods_sf <- geojson_sf(nyc_nta_df$the_geom.coordinates, expand_geometries = FALSE, crs = 4326)
#nyc_neighborhoods_sf <- st_sf(nyc_nta_df, sf_column_name = "the_geom.coordinates", crs = 4236)
#merged_df1 <- st_sfc(st_point(merged_df, coords = c("longitude", "latitude"), crs=4326))

bronx_sf <- st_join(bronx_df, nyc_nta_sf["ntaname"])


####
#Aggregating Monthly Home Sales Data by Neighborhood

#SALES_MONTH <- month(bronx_sf$SALE_DATE)
#SALES_YEAR <- year(bronx_sf$SALE_DATE)

#SALES_YEAR_MONTH <- paste0(SALES_YEAR,"-",SALES_MONTH)
#SALES_YEAR_MONTH <- ym(SALES_YEAR_MONTH)

bronx_sf$SALES_YEAR <- strftime(bronx_sf$SALE_DATE, "%Y")
bronx_sf$SALES_MONTH <- strftime(bronx_sf$SALE_DATE, "%m")


#Creating value metric "price_per_sqft" - price per square footage.
bronx_sf$PRICE_PER_SQFT <- bronx_sf$SALE_PRICE / bronx_sf$GROSS_SQ_FT


#Aggregate sale metrics by month for each neighborhood "ntaname"

#Using lubridate package to pair sales with month and year of sale
bronx_sf$SALE_YEAR_MONTH <- floor_date(bronx_sf$SALE_DATE, "month")

bronx_monthly_medsales <- bronx_sf[,c("BOROUGH","ntaname", "SALE_YEAR_MONTH", "SALE_PRICE", "PRICE_PER_SQFT")] 

#bronx_monthly_medsales$MEDIAN_SALE_PRICE <- aggregate(SALE_PRICE ~ SALE_YEAR_MONTH + ntaname, data = bronx_monthly_medsales, FUN = median)

bronx_monthly_medsales <- bronx_monthly_medsales %>% 
  group_by(ntaname, SALE_YEAR_MONTH) %>%
  summarise(MEDIAN_SALE_PRICE = median(SALE_PRICE),
            MEDIAN_PRICE_PER_SQFT = median(PRICE_PER_SQFT),
            MAX_SALE_PRICE = max(SALE_PRICE),
            MIN_SALE_PRICE = min(SALE_PRICE)
  )

#STATEN ISLAND

#Set working directory
setwd("~aaronwoodward/Desktop")

#Loading property sales data
statenisland_df <- read.csv("rollingsales_statenisland.csv")

#Cleaning property sales data
statenisland_df <- janitor::row_to_names(statenisland_df, 4, remove_rows_above = TRUE) 
head(statenisland_df)
colnames(statenisland_df)

#Renaming variables:
statenisland_df <- statenisland_df %>% 
  rename(SALE_PRICE = `SALE PRICE`, 
         RESIDENTIAL_UNITS = `RESIDENTIAL UNITS`,
         COMMERCIAL_UNITS = `COMMERCIAL UNITS`,
         TOTAL_UNITS = `TOTAL UNITS`,
         LAND_SQ_FT = `LAND SQUARE FEET`,
         GROSS_SQ_FT = `GROSS SQUARE FEET`,
         SALE_DATE = `SALE DATE`,
         APT_NUMBER = `APARTMENT NUMBER`,
         ZIP_CODE = `ZIP CODE`,
         YEAR_BUILT = `YEAR BUILT`,
         TAX_CLASS_AT_PRESENT = `TAX CLASS AT PRESENT`,
         TAX_CLASS_AT_TOS = `TAX CLASS AT TIME OF SALE`,
         BUILDING_CLASS_AT_PRESENT = `BUILDING CLASS AT PRESENT`,
         BUILDING_CLASS_AT_TOS = `BUILDING CLASS AT TIME OF SALE`,
         BUILDING_CLASS_CATEGORY = `BUILDING CLASS CATEGORY`,
         BOROUGH_CODE = BOROUGH)

statenisland_df <- statenisland_df %>% 
  mutate(BOROUGH = "STATEN ISLAND")

#Changing Data types
statenisland_df$SALE_PRICE <- as.numeric(gsub(",", "", statenisland_df$SALE_PRICE))
statenisland_df$RESIDENTIAL_UNITS <- as.numeric(gsub(",", "", statenisland_df$RESIDENTIAL_UNITS))
statenisland_df$COMMERCIAL_UNITS <- as.numeric(gsub(",", "", statenisland_df$COMMERCIAL_UNITS))
statenisland_df$TOTAL_UNITS <- as.numeric(gsub(",", "", statenisland_df$TOTAL_UNITS))
statenisland_df$LAND_SQ_FT <- as.numeric(gsub(",", "", statenisland_df$LAND_SQ_FT))
statenisland_df$GROSS_SQ_FT <- as.numeric(gsub(",", "", statenisland_df$GROSS_SQ_FT))
statenisland_df$SALE_DATE <- mdy(statenisland_df$SALE_DATE)
#statenisland_df$SALE_MONTH <- as.Date(statenisland_df$SALE_MONTH,"%m/%d/%y")

str(statenisland_df)

#Removing records that have sale prices <$100,000
statenisland_df <- statenisland_df[!(statenisland_df$SALE_PRICE<50000),]

summary(statenisland_df)

#Removing non-residential and rental property sales by subsetting BUILDING_CLASS_CATEGORY
statenisland_df <- subset(statenisland_df, BUILDING_CLASS_CATEGORY == "09 COOPS - WALKUP APARTMENTS" | 
                     BUILDING_CLASS_CATEGORY == "10 COOPS - ELEVATOR APARTMENTS" | 
                     BUILDING_CLASS_CATEGORY == "12 CONDOS - WALKUP APARTMENTS" | 
                     BUILDING_CLASS_CATEGORY == "13 CONDOS - ELEVATOR APARTMENTS" | 
                     BUILDING_CLASS_CATEGORY =="17 CONDO COOPS" | BUILDING_CLASS_CATEGORY=="01 ONE FAMILY DWELLINGS" )

#Cleaning Addresses
# Separating the apartment numbers and dashes from addresses
statenisland_df$ADDRESS <- gsub(",.*","",statenisland_df$ADDRESS)
statenisland_df$ADDRESS <- gsub("-","",statenisland_df$ADDRESS)

# get version of DCP PAD used to build rNYCclean package data
rNYCclean::pad_version

system.time({statenisland_df <- pad_addr(statenisland_df,"ADDR.pad","ADDRESS","BOROUGH_CODE","boro_code")})

#Getting clean addresses
statenisland_df <- statenisland_df %>%
  mutate(ADDRESS_CLEAN = ifelse(is.na(ADDR.pad),ADDRESS,ADDR.pad))

#Removing dashes from ADDRESS_CLEAN
statenisland_df$ADDRESS_CLEAN <- gsub("-","",statenisland_df$ADDRESS_CLEAN)

#Putting together full addresses
statenisland_df <- statenisland_df %>%
  mutate(FULL_ADDR = paste0(ADDRESS_CLEAN,", ",BOROUGH,", NY, ",ZIP_CODE))

###Geocoding using the tidygeocoder R package to get latitude and longitude for all addresses
#install.packages('tidygeocoder')

statenisland_df <- statenisland_df %>% 
  geocode(FULL_ADDR, method = 'osm', lat = latitude , long = longitude)

sum(is.na(statenisland_df$latitude))

#Missing lat/lon coordinates for 412 (9%) addresses

#Creating a dataframe of addresses that do not have coords.

statenisland_addr_na <- statenisland_df[is.na(statenisland_df$latitude),]
statenisland_addr_na_unique <- data.frame(unique(statenisland_addr_na$FULL_ADDR))

write.csv(statenisland_addr_na_unique, "~aaronwoodward/Desktop/statenisland_ADDR_NA")
#Cleaning addresses that the tidygeocoder did not return coords.
statenisland_addr_na$FULL_ADDR <- gsub("EXPWY","EXPRESSWAY",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("232 STREET","232ND STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("229 STREET","229TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("CAROLL","CARROLL",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("237 STREET","237TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("215 STREET","215TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("219 STREET","219TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("230 STREET","230TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("213 STREET","213TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("235 STREET","235TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("178 STREET","178TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("11B EDGEWATER PARK","11 EDGEWATER PARK",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("224 STREET","224TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("122C EDGEWATER PARK","122 EDGEWATER PARK",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("222 STREET","222ND STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("167 STREET","167TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("184 STREET","184TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("131B EDGEWATER PARK","131 EDGEWATER PARK",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("132B EDGEWATER PARK","132 EDGEWATER PARK",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("133D EDGEWATER PARK","133 EDGEWATER PARK",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("136D EDGEWATER PARK","136 EDGEWATER PARK",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("BAY VIEW","BAYVIEW",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("16B EDGEWATER PARK","16 EDGEWATER PARK",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("ST RAYMOND AVENUE","ST. RAYMOND AVENUE",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("179 STREET","179TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("202 STREET","202ND STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("239 STREET","239TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("260 STREET","260TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("AQUEDUCT AVENUE EAST","AQUEDUCT AVENUE",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("236 STREET","236TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("261 STREET","261ST STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("26A EDGEWATER PARK","26 EDGEWATER PARK",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("163 STREET","163RD STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("197 STREET","197TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("196 STREET","196TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("140 STREET","140TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("169 STREET","169TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("201 STREET","201ST STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("240 STREET","240TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("245 STREET","245TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("3A EDGEWATER PARK","3 EDGEWATER PARK",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("141 STREET","141ST STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("136 STREET","136TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("139 STREET","139TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("143 STREET","143RD STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("238 STREET","238TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("144 STREET","144TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("43B EDGEWATER PARK","43 EDGEWATER PARK",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("180 STREET","180TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("142 STREET","142ND STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("158 STREET","158TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("46A EDGEWATER PARK","46 EDGEWATER PARK",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("W 232 ST","WEST 232ND STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("W 236 ST","WEST 236TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("159 STREET","159TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("190 STREET","190TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("249 STREET","249TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("61E EDGEWATER PARK","61 EDGEWATER PARK",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("223 STREET","223RD STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("182 STREET","182ND STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("231 STREET","231ST STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("226 STREET","226TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("W 239 ST","WEST 239TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("217 STREET","217TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("218 STREET","218TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("E 231 ST","EAST 231ST STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("231 ST","231ST STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("75B EDGEWATER PARK","75 EDGEWATER PARK",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("W. 238 ST","WEST 238TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("EXPWY SR","EXPRESSWAY",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("82D EDGEWATER PARK","82 EDGEWATER PARK",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("216 STREET","216TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("228 STREET","228TH STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("85D EDGEWATER PARK","85 EDGEWATER PARK",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("241 STREET","241ST STREET",statenisland_addr_na$FULL_ADDR)
statenisland_addr_na$FULL_ADDR <- gsub("EXPRESSWAY SR","EXPRESSWAY",statenisland_addr_na$FULL_ADDR)


statenisland_addr_na <- statenisland_addr_na %>% 
  geocode(FULL_ADDR, method = 'osm', lat = latitude , long = longitude)

statenisland_addr_na <- subset(statenisland_addr_na, select = -c(latitude...28,longitude...29))
statenisland_addr_na <- statenisland_addr_na %>%
  rename(latitude = latitude...30,
         longitude = longitude...31)


sum(is.na(statenisland_addr_na$latitude))

# Dropping the statenisland addresses that don't have coords.
statenisland_addr_na <- statenisland_addr_na %>%
  filter(!is.na(latitude), !is.na(longitude))

#sum(is.na(statenisland_df$latitude))

#statenisland_addr_na_2 <- statenisland_addr_na_2 %>% 
#  geocode(FULL_ADDR, method = 'osm', lat = latitude , long = longitude)

#statenisland_addr_na_2 <- subset(statenisland_addr_na_2, select = -c(latitude...28,longitude...29))
#statenisland_addr_na_2 <- statenisland_addr_na_2 %>%
#  rename(latitude = latitude...30,
#         longitude = longitude...31)

#Joining the dataframes that had no coords
#statenisland_addr_na_joined1 <- full_join(statenisland_addr_na, statenisland_addr_na_2)
#statenisland_addr_na_joined1 <- statenisland_addr_na_joined1 %>%
#  filter(!is.na(latitude), !is.na(longitude))

#sum(is.na(statenisland_addr_na_joined1$latitude))
#sum(is.na(statenisland_addr_na_joined1$latitude))

##Joining datasets
#statenisland_addr_na_joined1 <- full_join(statenisland_addr_na, statenisland_addr_na_2)

statenisland_df <- full_join(statenisland_df, statenisland_addr_na)
sum(is.na(statenisland_df$latitude))

#Checking for duplicates
#statenisland_df[duplicated(statenisland_df),]


statenisland_df <- statenisland_df %>%
  filter(!is.na(latitude), !is.na(longitude))

sum(is.na(statenisland_df$latitude))

#All addresses now have coords, we need to convert these coords into geometries.

#library(geojsonsf)
#merged_df1 <- merged_df %>%
#  mutate(GEOMETRY = st_as_sf(merged_df, coords = c("longitude", "latitude"), crs=4326, 
#                             agr = "constant"))

statenisland_df <- st_as_sf(statenisland_df, coords = c(x = "longitude", y = "latitude"), crs = 4326)
nyc_nta_sf <- st_read(dsn  = "./2020 Neighborhood Tabulation Areas (NTAs) - Tabular/geo_export_1183c9a2-33e9-4c9c-9c60-b600cbed4e05.shp")
nyc_nta_sf <- st_transform(nyc_nta_sf, 4326)


#nyc_neighborhoods_sf <- as_Spatial(nyc_nta_df$the_geom.coordinates)

#nyc_neighborhoods_sf <- geojson_sf(nyc_nta_df$the_geom.coordinates, expand_geometries = FALSE, crs = 4326)
#nyc_neighborhoods_sf <- st_sf(nyc_nta_df, sf_column_name = "the_geom.coordinates", crs = 4236)
#merged_df1 <- st_sfc(st_point(merged_df, coords = c("longitude", "latitude"), crs=4326))

statenisland_sf <- st_join(statenisland_df, nyc_nta_sf["ntaname"])


####
#Aggregating Monthly Home Sales Data by Neighborhood

#SALES_MONTH <- month(statenisland_sf$SALE_DATE)
#SALES_YEAR <- year(statenisland_sf$SALE_DATE)

#SALES_YEAR_MONTH <- paste0(SALES_YEAR,"-",SALES_MONTH)
#SALES_YEAR_MONTH <- ym(SALES_YEAR_MONTH)

statenisland_sf$SALES_YEAR <- strftime(statenisland_sf$SALE_DATE, "%Y")
statenisland_sf$SALES_MONTH <- strftime(statenisland_sf$SALE_DATE, "%m")


#Creating value metric "price_per_sqft" - price per square footage.
statenisland_sf$PRICE_PER_SQFT <- statenisland_sf$SALE_PRICE / statenisland_sf$GROSS_SQ_FT


#Aggregate sale metrics by month for each neighborhood "ntaname"

#Using lubridate package to pair sales with month and year of sale
statenisland_sf$SALE_YEAR_MONTH <- floor_date(statenisland_sf$SALE_DATE, "month")

statenisland_monthly_medsales <- statenisland_sf[,c("BOROUGH","ntaname", "SALE_YEAR_MONTH", "SALE_PRICE", "PRICE_PER_SQFT")] 

#statenisland_monthly_medsales$MEDIAN_SALE_PRICE <- aggregate(SALE_PRICE ~ SALE_YEAR_MONTH + ntaname, data = statenisland_monthly_medsales, FUN = median)

statenisland_monthly_medsales <- statenisland_monthly_medsales %>% 
  group_by(ntaname, SALE_YEAR_MONTH) %>%
  summarise(MEDIAN_SALE_PRICE = median(SALE_PRICE),
            MEDIAN_PRICE_PER_SQFT = median(PRICE_PER_SQFT),
            MAX_SALE_PRICE = max(SALE_PRICE),
            MIN_SALE_PRICE = min(SALE_PRICE)
  )





####Plotting choropleth using plotly

df1 <- rbind(manhattan_monthly_medsales, brooklyn_monthly_medsales)
df2 <- rbind(df1, queens_monthly_medsales)
df <- rbind(df2, bronx_monthly_medsales)
df <- df[df$SALE_YEAR_MONTH == '2021-08-01',]
df <- df %>% st_drop_geometry() 
#df <- df[["ntaname", "MEDIAN_SALE_PRICE","MEDIAN_PRICE_PER_SQFT","MAX_SALE_PRICE","MIN_SALE_PRICE"]]
url <- '2020 Neighborhood Tabulation Areas (NTAs) - Tabular.geojson'
geojson <- fromJSON(file = url)

g <- list(
  fitbounds = "locations",
  visible = FALSE
)
  
fig <- plot_ly()
fig <- fig %>% add_trace(
  type = "choropleth",
  geojson=geojson,
  locations=df$ntaname,
  z=df$MEDIAN_SALE_PRICE,
  colorscale="Viridis",
  zmin=100000,
  zmax=3000000,
  featureidkey="properties.ntaname",
  hovertemplate = paste0("Neighborhood: %{location}<br>",
                         "Median Sale Price: %{z:$,.0f}")
  
  #hovertemplate = 'Median Sale Price: %{y:$.0f}<extra></extra>'
)

fig <- fig %>% colorbar(title = "Median Sale Price")

fig <- fig %>% layout(
  title = "Median Home Sale Price in New York City (excluding Staten Island) <br> by Neighborhood, August 2021"
#  hoverlabel = dict(align="left")
)

fig <- fig %>% layout(
  geo = g
)

fig

#nyc_nta_hoods <- st_read(dsn = "./2020 Neighborhood Tabulation Areas (NTAs) - Tabular/geo_export_1183c9a2-33e9-4c9c-9c60-b600cbed4e05.shp", package = "sf", quiet = TRUE)
#fig <- plot_geo(manhattan_monthly_medsales)
#fig

#"./2020 Neighborhood Tabulation Areas (NTAs) - Tabular/geo_export_1183c9a2-33e9-4c9c-9c60-b600cbed4e05.shp"


#plot_ly(nyc_nta_sf, color = I("gray90"), stroke = I("black"), span = I(1))