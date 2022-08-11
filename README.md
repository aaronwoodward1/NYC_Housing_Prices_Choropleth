# NYC_Housing_Prices_Choropleth
![7C80E8B4-1EE1-40F4-8CFC-6F318EB1B491_1_105_c](https://user-images.githubusercontent.com/45469389/184256696-102b6f7a-e8e5-43c7-917e-5a4530eadeca.jpeg)
![AA4BAD50-1162-4320-BC21-D21B34EFEE00](https://user-images.githubusercontent.com/45469389/184256721-8dafdb3a-a953-4446-84f1-7e68d831e3cb.jpeg)


This is a choropleth map of the median home prices in each neighborhood in New York City, excluding those in Staten Island. We generate this visualization through writing R code and using Plotly's interactive graphic plotting library. The datasets used for this visualization are Rolling Property Sales Data and Neighborhood Tabulation Area (NTA) shapefiles (neighborhood boundaries). 

The Data

Rolling Property Sales: Property sales are reported by the New York City Department of Finance (DOF). One of the datasets that DOF releases are Rolling Property Sales. These datasets are released on a quarterly bases, which captures, not only residential, but all property sales (commercial, industrial and warehouse) on a 12-month rolling basis. For this data visualization, we used the datasets that were published on DOF's website in April 2022. These dataset can include over 1 million records, which involves intense data wrangling and cleansing techniques, which will be presented in the code. The data can be accessed here: https://www1.nyc.gov/site/finance/taxes/property-rolling-sales-data.page. 

New York City Neighbor Tabulation Areas: This is the spatial dataset that is used for the visualization, which come in the form of a shapefile. The dataset have the boundaries or the spatial shapes and coordinates of each New York City neighborhood. Since, neighborhood boundaries are arbitrary, the NTA dataset helps in establishing such boundaries. For this visualization, we use the 2020 NTAs. which are based on the aggregation of the 2020 census tracts and nest within Community District Tabulation Areas (CDTA). This dataset can be found on the NYC Open Data portal: https://data.cityofnewyork.us/City-Government/2020-Neighborhood-Tabulation-Areas-NTAs-Mapped/4hft-v355.

The Process

We use various techniques in cleansing and shaping the datasets, especially using tidygeocoder to geocode the addresses in the Rolling Property Sales datasets once the addresses have been cleaned. Once the addresses have been geocoded, we use the spatial coordinates (longitude and latitude) to assign the addresses to their respective neighborhood. These processes will shape our data to be ready for analysis and plotting the choropleth map.

Without further adieu... Let's dig into the code!!!

