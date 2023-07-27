# planetR

Some R tools to search, activate and download satellite imagery from the Planet API (https://developers.planet.com/docs/api/). The current purpose of the package is to Search the API, batch activate all assets, and then batch download them. 

This is a fork from [bevingtona/planetR](https://github.com/bevingtona/planetR).

There are two API's: 
- v1 Planet API
- v2 Planet Orders API (can clip and pre-process scenes to AOI on server)

### Functions

```{r functions}

## current functions (API v1)
planetR::planet_search()
planetR::planet_activate()
planetR::planet_download()

## current functions (Orders API v2)
planetR::planet_order_request()
planetR::planet_order_download()
planetR::planet_order()

```

### Installation

You can install planetR directly from this GitHub repository. To do so, you will need the remotes package. Next, install and load the planetR package using remotes::install_github():

```{r installation}
install.packages("remotes")
remotes::install_github("bevingtona/planetR")
library(planetR)
```

#### Example using the Planet 'Orders' API

This is an example of how to search, activate and download assets using `planetR`.

```{r example}

#### LIBRARIES ####
library(planetR)
library(httr)
library(jsonlite)
library(stringr)
library(sf)

#### USER VARIABLES ####

# Site name that will be used in the export folder name
site = "MySite"

# Order name that will appear on your PlanetLabs web portal
order_name = "TestDownload"

# Set Workspace (optional)
setwd("")

# Set API
api_key = "" 

# Date range of interest
date_start <- as.Date("2022-01-01")
date_end   <- as.Date("2022-04-01")

# Metadata filters
cloud_lim <- 0.02 # percent from 0-1
item_name <- "PSScene" # (see https://developers.planet.com/docs/data/items-assets/)
product_bundle <- "analytic_8b_sr_udm2" # https://developers.planet.com/docs/integrations/gee/delivery/
asset <- "ortho_udm2" # (see https://developers.planet.com/docs/data/items-assets/)

# Set AOI (many ways to set this!) ultimately just need an extent object from terra::ext or sf::st_bbox
#         Note!! The terra package is prefered over the raster package, and in this package fork raster is not compatible.
# OPTION 1: Import feature
my_aoi       = read_sf("path_to_file.sqlite") # KML, SHP, SQLITE, or other
bbox         = sf::st_bbox(my_aoi)

# OPTION 2: Set bounding box manually
bbox         = terra::ext(-129,-127,50,51)

# Set/Create Export Folder
exportfolder <- paste(site, item_name, asset, lubridate::year(date_start), lubridate::year(date_end),  lubridate::yday(date_start),  lubridate::yday(date_end), sep = "_")

if(!(dir.exists("exports"))){
dir.create("exports", showWarnings = F)
}

dir.create(file.path("exports", exportfolder), showWarnings = F)

# Planet Orders API

planet_order(api_key = api_key, 
             bbox = bbox, 
             date_end = date_end,
             date_start = date_start,
             cloud_lim = cloud_lim, 
             item_name = item_name, 
             product_bundle = product_bundle,
             asset = asset,
             order_name = order_name,
             exportfolder = exportfolder,
             mostrecent = 1 # downloads the 1 most recent image
             )
             

[1] "Found 6 suitable PSScene4Band analytic_sr images"
[1] "Day of year: 290-300"
[1] "Year: 2016-2020"
[1] "Save the Order ID: cd16bf13-2f18-47e1-84ad-1d3d280326e3"
[1] "You can restart the download with `planet_order_download(order_id, order_name)`"
[1] "Order still being proccessed, trying again in 60 seconds..."
[1] "queued"
[1] "Order still being proccessed, trying again in 60 seconds..."
[1] "running"
[1] "Order still being proccessed, trying again in 60 seconds..."
[1] "running"
[1] "Order still being proccessed, trying again in 60 seconds..."
[1] "running"
[1] "Order still being proccessed, trying again in 60 seconds..."
[1] "running"
[1] "Order still being proccessed, trying again in 60 seconds..."
[1] "running"
[1] "Order still being proccessed, trying again in 60 seconds..."
[1] "running"
[1] "Order still being proccessed, trying again in 60 seconds..."
[1] "running"
[1] "Order still being proccessed, trying again in 60 seconds..."
[1] "running"
[1] "Starting download"
[1] "Download: 4%"
[1] "Download: 8%"
[1] "Download: 10%"
[1] "Download: 20%"
[1] "Download: 20%"
[1] "Download: 20%"
[1] "Download: 30%"
[1] "Download: 30%"
[1] "Download: 40%"
[1] "Download: 40%"
[1] "Download: 40%"
[1] "Download: 50%"
[1] "Download: 50%"
[1] "Download: 60%"
[1] "Download: 60%"
[1] "Download: 60%"
[1] "Download: 70%"
[1] "Download: 70%"
[1] "Download: 80%"
[1] "Download: 80%"
[1] "Download: 80%"
[1] "Download: 90%"
[1] "Download: 90%"
[1] "Download: 100%"
[1] "Download: 100%"
[1] "Download complete"
[1] "Items located in ../MySite_PSScene4Band_analytic_sr_2016_2020_290_300"

```

#### Example using the Planet 'Data' API

```{r example_v1}

# PLANET_SEARCH: Search API

  items <- planet_search(bbox = bbox,
                            date_end = date_end,
                            date_start = date_start,
                            cloud_lim = cloud_lim,
                            item_name = item_name,
                            asset = asset,
                            api_key = api_key)
              
  print(paste("Images available:", length(items), item_name, asset))

# PLANET_ACTIVATE: Batch Activate 

for(i in 1:length(items)) {
  item = items[i]
  planet_activate(item,
                  item_name = item_name,
                  asset = asset,
                  api_key = api_key)
  print(paste("Activating", i, "of", length(items)))
}

# PLANET_DOWNLOAD: Batch Download 

for(i in 1:length(items)) {
  item = items[i]
  planet_download(item,
                  item_name = item_name,
                  asset = asset,
                  exportfolder = exportfolder,
                  api_key = api_key)
  print(paste("Downloading", i, "of", length(items)))
}
  
```
![](images/download_example.png)

