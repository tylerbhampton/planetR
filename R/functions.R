#' A function to search Planet imagery
#'
#' This function allows you to search the Planet API
#' @param api_key your planet api key string
#' @param bbox bounding box made with ext() from the terra package; must be EPSG:4326 Projection; no default.
#' @param date_end Expects as.Date
#' @param date_start Expects as.Date
#' @param list_dates Default to NULL. a vector of dates, to substitute `date_start` and `date_end`
#' @param cloud_lim Cloud percentage from 0-1; defaults to 0.1, or 10 percent.
#' @param ground_control Defaults to TRUE, filter images to only those with ground control, ensures locational accuracy of 10 m RMSE or better
#' @param quality Defaults to "standard", other option is "test" see https://support.planet.com/hc/en-us/articles/4407808871697-Image-quality-Standard-vs-Test-imagery
#' @param item_name Defaults to "PSScene".
#' @param asset Defaults to "ortho_analytic_8b_sr"
#' @keywords Planet
#' @export


####
#### Code from https://www.lentilcurtain.com/posts/accessing-planet-labs-data-api-from-r/
####

planet_search <- function(bbox=bbox,
                          date_end = NULL,
                          date_start = NULL,
                          list_dates = NULL,
                          cloud_lim = 0.1,
                          ground_control = TRUE,
                          quality = "standard",
                          item_name = "PSScene",
                          asset = "ortho_analytic_8b_sr" ,
                          api_key
)

{

  #convert shapefile to geojson
  #shapefile of bounding box must be EPSG:4326 Projection
  geo_json_geometry <- list(
    type=jsonlite::unbox("Polygon"),
    coordinates = list(list(
      c(bbox$xmin,
        bbox$ymin),
      c(bbox$xmin,
        bbox$ymax),
      c(bbox$xmax,
        bbox$ymax),
      c(bbox$xmax,
        bbox$ymin),
      c(bbox$xmin,
        bbox$ymin)
    ))
  )

  # filter for items the overlap with our chosen geometry
  geometry_filter <- list(
    type= jsonlite::unbox("GeometryFilter"),
    field_name= jsonlite::unbox("geometry"),
    config= geo_json_geometry
  )

  #we will search for images for up to a month beforethe date we are interested in
  if(is.null(list_dates)==FALSE){

    dategte <- paste0(min(list_dates),"T00:00:00.000Z")
    datelte <- paste0(max(list_dates),"T00:00:00.000Z")

  }else{

    dategte <- paste0(date_start,"T00:00:00.000Z")
    datelte <- paste0(date_end,"T00:00:00.000Z")
  }

  # filter images by daterange
  date_range_filter <- list(
    type= jsonlite::unbox("DateRangeFilter"),
    field_name= jsonlite::unbox("acquired"),
    config= list(
      gte= jsonlite::unbox(dategte),
      lte= jsonlite::unbox(datelte))
  )


  # filter by cloud cover
  cloud_cover_filter <- list(
    type= jsonlite::unbox("RangeFilter"),
    field_name= jsonlite::unbox("cloud_cover"),
    config = list(
      lte= jsonlite::unbox(cloud_lim))
  )

  # filter by ground control, quality
  gq_filter <- list(
    type= jsonlite::unbox("AndFilter"),
    config = list(
      list(
        field_name= jsonlite::unbox("ground_control"),
        type= jsonlite::unbox("StringInFilter"),
        config = list(jsonlite::unbox(tolower(ground_control)))
      ),
      list(
        field_name= jsonlite::unbox("quality_category"),
        type= jsonlite::unbox("StringInFilter"),
        config = list(jsonlite::unbox(quality))
      ))
  )

  # combine filters
  filter_configs <- list(
    type= jsonlite::unbox("AndFilter"),
    config = list(date_range_filter, cloud_cover_filter, gq_filter, geometry_filter) #, coverage_filter
  )

  #build request
  search_endpoint_request <- list(
    item_types = item_name,
    filter = filter_configs
  )

  #convert request to JSON
  body_json <- jsonlite::toJSON(search_endpoint_request,pretty=TRUE)

  #API request config
  url <- 'https://api.planet.com/data/v1/quick-search'
  body <- body_json

  #send API request
  request <- httr::POST(url, body = body, httr::content_type_json(), httr::authenticate(api_key, ""))
  # Read first page
  res <- jsonlite::fromJSON(httr::content(request, as = "text", encoding = "UTF-8"))

  if(length(res$features$`_permissions`)==0){print("Zero features found matching description")}
  if(length(res$features$`_permissions`)>0){
    check_permission <- function(res){
      asset_request = asset
      # Check Permissions
      permissions <- do.call(rbind, lapply(1:length(res$features$`_permissions`),function(i){

        permissions <- stringr::str_split(res$features$`_permissions`[[i]], ":", simplify = T)
        permissions <- data.frame(id = res$features$id[i],
                                  i = i,
                                  asset = gsub("assets.","",permissions[,1]),
                                  instrument = res$features$properties$instrument[i],
                                  geometry = paste0(c(
                                    res$features$geometry$coordinates[[i]][,,1],
                                    res$features$geometry$coordinates[[i]][,,2]
                                  ),collapse=","),
                                  permission = permissions[,2])
        return(permissions)}))

      permissions = subset(permissions,asset==asset_request & permission=="download")
      return(permissions)
    }

    permissions <- check_permission(res)
    if(nrow(permissions)==0){print("Zero features found matching description")}
    if(nrow(permissions)>0){
      # Read following pages, if exist
      while(is.null(res$`_links`$`_next`)==FALSE){
        request <- httr::GET(httr::content(request)$`_links`$`_next`, httr::content_type_json(), httr::authenticate(api_key, ""))
        res <- jsonlite::fromJSON(httr::content(request, as = "text", encoding = "UTF-8"))
        if(is.null(unlist(res$features))==FALSE){
          permissions <- rbind(permissions, check_permission(res))
        }
      }

      permissions <- permissions[!is.na(permissions$id),]

      if(unique(permissions$permission) == "download"){
        print(paste("You have DOWNLOAD permissions for these images."))

        permissions$date = as.Date.character(permissions$id,format = "%Y%m%d")
        permissions$yday = as.numeric(format(permissions$date, "%j"))

        if(is.null(list_dates)==FALSE){

          permissions <- permissions[permissions$date %in% list_dates,]
          print(paste("Found",nrow(permissions),"suitable",item_name, asset, "images that you have permission to download."))
          print(paste("In list of",length(list_dates), "dates from", min(list_dates),"to", max(list_dates)))

        }else{

          start_doy <- lubridate::yday(date_start)
          end_doy <- lubridate::yday(date_end)

          #permissions <- permissions[permissions$yday>=start_doy & permissions$yday<=end_doy,]
          print(paste("Found",nrow(permissions),"suitable",item_name, asset, "images that you have permission to download."))
          print(paste("Between yday:", start_doy, "to", end_doy))

        }

        if(nrow(permissions)>0){
          return(permissions)
        }else{
          print(paste("You DO NOT have DOWNLOAD permissions for these images. You have", toupper(unique(permissions$permission)), "permission"))
        }
      }
    }
  }
}



#' A function to create bbox objects to feed into planet_search()
#'
#' This function can take input values and create a bbox to be fed to the `planet_search()` function.
#' @param coords a vector of longitude latitude values, format: c(long1, long2, lat1, lat2) or c(long1,lat1)
#' @param outputtype a character string specifying sf::st_bbox (DEFAULT) or terra::ext  as the output format
#' @param bufferdist a distance in unit lat/long degrees for buffering if only a single point is provided in `coords`. Defaults to 0.01 degrees approximately 1 kilometre.
#' @keywords Planet
#' @export


bbox_coords = function(coords,
                       outputtype="sf", # possible values "sf" or "terra
                       bufferdist=0.01
                       )
{
  if(length(coords)==4){
    if(outputtype=="terra"){bbox = terra::ext(coords) ; return(bbox)}
    if(outputtype=="sf"){bbox = sf::st_bbox(terra::ext(coords)) ; return(bbox)}
  }
  if(length(coords)==2){
    print("Defaulting to outputtype=sf and point buffering")
    coords = sf::st_as_sf(data.frame(long=coords[1],lat=coords[2]),coords=c("long","lat"))
    bbox = sf::st_bbox(sf::st_buffer(coords,bufferdist)) ; return(bbox)
  }

}

#' A function to batch activate the results from planet_search()
#'
#' This function allows you to activate assets using the Planet 'Data' API. Assets cannot be downloaded until activated. Function meant to be performed in a loop.
#' @param api_key a string containing your API Key for your planet account
#' @param item ID of one item (output from `planet_search()`)
#' @param item_name Defaults to "PSOrthoTile".
#' @param asset Defaults to "ortho_analytic_8b_sr"
#' @keywords Planet
#' @export


planet_activate = function(item,
                           item_name = "PSOrthoTile",
                           asset="ortho_analytic_8b_sr",
                           api_key)
{
  url <- paste0("https://api.planet.com/data/v1/item-types/",item_name,"/items/",item)
  print(url)

  # GET BASICS ASSET

  get <- httr::GET(url, httr::content_type_json(), httr::authenticate(api_key, ""))

  if(get$status_code == 429){
    print(paste("Status code:", get$status_code, "rate limit error: poll"))}
  if(get$status_code == 200){
    print(paste("Status code:", get$status_code, "authenticated"))}
  if(get$status_code == 404){
    print(paste("Status code:", get$status_code, "error"))}

  # PARSE CONTENT TO GET ACTIVATION CODE

  contents <- httr::content(get, "parse")

  activate = httr::GET(contents$`_links`$assets, httr::authenticate(api_key, ""))

  if(max(names(httr::content(activate)) %in% asset) == 1){
    activated = httr::POST(httr::content(activate)[[asset]][["_links"]][["activate"]], httr::authenticate(api_key, ""))
    if(activated$status_code == 204){
      print(paste("Status code:", activated$status_code, "Ready to download"))}
    if(activated$status_code == 202){
      print(paste("Status code:", activated$status_code, "Not ready to download"))}
    if(activated$status_code == 200){
      print(paste("Status code:", activated$status_code, "Not ready to download"))}
  }else{print(paste("No", item_name, asset, "data available for activation"))}}


#' A function to download activated Planet imagery
#'
#' This function allows you to download imagery using the Planet 'Data' API, after use of the `planet_activate()` function.
#' @param api_key a string containing your API Key for your planet account
#' @param item ID of one item (output from `planet_search()`)
#' @param item_name Defaults to "PSOrthoTile".
#' @param asset Defaults to "ortho_analytic_8b_sr"
#' @param exportfolder The name of the directory you want your files to be downloaded into
#' @param overwrite Defaults to TRUE. Overwrite file in `exportfolder`?
#' @keywords Planet
#' @export


planet_download = function(item,
                           item_name = "PSOrthoTile",
                           asset="ortho_analytic_8b_sr",
                           exportfolder,
                           api_key,
                           overwrite = T)
{

  url <- paste0("https://api.planet.com/data/v1/item-types/",item_name,"/items/",item)
  # print(url)

  # GET BASICS ASSET

  get <- httr::GET(url, httr::content_type_json(), httr::authenticate(api_key, ""))

  if(get$status_code == 429){
    print(paste("Status code:", get$status_code, "rate limit error: poll"))}
  if(get$status_code == 200){
    print(paste("Status code:", get$status_code, "authenticated"))}
  if(get$status_code == 404){
    print(paste("Status code:", get$status_code, "error"))}

  # PARSE CONTENT TO GET ACTIVATION CODE

  contents <- httr::content(get, "parse")

  activate = httr::GET(contents$`_links`$assets, httr::authenticate(api_key, ""))

  if(max(names(httr::content(activate)) %in% asset) == 1){

    for(t in seq(1,1000,1)) {
      print(t)
      activated = httr::POST(httr::content(activate)[[asset]][["_links"]][["activate"]], httr::authenticate(api_key, ""))
      if(activated$status_code != 204){
        if(t==1){
          print("Please note: The first download can take up to 5 minutes to start after being activated")
        }else{
          print(paste("Status", activated$status_code, "retry in 10 seconds (download not ready)"))
          Sys.sleep(10)}}
      else {break}}

    activate = httr::GET(contents$`_links`$assets, httr::authenticate(api_key, ""))

    print("Download")

    download = httr::GET(httr::content(activate)[[asset]][["_links"]][["_self"]], httr::authenticate(api_key, ""))

    link = httr::content(download, "parsed")

    export = paste0(exportfolder, "/", contents$id,".tif")

    httr::RETRY("GET", link$location, httr::write_disk(export, overwrite = overwrite), httr::progress("down"), httr::authenticate(api_key, ""))

  }else{print(paste("No", item_name, asset, "data available for download"))}
}


## PlanetR Request Orders API

#' A function to order Planet imagery
#'
#' This function allows you to search and activate orders from the Planet 'Orders' API. This function wraps the `planet_search()` function and places an order for the specified number of 'most recent' images.
#' @param api_key a string containing your API Key for your planet account
#' @param bbox bounding box made with ext() from the terra package; must be EPSG:4326 Projection; no default.
#' @param date_start a date object
#' @param date_end a date object
#' @param list_dates Default to NULL. a vector of dates, to substitute `date_start` and `date_end`
#' @param cloud_lim Cloud percentage from 0-1; defaults to 0.1, or 10 percent.
#' @param item_name Defaults to "PSScene4Band".
#' @param product_bundle Defaults to "analytic_sr_udm2
#' @param asset Defaults to "ortho_analytic_8b_sr"
#' @param order_name The name you want to assign to your order. Defaults to "AutomationTEST"
#' @param mostrecent Integer of how many of the most recent images will be downloaded. Default is 0 (download all images).
#' @keywords Planet
#' @export


planet_order_request <-
  function(bbox,
           date_start,
           date_end,
           list_dates=NULL,
           cloud_lim=0.1,
           item_name = "PSScene4Band",
           product_bundle = "analytic_sr",
           asset = "ortho_analytic_8b_sr",
           order_name,
           mostrecent,
           api_key) {
    #SEARCH FOR IMAGES

    if(is.null(list_dates)==FALSE){
      print("Search from date list")
      items <- planet_search(bbox=bbox,
                             list_dates=list_dates,
                             cloud_lim=cloud_lim,
                             item_name=item_name,
                             asset = asset,
                             api_key=api_key)

    }else{
      print("Search from yday and year ranges")
      items <- planet_search(bbox=bbox,
                             date_end=date_end,
                             date_start=date_start,
                             cloud_lim=cloud_lim,
                             item_name=item_name,
                             asset = asset,
                             api_key=api_key)

    }
    items = items$id
    if(mostrecent > 0){
      items = sort(items, decreasing = T)[1:mostrecent]
      print(paste("Selected",mostrecent,"most recent images."))
    }


    #ORDER API
    products = list(
      list(
        item_ids = items,
        item_type = jsonlite::unbox(item_name),
        product_bundle = jsonlite::unbox(product_bundle)
      )
    )

    aoi = list(type = jsonlite::unbox("Polygon"),
               coordinates = list(list(
                 c(bbox$xmin,
                   bbox$ymin),
                 c(bbox$xmin,
                   bbox$ymax),
                 c(bbox$xmax,
                   bbox$ymax),
                 c(bbox$xmax,
                   bbox$ymin),
                 c(bbox$xmin,
                   bbox$ymin)
               )))

    #json structure needs specific nesting, double nested for tools hence the list(list())
    clip = list(aoi = aoi)
    tools <-  list(list(clip = clip))

    #Build request body and convert to json
    order_name = jsonlite::unbox(order_name)
    order_body <-
      list(name = order_name,
           products = products,
           tools = tools)

    order_json <- jsonlite::toJSON(order_body, pretty = TRUE)

    url = "https://api.planet.com/compute/ops/orders/v2"

    #Sent request (will make order, NOT REVERSIBLE, will show up on planet account)
    request <- httr::POST(url,
                          body = order_json,
                          httr::content_type_json(),
                          username = api_key)

    #request content
    post_content <- httr::content(request)

    if (!is.null(post_content$field$Details[[1]]$message)) {
      print(post_content$field$Details[[1]]$message)
    }

    order_id <- post_content$id

    print(paste("Save the Order ID:", order_id))
    print("You can restart the download with `planet_order_download(order_id, order_name)`")

    return(order_id)
  }



## PlanetR Request Orders API

#' A function to order Planet imagery
#'
#' This function allows you to search and activate orders from the Planet 'Orders' API. This function allows for manual selection of items from the `planet_search()` function
#' @param api_key a string containing your API Key for your planet account
#' @param items a vector containing items to request: an output of `planet_search()`
#' @param doclip Indicates whether to CLIP *each* image in the order to the *same* bbox. Defaults to TRUE. Should only be used if images cover the same geographic extent.
#' @param bbox Used if doclip=TRUE. Bounding box made with ext() from the terra package; must be EPSG:4326 Projection; no default.
#' @param item_name Defaults to "PSScene".
#' @param product_bundle Defaults to "analytic_sr_udm2
#' @param asset Defaults to "ortho_analytic_8b_sr"
#' @param order_name The name you want to assign to your order.
#' @keywords Planet
#' @export


planet_order_request_items <-
  function(items,
           doclip = TRUE,
           bbox = NULL,
           item_name = "PSScene4Band",
           product_bundle = "analytic_sr",
           asset = "ortho_analytic_8b_sr",
           order_name,
           api_key) {


    #ORDER API
    products = list(
      list(
        item_ids = items,
        item_type = jsonlite::unbox(item_name),
        product_bundle = jsonlite::unbox(product_bundle)
      )
    )

    if(doclip){
      aoi = list(type = jsonlite::unbox("Polygon"),
                 coordinates = list(list(
                   c(bbox$xmin,
                     bbox$ymin),
                   c(bbox$xmin,
                     bbox$ymax),
                   c(bbox$xmax,
                     bbox$ymax),
                   c(bbox$xmax,
                     bbox$ymin),
                   c(bbox$xmin,
                     bbox$ymin)
                 )))

      #json structure needs specific nesting, double nested for tools hence the list(list())
      clip = list(aoi = aoi)
      tools <-  list(list(clip = clip))
    }

    #Build request body and convert to json
    order_name = jsonlite::unbox(order_name)
    if(doclip){order_body <-
      list(name = order_name,
           products = products,
           tools = tools)}
    if(!doclip){order_body <-
      list(name = order_name,
           products = products)}

    order_json <- jsonlite::toJSON(order_body, pretty = TRUE)

    url = "https://api.planet.com/compute/ops/orders/v2"

    #Sent request (will make order, NOT REVERSIBLE, will show up on planet account)
    request <- httr::POST(url,
                          body = order_json,
                          httr::content_type_json(),
                          username = api_key)

    #request content
    post_content <- httr::content(request)

    if (!is.null(post_content$field$Details[[1]]$message)) {
      print(post_content$field$Details[[1]]$message)
    }

    order_id <- post_content$id

    print(paste("Save the Order ID:", order_id))
    print("You can restart the download with `planet_order_download(order_id, order_name)`")

    return(order_id)
  }


## PlanetR Orders API Download

#' A function to order Planet imagery
#'
#' This function allows you to download orders from the Planet 'Orders' API, after placing the order using the `planet_order_request()` or `planet_order_request_items()` functions
#' @param api_key a string containing your API Key for your planet account
#' @param order_id request order id (output from `planet_order_request()` or `planet_order_request_items()`)
#' @param exportfolder The name of the directory you want your files to be downloaded into
#' @param wait defaults to FALSE. Should the algorithm wait until the order is ready (TRUE), or cancel the task (FALSE)
#' @keywords Planet
#' @export


planet_order_download <- function(order_id,
                                  exportfolder,
                                  api_key,
                                  wait = FALSE
                                  ) {
  #GET order for download
  #If you lose the order_id, don't redo the request, log onto planet and find it in the orders menu
  #order_id for example SMV2 order: "dab92990-ce3a-456c-8ad6-ca0c569b4a1a"
  url2 = paste0("https://api.planet.com/compute/ops/orders/v2/", order_id)

  get_order <- httr::GET(url = url2,
                         username = api_key)

  if(get_order$status_code == 404){
    print(httr::content(get_order)$message)
  }
  if(get_order$status_code == 200){
    #Download links are in here, under _links>results>location
    get_content <- httr::content(get_order)
    #When state = 'success', ready for download


    #check if order is ready
    if(wait){
      while (get_content$state != "success") {
        print("Order still being proccessed, trying again in 60 seconds...")
        print(get_content$state)
        Sys.sleep(60)
        get_order <- httr::GET(url = url2, username = api_key)
        get_content <- httr::content(get_order)
      }
    }

    if(get_content$state == "success"){

      ##Time to download!
      print("Starting download")

      #First create download folder:
      dir.create(exportfolder, showWarnings = F)

      #Download each item in order
      for (i in 1:length(get_content$`_links`$results)) {
        print(paste0("Download: ", signif(100 * (
          i / length(get_content$`_links`$results)
        ), 1), "%"))
        #find item names in order contents
        name <- get_content$`_links`$results[[i]]$name
        findslash <- gregexpr("/", name)
        startchar <- findslash[[1]][length(findslash[[1]])] + 1
        filename <- substr(name, startchar, nchar(name))

        download_url <- get_content$`_links`$results[[i]]$location

        httr::RETRY(
          "GET",
          url = download_url,
          username = api_key,
          httr::write_disk(
            path = paste(exportfolder, filename, sep = "/"),
            overwrite = TRUE
          )
        )

      }

      print(paste0("Download complete"))
      print(paste0("Items located in ", getwd(), "/", exportfolder))
    }
  }
}


## PlanetR Orders API Search, Activate, Download

#' A function to order Planet imagery
#'
#' This function allows you to download orders from the Planet 'Orders' API. This function serves as a wrapper of `planet_order_request()` and `planet_order_download()`
#' @param api_key a string containing your API Key for your planet account
#' @param bbox bounding box made with ext() from the terra package; must be EPSG:4326 Projection; no default.
#' @param date_start a date object
#' @param date_end a date object
#' @param list_dates Default to NULL. a vector of dates, to substitute `date_start` and `date_end`
#' @param cloud_lim Cloud percentage from 0-1; defaults to 0.1, or 10 percent.
#' @param item_name Defaults to "PSScene".
#' @param product_bundle Defaults to "analytic_sr_udm2
#' @param asset Defaults to "ortho_analytic_8b_sr"
#' @param order_name The name you want to assign to your order
#' @param exportfolder The name of the directory you want your files to be downloaded into
#' @param mostrecent Integer of how many of the most recent images will be downloaded. Default is 0 (download all images).
#' @keywords Planet
#' @export


planet_order <- function(bbox,
                         date_start,
                         date_end,
                         list_dates=NULL,
                         cloud_lim = 0.1,
                         item_name="PSScene",
                         product_bundle="analytic_sr_udm2",
                         asset="ortho_analytic_8b_sr",
                         order_name,
                         exportfolder,
                         mostrecent = 0,
                         api_key) {


  if(is.null(list_dates)==FALSE){

    order_id <- planet_order_request(bbox=bbox,
                                     list_dates=list_dates,
                                     cloud_lim=cloud_lim,
                                     item_name=item_name,
                                     product_bundle=product_bundle,
                                     asset = asset,
                                     order_name=order_name,
                                     api_key=api_key)
    return(order_id)

  }else{

    order_id <- planet_order_request(bbox=bbox,
                                     date_end=date_end,
                                     date_start=date_start,
                                     cloud_lim=cloud_lim,
                                     item_name=item_name,
                                     asset = asset,
                                     product_bundle=product_bundle,
                                     order_name=order_name,
                                     api_key=api_key,
                                     mostrecent=mostrecent)
    return(order_id)


  }

  planet_order_download(order_id, exportfolder, api_key)

}

