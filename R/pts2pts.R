#' pts2pts -- Points to Points
#'
#' A function that calculates the total number of osm_points located within a given radius from provided points. Points are derived from OpenStreetMaps database.
#'
#' @author Ewa Dobrowolska
#' @param key character vector of the key values required by OpenStreetMaps. See \link[osmdata]{available_features} for all the possible key values.
#' @param value character vector of the tag values matching the key values. See \link[osmdata]{available_tags} for all the possible tag values.
#' @param pts spatial points object. Object of class: `SpatialPointsDataFrame`, `SpatialPoints`, `SpatialPixels`, `SpatialPixelsDataFrame`, `sf`, `sfc`. The points, around  which the features will be aggregated.
#' @param radius numeric. The radius, within which the features will be aggregated.
#' @param sleep numeric. The waiting time (in seconds) between deriving the next OSM feature. Increasing its value might solve some problems with accessing the OSM server.
#' @param outside binary. If `TRUE`, only osm_points located outside osm_polygons will be considered. (optional)
#' @param binary binary. If `TRUE`, the result will be provided in binary form, i.e. the result will be equal to 0, if there are no objects labeled with the provided key and value types, and 1 otherwise. (optional)
#' @param multi binary. If `TRUE`, the `osm_multipolygons` will be decoded into `osm_polygons`, `osm_multilines` into `osm_lines`, and considered along with polygons and linestrings. (optional)
#' @return A data frame object. Each row represents one point provided with the pts argument. Each column is one aggregated OpenStreetMaps feature.
#' @importFrom sf st_as_sf
#' @importFrom sf st_transform
#' @importFrom sf st_bbox
#' @importFrom sf st_intersects
#' @importFrom sf st_union
#' @importFrom sf st_within
#' @importFrom sf st_geometry
#' @importFrom sf st_buffer
#' @importFrom osmdata opq
#' @importFrom osmdata add_osm_feature
#' @importFrom osmdata osmdata_sf
#' @importFrom dplyr %>%
#' @export

pts2pts<-function(key, value, pts, radius, sleep=1, binary=F, outside=F, multi=F){

    # Potential problems
    {
      if(!all(is.vector(key), is.character(key))) stop("'keys' is not a character vector")
      if(!all(is.vector(value), is.character(value))) stop("'values' is not a character vector")
      if(!any(class(pts)=="SpatialPointsDataFrame",
              class(pts)=="SpatialPoints",
              class(pts)=="SpatialPixels",
              class(pts)=="SpatialPixelsDataFrame",
              class(pts)=="sf",
              class(pts)=="sfc")) stop("'pts' is not a spatial object")

      if(length(key)>length(value)) stop("More keys than values")
      if(length(key)<length(value)) stop("More values than keys")
    }

    # Data preparation
    {
      pts.sf<-st_as_sf(pts)
      pts.sf <- pts.sf %>% st_transform(crs = "+proj=longlat +datum=NAD83")
      circles<-st_buffer(pts.sf,radius) %>% st_geometry()
      bbox<-st_bbox(circles)
      df<-data.frame(ID=1:length(circles))
    }

    for(i in 1:length(key)) {
      variable.sf = opq(bbox) %>%
        add_osm_feature(key=key[i], value = value[i]) %>%
        osmdata_sf()

      if(dim(variable.sf$osm_points)[1]>0)
      {
        variable.sf$osm_points <- variable.sf$osm_points %>% st_transform(crs = "+proj=longlat +datum=NAD83")

        if1<-(outside & !multi & length(variable.sf$osm_polygons)>0)
        if2<-(outside & multi & (length(variable.sf$osm_polygons)>0 | length(variable.sf$osm_multipolygons)>0))

        if(if1 | if2) {
          if(multi & length(variable.sf$osm_multipolygons)>0) {
            union<-st_union(variable.sf$osm_polygons, variable.sf$osm_multipolygons)
          } else union<-st_union(variable.sf$osm_polygons)
          union <- union %>% st_transform(crs = "+proj=longlat +datum=NAD83")
          p_list<-!lengths(st_intersects(variable.sf$osm_points, union))
          points<-variable.sf$osm_points[p_list,]
        } else points<-variable.sf$osm_points

        with_list<-st_within(variable.sf$osm_points, circles)
        table<-with_list %>% unlist() %>% table()

        agg<-data.frame(ID=table %>% names() %>% as.numeric(), variable=table %>% as.numeric())

        df<-merge(df, agg, by="ID",  all.x=TRUE, sort=TRUE)
        df$variable[is.na(df$variable)]<-0
        if(binary) df$variable<-ifelse(df$variable, 1, 0)
        colnames(df)[dim(df)[2]]<-paste0(value[i])
      } else message(paste0("No points found: key ", key[i], ", value ", value[i]), ". Perhaps a spelling error?")
      Sys.sleep(sleep)
    }

    return(df)
  }
