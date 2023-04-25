#' poly2pts_i -- Polygons to Points: intersection
#'
#' A function that calculates the total number of osm_polygons, that intersect the neighbourhood of provided points. Data is derived from OpenStreetMaps database.
#'
#' @author Ewa Dobrowolska
#' @param key character vector of the key values required by OpenStreetMaps. See \link[osmdata]{available_features} for all the possible key values.
#' @param value character vector of the tag values matching the key values. See \link[osmdata]{available_tags} for all the possible tag values.
#' @param pts spatial points object. Object of class: `SpatialPointsDataFrame`, `SpatialPoints`, `SpatialPixels`, `SpatialPixelsDataFrame`, `sf`, `sfc`. The points, around  which the features will be aggregated.
#' @param radius numeric. The radius, within which the features will be aggregated.
#' @param sleep numeric. The waiting time (in seconds) between deriving the next OSM feature. Increasing its value might solve some problems with accessing the OSM server.
#' @param binary binary. If `TRUE`, the result will be provided in binary form, i.e. the result will be equal to 0, if there are no objects labeled with the provided key and value types, and 1 otherwise. (optional)
#' @param multi binary. If `TRUE`, the `osm_multipolygons` will be decoded into `osm_polygons`, `osm_multilines` into `osm_lines`, and considered along with polygons and linestrings. (optional)
#' @param minsize numeric. If you wish to consider only large polygons, provide the threshold size in m^2. (optional)
#' @return A data frame object. Each row represents one point provided with the pts argument. Each column is one aggregated OpenStreetMaps feature.
#' @importFrom sf st_as_sf
#' @importFrom sf st_transform
#' @importFrom sf st_bbox
#' @importFrom sf st_intersects
#' @importFrom sf st_union
#' @importFrom sf st_within
#' @importFrom sf st_area
#' @importFrom sf st_cast
#' @importFrom sf st_intersection
#' @importFrom sf st_geometry
#' @importFrom sf st_buffer
#' @importFrom stats aggregate
#' @importFrom osmdata opq
#' @importFrom osmdata add_osm_feature
#' @importFrom osmdata osmdata_sf
#' @importFrom dplyr %>%
#' @export

poly2pts_i<-function(key, value, pts, radius, sleep=1, binary=F, multi=T, minsize=0){

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

    if(multi & length(variable.sf$osm_multipolygons)>0){
      poly<-c(variable.sf$osm_polygons, st_cast(variable.sf$osm_multipolygons, "POLYGON"))$geometry
    }else poly<-variable.sf$osm_polygons$geometry

    if(length(poly)>0)
    {
      if(minsize>0) poly<-poly[which(as.numeric(st_area(poly))>minsize)]

      poly <- poly %>% st_transform(crs = "+proj=longlat +datum=NAD83")

      a<-st_intersects(circles, poly)
      result<-lapply(a, length) %>% unlist()

      df$variable<-result
      if(binary) df$variable<-ifelse(df$variable, 1, 0)
      colnames(df)[dim(df)[2]]<-paste0(value[i])
    } else message(paste0("No polygons found: key ", key[i], ", value ", value[i]), ". Perhaps a spelling error?")
    Sys.sleep(sleep)
  }

  return(df)
}

