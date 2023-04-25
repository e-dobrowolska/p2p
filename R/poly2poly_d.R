#' poly2poly_d -- Polygons to Polygons: distance
#'
#' A function that calculates the minimum distance between `osm_polygons` and provided polygons. Data is derived from OpenStreetMaps database.
#'
#' @author Ewa Dobrowolska
#' @param key character vector of the key values required by OpenStreetMaps. See \link[osmdata]{available_features} for all the possible key values.
#' @param value character vector of the tag values matching the key values. See \link[osmdata]{available_tags} for all the possible tag values.
#' @param region spatial polygon object. Object of class:  `SpatialPolygonsDataFrame`, `SpatialPolygons`, `SpatialGridDataFrame`, `SpatialGrid`, `Polygon`, `Polygons`, `sf`, or `sfc`. The polygons, to which the points will be aggregated.
#' @param sleep numeric. The waiting time (in seconds) between deriving the next OSM feature. Increasing its value might solve some problems with accessing the OSM server.
#' @param binary binary. If `TRUE`, the result will be provided in binary form, i.e. the result will be equal to 0, if there are no objects labeled with the provided key and value types, and 1 otherwise. (optional)
#' @param multi binary. If `TRUE`, the `osm_multipolygons` will be decoded into `osm_polygons`, `osm_multilines` into `osm_lines`, and considered along with polygons and linestrings. (optional)
#' @param minsize numeric. If you wish to consider only large polygons, provide the threshold size in m^2. (optional)
#' @return a data frame object. Each row represents one polygon provided with the region argument. Each column is one aggregated OpenStreetMaps feature.
#' @importFrom sf st_as_sf
#' @importFrom sf st_transform
#' @importFrom sf st_bbox
#' @importFrom sf st_intersects
#' @importFrom sf st_union
#' @importFrom sf st_within
#' @importFrom sf st_area
#' @importFrom sf st_cast
#' @importFrom sf st_intersection
#' @importFrom sf st_distance
#' @importFrom stats aggregate
#' @importFrom osmdata opq
#' @importFrom osmdata add_osm_feature
#' @importFrom osmdata osmdata_sf
#' @importFrom dplyr %>%
#' @export

poly2poly_d<-function(key, value, region, sleep=1, binary=F, multi=T, minsize=0){

  # Potential problems
  {
    if(!all(is.vector(key), is.character(key))) stop("'key' is not a character vector")
    if(!all(is.vector(value), is.character(value))) stop("'value' is not a character vector")
    if(!any(class(region)=="SpatialPolygonsDataFrame",
            class(region)=="SpatialPolygons",
            class(region)=="SpatialGrid",
            class(region)=="SpatialGridDataFrame",
            class(region)=="Polygon",
            class(region)=="Polygons",
            class(region)=="sf",
            class(region)=="sfc")) stop("'region' is not a spatial object")

    if(length(key)>length(value)) stop("More keys than values")
    if(length(key)<length(value)) stop("More values than keys")
  }

  # Data preparation
  {
    region.sf<-st_as_sf(region)
    region.sf <- region.sf %>% st_transform(crs = "+proj=longlat +datum=NAD83")
    bbox<-st_bbox(region.sf)
    df<-data.frame(ID=1:dim(region.sf)[1])
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

      dist<-st_distance(region.sf, poly) %>% as.data.frame()
      result<-apply(dist, 1, min)

      df$variable<-result
      if(binary) df$variable<-ifelse(df$variable, 1, 0)
      colnames(df)[dim(df)[2]]<-paste0(value[i])
    } else message(paste0("No polygons found: key ", key[i], ", value ", value[i]), ". Perhaps a spelling error?")
    Sys.sleep(sleep)
  }

  return(df)
}


