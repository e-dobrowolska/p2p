#' lines2poly -- Lines to Polygons
#'
#' A function that calculates the total length of `osm_lines` that intersect provided polygons. Data is derived from OpenStreetMaps database.
#'
#' @author Ewa Dobrowolska
#' @param key character vector of the key values required by OpenStreetMaps. See \link[osmdata]{available_features} for all the possible key values.
#' @param value character vector of the tag values matching the key values. See \link[osmdata]{available_tags} for all the possible tag values.
#' @param region spatial polygon object. Object of class:  `SpatialPolygonsDataFrame`, `SpatialPolygons`, `SpatialGridDataFrame`, `SpatialGrid`, `Polygon`, `Polygons`, `sf`, or `sfc`. The polygons, to which the points will be aggregated.
#' @param sleep numeric. The waiting time (in seconds) between deriving the next OSM feature. Increasing its value might solve some problems with accessing the OSM server.
#' @param binary binary. If `TRUE`, the result will be provided in binary form, i.e. the result will be equal to 0, if there are no objects labeled with the provided key and value types, and 1 otherwise. (optional)
#' @param multi binary. If `TRUE`, the `osm_multipolygons` will be decoded into `osm_polygons`, `osm_multilines` into `osm_lines`, and considered along with polygons and linestrings. (optional)
#' @return a data frame object. Each row represents one polygon provided with the region argument. Each column is one aggregated OpenStreetMaps feature.
#' @importFrom sf st_as_sf
#' @importFrom sf st_transform
#' @importFrom sf st_bbox
#' @importFrom sf st_intersects
#' @importFrom sf st_union
#' @importFrom sf st_within
#' @importFrom sf st_length
#' @importFrom sf st_cast
#' @importFrom sf st_intersection
#' @importFrom stats aggregate
#' @importFrom osmdata opq
#' @importFrom osmdata add_osm_feature
#' @importFrom osmdata osmdata_sf
#' @importFrom dplyr %>%
#' @examples
#' @export

lines2poly<-function(key, value, region, sleep=1, binary=F, multi=T){

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

    if(multi & length(variable.sf$osm_multilines)>0) {
      lines<-c(variable.sf$osm_lines,st_cast(variable.sf$osm_multilines, "LINESTRING"))
      } else lines<-variable.sf$osm_lines

    if(length(lines)>0)
    {
      lines <- lines %>% st_transform(crs = "+proj=longlat +datum=NAD83")

      id<-st_intersects(lines, region.sf) %>% unlist()
      length<-st_intersection(lines, region.sf) %>% st_length()
      id_length<-data.frame(ID=id, variable=length)
      agg<-aggregate(id_length$variable, by=list(id_length$ID), FUN="sum")
      colnames(agg)<-c("ID", "variable")
      agg$variable<-agg$variable %>% as.numeric()

      df<-merge(df, agg, by="ID",  all.x=TRUE, sort=TRUE)
      df$variable[is.na(df$variable)]<-0
      if(binary) df$variable<-ifelse(df$variable, 1, 0)
      colnames(df)[dim(df)[2]]<-paste0(value[i])
    } else message(paste0("No lines found: key ", key[i], ", value ", value[i]), ". Perhaps a spelling error?")
    Sys.sleep(sleep)
  }

  return(df)
}
