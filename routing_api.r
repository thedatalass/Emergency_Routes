library(igraph)
library(plumber)

allRoads <- sf::st_read('../data/partitioned_roads_roseau_colored2.gpkg')
allShelters <- sf::st_read('../data/roseau_shelters.gpkg')
allShelterId <- allShelters$closest_intersection
allBuildings <- sf::st_read('../data/roseau_buildings.gpkg')
damagedRoads <- sf::st_read('../data/mariaDamagedRoads.gpkg')
damagedAreas <- sf::st_read('../data/mariaDamagedAreas.gpkg')

allShapes <- list(
  allRoads =  allRoads %>%
    geojsonsf::sf_geojson(),
  buildings = allBuildings %>%
    geojsonsf::sf_geojson(),
  shelters = allShelters %>%
    geojsonsf::sf_geojson(),
  damagedAreas = damagedAreas %>%
    geojsonsf::sf_geojson()
)

allRoads <- allRoads %>%
  dplyr::mutate(singleSource = purrr::map(singleSource, ~stringr::str_split(.x, '_')[[1]]))

allGraphs = list(
  fullG = read_graph('../data/roseau_roads.graphml', 'graphml')
)

resetMap <- function(){
  allGraphs$currentG <<- allGraphs$fullG
}

resetMap()

removeRoads <- function(g, roadIds){
  print('removeRoads')
  print(roadIds)
  roads <- E(g)[edge_id %in% roadIds]
  rmH <- head_of(g, roads)
  rmT <- tail_of(g, roads)
  
  removeEdges <- purrr::map2(rmH, rmT, function(x,y){
    E(g)[rmH %--% rmT]
  }) %>%
    do.call(c, .)
  
  print(removeEdges$edge_id)
  
  g <- g - removeEdges
  g
}

trappedArea <- function(g, nodeId){
  print(igraph::groups(igraph::components(g)))
  trappedVertices <- components(g) %>%
    igraph::groups() %>%
    .[purrr::map_lgl(., function(x){!any(allShelterId %in% x)})] %>%
    do.call(c, .)
  
  print(trappedVertices)
    # .[purrr::map_lgl(., function(x){nodeId %in% x})] %>%
    # .[[1]]
  
  trappedEdges <- E(g)[trappedVertices %--% V(g)]$edge_id
  
  return(
    list(trappedVertices=trappedVertices, trappedEdges = trappedEdges)
  )
}

closestShelter <- function(g, nodeId){
  allPaths <- shortest_paths(g, nodeId, V(g)[isShelter], output = 'both', weights = E(g)$d_weighted)
  
  response <- list()
  print(allPaths$epath)
  if(any(purrr::map_dbl(allPaths$epath, length) > 0)){
    totDist <- purrr::map_dbl(allPaths$epath, ~sum(.x$d_weighted))
    
    response$type <- "vpath"
    response$data <- allPaths$vpath[[(1:length(totDist))[totDist == min(totDist[totDist != 0])]]]
  } else {
    response$type <- 'emptypath'
    response$data <- trappedArea(g, nodeId)
  }
  
  return(response)
}


#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#* GET shape file
#* @get /remove
function(edgeid=""){
  edgeid = stringr::str_split(edgeid, '_')[[1]]
  allGraphs$currentG <<- removeRoads(allGraphs$currentG, edgeid)
  return(edgeid)
}

#* GET shape file
#* @get /removeMaria
function(){
  edgeIds <- damagedRoads$edge_id
  print(edgeIds)
  allGraphs$currentG <<- removeRoads(allGraphs$currentG, edgeIds)
  return(edgeIds)
}

#* GET shape file
#* @get /getShape
function(res, name=""){
  res$body <- allShapes[[name]]
  return(res)
}

#* GET shape file
#* @get /closestShelter
function(res, id=""){
  g <- allGraphs$currentG

  routeInfo <- closestShelter(g, id)
  print(routeInfo)
  response <- list()
  
  if(routeInfo$type == 'vpath'){
    path2shelter <- dplyr::filter(allRoads, purrr::map_lgl(singleSource, function(x){
      all(x %in% routeInfo$data$name)
    })) %>%
      dplyr::select(edge_id, way_id) %>%
      geojsonsf::sf_geojson()
    
    res$body = path2shelter
  }
  
  if(routeInfo$type == 'emptypath'){
    res$body <- jsonlite::toJSON(routeInfo$data)
  }
  return(res)
}

#* GET reset
#* @get /reset
function(){
  resetMap()
  return('blah')
}


