library(dplyr)
library(leaflet)
library(purrr)
library(sf)
library(utils)

r <- shiny::reactiveValues()
wd <- "/srv/shiny-server/wells/data"

# WellInfrastructure from Petrinex as csv
# pool region from AER_order_system from shapefile page on aer
# abandoned wells is the TT (not forest) file from shapefile aer page

fname <- utils::unzip(paste0(wd, "/ABNDWells_SHPTT.zip"), list = TRUE)$Name

utils::unzip(paste0(wd, "/ABNDWells_SHPTT.zip"), files = fname, exdir = wd, overwrite = TRUE)
layer <- sub(".*/", "", sub(".shp.*", "", grep(".shp", fname, value = TRUE)[1]))
dsn <- sub(paste0(layer, ".*"), "", file.path(wd, fname)[1])
out <- sf::read_sf(dsn = dsn, layer = layer, quiet = TRUE) %>% 
  dplyr::filter(Status == "Abandoned", 
                Fluid != "Not Applicable") %>% 
  dplyr::distinct()
# If pipe used - will not transform data (will provide warning)
out <- sf::st_transform(out, crs = 4326)

additionalInfo <- readRDS(paste0(wd, "/well-infrastructure-AB.rds"))

wellsWithInfo <- dplyr::left_join(out, additionalInfo, by = c("Licence" = "LicenceNumber")) %>% 
  dplyr::group_by(Licence) %>% 
  dplyr::filter(FinalTotalDepth == max(FinalTotalDepth)) %>% 
  dplyr::ungroup()

# Read in in Pool Regions
fname2 <- utils::unzip(paste0(wd, "/poolRegions.zip"), list = TRUE)$Name

utils::unzip(paste0(wd, "/poolRegions.zip"), files = fname2, exdir = wd, overwrite = TRUE)
layer2 <- sub(".*/", "", sub(".shp.*", "", grep(".shp", fname2, value = TRUE)[1]))
dsn2 <- sub(paste0(layer2, ".*"), "", file.path(wd, fname2)[1])
poolRegion <- sf::read_sf(dsn = dsn2, layer = layer2, quiet = TRUE)
poolRegion <- sf::st_transform(poolRegion, crs = 4326)

# Read in Abandonment polygons
fname3 <- utils::unzip(paste0(wd, "/abandonedWells.zip"), list = TRUE)$Name

utils::unzip(paste0(wd, "/abandonedWells.zip"), files = fname3, exdir = wd, overwrite = TRUE)
layer3 <- sub(".*/", "", sub(".shp.*", "", grep(".shp", fname3, value = TRUE)[1]))
dsn3 <- sub(paste0(layer3, ".*"), "", file.path(wd, fname3)[1])
abandonmentArea <- sf::read_sf(dsn = dsn3, layer = layer3, quiet = TRUE)
abandonmentArea <- sf::st_transform(abandonmentArea, crs = 4326)

area <- sf::st_within(wellsWithInfo$geometry, abandonmentArea$geometry)

area[lengths(area) == 0] <- 0
area <- dplyr::tibble(area) %>% 
  dplyr::mutate(area = purrr::map_dbl(area, first))

wellsWithInfo <- wellsWithInfo %>% 
  cbind(area) %>% 
  dplyr::mutate(Location = dplyr::case_when(area == 1 ~ "Lloydminster", 
                                            area == 2 ~ "Medicine Hat",
                                            area == 3 ~ "High Level",
                                            area == 4 ~ "Athabasca/Peace River",
                                            area == 5 ~ "Calgary/Edmonton",
                                            area == 6 ~ "Drayton Valley/Grande Prairie",
                                            TRUE ~ NA), 
                key = dplyr::case_when(FinalTotalDepth <= 1199 ~ "a", 
                                       FinalTotalDepth <= 1999 ~ "b", 
                                       FinalTotalDepth <= 2499 ~ "c", 
                                       FinalTotalDepth <= 2999 ~ "d", 
                                       TRUE ~ "e")) %>% 
  dplyr::filter(!is.na(Location))

# Read in Reclamation Costs
fname4 <- utils::unzip(paste0(wd, "/reclamation.zip"), list = TRUE)$Name

utils::unzip(paste0(wd, "/reclamation.zip"), files = fname4, exdir = wd, overwrite = TRUE)
layer4 <- sub(".*/", "", sub(".shp.*", "", grep(".shp", fname4, value = TRUE)[1]))
dsn4 <- sub(paste0(layer4, ".*"), "", file.path(wd, fname4)[1])
reclamationMap <- sf::read_sf(dsn = dsn4, layer = layer4, quiet = TRUE)
reclamationMap <- sf::st_transform(reclamationMap, crs = 4326)

recArea <- sf::st_within(wellsWithInfo$geometry, reclamationMap$geometry)

recArea[lengths(recArea) == 0] <- 0
recArea <- dplyr::tibble(recArea) %>% 
  dplyr::mutate(recArea = purrr::map_dbl(recArea, first))

wellsWithInfo <- wellsWithInfo %>% 
  cbind(recArea) %>% 
  dplyr::mutate(reclamationArea = dplyr::case_when(recArea == 1 ~ "Grasslands Area East", 
                                                   recArea == 2 ~ "Grasslands Area West",
                                                   recArea == 3 ~ "Parklands Area",
                                                   recArea == 4 ~ "Alpine Area",
                                                   recArea == 5 | recArea == 8 ~ "Foothills Area",
                                                   recArea == 6 ~ "Western Boreal Area ",
                                                   recArea == 7 ~ "Boreal Area",
                                                   TRUE ~ NA)) %>% 
  dplyr::filter(!is.na(reclamationArea))

# Read in Cost Tables
recCost <- readRDS(paste0(wd, "/reclamationCost.rds"))
dataCost <- readRDS(paste0(wd, "/dataCost.rds")) %>% 
  dplyr::mutate(key = dplyr::case_when(Depth == 1199 ~ "a", 
                                       Depth == 1999 ~ "b", 
                                       Depth == 2499 ~ "c", 
                                       Depth == 2999 ~ "d", 
                                       TRUE ~ "e")) 

wellsWithInfo <- wellsWithInfo %>% 
  dplyr::left_join(recCost, by = "reclamationArea", relationship = "many-to-many") %>% 
  dplyr::left_join(dataCost, by = c("Location", "key"), relationship = "many-to-many")
