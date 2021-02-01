# load packages
library(prioritizrdata)
library(prioritizr)
library(raster)

# define parameters
x_range <- c(0, 1)
y_range <- c(0, 0.5)

# load data
data(tas_pu, tas_features)

# define new extent for data
ext1 <- raster::extent(tas_pu)
ext2 <- raster::extent(tas_pu)
ext2@xmin <- ext1@xmin + ((ext1@xmax - ext1@xmin) * x_range[1])
ext2@xmax <- ext1@xmin + ((ext1@xmax - ext1@xmin) * x_range[2])
ext2@ymin <- ext1@ymin + ((ext1@ymax - ext1@ymin) * y_range[1])
ext2@ymax <- ext1@ymin + ((ext1@ymax - ext1@ymin) * y_range[2])

# spatially subset the planning units
pu_idx <- rgeos::gIntersects(tas_pu, as(ext2, "SpatialPolygons"), byid = TRUE)
tas_pu <- tas_pu[pu_idx[1, ], ]

# select features that intersect with the planning units
tas_features <- raster::crop(tas_features, ext2)
rij <- rij_matrix(tas_pu, tas_features)
feat_idx <- rowSums(rij) > 10
tas_features <- tas_features[[which(feat_idx)]]

# modify datasets
tas_pu[["locked_out"]] <-
  as.numeric((tas_pu[["cost"]] >
    quantile(tas_pu[["cost"]], 0.99)[[1]]) & !tas_pu[["locked_in"]])

# manually update CRS
tas_pu@proj4string <- as(sf::st_crs(32755), "CRS")
raster::crs(tas_features) <- as(sf::st_crs(32755), "CRS")

# save data
rgdal::writeOGR(tas_pu, "data", "pu", overwrite = TRUE ,
                driver = "ESRI Shapefile")
writeRaster(tas_features, "data/vegetation.tif", overwrite = TRUE,
            NAflag = 9999)
