library(gdalcubes)
library(rstac)
library(sf)
library(viridis)
library(stars)
library(tmap)

s = stac("https://earth-search.aws.element84.com/v0")
bbox_wgs84 = c(xmin = 28.2068600040001, ymin =-31.2983299989999, xmax = 30.194719971, ymax = -30.0017999699999)  # replace ? with appropriate values
items = s |>
  stac_search(collections = "sentinel-s2-l2a-cogs",
              bbox = c(bbox_wgs84["xmin"], bbox_wgs84["ymin"],
                       bbox_wgs84["xmax"], bbox_wgs84["ymax"]), 
              datetime = "2022-01-01/2022-12-31",
              limit = 500) |>
  post_request()
items

names(items$features[[10]])
items$features[[10]]$assets$B05
items$features[[10]]$properties$`eo:cloud_cover`
s2_collection = stac_image_collection(items$features)
s2_collection

assets = c("B01","B02","B03","B04","B05","B06", "B07","B08","B8A","B09","B11","SCL")
s2_collection = stac_image_collection(items$features, asset_names = assets, property_filter = function(x) {x[["eo:cloud_cover"]] < 20})

s2_collection


# Adjust the spatial resolution to 10m 
cube_geom = cube_view(srs="EPSG:4326", 
                      dx=0.0001, dy=0.0001,  # 10m resolution for EPSG:4326
                      dt="P1Y", 
                      aggregation="median", 
                      resampling = "average",
                      extent=list(t0 = "2022-01-01", 
                                  t1 = "2022-12-31",
                                  left=bbox_wgs84[["xmin"]] - 0.0001,  
                                  right=bbox_wgs84[["xmax"]] + 0.0001,
                                  top=bbox_wgs84[["ymax"]] + 0.0001, 
                                  bottom=bbox_wgs84[["ymin"]] - 0.0001))


cube_geom

#cloud mask for Sentinel-2
S2.mask = image_mask("SCL", values = c(3,8,9))
gdalcubes_options(threads = 16)

# Define MSAVI color for visualization
msavi.col = function(n) {
  rev(sequential_hcl(n, "Blue-Red"))
}

# compute and plot the MSAVI
system.time({
  raster_cube(s2_collection, cube_geom, S2.mask) |>
    select_bands(c("B04", "B08")) |>  # Select necessary bands for MSAVI computation
    apply_pixel("(2 * B08 + 1 - sqrt((2 * B08 + 1)^2 - 8 * (B08 - B04))) / 2", "MSAVI") |>
    filter_pixel("MSAVI > 0") |>
    plot(key.pos = 1, zlim=c(0,1), col = msavi.col)
})













