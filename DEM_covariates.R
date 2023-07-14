library(elevatr)
library(viridis)
library(tidyverse)

# Read the shapefile of the study area and other related data
inpath<-"C:\\workspace\\Kirinyet-development\\data\\CSA_Baseline_SOC\\"
CAA<-st_read(paste0(inpath,"Milcah\\CSA_Conservation_Agreement_Areas.shp"))
CSA_NL <-st_read(paste0(inpath, "Milcah\\CSA_NAtional_Landscapes.shp"))
SOC_locations<-st_read(paste0(inpath, "Milcah\\SOC_GPS_Locations.shp"))

mzimvubu <- CSA_NL[CSA_NL$Name == "Mzimvubu Landscape", ] #subset NLA to mzimvubu
plot(mzimvubu["Name"], main= "Mzimvubu")

mzimvubu_sf <- as(mzimvubu, "sf")
#### Elevation,Apect,TWI,Slope ####
#https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html

mzimvubu_sp <- mzimvubu_sf[!st_is_empty(mzimvubu_sf$geometry),]#remove empty geometries
elevation <- elevatr::get_elev_raster(locations = mzimvubu_sp, z = 10)
plot(elevation, main="DEM (meters)")
plot(mzimvubu_sp,col="NA",border="black", add = TRUE)


slope = terrain(elevation,opt='slope', unit='degrees')
aspect = terrain(elevation,opt='aspect',unit='degrees')

#Plot slope
plot(slope,main="Slope for mzimvubu (degrees)", col=topo.colors(6,alpha=0.6))
plot(mzimvubu_sp, add=TRUE,col="NA",border="black", lwd=0.5)


# Calculate Topographic Wetness Index (TWI)
slope_radians <- terrain(elevation, opt = "slope", unit = "radians")
area <- terrain(elevation, opt = "TPI")
area <- exp(area)
twi <- log(area / tan(slope_radians))

# Plot Topographic Wetness Index
plot(twi, main="Topographic Wetness Index for mzimvubu", col=topo.colors(6, alpha=0.6))
plot(mzimvubu_sp, add=TRUE, col="NA", border="black", lwd=0.5)

