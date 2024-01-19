###The script used for correlation analysis of pointiness index and environmental variables
##Author: Aleksandar Stanojković (Palacký University Olomouc)
#For any issues/questions contact at aleksandar.stanojkovicdbe@gmail.com

Pointiness <- read_excel("Pointiness.xlsx") #this file had values of pointiness index
All.variables <- read.csv("E:/Microcoleus/All variables.csv")
point <- as.numeric(Pointiness$`Pointiness mean`)

#transform columns into numberic vectors
variables <- All.variables[, -1] #to remove first column with strains' names
variables <- lapply(variables, as.numeric)
cor.results <- sapply(variables,
                      function(x) cor.test(point,x))
write.csv(cor.results, "correlation_results.csv")

#Extract values for P concentration in soil using database published in 10.1038/s41597-023-02022-4
#First, we need to convert our coordinates into the coordinate system used for the database, which is in tif file.
#coords_Microcoleus file has all the coordinates for the extraction of values

d <- data.frame(lon=coords_Microcoleus$Longitude, lat=coords_Microcoleus$Latitude)
coordinates(d) <- c("lon", "lat")
proj4string(d) <- CRS("EPSG:4326")
CRS.new <- CRS("ESRI:54034")
transformed_coords <- spTransform(d, CRS.new)
write.csv(transformed_coords, "transformed_coords.csv")

Transformed_coords <- read.csv("E:/Microcoleus/transformed_coords.csv")
raster_olsen <- list.files("./Olsen/", pattern = 'tif$', full.names = TRUE)
olsen <- stack(raster_olsen)
olsen_bio <- extract(olsen, Transformed_coords[, 2:3])
olsen_P <- as.numeric(olsen_bio)
cor.test(point, olsen_P) #cor -0.1752957; p-value = 0.02614

#Extract values for average temperature (layer available at WorldClim2)
files_avgT <- list.files("./average_temperature/", pattern = 'tif', full.names = TRUE)
bio_avgT <- stack(files_avgT)
avgT <- extract(bio_avgT, coords_Microcoleus[,2:3])
cor.test(point, avgT) #cor 0.3644303; p-value 4.92E-07

