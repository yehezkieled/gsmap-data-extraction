rm(list=ls(all=TRUE))
library(sp)               #library for spatial data
library(raster)           #library for raster data
library(tidyverse)
library(R.utils)          #unzipping files

load(paste0("F:\\Dataset\\Pixel File", "\\final.pixel.10km.Indonesia.RData"))

read_gsmap <- function(fname, nvals){
  ## gsmap daily world data is 4 byte little endian float
  on.exit(close(flcon))
  flcon <- file(fname, 'rb')
  readBin(flcon, "double", n = nvals, endian = "little", size = 4)
}

coordinates.in.use <- coordinates(cbind(
  final.pixel.10km.Indonesia$village.pixel.name.longitude,
  final.pixel.10km.Indonesia$village.pixel.name.latitude
))

get.raster <- function(GSMaP.data){
  nrow <- 1200
  ncol <- 3600
  data.dat.file <- matrix(read_gsmap(GSMaP.data, (nrow * ncol)),
                          nrow = nrow,
                          ncol = ncol,
                          byrow = TRUE)
  raster.dat.file <- raster(data.dat.file,
                            xmn = -180,
                            xmx = 180,
                            ymn = -60,
                            ymx = 60,
                            crs = CRS("+init=epsg:4326"))
  
  cell.index <- cellFromXY(raster.dat.file, coordinates.in.use)
  data.extracted <- t(raster :: extract(raster.dat.file, cell.index))
  
  # return(get.sum.pixels(data.extracted))
  return(data.extracted)
}

file.check <- function(gs.file, year, month){
  if(nchar(gs.file) == 59){
    dat.file <- gunzip(paste0(wd.year,
                       "/", year,
                       "/", month,
                       "/", gs.file))
    return(dat.file)
  } else {
    dat.file <- paste0(wd.year,
                       "/", year,
                       "/", month,
                       "/", gs.file)
    return(dat.file)
  }
}


final.data <- data.frame(final.pixel.10km.Indonesia$village.pixel.name.PIxelName)
final.data <- t(final.data)

wd.year <- "F:/Dataset/GSMaP/sample GSMaP"
list.year <- list.files(wd.year)

for(i in 1:length(list.year)){
  wd.month <- paste0(wd.year,
                     "/",
                     list.year[i])
  list.month <- list.files(wd.month)
  
  for(j in 1:length(list.month)){
    wd.day <- paste0(wd.month,
                     "/",
                     list.month[j])
    list.day <- list.files(wd.day)
    # final.data.per.day <- data.frame() 
    
    for(k in 1:length(list.day)){
      cat("year=",
          list.year[i],
          "month=",
          list.month[j],
          "files=",
          list.day[k],
          "\n")
      
      dat.file <- file.check(list.day[k],
                             list.year[i],
                             list.month[j])         #unzipping the .dat files
      # final.data.per.day <- rbind(final.data.per.day,
      #                             get.raster(dat.file))
      # final.data <- rbind(final.data, final.data.per.day)
      final.data <- rbind(final.data,
                          get.raster(dat.file))

      rownames(final.data)[dim(final.data)[1]] <- paste0(list.year[i] ," - ", list.month[j], " - ", k)
    }
  }
}

colnames(final.data) <- final.data[1,]
final.data <- final.data[-1,]

write.csv(final.data, paste0(getwd(), "/GSMaP_data.csv"))
