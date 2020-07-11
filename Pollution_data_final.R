library(eurostat)
library(readxl)
library(rgdal)


#---------------------------------shapefile eurostat nuts 3 level--------------------------------------------
spdf <- get_eurostat_geospatial(output_class = "spdf", resolution = "10", nuts_level = "3", year = "2013",cache = TRUE,
                                update_cache = FALSE, cache_dir = NULL)
oversea<-read_xlsx("less_developed.xlsx",4)
bad<-c("AL", "BA", "MK", "RS", "TR")#because of bad data quality in fdi Data

spdf <- spdf[!substr(spdf$id,1,4) %in% oversea$NUTS, ]
spdf <- spdf[!substr(spdf$id,1,4) %in% oversea$Expl, ]
spdf <- spdf[!spdf$CNTR_CODE %in% bad, ]


###-----------------------------------------------------pollution raster-----------------------------------------------
#---------------------------------------------------------pm25-----------------------------------------------------
library(raster)

#read in all data
setwd("./PM25")



brk <- lapply(list.files(path = "./", pattern = "tif$"), raster)
projection(brk[[1]])==projection(brk[[8]])

#projection
proj4string(spdf)
spdf<-spTransform(spdf, crs(brk[[8]]))



brk2<-lapply(brk,function(x) replace(x, x<=0, NA))# why now large list? because with raster not all of the data was loaded into memory and after the function this changed: see https://mgimond.github.io/Spatial/reading-and-writing-spatial-data-in-r.html
projection(brk2[[1]])

# brk[[8]][brk[[8]]>=60]<-NA #2015 große ausreißer nach oben wie damit umgehen? robustnes test?

brk<-lapply(brk2, function(x) extract(x, spdf, fun=mean,na.rm=TRUE, weights=FALSE))


sort(sapply(ls(),function(x){object.size(get(x))})) 


#save(brk, file="pm25.RData")


#----------------------------------------------------pm10-------------------------------------------

#setwd
setwd("..")
setwd("./PM10")

#read in all data
pm10 <- lapply(list.files(path = "./", pattern = "tif$"), raster)
projection(pm10[[1]])==projection(pm10[[10]])

#projection
proj4string(spdf)
spdf<-spTransform(spdf, crs(pm10[[1]]))

#NA dealing
brk2<-lapply(pm10,function(x) replace(x, x<=0, NA))
projection(brk2[[1]])

# instersection
pm102<-lapply(brk2, function(x) extract(x, spdf, fun=mean,na.rm=TRUE, weights=FALSE))

#save(pm102, file="pm102.RData")

#----------------------------------------------------o3-------------------------------------------
setwd("..")
setwd("./O393")

#read in all data
o3 <- lapply(list.files(path = "./", pattern = "tif$"), raster)
projection(o3[[1]])==projection(o3[[10]])#check same projection

#projection
proj4string(spdf)
spdf<-spTransform(spdf, crs(o3[[1]]))

# NA dealing
brk2<-lapply(o3,function(x) replace(x, x<=0, NA))
projection(brk2[[1]])

# intersection
o32<-lapply(brk2, function(x) extract(x, spdf, fun=mean,na.rm=TRUE, weights=FALSE))# schon ein kleiner unterschied wenn mit gewichte, überlegen ob nicht doch irendwie möglich sie mit einfließen zu lassen ##er<-o32[[8]]-o32w[[8]] #summary(er)

#save(o32, file="o32.RData")



##---------------------------------------------------correltion tests-------------------------------------

load("o32.RData")
load("pm25.RData")
load("pm10.RData")


# spdf$pm2509 <- Reduce("+",brk[c(1:2)])/length(brk[c(1:2)])
# spdf$pm2515 <- Reduce("+",brk[-c(1,2)])/length(brk[-c(1,2)])
# spdf$pm25152 <- Reduce("+",brk[-c(1,2,7,8)])/length(brk[-c(1,2,7,8)])#fast genau selber wert wie bei variante 1 


# spdf$pm1009 <- Reduce("+",pm102[c(1:4)])/length(pm102[c(1:4)])
# spdf$pm1015 <- Reduce("+",pm102[-c(1:4)])/length(pm102[-c(1:4)])
# spdf$pm10152 <- Reduce("+",pm102[-c(1,2,3,4,9,10)])/length(pm102[-c(1,2,3,4,9,10)])#fast genau selber wert wie bei variante 1 

# spdf$o309 <- Reduce("+",o32[c(1:4)])/length(o32[c(1:4)])
# spdf$o315 <- Reduce("+",o32[-c(1:4)])/length(o32[-c(1:4)])
# spdf$o3152 <- Reduce("+",o32[-c(1,2,3,4,9,10)])/length(o32[-c(1,2,3,4,9,10)])#fast genau selber wert wie bei variante 1, allerdings hier die korrelation fast noch am unterschiedlichsten

# cor.mat<-cor(spdf@data[,-c(1:7)], method = c("pearson"))
# 
# cols = rev(terrain.colors(255))
# spplot(spdf, "pm1015", col.regions=cols, lwd=0)
# spplot(spdf, "pm2515", col.regions=cols, lwd=0)

com15<-cbind(brk[[8]], pm102[[10]], o32[[10]])
com14<-cbind(brk[[7]], pm102[[9]], o32[[9]])

cor.mat15<-cor(com15, method = c("spearman"))
cor.mat14<-cor(com14, method = c("spearman"))

