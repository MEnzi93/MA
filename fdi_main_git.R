library(eurostat)
library(readxl)
library(rgdal)
library(dplyr)
library(tidyr)
library(readODS)

setwd("C:/WU/Master/AAMasterarbeit/mögliche Themen/FDI")

# ----------------------------------------------------Functions

AICsplm = function(object, k=2, criterion=c("AIC", "BIC")){ 
  sp = summary(object)
  l = sp$logLik
  np = length(coef(sp))
  N = nrow(sp$model)
  if (sp$effects=="sptpfe") {
    n = length(sp$res.eff[[1]]$res.sfe) 
    T = length(sp$res.eff[[1]]$res.tfe) 
    np = np+n+T
  }
  if (sp$effects=="spfe") {
    n = length(sp$res.eff[[1]]$res.sfe)
    np = np+n+1 
  }
  if (sp$effects=="tpfe") {
    T = length(sp$res.eff[[1]]$res.tfe)
    np = np+T+1
  }
  if (criterion=="AIC"){
    aic = -2*l+k*np
    names(aic) = "AIC"
    return(aic)
  }
  if (criterion=="BIC"){
    bic = -2*l+log(N)*np
    names(bic) = "BIC"
    return(bic)
  } 
}
#© 2020 GitHub, Inc.
#https://github.com/rfsaldanha/ecoespacialunicamp/blob/master/AICsplm.R


AICplm <- function(mod){
  # Number of observations
  n.N   <- nrow(mod$model)
  # Residuals vector
  u.hat <- residuals(mod)
  # Variance estimation
  s.sq  <- log( (sum(u.hat^2)/(n.N)))
  # Number of parameters (incl. constant) + one additional for variance estimation
  p     <-  length(coef(mod)) + 1
  
  # Note: minus sign cancels in log likelihood
  aic <- 2*p  +  n.N * (  log(2*pi) + s.sq  + 1 ) 
  
  return(aic)
}
#https://github.com/rfsaldanha/ecoespacialunicamp/blob/master/AICplm.R

ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}


#cite gtools weil function von ihnen?
stars.pval <- function(p.value)
{
  unclass(
    symnum(p.value, corr = FALSE, na = FALSE,
           cutpoints = c(0,0.01, 0.05, 0.1, 1),
           symbols = c("***", "**", "*", " "))
  )
}

#---------------------------------shapefile eurostat nuts 3 level--------------------------------------------
# spdf <- get_eurostat_geospatial(output_class = "spdf", resolution = "10", nuts_level = "3", year = "2013",cache = TRUE,update_cache = FALSE, cache_dir = NULL)
load("./Data/spdf1032013.RData")

spdf<-shp
remove(shp)
#load("C:/Users/Enzinger/AppData/Local/Temp/Rtmp4sx5Xr/eurostat/spdf1032013.RData")
# spdf<-shp
# remove(shp)
oversea<-read_xlsx("C:/WU/Master/AAMasterarbeit/mögliche Themen/Eu_structural_investment/less_developed.xlsx",4)
bad<-c("AL", "BA", "MK", "RS", "TR")#because of bad data quality in fdi Data

spdf <- spdf[!substr(spdf$id,1,4) %in% oversea$NUTS, ]
spdf <- spdf[!substr(spdf$id,1,4) %in% oversea$Expl, ]
spdf <- spdf[!spdf$CNTR_CODE %in% bad, ]

##########----------------------------------------covariates----------------------------------------------------
start<-2003
end<-2015

#fdi

csvs <- c(
  "./Data/main-data/ind_400_b09-vfdi_data.csv",
  "./Data/main-data/ind_401_a10-vfdi_data.csv",
  "./Data/main-data/ind_403_man-vfdi_data.csv",
  "./Data/main-data/ind_404_ser-vfdi_data.csv",
  "./Data/main-data/ind_405_oth-vfdi_data.csv"
  )
name<-c("2009","2015","man","ser","oth")

fdidat<-lapply(csvs,read.csv)
fdidat<-lapply(fdidat, function(x) x[,c(7,11)])

for(i in 1:5){
  colnames(fdidat[[i]])[2] <- name[i]
}

fdidat<-Reduce(full_join,fdidat)

fdidat<-fdidat%>%gather(year,value,2:3)%>%filter(tunit_code %in% unique(spdf$id))
#fdidat$year<-recode(fdidat$year, y09="2009",y15="2015")

fdidat$man<-fdidat$man/2
fdidat$ser<-fdidat$ser/2
fdidat$oth<-fdidat$oth/2

# low quality of intra european fdis: Albania, Bosnia and Herzegovina, The former Yugoslavian Republic of Macedonia (fyROM), Serbia and Turkey
#AL, BA, MK, RS, TR


#-----------------------------------2013 cam 
#Daten auch heruntergeladen
urls <- c(
  "https://urban.jrc.ec.europa.eu/rel2018/static/data/indicators/csv/cepop_nuts3.csv",
  "https://urban.jrc.ec.europa.eu/rel2018/static/data/indicators/csv/cegdptotal_nuts3.csv",
  "https://urban.jrc.ec.europa.eu/rel2018/static/data/indicators/csv/cegvaagri_nuts3.csv",
  "https://urban.jrc.ec.europa.eu/rel2018/static/data/indicators/csv/cegvaconstr_nuts3.csv",
  "https://urban.jrc.ec.europa.eu/rel2018/static/data/indicators/csv/cegvafbs_nuts3.csv",
  "https://urban.jrc.ec.europa.eu/rel2018/static/data/indicators/csv/cegvaind_nuts3.csv",
  "https://urban.jrc.ec.europa.eu/rel2018/static/data/indicators/csv/cegvanonmrkt_nuts3.csv",
  "https://urban.jrc.ec.europa.eu/rel2018/static/data/indicators/csv/cegvawrtafic_nuts3.csv",
  "https://urban.jrc.ec.europa.eu/rel2018/static/data/indicators/csv/cegvatotal_nuts3.csv",
  "https://urban.jrc.ec.europa.eu/rel2018/static/data/indicators/csv/ceemptotal_nuts3.csv",
  "https://urban.jrc.ec.europa.eu/rel2018/static/data/indicators/csv/ceempagri_nuts3.csv",
  "https://urban.jrc.ec.europa.eu/rel2018/static/data/indicators/csv/ceempconstr_nuts3.csv",
  "https://urban.jrc.ec.europa.eu/rel2018/static/data/indicators/csv/ceempfbs_nuts3.csv",
  "https://urban.jrc.ec.europa.eu/rel2018/static/data/indicators/csv/ceempind_nuts3.csv",
  "https://urban.jrc.ec.europa.eu/rel2018/static/data/indicators/csv/ceempnonmrkt_nuts3.csv",
  "https://urban.jrc.ec.europa.eu/rel2018/static/data/indicators/csv/ceempwrtafic_nuts3.csv"
)

name<-c("pop","rgdp","agri","constr","fbs","ind","nonmarkt","wrtafic","total","lptotal","lpagri","lpconstr","lpfbs","lpind","lpnonmrkt","lpwrtafic")

cam<-lapply(urls,read.csv)
cam<-lapply(cam, function(x) x[,-c(2,3)])

for(i in 1:length(name)){
  colnames(cam[[i]])[3] <- name[i]
}
cam<-Reduce(full_join,cam)

#sum(is.na(cam %>% filter(REF_YEAR>=start & REF_YEAR<=end)))

cam<-cam %>% filter(REF_YEAR>=start & REF_YEAR<=end) %>% mutate(agri=agri/total,constr=constr/total,fbs=fbs/total,ind=ind/total,nonmarkt=nonmarkt/total,wrtafic=wrtafic/total,lptotal=rgdp/lptotal,lpagri=agri/lpagri,lpconstr=constr/lpconstr,lpfbs=fbs/lpfbs,lpind=ind/lpind,lpnonmrkt=nonmarkt/lpnonmrkt,lpwrtafic=wrtafic/lpwrtafic)

cam1<-cam %>% group_by(NUTS_CODE) %>% filter(REF_YEAR>=2010) %>% summarize_all(funs(mean(., na.rm = FALSE)))
cam2<-cam %>% group_by(NUTS_CODE) %>% filter(REF_YEAR<2010) %>% summarize_all(funs(mean(., na.rm = FALSE)))
cam<-rbind(cam2,cam1)#[,-ncol(cam2)]

cam<- na.omit(cam)

cam$REF_YEAR<-recode(cam$REF_YEAR, "2006"="2009","2012.5"="2015")

#join variables 

data<-cam %>% left_join(fdidat,  by=c("NUTS_CODE"="tunit_code","REF_YEAR"="year"))
# IS,NO,CH,ME,LI fehlen weil nicht in EU # später vielleicht noch von Eurostat holen 


#-----------------------------------------calculate square kilometers-------------------------------------
#https://stackoverflow.com/questions/48511813/use-polygon-area-to-calculate-the-population-density-in-r
library(geosphere)
# store each polygon's boundaries
list.of.polygon.boundaries <- sapply( X = slot( object = spdf, name = "polygons" )
                                      , FUN = function(i) 
                                        sp::coordinates( obj = slot( object = i, name = "Polygons")[[1]] )
)

# calculate each polygon's area in square kilometers
spdf@data$Square_kilometer <- sapply( X = list.of.polygon.boundaries
                                      , FUN = geosphere::areaPolygon)/1000000

# View the first six results
head(
  spdf@data[ c( "id", "Square_kilometer" ) ]
)# anhand von Burgenland überprüft, passt fast genau, 5 km² unterschied

data<-data %>% left_join(spdf@data[c("id", "Square_kilometer")],  by=c("NUTS_CODE"="id"))

#gArea(spdf, byid=TRUE)#falsche Werte?

#--------------------------------pollution

load("./Data/o32.RData")
load("./Data/pm25.RData")
load("./Data/pm10.RData")

pollu<-rbind(
  cbind(spdf@data[1],pm25=Reduce("+",brk[c(1:2)])/length(brk[c(1:2)]),pm10 = Reduce("+",pm102[c(1:4)])/length(pm102[c(1:4)]),o3=Reduce("+",o32[c(1:4)])/length(o32[c(1:4)]),year=2009),
  
  cbind(spdf@data[1],pm25=Reduce("+",brk[-c(1,2)])/length(brk[-c(1,2)]),pm10 = Reduce("+",pm102[-c(1:4)])/length(pm102[-c(1:4)]),o3=Reduce("+",o32[-c(1:4)])/length(o32[-c(1:4)]),year=2015))


data$REF_YEAR<-as.numeric(data$REF_YEAR)
data<-data %>% left_join(pollu,  by=c("NUTS_CODE"="id","REF_YEAR"="year"))
colnames(data)[1:2]<-c("nuts","year")



#---------------------------------------weights matrix--------------------------------------------------------
# n_occur <- data.frame(table(data$nuts))
# n_occur[n_occur$Freq = 1,]

#ich verliere anscheinend wien durch die labour productivity variablen, vielleicht auch noch andere Städte? ja ich verliere allgemein recht viele Städte, von 7 zufällig ausgewählten gibt es nur 3 

data<-data[!data$nuts == "UKI72",]#the only region which just apears once 

#plot(spdf)
spdf <- spdf[spdf$id %in% data$nuts, ]
# setdiff(unique(data$nuts),spdf$id)#to see if we have the same entrys for both data
data<-data[data$nuts %in% spdf$id,]
#W.matrix

library(spdep)

coords <-coordinates(spdf)
distw.tot <- dnearneigh(coords,0,Inf, row.names = spdf$id)
dnbdist.tot <- nbdists(distw.tot, coords)
gl.tot <- lapply(dnbdist.tot, function(x) 1/x^2) #calculating inverse distances and take x^2 because we want to put more weight on closer neighbours
W.list.inv <- nb2listw(distw.tot, glist=gl.tot, zero.policy=FALSE, style = "W")
W.dis<-listw2mat(W.list.inv)

# gl.tot <- lapply(dnbdist.tot, function(x) 1/x) #calculating inverse distances and take x^2 because we want to put more weight on closer neighbours
# W.list.inv1 <- nb2listw(distw.tot, glist=gl.tot, zero.policy=FALSE, style = "W")
# W.dis1<-listw2mat(W.list.inv)

k.near <- knearneigh(coords, k=7) #indexing neighbors based on k=5
k7 <- knn2nb(k.near) #creating neighborhood list based on the k(5) nearest neighbors
W.list.7 <- nb2listw(k7, style = "W", zero.policy = FALSE) 

test<-diag(1289)
test_w<-cbind(0,test)
test_w<-test_w[,-1290]
#test_w<-test_w+t(test_w) variante 2
test_w[1289,1288]<-1# better than the other, but interpretation?
#test_w[2:1288,]<-test_w[2:1288,]/2

W.list.1<-mat2listw(test_w)



data<-data[order(data$year,match(data$nuts,rownames(W.dis))),]
data$country<-substr(data$nuts,1,2)




remove(distw.tot)
remove(dnbdist.tot)
remove(gl.tot)
remove(brk)
remove(o32)
remove(pm102)
remove(bad)
remove(cam)
remove(cam1)
remove(cam2)
#remove(coords)
remove(pollu)
remove(list.of.polygon.boundaries)
remove(fdidat)
#remove(oversea)
remove(csvs)
remove(urls)

#-----------------------------------------------Descriptives-------------------------------------------------
#---------necessacry data manipulation

head(data)
fmall<-log(pm10)~ihs(value/pop)+I(ihs(value/pop)^2)+log(rgdp/pop)+I(log(rgdp/pop)^2)+log(pop/Square_kilometer)+I(log(pop/Square_kilometer)^2)+log(ind)+log(fbs)+log(agri)+log(constr)+log(wrtafic)+log(nonmarkt)+log(lptotal)+log(lpind)+log(lpfbs)+log(lpconstr)+log(lpwrtafic)+log(lpnonmrkt)+log(o3)+I(as.numeric(year)*log(Square_kilometer))+I(as.numeric(year)*log(Square_kilometer)^2)



library(plm)

fd<-plm(fmall, data, model = "fd")

#---------first difference
d<-c(1,-1)
d<-as.vector(d)
dim(d) <- c(1,2)
ident<-diag(length(unique(data$nuts)))
D<-kronecker(ident,d)

x<-as.matrix(cbind(1,fd$model))#der 1er ist unnötig

trans<-(-1)*D%*%x
trans<-as.data.frame(trans)

dim(D)

head(trans)
colnames(trans)<-c("b0","pm10","fdi","fdi2","gdp","gdp2","popdens","popdens2","ind","fbs","agri","constr","wrtafic","nonmarkt","lptotal","lpind","lpfbs","lpconstr","lpwrtafic","lpnonmrkt","o3","sqkil","sqkil2")

fmfd<-pm10~fdi+fdi2+gdp+gdp2+popdens+popdens2+ind+fbs+agri+constr+wrtafic+nonmarkt+lptotal+lpind+lpfbs+lpconstr+lpwrtafic+lpnonmrkt+o3+sqkil+sqkil2

fd1<-lm(fmfd, data=trans)

summary(fd)
summary(fd1)

trans$nuts<-data$nuts[1:(nrow(data)/2)]
trans$country<-data$country[1:(nrow(data)/2)]


#--------------------data descriptives
fmall<-log(pm10)~ihs(value/pop)+I(ihs(value/pop)^2)+log(rgdp/pop)+I(log(rgdp/pop)^2)+log(pop/Square_kilometer)+log(ind)+log(fbs)+log(agri)+log(constr)+log(wrtafic)+log(nonmarkt)+log(lpind)+log(lpfbs)+log(lpconstr)+log(lpwrtafic)+log(lpnonmrkt)+log(o3)+I(as.numeric(year)*log(Square_kilometer))+I(as.numeric(year)*log(Square_kilometer)^2)


plot(density(ihs(data[data$value!=0,]$value)))#if we exclude those with an fdi value of zero
plot(density(trans$pm10))
plot(density(trans$gdp))
plot(density(trans$ind))
plot(density(trans$popdens))
plot(density(trans$fbs))
plot(density(trans$lptotal))
plot(density(trans$lpind))
plot(density(trans$sqkil))
plot(density(trans$o3))
plot(density(data$wrtafic))



#--------------------test for multicolinearity 
#multicolineraity test as in The emissions reduction effect and technical progress effect ofenvironmental regulation policy tools S.6

fmfd1<-pm10~fdi+fdi2+gdp+gdp2+popdens+popdens2+ind+fbs+agri+constr+wrtafic+nonmarkt+lptotal+lpind+lpfbs+lpconstr+lpwrtafic+lpnonmrkt+o3+sqkil+sqkil2
fmfd2<-pm10~fdi+fdi2+gdp+gdp2+popdens+ind+fbs+agri+constr+wrtafic+nonmarkt+lpind+lpfbs+lpconstr+lpwrtafic+lpnonmrkt+o3+sqkil+sqkil2

fd2<-lm(fmfd1, data=trans)
fd3<-lm(fmfd2, data=trans)

library(car)
#car::vif(Ols)#Gdp extrem hoch, laut oberen Paper sollte unter 10 sein# #how vif is calculated: 1/(1-R2) see https://statisticalhorizons.com/multicollinearity # auf website steht wenn x und x^2 inkludiert dann wahrscheinlich und ok wenn hoch# ist ok weil keine negativen konsequenzen laut dem Beitrag

#Bei kontrollvariablen ist Mulitcolineratiy less a of a problem
# in unserem fall ist daher vor allem Fdi von hoher relevanz bei der Interpretation, bei den anderen erwähnen dass die VIF teilweise höher ist (über 2.5) bei gdp und wrtafic zum beispiel und daher die ergbenisse mit vorsicht zu genießen sind

#nur relevant für gediffte varialen
car::vif(fd1)# lptotal sehr hohe vif # ansonsten passt es aber sehr gut und liegt nur an den squared terms der variablen
car::vif(fd3)#ohne vif wird auch die von den anderen vif besser
# unter umständen auch für IV Regression testen 


#---------------------Pearson correlation

cor.mat<-cor(trans[,-1], method = c("pearson"))
cor.mat

library(xtable)
xtable(cor.mat)

#--------------------spatial structure
#---------global MI 


#pm
mi_pm09<-moran.test(log(data$pm10[1:(nrow(data)/2)]), listw = W.list.inv, alternative = "greater", randomisation = FALSE)
mi_pm15<-moran.test(log(data$pm10[(nrow(data)/2+1):(nrow(data))]), listw = W.list.inv, alternative = "greater", randomisation = FALSE)
mi_pmdiff<-moran.test(log(data$pm10[1:(nrow(data)/2)])-log(data$pm10[(nrow(data)/2+1):(nrow(data))]), listw = W.list.inv, alternative = "greater", randomisation = FALSE)



#fdi
mi_fi09<-moran.test(ihs(data$value[1:(nrow(data)/2)]), listw = W.list.inv, alternative = "greater", randomisation = FALSE)
mi_fi15<-moran.test(ihs(data$value[(nrow(data)/2+1):(nrow(data))]), listw = W.list.inv, alternative = "greater", randomisation = FALSE)
mi_fidiff<-moran.test(ihs(data$value[1:(nrow(data)/2)])-ihs(data$value[(nrow(data)/2+1):(nrow(data))]), listw = W.list.inv, alternative = "greater", randomisation = FALSE)


#gdp
mi_gd09<-moran.test(log(data$rgdp[1:(nrow(data)/2)]), listw = W.list.inv, alternative = "greater", randomisation = FALSE)
mi_gd15<-moran.test(log(data$rgdp[(nrow(data)/2+1):(nrow(data))]), listw = W.list.inv, alternative = "greater", randomisation = FALSE)
mi_gddiff<-moran.test(log(data$rgdp[1:(nrow(data)/2)])-log(data$rgdp[(nrow(data)/2+1):(nrow(data))]), listw = W.list.inv, alternative = "greater", randomisation = FALSE)

#o3
mi_o309<-moran.test(log(data$o3[1:(nrow(data)/2)]), listw = W.list.inv, alternative = "greater", randomisation = FALSE)
mi_o315<-moran.test(log(data$o3[(nrow(data)/2+1):(nrow(data))]), listw = W.list.inv, alternative = "greater", randomisation = FALSE)
mi_o3diff<-moran.test(log(data$o3[1:(nrow(data)/2)])-log(data$o3[(nrow(data)/2+1):(nrow(data))]), listw = W.list.inv, alternative = "greater", randomisation = FALSE)

#residuals
mi_res09<-moran.test(Ols$residuals[1:(nrow(data)/2)], listw = W.list.inv, alternative = "greater", randomisation = FALSE)
mi_res15<-moran.test(Ols$residuals[(nrow(data)/2+1):(nrow(data))], listw = W.list.inv, alternative = "greater", randomisation = FALSE)
mi_resdiff<-moran.test(fd1$residuals, listw = W.list.inv, alternative = "greater", randomisation = FALSE)



mi_pos<-function(obj,obj1,obj2,name){
  name<-cbind(paste0(round(obj$estimate[1],3),stars.pval(obj$p.value)),round(obj$statistic,3),paste0(round(obj1$estimate[1],3),stars.pval(obj1$p.value)),round(obj1$statistic,3),paste0(round(obj2$estimate[1],3),stars.pval(obj2$p.value)),round(obj2$statistic,3))
return(name)
}

mi_pm<-mi_pos(mi_pm09,mi_pm15,mi_pmdiff)
mi_fi<-mi_pos(mi_fi09,mi_fi15,mi_fidiff)
mi_gd<-mi_pos(mi_gd09,mi_gd15,mi_gddiff)
mi_o3<-mi_pos(mi_o309,mi_o315,mi_o3diff)
mi_res<-mi_pos(mi_res09,mi_res15,mi_resdiff)


mi<-rbind(mi_pm,mi_fi,mi_gd,mi_o3,mi_res)
rownames(mi)<-c("$PM_{10}$","$FDI$","$GDP$","$O_3$","$RES$")
mi<-rbind(c("Moran’s I","Z-value"),mi)
library(xtable)
tabl<-xtable(mi, caption = "Moran´s I for some variables of Interest")
print(tabl)

addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- paste0(paste0('& \\multicolumn{2}{c}{', c("2003-2009","2010-2015","Differnce"), '}', collapse=''), '\\\\')
align(tabl) <- rep("l",7)
print(tabl, add.to.row=addtorow, include.colnames=F,sanitize.text.function=function(x){x},caption.placement = "top")

#-----------local MI


localI <- localmoran(trans$pm10, listw = W.list.inv, alternative = "greater")
View(localI)


p.adjust(localI[,5], method = "bonferroni")#always do that if we test a lot of hypothesis simultaneously


par(mfrow = c(1, 2))


library(GISTools)
sids79.shading <- auto.shading(c(localI[,1], -localI[,1]),n=4,
                               cols=brewer.pal(4, "PuOr"))
choropleth(spdf, localI[,1], shading=sids79.shading, main="Local Moran´s I for Difference in Pm Pollution",border=1, lwd = 0.1,lty=0)
#locator(1)

choro.legend(-10.42525,  70.69808, sh=sids79.shading, cex=0.6, title="Local Moran's I")

count <- get_eurostat_geospatial(output_class = "spdf", resolution = "10", nuts_level = "0", year = "2013",cache = TRUE,update_cache = FALSE, cache_dir = NULL)

count <- count[!substr(count$id,1,4) %in% oversea$NUTS, ]
count <- count[!substr(count$id,1,4) %in% oversea$Expl, ]
count <- count[count$CNTR_CODE %in% unique(trans$country), ]
plot(count,add=TRUE)


localI_fdi <- localmoran(trans$fdi, listw = W.list.inv, alternative = "greater")
p.adjust(localI_fdi[,5], method = "bonferroni")#always do that if we test a lot of hypothesis simultaneously

sids79.shading <- auto.shading(c(localI_fdi[,1], -localI_fdi[,1]),n=4,
                               cols=brewer.pal(4, "PuOr"))
choropleth(spdf, localI_fdi[,1], shading=sids79.shading, main="Local Moran´s I for Difference in FDI",border=1, lwd = 0.1,lty=0)


choro.legend(-10.42525, 70.69808, sh=sids79.shading, cex=0.6, title="Local Moran's I")
plot(count,add=TRUE)



localI_gdp <- localmoran(trans$gdp, listw = W.list.inv, alternative = "greater")
p.adjust(localI_gdp[,5], method = "bonferroni")#always do that if we test a lot of hypothesis simultaneously

sids79.shading <- auto.shading(c(localI_gdp[,1], -localI_gdp[,1]),n=4,
                               cols=brewer.pal(4, "PuOr"))
choropleth(spdf, localI_gdp[,1], shading=sids79.shading, main="Local Moran´s I for Difference in GDP",border=1, lwd = 0.1,lty=0)


choro.legend(-10.42525, 70.69808, sh=sids79.shading, cex=0.6, title="Local Moran's I")
plot(count,add=TRUE)

localI_res <- localmoran(fd1$residuals, listw = W.list.inv, alternative = "greater")
p.adjust(localI_res[,5], method = "bonferroni")#always do that if we test a lot of hypothesis simultaneously

sids79.shading <- auto.shading(c(localI_res[,1], -localI_res[,1]),n=4,
                               cols=brewer.pal(4, "PuOr"))
choropleth(spdf, localI_res[,1], shading=sids79.shading, main="Local Moran´s I for the Residuals of the FD Model",border=1, lwd = 0.1,lty=0)

choro.legend(-10.42525, 70.69808, sh=sids79.shading, cex=0.6, title="Local Moran's I")
plot(count,add=TRUE)


#-------------GWR
#scheint allerdings nicht wirklich sinnvoll da der estimator von ols auch nicht signifikant ist, plott sieht auch so aus
# mit fd daten allerdings super 
fmfd<-pm10~fdi+gdp+gdp2+popdens+ind+fbs+agri+constr+wrtafic+nonmarkt+lpind+lpfbs+lpconstr+lpwrtafic+lpnonmrkt+o3+sqkil+sqkil2
summary(lm(fmfd,trans))
library(spgwr)
coords <-coordinates(spdf)
bwG <- gwr.sel(fmfd, data = trans, gweight = gwr.Gauss,verbose = TRUE,coords = coords)
gwr1<-gwr(fmfd,data = trans, coords = coords, bandwidth = bwG, hatmatrix=TRUE, se.fit=TRUE)
gwr1



x<-as.data.frame(gwr1$SDF)
spdf$beta_fdi<-x[,"fdi"]
spdf$beta_fdi2<-x[,"fdi2"]


par(mfrow = c(1, 2))

sids79.shading <- auto.shading(c(-spdf$beta_fdi,spdf$beta_fdi),n=5,
                               cols=brewer.pal(5, "RdYlGn"))
#sids79.shading$cols
#display.brewer.pal(11, "RdYlGn")
#sids79.shading$cols<-c("#D73027","#FDAE61","#D9EF8B","#A6D96A","#1A9850")
sids79.shading$cols<-c("#1A9850","#A6D96A","#D9EF8B","#FDAE61","#D73027")
choropleth(spdf, spdf$beta_fdi, shading=sids79.shading, main="Beta FDI",border=1, lwd = 0.1,lty=0)
#locator(1) allows us to select one point at this coordinates
choro.legend(-13.50322,  70.918, sh=sids79.shading, cex=0.6, title="Beta FDI",fmt="%1.3f")
plot(count,add=TRUE)

sids79.shading <- auto.shading(c(spdf$beta_fdi2,-spdf$beta_fdi2),
                               cols=brewer.pal(5, "RdYlGn"))
sids79.shading$cols<-c("#1A9850","#A6D96A","#D9EF8B","#FDAE61","#D73027")
choropleth(spdf, spdf$beta_fdi2, shading=sids79.shading, main="Beta FDI²",border=1, lwd = 0.1,lty=0)
#locator(1) allows us to select one point at this coordinates
choro.legend(-13.50322,  70.918, sh=sids79.shading, cex=0.6, title="Beta FDI²",fmt="%1.3f")
plot(count,add=TRUE)




sigTest<- abs(x$fdi) -2 * x$fdi_se 

spdf$ind_Sig<-sigTest
sids79.shading <- auto.shading(spdf$ind_Sig,n=4,quantileCuts(spdf$ind_Sig,n=2,params = c(.87892)),
                               cols=brewer.pal(4, "Reds"))
sids79.shading$breaks[3]<-0
sids79.shading$cols<-c("#FC9272","#FCBBA1","#FEE0D2","#2171B5")
choropleth(spdf, spdf$ind_Sig, shading=sids79.shading, main="FDI",border=1, lwd = 0.1,lty=0)
#locator(1) allows us to select one point at this coordinates
choro.legend(-11.10322,  70.918, sh=sids79.shading, title="Significance FDI", cex=0.6,fmt="%1.2f")
plot(count,add=TRUE)


sigTest<- abs(x$fdi2) -2 * x$fdi2_se 

spdf$ind_Sig2<-sigTest
sids79.shading <- auto.shading(spdf$ind_Sig2,n=4,
                               cols=brewer.pal(4, "Reds"))
sids79.shading$breaks[3]<-0
sids79.shading$cols<-c("#FC9272","#FCBBA1","#FEE0D2","#2171B5")
choropleth(spdf, spdf$ind_Sig2, shading=sids79.shading, main="FDI²",border=1, lwd = 0.1,lty=0)
#locator(1) allows us to select one point at this coordinates
choro.legend(-13.50322,  70.918, sh=sids79.shading, title="Significance FDI²", cex=0.6,fmt="%1.3f")
plot(count,add=TRUE)




#------------------------------------------------estimation----------------------------------------------




fmall<-log(pm10)~ihs(value/pop)+I(ihs(value/pop)^2)+log(rgdp/pop)+I(log(rgdp/pop)^2)+log(pop/Square_kilometer)+log(ind)+log(fbs)+log(agri)+log(constr)+log(wrtafic)+log(nonmarkt)+log(lpind)+log(lpfbs)+log(lpconstr)+log(lpwrtafic)+log(lpnonmrkt)+log(o3)+I(as.numeric(year)*log(Square_kilometer))+I(as.numeric(year)*log(Square_kilometer)^2)



Ols<-lm(fmall, data)
summary(Ols)

plot(residuals(Ols))
acf(residuals(Ols))

library(plm)

plm<-plm(fmall, data, model = "within", effect = "twoways")
fd1<-plm(fmall, data, model = "fd")
Ols<-plm(fmall, data, model = "pooling")
summary(plm)
summary(fd)



#------------spatial tests
library(splm)

fm10<-log(pm10)~ihs(value/pop)+I(ihs(value/pop)^2)+log(rgdp/pop)+I(log(rgdp/pop)^2)+log(pop/Square_kilometer)+I(log(pop/Square_kilometer)^2)+log(ind)+log(fbs)+log(agri)+log(constr)+log(wrtafic)+log(nonmarkt)+log(lpind)+log(lpfbs)+log(lpconstr)+log(lpwrtafic)+log(lpnonmrkt)+log(o3)+I(as.numeric(year)*log(Square_kilometer))+I(as.numeric(year)*log(Square_kilometer)^2)+as.factor(nuts)+I(as.numeric(year))

slmtest(fm10,data = data,listw=W.list.inv, test="rlml")#highly significant both should be used/ do not test again
slmtest(fm10,data = data ,listw=W.list.inv, test="rlme")#highly significant both should be used/ do not test again

#spatial hausmann test, even if i take fixed effects for sure
spherrlag <- sphtest(x=fmall, data=data, listw=W.list.inv, spatial.model = "sarar", method = "ML",errors = "KKP")
spherrlag# one model is inconsitent hence fixed effects are also supported by the spatial hausmann test





#-------------------------------------------------HAC Errors-----------------------------------------------------

library(sphet)


id <- seq(1, (nrow(data)/2))
# d <- distance(coord=coords, region.id = id, output = TRUE, type = "distance",
#               shape.name = "shapefile", region.id.name="id", firstline = TRUE,
#              file.name = "nuts_in.GWT",cutoff = 1)#region id eigentlich unnötig weil das das package sowieso macht
coldist <- read.gwt2dist(file = "nuts_in.GWT",  region.id = id,skip = 1)

fmfd<-pm10~fdi+fdi2+gdp+gdp2+popdens+ind+agri+wrtafic+lpind+lpwrtafic+o3+sqkil+sqkil2

SAR.2STLS.HAC <- stslshac(fmfd, data=trans, listw=W.list.inv, distance = coldist, HAC = TRUE, type ="Bisquare",zero.policy=FALSE)
SAR.2STLS <- stslshac(fmfd, data=trans, listw=W.list.inv, distance = coldist, HAC = FALSE, type ="Bisquare",zero.policy=FALSE)
summary(SAR.2STLS.HAC)#sieht nun doch weniger signifikant aus, allerdings ist es besser mit ohne HAC estimation zu vergleichen# mit ersten drei kernels getestet, ganz leichte schwankungen nur bei den signifikants levels
summary(SAR.2STLS)# in comparision to HAC, just skil parameter changes a lot
summary(gstslshet(fmfd, data=trans, listw=W.list.inv, initial.value = 0.2))

# we should not use those results if the results (different signifcance levels) do not change. see explanation sphet package page 10. Therefore we should maybe just include it in the robustness section. 

plot(density(residuals(SAR.2STLS.HAC)))
acf(residuals(SAR.2STLS.HAC))




east<-c("CZ","BG","EE","HR","HU","RO","SI","SK","PL","LT","LV")

spdf_east<-spdf[spdf$CNTR_CODE %in% east, ]

coords <-coordinates(spdf_east)
distw.tot <- dnearneigh(coords,0,Inf, row.names = spdf_east$id)
dnbdist.tot <- nbdists(distw.tot, coords)
gl.tot <- lapply(dnbdist.tot, function(x) 1/x^2)

W.list.inv_east <- nb2listw(distw.tot, glist=gl.tot, zero.policy=FALSE, style = "W")
W.di_east<-listw2mat(W.list.inv_east)

id <- seq(1, (nrow(W.di_east)))
d <- distance(coord=coords, region.id = id, output = TRUE, type = "distance",
              shape.name = "shapefile", region.id.name="id", firstline = TRUE,
              file.name = "nuts_in_east.GWT",cutoff = 1)
coldist <- read.gwt2dist(file = "nuts_in_east.GWT",  region.id = id,skip = 1)

trans_east<-trans %>% filter(country %in% east)
fmfd<-pm10~fdi+fdi2+gdp+gdp2+popdens+ind+fbs+constr+lpind+lpconstr+o3
SAR.2STLS.HAC_E <- stslshac(fmfd, data=trans_east[order(match(trans_east$nuts,rownames(W.di_east))),], listw=W.list.inv_east, distance = coldist, HAC = TRUE, type ="Triangular",zero.policy=FALSE)
summary(SAR.2STLS.HAC_E)#estimate for spatial parameter larger than one? results do differ really much from those estimated by the splm model


spdf_west<-spdf[!spdf$CNTR_CODE %in% east, ]

coords <-coordinates(spdf_west)
distw.tot <- dnearneigh(coords,0,Inf, row.names = spdf_west$id)
dnbdist.tot <- nbdists(distw.tot, coords)
gl.tot <- lapply(dnbdist.tot, function(x) 1/x^2)

W.list.inv_west <- nb2listw(distw.tot, glist=gl.tot, zero.policy=FALSE, style = "W")
W.di_west<-listw2mat(W.list.inv_west)

id <- seq(1, (nrow(W.di_west)))
d <- distance(coord=coords, region.id = id, output = TRUE, type = "distance",
              shape.name = "shapefile", region.id.name="id", firstline = TRUE,
              file.name = "nuts_in_west.GWT",cutoff = 1)
coldist <- read.gwt2dist(file = "nuts_in_west.GWT",  region.id = id,skip = 1)


fmfd<-pm10~fdi+fdi2+gdp+gdp2+popdens+ind+fbs+constr+lpconstr+lpwrtafic+o3+sqkil+sqkil2
SAR.2STLS.HAC_W <- stslshac(fmfd, data=trans %>% filter(!country %in% east), listw=W.list.inv_west, distance = coldist, HAC = TRUE, type ="Triangular",zero.policy=FALSE)
summary(SAR.2STLS.HAC_W)




#----------------------------------------------------------queen---------------------------------
zero<-c("DK014","EL421","EL422","EL411","EL412","EL413","EL621","EL622","EL623","EL624","ES531","ES532","ES533","SE214","UKJ34","UKM64","UKM65","UKM66","MT001","MT002")


data<-data[!data$nuts %in% zero,]

#plot(spdf)
spdf <- spdf[spdf$id %in% data$nuts, ]
# setdiff(unique(data$nuts),spdf$id)#to see if we have the same entrys for both data
data<-data[data$nuts %in% spdf$id,]
#W.matrix

queen_nb <- poly2nb(spdf, row.names = spdf$id, queen = T)  #creates a neighborhoodlist
W.list.queen <- nb2listw(queen_nb, style = "W", zero.policy = TRUE) #creates a weights-list
W.queen <- listw2mat(W.list.queen) #creates a weigths matrix


# we can also plot weights matrices
plot(spdf)


data<-data[order(data$year,match(data$nuts,rownames(W.queen))),]

sarsem14<-spml(fmall, data, listw=W.list.queen, model="within",effect = "twoways", lag=TRUE, spatial.error="b", method="eigen", quiet=TRUE, zero.policy = TRUE, tol.solve = 1e-10)

summary(sarsem14)

plot(residuals(sarsem14))
acf(residuals(sarsem14))




#---------------------------------------------spatialreg with difference equation----------------------------------
library(spatialreg)




fmfd<-pm10~fdi+fdi2+gdp+gdp2+popdens+ind+agri+wrtafic+lpind+lpwrtafic+o3+sqkil+sqkil2

fd1<-lm(fmfd,trans)


#fmfd<-pm10~fdi+gdp+gdp2+ind+popdens+fbs+lptotal+lpind+sqkil+sqkil2+fdilpind+o3+I(lpind^2)+I(fdi*gdp)+I(fdi*ind)++I((fdi*ind)^2)+I(fdi^2)
SEM <- errorsarlm(fmfd, data=trans, W.list.inv, tol.solve=1.0e-30)
summary(SEM)


SAR <- lagsarlm(formula = fmfd, listw = W.list.inv, data=trans, tol.solve=1.0e-30)
summary(SAR, correlation=FALSE)


Durbin <- lagsarlm(formula = fmfd, listw = W.list.inv, data=trans, tol.solve=1.0e-30, type="mixed")#just to lagged variables are significant (gdp, o3)+and AIC improvement is not as large as it is for SAC case
summary(Durbin, correlation=FALSE)

acf(residuals(Durbin))

#sac <- sacsarlm(formula = fmfd, listw = W.list.inv, type="sac",listw2 = W.list.1 ,data=trans, tol.solve=1.0e-30)#better for autocorrelation but fdi looses its influence
sac <- sacsarlm(formula = fmfd, listw = W.list.inv, type="sac",data=trans, tol.solve=1.0e-30)#best for fmall if we look at the log likelihood
summary(sac, correlation=FALSE)


sac.impacts<-impacts(sac, listw = W.list.inv)
sac.impacts


W<-as(as_dgRMatrix_listw(W.list.inv), "CsparseMatrix") #drop 0
trMat<-trW(W, type="mult")
trMC<-trW(W, type="MC")
set.seed(12345)
sac.impacts2<-impacts(sac, tr=trMat, R=1000)
z<-summary(sac.impacts2, zstats=TRUE, short=TRUE)
#install.packages("coda")
library(coda)
HPDinterval(sar.impacts2, choice="direct")
HPDinterval(sar.impacts2, choice ="indirect")
HPDinterval(sar.impacts2, choice="total")

z$res
y<-list(z$pzmat[,1],z$pzmat[,2],z$pzmat[,3])
a<-list(z$semat[,1],z$semat[,2],z$semat[,3])
test<-lapply(seq_along(z$res),function(i) paste0(round(unlist(z$res[i]),3),"$^{",stars.pval(unlist(y[i])),"}$"," (",round(unlist(a[i]),3),")"))#https://stackoverflow.com/questions/15273941/sum-of-two-lists-with-lists-in-r

df <- data.frame(matrix(unlist(test), nrow=13, byrow=F),stringsAsFactors=FALSE)
rownames(df)<-c("$FDI$", "$FDI^2$","$GDP$", "$GDP^2$", "$POP$", "$IND$", "$AGRI$", "$WRTAFIC$", "$LPIND$","$LPWRTAFIC$","$O_3$","$AREA$","$AREA^2$")
colnames(df)<-c("Direct","Indirect","Total")

library(xtable)
df<-xtable(df, caption = "Impacts of the FD-SARAR Model",label = "Impacts")
align(df) <- c("l", "c","c","c")
print(df,sanitize.text.function=function(x){x},caption.placement = "top")

summary(sac.impacts2, zstats=TRUE, short=TRUE)

fmfd<-pm10~fdi+fdi2+gdp+gdp2+popdens+wrtafic+lptotal+lpwrtafic+o3+sqkil+sqkil2#best model for sac if we start with the fmall equation
fmfd<-pm10~fdi2+gdp+gdp2+popdens2+agri+o3#best model for sac if start with fmall equation without size term

plot(density(trans$sqkil))

#-------------------------------------------different country groups with spatialreg---------------------------------
fmfd<-pm10~fdi+fdi2+gdp+gdp2+popdens+ind+fbs+agri+constr+wrtafic+nonmarkt+lpind+lpfbs+lpconstr+lpwrtafic+lpnonmrkt+o3+sqkil+sqkil2



fmfd<-pm10~fdi+fdi2+gdp+gdp2+popdens+ind+fbs+constr+lpind+lpconstr+o3
east<-c("CZ","BG","EE","HR","HU","RO","SI","SK","PL","LT","LV")

spdf_east<-spdf[spdf$CNTR_CODE %in% east, ]

coords <-coordinates(spdf_east)
distw.tot <- dnearneigh(coords,0,Inf, row.names = spdf_east$id)
dnbdist.tot <- nbdists(distw.tot, coords)
gl.tot <- lapply(dnbdist.tot, function(x) 1/x^2) #calculating inverse distances and take x^2 because we want to put more weight on closer neighbours
W.list.inv_east <- nb2listw(distw.tot, glist=gl.tot, zero.policy=FALSE, style = "W")
W.di_east<-listw2mat(W.list.inv_east)

sac1 <- sacsarlm(formula = fmfd, listw = W.list.inv_east, type="sac",data=trans %>% filter(country %in% east), tol.solve=1.0e-30)
summary(sac1)
acf(residuals(sac1))



fmfd<-pm10~fdi+fdi2+gdp+gdp2+popdens+ind+fbs+constr+lpconstr+lpwrtafic+o3+sqkil+sqkil2
spdf_west<-spdf[!spdf$CNTR_CODE %in% east, ]

coords <-coordinates(spdf_west)
distw.tot <- dnearneigh(coords,0,Inf, row.names = spdf_west$id)
dnbdist.tot <- nbdists(distw.tot, coords)
gl.tot <- lapply(dnbdist.tot, function(x) 1/x^2) #calculating inverse distances and take x^2 because we want to put more weight on closer neighbours
W.list.inv_west <- nb2listw(distw.tot, glist=gl.tot, zero.policy=FALSE, style = "W")
W.di_west<-listw2mat(W.list.inv_west)

sac2 <- sacsarlm(formula = fmfd, listw = W.list.inv_west, type="sac",data=trans %>% filter(!country %in% east), tol.solve=1.0e-30)
summary(sac2)
acf(residuals(sac2))



fmfd<-pm10~fdi+fdi2+gdp+gdp2+popdens+ind+agri+wrtafic+nonmarkt+lpconstr+lpwrtafic+o3+sqkil
small<-c("DE","UK","NL","BE")#looks now much better, DE and UK are most important

spdf_small <- spdf[spdf$CNTR_CODE %in% small, ]
#W.matrix

coords <-coordinates(spdf_small)
distw.tot <- dnearneigh(coords,0,Inf, row.names = spdf_small$id)
dnbdist.tot <- nbdists(distw.tot, coords)
gl.tot <- lapply(dnbdist.tot, function(x) 1.345756/x^2) #calculating inverse distances and take x^2 because we want to put more weight on closer neighbours
W.list.inv_small <- nb2listw(distw.tot, glist=gl.tot, zero.policy=FALSE, style = "W")
W.dis_small<-listw2mat(W.list.inv_small)

sac3 <- sacsarlm(formula = fmfd, listw = W.list.inv_small, type="sac",data=trans%>% filter(country %in% small), tol.solve=1.0e-30)
summary(sac3)
acf(residuals(sac3))




spdf_nsmall <- spdf[!spdf$CNTR_CODE %in% c(small,east), ]
#W.matrix

coords <-coordinates(spdf_nsmall)
distw.tot <- dnearneigh(coords,0,Inf, row.names = spdf_nsmall$id)
dnbdist.tot <- nbdists(distw.tot, coords)
gl.tot <- lapply(dnbdist.tot, function(x) 1.345756/x^2) #calculating inverse distances and take x^2 because we want to put more weight on closer neighbours
W.list.inv_nsmall <- nb2listw(distw.tot, glist=gl.tot, zero.policy=FALSE, style = "W")
W.dis_nsmall<-listw2mat(W.list.inv_nsmall)

sac4 <- sacsarlm(formula = fmfd, listw = W.list.inv_nsmall, type="sac",data=trans%>% filter(!country %in% c(small,east)), tol.solve=1.0e-30)
summary(sac4)
acf(residuals(sac4))



spdf_ger <- spdf[spdf$CNTR_CODE %in% "DE", ]
#W.matrix

coords <-coordinates(spdf_ger)
distw.tot <- dnearneigh(coords,0,Inf, row.names = spdf_ger$id)
dnbdist.tot <- nbdists(distw.tot, coords)
gl.tot <- lapply(dnbdist.tot, function(x) 1.345756/x^2) #calculating inverse distances and take x^2 because we want to put more weight on closer neighbours
W.list.inv_ger <- nb2listw(distw.tot, glist=gl.tot, zero.policy=FALSE, style = "W")
W.dis_ger<-listw2mat(W.list.inv_ger)

sac5 <- sacsarlm(formula = fmfd, listw = W.list.inv_ger, type="sac",data=trans%>% filter(country %in% "DE"), tol.solve=1.0e-30)
summary(sac5)
acf(residuals(sac5))




fmfd<-pm10~fdi+fdi2+gdp+gdp2+popdens+ind+fbs+nonmarkt+lpfbs+o3+sqkil+sqkil2
GWR<-c("PT","SI","AT","HR","CZ","PL","ES")#looks now much better, DE and UK are most important

spdf_gwr <- spdf[spdf$CNTR_CODE %in% GWR, ]
#W.matrix

coords <-coordinates(spdf_gwr)
distw.tot <- dnearneigh(coords,0,Inf, row.names = spdf_gwr$id)
dnbdist.tot <- nbdists(distw.tot, coords)
gl.tot <- lapply(dnbdist.tot, function(x) 1.345756/x^2) #calculating inverse distances and take x^2 because we want to put more weight on closer neighbours
W.list.inv_gwr <- nb2listw(distw.tot, glist=gl.tot, zero.policy=FALSE, style = "W")
W.dis_gwr<-listw2mat(W.list.inv_gwr)

sac6 <- sacsarlm(formula = fmfd, listw = W.list.inv_gwr, type="sac",data=trans%>% filter(country %in% GWR), tol.solve=1.0e-30)
summary(sac6)
acf(residuals(sac6))

#----------------------------------------------residual diagnostics------------------------------------------
bptest.sarlm(sac)#test for homoskedastisity, the null homoskedastisity can bee rejected, hence there seems to be heteroskedastisity

#but no problem since we can correct for heteroskedastisity according to https://books.google.at/books?id=64vt5TDBNLwC&pg=PA487&lpg=PA487&dq=serial+correlation+if+t%3D2&source=bl&ots=JA02mNjrsa&sig=ACfU3U2XzbG1JWpUkARAONfjn8F9xsmKUQ&hl=en&sa=X&ved=2ahUKEwid7dai2fbmAhXHDGMBHUFMBpcQ6AEwEHoECAwQAQ#v=onepage&q=serial%20correlation%20if%20t%3D2&f=false by for exampele: https://www.brodrigues.co/blog/2018-07-08-rob_stderr/


x<-acf(residuals(sac))
x$acf# yes there is autocorrelation in the residuals, controll for it with hac errors or different W matrix


plot(sac$y-sac$residuals,sac$residuals)#fitted values are the difference between real values and the error according to http://karthur.org/2016/fixed-effects-panel-models-in-r.html
plot(Durbin$y-Durbin$residuals,Durbin$residuals)

qqnorm(residuals(sac), ylab = 'Residuals')
qqline(residuals(sac), probs = c(0.05, 0.95))
hist(residuals(sac), xlab = 'Residuals')

plot(density(residuals(sac)))#the look nicely normal distributed with mean zero

require(faraway)
# Create `labs` (labels) for 1 through 1704 observations
X <- as.matrix(sac$X)
P <- X %*% solve(t(X) %*% X) %*% t(X)
halfnorm(diag(P), labs = 1:nrow(trans), ylab = 'Leverages', nlab = 8)#wieder 4
halfnorm(diag(P))
