library(eurostat)
library(readxl)
library(geosphere)
library(rgdal)
library(dplyr)
library(tidyr)
library(readODS)
library(plm)
library(spdep)
library(spgwr)
library(spatialreg)
library(sphet)
library(tibble)
library(xtable)
library(stargazer)
library(lmtest)
#library(car)#erst unten bei vif weil sonst überschneidung mit recode von dplyr


setwd("C:/WU/Master/AAMasterarbeit/mögliche Themen/FDI")

# ----------------------------------------------------Functions

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


#cite gtools weil function von ihnen?
stars.pval <- function(p.value)
{
  unclass(
    symnum(p.value, corr = FALSE, na = FALSE,
           cutpoints = c(0,0.01, 0.05, 0.1, 1),
           symbols = c("***", "**", "*", " "))
  )
}

sarlm2star<-function(sarlm,plm)
{
dummy<-plm
dummy$coefficients<-c(sarlm$rho,sarlm$lambda,sarlm$coefficients)
dummy$vcov<-sarlm$resvar[-1,-1]
dummy$residuals<-sarlm$residuals
dummy$df.residual<-nrow(sarlm$X)-sarlm$parameters
return(dummy)
}

sIVhac2star<-function(sIVhac,plm)
{
dummy1<-plm
dummy1$coefficients<-c(sIVhac$coefficients)
dummy1$vcov<-sIVhac$var
dummy1$residuals<-sIVhac$residuals
dummy1$df.residual<-nrow(sIVhac$model)-nrow(sIVhac$var)-1
return(dummy1)
}

#------------------------------------------------------------Data---------------------------------------------------

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


fdidat$man<-fdidat$man/2*1000#to correct for 1000 €
fdidat$ser<-fdidat$ser/2*1000#to correct for 1000 €
fdidat$oth<-fdidat$oth/2*1000#to correct for 1000 €
fdidat$value<-fdidat$value*1000#to correct for 1000 €
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

cam<-cam %>% filter(REF_YEAR>=start & REF_YEAR<=end) %>% mutate(lptotal=rgdp/lptotal,lpagri=agri/lpagri,lpconstr=constr/lpconstr,lpfbs=fbs/lpfbs,lpind=ind/lpind,lpnonmrkt=nonmarkt/lpnonmrkt,lpwrtafic=wrtafic/lpwrtafic,agri=agri/total,constr=constr/total,fbs=fbs/total,ind=ind/total,nonmarkt=nonmarkt/total,wrtafic=wrtafic/total)

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
#library(geosphere)
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


#--------------------------------pollution

load("./Data/o32.RData")
load("./Data/pm25.RData")
load("./Data/pm10.RData")

sum(pm102[[10]]>10)
sum(brk[[8]]>10)
826/1289

  
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


spdf <- spdf[spdf$id %in% data$nuts, ]
# setdiff(unique(data$nuts),spdf$id)#to see if we have the same entrys for both data
data<-data[data$nuts %in% spdf$id,]




coords <-coordinates(spdf)
distw.tot <- dnearneigh(coords,0,Inf, row.names = spdf$id)
dnbdist.tot <- nbdists(distw.tot, coords)
gl.tot <- lapply(dnbdist.tot, function(x) 1/x^2) #calculating inverse distances and take x^2 because we want to put more weight on closer neighbours
W.list.inv <- nb2listw(distw.tot, glist=gl.tot, zero.policy=FALSE, style = "W")
W.dis<-listw2mat(W.list.inv)

#order data as the shapefile
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


#----------------------------------------first difference Transformation----------------------------------------------


fmall<-log(pm10)~I(value/rgdp)+I((value/rgdp)^2)+log(rgdp/pop)+I(log(rgdp/pop)^2)+log(pop)+I(log(pop)^2)+log(ind)+log(fbs)+log(agri)+log(constr)+log(wrtafic)+log(nonmarkt)+log(lptotal)+log(lpind)+log(lpfbs)+log(lpconstr)+log(lpwrtafic)+log(lpnonmrkt)+log(o3)+I(as.numeric(year)*log(Square_kilometer))+I(as.numeric(year)*log(Square_kilometer)^2)

fd<-plm(fmall, data, model = "fd")


d<-c(1,-1)
d<-as.vector(d)
dim(d) <- c(1,2)
ident<-diag(length(unique(data$nuts)))
D<-kronecker(ident,d)

x<-as.matrix(cbind(1,fd$model))#der 1er ist unnötig

trans<-(-1)*D%*%x
trans<-as.data.frame(trans)


colnames(trans)<-c("b0","pm10","fdi","fdi2","gdp","gdp2","popdens","popdens2","ind","fbs","agri","constr","wrtafic","nonmarkt","lptotal","lpind","lpfbs","lpconstr","lpwrtafic","lpnonmrkt","o3","sqkil","sqkil2")

fmfd<-pm10~fdi+fdi2+gdp+gdp2+popdens+popdens2+ind+fbs+agri+constr+wrtafic+nonmarkt+lptotal+lpind+lpfbs+lpconstr+lpwrtafic+lpnonmrkt+o3+sqkil+sqkil2

fd1<-lm(fmfd, data=trans)

summary(fd)
summary(fd1)

trans$nuts<-data$nuts[1:(nrow(data)/2)]
trans$country<-data$country[1:(nrow(data)/2)]

#----------------------------------------------Data descriptives-------------------------------------------------

#---------vif
#multicolineraity test as in The emissions reduction effect and technical progress effect ofenvironmental regulation policy tools S.6
fmfd2<-pm10~fdi+fdi2+gdp+gdp2+popdens+ind+fbs+agri+constr+wrtafic+lpind+lpfbs+lpconstr+lpwrtafic+o3+sqkil+sqkil2

fd3<-lm(fmfd2, data=trans)

library(car)
#car::vif(Ols)#Gdp extrem hoch, laut oberen Paper sollte unter 10 sein# #how vif is calculated: 1/(1-R2) see https://statisticalhorizons.com/multicollinearity # auf website steht wenn x und x^2 inkludiert dann wahrscheinlich und ok wenn hoch# ist ok weil keine negativen konsequenzen laut dem Beitrag

#Bei kontrollvariablen ist Mulitcolineratiy less a of a problem
# in unserem fall ist daher vor allem Fdi von hoher relevanz bei der Interpretation, bei den anderen erwähnen dass die VIF teilweise höher ist (über 2.5) bei gdp und wrtafic zum beispiel und daher die ergbenisse mit vorsicht zu genießen sind

#nur relevant für gediffte varialen
VIF<-car::vif(fd3)

#----------gmi
#pm
mi_pm09<-moran.test(log(data$pm10[1:(nrow(data)/2)]), listw = W.list.inv, alternative = "greater", randomisation = FALSE)
mi_pm15<-moran.test(log(data$pm10[(nrow(data)/2+1):(nrow(data))]), listw = W.list.inv, alternative = "greater", randomisation = FALSE)
mi_pmdiff<-moran.test(trans$pm10, listw = W.list.inv, alternative = "greater", randomisation = FALSE)

#fdi/gdp
mi_fi09<-moran.test((data$value[1:(nrow(data)/2)]/data$rgdp[1:(nrow(data)/2)]), listw = W.list.inv, alternative = "greater", randomisation = FALSE)
mi_fi15<-moran.test((data$value[(nrow(data)/2+1):(nrow(data))]/data$rgdp[(nrow(data)/2+1):(nrow(data))]), listw = W.list.inv, alternative = "greater", randomisation = FALSE)
mi_fidiff<-moran.test(trans$fdi, listw = W.list.inv, alternative = "greater", randomisation = FALSE)


#Gdp/pop
mi_gd09<-moran.test(log(data$rgdp[1:(nrow(data)/2)]/data$pop[1:(nrow(data)/2)]), listw = W.list.inv, alternative = "greater", randomisation = FALSE)
mi_gd15<-moran.test(log(data$rgdp[(nrow(data)/2+1):(nrow(data))]/data$pop[(nrow(data)/2+1):(nrow(data))]), listw = W.list.inv, alternative = "greater", randomisation = FALSE)
mi_gddiff<-moran.test(trans$gdp, listw = W.list.inv, alternative = "greater", randomisation = FALSE)

#o3
mi_o309<-moran.test(log(data$o3[1:(nrow(data)/2)]), listw = W.list.inv, alternative = "greater", randomisation = FALSE)
mi_o315<-moran.test(log(data$o3[(nrow(data)/2+1):(nrow(data))]), listw = W.list.inv, alternative = "greater", randomisation = FALSE)
mi_o3diff<-moran.test(trans$o3, listw = W.list.inv, alternative = "greater", randomisation = FALSE)

#residuals
mi_res09<-moran.test(Ols$residuals[1:(nrow(data)/2)], listw = W.list.inv, alternative = "greater", randomisation = FALSE)
mi_res15<-moran.test(Ols$residuals[(nrow(data)/2+1):(nrow(data))], listw = W.list.inv, alternative = "greater", randomisation = FALSE)
mi_resdiff<-moran.test(fd1$residuals, listw = W.list.inv, alternative = "greater", randomisation = FALSE)


#-------lmi

#pm10
# von https://stackoverflow.com/questions/37664728/create-a-map-of-spatial-clusters-lisa-in-r
lmoran <- localmoran(trans$pm10, W.list.inv)

# padronize the variable and save it to a new column
trans$s_pm10 <- scale(trans$pm10)  %>% as.vector()

# create a spatially lagged variable and save it to a new column
trans$lag_s_pm10 <- lag.listw(W.list.inv, trans$s_pm10)

# create a new variable identifying the moran plot quadrant for each observation, dismissing the non-significant ones
trans$quad_sig <- NA
sig<-.05
lmoran[,5]<-p.adjust(lmoran[,5], method = "bonferroni")

# high-high quadrant
trans[(trans$s_pm10 >= 0 & 
         trans$lag_s_pm10 >= 0) & 
        (lmoran[, 5] <= sig), "quad_sig"] <- "high-high"
# low-low quadrant
trans[(trans$s_pm10 <= 0 & 
         trans$lag_s_pm10 <= 0) & 
        (lmoran[, 5] <= sig), "quad_sig"] <- "low-low"
# high-low quadrant
trans[(trans$s_pm10 >= 0 & 
         trans$lag_s_pm10 <= 0) & 
        (lmoran[, 5] <= sig), "quad_sig"] <- "high-low"
# low-high quadrant
trans[(trans$s_pm10 <= 0 
       & trans$lag_s_pm10 >= 0) & 
        (lmoran[, 5] <= sig), "quad_sig"] <- "low-high"
# non-significant observations
trans[(lmoran[, 5] > sig), "quad_sig"] <- "not signif."  

trans$quad_sig <- as.factor(trans$quad_sig)


#gdp
lmoran <- localmoran(trans$gdp, W.list.inv)

trans$s_pm10 <- scale(trans$gdp)  %>% as.vector()
trans$lag_s_pm10 <- lag.listw(W.list.inv, trans$s_pm10)

trans$quad_sig_gdp <- NA
sig<-.05
lmoran[,5]<-p.adjust(lmoran[,5], method = "bonferroni")

# high-high quadrant
trans[(trans$s_pm10 >= 0 & 
         trans$lag_s_pm10 >= 0) & 
        (lmoran[, 5] <= sig), "quad_sig_gdp"] <- "high-high"
# low-low quadrant
trans[(trans$s_pm10 <= 0 & 
         trans$lag_s_pm10 <= 0) & 
        (lmoran[, 5] <= sig), "quad_sig_gdp"] <- "low-low"
# high-low quadrant
trans[(trans$s_pm10 >= 0 & 
         trans$lag_s_pm10 <= 0) & 
        (lmoran[, 5] <= sig), "quad_sig_gdp"] <- "high-low"
# low-high quadrant
trans[(trans$s_pm10 <= 0 
       & trans$lag_s_pm10 >= 0) & 
        (lmoran[, 5] <= sig), "quad_sig_gdp"] <- "low-high"
# non-significant observations
trans[(lmoran[, 5] > sig), "quad_sig_gdp"] <- "not signif."  

trans$quad_sig_gdp <- as.factor(trans$quad_sig_gdp)


#---------gwr

fmfd<-pm10~fdi+fdi2+gdp+gdp2+popdens+ind+fbs+agri+constr+wrtafic+lpind+lpfbs+lpconstr+lpwrtafic+o3+sqkil+sqkil2
summary(lm(fmfd,trans))

coords <-coordinates(spdf)
bwG <- gwr.sel(fmfd, data = trans, gweight = gwr.Gauss,verbose = TRUE,coords = coords)
gwr1<-gwr(fmfd,data = trans, coords = coords, bandwidth = bwG, hatmatrix=TRUE, se.fit=TRUE)

x<-as.data.frame(gwr1$SDF)
trans$beta_fdi<-x[,"fdi"]
trans$beta_fdi2<-x[,"fdi2"]



fmfd<-pm10~fdi+gdp+gdp2+popdens+ind+fbs+agri+constr+wrtafic+lpind+lpfbs+lpconstr+lpwrtafic+o3+sqkil+sqkil2

bwG <- gwr.sel(fmfd, data = trans, gweight = gwr.Gauss,verbose = TRUE,coords = coords)
gwr1<-gwr(fmfd,data = trans, coords = coords, bandwidth = bwG, hatmatrix=TRUE, se.fit=TRUE)

x<-as.data.frame(gwr1$SDF)
trans$beta_fdi_only<-x[,"fdi"]


#--------------------------------------------für die plots---------------------------------------------------

test<-data[(nrow(data)/2+1):(nrow(data)),c("nuts","pm10","value","pm25","rgdp")]
test$fdi<-test$value
test$value<-test$value/test$rgdp
test<-left_join(test,trans[,c("nuts","quad_sig","quad_sig_gdp")], by="nuts")
test<-left_join(test,trans[,c("nuts","beta_fdi","beta_fdi2","beta_fdi_only")], by="nuts")

#-----------------------------------------------------------------------------------------------------------
#------------------------------------------------Estimation-------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

#---------------------------------------------spatialreg for full sample----------------------------------

fmall<-log(pm10)~I(value/rgdp)+log(rgdp/pop)+I(log(rgdp/pop)^2)+log(pop)+log(ind)+log(agri)+log(constr)+log(wrtafic)+log(lpind)+log(lpconstr)+log(lpwrtafic)+log(o3)+I(as.numeric(year)*log(Square_kilometer))+I(as.numeric(year)*log(Square_kilometer)^2)

Ols<-plm(fmall, data, model = "pooling")
fd<-plm(fmall, data, model = "fd")
summary(Ols)
summary(fd)

fmfd<-pm10 ~ fdi + gdp + gdp2 + popdens + ind + agri + 
  constr + wrtafic + lpind + lpconstr + 
  lpwrtafic + o3 + sqkil + sqkil2# without square term at this point#

fd1<-lm(fmfd,trans)

#step exclude the fbs variables since they seem not to influence the results in the most important models, further the interpretation of those is difficult
#important, agri, industry, and constr, wrtafic for transport. 
# after those two steps this is our final model for the full sample# schritte zur Entscheidung siehe auch im Text


SEM <- errorsarlm(fmfd, data=trans, W.list.inv, tol.solve=1.0e-30)
summary(SEM)


SAR <- lagsarlm(formula = fmfd, listw = W.list.inv, data=trans, tol.solve=1.0e-30)
summary(SAR, correlation=FALSE)


Durbin <- lagsarlm(formula = fmfd, listw = W.list.inv, data=trans, tol.solve=1.0e-30, type="mixed")#just to lagged variables are significant (gdp, o3)+and AIC improvement is not as large as it is for SAC case
summary(Durbin, correlation=FALSE)# construction sector important spatial lag? But not in the regions themself?# the construction sector could be of course pm promoting according to pm study


sac <- sacsarlm(formula = fmfd, listw = W.list.inv, type="sac",data=trans, tol.solve=1.0e-30)#best for fmall if we look at the log likelihood
summary(sac, correlation=FALSE)






#-------------------------------------------different country groups with spatialreg---------------------------------

# eastern Countries

fmfd<-pm10~fdi+gdp+popdens+ind+fbs+constr+lpind+lpfbs+lpconstr+o3+sqkil+sqkil2

east<-c("CZ","BG","EE","HR","HU","RO","SI","SK","PL","LT","LV")

spdf_east<-spdf[spdf$CNTR_CODE %in% east, ]

coords <-coordinates(spdf_east)
distw.tot <- dnearneigh(coords,0,Inf, row.names = spdf_east$id)
dnbdist.tot <- nbdists(distw.tot, coords)
gl.tot <- lapply(dnbdist.tot, function(x) 1/x^2)
W.list.inv_east <- nb2listw(distw.tot, glist=gl.tot, zero.policy=FALSE, style = "W")
W.di_east<-listw2mat(W.list.inv_east)

sac1 <- sacsarlm(formula = fmfd, listw = W.list.inv_east, type="sac",data=trans %>% filter(country %in% east), tol.solve=1.0e-30)# no squared term for fdi#significant fdi
summary(sac1)
acf(residuals(sac1))
bptest.sarlm(sac1)


# without western countries 

fmfd<-pm10~fdi+gdp+gdp2+popdens+fbs+constr+lpconstr+lpwrtafic+o3+sqkil+sqkil2
#exclude lpfbs further because no meaning and highly insignificant
#fdi never ever significant no matter if we include squared term to or squared term only
#exclude industry sector and labour productivity seem to be not important
#exclude wrtafic
#exclude agriculture sector

spdf_west<-spdf[!spdf$CNTR_CODE %in% east, ]

coords <-coordinates(spdf_west)
distw.tot <- dnearneigh(coords,0,Inf, row.names = spdf_west$id)
dnbdist.tot <- nbdists(distw.tot, coords)
gl.tot <- lapply(dnbdist.tot, function(x) 1/x^2)
W.list.inv_west <- nb2listw(distw.tot, glist=gl.tot, zero.policy=FALSE, style = "W")
W.di_west<-listw2mat(W.list.inv_west)

sac2 <- sacsarlm(formula = fmfd, listw = W.list.inv_west, type="sac",data=trans %>% filter(!country %in% east), tol.solve=1.0e-30)# no squared term for population and fdi#insignificant
summary(sac2)
acf(residuals(sac2))
bptest.sarlm(sac2)


# Western Europe

fmfd<-pm10~fdi+fdi2+gdp+gdp2+popdens+agri+wrtafic+lpwrtafic+sqkil
#exclude skil2 then nonmarkt then lpnonmrkt even if it is significant then fbs and lpfbs then lpind and and ind
#then constry and lpconstr# last o3 --> final model for this large group

small<-c("DE","NL","BE","AT","FR","UK","IE")#looks now much better, DE and UK are most important #Ergebnisse ändern sich kaum wenn ich AT und DK dazu gebe

spdf_small <- spdf[spdf$CNTR_CODE %in% small, ]


coords <-coordinates(spdf_small)
distw.tot <- dnearneigh(coords,0,Inf, row.names = spdf_small$id)
dnbdist.tot <- nbdists(distw.tot, coords)
gl.tot <- lapply(dnbdist.tot, function(x) 1.345756/x^2)
W.list.inv_small <- nb2listw(distw.tot, glist=gl.tot, zero.policy=FALSE, style = "W")
W.dis_small<-listw2mat(W.list.inv_small)

sac3 <- sacsarlm(formula = fmfd, listw = W.list.inv_small, type="sac",data=trans%>% filter(country %in% small), tol.solve=1.0e-30)#squared fdi effect important, squared pop not
summary(sac3)
acf(residuals(sac3))#yes we need again hac adjusted standard errors
bptest.sarlm(sac3)



#NL-DE-BE-AT

ger<-c("DE","NL","BE","AT")
spdf_ger <- spdf[spdf$CNTR_CODE %in% ger, ]
#W.matrix

coords <-coordinates(spdf_ger)
distw.tot <- dnearneigh(coords,0,Inf, row.names = spdf_ger$id)
dnbdist.tot <- nbdists(distw.tot, coords)
gl.tot <- lapply(dnbdist.tot, function(x) 1.345756/x^2)
W.list.inv_ger <- nb2listw(distw.tot, glist=gl.tot, zero.policy=FALSE, style = "W")
W.dis_ger<-listw2mat(W.list.inv_ger)

fmfd<-pm10~fdi+gdp+popdens+ind+fbs+agri+constr+wrtafic+lpind+lpfbs+lpconstr+lpwrtafic+o3
#exclude sqkil2 then sqkil #popdens #nonmarkt then lpnonmrkt #cons # term for GDP2 # fdi2

sac5 <- sacsarlm(formula = fmfd, listw = W.list.inv_ger, type="sac",data=trans%>% filter(country %in% ger), tol.solve=1.0e-30)# no squared fdi term, but squared pop term
summary(sac5)
acf(residuals(sac5))#we need again the hac adjusted SE
bptest.sarlm(sac5)



#UK-IE-FR

uk<-c("IE","UK","FR")#,
spdf_uk <- spdf[spdf$CNTR_CODE %in% uk, ]

coords <-coordinates(spdf_uk)
distw.tot <- dnearneigh(coords,0,Inf, row.names = spdf_uk$id)
dnbdist.tot <- nbdists(distw.tot, coords)
gl.tot <- lapply(dnbdist.tot, function(x) 1.345756/x^2) #calculating inverse distances and take x^2 because we want to put more weight on closer neighbours 
W.list.inv_uk <- nb2listw(distw.tot, glist=gl.tot, zero.policy=FALSE, style = "W")
W.dis_uk<-listw2mat(W.list.inv_uk)

fmfd<-pm10~fdi+fdi2+gdp+gdp2+popdens+ind+wrtafic+lpind+lpwrtafic+o3+sqkil+sqkil2
# with or without fdi2 vom AIC her wäre ohne fdi2 zu bevorzugen 

sac7 <- sacsarlm(formula = fmfd, listw = W.list.inv_uk, type="sac",data=trans%>% filter(country %in% uk), tol.solve=1.0e-30)
summary(sac7)
acf(residuals(sac7))
bptest.sarlm(sac7)


#Southern Europe

south<-c("IT","ES","PT", "EL")#,
spdf_south <- spdf[spdf$CNTR_CODE %in% south, ]
#W.matrix

coords <-coordinates(spdf_south)
distw.tot <- dnearneigh(coords,0,Inf, row.names = spdf_south$id)
dnbdist.tot <- nbdists(distw.tot, coords)
gl.tot <- lapply(dnbdist.tot, function(x) 1.345756/x^2) #calculating inverse distances and take x^2 because we want to put more weight on closer neighbours 
W.list.inv_south <- nb2listw(distw.tot, glist=gl.tot, zero.policy=FALSE, style = "W")
W.dis_south<-listw2mat(W.list.inv_south)

fmfd<-pm10~fdi+fdi2+gdp+gdp2+popdens+agri+constr+lpconstr+o3+sqkil+sqkil2
#ecclude wrtafic # pop dens2 # fbs # fdi2 # ind
#nearly everything insignificant do not show those results 

sac8 <- sacsarlm(formula = fmfd, listw = W.list.inv_south, type="sac",data=trans%>% filter(country %in% south), tol.solve=1.0e-30)
summary(sac8)
acf(residuals(sac8))
bptest.sarlm(sac8)




#----------------------------------------------------sphet for different samples----------------------------------

#full sample

id <- seq(1, (nrow(data)/2))
# d <- distance(coord=coords, region.id = id, output = TRUE, type = "distance",
#               shape.name = "shapefile", region.id.name="id", firstline = TRUE,
#              file.name = "nuts_in.GWT",cutoff = 1)#region id eigentlich unnötig weil das das package sowieso macht
coldist <- read.gwt2dist(file = "nuts_in.GWT",  region.id = id,skip = 1)

fmfd<-pm10 ~ fdi + gdp + gdp2 + popdens + ind + agri + 
  constr + wrtafic + lpind + lpconstr + 
  lpwrtafic + o3 + sqkil + sqkil2

SAR.2STLS.HAC <- stslshac(fmfd, data=trans, listw=W.list.inv, distance = coldist, HAC = TRUE, type ="Triangular",zero.policy=FALSE)
SAR.2STLS <- stslshac(fmfd, data=trans, listw=W.list.inv, distance = coldist, HAC = FALSE, type ="Bisquare",zero.policy=FALSE)
summary(SAR.2STLS.HAC)#sieht nun doch weniger signifikant aus, allerdings ist es besser mit ohne HAC estimation zu vergleichen# mit ersten drei kernels getestet, ganz leichte schwankungen nur bei den signifikants levels
summary(SAR.2STLS)# in comparision to HAC, just skil parameter changes a lot
# we should not use those results if the results (different signifcance levels) do not change. see explanation sphet package page 10.





#Eastern countries

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
fmfd<-pm10~fdi+gdp+popdens+ind+fbs+constr+lpind+lpfbs+lpconstr+o3+sqkil+sqkil2#if i exclude lpfbs the results for FDI turn significant but they do not change a much
SAR.2STLS.HAC_E <- stslshac(fmfd, data=trans_east[order(match(trans_east$nuts,rownames(W.di_east))),], listw=W.list.inv_east, distance = coldist, HAC = TRUE, type ="Triangular",zero.policy=FALSE)
summary(SAR.2STLS.HAC_E)#if exclude BG and romania, results in 164 observation and 16 parameters but results for FDI are now highly significant at a 0.1% level 0.02274 point estimate. 

W<-as(as_dgRMatrix_listw(W.list.inv_east), "CsparseMatrix")
trMat2<-trW(W, type="mult")
sar_impact<-impacts(SAR.2STLS.HAC_E, tr=trMat2, R=1000)
summary(sar_impact, zstats=TRUE, short=TRUE)#not at significant at all



#Rest of Europe(expect eastern europe)

spdf_west<-spdf[!spdf$CNTR_CODE %in% east, ]

coords <-coordinates(spdf_west)
distw.tot <- dnearneigh(coords,0,Inf, row.names = spdf_west$id)
dnbdist.tot <- nbdists(distw.tot, coords)
gl.tot <- lapply(dnbdist.tot, function(x) 1/x^2)
W.list.inv_west <- nb2listw(distw.tot, glist=gl.tot, zero.policy=FALSE, style = "W")
W.di_west<-listw2mat(W.list.inv_west)

id <- seq(1, (nrow(W.di_west)))
# d <- distance(coord=coords, region.id = id, output = TRUE, type = "distance",
#               shape.name = "shapefile", region.id.name="id", firstline = TRUE,
#               file.name = "nuts_in_west.GWT",cutoff = 1)#region id eigentlich unnötig weil das das package sowieso macht
coldist <- read.gwt2dist(file = "nuts_in_west.GWT",  region.id = id,skip = 1)


fmfd<-pm10~fdi+gdp+gdp2+popdens+fbs+constr+lpconstr+lpwrtafic+o3+sqkil+sqkil2
SAR.2STLS.HAC_W <- stslshac(fmfd, data=trans %>% filter(!country %in% east), listw=W.list.inv_west, distance = coldist, HAC = TRUE, type ="Triangular",zero.policy=FALSE)
summary(SAR.2STLS.HAC_W)



#Western Euorpe

small<-c("DE","NL","BE","AT","FR","UK","IE")

coords <-coordinates(spdf_small)

id <- seq(1, (nrow(W.dis_small)))
# d <- distance(coord=coords, region.id = id, output = TRUE, type = "distance",
#               shape.name = "shapefile", region.id.name="id", firstline = TRUE,
#               file.name = "nuts_in_small.GWT",cutoff = 2)
coldist <- read.gwt2dist(file = "nuts_in_small.GWT",  region.id = id,skip = 1)

trans_small<-trans %>% filter(country %in% small)

fmfd<-pm10~fdi+fdi2+gdp+gdp2+popdens+agri+wrtafic+lpwrtafic+sqkil# is robust against the inclusion of other variables also if i include the significant lpnonmrkt variable
SAR.2STLS.HAC_S <- stslshac(fmfd, data=trans_small, listw=W.list.inv_small, distance = coldist, HAC = TRUE, type ="Triangular",zero.policy=FALSE)
summary(SAR.2STLS.HAC_S)
summary(sac3)

W<-as(as_dgRMatrix_listw(W.list.inv_small), "CsparseMatrix")
trMat2<-trW(W, type="mult")
sar_impact<-impacts(SAR.2STLS.HAC_S, tr=trMat2, R=1000)
summary(sar_impact, zstats=TRUE, short=TRUE)
#highly significant but some of the indirect effect are too, but go into the same direction 



#NL-DE-BE-AT

coords <-coordinates(spdf_ger)

id <- seq(1, (nrow(W.dis_ger)))
# d <- distance(coord=coords, region.id = id, output = TRUE, type = "distance",
#               shape.name = "shapefile", region.id.name="id", firstline = TRUE,
#               file.name = "nuts_in_ger.GWT",cutoff = 1)
coldist <- read.gwt2dist(file = "nuts_in_ger.GWT",  region.id = id,skip = 1)


fmfd<-pm10~fdi+gdp+popdens+ind+fbs+agri+wrtafic+lpind+lpfbs+lpwrtafic+o3
#exclude sqkil2 then sqkil #popdens #nonmarkt then lpnonmrkt #cons # term for GDP2 # fdi2
# fdi don´t really change if variables change
SAR.2STLS.HAC_G <- stslshac(fmfd, data=trans %>% filter(country %in% ger), listw=W.list.inv_ger, distance = coldist, HAC = TRUE, type ="Triangular",zero.policy=FALSE)
summary(SAR.2STLS.HAC_G)
summary(sac5)

W<-as(as_dgRMatrix_listw(W.list.inv_ger), "CsparseMatrix")
trMat2<-trW(W, type="mult")
sar_impact<-impacts(SAR.2STLS.HAC_G, tr=trMat2, R=1000)
summary(sar_impact, zstats=TRUE, short=TRUE)
#as we would expect them to be; direct significant for fdi/ pop,ind,agri,lpind and coefficients are nearly the same as in the table (slightly higher) and all indirect effects are insignificant



#UK-IE-FR

coords <-coordinates(spdf_uk)

id <- seq(1, (nrow(W.dis_uk)))
# d <- distance(coord=coords, region.id = id, output = TRUE, type = "distance",
#               shape.name = "shapefile", region.id.name="id", firstline = TRUE,
#               file.name = "nuts_in_uk.GWT",cutoff = 2)
coldist <- read.gwt2dist(file = "nuts_in_uk.GWT",  region.id = id,skip = 1)


fmfd<-pm10~fdi+fdi2+gdp+gdp2+popdens+ind+wrtafic+lpind+lpwrtafic+o3+sqkil+sqkil2
# better with squared term 

SAR.2STLS.HAC_U <- stslshac(fmfd, data=trans %>% filter(country %in% uk), listw=W.list.inv_uk, distance = coldist, HAC = TRUE, type ="Triangular",zero.policy=FALSE)
summary(SAR.2STLS.HAC_U)
summary(sac7)

W<-as(as_dgRMatrix_listw(W.list.inv_uk), "CsparseMatrix")
trMat2<-trW(W, type="mult")
sar_impact<-impacts(SAR.2STLS.HAC_U, tr=trMat2, R=1000)
summary(sar_impact, zstats=TRUE, short=TRUE)


#-------manufacturing share

data$man1<-data$man[1:1289]/(data$man[1:1289]+data$ser[1:1289]+data$oth[1:1289])

data$man1[is.na(data$man1)]<-0
data$man1[data$man1==Inf]<-NA

mean(data$man1,na.rm = TRUE)
mean(data[data$country %in% c(east,ger),]$man1,na.rm = TRUE)
mean(data[data$country %in% east2,]$man1,na.rm = TRUE)
mean(data[data$country %in% "DK",]$man1,na.rm = TRUE)
mean(data[data$country %in% south,]$man1,na.rm = TRUE)
mean(data[data$country %in% ger,]$man1,na.rm = TRUE)


#---------------------------------------different w-matrices using spatialreg and sphet----------------------------------------
fmfd<-pm10 ~ fdi + gdp + gdp2 + popdens + ind + agri + 
  constr + wrtafic + lpind + lpconstr + 
  lpwrtafic + o3 + sqkil + sqkil2
#has been controlled if this time fdi2 better describes the data better but again the linear relationship was supported, against a quadratic one.

#--------inverse distance wit 1/x
coords <-coordinates(spdf)
distw.tot <- dnearneigh(coords,0,Inf, row.names = spdf$id)
dnbdist.tot <- nbdists(distw.tot, coords)
gl.tot <- lapply(dnbdist.tot, function(x) 1/x)
W.list.inv <- nb2listw(distw.tot, glist=gl.tot, zero.policy=FALSE, style = "W")
W.dis<-listw2mat(W.list.inv)

#--------k=7
k.near <- knearneigh(coords, k=7) #indexing neighbors based on k=5
k7 <- knn2nb(k.near) #creating neighborhood list based on the k(5) nearest neighbors
W.list.7 <- nb2listw(k7, style = "W", zero.policy = FALSE) 


#----- Distance band matrix -----#or just use the k=4 instead

#To ensure everyone has at least 1 neighbor we compute the maximal distance between all 
#possible neighbors by creating a 1st-nearest neighbor list.

k1 <- knearneigh(coords, k=1) #indexing everyones closest neighbor
k1 <- knn2nb(k1) #transforming it into a neighborhood list
link.max <- max(unlist(nbdists(k1, coords=coords))) #choose maximal distance
link.max #printing max distance between neighbors

#next we create a neighborhood list for a distance of at least the previously calculated.
distw <- dnearneigh(coords, 0, link.max, row.names=spdf$id) #indexing all 
#neighbors within a circle of given distance
W.list <- nb2listw(distw, style="W", zero.policy=FALSE) #creates a weights-list




coords <-coordinates(spdf)
id <- seq(1, (nrow(W.dis)))



# d <- distance(coord=coords, region.id = id, output = TRUE, type = "NN",
#               shape.name = "shapefile", region.id.name="id", firstline = TRUE,
#               file.name = "nuts_in_inv.GWT",cutoff = FALSE,nn=7)
#coldist <- read.gwt2dist(file = "nuts_in_inv.GWT",  region.id = id,skip = 1)
# d <- distance(coord=coords, region.id = id, output = TRUE, type = "distance",
#               shape.name = "shapefile", region.id.name="id", firstline = TRUE,
#               file.name = "nuts_in_all.GWT",cutoff = 1)
coldist <- read.gwt2dist(file = "nuts_in_all.GWT",  region.id = id,skip = 1)



SAR.2STLS.HAC_i <- stslshac(fmfd, data=trans, listw=W.list.inv, distance = coldist, HAC = TRUE, type ="Triangular",zero.policy=FALSE)
summary(SAR.2STLS.HAC_i)
W<-as(as_dgRMatrix_listw(W.list.inv), "CsparseMatrix")
trMat2<-trW(W, type="mult")
sar_impact<-impacts(SAR.2STLS.HAC_i, tr=trMat2, R=1000)
summary(sar_impact, zstats=TRUE, short=TRUE)
#everything insignificant, reasonable since the effects would really high as lambda is not bounded by zero

SAR.2STLS.HAC_7 <- stslshac(fmfd, data=trans, listw=W.list.7, distance = coldist, HAC = TRUE, type ="Triangular",zero.policy=FALSE)
summary(SAR.2STLS.HAC_7)
W<-as(as_dgRMatrix_listw(W.list.7), "CsparseMatrix")
trMat2<-trW(W, type="mult")
sar_impact<-impacts(SAR.2STLS.HAC_7, tr=trMat2, R=1000)
summary(sar_impact, zstats=TRUE, short=TRUE)
#indirect effects of fdi gdp o3 lpwrtafic und pop significant/ but look the same as the results 

SAR.2STLS.HAC_d <- stslshac(fmfd, data=trans, listw=W.list, distance = coldist, HAC = TRUE, type ="Triangular",zero.policy=FALSE)
summary(SAR.2STLS.HAC_d)
W<-as(as_dgRMatrix_listw(W.list), "CsparseMatrix")
trMat2<-trW(W, type="mult")
sar_impact<-impacts(SAR.2STLS.HAC_d, tr=trMat2, R=1000)
summary(sar_impact, zstats=TRUE, short=TRUE)
#indirect effects of fdi gdp o3 lpwrtafic und pop are significant as well / but look the same as the results 




#-----queen

zero<-c("DK014","EL421","EL422","EL411","EL412","EL413","EL621","EL622","EL623","EL624","ES531","ES532","ES533","SE214","UKJ34","UKM64","UKM65","UKM66","MT001","MT002")

trans_queen<-trans[!trans$nuts %in% zero,]

#plot(spdf)
spdf <- spdf[spdf$id %in% trans_queen$nuts, ]
# setdiff(unique(trans_queen$nuts),spdf$id)#to see if we have the same entrys for both trans_queen
trans_queen<-trans_queen[trans_queen$nuts %in% spdf$id,]
#W.matrix

queen_nb <- poly2nb(spdf, row.names = spdf$id, queen = T)  #creates a neighborhoodlist
W.list.queen <- nb2listw(queen_nb, style = "W", zero.policy = TRUE) #creates a weights-list
W.queen <- listw2mat(W.list.queen) #creates a weigths matrix


trans_queen<-trans_queen[order(match(trans_queen$nuts,rownames(W.queen))),]

library(spatialreg)
saci <- sacsarlm(formula = fmfd, listw = W.list.inv, type="sac",data=trans, tol.solve=1.0e-30)
sac7 <- sacsarlm(formula = fmfd, listw = W.list.7, type="sac",data=trans, tol.solve=1.0e-30)
sacd <- sacsarlm(formula = fmfd, listw = W.list, type="sac",data=trans, tol.solve=1.0e-30)
sacq <- sacsarlm(formula = fmfd, listw = W.list.queen, type="sac",data=trans_queen, tol.solve=1.0e-30)

summary(saci)
summary(sac7)
summary(sacd)
summary(sacq)

acf(residuals(saci))
acf(residuals(sac7))
acf(residuals(sacd))
acf(residuals(sacq))


bptest.sarlm(saci)
bptest.sarlm(sac7)
bptest.sarlm(sacd)
bptest.sarlm(sacq)



coords <-coordinates(spdf)
id <- seq(1, (nrow(W.queen)))
# d <- distance(coord=coords, region.id = id, output = TRUE, type = "distance",
#               shape.name = "shapefile", region.id.name="id", firstline = TRUE,
#               file.name = "nuts_in_queen.GWT",cutoff = 1)
coldist <- read.gwt2dist(file = "nuts_in_queen.GWT",  region.id = id,skip = 1)

SAR.2STLS.HAC_Q <- stslshac(fmfd, data=trans_queen, listw=W.list.queen, distance = coldist, HAC = TRUE, type ="Triangular",zero.policy=FALSE)
summary(SAR.2STLS.HAC_Q)#quite some change for the queen matrix
W<-as(as_dgRMatrix_listw(W.list.queen), "CsparseMatrix")
trMat2<-trW(W, type="mult")
sar_impact<-impacts(SAR.2STLS.HAC_Q, tr=trMat2, R=1000)
summary(sar_impact, zstats=TRUE, short=TRUE)
#indirect effects of fdi gdp und pop significant/ but look the same as the results 


load("./Data/spdf1032013.RData")

spdf<-shp
remove(shp)

spdf <- spdf[spdf$id %in% trans$nuts, ]
trans<-trans[order(match(trans$nuts,rownames(W.dis))),]




#-----------------------------------------------------------------------------------------------------------
#----------------------------------------------Tables ------------------------------------------------------
#--------------------------------------------Descriptive tables---------------------------------------------
#vif table

VIF<-as.data.frame(VIF)

meandat<-data[,c("pm10","value","rgdp","pop","ind","fbs","agri","constr","wrtafic","lpind","lpfbs","lpconstr","lpwrtafic","o3")]
meandat$value<-meandat$value/meandat$rgdp
meandat$rgdp<-meandat$rgdp/meandat$pop

des<-sapply(Filter(is.numeric, meandat), function(x) c(mean = mean(x),med= median(x),min = min(x),max=max(x),sd=sd(x)))
des<-t(des)
des<-as.data.frame(des)
des$var<-c("pm10","fdi","gdp", "popdens","ind","fbs","agri","constr","wrtafic","lpind","lpfbs","lpconstr","lpwrtafic","o3")


des<-full_join(rownames_to_column(VIF), des, by = c("rowname" = "var"))
des<-des[c(18,1:17),]

rownames(des)<-c("$PM_{10}$","$\\%FDI$","$\\%FDI^2$","$GDPpc$", "$GDPpc^2$", "$POP$", "$IND$", "$FBS$", "$AGRI$", "$CONS$", "$WRT$", "$LPIND$", "$LPFBS$","$LPCONS$","$LPWRT$","$O_3$","$AREA$","$AREA^2$")
des<-des[,-1]
colnames(des)<-c("VIF","Mean","Median","Min","Max","SD")



df<-xtable(des, caption = "VIF for FD model and data descriptives",label = "tab:descriptives")
align(df) <- c("l", "r","r","r", "r","r","r")
#mdat<-matrix(c(rep(2,18),rep(2,3),rep(0,3),rep(2,5),rep(0,3),5),nrow=18,ncol=6)
print(df,sanitize.text.function=function(x){x},caption.placement = "top", digits=2)


#-----table gmi

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
rownames(mi)<-c("$\\ln{(PM_{10})}$","$\\%FDI$","$\\ln{(GDPpc)}$","$\\ln{(O_3)}$","$RES$")
mi<-rbind(c("Moran’s I","Z-value"),mi)
library(xtable)
tabl<-xtable(mi, caption = "Moran´s I for some variables of Interest")
print(tabl)

addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- paste0(paste0('& \\multicolumn{2}{c}{', c("2003-2009","2010-2015","Difference"), '}', collapse=''), '\\\\')
align(tabl) <- rep("l",7)
print(tabl, add.to.row=addtorow, include.colnames=F,sanitize.text.function=function(x){x},caption.placement = "top")


#---------------------------------------------------regression results------------------------------------------------

#full sample table

names(Ols$coefficients)<-names(fd1$coefficients)

bptest<-c("BP-Test",round(bptest(Ols)$statistic,3),round(bptest(fd1)$statistic,3),round(bptest.sarlm(sac)$statistic,3))
pval<-c("\\textit{ p-value}",round(bptest(Ols)$p.value,5),round(bptest(fd1)$p.value,5),round(bptest.sarlm(sac)$p.value,5))
aic<-c("AIC",round(AICplm(Ols),3),round(AIC(fd1),3),round(-sac$LL*2+2*sac$parameters,3))

stargazer(Ols,fd1,sarlm2star(sac,fd),sIVhac2star(SAR.2STLS.HAC,fd), type="latex", title = "Results for full Sample", style = "default",column.labels   = c("Pooled OLS","FD","FD-SAC","FD-SAR-HAC"), dep.var.labels = "$PM_{10}$", no.space=TRUE, single.row=TRUE,align = FALSE,keep.stat=c("n"),add.lines = list(bptest,pval,aic),covariate.labels = c("$\\lambda$", "$\\rho$","$W*PM_{10}$","$\\% FDI)$", "$\\ln{(GDPpc)}$", "$\\ln{(GDPpc)}^2$", "$\\ln{(POP)}$", "$\\ln{(IND)}$", "$\\ln{(AGRI)}$", "$\\ln{(CONS)}$", "$\\ln{(WRT)}$", "$\\ln{(LPIND)}$","$\\ln{(LPCONS)}$","$\\ln{(LPWRT)}$","$\\ln{(O_3)}$","$\\ln{(AREA)}$","$\\ln{(AREA)}^2$","$Constant$"), label = "tab:full")#,add.lines = list(aic)#
#,covariate.labels = c("$\\lambda$", "$\\rho$", "$FDI$", "$FDI^2$","$GDP$", "$GDP^2$", "$POP$", "$IND$", "$AGRI$", "$WRT$", "$LPIND$","$LPWRT$","$O_3$","$AREA$","$AREA^2$", "$W*FDI$", "$W*FDI^2$","$W*GDP$", "$W*GDP^2$", "$W*POP$", "$W*IND$", "$W*AGRI$", "$W*WRT$", "$W*LPIND$","$W*LPWRT$","$W*O_3$","$W*AREA$","$W*AREA^2$","$Constant$")
bptest.sarlm(sac)$p.value
summary(sac)
df.residual(Ols)
df.residual(sac)


#--------impact table

W<-as(as_dgRMatrix_listw(W.list.inv), "CsparseMatrix")
trMat2<-trW(W, type="mult")
trMC2<-trW(W, type="MC")
sar_impact<-impacts(SAR.2STLS.HAC, tr=trMat2, R=1000)
summary(sar_impact, zstats=TRUE, short=TRUE)


z<-summary(sar_impact, zstats=TRUE, short=TRUE)

y<-list(z$pzmat[,1],z$pzmat[,2],z$pzmat[,3])
a<-list(z$semat[,1],z$semat[,2],z$semat[,3])
test<-lapply(seq_along(z$res),function(i) paste0(round(unlist(z$res[i]),3),"$^{",stars.pval(unlist(y[i])),"}$"," (",round(unlist(a[i]),3),")"))#https://stackoverflow.com/questions/15273941/sum-of-two-lists-with-lists-in-r

df <- data.frame(matrix(unlist(test), nrow=14, byrow=F),stringsAsFactors=FALSE)
rownames(df)<-c("$\\% FDI)$", "$\\ln{(GDPpc)}$", "$\\ln{(GDPpc)}^2$", "$\\ln{(POP)}$", "$\\ln{(IND)}$", "$\\ln{(AGRI)}$", "$\\ln{(CONS)}$", "$\\ln{(WRT)}$", "$\\ln{(LPIND)}$","$\\ln{(LPCONS)}$","$\\ln{(LPWRT)}$","$\\ln{(O_3)}$","$\\ln{(AREA)}$","$\\ln{(AREA)}^2$")
colnames(df)<-c("Direct","Indirect","Total")

library(xtable)
df<-xtable(df, caption = "Impacts of the FD-SAR-HAC Model",label = "tab:Impacts")
align(df) <- c("l", "c","c","c")
print(df,sanitize.text.function=function(x){x},caption.placement = "top")



#---------subsamples table


bptest<-c("BP-Test",round(bptest.sarlm(sac1)$statistic,3),round(bptest.sarlm(sac3)$statistic,3),round(bptest.sarlm(sac5)$statistic,3),round(bptest.sarlm(sac7)$statistic,3))
pval<-c("\\textit{ p-value}",round(bptest.sarlm(sac1)$p.value,5),round(bptest.sarlm(sac3)$p.value,5),round(bptest.sarlm(sac5)$p.value,5),round(bptest.sarlm(sac7)$p.value,5))
aic<-c("AIC",round(-sac1$LL*2+2*sac1$parameters,3),round(-sac3$LL*2+2*sac3$parameters,3),round(-sac5$LL*2+2*sac5$parameters,3),round(-sac7$LL*2+2*sac7$parameters,3))

stargazer(sIVhac2star(SAR.2STLS.HAC_E,fd),sIVhac2star(SAR.2STLS.HAC_S,fd),sIVhac2star(SAR.2STLS.HAC_G,fd),sIVhac2star(SAR.2STLS.HAC_U,fd), type="latex", title = "Results for Subsamples with HAC standard errors", style = "default",column.labels   = c("Eastern","Western","NE-DE-BE-AT","UK-IE-FR"), dep.var.labels = "$PM_{10}$", no.space=TRUE, single.row=TRUE,align = FALSE,keep.stat=c("n"),add.lines = list(bptest,pval,aic),covariate.labels = c("$W*PM_{10}$", "$\\%FDI$", "$\\%FDI^2$","$\\ln{(GDPpc)}$", "$\\ln{(GDPpc)}^2$", "$\\ln{(POP)}$", "$\\ln{(IND)}$", "$\\ln{(FBS)}$", "$\\ln{(CONS)}$", "$\\ln{(LPIND)}$","$\\ln{(LPFBS)}$","$\\ln{(LPCONS)}$","$\\ln{(O_3)}$", "$\\ln{(AGRI)}$","$\\ln{(WRT)}$","$\\ln{(LPWRT)}$","$\\ln{(AREA)}$","$\\ln{(AREA)}^2$","$Constant$"), label = "tab:subsample")#,add.lines = list(aic)###, ,###, "FBS", "CONS"


#----------different weights matrices



bptest<-c("BP-Test",round(bptest.sarlm(saci)$statistic,3),round(bptest.sarlm(sac7)$statistic,3),round(bptest.sarlm(sacd)$statistic,3),round(bptest.sarlm(sacq)$statistic,3))
pval<-c("\\textit{ p-value}",round(bptest.sarlm(saci)$p.value,5),round(bptest.sarlm(sac7)$p.value,5),round(bptest.sarlm(sacd)$p.value,5),round(bptest.sarlm(sacq)$p.value,5))
aic<-c("AIC",round(-saci$LL*2+2*saci$parameters,3),round(-sac7$LL*2+2*sac7$parameters,3),round(-sacd$LL*2+2*sacd$parameters,3),round(-sacq$LL*2+2*sacq$parameters,3))

stargazer(sIVhac2star(SAR.2STLS.HAC_i,fd),sIVhac2star(SAR.2STLS.HAC_7,fd),sIVhac2star(SAR.2STLS.HAC_d,fd),sIVhac2star(SAR.2STLS.HAC_Q,fd), type="latex", title = "Results for different spatial weights matrices", style = "default",column.labels   = c("Inv. dis.","k=7","Dis. band","Queen"), dep.var.labels = "$PM_{10}$", no.space=TRUE, single.row=TRUE,align = FALSE,keep.stat=c("n"),add.lines = list(bptest,pval,aic),covariate.labels = c("$W*PM_{10}$","$\\% FDI)$","$\\ln{(GDPpc)}$", "$\\ln{(GDPpc)}^2$", "$\\ln{(POP)}$", "$\\ln{(IND)}$", "$\\ln{(AGRI)}$", "$\\ln{(CONS)}$", "$\\ln{(WRT)}$", "$\\ln{(LPIND)}$","$\\ln{(LPCONS)}$","$\\ln{(LPWRT)}$","$\\ln{(O_3)}$","$\\ln{(AREA)}$","$\\ln{(AREA)}^2$","$Constant$"), label = "tab:spatialw")

