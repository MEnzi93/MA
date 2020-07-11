library(ggplot2)
theme_set(theme_bw())
library(sf)
library("rnaturalearth")
library("rnaturalearthdata")
library(RColorBrewer)
library(rgeos)
library(ggspatial)
library(maps)
library(ggthemes)

getwd()

spdf <- get_eurostat_geospatial(output_class = "spdf", resolution = "10", nuts_level = "3", year = "2013",cache = TRUE,update_cache = FALSE, cache_dir = NULL)


oversea<-read_xlsx("less_developed.xlsx",4)
bad<-c("AL", "BA", "MK", "RS", "TR")#because of bad data quality in fdi Data


spdf <- spdf[!substr(spdf$id,1,4) %in% oversea$NUTS, ]
spdf <- spdf[spdf$CNTR_CODE %in% unique(trans$country), ]

spdf <- gBuffer(spdf, byid=TRUE, width=0)# damit die nächsten 2 Befehle funktionieren 
# map_count<-fortify(spdf, region="CNTR_CODE")
map_eu <- fortify(spdf, region="id")
map_count <- fortify(spdf, region="CNTR_CODE")


map_eu <- map_eu%>% left_join(test, by=c("id"="nuts"))

map_eu <- map_eu%>% left_join(trans[,c("nuts","pm10")], by=c("id"="nuts"))



world <- ne_countries(scale = "medium", returnclass = "sf")


myPalette <- colorRampPalette(rev(brewer.pal(11, "RdYlGn")))
mypallette<-myPalette(5)
mypallette[3]<-"#FFFFFF"

ggplot(data = world) +
  geom_sf()+
  coord_sf(xlim =  c(-10, 30),ylim = c(36, 70))+
  geom_polygon(data=map_eu, aes(fill=cut(beta_fdi_only,breaks=c(-1,-0.022,-0.011,0.011,0.022,4),labels = c("under -0.022","-0.022 to -0.011","-0.011 to 0.011","0.011 to 0.022","over 0.022")), x=long, y=lat,group=group))+
  geom_path(data = map_count, aes(x=long, y=lat, group=group), color="black", size=0.15)+
  #, caption = "Source: ESPON - based on own calculations"
  # annotation_scale(location = "bl", width_hint = 0.5) + 
  #  annotation_north_arrow(location = "bl", which_north = "true",
  #                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                         style = north_arrow_fancy_orienteering)+
  #  scale_fill_gradientn(colours = myPalette(6),breaks = waiver(), na.value = "grey80",guide = "colourbar")+
  #,labels = c("under -0.022","-0.022 to -0.011","0.011 to 0.011","0.011 to 0.022","over 0.022")
  scale_fill_manual(values = mypallette, na.value = "grey60", name="FDI Coefficient")+
  theme_map()+
  theme(legend.position = c(0.01,.77), panel.background = element_rect(fill = "aliceblue"),legend.text=element_text(size=12),legend.title=element_text(size=13))
  

ggplot(data = world) +
  geom_sf()+
  coord_sf(xlim =  c(-10, 30),ylim = c(36, 70))+
  geom_polygon(data=map_eu, aes(fill=cut(beta_fdi,breaks=c(-1,-0.022,-0.011,0.011,0.022,4),labels = c("under -0.022","-0.022 to -0.011","-0.011 to 0.011","0.011 to 0.022","over 0.022")), x=long, y=lat,group=group))+
  geom_path(data = map_count, aes(x=long, y=lat, group=group), color="black", size=0.15)+
  #, caption = "Source: ESPON - based on own calculations"
  # annotation_scale(location = "bl", width_hint = 0.5) + 
  #  annotation_north_arrow(location = "bl", which_north = "true",
  #                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                         style = north_arrow_fancy_orienteering)+
  #  scale_fill_gradientn(colours = myPalette(6),breaks = waiver(), na.value = "grey80",guide = "colourbar")+
  #,labels = c("under -0.022","-0.022 to -0.011","0.011 to 0.011","0.011 to 0.022","over 0.022")
  scale_fill_manual(values = mypallette, na.value = "grey60", name="FDI Coefficient")+
  theme_map()+
  theme(legend.position = c(0.01,.77), panel.background = element_rect(fill = "aliceblue"),legend.text=element_text(size=12),legend.title=element_text(size=13))


ggplot(data = world) +
  geom_sf()+
  coord_sf(xlim =  c(-10, 30),ylim = c(36, 70))+
  geom_polygon(data=map_eu, aes(fill=cut(beta_fdi2,breaks=c(-1,-0.0027,-0.0016,0.0016,0.0027,4),labels = c("under -0.0027","-0.0027 to -0.0016","-0.0016 to 0.0016","0.0016 to 0.0027","over 0.0027")), x=long, y=lat,group=group))+
  geom_path(data = map_count, aes(x=long, y=lat, group=group), color="black", size=0.15)+
  #, caption = "Source: ESPON - based on own calculations"
  # annotation_scale(location = "bl", width_hint = 0.5) + 
  #  annotation_north_arrow(location = "bl", which_north = "true",
  #                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                         style = north_arrow_fancy_orienteering)+
  #  scale_fill_gradientn(colours = myPalette(6),breaks = waiver(), na.value = "grey80",guide = "colourbar")+
  #,labels = c("under -0.022","-0.022 to -0.011","0.011 to 0.011","0.011 to 0.022","over 0.022")
  scale_fill_manual(values = mypallette, na.value = "grey60", name="FDI² Coefficient")+
  theme_map()+
  theme(legend.position = c(0.01,.77), panel.background = element_rect(fill = "aliceblue"),legend.text=element_text(size=12),legend.title=element_text(size=13))




ggplot(data = world) +
  geom_sf()+
  geom_polygon(data=map_eu, aes(fill=cut(pm10,c(0,10,15,20,25,30,35,40,50),labels = c("<10","10-15","15-20","20-25","25-30","30-35","35-40","40<")), x=long, y=lat,group=group))+
  geom_path(data = map_count, aes(x=long, y=lat, group=group), color="black", size=0.15) +
  labs(x=NULL, y=NULL, caption = "Source: EEA - based on own calculations")+
#  annotation_scale(location = "bl", width_hint = 0.5) +
#  annotation_north_arrow(location = "bl", which_north = "true",
#                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
#                         style = north_arrow_fancy_orienteering)+
  scale_fill_manual(values = c("#006837","#4CB05C","#B7E075","#FEF0A7","#FDBE6E","#EA5839","#D22B26","#A50026"), na.value = "grey60", name="PM 10")+

  theme_map()+
  coord_sf(xlim =  c(-10, 30),ylim = c(36, 70))+
  theme(legend.position = c(0.01,.68), panel.background = element_rect(fill = "aliceblue"),legend.text=element_text(size=12),legend.title=element_text(size=13))
 # theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "white"))


ggplot(data = world) +
  geom_sf()+
    geom_polygon(data=map_eu, aes(fill=cut(value,c(0,0.0000000000000001,.001,0.01,0.05,.15,50),labels = c("zero","<0.1%","0.1%-1%","1%-5%","5%-15%","15%<"),include.lowest = TRUE), x=long, y=lat,group=group))+
    geom_path(data = map_count, aes(x=long, y=lat, group=group), color="black", size=0.15) +
    labs(x=NULL, y=NULL, caption = "Source: ESPON - based on own calculations")+
    #  annotation_scale(location = "bl", width_hint = 0.5) +
    #  annotation_north_arrow(location = "bl", which_north = "true",
    #                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
    #                         style = north_arrow_fancy_orienteering)+
    scale_fill_manual(values =  c("#FFFFFF","#FDE0DD", "#FA9FB5", "#F768A1", "#AE017E" ,"#7A0177"), na.value = "grey60", name="FDI Dependence")+

    theme_map()+
    coord_sf(xlim =  c(-10, 30),ylim = c(36, 70))+
    theme(legend.position = c(0.01,.74), panel.background = element_rect(fill = "aliceblue"),legend.text=element_text(size=12),legend.title=element_text(size=13))
#,probs = seq(0, 1, 0.2),na.rm = TRUE)


#myPalette1 <- colorRampPalette(brewer.pal(9, "RdPu"))
#myPalette1(9)



ggplot(data = world) +
  geom_sf()+
  geom_polygon(data=map_eu, aes(fill=quad_sig, x=long, y=lat,group=group))+
  geom_path(data = map_count, aes(x=long, y=lat, group=group), color="black", size=0.15) +
  labs(x=NULL, y=NULL)+
  #  annotation_scale(location = "bl", width_hint = 0.5) +
  #  annotation_north_arrow(location = "bl", which_north = "true",
  #                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                         style = north_arrow_fancy_orienteering)+
  scale_fill_manual(values =  c("blue","green", "white"), na.value = "grey60", name="Clusters")+
  #scale_fill_brewer(palette = "Set1")+
  theme_map()+
  coord_sf(xlim =  c(-10, 30),ylim = c(36, 70))+
  theme(legend.position = c(0.01,.82), panel.background = element_rect(fill = "aliceblue"),legend.text=element_text(size=13),legend.title=element_text(size=14))



ggplot(data = world) +
  geom_sf()+
  geom_polygon(data=map_eu, aes(fill=quad_sig_gdp, x=long, y=lat,group=group))+
  geom_path(data = map_count, aes(x=long, y=lat, group=group), color="black", size=0.15) +
  labs(x=NULL, y=NULL)+
  #  annotation_scale(location = "bl", width_hint = 0.5) +
  #  annotation_north_arrow(location = "bl", which_north = "true",
  #                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                         style = north_arrow_fancy_orienteering)+
  scale_fill_manual(values =  c("blue","green", "white"), na.value = "grey60", name="Clusters")+
  #scale_fill_brewer(palette = "Set1")+
  theme_map()+
  coord_sf(xlim =  c(-10, 30),ylim = c(36, 70))+
  theme(legend.position = c(0.01,.82), panel.background = element_rect(fill = "aliceblue"),legend.text=element_text(size=13),legend.title=element_text(size=14))




ggplot(data = world) +
  geom_sf()+
  coord_sf(xlim =  c(-10, 30),ylim = c(37, 70))+
  geom_polygon(data=map_eu, aes(fill=cut(pm10,breaks=c(-5,-.1,0,.1,5),labels = c("under -0.1%","-0.1% - 0%","0% - 0.1%","over 0.1%")), x=long, y=lat,group=group))+
  geom_path(data = map_count, aes(x=long, y=lat, group=group), color="black", size=0.15)+
  #, caption = "Source: ESPON - based on own calculations"
  # annotation_scale(location = "bl", width_hint = 0.5) + 
  #  annotation_north_arrow(location = "bl", which_north = "true",
  #                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                         style = north_arrow_fancy_orienteering)+
  #  scale_fill_gradientn(colours = myPalette(6),breaks = waiver(), na.value = "grey80",guide = "colourbar")+
  #,labels = c("under -0.022","-0.022 to -0.011","0.011 to 0.011","0.011 to 0.022","over 0.022")
  scale_fill_manual(values = myPalette(4), na.value = "grey80", name="Ln aprox. growth rate PM10")+
  theme_map()+
  theme(legend.position = c(0.01,.80), panel.background = element_rect(fill = "aliceblue"),legend.text=element_text(size=12),legend.title=element_text(size=13))
  
ggplot(trans, aes(x=pm10)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+
  labs(x="Ln aprox. growth rate PM10", y = "")+
  theme_classic()+
  theme(axis.line.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),text = element_text(size=14))


