library(bfast)
setwd("/hds/dados_work/LapigDrive/series_temporais_tanguro_2/csv/")
arquivos2=Sys.glob("*.csv")
arquivos=arquivos2[3:length(arquivos2)]

arquivos <- grep("27621", arquivos, value = TRUE)

#Lista com todos os csv
pixels=list()
for(i in 1:length(arquivos)){
  
  pixels[[i]]=read.csv(file= arquivos[i], header = T)
  
}

#Lista com todos os pontos centrais
ts=list()
a=1
for (j in 1:length(arquivos)){
  
  ts[[j]]=ts(as.numeric(pixels[[j]][5,4:395]), start= 2000.49, frequency = 23)
  
}

#Retirar NA's
i=1
a=1
b=1
for(i in 1:length(ts)){
  
  for(j in 1:392){
    if(is.na(ts[[i]][j])){
      if(is.na(ts[[i]][j-1])){
        a=2
      }else(a=1)
      if(is.na(ts[[i]][j+1])){
        b=2
      }else(b=1)
      ts[[i]][j]=mean(ts[[i]][j-a],ts[[i]][j+b])
    } else(print(c(i,j)))
  }
}

bb=bfast(ts[[1]], season = "harmonic", max.iter = 1)
plot(bb)
t=paste0("Ponto ", substr(arquivos[i], 15,19),": ",titulos[i])
plot(bb, main=t)

plot(bb, main = "", xlab = "data")


?bfast


library(ggplot2)
library(reshape2)
#Packages, functions and configurations
options(scipen = 999)
library(raster)
library(rgdal)
library(scales)
library(ggplot2)
library(gpclib)
library(maptools)
library(rasterVis)
gpclibPermit()


Time <- as.numeric(time(bb$Yt))
Yt <- as.numeric(bb$Yt)
Tt <- as.numeric(bb$output[[1]]$Tt)
St <- as.numeric(bb$output[[1]]$St)
#et <- as.numeric(bb$output[[1]]$)

bfastPLOT <- as.data.frame(cbind(Time, Yt, Tt, St))
head(bfastPLOT)

bfMelt <- melt(bfastPLOT, id = c('Time'), value.name = "value")
head(bfMelt)

ggplot(data = bfMelt, aes(x = Time, y = value, colour = variable))+
  geom_line(show.legend = FALSE)+
  facet_grid(variable ~ ., scales = "free")
  
?plot.bfast
  
  plot(bb, main = "")
  
  
  
  #Areas analisadas
  ggplot(data = fortfyGridBr) +
  geom_polygon(aes(x = long, fill = id , y = lat, group = group),
               fill = alpha("gray", 0.5),
               colour = "gray") + 
  geom_polygon(data = fortfyGridBr[fortfyGridBr$id %in% id, ],
               aes(x = long, fill = id , y = lat, group = group),
               fill = "red", colour = "black") + 
  labs(x = "longitude", y = 'latitude') +
  ggtitle("Áreas para analisar mascara e pastagem")+
  guides(colour=TRUE, fill = TRUE) + #remove legenda
  coord_equal() + 
  theme_light(base_size = 14,
              base_family = "")




