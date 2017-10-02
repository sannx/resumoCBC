setwd("C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\outubro")
dir()
dat_or=read.table(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\outubro\\acerto.txt", header = T)



dat=dat_or

#############
algo=unique(dat_or$algoritmo)
dat=dat_or[dat_or$algoritmo==as.character(algo[4]),]
h=unique(dat$h)

h1=c(sum(dat$acertos[dat$h==h[1]]),
     sum(dat$erro_detec[dat$h==h[1]]),
     sum(dat$erro_n_detec[dat$h==h[1]]))


h2=c(sum(dat$acertos[dat$h==h[2]]),
     sum(dat$erro_detec[dat$h==h[2]]),
     sum(dat$erro_n_detec[dat$h==h[2]]))

h3=c(sum(dat$acertos[dat$h==h[3]]),
     sum(dat$erro_detec[dat$h==h[3]]),
     sum(dat$erro_n_detec[dat$h==h[3]]))

h4=c(sum(dat$acertos[dat$h==h[4]]),
     sum(dat$erro_detec[dat$h==h[4]]),
     sum(dat$erro_n_detec[dat$h==h[4]]))

h5=c(sum(dat$acertos[dat$h==h[5]]),
     sum(dat$erro_detec[dat$h==h[5]]),
     sum(dat$erro_n_detec[dat$h==h[5]]))

h6=c(sum(dat$acertos[dat$h==h[6]]),
     sum(dat$erro_detec[dat$h==h[6]]),
     sum(dat$erro_n_detec[dat$h==h[6]]))

h7=c(sum(dat$acertos[dat$h==h[7]]),
     sum(dat$erro_detec[dat$h==h[7]]),
     sum(dat$erro_n_detec[dat$h==h[7]]))

h8=c(sum(dat$acertos[dat$h==h[8]]),
     sum(dat$erro_detec[dat$h==h[8]]),
     sum(dat$erro_n_detec[dat$h==h[8]]))

h9=c(sum(dat$acertos[dat$h==h[9]]),
     sum(dat$erro_detec[dat$h==h[9]]),
     sum(dat$erro_n_detec[dat$h==h[9]]))

h10=c(sum(dat$acertos[dat$h==h[10]]),
     sum(dat$erro_detec[dat$h==h[10]]),
     sum(dat$erro_n_detec[dat$h==h[10]]))

hh=matrix(c(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10), ncol=3, byrow = T)
rownames(nbw)=1:10


barplot(t(hh), beside = T, main="Eficácia segundo valor de h",
        legend = c("Acerto","Erro de Comissão","Erro de Omissão"),
        xlab = " h",ylab="Quantidade")

box()





##################################################################################
##################################################################################
##################################################################################
##################################################################################

library(reshape2)
library(ggplot2)

blw=blw[c(1,2,3)]
bl=bl[c(1,2,3)]
nb=nb[c(1,2,3)]
nbw=nbw[c(1,2,3)]

blw <- data.frame(blw)
names(blw) <- c("Acerto", "Comissão", "Omissão")
blw$H <- c("01","02","03","04","05","06","07","08","09","10") #mudar para ano
blw$Algoritmo="BFASTa Wavelet"

blwMelt <- melt(blw, value.name = 'value')
names(blwMelt) <- c("H", "Algoritmo","Acuracia", "NBK")
############
bl <- data.frame(bl)
names(bl) <- c("Acerto", "Comissão", "Omissão")
bl$H <- c("01","02","03","04","05","06","07","08","09","10")
bl$Algoritmo="BFASTa"

blMelt <- melt(bl, value.name = 'value')

names(blMelt) <- c("H", "Algoritmo","Acuracia", "NBK")

############
nbw <- data.frame(nbw)
names(nbw) <- c("Acerto", "Comissão", "Omissão")
nbw$H <- c()
nbw$Algoritmo="BFAST Wavelet"

nbwMelt <- melt(nbw, value.name = 'value')

names(nbwMelt) <- c("H", "Algoritmo","Acuracia", "NBK")
###############
nb <- data.frame(nb)
names(nb) <- c("Acerto", "Comissão", "Omissão")
nb$H <- c(6,12,18,24,)
nb$Algoritmo="BFAST"

nbMelt <- melt(nb, value.name = 'value')

names(nbMelt) <- c("H", "Algoritmo","Acuracia", "NBK")


bfast=rbind(blMelt,blwMelt,nbMelt,nbwMelt)

write.csv(bfast, file = 'L:\\DATASAN\\BfastFigura4.csv', row.names = FALSE)

bfast <- read.csv('/hds/dados_work/GitHub/resumoCBC/BfastFigura4.csv')
head(bfast)
sapply(bfast, class)

ggplot(data = bfast, aes(x = H, y = NBK, colour = Acuracia))+
  geom_point()+
  facet_grid(Algoritmo ~ ., scales = "free")


##################################################################################
##################################################################################
##################################################################################
##################################################################################

