################################################################################################
################################################################################################
################################################################################################
################################################################################################
####Sazonalidade
dat2=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_mean.csv")
dat2=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_cent.csv")

erros=dat2$erro_detec+dat2$erro_n_detec
dat2$dat_mean...6.=dat2$acerto_ausencia+dat2$acerto_pastagem_soja+dat2$acerto_soja_sojamilho+dat2$acerto_soja_seringa+dat2$acerto_floresta_pastagem
dat2=cbind(dat2,erros)

fator=as.character(unique(dat2$season))
xx=c(sum(dat2$dat_mean...6.[dat2$season=="harmonic"]),
     sum(dat2$erro_detec[dat2$season=="harmonic"]),
     sum(dat2$erro_n_detec[dat2$season=="harmonic"]),
     
     sum(dat2$dat_mean...6.[dat2$season=="dummy"]),
     sum(dat2$erro_detec[dat2$season=="dummy"]),
     sum(dat2$erro_n_detec[dat2$season=="dummy"]),
     
     sum(dat2$dat_mean...6.[dat2$season=="none"]),
     sum(dat2$erro_detec[dat2$season=="none"]),
     sum(dat2$erro_n_detec[dat2$season=="none"]))

dados=t(matrix(xx, nrow=3, ncol=3, dimnames = list(c("acertos","erro de comissão","erro de omissão"),fator) ))
chisq.test(dados)



################################################################################################
################################################################################################
################################################################################################
################################################################################################
####Valor de h
dat2=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_mean.csv")
dat2=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_cent.csv")

erros=dat2$erro_detec+dat2$erro_n_detec
dat2$dat_mean...6.=dat2$acerto_ausencia+dat2$acerto_pastagem_soja+dat2$acerto_soja_sojamilho+dat2$acerto_soja_seringa+dat2$acerto_floresta_pastagem
dat2=cbind(dat2,erros)

fator=as.character(unique(dat2$h))
xx=c(sum(dat2$dat_mean...6.[dat2$h==fator[1]]),
     sum(dat2$erro_detec[dat2$h==fator[1]]),
     sum(dat2$erro_n_detec[dat2$h==fator[1]]),
     
     sum(dat2$dat_mean...6.[dat2$h==fator[2]]),
     sum(dat2$erro_detec[dat2$h==fator[2]]),
     sum(dat2$erro_n_detec[dat2$h==fator[2]]),
     
     sum(dat2$dat_mean...6.[dat2$h==fator[3]]),
     sum(dat2$erro_detec[dat2$h==fator[3]]),
     sum(dat2$erro_n_detec[dat2$h==fator[3]]),
     
     sum(dat2$dat_mean...6.[dat2$h==fator[4]]),
     sum(dat2$erro_detec[dat2$h==fator[4]]),
     sum(dat2$erro_n_detec[dat2$h==fator[4]]),
     
     sum(dat2$dat_mean...6.[dat2$h==fator[5]]),
     sum(dat2$erro_detec[dat2$h==fator[5]]),
     sum(dat2$erro_n_detec[dat2$h==fator[5]]),
     
     sum(dat2$dat_mean...6.[dat2$h==fator[6]]),
     sum(dat2$erro_detec[dat2$h==fator[6]]),
     sum(dat2$erro_n_detec[dat2$h==fator[6]]),
     
     sum(dat2$dat_mean...6.[dat2$h==fator[7]]),
     sum(dat2$erro_detec[dat2$h==fator[7]]),
     sum(dat2$erro_n_detec[dat2$h==fator[7]]),
     
     sum(dat2$dat_mean...6.[dat2$h==fator[8]]),
     sum(dat2$erro_detec[dat2$h==fator[8]]),
     sum(dat2$erro_n_detec[dat2$h==fator[8]]),
     
     sum(dat2$dat_mean...6.[dat2$h==fator[9]]),
     sum(dat2$erro_detec[dat2$h==fator[9]]),
     sum(dat2$erro_n_detec[dat2$h==fator[9]]),
     
     sum(dat2$dat_mean...6.[dat2$h==fator[10]]),
     sum(dat2$erro_detec[dat2$h==fator[10]]),
     sum(dat2$erro_n_detec[dat2$h==fator[10]]))

dados=t(matrix(xx, nrow=3, ncol=10, dimnames = list(c("acertos","erro de comissão","erro de omissão"),fator) ))
chisq.test(dados)


par(mar=c(4,4,4,4),cex.axis=0.9)
names=as.character(1:10)
barplot(t(dados),beside = T ,main="Acertos e erros segundo valor de h (pixel central)",col=c("black","8","white"), 
        las=1,names.arg = names, ylab="quantidade",xlab="h", ylim=c(1,1500))

legend(23.50,1550,colnames(dados),fill =c("black","gray","white"),   bty="n")

box()


barplot(t(dados),beside = T ,main="Acertos e erros segundo valor de h (média dos pixels)",col=c("black","8","white"), 
        las=1,names.arg = names, ylab="quantidade",xlab="h", ylim=c(1,1500))

legend(23.50,1550,colnames(dados),fill =c("black","gray","white"),   bty="n")

box()


################################################################################################
################################################################################################
################################################################################################
################################################################################################
####Algoritmo
dat2=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_mean.csv")
dat2=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_cent.csv")

erros=dat2$erro_detec+dat2$erro_n_detec
dat2$dat_mean...6.=dat2$acerto_ausencia+dat2$acerto_pastagem_soja+dat2$acerto_soja_sojamilho+dat2$acerto_soja_seringa+dat2$acerto_floresta_pastagem
dat2=cbind(dat2,erros)

fator=as.character(unique(dat2$algoritmo))
xx=c(sum(dat2$dat_mean...6.[dat2$algoritmo==fator[1]]),
     sum(dat2$erro_detec[dat2$algoritmo==fator[1]]),
     sum(dat2$erro_n_detec[dat2$algoritmo==fator[1]]),
     
     sum(dat2$dat_mean...6.[dat2$algoritmo==fator[2]]),
     sum(dat2$erro_detec[dat2$algoritmo==fator[2]]),
     sum(dat2$erro_n_detec[dat2$algoritmo==fator[2]]),
     
     sum(dat2$dat_mean...6.[dat2$algoritmo==fator[3]]),
     sum(dat2$erro_detec[dat2$algoritmo==fator[3]]),
     sum(dat2$erro_n_detec[dat2$algoritmo==fator[3]]),
     
     sum(dat2$dat_mean...6.[dat2$algoritmo==fator[4]]),
     sum(dat2$erro_detec[dat2$algoritmo==fator[4]]),
     sum(dat2$erro_n_detec[dat2$algoritmo==fator[4]]))

dados=t(matrix(xx, nrow=3, ncol=4, dimnames = list(c("acertos","erro de comissão","erro de omissão"),fator) ))
chisq.test(dados)


par(mar=c(4,4,4,1),cex.axis=0.9)
names=fator
barplot(t(dados),beside = T ,main="Acertos e erros segundo 
algoritmo (média dos pixels)",col=c("black","8","white"), 
        las=1,names.arg = names, ylab="quantidade",xlab="algoritmo", ylim=c(1,2500))

legend(0.1,2550,colnames(dados),fill =c("black","gray","white"),   bty="n")

box()


par(mar=c(4,4,4,1),cex.axis=0.9)
names=fator
barplot(t(dados),beside = T ,main="Acertos e erros segundo 
algoritmo (pixel central)",col=c("black","8","white"), 
        las=1,names.arg = names, ylab="quantidade",xlab="algoritmo", ylim=c(1,2500))

legend(0.1,2550,colnames(dados),fill =c("black","gray","white"),   bty="n")

box()
################################################################################################
################################################################################################
################################################################################################
################################################################################################
####Filtro
dat2=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_mean.csv")
dat2=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_cent.csv")

erros=dat2$erro_detec+dat2$erro_n_detec
dat2$dat_mean...6.=dat2$acerto_ausencia+dat2$acerto_pastagem_soja+dat2$acerto_soja_sojamilho+dat2$acerto_soja_seringa+dat2$acerto_floresta_pastagem
dat2=cbind(dat2,erros)

fator=as.character(unique(dat2$algoritmo))
xx2=c(sum(dat2$dat_mean...6.[dat2$algoritmo==fator[1]]),
     sum(dat2$erro_detec[dat2$algoritmo==fator[1]]),
     sum(dat2$erro_n_detec[dat2$algoritmo==fator[1]]),
     
     sum(dat2$dat_mean...6.[dat2$algoritmo==fator[2]]),
     sum(dat2$erro_detec[dat2$algoritmo==fator[2]]),
     sum(dat2$erro_n_detec[dat2$algoritmo==fator[2]]),
     
     sum(dat2$dat_mean...6.[dat2$algoritmo==fator[3]]),
     sum(dat2$erro_detec[dat2$algoritmo==fator[3]]),
     sum(dat2$erro_n_detec[dat2$algoritmo==fator[3]]),
     
     sum(dat2$dat_mean...6.[dat2$algoritmo==fator[4]]),
     sum(dat2$erro_detec[dat2$algoritmo==fator[4]]),
     sum(dat2$erro_n_detec[dat2$algoritmo==fator[4]]))

xx=c(sum(xx2[1],xx2[10]),sum(xx2[2],xx2[11]),sum(xx2[3],xx2[12]),
     sum(xx2[4],xx2[7]),sum(xx2[5],xx2[8]),sum(xx2[6],xx2[9]))

names=c("Filtrado","Não Filtrado")
dados=t(matrix(xx, nrow=3, ncol=2, dimnames = list(c("acertos","erro de comissão","erro de omissão"),names) ))
chisq.test(dados)


par(mar=c(4,4,4,1),cex.axis=0.9)

barplot(t(dados),beside = T ,main="Acertos e erros segundo filtro (média dos pixels)",col=c("black","8","white"), 
        las=1,names.arg = names, ylab="quantidade",xlab="algoritmo", ylim=c(1,4500))

legend(0.6,4500,c("acertos","e. comissão",
                   "e. omissão"),fill =c("black","gray","white"),   bty="n")

box()


par(mar=c(4,4,4,1),cex.axis=0.9)

barplot(t(dados),beside = T ,main="Acertos e erros segundo filtro (pixel central)",col=c("black","8","white"), 
        las=1,names.arg = names, ylab="quantidade",xlab="algoritmo", ylim=c(1,4500))

legend(0.6,4500,c("acertos","e. comissão",
                  "e. omissão"),fill =c("black","gray","white"),   bty="n")

box()

################################################################################################
################################################################################################
################################################################################################
################################################################################################
####Uso do solo
dat_mean=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_mean.csv")
dat_mean=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_cent.csv")

erros=dat_mean$erro_detec+dat_mean$erro_n_detec

dat_mean$dat_mean...6.=dat_mean$acerto_ausencia+dat_mean$acerto_pastagem_soja+dat_mean$acerto_soja_sojamilho+dat_mean$acerto_soja_seringa+dat_mean$acerto_floresta_pastagem

dat_mean=cbind(dat_mean,erros)

fator=as.character(unique( substr(as.character(dat_mean$identificador.do.pixel) , 1,12)))


xx=c(sum(dat_mean$dat_mean...6.[substr(as.character(dat_mean$identificador.do.pixel) , 1,12)==fator[1]]),
     sum(dat_mean$erro_detec[substr(as.character(dat_mean$identificador.do.pixel) , 1,12)==fator[1]]),
     sum(dat_mean$erro_n_detec[substr(as.character(dat_mean$identificador.do.pixel) , 1,12)==fator[1]]),
     
     sum(dat_mean$dat_mean...6.[substr(as.character(dat_mean$identificador.do.pixel) , 1,12)==fator[2]]),
     sum(dat_mean$erro_detec[substr(as.character(dat_mean$identificador.do.pixel) , 1,12)==fator[2]]),
     sum(dat_mean$erro_n_detec[substr(as.character(dat_mean$identificador.do.pixel) , 1,12)==fator[2]]),
     
     sum(dat_mean$dat_mean...6.[substr(as.character(dat_mean$identificador.do.pixel) , 1,12)==fator[3]]),
     sum(dat_mean$erro_detec[substr(as.character(dat_mean$identificador.do.pixel) , 1,12)==fator[3]]),
     sum(dat_mean$erro_n_detec[substr(as.character(dat_mean$identificador.do.pixel) , 1,12)==fator[3]]),
     
     sum(dat_mean$dat_mean...6.[substr(as.character(dat_mean$identificador.do.pixel) , 1,12)==fator[4]]),
     sum(dat_mean$erro_detec[substr(as.character(dat_mean$identificador.do.pixel) , 1,12)==fator[4]]),
     sum(dat_mean$erro_n_detec[substr(as.character(dat_mean$identificador.do.pixel) , 1,12)==fator[4]]),
     
     sum(dat_mean$dat_mean...6.[substr(as.character(dat_mean$identificador.do.pixel) , 1,12)==fator[5]]),
     sum(dat_mean$erro_detec[substr(as.character(dat_mean$identificador.do.pixel) , 1,12)==fator[5]]),
     sum(dat_mean$erro_n_detec[substr(as.character(dat_mean$identificador.do.pixel) , 1,12)==fator[5]]),
     
     sum(dat_mean$dat_mean...6.[substr(as.character(dat_mean$identificador.do.pixel) , 1,12)==fator[6]]),
     sum(dat_mean$erro_detec[substr(as.character(dat_mean$identificador.do.pixel) , 1,12)==fator[6]]),
     sum(dat_mean$erro_n_detec[substr(as.character(dat_mean$identificador.do.pixel) , 1,12)==fator[6]]))


dados=t(matrix(xx, nrow=3, ncol=6, dimnames = list(c("acertos","erro de comissão","erro de omissão"),fator) ))

dados_temp=dados
dados_temp[1,]=dados[3,]
dados_temp[2,]=dados[1,]
dados_temp[3,]=dados[4,]
dados_temp[4,]=dados[2,]
dados_temp[5,]=dados[5,]
dados_temp[6,]=dados[6,]

# legend(2.2,2080,colnames(dados),density = rep(40,3), col=c("darkgreen","orange","red"))
par(mar=c(7,4,3,2),cex.axis=0.9)
names=c("Soja-Milho","Floresta-Floresta","Pastagem","Seringa","Soja","Soja-Seringa")
barplot(t(dados_temp),beside = T ,main="Eficácia segundo classe de uso
do solo (média dos pixels)",col=c("black","gray","white"), las=2,names.arg = names, ylim=c(1,3500))

legend(17.02,3600,c("acertos","e. comissão", "e. omissão" ), fill=c("black","gray","white"),   bty="n")

box()



par(mar=c(7,4,3,2),cex.axis=0.9)
names=c("Soja-Milho","Floresta-Floresta","Pastagem","Seringa","Soja","Soja-Seringa")
barplot(t(dados),beside = T ,main="Eficácia segundo classe de uso
        do solo (pixel central)",col=c("black","gray","white"), las=2,names.arg = names, ylim=c(1,3500))

legend(17.02,3600,c("acertos","e. comissão", "e. omissão" ), fill=c("black","gray","white"),   bty="n")

box()
#qui quadrado independencia
chisq.test(dados)
################################################################################################
################################################################################################
################################################################################################
################################################################################################
####Metodologia
dat2=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_mean.csv")
dat1=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_cent.csv")

erros=dat2$erro_detec+dat2$erro_n_detec
dat2$dat_mean...6.=dat2$acerto_ausencia+dat2$acerto_pastagem_soja+dat2$acerto_soja_sojamilho+dat2$acerto_soja_seringa+dat2$acerto_floresta_pastagem
dat2=cbind(dat2,erros)

erros=dat1$erro_detec+dat1$erro_n_detec
dat1$dat_mean...6.=dat1$acerto_ausencia+dat1$acerto_pastagem_soja+dat1$acerto_soja_sojamilho+dat1$acerto_soja_seringa+dat1$acerto_floresta_pastagem
dat1=cbind(dat1,erros)

xx=c(sum(dat2$dat_mean...6.),
     sum(dat2$erro_detec),
     sum(dat2$erro_n_detec),
     
     sum(dat1$dat_mean...6.),
     sum(dat1$erro_detec),
     sum(dat1$erro_n_detec))

fator=c("Média dos pixels","Pixel central")
dados=t(matrix(xx, nrow=3, ncol=2, dimnames = list(c("acertos","erro de comissão","erro de omissão"),fator) ))
chisq.test(dados)

par(mar=c(4,4,4,4),cex.axis=0.9)
names=fator
barplot(t(dados),beside = T ,main="Acertos e erros segundo metodologia",col=c("black","8","white"), 
        las=1,names.arg = names, ylab="quantidade",xlab="metodologia", ylim=c(1,7500))

legend(0.60,13850,colnames(dados),fill =c("black","gray","white"),   bty="n")

box()

