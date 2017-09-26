################################################################################################
################################################################################################
################################################################################################
################################################################################################
####Sazonalidade
dat2=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_mean.csv")
dat2=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_cent.csv")

library(lmtest)
tempo_h=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$season[] =="harmonic"]
tempo_d=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$season[] =="dummy"]
tempo_n=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$season[] =="none"]
tab_tempos_season=cbind(tempo_h,tempo_d,tempo_n)

#boxplot
box=boxplot(tab_tempos_season, xlab="Season", ylab="Tempo (segundos)", 
            names=c("harmonic", "dummy",    "none"),
            main="Season vs. Tempo de Execução
            (média dos pixels)")




#wilcox
wilcox.test(tempo_h,tempo_d, paired = T)
wilcox.test(tempo_h,tempo_n, paired = T)
wilcox.test(tempo_n,tempo_d, paired = T)

#Medias e desvios
medias=apply(tab_tempos_season,2,mean)
names(medias)=c("Média harmonic","Média dummy","Média none")
medias

################################################################################################
################################################################################################
################################################################################################
################################################################################################
###Valor de H
dat2=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_mean.csv")
dat2=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_cent.csv")

tempo_h1=dat2$tempo.de.execuÃ.Ã.o..segundos.[round(dat2$h[],8) == 0.02909758]
tempo_h2=dat2$tempo.de.execuÃ.Ã.o..segundos.[round(dat2$h[],8) ==  0.05819515]
tempo_h3=dat2$tempo.de.execuÃ.Ã.o..segundos.[round(dat2$h[],8) ==0.08729273]
tempo_h4=dat2$tempo.de.execuÃ.Ã.o..segundos.[round(dat2$h[],8) == 0.11639031]
tempo_h5=dat2$tempo.de.execuÃ.Ã.o..segundos.[round(dat2$h[],8) == 0.14548788]
tempo_h6=dat2$tempo.de.execuÃ.Ã.o..segundos.[round(dat2$h[],8) ==0.17458546]
tempo_h7=dat2$tempo.de.execuÃ.Ã.o..segundos.[round(dat2$h[],8) == 0.20368304]
tempo_h8=dat2$tempo.de.execuÃ.Ã.o..segundos.[round(dat2$h[],8) ==0.23278061]
tempo_h9=dat2$tempo.de.execuÃ.Ã.o..segundos.[round(dat2$h[],8) ==   0.26187819]
tempo_h10=dat2$tempo.de.execuÃ.Ã.o..segundos.[round(dat2$h[],8) ==   0.29097577]

tab_tempos_h=cbind(tempo_h1,tempo_h2,tempo_h3,tempo_h4,tempo_h5,
                   tempo_h6,tempo_h7,tempo_h8,tempo_h9,tempo_h10)


#boxplot


box=boxplot(tab_tempos_h, xlab="h", ylab="Tempo (segundos)", 
            names=seq(6,60,by=6),
            main="Valor de h vs. Tempo de Execução (média dos pixels)")


box=boxplot(tab_tempos_h, xlab="h", ylab="Tempo (segundos)", 
            names=seq(6,60,by=6),
            main="Valor de h vs. Tempo de Execução (pixel central)")

medias=apply(tab_tempos_h,2,mean)
print("médias")
medias
as.numeric(medias)

wilcox.test(tempo_h1,tempo_h2, paired = T)
wilcox.test(tempo_h2,tempo_h3, paired = T)
wilcox.test(tempo_h3,tempo_h4, paired = T)
wilcox.test(tempo_h4,tempo_h5, paired = T)
wilcox.test(tempo_h5,tempo_h6, paired = T)
wilcox.test(tempo_h6,tempo_h7, paired = T)
wilcox.test(tempo_h7,tempo_h8, paired = T)
wilcox.test(tempo_h8,tempo_h9, paired = T)
wilcox.test(tempo_h9,tempo_h10, paired = T)


################################################################################################
################################################################################################
################################################################################################
################################################################################################
####Algoritmo
dat2=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_mean.csv")
dat2=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_cent.csv")

algoritmos=as.character(unique(dat2$algoritmo))
algoritmos_rep=c(rep(algoritmos[1], 18090),rep(algoritmos[2], 18090),rep(algoritmos[3], 18090),rep(algoritmos[4], 18090))
#organizando tempos
tempo_luis_w=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$algoritmo[] =="bfast_luis_wavelet_java.r"]
tempo_luis=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$algoritmo[] =="bfast_luis_java.r"]
tempo_new=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$algoritmo[] =="new_bfast_java.r"]
tempo_new_w=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$algoritmo[] =="new_bfast_wavelet_java.r"]
tab_tempos_algoritmo=cbind(tempo_luis_w,tempo_luis,tempo_new,tempo_new_w)

#boxplot
box=boxplot(tab_tempos_algoritmo, xlab="Algoritmos", ylab="Tempo (segundos)", 
            names=c("luis_wavelet_java.r", "luis_java.r" ,       
                    "new_java.r",          "new_wavelet_java.r"),
            main="Algoritmo vs. Tempo de Execução (pixel central)")

box=boxplot(tab_tempos_algoritmo, xlab="Algoritmos", ylab="Tempo (segundos)", 
            names=c("luis_wavelet_java.r", "luis_java.r" ,       
                    "new_java.r",          "new_wavelet_java.r"),
            main="Algoritmo vs. Tempo de Execução (média dos pixels)")

#wilcox

wilcox.test(tempo_luis,tempo_luis_w,paired = T)
wilcox.test(tempo_luis,tempo_new,paired = T)
wilcox.test(tempo_luis,tempo_new_w,paired = T)

wilcox.test(tempo_luis_w,tempo_new,paired = T)
wilcox.test(tempo_luis_w,tempo_new_w,paired = T)

wilcox.test(tempo_new,tempo_new_w,paired = T)

#Medias e desvios
medias=apply(tab_tempos_algoritmo,2,mean)
print("médias")
medias



################################################################################################
################################################################################################
################################################################################################
################################################################################################
####Filtro
dat2=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_mean.csv")
dat2=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_cent.csv")

algoritmos=as.character(unique(dat2$algoritmo))
algoritmos_rep=c(rep(algoritmos[1], 18090),rep(algoritmos[2], 18090),rep(algoritmos[3], 18090),rep(algoritmos[4], 18090))
#organizando tempos
tempo_luis_w=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$algoritmo[] =="bfast_luis_wavelet_java.r"]
tempo_luis=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$algoritmo[] =="bfast_luis_java.r"]
tempo_new=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$algoritmo[] =="new_bfast_java.r"]
tempo_new_w=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$algoritmo[] =="new_bfast_wavelet_java.r"]
tab_tempos_algoritmo=cbind(tempo_luis_w,tempo_luis,tempo_new,tempo_new_w)

wavelet=c(tempo_luis_w,tempo_new_w)
not_w=c(tempo_luis,tempo_new)
tab_tempos_algoritmo=cbind(wavelet,not_w)

#boxplot
box=boxplot(tab_tempos_algoritmo, xlab="Algoritmos", ylab="Tempo (segundos)", 
            names=c("Filtrado","Não Filtrado"),
            main="Filtro vs. Tempo de Execução (média dos pixels)")


box=boxplot(tab_tempos_algoritmo, xlab="Algoritmos", ylab="Tempo (segundos)", 
            names=c("Filtrado","Não Filtrado"),
            main="Filtro vs. Tempo de Execução (pixel central)")
#wilcox

wilcox.test(wavelet, not_w,paired = T)
wilcox.test(wavelet, not_w,paired = F)

#Medias e desvios
medias=apply(tab_tempos_algoritmo,2,mean)
print("médias")
medias




################################################################################################
################################################################################################
################################################################################################
################################################################################################
####Numero de |BP
dat2=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_mean.csv")
dat2=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_cent.csv")


bp=nchar(as.character(dat2$dat_mean...6.))
#plot(dat2$tempo.de.execuÃ.Ã.o..segundos.~bp)
n_u=unique(bp)
medias=c()

for( i in 1:length(n_u)){
  
  medias[i]=  mean(dat2$tempo.de.execuÃ.Ã.o..segundos[bp==n_u[i]])
  
}
plot(medias,type="b", ylab = "Tempo médio de execução", xlab="Quantidade de BreakPoints encontrados", main="Quantidade de BreakPoints encontrados
     vs. Tempo médio de execução (média dos pixels)")

lines(4,medias[4], col="red", type = "b",pch=17)
lines(7,medias[7], col="red", type = "b",pch=17)

lines(5,medias[5], col="green", type = "b",pch=15)
lines(6,medias[6], col="green", type = "b",pch=15)
#lines(12,medias[12], col="blue", type = "b",pch=15)


lines(8,medias[8], col="blue", type = "b",pch=16)
lines(9,medias[9], col="blue", type = "b",pch=16)
lines(10,medias[10], col="blue", type = "b",pch=16)
lines(12,medias[12], col="green", type = "b",pch=16)
lines(11,medias[11], col="blue", type = "b",pch=16)


plot(medias,type="b", ylab = "Tempo médio de execução", xlab="Quantidade de BreakPoints encontrados", main="Quantidade de BreakPoints encontrados
     vs. Tempo médio de execução (pixel central)")


lines(8,medias[8], col="blue", type = "b",pch=16)
lines(9,medias[9], col="blue", type = "b",pch=16)
lines(10,medias[10], col="blue", type = "b",pch=16)
lines(12,medias[12], col="blue", type = "b",pch=16)
#lines(11,medias[11], col="blue", type = "b",pch=16)

lines(3,medias[3], col="green", type = "b",pch=17)
lines(13,medias[13], col="green", type = "b",pch=16)

a1=dat2$tempo.de.execuÃ.Ã.o..segundos[bp==n_u[1]]
a2=dat2$tempo.de.execuÃ.Ã.o..segundos[bp==n_u[2]]
a3=dat2$tempo.de.execuÃ.Ã.o..segundos[bp==n_u[3]]
a4=dat2$tempo.de.execuÃ.Ã.o..segundos[bp==n_u[4]]
a5=dat2$tempo.de.execuÃ.Ã.o..segundos[bp==n_u[5]]
a6=dat2$tempo.de.execuÃ.Ã.o..segundos[bp==n_u[6]]
a7=dat2$tempo.de.execuÃ.Ã.o..segundos[bp==n_u[7]]
a8=dat2$tempo.de.execuÃ.Ã.o..segundos[bp==n_u[8]]
a9=dat2$tempo.de.execuÃ.Ã.o..segundos[bp==n_u[9]]
a10=dat2$tempo.de.execuÃ.Ã.o..segundos[bp==n_u[10]]
a11=dat2$tempo.de.execuÃ.Ã.o..segundos[bp==n_u[11]]
a12=dat2$tempo.de.execuÃ.Ã.o..segundos[bp==n_u[12]]
a13=dat2$tempo.de.execuÃ.Ã.o..segundos[bp==n_u[13]]

wilcox.test(a1,a2,paried=T)
wilcox.test(a2,a3,paried=T)
wilcox.test(a3,a4,paried=T)
wilcox.test(a4,a5,paried=T)
wilcox.test(a5,a6,paried=T)
wilcox.test(a6,a7,paried=T)
wilcox.test(a7,a8,paried=T)
wilcox.test(a8,a9,paried=T)
wilcox.test(a9,a10,paried=T)
wilcox.test(a10,a11,paried=T)
wilcox.test(a11,a12,paried=T)
wilcox.test(a12,a13,paried=T)


wilcox.test(a1,a2,paried=T)
wilcox.test(a2,a3,paried=T)
wilcox.test(a3,a4,paried=T)
wilcox.test(a4,a5,paried=T)
wilcox.test(a5,a6,paried=T)
wilcox.test(a6,a7,paried=T)
wilcox.test(a7,a8,paried=T)
wilcox.test(a8,a9,paried=T)
wilcox.test(a9,a10,paried=T)
wilcox.test(a10,a11,paried=T)
wilcox.test(a11,a12,paried=T)
wilcox.test(a12,a13,paried=T)


################################################################################################
################################################################################################
################################################################################################
################################################################################################
####Metodologia
dat=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_mean.csv")
dat2=read.csv(file="C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN\\acertos_cent.csv")

wilcox.test(dat$tempo.de.execuÃ.Ã.o..segundos.,dat2$tempo.de.execuÃ.Ã.o..segundos., paired = T)
tempo2=cbind(dat$tempo.de.execuÃ.Ã.o..segundos.,dat2$tempo.de.execuÃ.Ã.o..segundos.)
tempo=c(dat$tempo.de.execuÃ.Ã.o..segundos.,dat2$tempo.de.execuÃ.Ã.o..segundos.)
metodologia=c(rep("Média", length(dat$tempo.de.execuÃ.Ã.o..segundos.)),
              rep("Central", length(dat$tempo.de.execuÃ.Ã.o..segundos.)))

modelo1=glm(metodologia~tempo,family=binomial(link="logit"))
summary(modelo1)

box=boxplot(tempo2, xlab="Metodologia", ylab="Tempo (segundos)", 
            names=c("Média dos pixels","Pixel central"),
            main="Metodologia vs. Tempo de Execução")



#Medias e desvios
medias=apply(tempo2,2,mean)
names(medias)=c("Média","Central")
medias
