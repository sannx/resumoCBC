setwd("C:\\Users\\ana.andrade\\Documents\\Ana\\SAVIO\\SAN")
dir()
dat_mean=read.table("bfast_output_mean.txt")
dat_cent=read.table("bfast_output_pix_cent.txt")
names(dat_cent)=c("algoritmo" ,"h","season","identificador.do.pixel" ,     
                  "tempo.de.execuÃ.Ã.o..segundos.", "last.breakpoint","slope")
names(dat_mean)=c("algoritmo" ,"h","season","identificador.do.pixel" ,     
                  "tempo.de.execuÃ.Ã.o..segundos.", "last.breakpoint","slope")




##################################
##################################
#####Associaçoes para dat_mean####
##################################
##################################


dat2=dat_mean
#Função para remover outliers
remove_outliers <- function(x, na.rm = TRUE) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


##### Hipoteses

# Há associação tempo-algoritmo?
# 4 amostras pareadas
#Friedman test

#
algoritmos=as.character(unique(dat2$algoritmo))
algoritmos_rep=c(rep(algoritmos[1], 18090),rep(algoritmos[2], 18090),rep(algoritmos[3], 18090),rep(algoritmos[4], 18090))
#organizando tempos
tempo_luis_w=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$algoritmo[] =="bfast_luis_wavelet_java.r"]
tempo_luis=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$algoritmo[] =="bfast_luis_java.r"]
tempo_new=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$algoritmo[] =="new_bfast_java.r"]
tempo_new_w=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$algoritmo[] =="new_bfast_wavelet_java.r"]
tab_tempos_algoritmo=cbind(tempo_luis_w,tempo_luis,tempo_new,tempo_new_w)


#conferindo
nrow(dat2)==length(tempo_luis)+length(tempo_new_w)+length(tempo_luis_w)+length(tempo_new)
ncol(tab_tempos_algoritmo)
nrow(tab_tempos_algoritmo)

#boxplot
box=boxplot(tab_tempos_algoritmo, xlab="Algoritmos", ylab="Tempo (segundos)", 
            names=c("luis_wavelet_java.r", "luis_java.r" ,       
                    "new_java.r",          "new_wavelet_java.r"),
            main="Algoritmo vs. Tempo de Execução (média dos pixels)")







#densidades
plot(density(tab_tempos_algoritmo[,1]), main="Densidades da variável tempo de execução, por algoritmo (média dos pixels)")
lines(density(tab_tempos_algoritmo[,2]), col="red")
lines(density(tab_tempos_algoritmo[,3]), col="blue")
lines(density(tab_tempos_algoritmo[,4]), col="green")

legend(2.5,1.7,c("luis_wavelet_java.r", "luis_java.r" ,       
                 "new_java.r",          "new_wavelet_java.r"), col =c("black","red","blue","green"),
       pch=c("-","-","-","-"))


#retirar pontos sem break
dat21=dat2[dat2$last.breakpoint[]!=2000049]
tempo_luis_w=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$algoritmo[] =="bfast_luis_wavelet_java.r" & dat2$last.breakpoint[]!=2000049]
tempo_luis=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$algoritmo[] =="bfast_luis_java.r"&dat2$last.breakpoint[]!=2000049]
tempo_new=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$algoritmo[] =="new_bfast_java.r"&dat2$last.breakpoint[]!=2000049]
tempo_new_w=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$algoritmo[] =="new_bfast_wavelet_java.r"&dat2$last.breakpoint[]!=2000049]
tab_tempos_algoritmo=cbind(tempo_luis_w,tempo_luis,tempo_new,tempo_new_w)


#boxplot
box=boxplot(tab_tempos_algoritmo, xlab="Algoritmos", ylab="Tempo (segundos)", 
            names=c("luis_wavelet_java.r", "luis_java.r" ,       
                    "new_java.r",          "new_wavelet_java.r"),
            main="Algoritmo vs. Tempo de Execução (média dos pixels)
Considerando apenas casos com presença de breakpoint")







#densidades
plot(density(tab_tempos_algoritmo[,1]), main="Densidades da variável tempo de execução, por algoritmo (média dos pixels)
     Considerando apenas casos com presença de breakpoint")
lines(density(tab_tempos_algoritmo[,2]), col="red")
lines(density(tab_tempos_algoritmo[,3]), col="blue")
lines(density(tab_tempos_algoritmo[,4]), col="green")

legend(3.0,1.45,c("luis_wavelet_java.r", "luis_java.r" ,       
             "new_java.r",          "new_wavelet_java.r"), col =c("black","red","blue","green"),
       pch=c("-","-","-","-"))

# dados_sem_out_1=remove_outliers(tab_tempos_algoritmo[,1])
# dados_sem_out_2=remove_outliers(tab_tempos_algoritmo[,2])
# dados_sem_out_3=remove_outliers(tab_tempos_algoritmo[,3])
# dados_sem_out_4=remove_outliers(tab_tempos_algoritmo[,4])
# dados_sem_out=cbind(dados_sem_out_1,dados_sem_out_2,dados_sem_out_3,dados_sem_out_4)
# box=boxplot(dados_sem_out, xlab="Algoritmos", ylab="slopepoints", 
#             names=c("luis_wavelet_java.r", "luis_java.r" ,       
#                     "new_java.r",          "new_wavelet_java.r"),
#             main="Algoritmo vs. Tempo de Execução (média dos pixels)")


#friedman
#H0: não há diferença significativa entre as medidas dos algoritmos
friedman.test(tab_tempos_algoritmo)
friedman.test(dados_sem_out)

#Medias e desvios
medias=apply(tab_tempos_algoritmo,2,mean)
desvios=apply(tab_tempos_algoritmo,2,sd)
medias
desvios

# Há associação tempo-season?
# 4 amostras pareadas
#Friedman test

tempo_h=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$season[] =="harmonic"]
tempo_d=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$season[] =="dummy"]
tempo_n=dat2$tempo.de.execuÃ.Ã.o..segundos.[dat2$season[] =="none"]
tab_tempos_season=cbind(tempo_h,tempo_d,tempo_n)


#conferindo
nrow(dat2)==length(tempo_h)+length(tempo_n)+length(tempo_d)
ncol(tab_tempos_season)
nrow(tab_tempos_season)


#boxplot
box=boxplot(tab_tempos_season, xlab="Season", ylab="Tempo (segundos)", 
            names=c("harmonic", "dummy",    "none"),
            main="Season vs. Tempo de Execução (média dos pixels)")

plot(density(tab_tempos_season[,1]), main="Densidades da variável tempo de execução, por season (média dos pixels)")
lines(density(tab_tempos_season[,2]), col="red")
lines(density(tab_tempos_season[,3]), col="blue")


legend(23,0.5,c("harmonic", "dummy",    "none"), 
       col =c("black","red","blue"),
       pch=c("-","-","-"))



dados_sem_out_1=remove_outliers(tab_tempos_season[,1])
dados_sem_out_2=remove_outliers(tab_tempos_season[,2])
dados_sem_out_3=remove_outliers(tab_tempos_season[,3])

dados_sem_out=cbind(dados_sem_out_1,dados_sem_out_2,dados_sem_out_3)
box=boxplot(dados_sem_out, na.rm=T, xlab="Season", ylab="Tempo (segundos)", 
            names=c("harmonic", "dummy",    "none"),
            main="Season vs. Tempo de Execução (média dos pixels)")


#friedman
#H0: não há diferença significativa entre as medidas dos algoritmos
friedman.test(tab_tempos_season)
friedman.test(dados_sem_out)

#Medias e desvios
medias=apply(tab_tempos_season,2,mean)
desvios=apply(tab_tempos_season,2,sd)
medias
desvios



# Há associação tempo-h?
# 4 amostras pareadas
#Friedman test

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


#conferindo
nrow(dat2)==length(tempo_h1)+length(tempo_h2)+length(tempo_h3)+
  length(tempo_h4)+length(tempo_h5)+length(tempo_h6)+length(tempo_h7)+
  length(tempo_h8)+length(tempo_h9)+length(tempo_h10)
ncol(tab_tempos_h)
nrow(tab_tempos_h)


#boxplot
box=boxplot(tab_tempos_h, xlab="h", ylab="Tempo (segundos)", 
            names=seq(6,60,by=6),
            main="h vs. Tempo de Execução (média dos pixels)")

#densidade
a <- c(1:10)
# Degradê de vermelho para amarelo
coresBasicas <- c(1:8)
barplot(coresBasicas, col=1)

plot(density(tab_tempos_h[,1]), main="Densidades
     da variável tempo de execução, por h (média dos pixels)", 
     col="black", ylim=c(0,0.8))
for(i in 2:10){
lines(density(tab_tempos_h[,i]), col=i)
}

# legend(23,0.5,c("harmonic", "dummy",    "none"), 
#        col =c("black","red","blue"),
#        pch=c("-","-","-"))


#friedman
#H0: não há diferença significativa entre as medidas dos algoritmos
friedman.test(tab_tempos_h)

#Medias e desvios
medias=apply(tab_tempos_h,2,mean)
desvios=apply(tab_tempos_h,2,sd)
medias
desvios

### Algoritmo x Breakpoints
# 4 amostras pareadas
#Friedman test
break_luis_w=dat2$last.breakpoint[dat2$algoritmo[] =="bfast_luis_wavelet_java.r"]
break_luis=dat2$last.breakpoint[dat2$algoritmo[] =="bfast_luis_java.r"]
break_new=dat2$last.breakpoint[dat2$algoritmo[] =="new_bfast_java.r"]
break_new_w=dat2$last.breakpoint[dat2$algoritmo[] =="new_bfast_wavelet_java.r"]
tab_break_algoritmo=cbind(break_luis_w,break_luis,break_new,break_new_w)

#conferindo
nrow(dat2)==length(break_luis)+length(break_new_w)+length(break_luis_w)+length(break_new)
ncol(tab_break_algoritmo)
nrow(tab_break_algoritmo)


#boxplot
box=boxplot(tab_break_algoritmo, xlab="Algoritmos", ylab="Breakpoints", 
            names=c("luis_wavelet_java.r", "luis_java.r" ,       
                    "new_java.r",          "new_wavelet_java.r"),
            main="Algoritmo vs. Last Breakpoint (média dos pixels)")


#densidades
plot(density(tab_break_algoritmo[,1]), 
     main="Densidades da variável last breakpoint,
     por algoritmo (média dos pixels)", ylim=c(0,0.01))
lines(density(tab_break_algoritmo[,2]), col="red")
lines(density(tab_break_algoritmo[,3]), col="blue")
lines(density(tab_break_algoritmo[,4]), col="green")

legend(2.5,1.7,c("luis_wavelet_java.r", "luis_java.r" ,       
                 "new_java.r",          "new_wavelet_java.r"), col =c("black","red","blue","green"),
       pch=c("-","-","-","-"))


#friedman
#H0: não há diferença significativa entre as medidas dos algoritmos
friedman.test(tab_break_algoritmo)

#Medias e desvios
medias=apply(tab_break_algoritmo,2,mean)
desvios=apply(tab_break_algoritmo,2,sd)
medias
desvios





### Algoritmo x Slope
# 4 amostras pareadas
#Friedman test
slope_luis_w=dat2$slope[dat2$algoritmo[] =="bfast_luis_wavelet_java.r"]
slope_luis=dat2$slope[dat2$algoritmo[] =="bfast_luis_java.r"]
slope_new=dat2$slope[dat2$algoritmo[] =="new_bfast_java.r"]
slope_new_w=dat2$slope[dat2$algoritmo[] =="new_bfast_wavelet_java.r"]
tab_slope_algoritmo=cbind(slope_luis_w,slope_luis,slope_new,slope_new_w)

#conferindo
nrow(dat2)==length(slope_luis)+length(slope_new_w)+length(slope_luis_w)+length(slope_new)
ncol(tab_slope_algoritmo)
nrow(tab_slope_algoritmo)


#boxplot
box=boxplot(tab_slope_algoritmo, xlab="Algoritmos", ylab="slopepoints", 
            names=c("luis_wavelet_java.r", "luis_java.r" ,       
                    "new_java.r",          "new_wavelet_java.r"),
            main="Algoritmo vs. Slope (média dos pixels)")


dados_sem_out=remove_outliers(tab_slope_algoritmo)
box=boxplot(dados_sem_out, xlab="Algoritmos", ylab="slopepoints", 
            names=c("luis_wavelet_java.r", "luis_java.r" ,       
                    "new_java.r",          "new_wavelet_java.r"),
            main="Algoritmo vs. Slope (média dos pixels)")

#friedman
#H0: não há diferença significativa entre as medidas dos algoritmos
friedman.test(tab_slope_algoritmo)


#Medias e desvios
medias=apply(tab_slope_algoritmo,2,mean)
desvios=apply(tab_slope_algoritmo,2,sd)
medias
desvios



##################################
##################################
##Associaçoes entre metodologias##
##################################
##################################

#tempos

tempos=cbind(dat_cent$tempo.de.execuÃ.Ã.o..segundos.,dat_mean$tempo.de.execuÃ.Ã.o..segundos.)
length(tempos)

box=boxplot(tempos, xlab="Metodologia", ylab="tempo", 
            names=c("Pixel central", "Média dos pixels"),
            main="Metodologia vs. Tempo de Execução")


plot(density(tempos[,1]), main="Densidades da variável tempo de execução, por metodologia")
lines(density(tempos[,2]), col="red")

legend(23,0.6,c("Pixel central", "Média dos pixels"), 
       col =c("black","red"),
       pch=c("-","-"))


#friedman
#H0: não há diferença significativa entre as medidas dos algoritmos
friedman.test(tempos)


#Medias e desvios
medias=apply(tempos,2,mean)
desvios=apply(tempos,2,sd)
medias
desvios


