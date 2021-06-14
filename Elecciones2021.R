library(funModeling)
library(sqldf)

rm(list = ls()) 


setwd("G:/Mis Documentos/")

df_ele <- read.csv("result_resumen_por_mesa_v2.txt", sep=",")




df_ele[df_ele$votos_pl==0,'votos_fp']

aux <- df_ele[is.na(df_ele$votos_pl),]


agrup_dist <- sqldf("select ubigeo, departamento, provincia, distrito, sum(votos_fp) votos_fp, 
sum(votos_pl) votos_pl , sum(total_votantes) votantes, count(1) mesas, 
avg(votos_pl) media_pl, 
avg(votos_fp) media_fp,
STDEV(votos_pl) desv_pl, 
STDEV(votos_fp) desv_fp, 
sum(case when votos_fp=0 then 1 else  0 end) votos_fp_0,
sum(case when votos_pl=0 then 1 else  0 end) votos_pl_0
from 
                    df_ele group by ubigeo,  departamento, provincia, distrito") 


agrup_dist$p_pl <- agrup_dist$votos_pl/agrup_dist$votantes 
agrup_dist$p_fp <- agrup_dist$votos_fp/agrup_dist$votantes 


agrup_dist <- agrup_dist[!is.na(agrup_dist$p_pl),]

## summary(agrup_dist)

dist_top <- agrup_dist[agrup_dist$p_pl>0.9 & agrup_dist$votos_fp_0>0 & agrup_dist$votos_pl_0==0,]



n <- nrow(dist_top)
for (i in 1:n)
{
  
  ubigeo <- dist_top[i,'ubigeo']
  X <- df_ele[df_ele$ubigeo==ubigeo,'votos_fp']
  X <- X[!is.na(X)]
  media <- agrup_dist[agrup_dist$ubigeo==ubigeo,'media_fp']
  desv <- agrup_dist[agrup_dist$ubigeo==ubigeo,'desv_fp']
  distrito <- agrup_dist[agrup_dist$ubigeo==ubigeo,'departamento']
  distrito <- paste0(distrito,"-",agrup_dist[agrup_dist$ubigeo==ubigeo,'provincia'])
  distrito <- paste0(distrito,"-",agrup_dist[agrup_dist$ubigeo==ubigeo,'distrito'])
  
  ## jpeg('rplot.jpg')
  control.x(X, media,desv, distrito, 0)
  
  
  dev.copy(png,paste0(distrito,".png")) 
  # dev.print(width=600,height=335)
  # dev.off(dev.prev())
  dev.off()
} 


############ genera grafico de control  

control.x<-function(x,mu,sigma, distrito,FP=0) 
{ 
  color<-c("#ffd05c",'#ffbc17')
  texto_tit <- "VOTOS FUERZA POPULAR" 
  if (FP==1) 
  {
    color <- c("#fe7b4a",'#fe4b09')
    texto_tit <- "VOTOS PERU LIBRE"
  }
  LCL<-round(mu-3*sigma,1)
  UCL<-round(mu+3*sigma ,1)
  if(LCL<0) LCL<-0
  tiempo<-1:length(x) 
  plot(tiempo,x,type='l',xaxt='n',xlim=c(0,(length(x)+2)),ylim=c(min(LCL,min(x))-2, 
                                                                 max(UCL,max(x))+2),
       col=color[1],
       xlab="Mesa",
       ylab="Votos")  
  axis(1,1:(length(x)+2),cex.axis=0.8,las=2) 
  abline(h=mu,lty=3)## para la media 
  text((max(tiempo)+1),mu,paste('prom=',round(mu,1)),pos=3,font=2,cex=0.8) 
  
  abline(h=UCL,lty=3) ## limite superior 
  
  text((max(tiempo)+1),UCL,paste('LS=',UCL),pos=3,font=2,cex=0.8) 
  
  abline(h=LCL,lty=3) ### limite inferior 
  
  text((max(tiempo)+1),LCL,paste('LI=',LCL),pos=3,font=2,cex=0.8) 
  
  for(i in 1:length(x)) ## pone puntos a los que estan dentro de las bandas 
    
  {  ### pone x a los que estan fuera de las bandas 
    
    if(x[i]>UCL) temp<-4 
    
    else if(x[i]<LCL) temp<-4 
    
    else temp<-19 
    
    points(tiempo[i],x[i],pch=temp, col=color[2]) 
    
  } 
  
  mtext(paste0(texto_tit," DISTRITO: ",distrito),side=3,font=4) 
} 

