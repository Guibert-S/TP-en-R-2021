library(tidyverse)
library(scales)
library(ggplot2)
library(corrplot)
library("na.tools")
library(lubridate)
library(stringr)
library(sqldf)
library(leaps)
library(modelr)
library(model4you)
library(RCurl)
# c'est inversé  homme et femme
#importation des donnees
homme = read.csv2("resultats_femmes.csv") # c'est inversé 
femme = read.csv2("resultats_hommes.csv")

# tri donne

homme$Var.7 =  NULL
femme$Var.7 =  NULL

# summary
summary(homme)

# nombre de NA 
colMeans(is.na(homme))
colMeans(is.na(femme))

# filtrer par epreuves 9 epreuves differentes
homme %>%  select(dis) %>% unique()

# selection 800m / marathon
homme_800m=filter(homme, dis=="middle-long/800-metres")
#homme_800m=filter(homme, dis=="road-running/marathon")

#homme_800m=filter(homme, dis=="middle-long/3000-metres")
femme_800m=filter(homme, dis=="middle-long/800-metres")




homme_800m$Mark=gsub("h","0",homme_800m$Mark) # remplacer les h par des 0

# 800m et 3000 m 


 homme_800m$resultat_sec=unlist(lapply(str_split(gsub("[.]",":",homme_800m$Mark),":"),function(x){
 as.numeric(x[1])*60+as.numeric(x[2])+as.numeric(x[3])/100}
 )) # calcul du temps en seconde

#marathon
#homme_800m$resultat_sec=unlist(lapply(str_split(gsub("[.]",":",homme_800m$Mark),":"),function(x){
#as.numeric(x[1])*3600+as.numeric(x[2])*60+as.numeric(x[3])}
#)) 

homme_800m$Mark
# un peu de data management pour les dates --> variables DOB (date of birth) et Date (date de la performance)
homme_800m$DOB=gsub("JAN",01,homme_800m$DOB)
homme_800m$DOB=gsub("FEB",02,homme_800m$DOB)
homme_800m$DOB=gsub("MAR",03,homme_800m$DOB)
homme_800m$DOB=gsub("APR",04,homme_800m$DOB)
homme_800m$DOB=gsub("MAY",05,homme_800m$DOB)
homme_800m$DOB=gsub("JUN",06,homme_800m$DOB)
homme_800m$DOB=gsub("JUL",07,homme_800m$DOB)
homme_800m$DOB=gsub("AUG",08,homme_800m$DOB)
homme_800m$DOB=gsub("SEP",09,homme_800m$DOB)
homme_800m$DOB=gsub("OCT",10,homme_800m$DOB)
homme_800m$DOB=gsub("NOV",11,homme_800m$DOB)
homme_800m$DOB=gsub("DEC",12,homme_800m$DOB)
homme_800m$DOB=gsub(" ","/",homme_800m$DOB)
homme_800m$DOB=as.Date(homme_800m$DOB,"%d/%m/%Y")

homme_800m$Date=gsub("JAN",01,homme_800m$Date)
homme_800m$Date=gsub("FEB",02,homme_800m$Date)
homme_800m$Date=gsub("MAR",03,homme_800m$Date)
homme_800m$Date=gsub("APR",04,homme_800m$Date)
homme_800m$Date=gsub("MAY",05,homme_800m$Date)
homme_800m$Date=gsub("JUN",06,homme_800m$Date)
homme_800m$Date=gsub("JUL",07,homme_800m$Date)
homme_800m$Date=gsub("AUG",08,homme_800m$Date)
homme_800m$Date=gsub("SEP",09,homme_800m$Date)
homme_800m$Date=gsub("OCT",10,homme_800m$Date)
homme_800m$Date=gsub("NOV",11,homme_800m$Date)
homme_800m$Date=gsub("DEC",12,homme_800m$Date)
homme_800m$Date=gsub(" ","/",homme_800m$Date)
homme_800m$Date=as.Date(homme_800m$Date,"%d/%m/%Y")

# calcul de l'age le jour de la performance
homme_800m$age_compet=as.numeric(round((homme_800m$Date-homme_800m$DOB)/365,2))
homme_800m$age_entier=as.numeric(round(homme_800m$age_compet,0)) #calcul de l'age entier

# Calcul de la vitesse en m/s

homme_800m$vitesse=(800/homme_800m$resultat_sec) # 800m 
#homme_800m$vitesse=(3000/homme_800m$resultat_sec) # 3000 m 
#homme_800m$vitesse=(42200/homme_800m$resultat_sec) # 42.2km = 42 200 m 
# retirer les na 
homme_800m = filter(homme_800m,!is.na(DOB))


#########################
# 1ère partie
##########################



#  on calcule la performance maximale par age
tapply(homme_800m$vitesse, homme_800m$age_entier,max)
aggregate(vitesse~age_entier,data=homme_800m,FUN= max)
homme_800max = homme_800m %>% group_by(age_entier) %>% summarise(max = max(vitesse))

# graphique 800m age + perf max
ggplot(homme_800max,aes(y=max,x=age_entier))+geom_point()+geom_smooth(se = FALSE)


# modele lineaire   age en abscisse 
mod_lm_800m <- lm( max~age_entier, homme_800max)
summary(mod_lm_800m)

# graphique 800m age + perf max modele lineaire
ggplot(homme_800max,aes(y=max,x=age_entier))+geom_point()+geom_smooth(se = FALSE,method = lm)+labs(x="Age ",y="Vitesse")

mod_poly_800m = lm( max~age_entier+I(age_entier^2), homme_800max)
summary(mod_poly_800m)

# modele polynomiale degre 2 
ggplot(homme_800max,aes(y=max,x=age_entier))+geom_point()+geom_smooth(method="lm",formula = y~poly(x,2))+labs(x="Age ",y="Vitesse")

# modele polynomiale degre 4
ggplot(homme_800max,aes(y=max,x=age_entier))+geom_point()+geom_smooth(method="lm",formula = y~poly(x,4))+geom_smooth(method="lm",formula = y~poly(x,2),color='red')+labs(x="Age ",y="Vitesse")
mod_poly_4_800m = lm( max~age_entier+I(age_entier^3)+I(age_entier^2)+I(age_entier^4), homme_800max)
summary(mod_poly_4_800m)

# modele polynomiale degre 5
mod_poly_5_800m = lm( max~age_entier+I(age_entier^3)+I(age_entier^2)+I(age_entier^4)+I(age_entier^5), homme_800max)

# modele polynomiale degre 6
mod_poly_6_800m = lm( max~age_entier+I(age_entier^3)+I(age_entier^2)+I(age_entier^4)+I(age_entier^5)+I(age_entier^6), homme_800max)


# modele Roger Moore 
MMC=function(age,vitesse,methode,nbpara,precision=10,borne=-Inf, initial=runif(nbpara)){
  
  SQ_methode=function(p){ #Cette fonction permet de calculer la somme carre des residus            #theta sont les parametre qu 
    sum((vitesse-methode(age,p))^2) }
  
  o=rep(10^6,precision) #o permet de stocker la fonction objectif qu'on voudra le plus bas
  r=rep(0,precision) #r permet de sotcker le R?
  para=rep(10^6,precision*(nbpara+1)) #pour stocker tout les parametres
  for (i in seq(nbpara+1,precision*(nbpara+1),nbpara+1)){ 
    sol=nlminb(start=initial + rnorm(nbpara,0,sqrt(abs(initial))),SQ_methode) 
    res= nlminb(start = sol$par,SQ_methode,lower=borne) 
    para[i:(i+nbpara-1)]=res$par #ici"
    o[i/(nbpara+1)]=res$objective}
  compt=which.min(o) 
  parametre=rep(0,nbpara)
  parametre=para[((nbpara+1)*compt):(((nbpara+1)*compt)+(nbpara-1))] #on stock les meilleurs parametres
  fit=methode(age,parametre) #regression 
  R2=sum((fit-mean(vitesse))^2)/sum((vitesse-mean(vitesse))^2) #R? 
  peak=age[methode(sort(age),parametre)==max(methode(sort(age),parametre))]
  return(list(parametre=parametre,objectif=min(o),R2=R2,peak=peak))}
f=function(x,p){
  p[1]*(1-exp(p[2]*x))+p[3]*(1-exp(p[4]*x))
}

modelmoore=MMC(homme_800max$age_entier,homme_800max$max,f,4,precision=5000)
summary(modelmoore)
# graphique Moore
plot(homme_800max$age_entier,homme_800max$max, pch=16, col="black",xlab="Age",ylab = "Vitesse")
predictions<-f(homme_800max$age_entier,modelmoore$parametre) # calcul des valeurs predites par le modele
lines(homme_800max$age_entier,predictions, col="red", lwd=2)

# Tous les modèles
ggplot(homme_800max,aes(y=max,x=age_entier))+geom_point(size=3)+geom_smooth(method="lm",formula = y~poly(x,4),se = FALSE)+geom_smooth(method="lm",formula = y~poly(x,2),color='red',se = FALSE)+labs(x="Age ",y="Vitesse")+geom_line(aes(y=predictions),colour="black",size  =1)+geom_smooth(method="lm",formula = y~x,se=FALSE,colour="orange")+theme_bw()+
geom_smooth(method="lm",formula = y~poly(x,5),color='pink',se = FALSE)+geom_smooth(method="lm",formula = y~poly(x,6),color='green',se = FALSE)
# 2 meilleurs modèles 
ggplot(homme_800max,aes(y=max,x=age_entier))+geom_point(size=2)+geom_line(aes(y=predictions),colour="black",size  =1)+geom_smooth(method="lm",formula = y~poly(x,6),se = FALSE,color='green',size=1)+theme_light()+labs(x="Age ",y="Vitesse")+geom_smooth(method="lm",formula = y~x,se=FALSE,colour="orange")+geom_smooth(method="lm",formula = y~poly(x,2),color='red',se = FALSE)

# modele IMAP


#Etude  des résidus
par(mfrow=c(2,2))
plot(mod_lm_800m)
plot(mod_poly_800m)
plot(mod_poly_4_800m)
plot(mod_poly_5_800m)
plot(mod_poly_6_800m)

qqnorm(mod_lm_800m$residuals)
qqline(mod_lm_800m$residuals)

qqnorm(mod_poly_800m$residuals)
qqline(mod_poly_800m$residuals)

qqnorm(mod_poly_4_800m$residuals)
qqline(mod_poly_4_800m$residuals)

qqnorm(mod_poly_5_800m$residuals)
qqline(mod_poly_5_800m$residuals)

#qqnorm(mod_poly_5_800m$residuals)
#qqline(mod_poly_5_800m$residuals)


par(mfrow=c(1,1))
# Interprétation des graphes à faire

# AIC / BIC ... 
bic = function(modele,n,k){
  return(n*log(rss(modele)/n)+k*log(n))
}
aic = function(modele,n,k){
  return(n*log(rss(modele)/n) +2*k+((2*k*(k+1))/(n-k-1)))
}


# Fonction qui créer un tableau 

critere = function(x,y,z,p,q){
  n=31 # R2 ajuste Moore n= 31 et k = 4
  k=4
  R2_ajuste_moore =1-( ((1-modelmoore$R2)*(n-1))/(n-k-1))
  RSS_Moore = sum((homme_800max$max-predictions)**2)
  AIC_Moore = n*log(RSS_Moore/n) +2*k+((2*k*(k+1))/(n-k-1))
  BIC_Moore = n*log(RSS_Moore/n) +k*log(n)
  a=aic(x,n=31,k=2)
  b=bic(x,n=31,k=2)
  c=summary(x)$adj.r.squared
  d=rss(x)
  a1=aic(y,n=31,k=3)
  b1=bic(y,n=31,k=3)
  c1=summary(y)$adj.r.squared
  d1=rss(y)
  a2=aic(z,n=31,k=5)
  b2=bic(z,n=31,k=5)
  c2=summary(z)$adj.r.squared
  d2=rss(z)
  a3=aic(p,n=31,k=6)
  b3=bic(p,n=31,k=6)
  c3=summary(p)$adj.r.squared
  d3=rss(p)
  a4=aic(q,n=31,k=7)
  b4=bic(q,n=31,k=7)
  c4=summary(q)$adj.r.squared
  d4=rss(q)
  res = cbind(a,b,c,d)
  res2 = cbind(a1,b1,c1,d1)
  res3=cbind(a2,b2,c2,d2)
  res_moore = cbind(AIC_Moore,BIC_Moore,R2_ajuste_moore,RSS_Moore)
  res5= cbind(a3,b3,c3,d3)
  res6= cbind(a4,b4,c4,d4)
  res = rbind(res,res2,res3,res5,res6,res_moore)
  colnames(res)= c("AIC","BIC","R ajusté","RSS")
  rownames(res) = c("linéaire","polynomiale 2","polynomiale 4","polynomiale 5","polynomiale 6","Moore")
  return(res)
}

# a la main  calcul du RSS
rss = sum((homme_800max$max-mod_lm_800m$fitted.values)**2)

# fusion tableau  récapitulatif
homme_800m_critere=critere(mod_lm_800m,mod_poly_800m,mod_poly_4_800m,mod_poly_5_800m,mod_poly_6_800m)
homme_800m_critere = round(homme_800m_critere,3)
homme_800m_critere
#write.csv(homme_800m_critere,file="tableau_800m")


# Graphique de distributions 
ggplot(homme_800m, aes(x=age_compet)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + labs(title="Distribution de l'âge au 800m féminin ",y="Densité ",x="Age")

ggplot(homme_800m, aes(x=vitesse)) + 
  geom_histogram(aes(y=..count../sum(..count..)), colour="black", fill="#8AA9E2")+ labs(title="Distribution de la vitesse au marathon féminin ",y="Densité ",x="Vitesse")+theme_bw()
  


View(homme_800m_critere)


View(homme_800max)

filter(homme_800m, vitesse > 7) %>% select(Competitor)

