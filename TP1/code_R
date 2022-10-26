library(tidyverse)
library(scales)
library(ggplot2)
library(corrplot)
library("na.tools")
library(Factoshiny)
library(FactoMineR)
library(FactoInvestigate)
library(missMDA)
wakecounty <- read.csv2("E:/Ecole/2A/methode_stat/wakecounty.csv")
wakecounty = read.csv2("wakecounty.csv")
head(wakecounty)

summary(wakecounty)
x=cor(wakecounty)
x

# on remplace la valeur manquante par la mÃ©diane car la variable reprÃ©sente le nombre de salle de bain est discrÃ¨te. 
#Prendre la mÃ©diane est une meilleur option que de supprimer la ligne car le jeu de donnÃ©es comporte seulement 100 observations.

wakecounty[1,6] = median(wakecounty$NoBaths)

#  enlever les colonnes
wakecounty = wakecounty[,-1,-11]
wakecounty$ID = NULL
wakecounty$Zip= NULL
wakecounty$Land= NULL


plot(wakecounty)



# Regrouper la valeur Total
wakecounty = wakecounty %>% mutate(Prix = Total)

# repartition en classe Total
wakecounty$Prix = case_when(
  wakecounty$Prix < 100000 ~ " moins de 100 000",
  wakecounty$Prix< 150000 ~ "100 000-150 000 ",
  wakecounty$Prix< 250000 ~ "150 000-250 000 ",
  wakecounty$Prix< 300000 ~ "250 000-300 000",
  wakecounty$Prix< 400000 ~ "300 000-400 000",
  TRUE ~ "400 000 et plus" )

wakecounty = wakecounty %>% mutate("Demi_hectare" = Acres)
# repartition en classe Acres (nombre de demi hectare)
wakecounty$Demi_hectare = case_when(
  wakecounty$Demi_hectare < 0.20 ~ " moins de 0.2 ",
  wakecounty$Demi_hectare< 0.40 ~ "entre 0.2 et 0.4 ",
  wakecounty$Demi_hectare< 0.60 ~ "entre 0.4 et 0.6 ",
  wakecounty$Demi_hectare< 0.80 ~ "entre 0.6 et 0.8",
  TRUE ~ "0.8 et plus" )


wakecounty = wakecounty %>% mutate("Anne_de_construction" = BuildYear )
wakecounty$Anne_de_construction = case_when(
  wakecounty$Anne_de_construction < 0.20 ~ " moins de 0.2 ",
  wakecounty$Anne_de_construction< 0.40 ~ "entre 0.2 et 0.4 ",
  wakecounty$Anne_de_construction< 0.60 ~ "entre 0.4 et 0.6 ",
  wakecounty$Anne_de_construction< 0.80 ~ "entre 0.6 et 0.8",
  TRUE ~ "0.8 et plus" )



wakecounty$Acres
wakecounty$Story = as.factor(wakecounty$Story)

prop.table(table(wakecounty$Prix))

# supprimer les valeurs extremes pour plus de visibilité
wakecounty2 =  filter(wakecounty,Total<550000)
wakecounty2[1,6] = median(wakecounty$NoBaths)

#######################################################################################################
                          GRAPHIQUES
########################################################################################################

# Univarié
ggplot(wakecounty2,aes(x=Total))+geom_histogram(fill="white",color='black',bins = 30)+ geom_vline(aes(xintercept=mean(Total)),color="blue", linetype="dashed", size=1)+labs(ylab="Nombre",xlab="Prix de la maison")
ggplot(wakecounty2,aes(x=Total))+geom_boxplot()+coord_flip()
ggplot(wakecounty,aes(x=Prix))+geom_bar(fill="darkgrey",color='black')+coord_flip()
#J'ai retiré les 2 plus grande valeurs pour pouvoir observer la repartition de plus près



# BivariÃ©

ggplot(wakecounty,aes(x=Total,y= YearBuilt))+geom_point(aes(color=wakecounty$Prix),size=2)+labs(color = "Prix des maisons",x="Prix des maisons",y="Année de construction",title = "Prix des maisons en fonction des années de construction")+geom_smooth()
ggplot(wakecounty2,aes(x=Total,y= YearBuilt))+geom_point(aes(color=wakecounty2$Prix),size=2)+labs(color = "Prix des maisons",x="Prix des maisons",y="Année de construction",title = "Prix des maisons en fonction des années de construction")+geom_smooth()


ggplot(wakecounty,aes(x=Total,y= SqFt))+geom_point(color="darkblue")+labs(color = "Prix des maisons",x="Prix des maisons",y="Pieds carré",title = "Prix des maisons en fonction de la surface d'habitation")
ggplot(wakecounty2,aes(x=Total,y= SqFt))+geom_point(color="darkblue")+geom_smooth()+labs(color = "Prix des maisons",x="Prix des maisons",y="Pieds carré",title = "Prix des maisons en fonction de la surface d'habitation")
ggplot(wakecounty2,aes(x=Prix,y= SqFt))+geom_boxplot(fill="white",color="#d1bd83")+coord_flip()+labs(color = "Prix des maisons",x="Prix des maisons",y="Pieds carré",title = "Prix des maisons en fonction de la surface d'habitation")



ggplot(wakecounty,aes(x=Total,y=as.factor(Story)))+geom_bar(stat="identity")+labs(y="Nombre d'étages de l'habitation",x="Prix des maisons",title ="Prix des maisons et Nombre d'étages") 
ggplot(wakecounty,aes(x=Total,y=as.factor(Story)))+geom_boxplot(fill="darkorange",color="black") + coord_flip()+labs(y="Nombre d'étages de l'habitation",x="Prix des maisons",title ="Prix des maisons et Nombre d'étages")
ggplot(wakecounty2,aes(x=Total,y=as.factor(Story)))+geom_boxplot(fill="darkorange",color="black") + coord_flip()+labs(y="Nombre d'étages de l'habitation",x="Prix des maisons",title ="Prix des maisons et Nombre d'étages")
ggplot(wakecounty2,aes(x=Total,y=as.factor(Story)))+geom_violin(fill="darkorange",color="black") + coord_flip()+labs(y="Nombre d'étages de l'habitation",x="Prix des maisons",title ="Prix des maisons et Nombre d'étages")

  


ggplot(wakecounty2,aes(y=Total,x= Acres))+geom_point()+geom_smooth() # corrige regrouper
ggplot(wakecounty2,aes(x=Prix,y= Acres))+geom_boxplot()+ coord_flip()

# NA a enlever
ggplot(wakecounty2,aes(y=Total,x=as.factor(wakecounty2$NoBaths)))+geom_boxplot(fill="gold",color="black")+theme_classic()+labs(title = "Prix des maisons en fonction du nombre de salle de bain",y="Prix de l'habitation",x="Nombre de salle de bain")
ggplot(wakecounty,aes(y=Total,x=as.factor(wakecounty$NoBaths)))+geom_boxplot(fill="gold",color="black")+theme_classic()+labs(title = "Prix des maisons en fonction du nombre de salle de bain",y="Prix de l'habitation",x="Nombre de salle de bain")

# NA enelever
ggplot(wakecounty,aes(y=Total,x=as.factor(wakecounty$Fireplaces)))+geom_boxplot(fill="orange",color="black")+theme_classic()+labs(title = "Prix des maisons en fonction du nombre de cheminée",y="Prix de l'habitation",x="Nombre de cheminée")
ggplot(wakecounty2,aes(y=Total,x=as.factor(wakecounty2$Fireplaces)))+geom_boxplot(fill="orange",color="black")+theme_classic()+labs(title = "Prix des maisons en fonction du nombre de cheminée",y="Prix de l'habitation",x="Nombre de cheminée")



ggplot(wakecounty,aes(x=Total,y= Building))+geom_point(aes(color=wakecounty$Prix),size=2)+labs(color = "Prix des maisons",x="Prix des maisons",y="Valeur du bâtiment de l'habitation en dollars",title = "Prix des maisons en fonction de la valeur du bâtiment")
ggplot(wakecounty2,aes(x=Total,y= Building))+geom_point(aes(color=wakecounty2$Prix),size=2)+labs(color = "Prix des maisons",x="Prix des maisons",y="Valeur du bâtiment de l'habitation en dollars",title = "Prix des maisons en fonction de la valeur du bâtiment")



wakecounty %>% group_by(NoBaths) %>% summarise(n=n())
wakecounty %>% group_by(Fireplaces) %>% summarise(n=n())


# Modele lineaire
wakecounty3 = read.csv2("wakecounty.csv")
wakecounty3[1,6] = median(wakecounty3$NoBaths)
wakecounty3$ID = NULL
wakecounty3$Zip= NULL
wakecounty3$Land= NULL

corrplot(cor(wakecounty3))

summary(wakecounty3$Zip)

wake_avant_1975 = wakecounty2 %>% filter(wakecounty2$YearBuilt < 1975)
wake_avant_1982 = wakecounty2 %>% filter(wakecounty2$YearBuilt < 1982)
wake_apres_1982 = wakecounty2 %>% filter(wakecounty2$YearBuilt >= 1982)

cor(wake_avant_1975$YearBuilt,wake_avant_1975$Total)
cor(wake_avant_1982$YearBuilt,wake_avant_1982$Total)
cor(wake_apres_1982$YearBuilt,wake_apres_1982$Total)

ggplot(wake_apres_1982,aes(x=Total,y= YearBuilt))+geom_point(aes(color=wake_apres_1982$Prix),size=2)+labs(color = "Prix des maisons",x="Prix des maisons",y="Année de construction",title = "Prix des maisons en fonction des années de construction")+geom_smooth()

prop.table(table(wake_apres_1982$YearBuilt,wake_apres_1982$Prix))



"""
ancienne maison / prix   ?
  maison plus vielle --> + grande ou plus petite / maison récente grande ?
  pourquoi maison 5 M ?
  pourquoi -100 000  ?
  
  
  modele pour estimer le prix de la maison (maison moderne / maison récente) (zip modele)

zip ? neuily ou 93 ?
  
  enlevezr NA nobath
  """


res = PCA(wakecounty2, graph=TRUE)
res

round(prop.table(table(wakecounty$Prix,wakecounty$Demi_hectare),margin = 1),2)

cor(wakecounty$Total,wakecounty$Acres)
cor(wakecounty2$Total,wakecounty2$Acres)

cor(wake_avant_1982$Total,wake_avant_1982$Acres)
cor(wake_apres_1982$Total,wake_apres_1982$Acres)


cor(wakecounty$Total,wakecounty$SqFt)
cor(wakecounty2$Total,wakecounty2$SqFt)

cor(wake_avant_1982$Total,wake_avant_1982$SqFt)
cor(wake_apres_1982$Total,wake_apres_1982$SqFt)

cor(wakecounty2$Total,wakecounty2$Building)
cor(wake_avant_1982$Total,wake_avant_1982$Building)
cor(wake_apres_1982$Total,wake_apres_1982$Building)
