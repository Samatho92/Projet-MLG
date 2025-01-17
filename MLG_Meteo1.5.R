
#Projet MLG Meteo Bale
#Samir Aidoudi
#Version 1.5
#08/01/2022

library(FactoMineR) #ACP
library(corrplot) #Matrice de Corr�lation
library(tidyverse) #Manipulation de Tableau
library(gdata) #Manipulation de Donn�es
library(dplyr) #Manipulation de Donn�es
library(leaps) #Regression
library(car) #Regression

#Initialisation 
rm(list=ls())

#1 Analyse transverse et transformation des donn�es 

#1.2 Data Quality
#Import des donn�es d'entrainement.
meteo_data =read.table("meteo.train.csv",sep=",",header=T,row.names=1)
summary(meteo_data)
# aucune donn�es inchoherente 

#1.3 Organisation du Dataset
#On va creer une variable Saison.
v1 = c(1:12) 
v2 =c("Hiver","Hiver","Printemps","Printemps","Printemps","Ete","Ete","Ete","Automne","Automne","Automne","Hiver")
Season_Month = data.frame(Month = v1, Season = v2) 

## on cree une clef unique pour les dates et saisons
meteo_data2=rbind(meteo_data[1:3])
date=data.frame(meteo_data[2],unite(data = meteo_data2,date,sep = "/"))
Season=right_join(Season_Month,date,by="Month")

meteo_all=cbind(Season[3],Season[2],meteo_data[6:46])

### Data set on garde que les valeurs moyennes
meteo_moy=cbind(meteo_all[1:20],meteo_data[46])

meteo_moy$date = as.Date(meteo_moy$date, format = "%Y/%m/%d")
#0n change les noms des colonnes
colnames(meteo_moy)[1]="Date"
colnames(meteo_moy)[3]="Temperature"
colnames(meteo_moy)[4]="Humidity"
colnames(meteo_moy)[5]="Pressure"
colnames(meteo_moy)[6]="Precipitation"
colnames(meteo_moy)[7]="Snowfall"
colnames(meteo_moy)[8]="Total.Cloud.Cover"
colnames(meteo_moy)[9]="High.Cloud.Cover"
colnames(meteo_moy)[10]="Medium.Cloud.Cover"
colnames(meteo_moy)[11]="Low.Cloud.Cover"
colnames(meteo_moy)[12]="Sunshine.Duration"
colnames(meteo_moy)[13]="Shortwave.Radiation"
colnames(meteo_moy)[14]="Wind.Speed.10"
colnames(meteo_moy)[15]="Wind.Direction.10"
colnames(meteo_moy)[16]="Wind.Speed.80"
colnames(meteo_moy)[17]="Wind.Direction.80"
colnames(meteo_moy)[18]="Wind.Speed.900"
colnames(meteo_moy)[19]="Wind.Direction.900"
colnames(meteo_moy)[20]="Wind.Gust"
colnames(meteo_moy)[21]="Pluie.Demain"

### On retravaille egalement le Dataset Complet 

meteo_all$date = as.Date(meteo_all$date, format = "%Y/%m/%d")
#0n change les noms des colonnes
colnames(meteo_all)[1]="Date"
colnames(meteo_all)[3]="Temperature"
colnames(meteo_all)[4]="Humidity"
colnames(meteo_all)[5]="Pressure"
colnames(meteo_all)[6]="Precipitation"
colnames(meteo_all)[7]="Snowfall"
colnames(meteo_all)[8]="Total.Cloud.Cover"
colnames(meteo_all)[9]="High.Cloud.Cover"
colnames(meteo_all)[10]="Medium.Cloud.Cover"
colnames(meteo_all)[11]="Low.Cloud.Cover"
colnames(meteo_all)[12]="Sunshine.Duration"
colnames(meteo_all)[13]="Shortwave.Radiation"
colnames(meteo_all)[14]="Wind.Speed.10"
colnames(meteo_all)[15]="Wind.Direction.10"
colnames(meteo_all)[16]="Wind.Speed.80"
colnames(meteo_all)[17]="Wind.Direction.80"
colnames(meteo_all)[18]="Wind.Speed.900"
colnames(meteo_all)[19]="Wind.Direction.900"
colnames(meteo_all)[20]="Wind.Gust"
colnames(meteo_all)[43]="Pluie.Demain"

#1.4 Graphes
# Etant donn� le nombres importants de variables nous allons juste afficher quelques graphique assez basiques

# graphe couverture nuageuse
gnu=ggplot(meteo_moy, aes(x = Pluie.Demain, y =High.Cloud.Cover)) +
  labs(title = "Couverture nuageuse en fonction de la pluie",
       x = "Pluie", y = "couverture nuageuse")
gnu+ geom_boxplot(fill = "lightyellow")

#graphe  shushine (reli� couverture nuageuse) et rayonement solaire bof
gsh=ggplot(meteo_moy, aes(x = Pluie.Demain, y =Sunshine.Duration)) +
  labs(title = "Ensoleillement en fonction de la pluie",
       x = "Pluie", y = "Ensoleillement")
gsh+ geom_boxplot(fill = "lightyellow")

#graphe pression bon r�sultat malgr�s des points abh�rents
gpa=ggplot(meteo_moy, aes(x = Pluie.Demain, y =Pressure)) +
  labs(title = "Pression en fonction de la pluie",
       x = "Pluie", y = "Pression")
gpa+ geom_boxplot(fill = "lightyellow")

#graphe vent 2 � prendre la direction et le speed � plus de 900m
gwd=ggplot(meteo_moy, aes(x = Pluie.Demain, y =Wind.Direction.900)) +
  labs(title = "Direction vent en fonction de la pluie",
       x = "Pluie", y = "Direction Vent")
gwd+ geom_boxplot(fill = "lightyellow") 

##  graphe en fonction de la saison
gse1=ggplot(meteo_moy, aes(x = Season, y = Temperature)) +
  geom_boxplot() +labs(x = "Season", y = "Temperature") +theme_bw()
gse1

## Graphes Pr�cipitation en fonction saison
gse2=ggplot(meteo_moy, aes(x = Season, y = Precipitation)) +
  geom_boxplot() +labs(x = "Season", y = "Precipitation") +theme_bw()
gse2

# Mainternant on change les donn�es de  pr�vison de pluie en Integer.
meteo_moy$Pluie.Demain=as.integer(meteo_moy$Pluie.Demain)
meteo_all$Pluie.Demain=as.integer(meteo_all$Pluie.Demain)

#Ainsi cette �tude pr�liminaire nous permet de voir les premi�res variables significatives et 
#pertinentes pour notre construction de mod�le, et de r�organiser notre dataset.
#Nous allons poursuivre cette analyse en �tudiant les liens entre variables et individus via une analyse en composante principale.

#2.ACP

#2.1 Matrice de Corr�lation et Analyse.
#Matrice de Corr�lation
Ca = cor(scale(meteo_all[, c(3:42)]))
# des choses sympas � commenter

# Pour que le graphique reste visible on affichera la matrice de Correlation des donn�es moyennes.
Cm = cor(scale(meteo_moy[, c(3:20)]))
corrplot(Cm, type = "upper", method = "square", order = "AOE")

#2.2 Chois des axes
#PCA sur les donn�es qualitatives Moyennes
meteo_moypca=PCA(meteo_moy[3:20])
meteo_moypca$eig
##En se r�f�rant au crit�re des 80% de variance nous gardons les 5 premiers axes. Cela revient � prendre 80.5% du signal
##Variance expliqu�e en respectant ce crit�re=> 80.5%

#Graphe des Axes.
barplot(meteo_moypca$eig[, 1],
        xlab = "n de composant", ylab = "valeur propre", 
        ylim = c(0, 7))

##PCA sur les donn�es qualitatives Completes
meteo_allpca=PCA(meteo_all[3:42])
meteo_allpca$eig

#Graphes des Axes.
barplot(meteo_allpca$eig[, 1],
        xlab = "n de composant", ylab = "valeur propre", 
        ylim = c(0, 15))
# En se r�f�rant au crit�re des 82% de variance nous gardons les 9 premiers axes. Cela revient � prendre 80.5% du signal
#Variance expliqu�e en respectant ce crit�re=> 82%

# Pour les 2 jeux de donn�es il est interessant de noter que les premiers axes ont des valeurs similaires
## De  plus si on applique le critere du coude sur le data set complet ,on observe un d�crochement (ou coude) dans la d�croissance, c'est-�-dire une acc�l�ration de cette derni�re, suivi d'une d�croissance plus r�guli�re, alors on s�lectionne les axes avant le d�crochement.
##En se basant sur ce crit�re nous devrions prendre 5 axes.

# Synthese pour le data set moyen on garde 5 axes et  7 axes pour le data set Complet.

#2.3 Analyses des individus
##graphes contributions individus

# Graphique sur les donn�es Moyennes
plot(meteo_moypca, choix = "ind", axes = c(1, 2), 
     select = "contrib 10" )
#Le graphique nous donne le sentiment qu'il existe un barycentre plut�t distinctif.

# Graphique  sur les donn�es Complet
plot(meteo_allpca, choix = "ind", axes = c(1, 2), 
     select = "contrib 10" )
#Tout comme pour les donn�es moyennes,le graphique nous donne le sentiment qu'il existe un barycentre plut�t distinctif.

#2.4 Analyse Automatiques des variables
#Descriptions quantitatives des dimensions et variables
meteo_moypca.desc = dimdesc(meteo_moypca)
meteo_allpca.desc = dimdesc(meteo_allpca)
#L'analyse automatique des axes donnent des (pour un grand nombre de variable) des r�sultats proches. 

#Axe 1
meteo_moypca.desc$Dim.1
meteo_allpca.desc$Dim.1
# L'axe 1 semble opposer le vent � la sunshine et shortwave duration et aussi � la temperature et pressure.
###commentaire nuage et vent

####axe 2 
meteo_moypca.desc$Dim.2
meteo_allpca.desc$Dim.2
# L'axe 2 semble opposer la couverture nuageuse au vent.
# Interessant de noter que pour les 2 sets de donn�es l'analyse automatique ne d�passent pas plus de 3 axes

#2.5 Conclusion ACP 

#L'ACP nous donne de nombreuses informations notamment sur les variables, leur corr�lations (positives ou n�gatives),
#le lien entre elles, ce qui nous sera fortement utile par la suite pour s�lectionner nos variables et construire des mod�les pertinents.
#De plus le fait de r�duire le data set � des donn�es moyennes ne constitue pas une perte significative d'informations. 
#Mais par acquis de conscience nous allons comparer 2 recherches de variables entre les 2 datasets.


#3 Recherche Variables et Mod�les.

#3.1 M�thode Automatique de choix de variables

#Modele Initial avec intercept Data Moyen
m0=glm(Pluie.Demain~1,data=meteo_moy, family = binomial)
#Modele avec toutes les variables (C complet)  Data Moyen
m1=glm(Pluie.Demain~.,data=meteo_moy, family = binomial)

##Modele  Initial avec intercept Data Complet
m0all=glm(Pluie.Demain~1,data=meteo_all, family = binomial)
##Modele avec toutes les variables (C complet) Data Complet
m1all=glm(Pluie.Demain~.,data=meteo_all, family = binomial)


#Choix variables M�thodes pas � pas.

#Construction Forward Data Complet 
mall_f=step(m0all, scope=list(lower=m0all, upper=m1all),data=meteo_all, direction="forward")
summary(mall_f)
#AIC 1296

#Construction Foward Data Moyen 
mmoy_f=step(m0, scope=list(lower=m0, upper=m1),data=meteo_moy, direction="forward")
summary(mmoy_f)
#AIC 1340

#Anova pour comparer les 2 r�sultats
anova(mmoy_f,mall_f,test = "LRT")
# Meme si le test indique de conserver le modele complet (1.78e-10) les r�sultats des r�sidus et de la deviance sont similaires.
# Nous pouvons garder donc le modele � partir des donn�es moyennes.

#Nous allons voir l'influence des variables dans le modele selection�.
anova(mmoy_f,test = "Chisq")

##La on lance d'autres types de M�thodes
##Methode Backward 
mmoy_b=step(m1, data=meteo_moy,direction="backward")
summary(mmoy_b)
#1351 AIC et pas d'autres variables ou information significatif.

###Methode Progressive
mmoy_bo=step(m0, scope = list(upper=m1),data=meteo_moy,direction="both")
summary(mmoy_bo)
#1340 AIC et pas d'autres variables ou information significatif.

#3.2 Choix de Mod�les
#Nous allons interesser au choix de modeles � partir des variables d'interets
#selection en ammont
choix_modele=regsubsets(Pluie.Demain~Medium.Cloud.Cover+Pressure+Wind.Direction.900+
                        Wind.Gust+Total.Cloud.Cover,int=T,
                        nbest=1,nvmax=10,method="exhaustive",data=meteo_moy)
resume=summary(choix_modele)

plot(choix_modele,scale="r2") # 5 Variables
plot(choix_modele,scale="adjr2") # 4 Variables
plot(choix_modele,scale="Cp")# 4 Variables
plot(choix_modele,scale="bic")# 3 Variables

# Ainsi par la suite nous allons nous interesser au modele sans le total Cloud Cover
# De plus j'ajoute la Temperature*Season afin d'obtenir un mod�le plus pertinent (cf summary(g1)).


#3.3 Mod�les Logistique vs Probit.

#Mod�le Logistique


g1=glm(formula = Pluie.Demain ~ Medium.Cloud.Cover + Pressure + 
        Wind.Direction.900 + Temperature:Season + Wind.Gust,
        family = binomial, data = meteo_moy)
summary(g1)
#1351 AIC

#Mod�le Probit
g1p=glm(formula = Pluie.Demain ~ Medium.Cloud.Cover + Pressure + 
         Wind.Direction.900 + Temperature:Season + Wind.Gust+
         Date,
         family = binomial(link = 'probit'), data = meteo_moy)
summary(g1p)
#1351 AIC

#Analyse de la deviance comparer 2 modeles
anova(g1,g1p, test = "LRT")

#On peut �galement prendre un autre critere.
BIC(g1p)
BIC(g1)
## Nous avons selectioner le modele g1 , pour confirmer que ce modele
## semble etre en adequation avec nos donn�es nous allons etudier le graphes des r�sidus

#3.4 Analyse des r�sidus du mod�le selection�.

residualPlots(g1)
#On peut voir plus en details :
prev_lin=predict(g1)
res_P=residuals(g1, type = "pearson") #Pearson
res_PS=rstandard(g1, type = "pearson") #Pearson standard
res_D=residuals(g1, type = "deviance") #Deviance
res_DS=rstandard(g1, type = "deviance") #Deviance standard

par(mfrow=c(2,2),pch=20)
plot(res_PS,xlab="index",ylab="Pearson Standard")
plot(prev_lin,res_PS,xlab="Prevision lineaire",
       ylab="Pearson Standard")
plot(res_DS,xlab="index",ylab="Deviance Standard")
plot(prev_lin,res_DS,xlab="Prevision lineaire",
       ylab="Deviance Standard")
#Analyse des r�sidus
##Aucun point ne semble etre loin de l'intervalle [-2,2] et pas de points abh�rent
##Les r�sidus de pearson sont �xag�res.

# Nous validons le choix du Mod�le Gaussien g1.
#Nous allons par la suite nous int�ress� a la pertinence du modele grace � la validation crois�

#4 Validation Crois�e et Pr�diction

#4.1  Validation Crois�e

#Initialisation on divise en K parties le dataset.
d=meteo_moy
k = 10  # Tout comme dans le TD je choisis K =10  
#Choix empirique j'ai vari� le K et j'obtiens de meilleur r�sultats k=10 (je ne saurais expliqu� la raison...)
index = sample(1:k, nrow(d), replace=T)
res.logistique = rep(NA, k)
res.probit = rep(NA, k)

#On lance l'Algorithme de Validation Kmean
##Par ailleurs on valide le modele avec les variables pr�c�demment valid�es via les fonctions log et probit. 

for(i in 1:k){
  reg.logistique = glm(
    Pluie.Demain ~ Medium.Cloud.Cover + Pressure + 
      Wind.Direction.900 + Temperature:Season + Wind.Gust+Date,
    family = binomial,
    data = d[index != i, ]
  )
  
  reg.probit = glm(
    Pluie.Demain ~ Medium.Cloud.Cover + Pressure + 
    Wind.Direction.900 + Temperature:Season + Wind.Gust+Date,
    family = binomial(link="probit"),
    data = d[index != i, ]
  )
  
  pred.logistique = predict(reg.logistique, newdata=d[index == i, ],
                            type="response")
  pred.probit = predict(reg.probit, newdata=d[index == i, ],
                        type="response")
  
  res.logistique[i] = mean(d[index==i, "Pluie.Demain"] == (pred.logistique >.5), na.rm = T)
  res.probit[i] = mean(d[index==i, "Pluie.Demain"] == (pred.probit >.5), na.rm = T)
  
}
# Fin de la boucle Kmean

#4.2 Erreur de Pr�diction

#On calcule les diff�rentes pr�dictions � partir des mod�les valid�es
#par Kmean (log,prob) et construite pr�decement.
pred.l = predict(reg.logistique, newdata=d[],
                          type="response")

pred.p = predict(reg.probit, newdata=d[],
                 type="response")

pred.p1 = predict(g1, newdata=d[],
                 type="response")

##On compare les differents r�sulats de pr�diction.
#Erreur moyenne du modele Log de la Kmean.
c=abs(pred.l - d["Pluie.Demain"])
dErreur=data.frame(c)
dErreur$Pluie.Demain=as.double(dErreur$Pluie.Demain)
mean(dErreur$Pluie.Demain)

##Erreur moyenne du modele probit de la Kmean.
c=abs(pred.p - d["Pluie.Demain"])
dErreur=data.frame(c)
dErreur$Pluie.Demain=as.double(dErreur$Pluie.Demain)
mean(dErreur$Pluie.Demain)

###Erreur moyenne du modele Prediction g1 Logistique valid� au chapitre 3
c=abs(pred.p1 - d["Pluie.Demain"])
dErreur=data.frame(c)
dErreur$Pluie.Demain=as.double(dErreur$Pluie.Demain)
mean(dErreur$Pluie.Demain)

# Ainsi nous validons le modele Logistique g1

#4.3 Pr�diction sur le dataset de test et export des r�sultats.

## par la suite nous allons pr�dire le % de pluie sur les donn�es de test.
#Import des donn�es de Test.
test_meteo_data =read.table("meteo.test.csv",sep=",",header=T,row.names=1)
summary(test_meteo_data)
# aucune donn�es inchoherente.

#On mets au format les donn�es de test iso au donn�es au data set Moyen(meteo_moy).
test_meteo_data2=rbind(test_meteo_data[1:3])
test_date=data.frame(unite(data = test_meteo_data2,date,sep = "/"),test_meteo_data[2])
tSeason=left_join(test_date,Season_Month,by="Month")
test_meteo_date3=cbind(tSeason[1],tSeason[3],test_meteo_data[6:45])
#On garde les donn�es moyennes.
test_meteo_moy=cbind(test_meteo_date3[1:20])
test_meteo_moy$date = as.Date(test_meteo_moy$date, format = "%Y/%m/%d")
#0n change les noms des colonnes.
colnames(test_meteo_moy)[1]="Date"
colnames(test_meteo_moy)[3]="Temperature"
colnames(test_meteo_moy)[4]="Humidity"
colnames(test_meteo_moy)[5]="Pressure"
colnames(test_meteo_moy)[6]="Precipitation"
colnames(test_meteo_moy)[7]="Snowfall"
colnames(test_meteo_moy)[8]="Total.Cloud.Cover"
colnames(test_meteo_moy)[9]="High.Cloud.Cover"
colnames(test_meteo_moy)[10]="Medium.Cloud.Cover"
colnames(test_meteo_moy)[11]="Low.Cloud.Cover"
colnames(test_meteo_moy)[12]="Sunshine.Duration"
colnames(test_meteo_moy)[13]="Shortwave.Radiation"
colnames(test_meteo_moy)[14]="Wind.Speed.10"
colnames(test_meteo_moy)[15]="Wind.Direction.10"
colnames(test_meteo_moy)[16]="Wind.Speed.80"
colnames(test_meteo_moy)[17]="Wind.Direction.80"
colnames(test_meteo_moy)[18]="Wind.Speed.900"
colnames(test_meteo_moy)[19]="Wind.Direction.900"
colnames(test_meteo_moy)[20]="Wind.Gust"
#Dataset au bon format!

#On peut effectuer une prediction sur le dataframe de test.
t=test_meteo_moy
#On construire le dataframe de previson � partir de la fonction qu'on estime la meilleure
tpred.p1 = predict(g1, newdata=t[],
                  type="response")
##On exporte les donn�es de pr�dictions.
##D'abord on construit un dataframe qu'on va par la suite modifi� pour le mettre au format exig�.
t1 = c(t$Date) 
t2 =c(tpred.p1)
t3= data.frame(Date = t1,Pluie = t2)
#On transforme les pourcentages de pr�vision en texte Bool�en "True" ,"False"
t3$Pluie=round(t3$Pluie)
x1 = c(0:1) 
x2 =c("FALSE","TRUE")
xPluie = data.frame(Pluie = x1, Prevision_Pluie = x2)
xPrevison=left_join(t3,xPluie,by="Pluie")

#DataFrame au format correct.
Prevision=data.frame(xPrevison[1],xPrevison[3])
#Export du dataframe des pr�visons.
write.csv(x = Prevision, file = "PrevisionMeteo.csv")

