
#**************************************************
#**************************************************
#**                    TP  AFC                   **
#**************************************************
#**************************************************

library("FactoMineR")
library("factoextra")
library("gplots")
library("corrplot") 

#*************************************************************
# Exemple                                                    *
                                                             
# Dans cet exemple, on consid?re deux variables qualitatives * 
# X et Y observ?es sur une population de taille n=592.       *
                                                             
# avec:                                                      
                                                             
# X : repr?sente les couleurs des yeux                       
# Y : repr?sente les couleurs des cheveux                    *
                                                             
# Le tableau de contingence obtenu est le suivant:           
                                                             
                                                             
# *********************************************              *
# *           * brun * ch?tain * roux * blond *              *
# *********************************************              *
# * marron    * 68   *   119   *  26  * 7     *              *
# * noisette  * 15   *    54   *  14  * 10    *              *
# * vert      * 5    *    29   *  14  * 16    *              *
# * bleu      * 20   *    84   *  17  * 94    *              *
# *********************************************              *
                                                             
#*************************************************************

# Cr?ation de tableau de contingence sur R:        

n=592

L1=c(68 ,119, 26, 7)
L2=c(15 ,54 ,14 ,10)
L3=c(5, 29, 14, 16)
L4=c(20 ,84 ,17 ,94)

# Les noms des colonnes

colonne=c("brun", "ch?tain", "roux", "blond")

# Les noms des lignes

ligne=c("marron", "noisette","vert","bleu")

# Tableau de contingence

N=matrix(c(L1,L2,L3,L4) ,4, 4, byrow=T, dimnames=list(ligne,colonne)) 
N 

dim(N)

NN=addmargins(N)
NN



# Visualisation du tableau de contingence
# library("gplots")

tab=as.table(N)
tab

balloonplot(t(tab), main = "", xlab = "", ylab = "",
            label = TRUE, show.margins = TRUE)

#*************************************************************
#*************************************************************


# Tableau des fr?quences

F=N/n
F

# Les matrices diagonales des effectifs marginaux: Dl et Dc

Dl=diag(c(sum(N[1,]),sum(N[2,]),sum(N[3,]),sum(N[4,])) ,4,4)
Dl

Dc=diag(c(sum(N[,1]),sum(N[,2]),sum(N[,3]),sum(N[,4])) ,4,4)
Dc

# Tableau des profils lignes: Pl=Dl^(-1) N

Pl=solve(Dl)%*%N
Pl

# Tableau des profils colonnes: N Dc^(-1)

Pc=N%*%solve(Dc)
Pc

# Profil moyen des lignes GI (centre de gravit?):

#   GI = (1/n)(Dl^(-1) N)'Dl I
#      = (1/n)(Pl)'Dl I

# avec  I d?signe le vecteur de R^I dont toutes les 
# composantes sont ?gales ? 1


I=matrix(1,4,1)
I

GI=(1/n)*t((solve(Dl)%*%N))%*%Dl%*%I
GI

# ACP des profils-lignes

# Tableau de donn?es X1:

X1=Pl
 
# La matrice ?diagonaliser est VM (voir page 29 de cours AFC):

# VM = N' Dl^(-1) N Dc^(-1) - n GI GI' Dc^(-1)
#    = L - n GI GI' Dc^(-1)

# avec L = N' Dl^(-1) N Dc^(-1)


VM=t(N)%*%solve(Dl)%*%N%*%solve(Dc)-n*GI%*%t(GI)%*%solve(Dc)
VM

L=t(N)%*%solve(Dl)%*%N%*%solve(Dc)
L

eigen(VM)
eigen(L)


#********************************************************
# R?alisation d'une AFC sur le tableau de contingence N *
#********************************************************


# R?alisation du test d'ind?pendance de khi-deux

khi2_test = chisq.test(N)
khi2_test 

# Calcul de quantille d'ordre 1-alpha de loi de khi-deux

qchisq(0.95,df=9) 

# Calcul de P-value

pchisq(138.29,df=9,lower.tail = FALSE) 
 
# ou

khi2_test$p.value 

#********************************************************
#             AFC avec  FactoMineR et factoextra        *
#********************************************************


#library("FactoMineR")
#library("factoextra")

?CA()
afc=CA(N, ncp=3, graph=TRUE)
afc

summary(afc)

# Remarque:**********************************************
# Le nombre d'axes maximum que l'on peut produire est:  *
# Hmax=Min(I-1, J-1)                                    *
#*********************************************************



#********************************************************
#                 Les valeurs propres                   *
#********************************************************

val_pro = get_eigenvalue (afc)
val_pro



fviz_eig(afc, addlabels = TRUE, choice = c("eigenvalue"))
fviz_eig(afc, addlabels = TRUE, choice = c("variance"))

#*******************************************************
#R?gle de Kaiser : Retenir un axe si le pourcentage    *
#                  d?inertie est sup?rieur ? (1/Hmax)  *
#                  Dans cet exemple: 1/Hmax=1/3= 33%   *
#*******************************************************

fviz_eig(afc,addlabels = TRUE, choice = c("variance"))+ 
  geom_hline (yintercept = 33.33, linetype = 2, color = "red")


# Carte factorielle

fviz_ca_biplot(afc,axes=c(1,2), repel = TRUE)

#*****************************************************
#                 Etude des lignes                   *
#*****************************************************

lignes = get_ca_row(afc)
lignes

# Les coordonn?es des lignes sur chaque axe

lignes$coord

# Qualit? de repr?sentation

lignes$cos2

#Remarque:*****************************************************
#La qualit? de repr?sentation de chaque ligne                 *
#sur l'axe s est donn?e par:                                  *
#                                                             *
#    QL(Li)=(GIHis)^2/d^2(Li, GI)                             *
#          =(coordonn?e de Li sur s)^2/d^2(Li, GI)            *
#                                                             *
#avec d^2(Li, GI) est la distance de khi-deux entre Li et GI. *
#et GI est le profil moyen                                    *
#**************************************************************

# Question: Calculer la distance de khi-deux entre Li et GI 
# avec i=1,...,4

for(k in 1:4){
disstt[k]=print(n*t(Pl[k,]-GI)%*%solve(Dc)%*%(Pl[k,]-GI))
}

# Visualiser la corr?lation

corrplot(lignes$cos2, is.corr=FALSE)  


# Les contributions aux axes 

lignes$contrib
fviz_contrib(afc, choice = "row", axes = 1:2, top = 10)


#Remarque:***************************************************** 
#La contribution de chaque ligne ? l'axe s est donn?e par:    *                            
#                                                             *       
#    CT(Li)=(fi.)*(GIHis)^2/lambda_s                          *
#          =(fi.)*(coordonn?e de Li sur s)^2/lambda_s         *
#                                                             *
#**************************************************************

#Exemple: (sum(N[2,]/n)*lignes$coord[2,1]^2)/ val_pro[1,1]

#library("corrplot")

corrplot(lignes$contrib, is.corr=FALSE)  

#Inertie de chaque ligne

lignes$inertia

#La somme des inerties de chaque ligne =l'inertie du nuage 
#                                       des profils-lignes
#                                      =la somme des valeurs 
#                                       propres


# Carte des lignes

fviz_ca_row (afc)


#*****************************************************
#                 Etude des colonnes                 *
#*****************************************************


colonnes = get_ca_col(afc)
colonnes 

colonnes$coord
colonnes$cos2
colonnes$contrib
colonnes$inertia


# Colorer en fonction du cos2: Biplot

fviz_ca_biplot(afc, col.row = "cos2", col.col = "cos2",
             gradient.cols = c ("green", "black"),
             repel = TRUE,axes=c(1,2))



#******************************************************
#*                     FIN                            *
#******************************************************
 

