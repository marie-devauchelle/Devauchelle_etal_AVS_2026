rm(list = ls())
#Etape 0 : package loading ####
library(ggplot2)
library(tidyverse)
library(data.table)
library(stringr)
library(sf)
library(vegan) #indices de diversité
library(cowplot) #ajouter les graphiques ggplot côte à côte
library(tictoc)
library(ggpubr)
library(forcats)
library(hillR)

# lecture carte régions forestières Ifn
setwd("~/These/data/volet1/spatial")
map_reg <- st_read("rnifn250.shp")

#lecture carte SER IFN
setwd("~/These/data/volet1/spatial")
map_ser <- st_read("ser_l93.shp")
names(map_ser) <- c("SER", "NomSER", "geometry")

#Etape 0 : Chargement des donnees d'entree ####
setwd("~/These/data")
#sortie habitat du PCH avec les donnees brute IFN + les non recensables donnes par Jean-Luc + les arbres issues de couv
hab <- fread("Classement_IFN_2005-2023_nr_arbres_couv.csv") #104 637 relevés classés
control <- unique(hab[EUR15_GEN!="",.(Assoc,EUR15_GEN)])
any(duplicated(control$EUR15_GEN)) 
#il y a des hab EUR15_GEN qui correspondent à plusieurs associations
#Il y a beaucoup d'associations par EUR15_GEN. Toutes les assos ne sont pas d'intérêt
#communautaire (et dans ce cas elles n'ont pas de EUR15_GEN)
control[duplicated(EUR15_GEN) | duplicated(EUR15_GEN, fromLast=TRUE)][order(EUR15_GEN)]

#Correction orthographe association
hab$Assoc <- ifelse(hab$Assoc == "Pruno padi-Fraxinetum exclesioris", "Pruno padi-Fraxinetum excelsioris", hab$Assoc)

#croisment avec les altitudes des placettes données par l'IGN : fichier transmis par JL
#Nous voulons conserver les altitudes jusqu'à 600m, zone où les chênes tempérés caducifoliés sont en limite 
#de niche. Cela afin de ne pas sélectionner les hêtraies qui se trouvent dans des zones où le chêne ne peut pas s'implanter
setwd("~/These/data")
alt_placette <- fread("altitudes_Ifn_2005_2023_synthese_JLD_v20241122.txt")
nrow(alt_placette) #121 419
#merge avec hab
hab$inhab <- 1; alt_placette$inalt <- 1
hab <- merge(hab, alt_placette, by = "IDP", all.x = TRUE)
table(hab$inhab, hab$inalt, useNA = "always")
hab$inalt <- NULL ; hab$inhab <- NULL

#selection ZP<600m d'altitude
#seuil altitude
# altmax <- 600
# 
# nrow(hab) #112 506
# hist(hab$ZP)
# hab <- hab[ZP<altmax]
# nrow(hab) # 85 637
# hist(hab$ZP)

#On choisi d'enlever les releves à moins de 7 sp prise en compte par le programme
#(assignation à l'alliance  Maciejewski et al 2021)
seuil_sp <- 7
nrow(hab) #118 815
hab <- hab[S_PHI>=seuil_sp]
nrow(hab) #116 515: il y a 2300 releves avec moins de 7 espèces

#donnees brutes sur les placettes
setwd("~/These/data/volet1")
placette_IFN_brute <- fread("export_dataifn_2005_2023/PLACETTE.csv")
nrow(placette_IFN_brute) #193 515, 2005-2023 : 205 312
# placette_sans_Corse <- placette_IFN_brute[!DEP %in% c("2A", "2B")]
# nrow(placette_sans_Corse) #202 482

#donnees issues de croisement IFN entre EM et idp DONNEES HISTORIQUES
#Attention ! Contient des IDP pour lesquels il n'ya pas de flore
setwd("~/These/data/volet1/Envoi_IFN_croisement_EM_IFN_012025")
crois_EM1 <- fread("couverture_placette_20250114.csv", dec=",")
crois_EM1$IDP <- crois_EM1$idp; crois_EM1$idp <- NULL
nrow(crois_EM1) #47 232 relevés #135 380

#explo croisement EM IFN
# test_placette <- setdiff(unique(placette_sans_Corse$IDP), unique(crois_EM$IDP))
# test_croisEM <- setdiff(unique(crois_EM$IDP), unique(placette_sans_Corse$IDP))
# length(test_placette)# 752 IDP dans placette mais pas dans crois_EM
# table(placette_IFN_brute[IDP %in% test_placette]$CAMPAGNE)#certaines campagnesplus concernées : 2016, 2017, 2018 et 2019
# table(placette_IFN_brute[IDP %in% test_placette]$DEP) #un peu partout
# # setwd("~/These/data/volet1")
# # fwrite(placette_IFN_brute[IDP%in% test_placette, .(CAMPAGNE, VISITE, IDP, XL, YL, DEP, SER)], "IDP_dans_placette_absents_croisement.csv", sep = ";")
# length(test_croisEM)# 16 537 IDP dans crois_EM mais pas dans placette : c'est ok : les points non levés 

#enlever les points mis en FR mais non présents sur EM (d'après Maxime, tableur de Maxime)
#Je fais le choix de les enlever car on ne sait pas leur histoire / Tout a été mis par défaut en FR (j'ai vérifié)
#Departements consernés 05/ 06/ 17/ 27/ 33 / 59/ 67/ 68/ 73/ 76 
setwd("~/These/data/volet1")
sans_EM <- fread("Placette_sans_Etat-Major.csv")
nrow(crois_EM1); nrow(sans_EM) #135 380; 231
crois_EM1 <- crois_EM1[!IDP %in% sans_EM$idp]
nrow(crois_EM1) #nrow(crois_EM) - nrow(sans_EM) = 135 149 : OK!

#création d'une nouvelle variable FA et FR en ne prenant comme forestier que le code
#9.1
#Test 1 TYPE_OCC_2 : avec croisement CEM et BDV2 : FA c'est 9.1 en CEM et FF en BDV2
crois_EM1[CEM_CODE_THEME=="EM9.1" & REGROUP_BF == "FF", FORET_EM_2 := 1]
crois_EM1[(CEM_CODE_THEME!="EM9.1" | is.na(CEM_CODE_THEME)) & REGROUP_BF == "FF", FORET_EM_2 := 0]
#Test 2 TYPE_OCC_3 : si c'est dans l'IFN c'est forestier donc : FA c'est 9.1 en CEM
crois_EM1[CEM_CODE_THEME=="EM9.1", FORET_EM_3 := 1]
crois_EM1[CEM_CODE_THEME!="EM9.1" | is.na(CEM_CODE_THEME), FORET_EM_3 := 0]
#crois_EM <- crois_EM[TYPE_OCC=="FA"|TYPE_OCC =="FR"]
# nrow(crois_EM) #124 122
crois_EM1[TYPE_OCC=="FA",FORET_EM := 1][TYPE_OCC=="FR", FORET_EM:= 0]
crois_EM <- crois_EM1[,.(IDP, FORET_EM, FORET_EM_2, FORET_EM_3, CEM_NEAR_DIST, BF_NEAR_DIST)]
table(crois_EM$FORET_EM, useNA = "always")
table(crois_EM$FORET_EM_2, useNA = "always")
table(crois_EM$FORET_EM_3, useNA = "always")


#Etape 0 : Preparation du tableur a analyser ####
info_placette <- unique(placette_IFN_brute[,.(IDP, XL, YL, DEP, SER, CSA, CAMPAGNE)])
nrow(info_placette) #205 312

# UTILISATION REGION FORESTIERE ANCIENNES
# croisement avec les régions forestières : on ajoute deux nouvelles colonnes, 
# REGN (numéro) et REGIONN (nom) des régions forestières
# dans la table placette_IFN_brute
placSf <- st_as_sf(unique(placette_IFN_brute[,.(IDP,XL,YL)]),coords=c("XL","YL"),crs=2154)
nrow(placSf) # 121 419 localités uniques #121 419
placReg <- st_intersection(placSf,map_reg[,c("REGN","REGIONN")])
nrow(placReg) # 121 403, on en perd 19, hors de la carte des régions, sans conséquence #121 403
info_placette <- merge(info_placette,data.table(st_drop_geometry(placReg)),by=c("IDP"),all.x=TRUE)

info_placette$inplac <- 1
Hab_IFN <- merge(hab[,.(IDP, CAMPAGNE, Assoc, TYPE_SYN, Pboncla, S_REL, S_PHI, ALL, EUR15_GEN, EUR15_ELE, Id_DocGegout09)], 
                 info_placette, by = c("IDP", "CAMPAGNE"), all.x = TRUE)
Hab_IFN[is.na(inplac),.(IDP)] #VIDE = OK ! 
nrow(Hab_IFN) #116 515 releves  #avec alt < 600m : 85 637, mise à jour 2023 : 118 815
Hab_IFN$inplac <- NULL; info_placette$inplac <- NULL

# On enlèves les peupleraies et autres formations non forestières
table(Hab_IFN$CSA, useNA = "always")
#1018 relevés en moins qui sont tous des peupleraies
Hab_IFN <- Hab_IFN[CSA == 1 | CSA == 2 | CSA == 3]
nrow(Hab_IFN) # 115 497 + 1018 = 116 515 OK!

# on enlève les relevés en J (Médit) et K (Corse) :

#avec SER et REGN
Hab_IFN[, GRECO:=substr(SER, 1, 1)]
#avec SER allu
#Hab_IFN[, GRECO:=substr(SERini, 1, 1)]

table(Hab_IFN$GRECO,useNA="always")
Hab_IFN <- Hab_IFN[!GRECO %in% c("J","K")]
nrow(Hab_IFN) # 106 679 relevés #avec alt < 600m : 78 974, 2023 : 109 847


#on attribue une categorie a chacune de ces EUR15s

#REGLES DE CLASSEMENT : 
#On classe à l'alliance
#- chenaies : Quercetalia ilicis, Quercetalia pubescentis, Quercetalia roboris, et alliance : Fraxino excelsioris-Quercion roboris (dans Fagetalia sylvaticae, il y a eu un
#changement dans PVF2, c'est passé en Quercetalia ?).
#- hetraies : Fagion sylvaticae, Luzulo fagion, Carpino betuli-Fagion. Ordre Fagetalia sylvaticae (à l'exception de Fraxion excelsioris-Quercion roboris)
#- ripisylves_marecages_tourbieres : ordre : Populetalia albae, Alnetalia glutinosa, Salicetalia auritae, Salicetalia purpureae.
#- pessieres_sapinieres : ordre: Piceetalia excelsae
#- pineraies :  Pinetalia sylvestris, et alliances : Rhododendro ferruginei-Vaccinion myrtilli, Pino sylvestris-Juniperetea sabinae.
#- forets_ravins : Acerion pseudoplatani

#je l'ai fait manuellement à partir de la donnée "type d'habitat" du doc Gégout 2009. 
# setwd("~/These/data/volet1")
# cat_asso <- fread("cat_asso_V2_complet.txt", encoding = "Latin-1")
# 
# Hab_IFN <- merge(Hab_IFN, cat_asso[,.(Assoc, Alliance, Cat)], by = "Assoc", all.x = TRUE)
# table(Hab_IFN[!is.na(EUR15_GEN)]$Cat, useNA = "always")
# 
# control <- unique(Hab_IFN[,.(Alliance, Cat)])
# any(duplicated(control$Alliance))#FALSE indique qu'une alliance est toujours dans la même Cat
# #La classification fonctionne bien par Alliance (avant les changements de Cat par EUR15GEN)

#création de Cat pour EUR15_GEN
Hab_IFN[EUR15_GEN=="9110", Cat:="beech forests"][EUR15_GEN=="9120", Cat:="beech forests"][EUR15_GEN=="9130", Cat:="beech forests"][
  EUR15_GEN=="9140", Cat:="beech forests"][EUR15_GEN=="9150", Cat:="beech forests"][EUR15_GEN=="9160", Cat:="oak forests"][
    EUR15_GEN=="9170", Cat :="oak forests"][EUR15_GEN=="9180*", Cat :="forest of slopes, screes and ravines"][EUR15_GEN=="9190", Cat :="oak forests"][
      EUR15_GEN=="91D0*", Cat :="alluvial, riparian and bog forests"][EUR15_GEN=="91E0*", Cat :="alluvial, riparian and bog forests"][
        EUR15_GEN=="91F0", Cat :="alluvial, riparian and bog forests"][EUR15_GEN=="9230", Cat :="oak forests"][EUR15_GEN=="92A0", Cat :="alluvial, riparian and bog forests"][
          EUR15_GEN=="9410", Cat :="spruce and fir forests"][EUR15_GEN=="9420", Cat :="Pine forests"][EUR15_GEN=="9430*", Cat :="Pine forests"][
            EUR15_GEN=="9540", Cat :="Pine forests"][EUR15_GEN=="9560*", Cat :="Pine forests"][EUR15_GEN=="",Cat :=NA]

table(Hab_IFN[!is.na(EUR15_GEN)]$Cat, useNA = "always")

control <- unique(Hab_IFN[,.(EUR15_GEN, Cat)])
any(duplicated(control$EUR15_GEN))
control2 <- control[duplicated(control$EUR15_GEN)|duplicated(control$EUR15_GEN,fromLast = TRUE)]
control2[order(EUR15_GEN, Cat)]

#UTILISATION REGN
#occ_REGN : nb de présence de l'HIC pour chaque REGN
Hab_IFN[,occ_REGN:= .N,by = .(REGN, EUR15_GEN)]
#pourcentage d'occurrence de l'association pour chaque REGN
Hab_IFN[,pc_REGN := 100 * occ_REGN/ .N, by = .(REGN)]
#freq_glob_Assoc : fréquence globale par HIC 
Hab_IFN[,freq_glob_Assoc := .N, by = .(EUR15_GEN)]

#SER et SER allu
#fréquence de chaque association dans chaque SER.
# freq_Assoc_SER <- unique(Hab_IFN[,.(SER, Assoc, occ_SER)])[order(occ_SER)]
# #histogrammes d'occurrence d'association par SER
# hist(log10(freq_Assoc_SER$occ_SER))
# ggplot(data = freq_Assoc_SER, aes(x = occ_SER)) + geom_histogram() + scale_x_continuous(trans='log10')

#REGN
#fréquence de chaque HIC dans chaque REGN.
freq_EUR15_REGN <- unique(Hab_IFN[,.(REGN, EUR15_GEN, occ_REGN)])[order(occ_REGN)]
#histogrammes d'occurrence d'HIC par REGN
hist(log10(freq_EUR15_REGN$occ_REGN))
ggplot(data = freq_EUR15_REGN, aes(x = occ_REGN)) + geom_histogram() + scale_x_continuous(trans='log10')

#on joint l'information historique aux habitats
nrow(Hab_IFN)#106 679, alt : 78 974
Hab_IFN_histo <-merge(Hab_IFN, crois_EM, by = "IDP", all = FALSE)
nrow(Hab_IFN_histo);length(unique(Hab_IFN$EUR15_GEN)); length(unique(Hab_IFN$Assoc))##105 907 releves avec histoire, 19 HIC (19 + 1 NON HIC); 129 assos
any(duplicated(Hab_IFN_histo$IDP)) #répétitions d'IDP !! : FALSE = OK ! 

#Sélection de la méthode FORET_EM_2
nrow(Hab_IFN_histo) #105 907
Hab_IFN_histo <- Hab_IFN_histo[!is.na(FORET_EM_2)]
nrow(Hab_IFN_histo) #102 349
#renommer FORET_EM en FORET_EM_1, pour renommer FORET_EM_2 en FORET_EM 
Hab_IFN_histo[,FORET_EM_1:=FORET_EM][,FORET_EM:=NULL]
Hab_IFN_histo[,FORET_EM:=FORET_EM_2][,FORET_EM_2:=NULL]

#Test avec zone tampon de 20 m
nrow(Hab_IFN_histo)#102 349
Hab_IFN_histo <- Hab_IFN_histo[CEM_NEAR_DIST>=20]
nrow(Hab_IFN_histo)#85 656 à 30m, 90 746 à 20m

#setwd("~/These/data/volet1")
#fwrite(Hab_IFN_histo, "Hab_IFN_Histo_2005-2023_sans-sel.csv")

##################################################################################################################################################
## Diversité d'habitats

#lecture de l'estimation des poids surfaciques par SER
setwd("~/These/data/volet1")
poids_surf_ser <- fread("poids_SER.txt")

#On crée une colonne pour indiquer si c'est un HIC ou pas
Hab_IFN_histo[!is.na(EUR15_GEN), HCI := "YES"][EUR15_GEN == "", HCI := "NO"]

#On merge avec les infos de poids surfacique
nrow(Hab_IFN_histo) #90746
Hab_IFN_histo <- merge(Hab_IFN_histo, poids_surf_ser[,.(SER, nbPlacSER, poids_ha_par_placette)], all.x = TRUE, by = "SER")
nrow(Hab_IFN_histo) #90746 : OK

#Creation tableau HIC seulement
HIC <- Hab_IFN_histo[HCI == "YES"]
nrow(Hab_IFN_histo); nrow(HIC) #90 746; 63 344
HIC_FA <- HIC[FORET_EM==1] ;nrow(HIC_FA) #35 556
HIC_FR <- HIC[FORET_EM==0] ;nrow(HIC_FR) #27 788

#Construction du jeu de données
FA <- sample(HIC_FA$IDP, 10000, replace = FALSE)
FR <- sample(HIC_FR$IDP, 10000, replace = FALSE)


ech1_FA <- HIC[IDP %in% FA,.(Nb_obs = .N), by = .(EUR15_GEN)]
ech1_FR <- HIC[IDP %in% FR, .(Nb_obs = .N), by = .(EUR15_GEN)]

ech_FA <- HIC_FA[,.(Nb_obs=.N), by = .(EUR15_GEN)]
ech_FR <- HIC_FR[,.(Nb_obs=.N), by = .(EUR15_GEN)]

diversity(ech_FA$Nb_obs, index = "simpson")
diversity(ech_FR$Nb_obs, index = "simpson")
diversity(ech_FA$Nb_obs, index = "shannon")/log2(length(ech_FA$EUR15_GEN))# indice Piélou
diversity(ech_FR$Nb_obs, index = "shannon")/log2(length(ech_FR$EUR15_GEN))# indice Piélou
exp(diversity(ech_FA$Nb_obs, index = "shannon"))
exp(diversity(ech_FR$Nb_obs, index = "shannon"))


Simpson1_FA <- diversity(ech1_FA$Nb_obs, index = "simpson") # 0 = 1 sp, 1 ) nb infinbi d'sp
Simpson1_FR <- diversity(ech1_FR$Nb_obs, index = "simpson")
Shannon1_FA <- diversity(ech1_FA$Nb_obs, index = "shannon") #plus la valeur est grande, div spé et hétérogénéité : ajouter Piélou ?
Shannon1_FR <- diversity(ech1_FR$Nb_obs, index = "shannon")


#2 eme ech
FA2 <- sample(HIC_FA$IDP, 10000, replace = FALSE)
FR2 <- sample(HIC_FR$IDP, 10000, replace = FALSE)

ech2_FA <- HIC[IDP %in% FA2,.(Nb_obs = .N), by = .(EUR15_GEN)]
ech2_FR <- HIC[IDP %in% FR2, .(Nb_obs = .N), by = .(EUR15_GEN)]

(Simpson2_FA <- diversity(ech2_FA$Nb_obs, index = "simpson")) # 0 = 1 sp, 1 ) nb infinbi d'sp
(Simpson2_FR <- diversity(ech2_FR$Nb_obs, index = "simpson"))
(Shannon2_FA <- diversity(ech2_FA$Nb_obs, index = "shannon")) #plus la valeur est grande, div spé et hétérogénéité : ajouter Piélou ?
(Shannon2_FR <- diversity(ech2_FR$Nb_obs, index = "shannon"))

#3 eme ech avec 1000 ind
FA3 <- sample(HIC_FA$IDP, 1000, replace = FALSE)
FR3 <- sample(HIC_FR$IDP, 1000, replace = FALSE)

ech3_FA <- HIC[IDP %in% FA3,.(Nb_obs = .N), by = .(EUR15_GEN)]
ech3_FR <- HIC[IDP %in% FR3, .(Nb_obs = .N), by = .(EUR15_GEN)]

(Simpson3_FA <- diversity(ech3_FA$Nb_obs, index = "simpson")) # 0 = 1 sp, 1 ) nb infinbi d'sp
(Simpson3_FR <- diversity(ech3_FR$Nb_obs, index = "simpson"))
(Shannon3_FA <- diversity(ech3_FA$Nb_obs, index = "shannon")) #plus la valeur est grande, div spé et hétérogénéité : ajouter Piélou ?
(Shannon3_FR <- diversity(ech3_FR$Nb_obs, index = "shannon"))

#Courbes d'accumulation
HIC_FA[,pres:=1]
comm_FA <- dcast(HIC_FA, IDP~EUR15_GEN, value.var = "pres" ,fill = 0)
comm_FA_df <- data.frame(comm_FA)
rownames(comm_FA_df) <- comm_FA_df$IDP
comm_FA_df$IDP <- NULL
HIC_FR[,pres:=1]
comm_FR <- dcast(HIC_FR, IDP~EUR15_GEN, value.var = "pres" ,fill = 0)
comm_FR_df <- data.frame(comm_FR)
rownames(comm_FR_df) <- comm_FR_df$IDP
comm_FR_df$IDP <- NULL

#method = exact
courb_acc_FA <- specaccum(comm_FA_df, method = "exact")
courb_acc_FR <- specaccum(comm_FR[,-"IDP"], method = "exact")

plot(courb_acc_FR, ci.type = "line", xlim=c(0,40000), col = "orange")
plot(courb_acc_FA, ci.type = "line", add = TRUE, col = "blue")
abline(v=14430, col = "lightgrey", lwd=1, lty=2)
#abline(v=1200, col = "lightgrey", lwd=1, lty=2)

#method = random
courb_acc_FA_r <- specaccum(comm_FA_df, method = "random", permutations = 1000)
courb_acc_FR_r <- specaccum(comm_FR[,-"IDP"], method = "random", permutations = 1000)

plot(courb_acc_FR_r, ci.type = "line", xlim=c(0,40000), col = "orange")
plot(courb_acc_FA_r, ci.type = "line", add = TRUE, col = "blue")
abline(v=14300, col = "lightgrey", lwd=1, lty=2)

max(courb_acc_FA_r$richness)#17 HIC au max en FA
max(courb_acc_FR_r$richness)# 19 HIC au max en FA

max(courb_acc_FA_r$sites) #35 556 sites max en FA
max(courb_acc_FR_r$sites) #27 788 sites max en FR

#cartes avec indices de diversité ##########################################################################################################
#calcul des indices de diversité

#Création d'un tableau avec sample aléatoire pour équilibre le nombre d'éch en FA/FR
Nb_FR_perSER <- Hab_IFN_histo[FORET_EM==0, .(FR = .N), by= .(SER)]
Nb_FA_perSER <- Hab_IFN_histo[FORET_EM==1, .(FA = .N), by= .(SER)]
Nb_FA_FR_perSER <- merge(Nb_FA_perSER, Nb_FR_perSER, by = "SER")
#on indique dans minFAFR dans quel type de forêt on a le minimum de plots
Nb_FA_FR_perSER[,min_EM:=ifelse(FA<FR, "1", "0")][,minFAFR:=ifelse(FA<FR, FA, FR)]
Nb_FA_FR_perSER[,max_EM:=ifelse(FA>FR, "1", "0")]
Nb_FA_FR_perSER[,diff_FA_FR := FA - FR]
nrow(Nb_FA_FR_perSER[min_EM == 1])#dans 43/76 SER on a plus de FR = 0.57
nrow(Nb_FA_FR_perSER[min_EM == 0])#dans 33/76 SER on a plus de FA = 0.43
sum(Nb_FA_FR_perSER$minFAFR)#29 848  * 2 = 59 696 relevés dans le data.table de sortie 

#CI-DESSOUS : faire tourner pour ré-échantilloner la carte nationale technique 1 : moyenne de fréquence puis
#calcul des indices 
# 
# #initialisation tableur boucle 1
#  rich_hab_SER_f <- data.table()
# 
#  for(i in c(1:200)){
#    #i <- 1
#    print(i);flush.console()
# 
# #initialisation du tableur boucle 2
#  sample_Hab_IFN <- data.table()
# 
# 
#      #boucle de remplissage du data.table avec sample correspondant au minimum de points.
#      for(SERl in Nb_FA_FR_perSER$SER){
#      #SERl <- "A11"
# 
#        dims <- Nb_FA_FR_perSER[SER==SERl]$minFAFR
#        EM_s <- Nb_FA_FR_perSER[SER==SERl]$min_EM
#        IDP_s <- sample(Hab_IFN_histo[SER==SERl & FORET_EM!=EM_s]$IDP, dims, replace = FALSE)
#        sample_Hab_IFN <- rbind(sample_Hab_IFN, Hab_IFN_histo[IDP%in%IDP_s])
#        sample_Hab_IFN <- rbind(sample_Hab_IFN, Hab_IFN_histo[SER==SERl & FORET_EM==EM_s])
#      }
#  rich_hab_SER_f <- rbind(rich_hab_SER_f, sample_Hab_IFN[HCI == "YES", .(Nb_obs_HIC = .N, iteration = i), by = .(EUR15_GEN, SER, FORET_EM)])
# 
#  nrow(sample_Hab_IFN)#59 696
#  }
# 
# #On calcule la moyenne d'observation de chaque HIC  par SER et type d'usage
# rich_hab_moy_SER <- rich_hab_SER_f[,.(Nb_obs_moy = round(mean(Nb_obs_HIC))), by = .(EUR15_GEN, SER, FORET_EM)]
# 
# #Si on ne ré-échantillonne pas pour avoir un nombre de relevé équivalents : faire tourner la ligne ci-dessous.
# #rich_hab_SER <- Hab_IFN_histo[HCI == "YES", .(Nb_obs_HIC = .N), by = .(EUR15_GEN, SER, FORET_EM)][order(SER, EUR15_GEN)]
# 
# # Si on ré-échantillonne :
# rich_hab_SER <- rich_hab_moy_SER[, ":="(shannon = diversity(Nb_obs_moy, index = "shannon"), simpson = diversity(Nb_obs_moy, index = "simpson")), by = .(SER, FORET_EM)]
# rich_hab_SER[, FORET_EM1:= FORET_EM]
# rich_hab_SER[, FORET_EM := ifelse(FORET_EM1==1, "FA", "FR")]
# rich_hab_SER[, rich_hab := length(unique(EUR15_GEN)), by = .(SER, FORET_EM)]
# rich_hab_SER[, Pielou := shannon/log(rich_hab), by = .(SER, FORET_EM)]
# #Pielou s'il y a du NA à cause de la F13 qui est n'a que 2 FA
# any(is.na(rich_hab_SER$Pielou))#il y a des NA
# # rich_hab_SER[which(is.na(rich_hab_SER$Pielou))]
# #corrélation des indices
# cor(rich_hab_SER[,.(shannon, simpson, rich_hab, Pielou)])
# 


#CI-DESSOUS : faire tourner pour ré-échantilloner la carte nationale, technique 2 : moyenne des indices

#initialisation 
set.seed(12) #reproductibilité
R=1000 #nb rep

#merge pour avoir les infos d'où et combien échantilloner directement sur Hab_IFN_histo pour le tirage dedans
Hab_IFN_histo2 <- merge(Hab_IFN_histo,Nb_FA_FR_perSER[,.(SER,minFAFR,max_EM)],by="SER",all.x=TRUE)
#Garder que l'utilisation histo dans lequel il faut sous-éch (pour lequel on a le max de placette)
Hab_IFN_histo2 <- Hab_IFN_histo2[FORET_EM==max_EM]

#sample R fois dans Hab_IFN_histo le nombre d'éch qui correspond au minimum de placette/SER entre FA et FR
Hab_IFN_histo_samp <- Hab_IFN_histo2[, 
    rbindlist(
    lapply(seq_len(R), function(r) .SD[sample(.N,unique(.SD$minFAFR), replace = FALSE)][, rep := r]
    )), by=SER]

#création d'un etable intérmédiaire avec le nombre de chaque type d'HIC par SER et itération (rep)
int <- Hab_IFN_histo_samp[HCI=="YES",.(N=.N),by=.(EUR15_GEN, SER, rep, FORET_EM)]

mean_indices_rep <- int[,.(FORET_EM=unique(FORET_EM), rich=length(unique(EUR15_GEN)), shannon=diversity(N, index = "shannon"),
                              simpson=diversity(N, index = "simpson"),
                              hill_2=diversity(N, index = "invsimpson")),by=.(SER,rep)]
mean_indices_rep[,":="(hill_1=exp(shannon), pielou=shannon/log(rich))][,hill_eq:=hill_1/rich]



# for(i in c(1:1000)){
#   #i <- 1
#   print(i);flush.console()
#   
#   #initialisation du tableur boucle 2
#   mean_indices <- data.table()
#   
#   
#   #boucle de remplissage du data.table avec sample correspondant au minimum de points.
#   #for(SERl in Nb_FA_FR_perSER$SER){
#     SERl <- "F13"
#     
#     dims <- Nb_FA_FR_perSER[SER==SERl]$minFAFR
#     EM_s <- Nb_FA_FR_perSER[SER==SERl]$max_EM
#     
#     IDP_s <- sample(Hab_IFN_histo[SER==SERl & FORET_EM==EM_s]$IDP, dims, replace = FALSE)
#     int <- Hab_IFN_histo[IDP%in%IDP_s & HCI == "YES", .(Num = .N), by = .(EUR15_GEN)]
#     indices <- Hab_IFN_histo[,.(SER=SERl, EM=EM_s, rich=length(int$EUR15_GEN), shannon = diversity(int$Num, index="shannon"),
#                                 simpson = diversity(int$Num, index="simpson"))]
#     indices <- indices[,pielou := shannon/log(rich)]
#     indices <- indices[,":="(hill_1 = exp(shannon), hill_2=diversity(int$Num, index="invsimpson"))][,hill_eq:=hill_1/rich]
#     mean_indices <- rbind(mean_indices, indices)
#     
#     if(indices$hill_2==Inf)break()
#     
#   #}
# mean_indices_rep <- rbind(mean_indices_rep, mean_indices[,iteration := i])
#     S
# }

mean_indices_rep

table(mean_indices_rep[pielou==Inf]$SER)
##ATTENTION F13, F12, G60 et I13 Pielou = Inf
#Car log(1)= 0 et on a souvent rich HIC = 1
#On choisit donc de pas prendre les Inf dans le calcul de Piélou. 
#Une explication : un faible nombre de plots
Nb_FA_FR_perSER[minFAFR<30]#C'est le cas de F13 et I13, qu'on peut juste ne pas faire apparaître car calculer 
#une richesse d'habitat pour moins 27 (I13), voir 2 (F13) placette, ça n'a pas grand chance, c'est beaucoup de
#hasard. 
#L'autre explication, c'est qu'il y a peu de HIC dans ces zones, par exemple pour G60 (Grands Causses) on est en
#supramed et j'ai déjà remarqué que certaines associations de sortoies sont classées en HIC dans les cahiers d'hab
#Mais pas par le programme.
#C'est le cas notamment pour les 9230, 9260, 9340.

#mean_indices_rep2 <- mean_indices_rep[,pielou_corr := ifelse(pielou==Inf, NA, pielou)]

#On calcule la moyenne d'observation de chaque HIC  par SER
mean_indices_SER <- mean_indices_rep[, .(av_rich=round(mean(rich)), av_shannon=mean(shannon),
                                                               av_simpson=mean(simpson), 
                                                               av_pielou=mean(pielou, na.rm=T),
                                         av_hill1=mean(hill_1), av_hill2=mean(hill_2, na.rm=T), 
                                         av_hill_eq=mean(hill_eq, na.rm=T)), by = .(SER, FORET_EM)]
 


#On rajoute ensuite les calculs des indices pour les utilisation avec le max de plots
int_SER_EM <- Hab_IFN_histo[HCI=="YES", .(Num = .N), by = .(SER, EUR15_GEN, FORET_EM)]
Nb_FA_FR_perSER[,SER_min_EM := paste0(SER, min_EM)] # Ca marche si jamais
int_SER_EM[, rich := length(unique(EUR15_GEN)), by = .(SER, FORET_EM)]
indices_SER_tot_sans <- int_SER_EM[,.(av_rich=unique(rich), av_shannon = diversity(Num, index="shannon"),
                                   av_simpson = diversity(Num, index="simpson"),
                                   av_hill2=diversity(Num, index = "invsimpson")), by=.(SER, FORET_EM)]
indices_SER_tot_sans <- indices_SER_tot_sans[,":="(av_pielou=av_shannon/log(av_rich),
                                                             av_hill1=exp(av_shannon))]
indices_SER_tot_sans <- indices_SER_tot_sans[,av_hill_eq := av_hill1/av_rich]
indices_SER_tot_sans[, SER_FORET_EM := paste0(SER, FORET_EM)]

indices_SER_min <- indices_SER_tot_sans[SER_FORET_EM %in% Nb_FA_FR_perSER$SER_min_EM][,SER_FORET_EM:=NULL]
#on recolle les 2 tableaux ensembles
mean_indices_SER_tot <- rbind(mean_indices_SER, indices_SER_min)
mean_indices_SER_tot[,FORET_EM:=ifelse(FORET_EM==0, "FR", "FA")]



#On regarde les corrélations entre les indices
cor(mean_indices_SER_tot[,.(av_rich, av_shannon, av_simpson, av_pielou, av_hill1, av_hill2, av_hill_eq)])

#check des indices calculés
# A11FA <- c(2, 231, 21, 4, 4, 10, 3)
# A11FA_2 <- c(2, 231, 21, 0, 4, 0, 0, 4, 10, 0, 0, 3)
# A11FR <- c(207,140, 1, 86, 15, 1, 66, 134, 1, 1, 25)
#
# diversity(A11FR, index = "simpson") #OK!

#####################################################################################################################
################### Carte indices

#tab des indices de diversité
ind_div_hab <- unique(mean_indices_SER_tot[,.(SER, FORET_EM, av_shannon, av_simpson, av_rich, av_pielou, av_hill1, av_hill2, av_hill_eq)])
#passer en format large
ind_div_hab_w <- dcast(ind_div_hab, SER ~ FORET_EM, value.var = c("av_shannon", "av_simpson",
                                                                  "av_rich", "av_pielou",
                                                                  "av_hill1", "av_hill2",
                                                                  "av_hill_eq"))
#calcul pour chaque indice FA-FR
ind_div_hab_w[, ":="(diff_shannon = av_shannon_FA - av_shannon_FR, diff_simpson = av_simpson_FA - av_simpson_FR,
                     diff_rich = av_rich_FA - av_rich_FR, diff_pielou = av_pielou_FA - av_pielou_FR,
                     diff_hill1 = av_hill1_FA - av_hill1_FR, diff_hill2 = av_hill2_FA - av_hill2_FR,
                     diff_hill_eq = av_hill_eq_FA - av_hill_eq_FR)]

#Choix de la gamme de couleur
#shannon, pielou, simpson
col7 <- c("#be8a00","#f1b000", "#ffe08b", "#CCCCCC", "#8ba9ff", "#3f71ff" , "#0b4cff")
#hill et richesse
col_4_2 <- c("#8b6600", "#be8a00", "#f1b000", "#ffe08b", "#CCCCCC", "#8ba9ff", "#3f71ff")


##Shannon ####

#Regarder le nb de régions
nrow(ind_div_hab_w[diff_shannon<0])#66/76 Ont un indice de shannon plus élevé en FR
nrow(ind_div_hab_w[diff_shannon>0])#10/76 Ont un indice de shannon plus élevé en FA
nrow(ind_div_hab_w[diff_shannon==0])#0/76


#faire des classes pour le plot en discret
shannon <- ind_div_hab_w[diff_shannon < -1.0, diff_class := "<-1"]
shannon <- ind_div_hab_w[diff_shannon < -0.5 & diff_shannon >= -1, diff_class := "-1:-0.5"]
shannon <- ind_div_hab_w[diff_shannon < 0 & diff_shannon >= -0.5, diff_class := "-0.5:0"]
shannon <- ind_div_hab_w[diff_shannon ==0, diff_class := "0"]
shannon <- ind_div_hab_w[diff_shannon > 0 & diff_shannon <= 0.5, diff_class := "0:0.5"]
shannon <- ind_div_hab_w[diff_shannon > 0.5 & diff_shannon <= 1, diff_class := "0.5:1"]
shannon <- ind_div_hab_w[diff_shannon > 1, diff_shannon := ">1"]

shannon <- shannon[,diff_shannon:=as.factor(diff_shannon)][,.(SER, diff_shannon, diff_class)]

reorder <- c("<-1", "-1:-0.5", "-0.5:0", "0", "0:0.5", "0.5:1", ">1")

#plot
map_ind_div_shannon <- merge(map_ser, shannon, by = "SER", all=TRUE)

div_shannon <- ggplot(data=map_ind_div_shannon)+geom_sf()+aes(fill=diff_class)+
  theme(axis.text = element_blank(),rect = element_blank(),axis.ticks = element_blank())+
  labs(fill="AF - RF")+  
  scale_fill_manual(na.value="white", breaks = reorder,values = col7)+
  theme(legend.position = c(0.94, 0.5),
        legend.title = element_text(size = 11), legend.text = element_text(size = 11))

## simpson ####

#Regarder le nb de régions
nrow(ind_div_hab_w[diff_simpson<0])#61/76 Ont un indice plus élevé en FR
nrow(ind_div_hab_w[diff_simpson>0])#15/76 Ont un indice plus élevé en FA
nrow(ind_div_hab_w[diff_simpson==0])#0/76

#faire des classes pour le plot en discret
simpson <- ind_div_hab_w[diff_simpson < -0.6, diff_class := "<-0.6"]
simpson <- ind_div_hab_w[diff_simpson < -0.3 & diff_simpson >= -0.6, diff_class := "-0.6:-0.3"]
simpson <- ind_div_hab_w[diff_simpson < 0 & diff_simpson >= -0.3, diff_class := "-0.3:0"]
simpson <- ind_div_hab_w[diff_simpson ==0, diff_class := "0"]
simpson <- ind_div_hab_w[diff_simpson > 0 & diff_simpson <= 0.3, diff_class := "0:0.3"]
simpson <- ind_div_hab_w[diff_simpson > 0.3 & diff_simpson <= 0.6, diff_class := "0.3:0.6"]
simpson <- ind_div_hab_w[diff_simpson > 0.6, diff_simpson := ">0.6"]

simpson <- simpson[,diff_simpson:=as.factor(diff_simpson)][,.(SER, diff_simpson, diff_class)]

reorder <- c("<-0.6", "-0.6:-0.3", "-0.3:0", "0", "0:0.3", "0.3:0.6", ">0.6")

#plot
map_ind_div_simpson <- merge(map_ser, simpson, by = "SER", all=TRUE)

div_simpson <- ggplot(data=map_ind_div_simpson)+geom_sf()+aes(fill=diff_class)+
  theme(axis.text = element_blank(),rect = element_blank(),axis.ticks = element_blank())+
  labs(fill="AF - RF")+  
  scale_fill_manual(na.value="white", breaks = reorder,values = col7)+
  theme(legend.position = c(0.96, 0.5),
        legend.title = element_text(size = 11), legend.text = element_text(size = 11))

## richesse ####

#Regarder le nb de régions
nrow(ind_div_hab_w[diff_rich<0])#56/76 Ont un indice plus élevé en FR
nrow(ind_div_hab_w[diff_rich>0])#6/76 Ont un indice plus élevé en FA
nrow(ind_div_hab_w[diff_rich==0])#14/76

#faire des classes pour le plot en discret
rich <- ind_div_hab_w[diff_rich < -6, diff_class := "<-6"]
rich <- ind_div_hab_w[diff_rich < -4 & diff_rich >= -6, diff_class := "-6:-4"]
rich <- ind_div_hab_w[diff_rich < -2 & diff_rich >= -4, diff_class := "-4:-2"]
rich <- ind_div_hab_w[diff_rich < 0 & diff_rich >= -2, diff_class := "-2:0"]
rich <- ind_div_hab_w[diff_rich ==0, diff_class := "0"]
rich <- ind_div_hab_w[diff_rich > 0 & diff_rich <= 2, diff_class := "0:2"]
rich <- ind_div_hab_w[diff_rich > 2, diff_rich := ">2"]

rich <- rich[,diff_rich:=as.factor(diff_rich)][,.(SER, diff_rich, diff_class)]

reorder <- c("<-6", "-6:-4", "-4:-2", "-2:0","0", "0:2", ">2")

#plot
map_ind_div_rich <- merge(map_ser, rich, by = "SER", all=TRUE)

div_rich <- ggplot(data=map_ind_div_rich)+geom_sf()+aes(fill=diff_class)+
  theme(axis.text = element_blank(),rect = element_blank(),axis.ticks = element_blank())+
  labs(fill="AF - RF")+  
  scale_fill_manual(na.value="white", breaks = reorder,values = col_4_2)+
  theme(legend.position = c(0.94, 0.5),
        legend.title = element_text(size = 11), legend.text = element_text(size = 11))

## pielou ####

#Regarder le nb de régions
nrow(ind_div_hab_w[diff_pielou<0])#51/76 Ont un indice plus élevé en FR
nrow(ind_div_hab_w[diff_pielou>0])#24/76 Ont un indice plus élevé en FA
nrow(ind_div_hab_w[diff_pielou==0])#1/76

#faire des classes pour le plot en discret
pielou <- ind_div_hab_w[diff_pielou < -0.4, diff_class := "<-0.4"]
pielou <- ind_div_hab_w[diff_pielou < -0.2 & diff_pielou >= -0.4, diff_class := "-0.4:-0.2"]
pielou <- ind_div_hab_w[diff_pielou < 0 & diff_pielou >= -0.2, diff_class := "-0.2:0"]
pielou <- ind_div_hab_w[diff_pielou ==0, diff_class := "0"]
pielou <- ind_div_hab_w[diff_pielou > 0 & diff_pielou <= 0.2, diff_class := "0:0.2"]
pielou <- ind_div_hab_w[diff_pielou > 0.2 & diff_pielou <= 0.4, diff_class := "0.2:0.4"]
pielou <- ind_div_hab_w[diff_pielou > 0.4, diff_pielou := ">0.4"]

pielou <- pielou[,diff_pielou:=as.factor(diff_pielou)][,.(SER, diff_pielou, diff_class)]

reorder <- c("<-0.4", "-0.4:-0.2", "-0.2:0", "0", "0:0.2", "0.2:0.4", ">0.4")

#plot
map_ind_div_pielou <- merge(map_ser, pielou, by = "SER", all=TRUE)

div_pielou <- ggplot(data=map_ind_div_pielou)+geom_sf()+aes(fill=diff_class)+
  theme(axis.text = element_blank(),rect = element_blank(),axis.ticks = element_blank())+
  labs(fill="AF - RF")+  
  scale_fill_manual(na.value="white", breaks = reorder,values = col7)+
  theme(legend.position = c(0.96, 0.5),
        legend.title = element_text(size = 11), legend.text = element_text(size = 11))

plot_grid(div_shannon, div_simpson, div_rich, div_pielou, ncol = 2, nrow = 2,
          labels = c("a) shannon", "b) simpson", "c) richness", "d) pielou"))
#####################################################################################################

## hill1 ####

#Regarder le nb de régions
nrow(ind_div_hab_w[diff_hill1<0])#67/76 Ont un indice plus élevé en FR
nrow(ind_div_hab_w[diff_hill1>0])#9/76 Ont un indice plus élevé en FA
nrow(ind_div_hab_w[diff_hill1==0])#0/76

#faire des classes pour le plot en discret
hill1 <- ind_div_hab_w[diff_hill1 < -6, diff_class := "<-6"]
hill1 <- ind_div_hab_w[diff_hill1 < -4 & diff_hill1 >= -6, diff_class := "-6:-4"]
hill1 <- ind_div_hab_w[diff_hill1 < -2 & diff_hill1 >= -4, diff_class := "-4:-2"]
hill1 <- ind_div_hab_w[diff_hill1 < 0 & diff_hill1 >= -2, diff_class := "-2:0"]
hill1 <- ind_div_hab_w[diff_hill1 ==0, diff_class := "0"]
hill1 <- ind_div_hab_w[diff_hill1 > 0 & diff_hill1 <= 2, diff_class := "0:2"]
hill1 <- ind_div_hab_w[diff_hill1 > 2, diff_hill1 := ">2"]

hill1 <- hill1[,diff_hill1:=as.factor(diff_hill1)][,.(SER, diff_hill1, diff_class)]

reorder <- c("<-6", "-6:-4", "-4:-2", "-2:0","0", "0:2", ">2")
#plot
map_ind_div_hill1 <- merge(map_ser, hill1, by = "SER", all=TRUE)

div_hill1 <- ggplot(data=map_ind_div_hill1)+geom_sf()+aes(fill=diff_class)+
  theme(axis.text = element_blank(),rect = element_blank(),axis.ticks = element_blank())+
  labs(fill="AF - RF")+  
  scale_fill_manual(na.value="white", breaks = reorder,values = col_4_2)+
  theme(legend.position = c(0.94, 0.5),
        legend.title = element_text(size = 16), legend.text = element_text(size = 14))

## hill2 ####

#Regarder le nb de régions
nrow(ind_div_hab_w[diff_hill2<0])#62/76 Ont un indice plus élevé en FR
nrow(ind_div_hab_w[diff_hill2>0])#14/76 Ont un indice plus élevé en FA
nrow(ind_div_hab_w[diff_hill2==0])#0/76

#faire des classes pour le plot en discret
hill2 <- ind_div_hab_w[diff_hill2 < -6, diff_class := "<-6"]
hill2 <- ind_div_hab_w[diff_hill2 < -4 & diff_hill2 >= -6, diff_class := "-6:-4"]
hill2 <- ind_div_hab_w[diff_hill2 < -2 & diff_hill2 >= -4, diff_class := "-4:-2"]
hill2 <- ind_div_hab_w[diff_hill2 < 0 & diff_hill2 >= -2, diff_class := "-2:0"]
hill2 <- ind_div_hab_w[diff_hill2 ==0, diff_class := "0"]
hill2 <- ind_div_hab_w[diff_hill2 > 0 & diff_hill2 <= 2, diff_class := "0:2"]
hill2 <- ind_div_hab_w[diff_hill2 > 2, diff_hill2 := ">2"]

hill2 <- hill2[,diff_hill2:=as.factor(diff_hill2)][,.(SER, diff_hill2, diff_class)]

reorder <- c("<-6", "-6:-4", "-4:-2", "-2:0","0", "0:2", ">2")

#plot
map_ind_div_hill2 <- merge(map_ser, hill2, by = "SER", all=TRUE)

div_hill2 <- ggplot(data=map_ind_div_hill2)+geom_sf()+aes(fill=diff_class)+
  theme(axis.text = element_blank(),rect = element_blank(),axis.ticks = element_blank())+
  labs(fill="AF - RF")+  
  scale_fill_manual(na.value="white", breaks = reorder,values = col_4_2)+
  theme(legend.position = c(0.94, 0.5),
        legend.title = element_text(size = 16), legend.text = element_text(size = 14))

## hill eq ####

#Regarder le nb de régions
nrow(ind_div_hab_w[diff_hill_eq<0])#36/76 Ont un indice plus élevé en FR
nrow(ind_div_hab_w[diff_hill_eq>0])#39/76 Ont un indice plus élevé en FA
nrow(ind_div_hab_w[diff_hill_eq==0])#1/76

#faire des classes pour le plot en discret
hill_eq <- ind_div_hab_w[diff_hill_eq < -0.6, diff_class := "<-0.6"]
hill_eq <- ind_div_hab_w[diff_hill_eq < -0.3 & diff_hill_eq >= -0.6, diff_class := "-0.6:-0.3"]
hill_eq <- ind_div_hab_w[diff_hill_eq < 0 & diff_hill_eq >= -0.3, diff_class := "-0.3:0"]
hill_eq <- ind_div_hab_w[diff_hill_eq ==0, diff_class := "0"]
hill_eq <- ind_div_hab_w[diff_hill_eq > 0 & diff_hill_eq <= 0.3, diff_class := "0:0.3"]
hill_eq <- ind_div_hab_w[diff_hill_eq > 0.3 & diff_hill_eq <= 0.6, diff_class := "0.3:0.6"]
hill_eq <- ind_div_hab_w[diff_hill_eq > 6, diff_hill_eq := ">0.6"]

hill_eq <- hill_eq[,diff_hill_eq:=as.factor(diff_hill_eq)][,.(SER, diff_hill_eq, diff_class)]

reorder <- c("<-0.6", "-0.6:-0.3", "-0.3:0", "0", "0:0.3", "0.3:0.6", ">0.6")

#plot
map_ind_div_hill_eq <- merge(map_ser, hill_eq, by = "SER", all=TRUE)

div_hill_eq <- ggplot(data=map_ind_div_hill_eq)+geom_sf()+aes(fill=diff_class)+
  theme(axis.text = element_blank(),rect = element_blank(),axis.ticks = element_blank())+
  labs(fill="AF - RF")+  
  scale_fill_manual(na.value="white", breaks = reorder,values = col7)+
  theme(legend.position = c(0.94, 0.5),
        legend.title = element_text(size = 16), legend.text = element_text(size = 14))

###################################################################################################


# #continue
# map_ind_div_shannon <- merge(map_ser, ind_div_habw_shannon, by = "SER", all=TRUE)
# 
# ggplot(data=map_ind_div_shannon)+geom_sf()+aes(fill=diff_FA_FR)+
#   theme(axis.text = element_blank(),rect = element_blank(),axis.ticks = element_blank())+
#   labs(fill="difference FA vs FR")+
#   #scale_fill_continuous(na.value="white",label = function(x) sprintf("%.2f", x),trans="reverse")+
#   scale_fill_viridis_c(na.value="white",option = "plasma")+
#   theme(legend.position = c(0.94, 0.6))+
#   #geom_sf(data=map_ind_div_shannon[map_ind_div_shannon$diff_FA_FR>0,],fill="gold1")+
#   ggtitle("shannon")

# #diversité d'habitat échelle nationale #######
# #Création d'un data.table avec un comptage par type de forêt, par SER et par type d'hab
# mean_indices_SER_tot_nat <- Hab_IFN_histo[HCI == "YES", .(Nb_obs_HIC = .N), by = .(EUR15_GEN, SER)]
# mean_indices_SER_tot_nat[, ":="(shannon = diversity(Nb_obs_HIC, index = "shannon"), simpson = diversity(Nb_obs_HIC, index = "simpson")), by = .(SER)]
# mean_indices_SER_tot_nat[, rich_hab := length(unique(EUR15_GEN)), by = .(SER)]
# mean_indices_SER_tot_nat[, Pielou := shannon/log(rich_hab), by = .(SER)]
# 
# map_shannon_nat <- merge(map_ser, mean_indices_SER_tot_nat, by = "SER", all=TRUE)
# 
# ggplot(data=map_shannon_nat)+geom_sf()+aes(fill=shannon)+
#   theme(axis.text = element_blank(),rect = element_blank(),axis.ticks = element_blank())+
#   labs(fill="shannon index")+
#   #scale_fill_continuous(na.value="white",label = function(x) sprintf("%.2f", x),trans="reverse")+
#   scale_fill_viridis_c(na.value="white",option = "plasma", trans="reverse")+
#   theme(legend.position = c(0.94, 0.6))+
#   #geom_sf(data=map_ind_div_shannon[map_ind_div_shannon$diff_FA_FR>0,],fill="gold1")+
#   ggtitle("indice de shannon par SER tous types ancienneté")
# 
# ggplot(data=map_shannon_nat)+geom_sf()+aes(fill=rich_hab)+
#   theme(axis.text = element_blank(),rect = element_blank(),axis.ticks = element_blank())+
#   labs(fill="richesse hab")+
#   #scale_fill_continuous(na.value="white",label = function(x) sprintf("%.2f", x),trans="reverse")+
#   scale_fill_viridis_c(na.value="white",option = "plasma", trans="reverse")+
#   theme(legend.position = c(0.94, 0.6))+
#   #geom_sf(data=map_ind_div_shannon[map_ind_div_shannon$diff_FA_FR>0,],fill="gold1")+
#   ggtitle("richesse habitat par SER tous types ancienneté")
# 
# ggplot(data=map_shannon_nat)+geom_sf()+aes(fill=Pielou)+
#   theme(axis.text = element_blank(),rect = element_blank(),axis.ticks = element_blank())+
#   labs(fill="Pielou index")+
#   #scale_fill_continuous(na.value="white",label = function(x) sprintf("%.2f", x),trans="reverse")+
#   scale_fill_viridis_c(na.value="white",option = "plasma", trans="reverse")+
#   theme(legend.position = c(0.94, 0.6))+
#   #geom_sf(data=map_ind_div_shannon[map_ind_div_shannon$diff_FA_FR>0,],fill="gold1")+
#   ggtitle("indice de piélou par SER tous types ancienneté")

################### Carte richesse hab
#passer en format large
ind_div_habw_rich_hab <- dcast(ind_div_hab, SER ~ FORET_EM, value.var = "av_rich")

#calcul de la différence entre le % HIC en FA et % HIC en FR
ind_div_habw_rich_hab[,diff_FA_FR := FA-FR]

#plot continue
# map_ind_div_rich_hab <- merge(map_ser, ind_div_habw_rich_hab, by = "SER", all=TRUE)
# 
# ggplot(data=map_ind_div_rich_hab)+geom_sf()+aes(fill=diff_FA_FR)+
#   theme(axis.text = element_blank(),rect = element_blank(),axis.ticks = element_blank())+
#   labs(fill="difference hab richness FA vs FR")+
#   scale_fill_continuous(na.value="white",label = function(x) sprintf("%.2f", x),trans="reverse")+
#   theme(legend.position = c(0.94, 0.6))+
#   geom_sf(data=map_ind_div_rich_hab[map_ind_div_rich_hab$diff_FA_FR>0,],fill="gold1")+
#   ggtitle("habitat richness")

#plot discret
#faire des classes pour le plot en discret
ind_div_habw_rich_hab[diff_FA_FR < -6, diff_class := "-8:-6"]
ind_div_habw_rich_hab[diff_FA_FR < -4 & diff_FA_FR >= -6, diff_class := "-6:-4"]
ind_div_habw_rich_hab[diff_FA_FR < -2 & diff_FA_FR >= -4, diff_class := "-4:-2"]
ind_div_habw_rich_hab[diff_FA_FR <= -1 & diff_FA_FR >= -2, diff_class := "-2:-1"]
ind_div_habw_rich_hab[diff_FA_FR == 0, diff_class := "0"]
ind_div_habw_rich_hab[diff_FA_FR < 2 & diff_FA_FR >= 1, diff_class := "1:2"]
ind_div_habw_rich_hab[diff_FA_FR >= 2, diff_class := "2:4"]


#test plot en discret
col7 <- c("#d49900", "#e2a400", "#f0b000", "#febc1c", "#CCCCCC", "#4d86ff", "#0052ff")
reorder <- c("-8:-6", "-6:-4", "-4:-2","-2:-1", "0", "1:2", "2:4")

#plot
map_ind_div_rich_hab <- merge(map_ser, ind_div_habw_rich_hab, by = "SER", all=TRUE)

div_rich_hab <- ggplot(data=map_ind_div_rich_hab)+geom_sf()+aes(fill=diff_class)+
  theme(axis.text = element_blank(),rect = element_blank(),axis.ticks = element_blank())+
  labs(fill="AF - RF")+  
  scale_fill_manual(na.value="white", breaks = reorder, values = col7)+
  theme(legend.position = c(0.94, 0.5),
        legend.title = element_text(size = 16), legend.text = element_text(size = 14))

################### Carte Piélou
#passer en format large
ind_div_habw_pielou <- dcast(ind_div_hab, SER ~ FORET_EM, value.var = "Pielou")

#calcul de la différence entre le % HIC en FA et % HIC en FR
ind_div_habw_pielou[,diff_FA_FR := FA-FR]
nrow(ind_div_habw_pielou[diff_FA_FR<0])#52/76 Ont un indice de piélou plus élevé en FR
nrow(ind_div_habw_pielou[diff_FA_FR>0])#24/76 Ont un indice de piélou plus élevé en FA
nrow(ind_div_habw_pielou[diff_FA_FR==0])#0/76

#plot discret
#faire des classes pour le plot en discret
ind_div_habw_pielou[diff_FA_FR < 0.5, diff_class := "<-0.5"]
ind_div_habw_pielou[diff_FA_FR < -0.35 & diff_FA_FR >= -0.5, diff_class := "-0.5:-0.35"]
ind_div_habw_pielou[diff_FA_FR < -0.20 & diff_FA_FR >= -0.35, diff_class := "-0.35:-0.20"]
ind_div_habw_pielou[diff_FA_FR < 0 & diff_FA_FR >= -0.20, diff_class := "-0.20:0"]
ind_div_habw_pielou[diff_FA_FR > 0 & diff_FA_FR < 0.20, diff_class := "0:0.20"]
ind_div_habw_pielou[diff_FA_FR <= 0.35 & diff_FA_FR >0.20, diff_class := "0.20:0.35"]
ind_div_habw_pielou[diff_FA_FR >0.35, diff_class := "0.35:0.45"]


#test plot en discret
col6 <- c("#d49900", "#e2a400", "#f0b000", "#febc1c", "#4d86ff", "#0052ff")
reorder <- c("<-0.5", "-0.5:-0.35", "-0.35:-0.20", "-0.20:0", "0:0.20", "0.35:0.45")

#plot
map_ind_div_pielou <- merge(map_ser, ind_div_habw_pielou, by = "SER", all=TRUE)

div_pielou <- ggplot(data=map_ind_div_pielou)+geom_sf()+aes(fill=diff_class)+
  theme(axis.text = element_blank(),rect = element_blank(),axis.ticks = element_blank())+
  labs(fill="AF - RF")+  
  scale_fill_manual(na.value="white", breaks = reorder, values = col6)+
  theme(legend.position = c(0.94, 0.5),
        legend.title = element_text(size = 16), legend.text = element_text(size = 14))

############ Carte simpson
#passer en format large
ind_div_habw_simpson <- dcast(ind_div_hab, SER ~ FORET_EM, value.var = "simpson")

#calcul de la différence entre le % HIC en FA et % HIC en FR
ind_div_habw_simpson[,diff_FA_FR := FA-FR]
nrow(ind_div_habw_simpson[diff_FA_FR<0])#63/76 Ont un indice de simpson plus élevé en FR
nrow(ind_div_habw_simpson[diff_FA_FR>0])#13/76 Ont un indice de simpson plus élevé en FA
nrow(ind_div_habw_simpson[diff_FA_FR==0])#0/76

#plot discret
#faire des classes pour le plot en discret
ind_div_habw_simpson[diff_FA_FR < 0.6, diff_class := "<-0.6"]
ind_div_habw_simpson[diff_FA_FR < -0.45 & diff_FA_FR >= -0.6, diff_class := "-0.6:-0.45"]
ind_div_habw_simpson[diff_FA_FR < -0.3 & diff_FA_FR >= -0.45, diff_class := "-0.45:-0.30"]
ind_div_habw_simpson[diff_FA_FR < -0.15 & diff_FA_FR >= -0.3, diff_class := "-0.30:-0.15"]
ind_div_habw_simpson[diff_FA_FR < 0 & diff_FA_FR >= -0.15, diff_class := "-0.15:<0"]
ind_div_habw_simpson[diff_FA_FR <= 0.15 & diff_FA_FR >0, diff_class := "0:0.15"]
ind_div_habw_simpson[diff_FA_FR >0.15, diff_class := ">0.15"]


#test plot en discret
col7 <- c("#c78d00", "#e2a400", "#f0b000", "#febc1c", "#ffc82e", "#4d86ff", "#0052ff")
reorder <- c("<-0.6", "-0.6:-0.45", "-0.45:-0.30", "-0.30:-0.15", "-0.15:<0", "0:0.15", ">0.15")

#plot
map_ind_div_simpson <- merge(map_ser, ind_div_habw_simpson, by = "SER", all=TRUE)

div_simpson <- ggplot(data=map_ind_div_simpson)+geom_sf()+aes(fill=diff_class)+
  theme(axis.text = element_blank(),rect = element_blank(),axis.ticks = element_blank())+
  labs(fill="AF - RF")+  
  scale_fill_manual(na.value="white", breaks = reorder, values = col7)+
  theme(legend.position = c(0.94, 0.5),
        legend.title = element_text(size = 16), legend.text = element_text(size = 14))

########################################################################################################################################################
#Graphs avec 3 cartes 

#Graph surface et 2 indices de diversité
plot_grid(div_shannon, div_simpson, ncol = 2, nrow = 1)
plot_grid(div_shannon, div_simpson, div_rich_hab, div_pielou, ncol = 2, nrow = 2)

#####################################################################################################################################################

ind_div_habw_shannon[,diff_shannon := diff_FA_FR]
ind_div_habw_pielou[,diff_pielou := diff_FA_FR]
ind_div_habw_rich_hab[,diff_rich_hab := diff_FA_FR]
ind_div_habw_simpson[,diff_simpson := diff_FA_FR]
ind_divw <- merge(ind_div_habw_shannon[,.(SER, diff_shannon)], ind_div_habw_pielou[,.(SER, diff_pielou)], by = "SER")
ind_divw <- merge(ind_divw, ind_div_habw_rich_hab[,.(SER, diff_rich_hab)], by = "SER")
ind_divw <- merge(ind_divw, ind_div_habw_simpson[,.(SER, diff_simpson)], by = "SER")
cor(ind_divw[,.(diff_shannon, diff_pielou, diff_rich_hab, diff_simpson)])
#shannon plus lié à Piélou qu'à Richesse d'habitat

Hab_HIC <- Hab_IFN_histo[HCI=="YES"]
prop <- prop.table(table(Hab_HIC$FORET_EM))
prop_FA <- prop[2]
prop_FR <- prop[1]

res <- vector(mode = "numeric")
res <- rep(NA, 100000)

# for(i in 1:20000){
#   print(i); flush.console()
#   tic()
#  Hab_HIC[,FORET_sim := sample(c(0,1), nrow(Hab_HIC), replace = TRUE, prob = c(prop_FR, prop_FA))]
#   prop.table(table(Hab_HIC$FORET_sim))
#   pi_num <- Hab_HIC[,.(pi_num = .N ), by =.(EUR15_GEN, FORET_sim)]
#   pi_denom <- Hab_HIC[,.(pi_denom = .N), by = .(FORET_sim)]
#   pi <- merge(pi_num, pi_denom, by = "FORET_sim", all = TRUE)
#   pi[,pi:= pi_num/pi_denom]
#   shannon <- pi[,.(shannon = -sum(pi * log2(pi))), by = "FORET_sim"]
#   diff_shannon <- shannon[FORET_sim==1]$shannon - shannon[FORET_sim==0]$shannon
# 
#   res[i]=diff_shannon
#   toc()
# }
 mean(res) #pour 100 000 : mean res = 6.849171e-05, sd = 0.01221364
 sd(res)
 hist(res).test(ind_divw$diff_shannon)

t.test(ind_divw$diff_rich_hab)
t.test(ind_divw$diff_pielou)

#tirage pour réattribution des points sans tenir compte de l'histoire
# significativité des indices de div à l'échelle nationale


#par SER
prop.table(table(Hab_HIC$FORET_EM))
pi_num <- Hab_HIC[,.(pi_num = .N ), by =.(EUR15_GEN, FORET_EM, SER)]
pi_denom <- Hab_HIC[,.(pi_denom = .N), by = .(FORET_EM, SER)]
pi <- merge(pi_num, pi_denom, by = c("FORET_EM", "SER"))
pi[,pi:= pi_num/pi_denom]
shannon <- pi[,.(shannon = -sum(pi * log2(pi))), by = c("FORET_EM", "SER")]
diff_shannon <- merge(shannon[FORET_EM==0], shannon[FORET_EM==1], by = "SER", all = TRUE)
diff_shannon[,diff_shannon := shannon.y- shannon.x]

#France entière
pi_num <- Hab_HIC[,.(pi_num = .N ), by =.(EUR15_GEN, FORET_EM)]
pi_denom <- Hab_HIC[,.(pi_denom = .N), by = .(FORET_EM)]
pi <- merge(pi_num, pi_denom, by = "FORET_EM", all = TRUE)
pi[,pi:= pi_num/pi_denom]
shannon <- pi[,.(shannon = -sum(pi * log2(pi))), by = "FORET_EM"]
(diff_shannon <- shannon[FORET_EM==1]$shannon - shannon[FORET_EM==0]$shannon)

################################################################################################
# setwd("~/These/data/volet1")
# fwrite(ind_div_hab, "indices_div_SER_FORET_EM.csv", dec=".")
