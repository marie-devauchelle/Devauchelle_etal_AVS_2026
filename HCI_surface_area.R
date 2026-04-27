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
#POURCENTAGE HIC#

#lecture de l'estimation des poids surfaciques par SER
setwd("~/These/data/volet1")
poids_surf_ser <- fread("poids_SER.txt")

#On crée une colonne pour indiquer si c'est un HIC ou pas
Hab_IFN_histo[!is.na(EUR15_GEN), HCI := "YES"][EUR15_GEN == "", HCI := "NO"]

#On merge avec les infos de poids surfacique
nrow(Hab_IFN_histo) #90746
Hab_IFN_histo <- merge(Hab_IFN_histo, poids_surf_ser[,.(SER, nbPlacSER, poids_ha_par_placette)], all.x = TRUE, by = "SER")
nrow(Hab_IFN_histo) #90746 : OK

#Nb de placette par habitat en FA et FR et dans chaque SER
Nb_placette_hab <- Hab_IFN_histo[,.(Nb_placette = .N), by = .(SER, FORET_EM, EUR15_GEN)][order(EUR15_GEN, SER, FORET_EM)]
Nb_placette_hab[EUR15_GEN=="", habitat := "NON-HIC"][EUR15_GEN=="9110", habitat := "Hêtraies du Luzulo-Fagetum"][
  EUR15_GEN=="9120", habitat := "Hêtraies acidophiles atlantiques"][EUR15_GEN=="9130", habitat :="Hêtraies du Asperulo-Fagetum"][
  EUR15_GEN=="9140", habitat :="Hêtraies subalpines"][EUR15_GEN=="9150", habitat :="Hêtraies calcicoles"][
  EUR15_GEN=="9160", habitat :="Chênaies pédonculées"][EUR15_GEN=="9170", habitat :="Chênaies-charmaies"][
  EUR15_GEN=="9180*", habitat := "Forêts de pentes"][EUR15_GEN=="9190", habitat := "Chênaies acidophiles à Quercus robur"][
  EUR15_GEN=="91D0*", habitat :="Tourbières boisées"][EUR15_GEN=="91E0*", habitat :="Forêts alluviales"][EUR15_GEN=="91F0", habitat :="Forêts des grands fleuves"][
  EUR15_GEN=="9230", habitat :="Chenaies aquitaines"][EUR15_GEN=="92A0", habitat :="Forêts galeries méditerranéennes"][
  EUR15_GEN=="9410", habitat :="pessières montagnardes"][EUR15_GEN=="9420", habitat :="Forêts alpines"][EUR15_GEN=="9430*", habitat :="Pinèdes à crochets"][
  EUR15_GEN=="9540", habitat :="Pinèdes méditerranéennes"][EUR15_GEN=="9560*", habitat :="Thuriferaies"]

#Meme chose mais juste diff FA/ FR
placette_utilisation <- Hab_IFN_histo[,.(Nb_placette = .N), by = .(FORET_EM, EUR15_GEN)][order(EUR15_GEN, FORET_EM)]
placette_utilisation[EUR15_GEN=="", habitat := "NON-HIC"][EUR15_GEN=="9110", habitat := "Hêtraies du Luzulo-Fagetum"][
  EUR15_GEN=="9120", habitat := "Hêtraies acidophiles atlantiques"][EUR15_GEN=="9130", habitat :="Hêtraies du Asperulo-Fagetum"][
    EUR15_GEN=="9140", habitat :="Hêtraies subalpines"][EUR15_GEN=="9150", habitat :="Hêtraies calcicoles"][
      EUR15_GEN=="9160", habitat :="Chênaies pédonculées"][EUR15_GEN=="9170", habitat :="Chênaies-charmaies"][
        EUR15_GEN=="9180*", habitat := "Forêts de pentes"][EUR15_GEN=="9190", habitat := "Chênaies acidophiles à Quercus robur"][
          EUR15_GEN=="91D0*", habitat :="Tourbières boisées"][EUR15_GEN=="91E0*", habitat :="Forêts alluviales"][EUR15_GEN=="91F0", habitat :="Forêts des grands fleuves"][
            EUR15_GEN=="9230", habitat :="Chenaies aquitaines"][EUR15_GEN=="92A0", habitat :="Forêts galeries méditerranéennes"][
              EUR15_GEN=="9410", habitat :="pessières montagnardes"][EUR15_GEN=="9420", habitat :="Forêts alpines"][EUR15_GEN=="9430*", habitat :="Pinèdes à crochets"][
                EUR15_GEN=="9540", habitat :="Pinèdes méditerranéennes"][EUR15_GEN=="9560*", habitat :="Thuriferaies"]

placette_utilisation_w <- dcast(placette_utilisation, habitat ~ FORET_EM, value.var = "Nb_placette")

# setwd("~/These/data/volet1")
# fwrite(placette_utilisation_w, "nb_placettes_FA_vs_FR_2005-2023.csv", sep=";")

Nb_placette_hab_HIC <- Hab_IFN_histo[, .(Nb_plac = .N), by = .(EUR15_GEN)][order(Nb_plac)]
Nb_placette_hab_HIC[EUR15_GEN=="", habitat := "NON-HIC"][EUR15_GEN=="9110", habitat := "Hêtraies du Luzulo-Fagetum"][
  EUR15_GEN=="9120", habitat := "Hêtraies acidophiles atlantiques"][EUR15_GEN=="9130", habitat :="Hêtraies du Asperulo-Fagetum"][
    EUR15_GEN=="9140", habitat :="Hêtraies subalpines"][EUR15_GEN=="9150", habitat :="Hêtraies calcicoles"][
      EUR15_GEN=="9160", habitat :="Chênaies pédonculées"][EUR15_GEN=="9170", habitat :="Chênaies-charmaies"][
        EUR15_GEN=="9180*", habitat := "Forêts de pentes"][EUR15_GEN=="9190", habitat := "Chênaies acidophiles à Quercus robur"][
          EUR15_GEN=="91D0*", habitat :="Tourbières boisées"][EUR15_GEN=="91E0*", habitat :="Forêts alluviales"][EUR15_GEN=="91F0", habitat :="Forêts des grands fleuves"][
            EUR15_GEN=="9230", habitat :="Chenaies aquitaines"][EUR15_GEN=="92A0", habitat :="Forêts galeries méditerranéennes"][
              EUR15_GEN=="9410", habitat :="pessières montagnardes"][EUR15_GEN=="9420", habitat :="Forêts alpines"][EUR15_GEN=="9430*", habitat :="Pinèdes à crochets"][
                EUR15_GEN=="9540", habitat :="Pinèdes méditerranéennes"][EUR15_GEN=="9560*", habitat :="Thuriferaies"]
Nb_placette_hab_HIC[, perc_rel := round((Nb_plac/nrow(Hab_IFN_histo))*100, digits = 2)]
Nb_placette_hab_HIC[EUR15_GEN!="", perc_rel_HIC := round((Nb_plac/nrow(Hab_IFN_histo[HCI =="YES"]))*100, digits = 2)]

#Création carte pourcentage de surface d'HIC par région 
Nb_HIC_SER <- Hab_IFN_histo[HCI == "YES", .(Nb_HIC_SER = .N), by = .(SER, FORET_EM)][order(SER, FORET_EM)]
Nb_SER <- Hab_IFN_histo[, .(Nb_SER = .N), by = .(SER, FORET_EM)][order(SER, FORET_EM)]
dim(Nb_HIC_SER);dim(Nb_SER)#152; 3
pourc_HIC_FAFR_SER <- merge(Nb_HIC_SER, Nb_SER, by =c("SER", "FORET_EM"))
pourc_HIC_FAFR_SER[, freq := Nb_HIC_SER/Nb_SER]
pourc_HIC_FAFR_SER[,FORET_EM1:=FORET_EM]
table(pourc_HIC_FAFR_SER$FORET_EM)#OK que 2 valeurs 1 ou 0 pour FORET_EM
pourc_HIC_FAFR_SER[,FORET_EM := ifelse(FORET_EM1==1, "FA", "FR")]
#passer en format large
pourc_HIC_FAFR_SER_w <- dcast(pourc_HIC_FAFR_SER, SER ~ FORET_EM, value.var = "freq")

#calcul de la différence entre le % HIC en FA et % HIC en FR
pourc_HIC_FAFR_SER_w[,diff_FA_FR := FA-FR]
nrow(pourc_HIC_FAFR_SER_w[diff_FA_FR<0])#18/76 SER dans lesquelles surf FR HIC > surf FA HIC : 23%
nrow(pourc_HIC_FAFR_SER_w[diff_FA_FR>0])#58/76 SER dans lesquelles surf FA HIC > surf FR HIC : 76%
nrow(pourc_HIC_FAFR_SER_w[diff_FA_FR==0])#0/76

#faire des classes pour le plot en discret
pourc_HIC_FAFR_SER_w[diff_FA_FR < -0.10, diff_class := "-0.15:-0.10"]
pourc_HIC_FAFR_SER_w[diff_FA_FR < -0.05 & diff_FA_FR >= -0.10, diff_class := "-0.10:-0.05"]
pourc_HIC_FAFR_SER_w[diff_FA_FR < -0.00 & diff_FA_FR >= -0.05, diff_class := "-0.05:-0.00"]
pourc_HIC_FAFR_SER_w[diff_FA_FR == 0, diff_class := "0.00"]
pourc_HIC_FAFR_SER_w[diff_FA_FR < 0.05 & diff_FA_FR > 0, diff_class := "0.00:0.05"]
pourc_HIC_FAFR_SER_w[diff_FA_FR < 0.10 & diff_FA_FR >= 0.05, diff_class := "0.05:0.10"]
pourc_HIC_FAFR_SER_w[diff_FA_FR < 0.15 & diff_FA_FR >= 0.10, diff_class := "0.10:0.15"]
pourc_HIC_FAFR_SER_w[diff_FA_FR > 0.15, diff_class := "0.15:0.25"]

#plot
map_pourc_HIC <- merge(map_ser, pourc_HIC_FAFR_SER_w, by = "SER", all=TRUE)
pourc_HIC_FA <- ggplot(data=map_pourc_HIC)+geom_sf()+aes(fill=FA)+
    theme(axis.text = element_blank(),rect = element_blank(),axis.ticks = element_blank())+
    labs(fill="pourcentage HIC")+
    # scale_fill_continuous(na.value="white",label = function(x) sprintf("%.2f", x),trans="reverse")+
    scale_fill_viridis_c(na.value="white",option = "plasma", trans="reverse")+
    theme(legend.position = c(0.97, 0.6))+
    ggtitle("b) % HIC FA")
  #geom_sf(data=map_pourc_HIC[map_pourc_HIC$diff_FA_FR<0,],fill="gold1")

map_pourc_HIC <- merge(map_ser, pourc_HIC_FAFR_SER_w, by = "SER", all=TRUE)
pourc_HIC_FR <- ggplot(data=map_pourc_HIC)+geom_sf()+aes(fill=FR)+
  theme(axis.text = element_blank(),rect = element_blank(),axis.ticks = element_blank())+
  labs(fill="pourcentage HIC FR")+
  # scale_fill_continuous(na.value="white",label = function(x) sprintf("%.2f", x),trans="reverse")+
  scale_fill_viridis_c(na.value="white",option = "plasma", trans="reverse")+
  theme(legend.position = c(0.97, 0.6))+
  ggtitle("c) %HIC FR")

#test plot en discret
col8 <- c("#FFFF66", "#FFFF99", "#FFFFCC", "#CCCCCC", "#0000FF", "#0000CC", "#000099", "#000033")
col8 <- c("#f0b000", "#ffca2e", "#ffe44c", "#CCCCCC", "#0052ff", "#1c44d0", "#2337a2", "#222b77")
map_pourc_HIC <- merge(map_ser, pourc_HIC_FAFR_SER_w, by = "SER", all=TRUE)
reorder <- c("-0.15:-0.10", "-0.10:-0.05", "-0.05:-0.00", "0.00", "0.00:0.05", "0.05:0.10", "0.10:0.15", "0.15:0.25")

surf_HIC <- ggplot(data=map_pourc_HIC)+geom_sf()+aes(fill=diff_class)+
  theme(axis.text = element_blank(),rect = element_blank(),axis.ticks = element_blank())+
  labs(fill="AF - RF")+  
  scale_fill_manual(na.value="white", breaks = reorder,values = col8)+
  theme(legend.position = c(0.94, 0.6))+
  ggtitle("HCI percentage in AF vs RF")

surf_HIC

###################################################################################################                        
#Calcul de pourcentage 
# (perc_HIC_FA <- length(Hab_IFN_histo[HIC=="OUI" & FORET_EM == 1]$IDP)/length(Hab_IFN_histo$IDP)*100) #39 % HIC en FA
# (perc_HIC_FR <- length(Hab_IFN_histo[HIC=="OUI" & FORET_EM == 0]$IDP)/length(Hab_IFN_histo$IDP)*100)  #31 % HIC en FR
# (perc_NON_HIC_FA <- length(Hab_IFN_histo[HIC=="NON" & FORET_EM == 1]$IDP)/length(Hab_IFN_histo$IDP)*100) #11 % de non HIC en FA
# (perc_NON_HIC_FR <- length(Hab_IFN_histo[HIC=="NON" & FORET_EM == 0]$IDP)/length(Hab_IFN_histo$IDP)*100) #19 % de non HIC en FR
# 
# #Sur les habitats HIC : 
# length(Hab_IFN_histo[HIC=="OUI" & FORET_EM == 1]$IDP)/length(Hab_IFN_histo[HIC=="OUI"]$IDP)*100 # 56% des HIC sont en FA
# length(Hab_IFN_histo[HIC=="OUI" & FORET_EM == 0]$IDP)/length(Hab_IFN_histo[HIC=="OUI"]$IDP)*100 # 44 % des HIC sont en FR
# 
# #creation tableur à plotter
# perc <- Hab_IFN_histo[, .(Nb_rel = .N), by = .(FORET_EM, HIC)]
# perc[, perc_tout := (Nb_rel/sum(Nb_rel))*100]
# perc[FORET_EM==1 & HIC== "OUI",lab := "FA_HIC"]
# perc[FORET_EM==1 & HIC== "NON",lab := "FA_NON_HIC"]
# perc[FORET_EM==0 & HIC== "OUI",lab := "FR_HIC"]
# perc[FORET_EM==0 & HIC== "NON",lab := "FR_NON_HIC"]
# 
# #plot camembert
# ggplot(data=perc, aes(x="", y= perc_tout, fill=lab)) +
#   geom_bar(stat = "identity",width = 1,col="grey20") + coord_polar("y") +
#   theme_void() + 
#   geom_text(aes(label = round(perc_tout)),position = position_stack(vjust = 0.5)) +
#   scale_fill_manual(values=c("mediumpurple4", "gray38", "mediumpurple2", "lightgrey")) #autre, hetre, ch_pedonculé, ch_sessile, ch_pubescent
# 
# #même chose mais pour avoir par HIC
# perc_HIC <- perc[, sum := sum(Nb_rel), by = .(HIC)]
# perc_HIC[,perc_HIC := (Nb_rel/sum) * 100]
# 
# perc_HIC_1 <- perc_HIC[HIC=="OUI"]
# perc_HIC_2 <- perc_HIC[HIC=="NON"]
# 
# 
# # #plot camembert HIC
# # ggplot(data=perc_HIC_1, aes(x="", y= perc_HIC, fill=lab)) +
# #   geom_bar(stat = "identity",width = 1,col="grey20") + coord_polar("y") +
# #   theme_void() + 
# #   geom_text(aes(label = round(perc_HIC)),position = position_stack(vjust = 0.5)) +
# #   scale_fill_manual(values=c("mediumpurple4","mediumpurple2")) #autre, hetre, ch_pedonculé, ch_sessile, ch_pubescent
# # 
# # #plot camembert NON HIC
# # ggplot(data=perc_HIC_2, aes(x="", y= perc_HIC, fill=lab)) +
# #   geom_bar(stat = "identity",width = 1,col="grey20") + coord_polar("y") +
# #   theme_void() + 
# #   geom_text(aes(label = round(perc_HIC)),position = position_stack(vjust = 0.5)) +
# #   scale_fill_manual(values=c("gray38","lightgrey")) #autre, hetre, ch_pedonculé, ch_sessile, ch_pubescent
# 
# #pourcentage de FA en HIC contre FA en non-HIC
# perc_FA_HIC <- length(Hab_IFN_histo[HIC=="OUI" & FORET_EM == 1]$IDP)/length(Hab_IFN_histo[FORET_EM == 1]$IDP)*100 #78 % FA est HIC
# perc_FA_non_HIC <- length(Hab_IFN_histo[HIC=="NON" & FORET_EM == 1]$IDP)/length(Hab_IFN_histo[FORET_EM == 1]$IDP)*100 #22 % FA est non-HIC
# 
# #pourcentage de FR en HIC contre FA en non-HIC
# perc_FR_HIC <- length(Hab_IFN_histo[HIC=="OUI" & FORET_EM == 0]$IDP)/length(Hab_IFN_histo[FORET_EM == 0]$IDP)*100 #62 % FR est HIC
# perc_FR_non_HIC <- length(Hab_IFN_histo[HIC=="NON" & FORET_EM == 0]$IDP)/length(Hab_IFN_histo[FORET_EM == 0]$IDP)*100 #38 % FR est non-HIC

#barplot des pourcentages
perc_tab_barplot <- Hab_IFN_histo[,.(nb_plac=.N), by = .(FORET_EM, HCI)]
perc_tab_barplot[,tot_pl := sum(nb_plac), by = .(FORET_EM)]
perc_tab_barplot[,perc_HIC := round((nb_plac/tot_pl)*100, digits = 0), by = .(FORET_EM)]
perc_tab_barplot[,lab:=ifelse(FORET_EM==1, "AF", "RF")][,FORET_EM:=NULL]
#construction toute forêt confondue
tout_foret <- perc_tab_barplot[,.(nb_plac = sum(nb_plac)),by =.(HCI)]
tout_foret[,tot_pl:=sum(nb_plac)][,perc_HIC := round((nb_plac/tot_pl)*100, digits = 0)]
tout_foret[,lab:="ALL"]
#fusion des 2
perc_tab_barplot <- rbind(perc_tab_barplot, tout_foret)
#reorder factors

#barplot en surface

#verification des poids surfaciques
surf_fr <- unique(Hab_IFN_histo[,.(SER, nbPlacSER ,poids_ha_par_placette)]) #76 SER étudiées (91 - 3 Corse, 7 Med et 5 allu) = OK!
sum(surf_fr$poids_ha_par_placette * surf_fr$nbPlacSER)  #15 601 375 ha, On a enlevé la Med et la Corse, ça semble ok.
sum(surf_fr$nbPlacSER) #108 853 il y a aussi les placettes non forestières et etc... Mais logique qu'on perde de la surface par la suite
#on construit la table
surf_tab_barplot <- Hab_IFN_histo[,.(area = sum(poids_ha_par_placette)), by = .(FORET_EM, HCI)]
(tot_surf_etu <- sum(surf_tab_barplot$area)) #12 978 775, pourquoi il y a de la perte? C'est fait sur les 90 746 placettes restantes après zone
#tampon, peupliers etc...
#on recalcule un % avec le total de surface étudié
surf_tab_barplot[,surf_tot_EM:=sum(area), by =.(FORET_EM)]
surf_tab_barplot[,perc_surf:=round((area/surf_tot_EM)*100, digits = 0)]
surf_tab_barplot[,lab:=ifelse(FORET_EM==1, "AF", "RF")][,FORET_EM:=NULL]
#construction toute forêt confondue
surf_tout_foret <- surf_tab_barplot[,.(area = sum(area)),by =.(HCI)]
surf_tout_foret[,surf_tot_EM:=tot_surf_etu][,perc_surf := round((area/surf_tot_EM)*100, digits = 0)]
surf_tout_foret[,lab:="ALL"]


#plot surface FA et FR
ggplot(surf_tab_barplot, aes(x = lab, y = area, fill=HCI))+geom_bar(position = "dodge", stat = "identity") +
  theme_bw() + 
  geom_text(aes(label = round(perc_surf)), position = position_dodge(width = 1), vjust = -0.5, size = 4)+
  scale_fill_manual(values=c("mediumpurple2","mediumpurple4"))
#plot surface France entière
ggplot(surf_tout_foret, aes(x = lab, y = area, fill=HCI))+geom_bar(position = "dodge", stat = "identity") +
  theme_bw() + 
  geom_text(aes(label = round(perc_surf)), position = position_dodge(width = 1), vjust = -0.5, size = 4)+
  scale_fill_manual(values=c("mediumpurple2","mediumpurple4"))

#plots avec les pourcentages
ggplot(perc_tab_barplot, aes(x = perc_HIC, y = lab, fill=HCI))+
  geom_bar(position = "stack", stat = "identity", width = 0.6) + 
  theme_bw() + 
  geom_text(aes(label = round(perc_HIC)), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("mediumpurple4","mediumpurple2"))

#TEST FISHER FRANCE ENTIERE (sans correction surfacique)
#construction tableau de contingence
AF <- c(perc_tab_barplot[lab == "AF" & HCI=="YES"]$nb_plac, perc_tab_barplot[lab == "AF" & HCI=="NO"]$nb_plac)
RF <- c(perc_tab_barplot[lab == "RF" & HCI=="YES"]$nb_plac, perc_tab_barplot[lab == "RF" & HCI=="NO"]$nb_plac)
TAB <- rbind(AF,RF)
colnames(TAB) <- c("HCI","NON-HCI")
#test
Fisher <- fisher.test(TAB)#p_value <2.2e-16

#TEST FISHER FRANCE ENTIERE (avec correction surfacique)
#construction tableau de contingence
AF <- c(surf_tab_barplot[lab == "AF" & HCI=="YES"]$perc_surf, surf_tab_barplot[lab == "AF" & HCI=="NO"]$perc_surf)
RF <- c(surf_tab_barplot[lab == "RF" & HCI=="YES"]$perc_surf, surf_tab_barplot[lab == "RF" & HCI=="NO"]$perc_surf)
TAB <- rbind(AF,RF)
colnames(TAB) <- c("HCI","NON-HCI")
#test
Fisher <- fisher.test(TAB)#p_value = 0.015 : en fait plus l'échantillon est grand plus c'est sign
# à ce stade ça en sert presque à rien de faire le test. 


#########################################################################################################################################################
#tests subsidiaires discussion

#lien entre ancienneté de l'état boisé et Pboncla puis S_PHI
ggplot(data=Hab_IFN_histo, aes(x = as.factor(FORET_EM), y = Pboncla))+geom_boxplot()
ggplot(data=Hab_IFN_histo, aes(x = as.factor(FORET_EM), y = S_PHI))+geom_boxplot()
Hab_IFN_histo_disc <- Hab_IFN_histo[,FORET_EM_2 := as.factor(FORET_EM)]
ggqqplot(Hab_IFN_histo_disc, x = "Pboncla", facet.by = "FORET_EM_2")#c'est pas du tout normal : Bernouilli
ggqqplot(Hab_IFN_histo_disc, x = "S_PHI", facet.by = "FORET_EM_2")#c'est pas du tout normal : exp ?

cor(Hab_IFN_histo[,.(Pboncla, S_PHI)])#correlation entre S_PHI et Pboncla = -0.41
#Test ANCOVA
ggplot(data = Hab_IFN_histo, aes(x = Pboncla, y = S_PHI, col = as.factor(FORET_EM)))+geom_point()

#lien entre HIC et non-HIC et Pboncla puis S_PHI
ggplot(data=Hab_IFN_histo, aes(x = as.factor(HCI), y = Pboncla))+geom_boxplot()
ggplot(data=Hab_IFN_histo, aes(x = as.factor(HCI), y = S_PHI))+geom_boxplot()
ggplot(data=Hab_IFN_histo[S_REL>10 & S_REL <40], aes(x = as.factor(FORET_EM), y = S_REL))+geom_boxplot()

#lien entre le type d'HIC et Pboncla
ggplot(data=Hab_IFN_histo[HCI=="YES"], aes(x = as.factor(EUR15_GEN), y = Pboncla))+geom_boxplot()

# setwd("~/These/data/volet1")
# fwrite(pourc_HIC_FAFR_SER, "pourc_HIC_FORETEM_SER.csv", dec=".")

tout_indices <- merge(pourc_HIC_FAFR_SER, ind_div_hab, by=c("SER", "FORET_EM"), all=TRUE)
tout_indices[, GRECO:=substr(SER, 1, 1)]
unique_diff_pielou_perc <- merge(pourc_HIC_FAFR_SER_w, ind_div_habw_pielou)

#lien entre % HIC et indice de diversité
ggplot(data=tout_indices, aes(x= Pielou, y = freq, col=FORET_EM))+geom_point()
ggplot(data=tout_indices[GRECO=="A"], aes(x= Pielou, y = freq, col=FORET_EM))+geom_point()
ggplot(data=tout_indices[GRECO=="C"], aes(x= Pielou, y = freq, col=FORET_EM))+geom_point()
cor(tout_indices[,.(Pielou, shannon, rich_hab, freq)])

#visualisation par GRECO du nombre d'habitat et de la répartition des placettes
Hab_nbpl_GRECO <- Hab_IFN_histo[,.(Nb = .N ), by =.(GRECO, EUR15_GEN)][order(GRECO)]
Hab_nbpl_GRECO[EUR15_GEN=="", habitat := "NON-HIC"][EUR15_GEN=="9110", habitat := "Hêtraies du Luzulo-Fagetum"][
  EUR15_GEN=="9120", habitat := "Hêtraies acidophiles atlantiques"][EUR15_GEN=="9130", habitat :="Hêtraies du Asperulo-Fagetum"][
    EUR15_GEN=="9140", habitat :="Hêtraies subalpines"][EUR15_GEN=="9150", habitat :="Hêtraies calcicoles"][
      EUR15_GEN=="9160", habitat :="Chênaies pédonculées"][EUR15_GEN=="9170", habitat :="Chênaies-charmaies"][
        EUR15_GEN=="9180*", habitat := "Forêts de pentes"][EUR15_GEN=="9190", habitat := "Chênaies acidophiles à Quercus robur"][
          EUR15_GEN=="91D0*", habitat :="Tourbières boisées"][EUR15_GEN=="91E0*", habitat :="Forêts alluviales"][EUR15_GEN=="91F0", habitat :="Forêts des grands fleuves"][
            EUR15_GEN=="9230", habitat :="Chenaies aquitaines"][EUR15_GEN=="92A0", habitat :="Forêts galeries méditerranéennes"][
              EUR15_GEN=="9410", habitat :="pessières montagnardes"][EUR15_GEN=="9420", habitat :="Forêts alpines"][EUR15_GEN=="9430*", habitat :="Pinèdes à crochets"][
                EUR15_GEN=="9540", habitat :="Pinèdes méditerranéennes"][EUR15_GEN=="9560*", habitat :="Thuriferaies"]
# setwd("~/These/data/volet1")
# fwrite(Hab_nbpl_GRECO, "nb_placettes_GRECO_2005-2023.csv", sep=";", )

Hab_nbpl_SER <- Hab_IFN_histo[,.(Nb = .N ), by =.(SER, EUR15_GEN, FORET_EM)][order(SER)]
Hab_nbpl_SER[EUR15_GEN=="", habitat := "NON-HIC"][EUR15_GEN=="9110", habitat := "Hêtraies du Luzulo-Fagetum"][
  EUR15_GEN=="9120", habitat := "Hêtraies acidophiles atlantiques"][EUR15_GEN=="9130", habitat :="Hêtraies du Asperulo-Fagetum"][
    EUR15_GEN=="9140", habitat :="Hêtraies subalpines"][EUR15_GEN=="9150", habitat :="Hêtraies calcicoles"][
      EUR15_GEN=="9160", habitat :="Chênaies pédonculées"][EUR15_GEN=="9170", habitat :="Chênaies-charmaies"][
        EUR15_GEN=="9180*", habitat := "Forêts de pentes"][EUR15_GEN=="9190", habitat := "Chênaies acidophiles à Quercus robur"][
          EUR15_GEN=="91D0*", habitat :="Tourbières boisées"][EUR15_GEN=="91E0*", habitat :="Forêts alluviales"][EUR15_GEN=="91F0", habitat :="Forêts des grands fleuves"][
            EUR15_GEN=="9230", habitat :="Chenaies aquitaines"][EUR15_GEN=="92A0", habitat :="Forêts galeries méditerranéennes"][
              EUR15_GEN=="9410", habitat :="pessières montagnardes"][EUR15_GEN=="9420", habitat :="Forêts alpines"][EUR15_GEN=="9430*", habitat :="Pinèdes à crochets"][
                EUR15_GEN=="9540", habitat :="Pinèdes méditerranéennes"][EUR15_GEN=="9560*", habitat :="Thuriferaies"]


#Repartition assoc par GRECO
Assoc_nbpl_GRECO <- Hab_IFN_histo[,.(Nb = .N ), by =.(GRECO, EUR15_GEN, FORET_EM, Assoc)][order(GRECO)]
Assoc_nbpl_GRECO[,Nb_tot_GRECO:= sum(Nb), by = .(GRECO, FORET_EM)]
Assoc_nbpl_GRECO[,perc_pl := Nb/Nb_tot_GRECO]
Assoc_nbpl_GRECO[EUR15_GEN=="", habitat := "NON-HIC"][EUR15_GEN=="9110", habitat := "Hêtraies du Luzulo-Fagetum"][
  EUR15_GEN=="9120", habitat := "Hêtraies acidophiles atlantiques"][EUR15_GEN=="9130", habitat :="Hêtraies du Asperulo-Fagetum"][
    EUR15_GEN=="9140", habitat :="Hêtraies subalpines"][EUR15_GEN=="9150", habitat :="Hêtraies calcicoles"][
      EUR15_GEN=="9160", habitat :="Chênaies pédonculées"][EUR15_GEN=="9170", habitat :="Chênaies-charmaies"][
        EUR15_GEN=="9180*", habitat := "Forêts de pentes"][EUR15_GEN=="9190", habitat := "Chênaies acidophiles à Quercus robur"][
          EUR15_GEN=="91D0*", habitat :="Tourbières boisées"][EUR15_GEN=="91E0*", habitat :="Forêts alluviales"][EUR15_GEN=="91F0", habitat :="Forêts des grands fleuves"][
            EUR15_GEN=="9230", habitat :="Chenaies aquitaines"][EUR15_GEN=="92A0", habitat :="Forêts galeries méditerranéennes"][
              EUR15_GEN=="9410", habitat :="pessières montagnardes"][EUR15_GEN=="9420", habitat :="Forêts alpines"][EUR15_GEN=="9430*", habitat :="Pinèdes à crochets"][
                EUR15_GEN=="9540", habitat :="Pinèdes méditerranéennes"][EUR15_GEN=="9560*", habitat :="Thuriferaies"]#pourcentage de rel par hab et SER et usage histo : ex : 100 % pour tous les hab en FA d'une SER et 100 % tous les hab en FR d'une SER : corrige biais



#repartition FA et FR
Hab_nbpl_SER[,perc_hab_SER := round((Nb/sum(Nb))*100, digits = 2), by = .(SER, FORET_EM)]
Hab_nbpl <- Hab_nbpl_SER[,.(nb_pl_hab = sum(Nb)), by = .(EUR15_GEN)]
Hab_nbpl[,perc_hab_tot := round((nb_pl_hab/sum(nb_pl_hab))*100, digits = 1)][order(nb_pl_hab)]
#histogramme des freq d'habitats tot
ggplot(Hab_nbpl,aes(x = fct_reorder(EUR15_GEN, nb_pl_hab), y = nb_pl_hab))+ geom_col(fill="lightgrey") +
  geom_text(aes(label = perc_hab_tot), size = 4, vjust=0.7)+theme_minimal()

#même chose avec que les HIC
Hab_nbpl[EUR15_GEN!="", perc_HIC_tot := round((nb_pl_hab/sum(nb_pl_hab))*100, digits = 1)][order(nb_pl_hab)]
ggplot(Hab_nbpl[EUR15_GEN!=""],aes(x = fct_reorder(EUR15_GEN, nb_pl_hab), y = nb_pl_hab))+ geom_col(fill="lightgrey") +
  geom_text(aes(label = perc_HIC_tot), size = 4, vjust=0.7)+theme_minimal()

#En faisant le pourcentage de placette en 9120 et 9130
Hab_nbpl_SER[EUR15_GEN == "9120" | EUR15_GEN == "9130",pl_hetraies_SER := sum(Nb), by = .(SER, FORET_EM)]
Hab_nbpl_hetraie_SER <- Hab_nbpl_SER[,.(perc_hetraie_SER = (pl_hetraies_SER/sum(Nb))*100), by = .(SER, FORET_EM)]
Hab_nbpl_hetraie_SER <- unique(Hab_nbpl_hetraie_SER[!is.na(perc_hetraie_SER),.(SER, perc_hetraie_SER, FORET_EM)])
Hab_nbpl_hetraie_SER[,FORET_EM1 := FORET_EM]
Hab_nbpl_hetraie_SER[,FORET_EM := ifelse(FORET_EM1==1, "FA", "FR")]
Hab_nbpl_hetraie_SER$FORET_EM1 <- NULL
tout_indices <- merge(tout_indices, Hab_nbpl_hetraie_SER, by=c("SER", "FORET_EM"), all.x = TRUE)
tout_indices[is.na(perc_hetraie_SER), perc_hetraie_SER:=0]
cor(tout_indices[,.(Pielou, shannon, rich_hab, freq, perc_hetraie_SER)]) #frequence d'HIC est très fortement correlée à perc_hetraie_SER
ggplot(data=tout_indices, aes(x= Pielou, y = perc_hetraie_SER, col=FORET_EM))+geom_point() #On a bien l'air d'avoir une relation négative entre %
#9120 + 9130 et Pielou (ou shannon)

#liste des SER où % surface d'HIC est supérieur en FR
perc_sup_FR <- pourc_HIC_FAFR_SER_w[diff_FA_FR<=0]
#Quels habitats sont majoritaires dans les SER où le % HIC est supérieur en FR
View(Hab_nbpl_SER[SER %in% perc_sup_FR$SER]) # Ca concerne 20 SER


ggplot(data=tout_indices, aes(x= Pielou, y = perc_hetraie_SER, col=FORET_EM))+geom_point()+facet_wrap(vars(GRECO))
ggplot(data=tout_indices, aes(x= freq, y = perc_hetraie_SER, col=FORET_EM))+geom_point()+facet_wrap(vars(GRECO))#relation assez nette 

#Habitat dominant par SER
hab_dom_SER <- Hab_nbpl_SER[,.SD[which.max(perc_hab_SER)], by=.(SER)]

# setwd("~/These/data/volet1")
# fwrite(hab_dom_SER, "habitat_dominant_par_SER_sans_histo.csv", sep=";")


#On essaye de voir s'il y a des régions dans lesquelles on détecte moins bien les hab
Hab_IFN_histo[,.(moy_Poncla_SER = mean(Pboncla), mean_S_PHI = mean(S_PHI)), by = .(SER)]
Hab_IFN_histo[,.(moy_Poncla_GRECO = mean(Pboncla), mean_S_PHI = mean(S_PHI)), by = .(GRECO)]
ggplot(data = Hab_IFN_histo, aes(x=as.factor(SER), y=Pboncla))+geom_boxplot()
ggplot(data = Hab_IFN_histo, aes(x=as.factor(SER), y=S_PHI))+geom_boxplot()


#Richesse spé entre FA et FR au sein d'un habitat
Hab_IFN_histo[,mean_S := mean(S_PHI), by=.(EUR15_GEN, FORET_EM)]
view(unique(Hab_IFN_histo[,.(mean_S, EUR15_GEN, FORET_EM)]))

