# Relationship of HCI types with historical land use (AF or RF) #
rm(list = ls())
#Etape 0 : package loading ####
library(ggplot2)
library(data.table)
library(stringr)
library(sf)


# downloading NFI forest regions
#Available at: https://inventaire-forestier.ign.fr/spip.php?rubrique267
setwd("~/These/data/volet1/spatial")
map_reg <- st_read("rnifn250.shp")

#Etape 0 : Dataset loading ####
setwd("~/These/data/volet1/Scripts_AVS_2026/Devauchelle_etal_AVS_2026")
# dataset from NFI classified plots from the automatic classification program
hab <- fread("plots_classified_habitats.csv") #118 815 classified surveys

#Orthographic correction
hab$Assoc <- ifelse(hab$Assoc == "Pruno padi-Fraxinetum exclesioris", "Pruno padi-Fraxinetum excelsioris", hab$Assoc)

#We remove surveys with less than 7 species taken into account by the classification
#program (S_PHI) / (assignation to the alliance  Maciejewski et al 2021)
seuil_sp <- 7
nrow(hab) #118 815
hab <- hab[S_PHI>=seuil_sp]
nrow(hab) #116 515:  2300 surveys with less than 7 species

#Raw plots informations dataset
#Available here: https://inventaire-forestier.ign.fr/dataifn/
setwd("~/These/data/volet1")
raw_NFI_info_plots <- fread("export_dataifn_2005_2023/PLACETTE.csv")
nrow(raw_NFI_info_plots) #205 312

#dataset from cross-referencing between NFI plots location and historical map
#and the French forest map BD FORET V2.
#this dataset was given by the IGN.
#NFI plots location are available at: https://inventaire-forestier.ign.fr/dataifn/
#The Historical "General Staff map" is abvailable at: https://www.data.gouv.fr/datasets/bd-carto-r-etat-major
# The French forest map is available at: https://cartes.gouv.fr/rechercher-une-donnee/dataset/IGNF_BD-FORET
#We considered as Ancient Forest (coded 1), plots where the historic map was
#strictly forested (coded EM9.1) and the actual map BD FORET V2 was also strictly forested (coded FF).
#Recent Forest was FF on BD FORET V2 and anything else than EM9.1 on the 
#historical map. 

#Warning ! Some plots (IDP) do not contain floristic surveys
setwd("~/These/data/volet1")
crois_EM <- fread("cross_reference_historical_NFI_plots.csv", dec=",")# 135 149

#Etape 0 : Merging infos to a dataset to analyse ####
plots_infos <- unique(raw_NFI_info_plots[,.(IDP, XL, YL, DEP, SER, CSA, CAMPAGNE)])
nrow(plots_infos) #205 312

# UTILISATION REGION FORESTIERE ANCIENNES
# croisement avec les régions forestières : on ajoute deux nouvelles colonnes, 
# REGN (numéro) et REGIONN (nom) des régions forestières
# dans la table placette_IFN_brute

#Cross referencing with French forest regions " régions forestières".
placSf <- st_as_sf(unique(raw_NFI_info_plots[,.(IDP,XL,YL)]),coords=c("XL","YL"),crs=2154)
nrow(placSf) # 121 419 localités uniques
placReg <- st_intersection(placSf,map_reg[,c("REGN","REGIONN")])
nrow(placReg) # 121 403, on en perd 19, hors de la carte des régions, sans conséquence
plots_infos <- merge(plots_infos,data.table(st_drop_geometry(placReg)),by=c("IDP"),all.x=TRUE)

plots_infos$inplac <- 1
NFI_hab <- merge(hab[,.(IDP,CAMPAGNE, Assoc, TYPE_SYN, Pboncla, S_REL, S_PHI, ALL, EUR15_GEN, EUR15_ELE, Id_DocGegout09)], 
                 plots_infos, by = c("IDP", "CAMPAGNE"), all.x = TRUE)
NFI_hab[is.na(inplac),.(IDP)] #empty = OK ! 
nrow(NFI_hab) #116 515 
NFI_hab$inplac <- NULL; plots_infos$inplac <- NULL

#We remove poplar plantation and other non-forested plots
table(NFI_hab$CSA, useNA = "always")
#1018 relevés en moins
NFI_hab <- NFI_hab[CSA == 1 | CSA == 2 | CSA == 3]
nrow(NFI_hab) # 115 497 + 1018 = 116 515 OK!  

#We remove surveys from Corsica (GRECO == K) and the Mediterannean region (GRECO ==J)
NFI_hab[, GRECO:=substr(SER, 1, 1)]

table(NFI_hab$GRECO,useNA="always")
NFI_hab <- NFI_hab[!GRECO %in% c("J","K")]
nrow(NFI_hab) # 106 679 surveys

#Creation of Cat: a category for each HCI (EUR15_GEN)
NFI_hab[EUR15_GEN=="9110", Cat:="beech forests"][EUR15_GEN=="9120", Cat:="beech forests"][EUR15_GEN=="9130", Cat:="beech forests"][
  EUR15_GEN=="9140", Cat:="beech forests"][EUR15_GEN=="9150", Cat:="beech forests"][EUR15_GEN=="9160", Cat:="oak forests"][
    EUR15_GEN=="9170", Cat :="oak forests"][EUR15_GEN=="9180*", Cat :="forest of slopes, screes and ravines"][EUR15_GEN=="9190", Cat :="oak forests"][
      EUR15_GEN=="91D0*", Cat :="alluvial, riparian and bog forests"][EUR15_GEN=="91E0*", Cat :="alluvial, riparian and bog forests"][
        EUR15_GEN=="91F0", Cat :="alluvial, riparian and bog forests"][EUR15_GEN=="9230", Cat :="oak forests"][EUR15_GEN=="92A0", Cat :="alluvial, riparian and bog forests"][
          EUR15_GEN=="9410", Cat :="spruce and fir forests"][EUR15_GEN=="9420", Cat :="Pine forests"][EUR15_GEN=="9430*", Cat :="Pine forests"][
            EUR15_GEN=="9540", Cat :="Pine forests"][EUR15_GEN=="9560*", Cat :="Pine forests"][EUR15_GEN=="",Cat :=NA]
  
table(NFI_hab[!is.na(EUR15_GEN)]$Cat, useNA = "always")

control <- unique(NFI_hab[,.(EUR15_GEN, Cat)])
any(duplicated(control$EUR15_GEN))
control2 <- control[duplicated(control$EUR15_GEN)|duplicated(control$EUR15_GEN,fromLast = TRUE)]
control2[order(EUR15_GEN, Cat)]

#Calculation of Habitat Range Distribution with REGN (French forest region)
#occ_REGN: number of presence of each HCI in each REGN
NFI_hab[,occ_REGN:= .N,by = .(REGN, EUR15_GEN)]
#pourcentage of occurrence of each HCI in each REGN
NFI_hab[,pc_REGN := 100 * occ_REGN/ .N, by = .(REGN)]
#freq_glob_HCI : global frequency for each HCI 
NFI_hab[,freq_glob_HCI := .N, by = .(EUR15_GEN)]

#frequency of each HCI in each REGN
freq_EUR15_REGN <- unique(NFI_hab[,.(REGN, EUR15_GEN, occ_REGN)])[order(occ_REGN)]
#histogram of HCI occurences by REGN 
hist(log10(freq_EUR15_REGN$occ_REGN))
ggplot(data = freq_EUR15_REGN, aes(x = occ_REGN)) + geom_histogram() + scale_x_continuous(trans='log10')

#merge between historical information and habitats
nrow(NFI_hab)#106 679
NFI_hab_histo <-merge(NFI_hab, crois_EM, by = "IDP", all = FALSE)
nrow(NFI_hab_histo);length(unique(NFI_hab$EUR15_GEN)); length(unique(NFI_hab_histo$Assoc))#105 907 surveys with history, 19 HCI (19 + 1 NON HCI); 129 associations
any(duplicated(NFI_hab_histo$IDP)) #plots IDP duplicated !! : FALSE = OK ! 

table(NFI_hab_histo$FORET_EM, useNA = "always") #nb of RF : 50 331, nb of AF : 52 018, Nb of NA : 3 558, Tot surveys with histo : 102 349

#histogramme des répartitions des distance à la BDV2
hist(NFI_hab_histo[is.na(FORET_EM_2) & BF_NEAR_DIST<20]$BF_NEAR_DIST) #On peut prendre 20m comme limite
#Les bosquets de moins de ? retrouver mail Maxime Bellifa
table(NFI_hab_histo[is.na(FORET_EM_2) & BF_NEAR_DIST>20]$CAMPAGNE)
table(NFI_hab_histo[is.na(FORET_EM_2) & BF_NEAR_DIST>20]$EUR15_GEN)
#Si on veut voir les forêts très récentes avec une certaine distance à la lisière
nrow(NFI_hab_histo[is.na(FORET_EM_2) & CAMPAGNE>2018 & BF_NEAR_DIST>20]) #il en reste que 816
#Sélection de la méthode FORET_EM_2
nrow(NFI_hab_histo) #105 907
NFI_hab_histo <- NFI_hab_histo[!is.na(FORET_EM_2)]
nrow(NFI_hab_histo) #102 349
#renommer FORET_EM en FORET_EM_1, pour renommer FORET_EM_2 en FORET_EM 
NFI_hab_histo[,FORET_EM_1:=FORET_EM][,FORET_EM:=NULL]
NFI_hab_histo[,FORET_EM:=FORET_EM_2][,FORET_EM_2:=NULL]

######Test zone tampon####
hist(NFI_hab_histo[CEM_NEAR_DIST<=20]$CEM_NEAR_DIST) #On peut prendre 30m comme limite
#Sans zone tampon 
table(NFI_hab_histo$FORET_EM, useNA = "always") #FA = 50 717, FR = 51 632, Tot = 102 349
#Zone tampon 20m
table(NFI_hab_histo[CEM_NEAR_DIST>=20]$FORET_EM, useNA = "always") #FR = 45 107, FA = 45 639, Tot = 90 746 / Perte de 13 % des rel
#Zone tampon 30 m
table(NFI_hab_histo[CEM_NEAR_DIST>=30]$FORET_EM, useNA = "always") #FR = 42 753, FA = 42 903, Tot = 85 656 / Perte de 16 693 rel = 16%
#Zone tampon 50 m
table(NFI_hab_histo[CEM_NEAR_DIST>=50]$FORET_EM, useNA = "always") #FR = 38 703, FA = 37 823, Tot = 76 526
#Zone tampon 100 m 
table(NFI_hab_histo[CEM_NEAR_DIST>=100]$FORET_EM, useNA = "always") #FR = 31 598, FA = 28 621, Tot = 60 219
#Zone tampon 300 m 
table(NFI_hab_histo[CEM_NEAR_DIST>=300]$FORET_EM, useNA = "always") #FR = 17 591, FA = 12 477, Tot = 30 068

#Test avec zone tampon de 20 m
nrow(NFI_hab_histo)#102 349
NFI_hab_histo <- NFI_hab_histo[CEM_NEAR_DIST>=20]
nrow(NFI_hab_histo)#85 656 à 30m, 90 746 à 20m

###################################################################################################################################################
#Fréquence par Association

#Etape 1 : Analyse de la frequence de point en FA et FR pour l'ensemble du jeu de données####
#Calcul du total de relevés en FA et FR sur l'ensemble du territoire
(Tot_Anc <- NFI_hab_histo[FORET_EM == 1, .N]) #releves FA = 53 173 #alt FA = 15 631
(Tot_Rec <- NFI_hab_histo[FORET_EM == 0, .N]) #releves FR = 52 256 #alt FR = 13 438

#Test : On cherche à savoir si les frec_anc et frec_rec sont significativement différentes par habitat
#Chi 2 ok si nb espéré par case >=5 donc nombre de releve tot >=10
#on fait Ficher et Chi 2
#On peut faire un test exact de Fisher : https://statsandr.com/blog/fisher-s-exact-test-in-r-independence-test-for-a-small-sample/

#CHOIX DES SEUILS
#REGN
#REGN dans lesquel l'association est présente au moins seuil_occ_REGN fois
#Et REGN qui représente au moins seuil_pc_REGN % des occurences de l'association
seuil_occ_REGN <- 7
seuil_pc_REGN <- 0.5


#REGN
#Initialisation du tableur de résultats et de liste_SER
Frequence_per_EUR15 <- data.table()

for(EUR15i in sort(unique(NFI_hab_histo[occ_REGN >= seuil_occ_REGN | pc_REGN >= seuil_pc_REGN]$EUR15_GEN))){
  
  #EUR15i <- "9180*"
  
  print(EUR15i);flush.console()
  Ligne <- NFI_hab_histo[EUR15i==EUR15_GEN]
  
  if(length(unique(Ligne$Cat))>1)next()
  
  # Sélection des REGN avec un seuil minimal de présence de l'habitat / habitat.
  #on sélectionne les placettes pour lesquelles on a 5 ou plus d'occurences de l'habitat par REGN et 0.5 % ou plus des relevés de la REGN
  
  #liste des REGN selectionnées par hab
  liste_REGN <- unique(Ligne[occ_REGN >= seuil_occ_REGN | pc_REGN >= seuil_pc_REGN]$REGN)
  #liste_REGN <- unique(NFI_hab_sel[Assoc == Associ]$REGN)
  Nb_rel_anc <- sum(Ligne$FORET_EM == 1 & Ligne$REGN %in% liste_REGN)
  Nb_rel_rec <- sum(Ligne$FORET_EM == 0 & Ligne$REGN %in% liste_REGN)
  Tot_Anc_Hab <- sum(NFI_hab_histo$FORET_EM==1 & NFI_hab_histo$REGN %in% liste_REGN)
  Tot_Rec_Hab <- sum(NFI_hab_histo$FORET_EM==0 & NFI_hab_histo$REGN %in% liste_REGN)
  
  
  # corrigé : tableau de contingence
  #on divise par le nombre de relevé tot en FA et en FR (sur les REGN où l'asso est présente)
  #pour réequilibrer le fait que les releves de l'IFN ne sont pas egalement repartis
  #entre FA et FR. 
  TAB <- rbind(c(Nb_rel_anc,Tot_Anc_Hab - Nb_rel_anc),
               c(Nb_rel_rec,Tot_Rec_Hab - Nb_rel_rec))
  
  test <- chisq.test(TAB, correct = FALSE)
  # new : test de Fisher  
  Fisher <- fisher.test(TAB)
  #mise en forme tableau
  Frequence_per_EUR15 <- rbind(Frequence_per_EUR15, 
                                     data.table(EUR15_GEN=EUR15i, Cat=unique(Ligne$Cat),  p_value_chi2 = test$p.value, p_value_Fisher = Fisher$p.value,
                                                freq_anc = 100 * Nb_rel_anc/Tot_Anc_Hab,freq_rec = 100 * Nb_rel_rec/Tot_Rec_Hab, 
                                                Nb_rel = Nb_rel_anc + Nb_rel_rec,Nb_rel_anc = Nb_rel_anc, Nb_rel_rec =  Nb_rel_rec,
                                                Tot_Anc_Hab = Tot_Anc_Hab, Tot_Rec_Hab = Tot_Rec_Hab,
                                                nb_REGN_hab = length(liste_REGN)))
  
  
  
  
  #on plot chaque EUR15 : ça rallonge beaucoup, on peut mettre en silence une fois qu'on l'a fait une fois. 
  # plot(st_geometry(map_REGN))
  # points(NFI_hab[Assoc == Associ]$XL, NFI_hab[Assoc == Associ]$YL, col = "red", pch=20)
  # title(Associ)
}
nrow(Frequence_per_EUR15); sum(Frequence_per_EUR15$Nb_rel) #19 HIC, 101 898 relevés sel

#ATTENTION
#on applique une correction pour la multiplicité des tests
Frequence_per_EUR15$p_value_Fisher_corrigee <- p.adjust(Frequence_per_EUR15$p_value_Fisher,method="fdr")

#Ajout risque relatif : le rapport de la plus grande de ces deux fréquences à la plus petite
Frequence_per_EUR15[,risk_rel := ifelse(freq_anc>freq_rec, freq_anc/freq_rec, freq_rec/freq_anc)]

#Ajout significativité du test
Frequence_per_EUR15[,sign_Fisher :=  ifelse(p_value_Fisher_corrigee <= 0.05, "*", "")
][,sign_Fisher :=  ifelse(p_value_Fisher_corrigee <= 0.01, "**", sign_Fisher)
][,sign_Fisher :=  ifelse(p_value_Fisher_corrigee <= 0.001, "***", sign_Fisher)][,sign_Fisher :=  ifelse(p_value_Fisher_corrigee <= 0.0001, "****", sign_Fisher)]
#Ajout de la direction de la significativité
Frequence_per_EUR15[,pref := ifelse(p_value_Fisher_corrigee <= 0.05 & freq_anc > freq_rec, "FA", "")][,pref := ifelse(p_value_Fisher_corrigee <= 0.05 & freq_rec > freq_anc, "FR", pref)]
View(Frequence_per_EUR15)

hist(log10(Frequence_per_EUR15$Nb_rel))

# setwd("~/These/data/volet1/choix_type_region")
# fwrite(Frequence_per_EUR15, "Frequence_par_Asso_filtre_REGN_v041124.txt", sep="\t")

##QUE FAIRE DES ASSOS MARGINALES MED ??##
#1 : Cas de hab Seslerio autumnalis-Ostryetum, sur la carte on voit bien que c'est en bordure de GRECO J. 
#On cherche à savoir s'il ya Ostrya carpinifolia dedans. 
# hab[Assoc=="Seslerio autumnalis-Ostryetum"]$IDP
# #necessité de charger tab_rel dans classement_habitats_IFN_2005-2022, jusqu'à setindex l. 256
# Seslerio_Ostryetum <- tab_rel[IDP%in%hab[Assoc=="Seslerio autumnalis-Ostryetum"]$IDP]
# View(Seslerio_Ostryetum)
# unique(Seslerio_Ostryetum$IDP)
# Seslerio_Ostryetum[LB_NOM=="Ostrya carpinifolia"]$IDP
# setdiff(unique(Seslerio_Ostryetum$IDP),Seslerio_Ostryetum[LB_NOM=="Ostrya carpinifolia"]$IDP)
# #10 des 44 relevés classés en Seslerio autumnalis-Ostryetum, ne contiennent pas de Ostrya carpinifolia. 

# On enlève les assos méd qui se trouvent en bordure de GRECO J, mais on garde
#Pino-Quercetum ilicis car il est présent sur le littoral ouest. 
# Frequence_per_EUR15 <- Frequence_per_EUR15[!(Assoc=="Asplenio onopteris-Quercetum ilicis" | Assoc == "Piptathero paradoxi-Quercetum ilicis" | Assoc=="Seslerio autumnalis-Ostryetum")]


###############################################################################################################################################

#Nombre d'EUR15s qui préfèrent chaque FA et FR
length(which(Frequence_per_EUR15$pref == "FA")) #5/19; 26% EUR15s préfèrent FA; alt : 9/56; 16% EUR15s préfèrent FA
length(which(Frequence_per_EUR15$pref == "FR")) #7/19; 37%  EUR15s préfèrent FR; alt : 23/56; 41% EUR15s préfèrent FR
length(which(Frequence_per_EUR15$pref == "")) # 7/19; 37%  EUR15s n'ont pas de préférence; alt : 24/56; 42%  

###############################################################################################################################################
##Figure 1) constatation hêtraie + en FA et chenaie + en FR

#REGLES DE CLASSEMENT : 
#On classe à l'alliance
#- chenaies : Quercetalia ilicis, Quercetalia pubescentis, Quercetalia roboris, et alliance : Fraxino excelsioris-Quercion roboris (dans Fagetalia sylvaticae, il y a eu un
#changement dans PVF2, c'est passé en Quercetalia ?).
#- hetraies : Fagion sylvaticae, Luzulo fagion, Carpino betuli-Fagion. Ordre Fagetalia sylvaticae (à l'exception de Fraxion excelsioris-Quercion roboris)
#- ripisylves_marecages_tourbieres : ordre : Populetalia albae, Alnetalia glutinosa, Salicetalia auritae, Salicetalia purpureae.
#- pessieres_sapinieres : ordre: Piceetalia excelsae
#- pineraies :  Pinetalia sylvestris, et alliances : Rhododendro ferruginei-Vaccinion myrtilli, Pino sylvestris-Juniperetea sabinae.
#- forets_ravins : Acerion pseudoplatani

#Ajout du nom français des EUR15_GEN
setwd("C:/Users/191007/Documents/These/data/volet1")
nom_frEUR15 <- fread("EUR15_GEN_corresp_cahier_hab.txt", encoding = "Latin-1")
Frequence_per_EUR15 <- merge(Frequence_per_EUR15, nom_frEUR15[,.(EUR15_GEN, Names_en)], by = "EUR15_GEN", all.x = TRUE) 

Frequence_per_EUR15[,dif_fr_fafr := freq_anc - freq_rec]

Frequence_per_EUR15[,EUR15_cat_sign := paste0(Cat,sign_Fisher, "_", EUR15_GEN)]
Frequence_per_EUR15[,annote_Nb_rel := paste0("(", Nb_rel, ")")]

SEL <- Frequence_per_EUR15[Nb_rel>=10 & EUR15_GEN!=""]

ggplot(SEL, aes(x=dif_fr_fafr, y= EUR15_cat_sign, fill = Cat))+ 
  scale_fill_manual(values = c("#56B4E9", "#009E73", "808080", "#E69F00","#D55E00", "#CC79A7")) + 
  geom_col() + 
  xlab ("Percentage in AF - Percentage in RF") + 
  ylab("HCI") +
  scale_y_discrete(label=paste(SEL$EUR15_GEN, SEL$Names_en),breaks= SEL$EUR15_cat_sign) +
  geom_text(aes(label = sign_Fisher), size = 5.7, vjust=0.7) + 
  geom_vline(xintercept = 0,linetype = "dashed", col = "#8c8c8c",size = 0.6) +
  geom_text(aes(x = 15, y = EUR15_cat_sign, label = annote_Nb_rel),size= 4, hjust = 1) +
  theme(legend.position = c(0.6,0.8))
#ajout flèche FA/FR tentative +
#theme(axis.line.x = element_line(arrow = arrow(length = unit(0.3, "cm"), type="closed", ends="both")))


#AJOUT D'UNE DIFFERENCE QUI NE DEPENDE PAS DE LA PROPORTION DE L'HABITAT
TEST <- SEL[,fq_FA_reg := Tot_Anc_Hab/(Tot_Anc_Hab+Tot_Rec_Hab)][,Nb_att_FA := round(fq_FA_reg*Nb_rel)]
TEST <- SEL[,diff_relative := ((Nb_rel_anc-Nb_att_FA)/Nb_rel)*100]

ggplot(TEST, aes(x=diff_relative, y= EUR15_cat_sign, fill = Cat))+ 
  scale_fill_manual(values = c("#56B4E9", "#009E73", "808080", "#E69F00","#D55E00", "#CC79A7")) + 
  geom_col() + 
  xlab ("Percentage expected in AF - Percentage observed in AF") + 
  ylab("HCI") +
  scale_y_discrete(label=paste(TEST$EUR15_GEN, TEST$Names_en),breaks= TEST$EUR15_cat_sign) +
  geom_text(aes(label = sign_Fisher), size = 5.7, vjust=0.6) + 
  geom_vline(xintercept = 0,linetype = "dashed", col = "#8c8c8c",size = 0.5) +
  geom_text(aes(x = 25, y = EUR15_cat_sign, label = annote_Nb_rel),size= 4, hjust = 1) +
  theme(legend.position = c(0.2,0.8))

View(TEST[,.(EUR15_GEN,Names_en, pref, sign_Fisher, Nb_rel, Nb_rel_rec, Nb_rel_anc, Nb_att_FA, fq_FA_reg, diff_relative)])

#AJOUT de l'indice de JC pour rétablir selon la pref sans avoir le biais de la taille
IND_JC <- SEL[, ind_JC:=(freq_anc-freq_rec)/(freq_anc+freq_rec)][, pourc_rel := round((Nb_rel/sum(Nb_rel))*100, digits = 2)][,Categories:= Cat]

ggplot(IND_JC, aes(x=ind_JC, y= EUR15_cat_sign, fill = Categories))+ 
  scale_fill_manual(values = c("#56B4E9", "#009E73", "808080", "#E69F00","#D55E00", "#CC79A7")) + 
  geom_col() + 
  xlab ("Preference Index") + 
  ylab("HCI") +
  scale_y_discrete(label=paste(IND_JC$EUR15_GEN, IND_JC$Names_en),breaks= IND_JC$EUR15_cat_sign) +
  geom_text(aes(label = sign_Fisher), size = 5.7, vjust=0.6) + 
  geom_text(aes(x = 0.6, y = EUR15_cat_sign, label = annote_Nb_rel),size= 5.5, hjust = 1) +
  geom_vline(xintercept = 0,linetype = "dashed", col = "#8c8c8c",size = 0.5) +
  theme(legend.position = c(0.2,0.35),
        legend.text = element_text(size=14),
        axis.text.y = element_text(size = 17))

ggplot(IND_JC, aes(x = pourc_rel, y = EUR15_cat_sign))+
  geom_col(fill = "lightgrey")+
  xlab("percentage of plots")+
  theme_classic()+
  scale_y_discrete(label=IND_JC$EUR15_GEN,breaks= IND_JC$EUR15_cat_sign)



