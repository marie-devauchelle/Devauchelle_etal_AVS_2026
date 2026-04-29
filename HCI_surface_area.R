# Surface area of HCIs at national and regional scale / Figure 2 and 3 #

rm(list = ls())
#Step 0 : package loading ####
library(ggplot2)
library(data.table)
library(stringr)
library(sf)
library(ggspatial)#north arrow and scale on maps


# downloading NFI forest regions ("régions forestières")
#Available at: https://inventaire-forestier.ign.fr/spip.php?rubrique267
setwd("~/These/data/volet1/spatial")
map_reg <- st_read("rnifn250.shp")

#downloading sylvoecoregion map (SER)
#Available at: https://inventaire-forestier.ign.fr/spip.php?rubrique267
setwd("~/These/data/volet1/spatial")
map_ser <- st_read("ser_l93.shp")
names(map_ser) <- c("SER", "NomSER", "geometry")

#step 0 : Dataset loading ####
setwd("~/These/data/volet1/Scripts_AVS_2026/Devauchelle_etal_AVS_2026")
# dataset from NFI classified plots from the automatic classification program
hab <- fread("plots_classified_habitats.csv") #118 815 classified surveys

#Orthographic correction
hab$Assoc <- ifelse(hab$Assoc == "Pruno padi-Fraxinetum exclesioris", "Pruno padi-Fraxinetum excelsioris", hab$Assoc)

#We remove surveys with less than 7 species taken into account by the classification
#program (S_PHI) / (assignation to the alliance  Maciejewski et al 2021)
t_sp <- 7
nrow(hab) #118 815
hab <- hab[S_PHI>=t_sp]
nrow(hab) #116 515:  2300 surveys with less than 7 species

#Raw plots informations dataset
#Available here: https://inventaire-forestier.ign.fr/dataifn/
setwd("~/These/data/volet1")
raw_NFI_info_plots <- fread("export_dataifn_2005_2023/PLACETTE.csv")
nrow(raw_NFI_info_plots) #205 312

#dataset from cross-referencing between NFI plots location and historical map
#and the French forest map BD FORET V2.
#this dataset was given by the IGN.
#NFI plots blurred location (at 700 m) are available at: https://inventaire-forestier.ign.fr/dataifn/
#The Historical "General Staff map" is abvailable at: https://www.data.gouv.fr/datasets/bd-carto-r-etat-major
# The French forest map is available at: https://cartes.gouv.fr/rechercher-une-donnee/dataset/IGNF_BD-FORET
#We considered as Ancient Forest (coded 1), plots where the historic map was
#strictly forested (coded EM9.1) and the actual map BD FORET V2 was also strictly forested (coded FF).
#Recent Forest was FF on BD FORET V2 and anything else than EM9.1 on the 
#historical map. 

#Warning ! Some plots (IDP) do not contain floristic surveys
setwd("~/These/data/volet1")
crois_EM <- fread("cross_reference_historical_NFI_plots.csv", dec=".", sep=",")
nrow(crois_EM)# 135 149

#Etape 0 : Merging infos to a dataset to analyse ####
plots_infos <- unique(raw_NFI_info_plots[,.(IDP, XL, YL, DEP, SER, CSA, CAMPAGNE)])
nrow(plots_infos) #205 312

#Cross referencing with French forest regions " régions forestières".
placSf <- st_as_sf(unique(raw_NFI_info_plots[,.(IDP,XL,YL)]),coords=c("XL","YL"),crs=2154)
nrow(placSf) # 121 419 localités uniques
placReg <- st_intersection(placSf,map_reg[,c("REGN","REGIONN")])
nrow(placReg) # 121 403; 16, are out of region maps, no importance
plots_infos <- merge(plots_infos,data.table(st_drop_geometry(placReg)),by=c("IDP"),all.x=TRUE)

plots_infos$inplac <- 1
NFI_hab <- merge(hab[,.(IDP,CAMPAGNE, Assoc, TYPE_SYN, Pboncla, S_REL, S_PHI, ALL, EUR15_GEN, EUR15_ELE, Id_DocGegout09)], 
                 plots_infos, by = c("IDP", "CAMPAGNE"), all.x = TRUE)
NFI_hab[is.na(inplac),.(IDP)] #empty = OK ! 
nrow(NFI_hab) #116 515 
NFI_hab$inplac <- NULL; plots_infos$inplac <- NULL

#We remove poplar plantation and other non-forested plots
table(NFI_hab$CSA, useNA = "always")
#1018 surveys removed
NFI_hab <- NFI_hab[CSA == 1 | CSA == 2 | CSA == 3]
nrow(NFI_hab) # 115 497 + 1018 = 116 515 OK!  

#We remove surveys from Corsica (GRECO == K) and the Mediterannean region (GRECO ==J)
NFI_hab[, GRECO:=substr(SER, 1, 1)]

table(NFI_hab$GRECO,useNA="always")
NFI_hab <- NFI_hab[!GRECO %in% c("J","K")]
nrow(NFI_hab) # 106 679 surveys

#Creation of Cat: a category for each HCI (EUR15_GEN) based on 
#the Habitat Directive and EUR28 manual.
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
NFI_hab_histo <- NFI_hab_histo[!is.na(FORET_EM)]; nrow(NFI_hab_histo) #102 349

table(NFI_hab_histo$FORET_EM, useNA = "always") #nb of RF : 50 331, nb of AF : 52 018, Nb of NA : 3 558, Tot surveys with histo : 102 349

#We select a 20 m buffer zone
nrow(NFI_hab_histo)#102 349
NFI_hab_histo <- NFI_hab_histo[CEM_NEAR_DIST>=20]
nrow(NFI_hab_histo)#90 746

table(NFI_hab_histo$FORET_EM, useNA = "always")#AF 45 639, RF 45 107
##################################################################################################################################################

#Regional scale

#loading an area weight for each plot depending on its biogeographical region (GRECO)
#The area weight was obtained by dividing the total forest area (from BD foret V2) of each 
#of the nine biogeographical regions by the number of NFI plots they contain. 
setwd("~/These/data/volet1")
w_surf_ser <- fread("poids_SER.txt")

#Creation of a column to indicate HCI or non-HCI
NFI_hab_histo[!is.na(EUR15_GEN), HCI := "YES"][EUR15_GEN == "", HCI := "NO"]

#merge NFI_hab_histo with weighted surfaces areas
nrow(NFI_hab_histo) #90746
NFI_hab_histo <- merge(NFI_hab_histo, w_surf_ser[,.(SER, nbPlacSER, poids_ha_par_placette)], all.x = TRUE, by = "SER")
nrow(NFI_hab_histo) #90746 : OK

#Creation of a map with percentage of HCI surface area per region (SER)
#Création carte pourcentage de surface d'HIC par région 
Nb_HIC_SER <- NFI_hab_histo[HCI == "YES", .(Nb_HIC_SER = .N), by = .(SER, FORET_EM)][order(SER, FORET_EM)]
Nb_SER <- NFI_hab_histo[, .(Nb_SER = .N), by = .(SER, FORET_EM)][order(SER, FORET_EM)]
dim(Nb_HIC_SER);dim(Nb_SER)#152; 3
pourc_HIC_FAFR_SER <- merge(Nb_HIC_SER, Nb_SER, by =c("SER", "FORET_EM"))
pourc_HIC_FAFR_SER[, freq := Nb_HIC_SER/Nb_SER]
pourc_HIC_FAFR_SER[,FORET_EM1:=FORET_EM]
table(pourc_HIC_FAFR_SER$FORET_EM)#OK que 2 valeurs 1 ou 0 pour FORET_EM
pourc_HIC_FAFR_SER[,FORET_EM := ifelse(FORET_EM1==1, "FA", "FR")]
#transform into wide format
pourc_HIC_FAFR_SER_w <- dcast(pourc_HIC_FAFR_SER, SER ~ FORET_EM, value.var = "freq")

#Calculation of % difference in HCI between AF and RF
pourc_HIC_FAFR_SER_w[,diff_FA_FR := FA-FR]
nrow(pourc_HIC_FAFR_SER_w[diff_FA_FR<0])#18/76 SER with surf area of HCI in  RF  > HIC surf area AF  : 23%
nrow(pourc_HIC_FAFR_SER_w[diff_FA_FR>0])#58/76 SER with surf area of HCI in  AF  > HIC surf area RF : 76%
nrow(pourc_HIC_FAFR_SER_w[diff_FA_FR==0])#0/76

#construction of classes to discretise the map
pourc_HIC_FAFR_SER_w[diff_FA_FR < -0.10, diff_class := "-0.15:-0.10"]
pourc_HIC_FAFR_SER_w[diff_FA_FR < -0.05 & diff_FA_FR >= -0.10, diff_class := "-0.10:-0.05"]
pourc_HIC_FAFR_SER_w[diff_FA_FR < -0.00 & diff_FA_FR >= -0.05, diff_class := "-0.05:-0.00"]
pourc_HIC_FAFR_SER_w[diff_FA_FR == 0, diff_class := "0.00"]
pourc_HIC_FAFR_SER_w[diff_FA_FR < 0.05 & diff_FA_FR > 0, diff_class := "0.00:0.05"]
pourc_HIC_FAFR_SER_w[diff_FA_FR < 0.10 & diff_FA_FR >= 0.05, diff_class := "0.05:0.10"]
pourc_HIC_FAFR_SER_w[diff_FA_FR < 0.15 & diff_FA_FR >= 0.10, diff_class := "0.10:0.15"]
pourc_HIC_FAFR_SER_w[diff_FA_FR > 0.15, diff_class := "0.15:0.25"]

#creation of color palette
col8 <- c("#FFFF66", "#FFFF99", "#FFFFCC", "#CCCCCC", "#0000FF", "#0000CC", "#000099", "#000033")
col8 <- c("#f0b000", "#ffca2e", "#ffe44c", "#CCCCCC", "#0052ff", "#1c44d0", "#2337a2", "#222b77")
map_pourc_HIC <- merge(map_ser, pourc_HIC_FAFR_SER_w, by = "SER", all=TRUE)
reorder <- c("-0.15:-0.10", "-0.10:-0.05", "-0.05:-0.00", "0.00", "0.00:0.05", "0.05:0.10", "0.10:0.15", "0.15:0.25")

#map of HCI percentage in AF vs RF by SER
ggplot(data=map_pourc_HIC)+geom_sf()+aes(fill=diff_class)+
  theme(axis.text = element_blank(),rect = element_blank(),axis.ticks = element_blank())+
  labs(fill="AF - RF")+  
  scale_fill_manual(na.value="white", breaks = reorder,values = col8)+
  theme(legend.position = c(0.94, 0.6))+
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "grid", 
                         pad_x = unit(0.7, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_orienteering)

##################################################################################
 #National scale

#barplot with weighted surface area
#check of the weighted surface areas
surf_fr <- unique(NFI_hab_histo[,.(SER, nbPlacSER ,poids_ha_par_placette)]) #76 studied SER  (91 - 3 Corse, 7 Med et 5 alluvial) = OK!
sum(surf_fr$poids_ha_par_placette * surf_fr$nbPlacSER)  #15 601 375 ha, without the mediterannean region and corsica, seems ok
sum(surf_fr$nbPlacSER) #108 853 There are also non-forest plots etc... Makes sense

#dataset construction
surf_tab_barplot <- NFI_hab_histo[,.(area = sum(poids_ha_par_placette)), by = .(FORET_EM, HCI)]
(tot_surf_etu <- sum(surf_tab_barplot$area)) #12 978 775 ha, It is just on the 90'746 plots selected after subsampling

#We recalculate the percentage based on the total studied area
surf_tab_barplot[,surf_tot_EM:=sum(area), by =.(FORET_EM)]
surf_tab_barplot[,perc_surf:=round((area/surf_tot_EM)*100, digits = 0)]
surf_tab_barplot[,lab:=ifelse(FORET_EM==1, "AF", "RF")][,FORET_EM:=NULL]
#with all the studied forest
surf_tout_foret <- surf_tab_barplot[,.(area = sum(area)),by =.(HCI)]
surf_tout_foret[,surf_tot_EM:=tot_surf_etu][,perc_surf := round((area/surf_tot_EM)*100, digits = 0)]
surf_tout_foret[,lab:="ALL"]

#Figure 2
#plot surface FA et FR
ggplot(surf_tab_barplot, aes(x = lab, y = area, fill=HCI))+geom_bar(position = "dodge", stat = "identity") +
  theme_bw() + 
  geom_text(aes(label = round(perc_surf)), position = position_dodge(width = 1), vjust = -0.5, size = 4)+
  scale_fill_manual(values=c("mediumpurple2","mediumpurple4"))+
  ylab("Area (ha)")

#Fisher's exact test at NATIONAL SCALE (with weighted surface area)
#contingency tab
AF <- c(surf_tab_barplot[lab == "AF" & HCI=="YES"]$perc_surf, surf_tab_barplot[lab == "AF" & HCI=="NO"]$perc_surf)
RF <- c(surf_tab_barplot[lab == "RF" & HCI=="YES"]$perc_surf, surf_tab_barplot[lab == "RF" & HCI=="NO"]$perc_surf)
TAB <- rbind(AF,RF)
colnames(TAB) <- c("HCI","NON-HCI")
#test
Fisher <- fisher.test(TAB)#p_value = 0.015 : In fact, the larger the sample size, the more significant the result.
# At this point, there's almost no point in running the test. 

#listy of SERs where the percentage of HCI surface area is higher in RF
perc_sup_FR <- pourc_HIC_FAFR_SER_w[diff_FA_FR<=0]

#Complementary plots
#plot HCI surface area in all forest types
ggplot(surf_tout_foret, aes(x = lab, y = area, fill=HCI))+geom_bar(position = "dodge", stat = "identity") +
  theme_bw() + 
  geom_text(aes(label = round(perc_surf)), position = position_dodge(width = 1), vjust = -0.5, size = 4)+
  scale_fill_manual(values=c("mediumpurple2","mediumpurple4"))

#plots with pourcentages
ggplot(perc_tab_barplot, aes(x = perc_HIC, y = lab, fill=HCI))+
  geom_bar(position = "stack", stat = "identity", width = 0.6) + 
  theme_bw() + 
  geom_text(aes(label = round(perc_HIC)), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("mediumpurple4","mediumpurple2"))
