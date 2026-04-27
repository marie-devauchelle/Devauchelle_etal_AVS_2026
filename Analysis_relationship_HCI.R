# Relationship of HCI types with historical land use (AF or RF) / Figure 1 #
rm(list = ls())
#step 0 : package loading ####
library(ggplot2)
library(data.table)
library(stringr)
library(sf)


# downloading NFI forest regions
#Available at: https://inventaire-forestier.ign.fr/spip.php?rubrique267
setwd("~/These/data/volet1/spatial")
map_reg <- st_read("rnifn250.shp")

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
#NFI plots location are available at: https://inventaire-forestier.ign.fr/dataifn/
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
###################################################################################################################################################

#Step 1 : Frequency of each HCI in AF and RF####

#We want to know if there is a significant difference between AF and RF frequency
#Within a HCI = Fisher's exact test  https://statsandr.com/blog/fisher-s-exact-test-in-r-independence-test-for-a-small-sample/


#Construction of the subsample within HCIs distribution's range
# selection of thresholds
t_occ_REGN <- 7
t_pc_REGN <- 0.5

#Calculation of Fisher's exact test for each HCI
#REGN
#Initializing the results spreadsheet and list_SER
Frequence_per_EUR15 <- data.table()

for(EUR15i in sort(unique(NFI_hab_histo[occ_REGN >= t_occ_REGN | pc_REGN >= t_pc_REGN]$EUR15_GEN))){
  
  #EUR15i <- "9180*"
  
  print(EUR15i);flush.console()
  Ligne <- NFI_hab_histo[EUR15i==EUR15_GEN]
  
  if(length(unique(Ligne$Cat))>1)next()
  
  #Selection of REGNs with a minimum threshold for habitat occurrence.
  #We select plots for which there are 5 or more habitat occurrences per REGN and 0.5% or more of the REGN
  
  #list of REGN selected by HCI
  liste_REGN <- unique(Ligne[occ_REGN >= t_occ_REGN | pc_REGN >= t_pc_REGN]$REGN)
  #liste_REGN <- unique(NFI_hab_sel[Assoc == Associ]$REGN)
  Nb_rel_anc <- sum(Ligne$FORET_EM == 1 & Ligne$REGN %in% liste_REGN)
  Nb_rel_rec <- sum(Ligne$FORET_EM == 0 & Ligne$REGN %in% liste_REGN)
  Tot_Anc_Hab <- sum(NFI_hab_histo$FORET_EM==1 & NFI_hab_histo$REGN %in% liste_REGN)
  Tot_Rec_Hab <- sum(NFI_hab_histo$FORET_EM==0 & NFI_hab_histo$REGN %in% liste_REGN)
  
  
  #contingency table
  #We divide by the total number of plots in AF and RF (on the REGN where the association is present)
  #to address the fact that IFN plots are not evenly distributed
  #between AF and RF 
  TAB <- rbind(c(Nb_rel_anc,Tot_Anc_Hab - Nb_rel_anc),
               c(Nb_rel_rec,Tot_Rec_Hab - Nb_rel_rec))
  
  test <- chisq.test(TAB, correct = FALSE)
  # new : test de Fisher  
  Fisher <- fisher.test(TAB)
  Frequence_per_EUR15 <- rbind(Frequence_per_EUR15, 
                                     data.table(EUR15_GEN=EUR15i, Cat=unique(Ligne$Cat),  p_value_chi2 = test$p.value, p_value_Fisher = Fisher$p.value,
                                                freq_anc = 100 * Nb_rel_anc/Tot_Anc_Hab,freq_rec = 100 * Nb_rel_rec/Tot_Rec_Hab, 
                                                Nb_rel = Nb_rel_anc + Nb_rel_rec,Nb_rel_anc = Nb_rel_anc, Nb_rel_rec =  Nb_rel_rec,
                                                Tot_Anc_Hab = Tot_Anc_Hab, Tot_Rec_Hab = Tot_Rec_Hab,
                                                nb_REGN_hab = length(liste_REGN)))
  
  
}
nrow(Frequence_per_EUR15); sum(Frequence_per_EUR15$Nb_rel) #19 HIC, 90 326 selected surveys

#WARNINGS
#a correction is applied to account for the multiplicity of tests
Frequence_per_EUR15$p_value_Fisher_corrigee <- p.adjust(Frequence_per_EUR15$p_value_Fisher,method="fdr")

#Relative risk: the ratio of the higher of these two frequencies to the lower one
Frequence_per_EUR15[,risk_rel := ifelse(freq_anc>freq_rec, freq_anc/freq_rec, freq_rec/freq_anc)]

#Add the test significance
Frequence_per_EUR15[,sign_Fisher :=  ifelse(p_value_Fisher_corrigee <= 0.05, "*", "")
][,sign_Fisher :=  ifelse(p_value_Fisher_corrigee <= 0.01, "**", sign_Fisher)
][,sign_Fisher :=  ifelse(p_value_Fisher_corrigee <= 0.001, "***", sign_Fisher)][,sign_Fisher :=  ifelse(p_value_Fisher_corrigee <= 0.0001, "****", sign_Fisher)]
#We add the direction of this significance topward either AF or RF
Frequence_per_EUR15[,pref := ifelse(p_value_Fisher_corrigee <= 0.05 & freq_anc > freq_rec, "FA", "")][,pref := ifelse(p_value_Fisher_corrigee <= 0.05 & freq_rec > freq_anc, "FR", pref)]

hist(log10(Frequence_per_EUR15$Nb_rel))

###############################################################################################################################################

#Number of HCI which are more prevalent in AF, RF or are neutral
length(which(Frequence_per_EUR15$pref == "FA")) #5/19; 26% HCIs are more prevalent in AF
length(which(Frequence_per_EUR15$pref == "FR")) #7/19; 37%  HCIs are more prevalent in RF
length(which(Frequence_per_EUR15$pref == "")) # 7/19; 37% HCIs are not significanlt prevalent in either RF or AF

###############################################################################################################################################
#Adding english name of HCIs with a corresponding table
setwd("C:/Users/191007/Documents/These/data/volet1")
nom_frEUR15 <- fread("EUR15_GEN_corresp_cahier_hab.txt", encoding = "Latin-1")
Frequence_per_EUR15 <- merge(Frequence_per_EUR15, nom_frEUR15[,.(EUR15_GEN, Names_en)], by = "EUR15_GEN", all.x = TRUE) 

Frequence_per_EUR15[,dif_fr_fafr := freq_anc - freq_rec]

Frequence_per_EUR15[,EUR15_cat_sign := paste0(Cat,sign_Fisher, "_", EUR15_GEN)]
Frequence_per_EUR15[,annote_Nb_rel := paste0("(", Nb_rel, ")")]

#We selected HCI with more 7 or more surveys to avoid drawing non
#reliable conclusions
SEL <- Frequence_per_EUR15[Nb_rel>=10 & EUR15_GEN!=""]

#Adding the relative difference to better represent affinity with RF or AF
#not depending to the number of plots of each HCIs
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
  theme(text=element_text(family="Arial"),
        legend.position = c(0.2,0.35),
        legend.text = element_text(size=14),
        axis.text.y = element_text(size = 17))

#percentage of plots for each HCI
ggplot(IND_JC, aes(x = pourc_rel, y = EUR15_cat_sign))+
  geom_col(fill = "lightgrey")+
  xlab("percentage of plots")+
  theme_classic()+
  scale_y_discrete(label=IND_JC$EUR15_GEN,breaks= IND_JC$EUR15_cat_sign)
