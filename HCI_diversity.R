# Diversity of HCIs at regional and national scale

rm(list = ls())
#Step 0 : package loading ####
library(ggplot2)
library(data.table)
library(stringr)
library(sf)
library(ggspatial) #north arrow and scale on maps
library(vegan) #diversity index
library(cowplot)

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

#Step 0 : Merging infos to a dataset to analyse ####
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
#Creation of a column to indicate HCI or non-HCI
NFI_hab_histo[!is.na(EUR15_GEN), HCI := "YES"][EUR15_GEN == "", HCI := "NO"]

#We only select HCIs for this part
HCI <- NFI_hab_histo[HCI == "YES"]
nrow(NFI_hab_histo); nrow(HCI) #90 746; 63 344
HCI_AF <- HCI[FORET_EM==1] ;nrow(HCI_AF) #35 556
HCI_RF <- HCI[FORET_EM==0] ;nrow(HCI_RF) #27 788

# NATIONAL SCALE#####################################################
#Rarefaction curves with specaccum in vegan
HCI_AF[,pres:=1]
comm_FA <- dcast(HCI_AF, IDP~EUR15_GEN, value.var = "pres" ,fill = 0)
comm_FA_df <- data.frame(comm_FA)
rownames(comm_FA_df) <- comm_FA_df$IDP
comm_FA_df$IDP <- NULL
HCI_RF[,pres:=1]
comm_FR <- dcast(HCI_RF, IDP~EUR15_GEN, value.var = "pres" ,fill = 0)
comm_FR_df <- data.frame(comm_FR)
rownames(comm_FR_df) <- comm_FR_df$IDP
comm_FR_df$IDP <- NULL

#method = exact
courb_acc_FA <- specaccum(comm_FA_df, method = "exact")
courb_acc_FR <- specaccum(comm_FR[,-"IDP"], method = "exact")

plot(courb_acc_FR, ci.type = "line", xlim=c(0,40000), col = "orange")
plot(courb_acc_FA, ci.type = "line", add = TRUE, col = "blue")
abline(v=14430, col = "lightgrey", lwd=1, lty=2)

#Figure 4 ####
#method = random
courb_acc_FA_r <- specaccum(comm_FA_df, method = "random", permutations = 1000)
courb_acc_FR_r <- specaccum(comm_FR[,-"IDP"], method = "random", permutations = 1000)

plot(courb_acc_FR_r, ci.type = "line", xlim=c(0,40000), col = "orange")
plot(courb_acc_FA_r, ci.type = "line", add = TRUE, col = "blue")
abline(v=14300, col = "lightgrey", lwd=1, lty=2)

max(courb_acc_FA_r$richness)#17 HCI max in AF
max(courb_acc_FR_r$richness)# 19 HCI max in RF

max(courb_acc_FA_r$sites) #35 556 sites max in AF
max(courb_acc_FR_r$sites) #27 788 sites max in RF

#REGIONAL SCALE ##########################################################################################################
#calculation of diversity indices

#Creating a table with a random sample to balance the number of plots in RF/AF
Nb_FR_perSER <- NFI_hab_histo[FORET_EM==0, .(FR = .N), by= .(SER)]
Nb_FA_perSER <- NFI_hab_histo[FORET_EM==1, .(FA = .N), by= .(SER)]
Nb_FA_FR_perSER <- merge(Nb_FA_perSER, Nb_FR_perSER, by = "SER")
#In minFAFR, we specify the type of forest with the fewest plots 
Nb_FA_FR_perSER[,min_EM:=ifelse(FA<FR, "1", "0")][,minFAFR:=ifelse(FA<FR, FA, FR)]
Nb_FA_FR_perSER[,max_EM:=ifelse(FA>FR, "1", "0")]
Nb_FA_FR_perSER[,diff_FA_FR := FA - FR]
nrow(Nb_FA_FR_perSER[min_EM == 1])#in 43/76 SER more plots in RF = 0.57
nrow(Nb_FA_FR_perSER[min_EM == 0])#in 33/76 SER more plots in AF = 0.43
sum(Nb_FA_FR_perSER$minFAFR)#29 848  * 2 = 59 696 plots in the data.table after the loop

#initialisation 
set.seed(12)
R=1000 #nb repetition

#merge to retrieve the information on where and how much to sample directly from NFI_hab_histo
NFI_hab_histo2 <- merge(NFI_hab_histo,Nb_FA_FR_perSER[,.(SER,minFAFR,max_EM)],by="SER",all.x=TRUE)
#We select only the type of historical use to sample (with the max plots)
NFI_hab_histo2 <- NFI_hab_histo2[FORET_EM==max_EM]

#sample R times from NFI_hab_histo the nb of observations corresponding to the minimum
#number of plots between RF and AF per SER
NFI_hab_histo_samp <- NFI_hab_histo2[, 
    rbindlist(
    lapply(seq_len(R), function(r) .SD[sample(.N,unique(.SD$minFAFR), replace = FALSE)][, rep := r]
    )), by=SER]

#creation of an intermediate table showing the number of each type of HCI per SER and iteration
int <- NFI_hab_histo_samp[HCI=="YES",.(N=.N),by=.(EUR15_GEN, SER, rep, FORET_EM)]

mean_indices_rep <- int[,.(FORET_EM=unique(FORET_EM), rich=length(unique(EUR15_GEN)), shannon=diversity(N, index = "shannon"),
                              simpson=diversity(N, index = "simpson"),
                              hill_2=diversity(N, index = "invsimpson")),by=.(SER,rep)]
mean_indices_rep[,":="(hill_1=exp(shannon), pielou=shannon/log(rich))][,hill_eq:=hill_1/rich]

mean_indices_rep

table(mean_indices_rep[pielou==Inf]$SER) #empty = OK!

Nb_FA_FR_perSER[minFAFR<30] 
#very few plot to sample : 2 or 27 in F13 and I13 respectively

#We calculate the average observation for each HCI by SER
mean_indices_SER <- mean_indices_rep[, .(av_rich=round(mean(rich)), av_shannon=mean(shannon),
                                                               av_simpson=mean(simpson), 
                                                               av_pielou=mean(pielou, na.rm=T),
                                         av_hill1=mean(hill_1), av_hill2=mean(hill_2, na.rm=T), 
                                         av_hill_eq=mean(hill_eq, na.rm=T)), by = .(SER, FORET_EM)]
 


#We then add the calculations for the indices to be used with the maximum number of plots
int_SER_EM <- NFI_hab_histo[HCI=="YES", .(Num = .N), by = .(SER, EUR15_GEN, FORET_EM)]
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
#we bind the two data.table together
mean_indices_SER_tot <- rbind(mean_indices_SER, indices_SER_min)
mean_indices_SER_tot[,FORET_EM:=ifelse(FORET_EM==0, "FR", "FA")]


#We study the correlation between indices
cor(mean_indices_SER_tot[,.(av_rich, av_shannon, av_simpson, av_pielou, av_hill1, av_hill2, av_hill_eq)])


#####################################################################################################################
################### Maps

#data.table with diversity indices
ind_div_hab <- unique(mean_indices_SER_tot[,.(SER, FORET_EM, av_shannon, av_simpson, av_rich, av_pielou, av_hill1, av_hill2, av_hill_eq)])
#wide format
ind_div_hab_w <- dcast(ind_div_hab, SER ~ FORET_EM, value.var = c("av_shannon", "av_simpson",
                                                                  "av_rich", "av_pielou",
                                                                  "av_hill1", "av_hill2",
                                                                  "av_hill_eq"))
#difference between AF and RF for each indice (FA-FR)
ind_div_hab_w[, ":="(diff_shannon = av_shannon_FA - av_shannon_FR, diff_simpson = av_simpson_FA - av_simpson_FR,
                     diff_rich = av_rich_FA - av_rich_FR, diff_pielou = av_pielou_FA - av_pielou_FR,
                     diff_hill1 = av_hill1_FA - av_hill1_FR, diff_hill2 = av_hill2_FA - av_hill2_FR,
                     diff_hill_eq = av_hill_eq_FA - av_hill_eq_FR)]

#Selecting the colour range
#shannon, pielou, simpson
col7 <- c("#be8a00","#f1b000", "#ffe08b", "#CCCCCC", "#8ba9ff", "#3f71ff" , "#0b4cff")
#hill et richesse
col_4_2 <- c("#8b6600", "#be8a00", "#f1b000", "#ffe08b", "#CCCCCC", "#8ba9ff", "#3f71ff")


##Shannon ####

#Number of region
nrow(ind_div_hab_w[diff_shannon<0])#66/76 higher indice in RF
nrow(ind_div_hab_w[diff_shannon>0])#10/76 higher indice in AF
nrow(ind_div_hab_w[diff_shannon==0])#0/76


#define classes for discrete-value plots
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
        legend.title = element_text(size = 11), legend.text = element_text(size = 11))+
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "grid", 
                         pad_x = unit(0.7, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_orienteering)
div_shannon

## simpson ####

#Number of regions
nrow(ind_div_hab_w[diff_simpson<0])#61/76 higher indice in RF
nrow(ind_div_hab_w[diff_simpson>0])#15/76 higher indice in AF
nrow(ind_div_hab_w[diff_simpson==0])#0/76

#define classes for discrete-value plots
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
        legend.title = element_text(size = 11), legend.text = element_text(size = 11))+
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "grid", 
                         pad_x = unit(0.7, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_orienteering)
div_simpson

## richness ####

#Number of region
nrow(ind_div_hab_w[diff_rich<0])#56/76 higher indice in RF
nrow(ind_div_hab_w[diff_rich>0])#6/76 higher indice in AF
nrow(ind_div_hab_w[diff_rich==0])#14/76

#define classes for discrete-value plots
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
        legend.title = element_text(size = 11), legend.text = element_text(size = 11))+
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "grid", 
                         pad_x = unit(0.7, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_orienteering)
div_rich

## pielou ####

#Number of regions
nrow(ind_div_hab_w[diff_pielou<0])#51/76 higher indice in RF
nrow(ind_div_hab_w[diff_pielou>0])#24/76 higher indice in AF
nrow(ind_div_hab_w[diff_pielou==0])#1/76

#define classes for discrete-value plots
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
        legend.title = element_text(size = 11), legend.text = element_text(size = 11))+
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "grid", 
                         pad_x = unit(0.7, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_orienteering)
div_pielou

################################################################
plot_grid(div_shannon, div_simpson, div_rich, div_pielou, ncol = 2, nrow = 2,
          labels = c("a) shannon", "b) simpson", "c) richness", "d) pielou"))

