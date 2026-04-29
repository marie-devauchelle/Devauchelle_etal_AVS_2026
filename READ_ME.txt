This repository stocks dataset and script from Devauchelle et al. Applied Vegetation Science 2026 
"Ancient forests contain large areas of Natura 2000 habitats while recent forests favor their diversity."
We evaluated the conservation value of ancient and recent forests through the concept of Habitat of Community Interest
defined by the Habitat Directive. Location : France.

The majority of our data are from the French National Institute for Geographic and forest information (IGN)
and are available online.
- The floristic dataset with blurred location at a 700 m radius is available at https://inventaire-forestier.ign.fr/dataifn/
- The vectorized historical map "General Staff map" (carte d'Etat-Major) is available
at: https://www.data.gouv.fr/datasets/bd-carto-r-etat-major
- The French forest map (BD FORET V2) is available at: https://cartes.gouv.fr/rechercher-une-donnee/dataset/IGNF_BD-FORET
For this study, the IGN provided us with a dataset cross-referencing the BD Forêt V2 map, the General Staff map,
and the unblurred geolocation coordinates of the NFI. Since this dataset does not belong to the authors of this paper, 
it is not freely available on this repository. However, we provide a script top cross-reference these maps using
the blurred dataset freely available online. 


- Analyis_relationship_HCI.R: Script that investigates the relationship between the distribution of habitat of community
interest type and temporal continuity. It allows for the creation of Figure 1.
- HCI_diversity.R: Script that investigates Difference in habitats of community interest diversity in ancient and
recent forests. Creation of Figure 4 and 5. 
- HCI_surface_area.R: Script that investigates difference in habitats of community interest surface area in ancient
and recent forests. Creation of Figure 2 and 3.
- EUR15_GEN_corresp_cahier_hab.txt: provides english names correspondences of HCIs codes. See Gégout, J., & Coudun, C. (2012) “The right relevé in the right vegetation unit: a new typicality index to reproduce expert judgement with an automatic classification programme.” Journal of Vegetation Science, 23(1), 24–32. https://doi.or/10.1111/j.1654-1103.2011.01337.x
- plots_classified_habitats.csv: output of the automatic classification program giving a phytosociological asssociation
to each NFI plot (based on its floristic survey). See " Maciejewski L., Pinto P.E., Wurpillot S., Drapier J., Cadet S., Muller S., Agou P., Renaux B., Gegout, J.-C. (2020) Vegetation unit assignments: phytosociology experts and classification programs show similar performance but low convergence. Applied Vegetation Science,23, 698-709. DOI: 10.1111/avsc.12516. "
- poids_SER.txt: provides a weighted surface area of each plot depedending on its forest region (SER). 