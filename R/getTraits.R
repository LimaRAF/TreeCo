#' @title Get TreeCo Species Traits
#'
#' @description To be included
#'
#' @param tree.data a data frame with the species names
#' @param spp.name character. The name of the column containing the
#'   species names. Defaults to "Name_submitted"
#' @param trait.cons a data frame with the extra, consensual species
#'   information
#' @param trait.sp data frame with the species trait information
#'   averaged by species
#' @param trait.gen data frame with the species trait information
#'   averaged by genus
#' @param trait.fam data frame with the species trait information
#'   averaged by family
#' @param generaliza a list containing the vectors of traits which can
#'   be generalized at genera and family levels, respectively.
#'   Defaults to list(c("wsg_gcm3","LeafType","SeedMass_g"),c("wsg_gcm3"))
#' @param add.spp data frame with species to be added to the trait
#'   retrieval
#' @param rm.colunas vector of extra columns names that will be
#'   removed from the original dataframe. Default is NULL but a vector
#'   is provided  with the most commonly removed columns
#' @param inc.colunas vector of columns names that should be included
#'   contained in the vector of often-excluded columns. Default is
#'   NULL (i.e. keep the vector of often-excluded columns the same)

#' @return To be described 
#'
#' @details To be included
#' 
#' @author Renato A. F. de Lima
#'
#' 
#' 
getTraits <- function(tree.data = NULL,
                      spp.name = "Name_submitted",
                      trait.cons = NULL,
                      trait.sp = NULL,
                      trait.gen = NULL,
                      trait.fam = NULL,
                      generaliza = list(c("wsg_gcm3","LeafType","SeedMass_g"),
                                        c("wsg_gcm3")),
                      add.spp = NULL,
                      rm.colunas = NULL,
                      inc.colunas = NULL) {
  
  ## Checking input
  
  
  # #### TRANFORMING THE VARIABLES ###
  # tree.data$ordem = as.character(tree.data$ordem)
  # tree.data$Name_submitted = as.character(tree.data$Name_submitted)
  # 
  # #### CREATING THE DOCUMENT WITH THE LIST OF DECISIONS TAKES AT EACH STEP ####
  # notas = NULL
  # 
  # #### CREATING THE LIST OF TAXA INSIDE THE ABUNDANCE DATASET ####
  # taxa = tree.data[,c("Name_submitted","family","genus","taxon.rank")] 
  # taxa = taxa[!duplicated(taxa$Name_submitted),]
  # taxa = taxa[order(taxa$Name_submitted),]
  # taxa = taxa[!taxa$taxon.rank %in% "unidentified",]
  # 
  # if(!is.null(add.spp)) {
  #   taxa1 = add.spp[,c("Name_submitted","family","genus","taxon.rank")]
  #   taxa = rbind.data.frame(taxa,taxa1)
  # }
  # 
  # ########################################################################
  # #### TREE TRAITS AVAILABLE FROM THE SOUTH AMERICA TREE SPECIES LIST ####
  # ########################################################################
  # #Info: threat status, endemism, establishment, dispersal syndrome and habit
  # names(trees.sa)[names(trees.sa)%in%"SpeciesKTSA"] = "Name_submitted"
  # taxa = merge(taxa,trees.sa[,c("Name_submitted","establishment.AF","establishment.BR","habito","succesional.group",
  #                               "dispersal.syndrome","geographical_distribution","status_final","status_brazil_2005",
  #                               "status_iucn_2005","life.form.reflora","habitat.reflora","vegtype.reflora",
  #                               "domain.reflora")],by="Name_submitted",all.x = TRUE)
  # 
  # #### TRAIT DATA EDITING ####
  # ## Species establishment (endemic or exotic/cultivated) in respect to the Atlantic Forest and to Brazil ##
  # taxa$establishment.AF[is.na(taxa$establishment.AF)&!is.na(taxa$establishment.BR)] = 
  #   taxa$establishment.BR[is.na(taxa$establishment.AF)&!is.na(taxa$establishment.BR)]
  # taxa$establishment.AF[taxa$establishment.AF%in%"check"&taxa$establishment.BR%in%c("native","cultivated")] = 
  #   taxa$establishment.BR[taxa$establishment.AF%in%"check"&taxa$establishment.BR%in%c("native","cultivated")]
  # taxa$establishment.AF[taxa$establishment.AF %in% c("cultivated","naturalized","not in Brazil")] = "exotic"
  # notas[1] = "The native/exotic classification is in respect to the Atlantic Forest and more broadly to Brazil"
  # 
  # ## Extinction threat level - Brazil + IUCN (global) ##
  # taxa$status_final[is.na(taxa$status_final)] = "not_evaluated_or_not_threatened"
  # taxa$extinction = as.factor(gsub("_iucn|_reflora|_author|_brasil","",taxa$status_final))
  # #CR=4, DD=0.5, EN=3, LC=0, NT=1, not_evaluated=0, VU=2
  # niveis = c(4,0.5,3,0,1,0,2)
  # names(niveis) = c("critically_endangered_CR","data_deficient_DD","endangered_EN","least_concern_LC","near_threatened_NT","not_evaluated_or_not_threatened","vulnerable_VU")
  # levels(taxa$extinction) = as.character(niveis[names(niveis) %in% levels(taxa$extinction)])
  # taxa$extinction = as.double(as.character(taxa$extinction))
  # taxa$extinction[taxa$taxon.rank %in% c("unidentified","genus","family")] = NA
  # taxa$status_final[taxa$taxon.rank %in% c("unidentified","genus","family")] = NA
  # notas = c(notas,paste("The values given for CR, DD, EN, LC, NT, NE, and VU were: ",paste(niveis, collapse=","),", respectively",sep=""))
  # 
  # ## Endemism level - in respect to the Atlantic Forest ##
  # #edtiting species geographical occurrence by genus
  # taxa$geographical_distribution[is.na(taxa$geographical_distribution) & taxa$genus %in% 
  #                                  c("Artocarpus","Citrus","Coffea","Camellia","Corymbia","Cupressus",
  #                                    "Dracaena","Eucalyptus","Eriobotrya","Hovenia","Leucaena","Ligustrum",
  #                                    "Mangifera","Mimusops","Morus","Pinus","Pittosporum","Platanus",
  #                                    "Ricinus","Spathodea","Syzygium","Tamarindus","Tecoma")] = "exotic"
  # taxa$geographical_distribution[is.na(taxa$geographical_distribution) & taxa$genus %in% 
  #                                  c("Neomitranthes","Salzmannia"
  #                                  )] = "eastern_south_america"
  # taxa$geographical_distribution[is.na(taxa$geographical_distribution) & taxa$genus %in% 
  #                                  c("Bocagea","Hornschuchia","Tovomitopsis","Urbanodendron","Behuria",
  #                                    "Macrotorus","Andradea","Ramisia","Macrothumia","Tripterodendron"
  #                                  )] = "regional_endemic"
  # taxa$geographical_distribution[is.na(taxa$geographical_distribution) & taxa$genus %in% 
  #                                  c("Kuhlmanniodendron","Paratecoma","Ophthalmoblapton","Arapatiella","Brodriguesia",
  #                                    "Grazielodendron","Harleyodendron","Hydrogaster","Curitiba","Riodocea","Melanopsidium",
  #                                    "Andreadoxa","Alatococcus","Trigoniodendron"
  #                                  )] = "local_endemic"
  # #More concise classification: C/E/N/W SA=1, exotic=-1, local=3, pan./neotropical=0, regional=2, SA=0 
  # taxa$endemism = as.factor(taxa$geographical_distribution)
  # niveis = c(1,1,-1,3,0,1,0,2,0,1,1,1)
  # names(niveis) = c("central_south_america","eastern_south_america","exotic","local_endemic","neotropical","northern_south_america",         
  #                   "pantropical","regional_endemic","south_american","south_and_central_south_america","southern_south_america","western_south_america")
  # levels(taxa$endemism) = as.character(niveis[names(niveis) %in% levels(taxa$endemism)])
  # taxa$endemism = as.double(as.character(taxa$endemism))
  # notas = c(notas,paste("The more concise values given for CSA, ESA, Exotic, Local endemic, Neotrop., NSA, Pantrop., Regional endemic, SA, SCSA, SSA and WSA were: ",paste(niveis, collapse=","),", respectively",sep=""))
  # #More detailed classification: C/E/N/W SA=2, exotic=-1, local=4, pan./neotropical=0, regional=3, SA=1
  # taxa$endemism1 = as.factor(taxa$geographical_distribution)
  # niveis = c(2,2,-1,4,0,2,0,3,0,1,2,2)
  # levels(taxa$endemism) = as.character(niveis[names(niveis) %in% levels(taxa$endemism)])
  # taxa$endemism = as.double(as.character(taxa$endemism))
  # notas = c(notas,paste("The more detailed values given for CSA, ESA, Exotic, Local endemic, Neotrop., NSA, Pantrop., Regional endemic, SA, SCSA, SSA and WSA were: ",paste(niveis, collapse=","),", respectively",sep=""))
  # #notas = c(notas,"")
  # 
  # ## Ecological/successional group ##
  # taxa$ecological.group = taxa$succesional.group
  # taxa$ecological.group[taxa$ecological.group %in% "climax"] = 4
  # taxa$ecological.group[taxa$ecological.group %in% "late_secondary"] = 3
  # taxa$ecological.group[taxa$ecological.group %in% c("early_secondary","early_seconday","early_secondary?")] = 2
  # taxa$ecological.group[taxa$ecological.group %in% c("pioneer")] = 1
  # taxa$ecological.group[taxa$ecological.group %in% c("non_pioneer")] = 2.5
  # taxa$ecological.group[taxa$ecological.group %in% c("light_demanding","pioneer?")] = 1.5
  # taxa$ecological.group[taxa$ecological.group %in% c("shade_tolerant","climax shade_tolerant","climax?")] = 3.5
  # taxa$ecological.group = suppressWarnings(as.double(taxa$ecological.group))
  # notas = c(notas,"The ecological group classification for Pioneer, Early_secondary, Late_secondary, and Climax species were 1, 2, 3, 4, respectively. In addition: Light-demanding= 1.5, Non-pioneer= 2.5 and Shade-tolerant= 3.5")
  # 
  # ## Automatic assignment of very common pioneer, early secondary, late secondary and climax genera
  # taxa$ecological.group[is.na(taxa$ecological.group) & taxa$genus %in% 
  #                         c("Trema","Celtis","Guazuma","Bixa","Dodonea","Heliocarpus","Tecoma","Ricinus","Leucaena",
  #                           "Manihot","Vernonanthura","Urera","Vasconcellea","Carica","Cecropia","Vismia","Chiococca",
  #                           "Cochlospermum","Commiphora","Curatella","Prosopis","Physocalymma","Wedelia","Chromolaena")] = 1
  # taxa$ecological.group[is.na(taxa$ecological.group) & taxa$genus %in% 
  #                         c("Lithraea","Vismia","Schinus","Moquiniastrum","Lithraea","Clethra","Aparisthmium","Schizolobium",
  #                           "Acrocomia","Schinopsis","Acca","Ligustrum","Alchornea","Erythrina","Annona","Sapium")] = 1.5
  # taxa$ecological.group[is.na(taxa$ecological.group) & taxa$genus %in% 
  #                         c("Zanthoxylum","Cedrela","Maclura","Enterolobium","Lonchocarpus","Lacistema","Tapirira",
  #                           "Cybistax","Prockia","Coutarea","Sparattosperma","Sapindus","Balfourodendron",
  #                           "Crateva","Hura","Simarouba","Plathymenia","Dasyphyllum","Peltophorum","Rhamnidium",
  #                           "Bowdichia","Basiloxylon","Pterogyne","Helietta","Tetrorchidium")] = 2
  # taxa$ecological.group[is.na(taxa$ecological.group) & taxa$genus %in% 
  #                         c("Blepharocalyx","Pimenta","Galipea","Senefeldera","Schoepfia","Tetrastylidium","Amphirrhox",
  #                           "Leretia","Platycyamus","Melanoxylon","Diploon","Sweetia")] = 3
  # notas = c(notas,"Missing classification of Ecological Groups for some species was carried at genus level for some genera typically pioneer (e.g. Trema, Celtis, Guazuma, Vernonanthura, Urera, Cecropia) or late_secondary (e.g. Blepharocalyx, Senefeldera, Schoepfia, Tetrastylidium, Amphirrhox, Platycyamus, Melanoxylon, Diploon, Sweetia)")
  # 
  # ## Final edits for ecological groups
  # taxa$ecological.group[is.na(taxa$ecological.group) & taxa$species.correct %in% 
  #                         c("Myrcia citrifolia","Myrceugenia ovalifolia","Aniba intermedia")] = 3
  # taxa$ecological.group[is.na(taxa$ecological.group) & taxa$species.correct %in% 
  #                         c("Citrus limon","Citrus sinensis","Coffea arabica","Bougainvillea praecox")] = 2
  # taxa$ecological.group[is.na(taxa$ecological.group) & taxa$species.correct %in% 
  #                         c("Crepidospermum atlanticum","Eugenia puberula")] = 2.5
  # ## Inspecting species still missing ecological groups info
  # #tmp = taxa[is.na(taxa$ecological.group)&!is.na(taxa$GS),]
  # #tmp1 = aggregate(tmp$N,list(tmp$species.correct),sum)
  # #tmp1$GS = aggregate(tmp$GS,list(tmp$species.correct),mean)$x
  # #tmp1$SuccesionalGroup = as.character(aggregate(tmp$SuccesionalGroup,list(tmp$species.correct),unique)$x)
  # #tmp1[order(tmp1$x),]
  # #tmp1[,c("species.correct","SuccesionalGroup","GS")]
  # #tmp1[tmp1$species.correct %in% c("Myrcia citrifolia","Myrceugenia ovalifolia","Crepidospermum atlanticum","Aniba intermedia"),c("species.correct","SuccesionalGroup","GS")]
  # 
  # 
  # ## Creating column for different life-forms (palms, ferns, etc)
  # taxa$life.form = NA
  # taxa$life.form[taxa$family %in% c("Arecaceae")] = "palm"
  # taxa$life.form[taxa$family %in% c("Araceae","Asparagaceae","Laxmanniaceae","Velloziaceae")] = "palmoids"
  # taxa$life.form[taxa$family %in% c("Cyatheaceae","Dicksoniaceae","Blechnaceae")] = "tree_fern"
  # taxa$life.form[taxa$family %in% c("Cactaceae")] = "succulent_tree"
  # taxa$life.form[taxa$family %in% c("Poaceae")] = "woody_bamboo"
  # taxa$life.form[taxa$family %in% c("Convolvulaceae","Cucurbitaceae","Menispermaceae","Smilacaceae",
  #                                   "Turneraceae","Vitaceae")] = "woody_vines_and_subshrubs"
  # #"Convolvulaceae" => "Ipomoea carnea"  "Ipomoea pinnata"    
  # #"Cucurbitaceae" => "Cayaponia tayuya" "Wilbrandia"   
  # #"Menispermaceae" => "Abuta grandifolia" "Cissampelos ovalifolia"    
  # #"Smilacaceae" => "Smilax fluminensis"
  # #"Turneraceae" =>  "Turnera blanchetiana" "Turnera calyptrocarpa" "Turnera diffusa" "Turnera macrophylla"
  # #"Vitaceae"=> "Cissus pulcherrima"
  # taxa$life.form[is.na(taxa$life.form)] = "woody_tree"
  # 
  # ## Dispersal syndromes ## 
  # #by families
  # taxa$dispersal.syndrome[!is.na(taxa$family) & is.na(taxa$dispersal.syndrome) & taxa$family %in% 
  #                           c("Myrtaceae","Lauraceae","Annonaceae","Chrysobalanaceae","Siparunaceae","Piperaceae",
  #                             "Aquifoliaceae","Erythroxylaceae","Arecaceae","Burseraceae","Clusiaceae")] = "zoochoric"
  # taxa$dispersal.syndrome[!is.na(taxa$Name_submitted) & grepl("Eucalyptus|Corymbia",taxa$Name_submitted)] = "anemochoric"
  # taxa$dispersal.syndrome[!is.na(taxa$family) & taxa$family %in% 
  #                           c("Cyatheaceae")] = "anemochoric" #Asteraceae,Bignoniaceae,Vochysiaceae??
  # 
  # #by genera
  # taxa$dispersal.syndrome[!is.na(taxa$genus) & is.na(taxa$dispersal.syndrome) & taxa$genus%in% 
  #                           c("Miconia","Leandra","Pisonia","Guapira","Myrsine","Pouteria","Solanum","Capsicum","Ficus","Mollinedia","Diospyros","Symplocos",
  #                             "Ardisia","Cabralea","Calophyllum","Cereus","Cheiloclinium","Diploon","Dipteryx","Lantana","Ossaea","Prockia","Sideroxylon","Anacardium","Emmotum","Hyeronima","Macropeplus","Meliosma",
  #                             "Pera","Pradosia","Pseudolmedia","Sacoglottis","Sessea","Simaba","Strychnos","Tapirira","Agonandra","Alchornea","Citronella","Heisteria","Henriettea","Jacaratia","Schinus","Tetrorchidium","Urera",
  #                             "Vantanea","Aureliana","Bunchosia","Cecropia","Celtis","Copaifera","Coussapoa","Humiriastrum","Pourouma","Prunus","Salacia","Sapium","Stylogyne","Talisia","Brunfelsia","Citharexylum","Dendropanax",
  #                             "Pilocarpus","Rauvolfia","Tabernaemontana","Zollernia","Cestrum","Chionanthus","Manilkara","Meriania","Micropholis","Ormosia","Virola","Lacistema","Sorocea","Vitex","Xylosma","Brosimum","Connarus",
  #                             "Euplassa","Guarea","Matayba","Schefflera","Aegiphila","Picramnia","Chrysophyllum","Daphnopsis","Styrax","Andira","Coccoloba","Cybianthus","Neea","Mouriri","Allophylus","Ouratea","Zanthoxylum",
  #                             "Cupania","Byrsonima","Casearia","Maytenus","Trichilia","Swartzia","Inga","Rubus","Citrus",
  #                             "Alibertia","Amaioua","Chiococca","Chomelia","Cordiera","Coussarea","Faramea","Genipa","Guettarda","Ixora","Kutchubaea","Palicourea","Posoqueria","Psychotria","Randia","Rudgea","Tocoyena",
  #                             "Vismia","Quiina","Quararibea","Phytolacca","Homalolepis","Simaba","Capparidastrum","Neocalyptrocalyx","Athenaea","Monteverdia"
  #                           )] = "zoochoric"
  # taxa$dispersal.syndrome[!is.na(taxa$genus) & is.na(taxa$dispersal.syndrome) & taxa$genus%in% 
  #                           c("Gymnanthes","Galipea","Jatropha","Pachystroma","Sebastiania","Cnidoscolus","Calliandra","Cassia","Esenbeckia","Helicteres","Mabea","Metrodorea","Mimosa","Actinostemon","Manihot","Senegalia",
  #                             "Senna","Croton","Senefeldera","Algernonia","Brasiliocroton","Stillingia"
  #                           )] = "autochoric"
  # taxa$dispersal.syndrome[!is.na(taxa$genus) & is.na(taxa$dispersal.syndrome) & taxa$genus%in% 
  #                           c("Adenocalymma","Pachira","Peltophorum","Plathymenia","Raulinoreitzia","Bougainvillea","Cedrela","Hymenolobium","Myrocarpus","Pinus","Pterodon","Ruprechtia","Alsophila","Lafoensia","Moquiniastrum","Seguieria",
  #                             "Symphyopappus","Triplaris","Callisthene","Centrolobium","Pterocarpus","Astronium","Cariniana","Lonchocarpus","Pseudobombax","Ceiba","Luehea","Tabebuia","Tachigali","Vernonanthura","Eriotheca","Terminalia",
  #                             "Handroanthus","Piptocarpha","Jacaranda","Cyathea","Dalbergia","Qualea","Tibouchina","Handroanthus","Vochysia","Baccharis","Kielmeyera","Aspidosperma","Machaerium","Vernonia","Eupatorium",
  #                             "Alseis","Bathysa","Coutarea","Rustia","Simira","Combretum","Luetzelburgia","Pleroma"
  #                           )] = "anemochoric"
  # 
  # #tmp = taxa[is.na(taxa$dispersal.syndrome),]
  # #tmp = tmp[!grepl(" ",tmp$Name_submitted),]
  # #sort(table(tmp$Name_submitted))
  # #sort(table(tmp$genus))
  # notas = c(notas,"Missing classification of Dispersal Syndrome for some species or morphospecies was carried at genus or family level (e.g. all Miconia are zoochorich and all ferns are anemochoric")
  # 
  # ####################################################################
  # #### MEAN TREE TRAITS AVAILABLE FROM THE TREECO TRAIT DATABASE  ####
  # ####################################################################
  # 
  # #### MERGING TRAIT AT SPECIES LEVEL ####
  # tmp = trait.spp[trait.spp$TAX %in% taxa$Name_submitted,]
  # taxa = merge(taxa,tmp,by.x="Name_submitted",by.y="TAX",all.x=T)
  # 
  # ##CHECK!!
  # #Checkings possible problems
  # #tax[!is.na(tax$wsg_gcm3)&tax$wsg_gcm3>1&tax$GS<2,c("species.correct","wsg","wsg_gcm3","SuccesionalGroup","GS")]
  # #tax[!is.na(tax$wsg_gcm3)&tax$wsg_gcm3<0.6&tax$GS>=3.5,c("species.correct","wsg","wsg_gcm3","SuccesionalGroup","GS")]
  # 
  # #### PRELIMINAR EDITING OF TRAIT DATA ####
  # #### UPDATE THIS PART OF THE CODE FROM THE CODES OF THE PAPER SUBMITTED TO SCIENCE ####
  # ## Seed mass ~ Seed volume
  # taxa$SeedVolume = NA
  # #for circular fruits
  # taxa$SeedVolume[is.na(taxa$SeedLength_cm)&is.na(taxa$SeedWidth_cm)&!is.na(taxa$SeedDiameter_cm)] = 
  #   (4/3)*pi*(as.double(taxa$SeedDiameter_cm[is.na(taxa$SeedLength_cm)&is.na(taxa$SeedWidth_cm)&!is.na(taxa$SeedDiameter_cm)])/2)^3
  # #for ellipsoidal fruits
  # taxa$SeedVolume[!is.na(taxa$SeedLength_cm)&!is.na(taxa$SeedWidth_cm)&!is.na(taxa$SeedDiameter_cm)] = 
  #   (4/3)*pi*(as.double(taxa$SeedDiameter_cm[!is.na(taxa$SeedLength_cm)&!is.na(taxa$SeedWidth_cm)&!is.na(taxa$SeedDiameter_cm)])/2)*
  #   (as.double(taxa$SeedWidth_cm[!is.na(taxa$SeedLength_cm)&!is.na(taxa$SeedWidth_cm)&!is.na(taxa$SeedDiameter_cm)])/2)*
  #   (as.double(taxa$SeedLength_cm[!is.na(taxa$SeedLength_cm)&!is.na(taxa$SeedWidth_cm)&!is.na(taxa$SeedDiameter_cm)])/2)
  # #plot(log(taxa$SeedMass_g) ~ log(taxa$SeedVolume)); abline(0,1,lwd=2,col=2)
  # #text(x=log(taxa$SeedVolume), y=log(taxa$SeedMass_g), labels=taxa$Name_submitted,cex=0.5,pos=1:3)
  # 
  # tmp = na.omit(taxa[,c("Name_submitted","SeedMass_g","SeedVolume")])
  # mod = lm(log(as.double(tmp$SeedMass_g[!tmp$Name_submitted %in% c("Miconia prasina","Myrcia pubipetala")])) ~ log(as.double(tmp$SeedVolume[!tmp$Name_submitted %in% c("Miconia prasina","Myrcia pubipetala")])))
  # #abline(mod)
  # ## CHECK!!
  # ##RE-CHECK THE EQUATION USING ONLY SPECIES WITH ALL SEED INFO (RAW TRAIT VALUES): length/width and/or diameter and seed mass
  # taxa$est_SeedMass_g = exp(coef(mod)[1] + coef(mod)[2]*log(taxa$SeedVolume))
  # #plot(taxa$est_SeedMass_g ~ taxa$SeedMass_g, log="xy"); abline(0,1,lwd=2,col=2)
  # #taxa$SeedMass_g[is.na(taxa$SeedMass_g)&!is.na(taxa$est_SeedMass_g)] = 
  # #  taxa$est_SeedMass_g[is.na(taxa$SeedMass_g)&!is.na(taxa$est_SeedMass_g)]
  # 
  # #seed mass <- 0.8*fruits/kg
  # #plot(SeedMass_g ~ FruitMass_g, data=trait.spp, log="xy",
  # #     ylim=c(6.150288e-06, 1.157476e+02),
  # #     xlim=c(0.00001, 850)); abline(0,1,lwd=2,col=2)
  # #tmp = na.omit(trait.spp[,c("SeedMass_g","FruitMass_g")])
  # #mod = lm(log(tmp$SeedMass_g) ~ log(tmp$FruitMass_g))
  # #abline(mod)
  # #abline(-0.5,0.85)
  # #text(x=trait.taxa$FruitMass_g, y=trait.taxa$SeedMass_g, labels=trait.taxa$TAX,cex=0.4,pos=1:3)
  # ##RE-CHECK THE EQUATION USING ONLY SPECIES WITH ALL SEED INFO (RAW TRAIT VALUES): fruit mass and seed mass
  # taxa$est_SeedMass_g[is.na(taxa$est_SeedMass_g)] = exp(coef(mod)[1] + coef(mod)[2]*log(taxa$FruitMass_g[is.na(taxa$est_SeedMass_g)]))
  # taxa$SeedMass_g[is.na(taxa$SeedMass_g)&!is.na(taxa$FruitMass_g)] = 
  #   taxa$est_SeedMass_g[is.na(taxa$SeedMass_g)&!is.na(taxa$FruitMass_g)]
  # notas = c(notas,"Missing information of Seed Mass was also obtained from studies providing values of number of seeds per kilogram. In additin, for some monospermic species was based on a conversion formula using Fruit_mass as an input")
  # 
  # #### MERGING TRAIT AT GENUS LEVEL ####
  # gen = as.character(trait.gen$TAX[trait.gen$TAX %in% taxa$genus])
  # trait = generaliza[[1]]  
  # for(j in 1:length(gen)) {
  #   tmp = taxa[taxa$genus %in% gen[j],]
  #   for(i in 1:length(trait)) {
  #     tmp[is.na(tmp[,trait[i]]),trait[i]] = as.character(trait.gen[as.character(trait.gen$TAX) %in% gen[j],trait[i]])
  #   }
  #   taxa[taxa$genus %in% gen[j],trait] = tmp[,trait]
  # }
  # notas = c(notas,paste("Missing info at species level was replaced by genus-level averages for the following traits: ",paste(generaliza[[1]], collapse=","),".",sep=""))
  # 
  # #### MERGING TRAIT AT FAMILY LEVEL ####
  # fam = as.character(trait.fam$TAX[trait.fam$TAX %in% taxa$family])
  # trait = generaliza[[2]] 
  # for(j in 1:length(fam)) {
  #   tmp = taxa[taxa$family %in% fam[j],]
  #   for(i in 1:length(trait)) {
  #     tmp[is.na(tmp[,trait[i]]),trait[i]] = as.character(trait.fam[as.character(trait.fam$TAX) %in% fam[j],trait[i]])
  #   }
  #   taxa[taxa$family %in% fam[j],trait] = tmp[,trait]
  # }
  # notas = c(notas,paste("Missing info at species level was replaced by family-level averages for the following traits: ",paste(generaliza[[2]], collapse=","),".",sep=""))
  # 
  # #### EDITING MISSING TRAIT DATA FOR ABUNDANT OR FREQUENT SPECIES IN TREECO ###
  # ## Maximum Height (m) ##
  # taxa$MaxHeight_m[!is.na(taxa$Name_submitted) & taxa$Name_submitted == "Vellozia variabilis"] = 2
  # taxa$MaxHeight_m[!is.na(taxa$Name_submitted) & taxa$Name_submitted == "Vellozia tubiflora"] = 2
  # taxa$MaxHeight_m[!is.na(taxa$Name_submitted) & taxa$Name_submitted == "Ipomoea carnea"] = 2.5
  # taxa$MaxHeight_m[!is.na(taxa$Name_submitted) & taxa$Name_submitted == "Erythroxylum nummularium"] = 4
  # taxa$MaxHeight_m[!is.na(taxa$Name_submitted) & taxa$Name_submitted == "Cratylia mollis"] = 4
  # taxa$MaxHeight_m[!is.na(taxa$Name_submitted) & taxa$Name_submitted == "Hyptis pachyphylla"] = 3
  # taxa$MaxHeight_m[!is.na(taxa$Name_submitted) & taxa$Name_submitted == "Croton adenodontus"] = 3
  # taxa$MaxHeight_m[!is.na(taxa$Name_submitted) & taxa$Name_submitted == "Eriope exaltata"] = 6
  # taxa$MaxHeight_m[!is.na(taxa$Name_submitted) & taxa$Name_submitted == "Simaba ferruginea"] = 13
  # taxa$MaxHeight_m[!is.na(taxa$Name_submitted) & taxa$Name_submitted == "Homalolepis ferruginea"] = 13
  # taxa$MaxHeight_m[!is.na(taxa$Name_submitted) & taxa$Name_submitted == "Erythroxylum pauferrense"] = 6
  # taxa$MaxHeight_m[!is.na(taxa$Name_submitted) & taxa$Name_submitted == "Ephedranthus dimerus"] = 20
  # taxa$MaxHeight_m[!is.na(taxa$Name_submitted) & taxa$Name_submitted == "Myrcia costeira"] = 9
  # taxa$MaxHeight_m[!is.na(taxa$Name_submitted) & taxa$Name_submitted == "Psychotria pubigera"] = 4
  # 
  # ## Getting leaf Length and Width from the speciesLink measurements ##
  # #  tmp = read.csv("C://Users//renat//Documents//raflima//Pos Doc//Databases//Species traits//Atributos das espécies//leaf_size_speciesLink.csv",as.is=T)
  # tmp = read.csv("E:/WD Backup.swstor/renato/NmQwODhiMTFiOWQ3NDcxOW/Volume{258b8dde-ad01-4a49-b95b-eb77d73987c4}//Users//renat//Documents//raflima//Pos Doc//Databases//Species traits//Atributos das especies//leaf_size_speciesLink.csv",as.is=T)
  # 
  # 
  # tmp = tmp[!is.na(tmp$Length),]
  # tmp1 = cbind.data.frame(aggregate(tmp$Length,list(tmp$species),mean),
  #                         W=aggregate(tmp$Width,list(tmp$species),mean)$x)
  # names(tmp1)[1:2] = c("species","L")
  # tmp1$LA = pi*tmp1$L*tmp1$W/4
  # notas = c(notas,"Missing information of Leaf Area was obtained from Leaf_length and Leaf_width using the formula of an ellipse: pi*L*W/4")
  # 
  # taxa$LeafArea[!is.na(taxa$Name_submitted)&taxa$Name_submitted == "Astrocaryum aculeatissimum"] = tmp1$LA[tmp1$species == "Astrocaryum aculeatissimum"]
  # taxa$LeafArea[!is.na(taxa$Name_submitted)&taxa$Name_submitted == "Euterpe edulis"] = tmp1$LA[tmp1$species == "Euterpe edulis"]
  # taxa$LeafArea[!is.na(taxa$Name_submitted)&taxa$Name_submitted == "Marlierea racemosa"] = tmp1$LA[tmp1$species == "Marlierea racemosa"]
  # taxa$LeafArea[!is.na(taxa$Name_submitted)&taxa$Name_submitted == "Myrcia aethusa"] = tmp1$LA[tmp1$species == "Myrcia aethusa"]
  # taxa$LeafArea[!is.na(taxa$Name_submitted)&taxa$Name_submitted == "Myrcia pubipetala"] = tmp1$LA[tmp1$species == "Myrcia pubipetala"]
  # taxa$LeafArea[!is.na(taxa$Name_submitted)&taxa$Name_submitted == "Pachystroma longifolium"] = tmp1$LA[tmp1$species == "Pachystroma longifolium"]
  # taxa$LeafArea[!is.na(taxa$Name_submitted)&taxa$Name_submitted == "Astrocaryum aculeatissimum"] = tmp1$LA[tmp1$species == "Astrocaryum aculeatissimum"]
  # 
  # ## Editing LeafType data ##
  # taxa$LeafType[grepl("compostas",taxa$LeafType)&!grepl("simples",taxa$LeafType)] = "compostas"
  # taxa$LeafType[!grepl("compostas",taxa$LeafType)&grepl("simples",taxa$LeafType)] = "simples"
  # taxa$LeafType[!grepl("compostas",taxa$LeafType)&!grepl("simples",taxa$LeafType)] = NA
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Annona",taxa$Name_submitted)] = "simples"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Bauhinia rufa",taxa$Name_submitted)] = "simples"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Cheiloclinium",taxa$Name_submitted)] = "simples"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Conchocarpus fontanesianus",taxa$Name_submitted)] = "simples"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Conchocarpus gaudichaudianus",taxa$Name_submitted)] = "simples"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Conchocarpus pentandrus",taxa$Name_submitted)] = "simples"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Esenbeckia grandiflora",taxa$Name_submitted)] = "simples"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Matayba",taxa$Name_submitted)] = "compostas"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Pseudobombax simplicifolium",taxa$Name_submitted)] = "simples"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Roupala montana",taxa$Name_submitted)] = "simples"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Swartzia simplex",taxa$Name_submitted)] = "simples"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Tabebuia insignis",taxa$Name_submitted)] = "compostas"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Vitex mexiae",taxa$Name_submitted)] = "compostas"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Weinmannia humilis",taxa$Name_submitted)] = "compostas"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Aralia warmingiana",taxa$Name_submitted)] = "compostas"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Eriotheca pubescens",taxa$Name_submitted)] = "compostas"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Malouetia cestroides",taxa$Name_submitted)] = "simples"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Joannesia princeps",taxa$Name_submitted)] = "compostas"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Malouetia cestroides",taxa$Name_submitted)] = "simples"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Piptocarpha axillaris|Piptocarpha macropoda|Piptocarpha regnellii",taxa$Name_submitted)] = "simples"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Protium heptaphyllum|Protium spruceanum|Protium widgrenii",taxa$Name_submitted)] = "compostas"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Schefflera calva|Schefflera macrocarpa",taxa$Name_submitted)] = "compostas"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Tabebuia roseoalba|Tabebuia aurea",taxa$Name_submitted)] = "compostas"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Vitex cymosa|Vitex megapotamica|Vitex polygama",taxa$Name_submitted)] = "compostas"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("Zeyheria montana|Zeyheria tuberculosa",taxa$Name_submitted)] = "compostas"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("\\|",taxa$LeafType)&grepl("Actinostemon",taxa$Name_submitted)] = "simples"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("\\|",taxa$LeafType)&grepl("Allophylus",taxa$Name_submitted)] = "compostas"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("\\|",taxa$LeafType)&grepl("Callisthene",taxa$Name_submitted)] = "simples"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("\\|",taxa$LeafType)&grepl("Celtis",taxa$Name_submitted)] = "simples"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("\\|",taxa$LeafType)&grepl("Dendropanax",taxa$Name_submitted)] = "simples"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("\\|",taxa$LeafType)&grepl("Faramea",taxa$Name_submitted)] = "simples"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("\\|",taxa$LeafType)&grepl("Jacaranda",taxa$Name_submitted)] = "compostas"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&grepl("\\|",taxa$LeafType)&grepl("Sebastiania",taxa$Name_submitted)] = "simples"
  # taxa$LeafType[grepl("compostas",taxa$LeafType)&grepl("simples",taxa$LeafType)] = NA
  # taxa$LeafArea[is.infinite(taxa$LeafArea)] = NA
  # 
  # #some final edits at genus or family level
  # taxa$LeafType[!is.na(taxa$family)&is.na(taxa$LeafType)&grepl("Annonaceae|Melastomataceae|Myrtaceae",taxa$family)] = "simples"
  # taxa$LeafType[!is.na(taxa$Name_submitted)&is.na(taxa$LeafType)&grepl("Beilschmiedia|Mezilaurus",taxa$Name_submitted)] = "simples"
  # 
  # #some final edits at species level (looked up in herbarium vouchers)
  # taxa$LeafType[!is.na(taxa$Name_submitted) & is.na(taxa$LeafType) & taxa$Name_submitted %in% 
  #                 c("Bauhinia_longifolia","Cavanillesia_umbellata","Guapira_areolata","Pogonophora_schomburgkiana"
  #                 )] = "simples"
  # taxa$LeafType[!is.na(taxa$Name_submitted) & is.na(taxa$LeafType) & taxa$Name_submitted %in% 
  #                 c("Caryocar_brasiliense","Cenostigma_bracteosum","Connarus_suberosus","Esenbeckia_febrifuga","Pachira_endecaphylla",
  #                   "Galipea_jasminiflora","Protium_atlanticum","Protium_icicariba","Sparattosperma_leucanthum"
  #                 )] = "compostas"
  # notas = c(notas,"Missing information of Leaf Type was obtained at genus-level for some taxa. Species with both compound and simple leafs along their onthogeny (e.g. Matayba, Roupala) were treated as having compound leaves")
  # 
  # ### Final edits: Seed size
  # taxa$SeedMass_g[is.na(taxa$SeedMass_g) & taxa$genus %in% c("Actinostemon")] = mean(as.double(unique(taxa$SeedMass_g[taxa$genus %in% c("Sebastiania")])))
  # taxa$SeedMass_g[is.na(taxa$SeedMass_g) & taxa$genus %in% c("Athenaea")] = mean(as.double(unique(taxa$SeedMass_g[taxa$genus %in% c("Capsicum")])))
  # taxa$SeedMass_g[is.na(taxa$SeedMass_g) & taxa$genus %in% c("Anaxagorea")] = mean(as.double(unique(taxa$SeedMass_g[taxa$genus %in% c("Xylopia")])))
  # 
  # ### Edits for very species rich genera without habit
  # ##VERIFICAR A PROPORÇÃO DE ÁRVORES E ARBUSTOS POR FAMILIA E GENERO PARA FECHAR AS LISTAGEMS ABAIXO, PRINCIPALMENTE GENEROS
  # #By family
  # taxa$habito[!is.na(taxa$family) & is.na(taxa$habito) & taxa$family%in% 
  #               c("Annonaceae","Burseraceae","Chrysobalanaceae","Lauraceae","Lecythidaceae","Moraceae","Myristicaceae","Myrtaceae","Proteaceae","Sapotaceae"
  #               )] = 1
  # taxa$habito[!is.na(taxa$family) & is.na(taxa$habito) & taxa$family%in% 
  #               c("Lacistemataceae","Loganiaceae","Pipeaceae","Siparunaceae","Violaceae"
  #               )] = 0.5
  # #By genus
  # taxa$habito[!is.na(taxa$genus) & is.na(taxa$habito) & taxa$genus%in% 
  #               c("Alchornea","Anadenanthera"
  #               )] = 1
  # taxa$habito[!is.na(taxa$genus) & is.na(taxa$habito) & taxa$genus%in% 
  #               c("Athenaea","Brunfelsia","Capsicum","Piper","Eumachia","Chiococca","Stachytarpheta","Schweiggeria"
  #               )] = 0.5
  # 
  # #### CHECKING COMMON SPECIES STILL WITHOUT TRAIT INFO ####
  # tmp = c("establishment.AF","habito","extinction","endemism","ecological.group","dispersal.syndrome",
  #         "wsg_gcm3","MaxHeight_m","MaxDbh_cm",
  #         "SeedLength_cm","SeedWidth_cm","SeedDiameter_cm","SeedMass_g","FruitMass_g",
  #         "LeafArea","LeafType","SuccesionalGroup",
  #         "Deciduousness","LT","PericarpConsistency","SexualSystem","PollinationSyndrome","SLA_cm2_g.1")
  # tmp1= NULL
  # for(i in 1:length(tmp)) {
  #   vrvl = tmp[i]
  #   if(is.numeric(taxa[,vrvl])) { tmp2 = taxa$Name_submitted[is.na(taxa[,vrvl])] 
  #   } else { tmp2 = taxa$Name_submitted[is.na(taxa[,vrvl])|(grepl("NA:",taxa[,vrvl])&!grepl("\\|",taxa[,vrvl]))]
  #   }
  #   tmp2 = tmp2[!grepl(" sp\\.",tmp2)&!is.na(tmp2)]
  #   tmp2 = tmp2[grepl(" ",tmp2)]
  #   if(is.null(tmp1)) { tmp1 = c(length(table(tmp2)),100*sum(table(tmp2))/dim(taxa)[1]) 
  #   } else { tmp1 = rbind.data.frame(tmp1,c(length(table(tmp2)),100*sum(table(tmp2))/dim(taxa)[1])) }
  # }
  # names(tmp1) =c("Nsp_without","prop.sp.without")
  # rownames(tmp1) = tmp
  # print("Check the number and proportion of species (not genera or families) without trait information.")
  # print("Proportion below 40% generally results in good coverage for all individuals at the community level.")
  # print(tmp1[order(tmp1$prop.sp.without),])
  # 
  # #### FILTERING THE SELECTED METADATA (COLUMNS) ####
  # colunas = c("X","species.original","Habit","Bark","LeafMargin","LeafTip","AcumenLength_cm","LeafSubtype","Leaflets",
  #             "LeafHairiness_upper","LeafHairiness_lower","FruitLipids","FruitWater","FruitProtein","FruitCarbo",
  #             "PollinationSubsyndrome","Anthesis","FlowerType","CorollaType","CorollaColour","SepalColour","CorollaLength_cm","CorollaWidth_cm",
  #             "ChemicalCompounds","PhysicalCompounds","Allelopathy","sem.kg","Flowers","Fruits","Seeds","LeafN","obs1","obs2","obs3",
  #             "GeogDist","Domain","Vegetation","TaxNotes","MinLeafLength_cm","MaxLeafLength_cm","MinLeafWidth_cm","MaxLeafWidth_cm",
  #             "LeafThickness_micro","FruitLength_cm","FruitWidth_cm","FruitDiameter_cm","LeafChlorophyllAB","LeafCarbon","LeafWater",
  #             "dry_mass_fraction","WoodCarbon_gkg.1","LeafDryMatter_mg.g","ChlorophyllContent_FCI","LeafTough_N.mm","PithProp","XylProp","BarkProp",
  #             "BarkProp","VesselDensity_n.mm2","VesselDiameter_microm","StomataDensity_n.mm2","StomataLength_microm","Kpot_kg.m.s.Mpa",
  #             "SuccesionalGroup","wsg")
  # if(is.null(rm.colunas)) {colunas1 = colunas}  else { colunas1 = c(colunas,rm.colunas) }
  # if(is.null(inc.colunas)) {colunas1 = colunas1}  else { colunas1 = colunas1[!colunas1 %in% inc.colunas] }
  # if(!is.null(colunas1)) taxa = taxa[,!names(taxa) %in% colunas1]
  # 
  # #### PREPARING AND RETURNING THE FINAL RESULTS ####
  # notas = gsub("\\\n","",notas)
  # notas1 = paste(">>>NOTE ",1:length(notas),": ",notas,sep="")
  # resulta = list(taxa, notas1)
  # names(resulta) = c("data_frame","editing_notes")
  # return(resulta)
  # 
  
}
  