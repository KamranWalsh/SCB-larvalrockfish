#relative abundance
library(ggplot2)
library(ggpubr)

ontogenyabundance <- data.frame(GrowthStage1=rep(c('Preflexion', 'Flexion', 'Postflexion'), each=11),
                                Prey=rep(c('Calanoida Copepodites', 'Calanoida Nauplii', 'Cladoceran', 'Copepoda Copepodites', 'Copepoda Nauplii',
                                           'Cyclopoida Copepodites', 'Cyclopoida Nauplii', 'Egg',  'Euphausiid', 'Euphausiid Nauplii', 'Poecilostomatoida Copepodites'), times=3),
                                Biomass = c(
                                  1.824817518,	47.44525547,	0,	0.364963504,	8.02919708,	0,	33.57664234,	7.664233577,	0,	0,	1.094890511,
                                  13.10638298,	44.25531915,	0.595744681,	1.957446809,	5.957446809,	1.872340426,	29.0212766,	2.723404255, 0	,0.425531915,	0.085106383,
                                  46.75324675,	22.20160792,	0.680272109,	3.277674706,	4.514533086,	4.823747681,	12.49226964,	4.205318491,	0.865800866, 0.061842919,	0.123685838
                                ))

level_order1 <- c('Preflexion', 'Flexion', 'Postflexion')
abundanceplot <- ggplot(ontogenyabundance, aes(fill=Prey, y=Biomass, x=factor(GrowthStage1, levels = level_order1))) +    
  geom_bar(position='stack', stat='identity') + xlab("Growth Stage") + ylab("% Abundance of Prey Taxa") + theme_classic() +
  scale_fill_manual(values = c("lightblue", "cornflowerblue", "dodgerblue4", "lightgreen", "springgreen3", "lightgoldenrod2", "tomato1",  "plum3", "mediumpurple3","indianred3", "sienna4")) +
  theme(title =element_text(size=14), axis.title=element_text(size=8), axis.text.y = element_text(size = 8), axis.text.x = element_text(size = 8))     
#abundanceplot
abundanceplotnolegend <- abundanceplot + theme(legend.position = "none")
#ggsave("ontogenyabundance.jpg", width = 3.5, height = 4, dpi = 300)

#relative biomass

ontogenybiomass <- data.frame(GrowthStage1=rep(c('Preflexion', 'Flexion', 'Postflexion'), each=11),
                              Prey=rep(c('Calanoida Copepodites', 'Calanoida Nauplii', 'Cladoceran', 'Copepoda Copepodites', 'Copepoda Nauplii', 
                                         'Cyclopoida Copepodites', 'Cyclopoida Nauplii', 'Egg', 'Euphausiid', 'Euphausiid Nauplii', 'Poecilostomatoida Copepodites'), times=3),
                              Biomass=c(
                                3.768054371,	39.03209197,	0,	0.899404673	,8.580347436,	0	,19.78525317,	22.87756789,	0,	0	,5.057280492,
                                37.38683101,	31.52802132,	0.355941454,	2.864498247,	2.55038271,	3.328803693,	12.68019613,	8.233103391,	0, 0.91857729,	0.153644746,
                                63.88295667,	3.618475691,	0.261970478,	2.478851341,	2.030988704,	3.562511491,	1.231116253,	1.764340838, 21.08407997,	0.025336174,	0.059372387
                              ))

level_order1 <- c('Preflexion', 'Flexion', 'Postflexion')
biomassplot <- ggplot(ontogenybiomass, aes(fill=Prey, y=Biomass, x=factor(GrowthStage1, levels = level_order1))) +    
  geom_bar(position='stack', stat='identity') + xlab("Growth Stage") + ylab("% Biomass of Prey Taxa") + 
  theme_classic() + scale_fill_manual(values = c("lightblue", "cornflowerblue", "dodgerblue4", "lightgreen", "springgreen3", "lightgoldenrod2", "tomato1", "plum3", "mediumpurple3","indianred3", "sienna4")) + 
  theme(title =element_text(size=14), axis.title=element_text(size=8), axis.text.y = element_text(size = 8), axis.text.x = element_text(size = 8))     
#biomassplot
biomassplotnolegend <- biomassplot + theme(legend.position = "none")

#IRI abundance

percentIRIabundance <- data.frame(GrowthStage3=rep(c('Preflexion', 'Flexion', 'Postflexion'), each=11),
                                  Prey=rep(c('Calanoida Copepodites', 'Calanoida Nauplii', 'Cladoceran', 'Copepoda Copepodites', 'Copepoda Nauplii',
                                             'Cyclopoida Copepodites', 'Cyclopoida Nauplii', 'Egg', 'Euphausiid', 'Euphausiid Nauplii', 'Poecilostomatoida Copepodites'), times=3),
                                  IRI=c(
                                    0.212698075,	58.0665745,	0, 0.010634904,	3.041582474, 0,	35.22280123,	3.349994683, 0, 0,	0.095714134,
                                    8.847955151,	53.41504605,	0.048749064,	0.600658112,	3.534307154,	0.612845378,	32.05947386	,0.835698243, 0,	0.04352595, 0.001741038,
                                    55.79335793,	23.34036198,	0.096643824,	1.862590054,	3.719908628,	3.015287296,	10.29344579,	1.672816728, 0.196801968,	0.00175716,	0.007028642
                                  )) 
level_order3 <- c('Preflexion', 'Flexion', 'Postflexion')
IRIabundanceplot2 <- ggplot(percentIRIabundance, aes(fill=Prey, y=IRI, x=factor(GrowthStage3, levels = level_order3))) + geom_bar(stat="identity")
IRIabundance <- IRIabundanceplot2 + xlab("Growth Stage") + ylab("Index of Relative Importance (%) Abundance") + theme_classic() + 
  scale_fill_manual(values = c("lightblue", "cornflowerblue", "dodgerblue4", "lightgreen", "springgreen3", "lightgoldenrod2", "tomato1", "plum3", "mediumpurple3", "indianred3", "sienna4")) + 
  theme(title =element_text(size=14), axis.title=element_text(size=8), axis.text.y = element_text(size = 8), axis.text.x = element_text(size = 8)) 
#IRIabundance
IRIabundanceplotnolegend <- IRIabundance + theme(legend.position = "none")

#IRI biomass 

percentIRIbiomass <- data.frame(GrowthStage3=rep(c('Preflexion', 'Flexion', 'Postflexion'), each=11),
                                Prey=rep(c('Calanoida Copepodites', 'Calanoida Nauplii', 'Cladoceran', 'Copepoda Copepodites', 'Copepoda Nauplii', 
                                           'Cyclopoida Copepodites', 'Cyclopoida Nauplii', 'Egg', 'Euphausiid', 'Euphausiid Nauplii', 'Poecilostomatoida Copepodites'), times=3),
                                IRI=c(
                                  0.420504465,	56.62635817, 0,	0.033457026,	3.830172342, 0,	25.75976237,	12.76536709, 0, 0,	0.564378541,
                                  29.96126229,	46.58436719,	0.035655788,	0.573891802,	1.469009704	,1.333827418,	16.83035645,	3.092760582, 0,	0.115021011,	0.003847771,
                                  83.1994811,	4.151582786,	0.040617078,	1.306729583	,1.763404149,	2.430330145,	1.107091157,	0.765943688, 5.230352522,	0.000785647,	0.003682144
                                )) 

level_order3 <- c('Preflexion', 'Flexion', 'Postflexion')
IRIabundanceplot <- ggplot(percentIRIbiomass, aes(fill=Prey, y=IRI, x=factor(GrowthStage3, levels = level_order3))) + geom_bar(stat="identity") 
IRIbiomass <- IRIabundanceplot + xlab("Growth Stage") + ylab("Index of Relative Importance (%) Biomass") + theme_classic() + 
  scale_fill_manual(values = c("lightblue", "cornflowerblue", "dodgerblue4", "lightgreen", "springgreen3", "lightgoldenrod2", "tomato1", "plum3", "mediumpurple3", "indianred3", "sienna4")) +
  theme(title =element_text(size=14), axis.title=element_text(size=8), axis.text.y = element_text(size = 8), axis.text.x = element_text(size = 8)) 
#IRIbiomass
IRIbiomassplotnolegend <- IRIbiomass + theme(legend.position = "none")

dietplots <- ggarrange(abundanceplotnolegend, IRIabundanceplotnolegend, biomassplotnolegend, IRIbiomassplotnolegend,
                       labels = c("A", "B", "C", "D"),
                       vjust = 1.2, 
                       hjust = -0.2,
                       common.legend = TRUE, legend = "right")
#ggsave("dietplots.jpg", width = 7, height = 6, dpi = 300)

spatiallevins <- data.frame(Station=rep(c('85-42.9S', '90-30S&W', '90-35S', '90-37S', '93.3-28S&W', '93.3-35S'), each=3),
                            GrowthStage=rep(c('Preflexion', 'Flexion', 'Postflexion'), times=6),
                            LevinsNicheBreadth=c(0.48421468, 0.24593589, 0,
                                                 0.424242424, 0.352759406, 0.235612744,
                                                 0.253205452, 0.349900019, 0.214992806,
                                                 0, 0.393055991, 0.517458204,
                                                 0, 0.381095101, 0.149872216,
                                                 0.856830601, 0.79342158, 0
                                                 
                            ))
level_order2 <- c('Preflexion', 'Flexion', 'Postflexion')
levinsplot2 <- ggplot(spatiallevins, aes(fill=factor(GrowthStage, levels = level_order2), y=LevinsNicheBreadth, x=Station)) + geom_bar(position='dodge', stat='identity') + scale_y_continuous(expand = c(0,0)) + ylab("Levins' Niche Breadth Index (Ba)") + xlab("Station") + theme_classic() + theme(title =element_text(size=12), axis.title=element_text(size=14), axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust=1, margin = margin(t = 0, r = 10, b = -10, l = 0)))
levinsplotlegend2 <- levinsplot2 + scale_fill_manual(name = "Growth Stage", values = c("lightblue", "cornflowerblue", "dodgerblue4"))
levinsplotlegend2
#ggsave("levinsplotlegend2.jpg", width = 7, height = 6, dpi = 300)

# selectivity 

Selectionsheet <- read.csv("~/Downloads/SelectivitySheet.csv")
#View(Selectionsheet)
GrowthStage1 <- Selectionsheet$Growth.Stage
preytaxa1 <- Selectionsheet$Prey.Taxa
Selectivity1 <- Selectionsheet$Percent

dt <- data.frame(GrowthStage1, preytaxa1, Selectivity1)


dt1 <- dt
dt1$preytaxa1
dt1$GrowthStage1
dt1$GrowthStage1 <- factor(dt1$GrowthStage1, levels = c('Preflexion', 'Flexion', 'Postflexion'), ordered = TRUE)
dt1$GrowthStage1l
dt1$preytaxa1 <- factor(dt1$preytaxa1, levels = c('Calanoid Copepodite', 'Calanoid Nauplii', 'Cyclopoid Copepodite', 'Cyclopoid Nauplii', 'Eggs', 'Euphausiids', 'Other Copepodites'), ordered = TRUE)
dt1$preytaxa1
my_y_title <- expression(paste("Prey Taxonomic Preferences of Larval ",  italic("Sebastes"),  " spp."))
labs(main=my_y_title)
selectivity <- ggplot(dt1, aes(fill=GrowthStage1, y=Selectivity1, x=preytaxa1)) + geom_bar(width=0.7, position='dodge', stat='identity') + theme_classic() + scale_fill_manual(name = "Growth Stage", values = c("lightblue", "cornflowerblue", "dodgerblue4")) + geom_hline(yintercept = 14.3, linetype=2) + xlab("Selection Taxa") + ylab("Prey Preference %") + 
  scale_y_continuous(expand = c(0,0)) + geom_text(x=6.05, y=15.1, label="Neutral Preference", size=4) + theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 9, angle = 45, hjust = 1)) + scale_x_discrete(labels=c("Calanoida Copepodites", "Calanoida Nauplii", "Cyclopoida Copepodites", "Cyclopoida Nauplii", "Egg", "Euphausiids", "Other Copepodites")) 
selectivity
#selectivity
#ggsave("selectivity.jpg", width = 7.5, height = 6, dpi = 300)