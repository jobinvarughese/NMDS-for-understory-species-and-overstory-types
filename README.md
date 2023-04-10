# NMDS-for-understory-species-and-overstory-types
NMDS of undrestory species matrix and grouped based on overstory types


###NMDS for Shola####
data<-read.csv(file ="~/Desktop/iiser/8 sem/Vegetation paper/2017_only_nilgiri_anamalai_palani/shola regeneration munnar-valparai-berijam3 for NMDS.csv", head = T, row.names = 1)
library(ggplot2)
library(tidyverse)
library (vegan)

mat1<-data[rowSums(data[,c(2,3,5,  12, 13, 15:21, 24:34,36, 37:43,47)])>0,] #Removing somes spp in only 1 or 2 plots
#To also amke the NMDS plot ore manageable

NMDS_dat<-mat1[,c(2,3,5,  12, 13, 15:21, 24:34,36, 37:43,47)]
str(NMDS_dat)

#Running NMDS
NMDS=metaMDS(NMDS_dat,
             distance = "bray",
             k = 4,
             maxit = 999, 
             trymax = 500,
             wascores = TRUE)
#distance matrix (here we use "bray"),
#your selected number of dimensions ("k"), 
#your max number of iterations (usually **"maxit = 999"**), and 
#the maximum number of random starts (usually "trymax = 250")

stressplot(NMDS) 
# Produces a Shepards diagram


#Simple plotting of NMDS 
plot(NMDS)


#Plotting NMDS on ggplot
data.scores <- as.data.frame(scores(NMDS$points)) 
data.scores$site <- rownames(data.scores) 
# create a column of site names, from the rownames of data.scores
data.scores$site
data.scores$grp <- factor(mat1$Type1)


# data.extented$tot_basal_ar_category, data.extented$VP0_2_category, data.extented$Daphniphyllum_category) 
#  add the grp variable created earlier

head(data.scores)  
#look at the data


species.scores <- as.data.frame(scores(NMDS, "species"))  
#Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  
# create a column of species, from the rownames of species.scores
head(species.scores)  
#look at the data
species.scores$NMDS2

grp.a <- data.scores[data.scores$grp == "Eucalyptus", ][chull(data.scores[data.scores$grp == 
                                                                            "Eucalyptus", c("MDS1", "MDS2")]), ]  # hull values for grp A
grp.b <- data.scores[data.scores$grp == "Pine", ][chull(data.scores[data.scores$grp == 
                                                                      "Pine", c("MDS1", "MDS2")]), ]  # hull values for grp B
grp.c <- data.scores[data.scores$grp == "Acacia", ][chull(data.scores[data.scores$grp == 
                                                                        "Acacia", c("MDS1", "MDS2")]), ]  # hull values for grp B
grp.d <- data.scores[data.scores$grp == "Mixed", ][chull(data.scores[data.scores$grp == 
                                                                       "Mixed", c("MDS1", "MDS2")]), ]  # hull values for grp B

hull.data <- rbind(grp.a, grp.b,grp.c,grp.d)  #combine grp.a and grp.b
hull.data

ggplot() + 
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.6, size=4) +  # add the species labels
  geom_point(data=data.scores,aes(x=MDS1,y=MDS2, colour=grp),size=3, alpha=0.6)+
  geom_polygon(data=hull.data,aes(x=MDS1,y=MDS2,fill=grp,group=grp),alpha=0.10) + # add the convex hulls
  scale_shape_identity()+
  #geom_text(data=data.scores,aes(x=MDS1,y=MDS2,label=site),size=1,vjust=0) +  # add the site labels
  coord_equal() +
  theme_bw()


###NMDS for invasives####
  
  library(ggrepel)
  data<-read.csv(file ="~/Desktop/iiser/8 sem/Vegetation paper/invasive_noshola_noothers_grtr_thnzerobasalar.csv", head = T, row.names = 1, check.names = FALSE)
  
  
  mat1<-data[rowSums(data[,c(4:16,18:19, 21:28, 40)])>0,] #30 = vince toxicum
  mat1$Type2[mat1$Type2=="Acacia-Eucalyptus"] <- "Mixed"
  
  
  NMDS_dat<-mat1[,c(4:16,18:19, 21:28,  40)]
  str(NMDS_dat)
  
  NMDS=metaMDS(NMDS_dat,
               distance = "bray",
               k = 4,
               maxit = 999, 
               trymax = 500,
               wascores = TRUE)
  #distance matrix (here we use "bray"),
  #your selected number of dimensions ("k"), 
  #your max number of iterations (usually **"maxit = 999"**), and 
  #the maximum number of random starts (usually "trymax = 250")
  
  stressplot(NMDS) 
  # Produces a Shepards diagram
  
  
  plot(NMDS)
  
  #plot(NMDS, type="none")#xlim=c(-10,10), ylim=c(-10,10))
  #for blank plot
  #my_colors<-c( "lightblue", "purple", "black",  "red")
  #my_pch <- c(15,16,17,18)
   #points(NMDS, display = 'sites',pch = my_pch[factor(mat1$Type2)],cex = 1, col=c( "lightblue", "purple", "black","red")[as.numeric(as.factor(mat1$Type2))])
  #orditorp(NMDS,display="species",col="red",air=0.01, cex = .4)
  #ordihull(NMDS, groups = mat1$Type2, draw = "polygon", lty = 1, col = c("#0073C2FF", "#EFC000FF", "#868686FF", "#FB8072"))
  #fit <- envfit(NMDS, mat1[,c(43,44,65)], perm = 999)
  #plot(fit, cex = 1)
  
  #points(NMDS, display = c("sites"), choices=1:2,cex = 1, pch=my_pch[factor(mat1$Type2)], col = my_colors[factor(mat1$Type2)], xlim=c(-2,4),  origin = TRUE) 
  #legend(2,-1.5,unique(mat1$Type2), cex = 1, pch= my_pch[unique(factor(mat1$Type2))],col = my_colors[unique(factor(mat1$Type2))])
  
  fort<-fortify(NMDS)
  
  data.scores <- as.data.frame(scores(NMDS$points)) 
  data.scores$site <- rownames(data.scores) 
  # create a column of site names, from the rownames of data.scores
  data.scores$site
  data.scores$grp <- factor(mat1$Type2)
  
  
  # data.extented$tot_basal_ar_category, data.extented$VP0_2_category, data.extented$Daphniphyllum_category) 
  #  add the grp variable created earlier
  
  head(data.scores)  
  #look at the data
  
  
  species.scores <- as.data.frame(scores(NMDS, "species"))  
  #Using the scores function from vegan to extract the species scores and convert to a data.frame
  species.scores$species <- rownames(species.scores)  
  # create a column of species, from the rownames of species.scores
  head(species.scores)  
  #look at the data
  species.scores$NMDS2
  
  grp.a <- data.scores[data.scores$grp == "Eucalyptus", ][chull(data.scores[data.scores$grp == 
                                                                     "Eucalyptus", c("MDS1", "MDS2")]), ]  # hull values for grp A
  grp.b <- data.scores[data.scores$grp == "Pinus", ][chull(data.scores[data.scores$grp == 
                                                                     "Pinus", c("MDS1", "MDS2")]), ]  # hull values for grp B
  grp.c <- data.scores[data.scores$grp == "Acacia", ][chull(data.scores[data.scores$grp == 
                                                                        "Acacia", c("MDS1", "MDS2")]), ]  # hull values for grp B
  grp.d <- data.scores[data.scores$grp == "Mixed", ][chull(data.scores[data.scores$grp == 
                                                                          "Mixed", c("MDS1", "MDS2")]), ]  # hull values for grp B
  
  hull.data <- rbind(grp.a, grp.b,grp.c,grp.d)  #combine grp.a and grp.b
  hull.data
  
 # {ggplot() + 
    geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species,fontface = "italic"),alpha=0.8, size=15) +  # add the species labels
    geom_point(data=data.scores,aes(x=MDS1,y=MDS2, colour=grp),size=10, alpha=1.0)+
    geom_polygon(data=hull.data,aes(x=MDS1,y=MDS2,fill=grp,group=grp),alpha=0.25) + # add the convex hulls
  scale_shape_identity()+ scale_color_manual(values=c("#FF5722", "#C62828", "#9E9E9E", "#FFCDD2"))+
  scale_fill_manual(values=c("#FF5722", "#C62828", "#9E9E9E", "#FFCDD2"))+
    #geom_text(data=data.scores,aes(x=MDS1,y=MDS2,label=site),size=1,vjust=0) +  # add the site labels
    coord_fixed(ratio = 2.0) +
    theme_bw()}


  
  ggplot() + 
    geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species, fontface = "italic"),alpha=0.6, size=4) +  # add the species labels
    geom_point(data=data.scores,aes(x=MDS1,y=MDS2, colour=grp),size=3, alpha=0.6)+
    geom_polygon(data=hull.data,aes(x=MDS1,y=MDS2,fill=grp,group=grp),alpha=0.10) + # add the convex hulls
    scale_shape_identity()+
    #geom_text(data=data.scores,aes(x=MDS1,y=MDS2,label=site),size=1,vjust=0) +  # add the site labels
    coord_equal() +
    theme(panel.background = element_rect(fill = "white", colour = "grey50",
                                          linewidth = 1),panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_line(colour = "grey90"),  legend.text = element_text(face = "italic")) 

  #legend.text = element_text(face = "italic")#
  
  hex <- hue_pal()(4)
  for(i in 1:8){
    print(hue_pal()(i))
  }
  
  mat1$Type2[mat1$Type2=="Acacia"] = "Other"
  mat1$Type2[mat1$Type2=="Mixed"] = "Other"
  mat1$Type2[mat1$Type2=="Pine"] = "Other"
  length( mat1$Type2[mat1$Type2=="Other"])
  length( mat1$Type2[mat1$Type2=="Eucalyptus"])
  mod<- adonis2(mat1[,c(4:16,18:19, 21:28,  40)]~ Type2, data=mat1)
  #Error in Ops.data.frame(data, mat1) : 
  #‘-’ only defined for equally-sized data frames
  library(devtools)
  install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
  library(pairwiseAdonis)
  pairwise.adonis(mat1[,c(4:16,18:19, 21:28,  40)] , mat1$Type2, reduce='Eucalyptus|Acacia')
anosim(x = mat1[,c(4:16,18:19, 21:28,  40)] , grouping = mat1$Type2, permutations = 9999, distance = "bray")
  


  
  

