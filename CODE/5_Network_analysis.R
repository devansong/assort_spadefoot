##################################################
##############A. Devan-Song#######################
#######Code for S. holbrookii assortativvity MS###
#######################Corvallis, Oregon, USA#####
##################March 2019###################### 
############Network Analysis for 2017 data########
#######New edits December 2020, Bend, OR, USA#####
#######New edits Jan 2022, Bend, OR, USA##########
##################################################

graphics.off()
rm(list = ls())
setwd("~/Dropbox/Publications_Work/Ecosphere_REV/DATA") #set working directory with csv files 

library(igraph)
library(ggplot2)
library(kSamples)
library(tidyr)
library(qgraph)
library(DataCombine)
library(dplyr)
library(reshape2)


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

##-- read in and format data
spade <- read.csv("2016_data_cleaned.csv") #load data with all the 2017 survey data 

#reformatting data ############################################ 
summary(spade)
spade$SVL_mm <- as.numeric(spade$SVL_mm)
spade$ident <- as.factor(spade$lineOrig)
spade <- spade %>% drop_na(SVL_mm) #drop any indiv with no SVL 
spade %>% drop_na(SVL_mm)
spade <- subset(spade, Adjusted_sex != "NA")
##############################################################

#read in matrix data created in Python 
e1 <- read.csv("~/Dropbox/Publications_Work/Ecosphere_REV/DATA/20220117_2016_matrix.csv",header=TRUE)
e1 <- na.omit(e1) #omit NAs 

#Here is where we decide what distance to define edges. 
e1$circ <- ifelse(e1$dist > 25,0,1) #column if distance is within 25 m of another toad 
e2 <- subset(e1,e1$circ==1)

#Create the network (igraph object)
links <- e2[,c("id1","id2")] 
nodes <- frogs <- spade[,c("ident","SVL_mm", "Adjusted_sex")] #pull out lineOrig, SVL_mm, Demography columns
net <- graph_from_data_frame(d=links, vertices=nodes, directed=F)
net <- simplify(net, remove.multiple=T,remove.loops=T,edge.attr.comb="mean")
net

##Plot network object in igraph, layout with kk 

clrs <- c("red", "blue", "grey", "yellow")
V(net)$color[which(V(net)$Adjusted_sex=="F")] <- clrs[[1]]
V(net)$color[which(V(net)$Adjusted_sex=="M")] <- clrs[[2]]
V(net)$color[which(V(net)$Adjusted_sex=="NBA")] <- clrs[[3]]
V(net)$color[which(V(net)$Adjusted_sex=="S")] <- clrs[[4]]
V(net)$square <- V(net)$SVL_mm

nodesize <- V(net)$square/10
setwd("~/Dropbox/Publications_Work/Ecosphere_REV/Figures") #set working directory to figures folder 

#graphics.off()
#plot.new()
#png("Network2017.png", units="in", width=10, height=9, res=300)
plot(net, layout = layout_with_kk, vertex.label=NA, vertex.size=nodesize, edge.color = rgb(70/255, 130/255, 180/255, 0.95))
legend(x=1, y = 1, legend=levels(as.factor(V(net)$Adjusted_sex))  , col = "black", bty = "n", pch=21, pt.bg = clrs, pt.cex = 2, cex = 1, text.col="black" , horiz = FALSE, inset = c(0.1, 0.1))
#dev.off()


######NETWORK METRICS##################

#calculate assortativity and error bar 
#BY SIZE## 
rsize <- assortativity(net, V(net)$SVL_mm, types2 = NULL, directed = FALSE)
rsize #0.2965877
vals <- c()
ris <- c()
for (i in E(net)) {
  n2 <- delete_edges(net, i)
  ri <- assortativity(n2, V(n2)$SVL_mm, types2 = NULL, directed = FALSE)
  ris <- append(ris, ri)
  val <- (ri - rsize)^2
  vals <- append(vals, val)
}
sigsize <- sqrt(sum(vals)) 
sigsize #0.01530423
errorsize <- (1.960*sigsize) /sqrt(1151)
errorsize # 0.0008837742

bysize <- cbind("By_size", rsize, sigsize, errorsize, (rsize-errorsize), (rsize+errorsize))
bysize <- as.data.frame(bysize)
colnames(bysize) <- c("i", "assortativity", "sigmasize", "error", "lowerCI", "upperCI")


#BY DEMOGRAPHY## 
newnet <- net
V(newnet)$Adjusted_sex <- as.factor(V(newnet)$Adjusted_sex)
rdemog <- assortativity_nominal(newnet, V(newnet)$Adjusted_sex, directed = FALSE)
rdemog #0.1504934
vals <- c()
ris <- c()
for (i in E(newnet)) {
  n2 <- delete_edges(newnet, i)
  ri <- assortativity_nominal(n2, V(n2)$Adjusted_sex, directed = FALSE)
  ris <- append(ris, ri)
  val <- (ri - rdemog)^2
  vals <- append(vals, val)
}
sigdemog <- sqrt(sum(vals))
sigdemog #0.01109274
errordemog <- (1.960*sigdemog) /sqrt(1151)
errordemog #0.0006777316

bysex <- cbind("By_sex", rdemog, sigdemog, errordemog, (rdemog-errordemog), (rdemog+errordemog))
bysex <- as.data.frame(bysex)
colnames(bysex) <- c("i", "assortativity", "sigmasize", "error", "lowerCI", "upperCI")


#Size assortativity for Males

#SORT BY DEMOG, THEN LOOK AT ASSORTATIVITY BY SIZE#######  

df <- data.frame("Adjusted_sex" =character(0),
                 "assortativity" = numeric(0), 
                 "sigma"= numeric(0), 
                 "error"= numeric(0))
#Size assortativity for Males

categories <- c("F", "M", "NBA", "S")
#categories <- as.data.frame(categories)
 

for (i in categories) {
  sub <- induced_subgraph(net, V(net)$Adjusted_sex == i)
  assortativity <- assortativity(sub, V(sub)$SVL_mm, types2 = NULL, directed = FALSE)
  vals <- c()
  ris <- c()
  for(j in E(sub)) {
    n2 <- delete_edges(sub, j)
    ri <- assortativity(n2, V(n2)$SVL_mm, types2 = NULL, directed = FALSE)
    ris <- append(ris, ri)
    val <- (ri - assortativity)^2
    vals <- append(vals, val)
  }
  sigmasize <- sqrt(sum(vals))
  error <- (1.96*sigmasize)/sqrt(gorder(sub))
  lowerCI <- assortativity-error
  upperCI <- assortativity+error
  row <- cbind(i, assortativity, sigmasize, error, lowerCI, upperCI)
  row <- as.data.frame(row)
  df<- rbind(df, row)
}

homophily_df <- rbind(df, bysize, bysex)

homophily_df


setwd("~/Dropbox/Publications_Work/Ecosphere_REV/DATA")

write.csv(homophily_df, file="homophily_2016_10m.csv", row.names=FALSE)


#import csv of homophily, created from jacknifing and assortativity above 

homophily<- read.csv("~/Dropbox/Publications_Work/Ecosphere_REV/DATA/homophily_2016_10m.csv",header=TRUE)
homophily <- as.data.frame(homophily)
homophily$i <- as.character(homophily$i)
homophily$i <- as.factor(homophily$i)


homophily$Category <- factor(homophily$i,levels = c("By_size", "By_sex", "F", "M", "NBA", "S"))

homophily$rename <- c("F*size", 
                      "M*size",
                      "NBA*size",
                      "S*size",
                      "By size",
                      "By stage")

homophily

setwd("~/Dropbox/Publications_Work/Ecosphere_REV/FIGURES")
png("Homophily_2016_10m.png", units="in", width=2.4, height=3, res=300)
ggplot(homophily, aes(x=rename, y=assortativity)) + 
  geom_errorbar(width=.1, aes(ymin=lowerCI, ymax=upperCI)) +
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  geom_point(shape=21, size=1) +
  ylim(-0.2, 0.35) + 
  xlab("")+
  ylab("Assortativity") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ 
  theme(axis.text.x = element_text(angle = 45, hjust =1))
dev.off()



##GETTING DEGREE DISTRIBUTPNS OF M F JUV

sF <- induced_subgraph(net, V(net)$Adjusted_sex == "F")
sM <- induced_subgraph(net, V(net)$Adjusted_sex == "M")
sNBA <- induced_subgraph(net, V(net)$Adjusted_sex == "NBA")
sS <- induced_subgraph(net, V(net)$Adjusted_sex == "S")




deg <- data.frame(degree=degree(net)) #all degrees 
deg$demog <- V(net)$Adjusted_sex
deg$lineOrig <- V(net)$name
setwd("~/Dropbox/Publications_Work/Ecosphere_REV/DATA")
write.csv(deg, file="Degree_10m_2016.csv", row.names = FALSE)

as.data.frame(V(sM)$name) # ids of males 
as.data.frame(V(sF)$name)# ids of females 
as.data.frame(V(sNBA)$name)# ids of juvie
as.data.frame(V(sS)$name)# ids of juvie

deg$demog <- as.character(deg$demog)
deg$Adjusted_sex <- as.character(deg$demog)
deg$SVL_mm <- V(net)$SVL_mm

ggplot(deg, aes(x=SVL_mm, y=degree, color=Adjusted_sex))+
  geom_point(alpha=0.7)

setwd("~/Dropbox/Publications_Work/Ecosphere_REV/FIGURES")

#PLOT SVL AGAINST DEGREE!!!! 
library(cowplot) 
# Main plot

plot.new()
#png("Figure_Exploratory_Degree_2016.png", units="in", width=7.5, height=5.5, res=300)
pmain <- ggplot(deg, aes(x=SVL_mm, y=degree, color=Adjusted_sex))+
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("Red", "Blue", "Grey", "Gold"))+ 
  ylab("Degree")+ 
  xlab("SVL (mm)")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
 #Marginal densities along x axis
xdens <- axis_canvas(pmain, axis = "x")+
  geom_density(data = deg, aes(x=SVL_mm, fill = Adjusted_sex),
               alpha = 0.7, size = 0.2)+
  ggpubr::fill_palette(palette = c("Red", "Blue", "Grey", "Gold"))
# Marginal densities along y axis
## Need to set coord_flip = TRUE, if you plan to use coord_flip()
ydens <- axis_canvas(pmain, axis = "y", coord_flip = TRUE)+
  geom_density(data = deg, aes(x=degree, fill = Adjusted_sex),
               alpha = 0.7, size = 0.2)+
  coord_flip() +
  ggpubr::fill_palette(palette = c("Red", "Blue","Grey",  "Gold"))
p1 <- insert_xaxis_grob(pmain, xdens, grid::unit(.2, "null"), position = "top")
p2<- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(p2)
dev.off()

#graphics.off()


# Density plots

library(plyr)
mu <- ddply(deg, "Adjusted_sex", summarise, grp.mean=mean(degree))
mu$Stage <- mu$Adjusted_sex
deg$Stage<- deg$Adjusted_sex
graphics.off()
plot.new()
setwd("~/Dropbox/Publications_Work/Ecosphere_REV/FIGURES")
png("degree_dist_2016_10m.png", units="in", width=3, height=2.5, res=300)
ggplot(deg, aes(x=degree, fill=Stage)) + 
  geom_density() + 
  ylab("Density") + 
  xlab("Degree") + 
  #scale_y_continuous(breaks = seq(min(0), max(0.12), by = 0.02), limits=c(0,0.12))+
  #scale_x_continuous(breaks = round(seq(min(0), max(25), by = 5),0), limits=c(0,25))+
  scale_fill_manual(values=c(rgb(255/255, 0/255, 0/255, 0.8),rgb(0/255, 0/255, 255/255, 0.5), rgb(211/255, 211/255, 211/255, 0.4), rgb(255/255, 255/255, 0/255, 0.4))) + 
  scale_color_manual(values=c("red", "blue", "grey", "gold"))+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Stage),
             linetype="dashed")+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position=c(0.8,0.7))
dev.off()


#extract degree of M 
fdeg <- subset(deg, demog=="F")
mdeg <- subset(deg, demog=="M")
nbadeg <- subset(deg, demog=="NBA")
sdeg <- subset(deg, demog=="S")

library("kSamples")
eeks <- ad.test(fdeg$degree,mdeg$degree,nbadeg$degree, sdeg$degree) 
summary(eeks)

eeks$ad

write.csv(eeks$ad, file="ADtest_2016_10m.csv")


#kruskal.test(degree(sM), degree(sJ))
#ad.test(degree(sM), degree(sJ))
#ad.test(degree(sM), degree(sF))



#CHISQARE

e3 <- subset(e2,e2$id1!=e2$id2)

head(e3)
library(ggpubr)
library(plyr)
emdiff <- ddply(e3, ~id1, summarize, meandiff = mean(diff), Adjusted_sex = head(sex1,1), svl = mean(svl1),
                nvert = length(id2), nsame = sum(sex2==sex1), ndiff = sum(sex2!=sex1))

net_dir <- graph_from_data_frame(d=links, vertices=nodes, directed=T)
net_dir <- simplify(net_dir, remove.multiple = F,remove.loops=T)
testdir <- igraph::as_data_frame(net_dir, what=c("edges"))
str(testdir)
testdir$fromSex <- NA
testdir$toSex <- NA
m <- 0
for (m in 1:nrow(testdir)) {
  testdir$fromSex[m] <- emdiff$Adjusted_sex[emdiff$id1==testdir$from[m]]
  testdir$toSex[m] <- emdiff$Adjusted_sex[emdiff$id1==testdir$to[m]]
}

contab <- table(testdir$fromSex, testdir$toSex)
contab
contabtest <- chisq.test(contab)
contabtest
contabtest$residuals
library(corrplot)

colnames(contabtest$residuals) <- c("F", "M", "NBA", "S")
rownames(contabtest$residuals) <- c("F", "M", "NBA", "S")

graphics.off()
par(mfrow=c(1,1), mar = c(0.5,0.5,0.5,0.5)) 
corrplot(contabtest$residuals,is.cor=FALSE, tl.col="black", cl.align = "l", mar = c(3.2, 0, 0, 3), cl.pos='b', type="lower", cl.length=5, cl.lim = c(-10,11)) ## set options here to do only color

setwd("~/Dropbox/Publications_Work/Ecosphere_REV/FIGURES")

png("Correlation_plot_2016_10m.png", units="in", width=6, height=6, res=300)
corrplot(contabtest$residuals,is.cor=FALSE, tl.col="black", cl.align = "l", mar = c(3.2, 0, 0, 3), cl.pos='b', type="lower", cl.length=5, cl.lim = c(-4,4)) 
dev.off()





