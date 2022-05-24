#Plot degree against features#


graphics.off()
rm(list = ls())

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
spade <- read.csv("fulldata_withdist.csv") #load data with all the survey data
sub <- subset(spade, Year == 2017)
deg <- read.csv("Degree_25m_2017.csv")

merged <- merge(sub, deg, by="lineOrig")


#PLOT SVL AGAINST DEGREE!!!! 
library(cowplot) 
# Main plot

ppool <- ggplot(merged, aes(x=Dist_to_breeding_pool, y=degree, color=Adjusted_sex))+
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("Red", "Blue", "Grey", "Gold"))+ 
  xlim(0, 900)+
  ylab("Degree")+ 
  xlab("Dist. to Nearest Breeding Pool (m)")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        legend.position="")
#Marginal densities along x axis
xdens <- axis_canvas(ppool, axis = "x")+
  geom_density(data = merged, aes(x=Dist_to_breeding_pool, fill = Adjusted_sex),
               alpha = 0.4, size = 0.2)+
  ggpubr::fill_palette(palette = c("Red", "Blue", "Grey", "Gold"))
# Marginal densities along y axis
## Need to set coord_flip = TRUE, if you plan to use coord_flip()
ydens <- axis_canvas(ppool, axis = "y", coord_flip = TRUE)+
  geom_density(data = merged, aes(x=degree, fill = Adjusted_sex),
               alpha = 0.4, size = 0.2)+
  coord_flip() +
  ggpubr::fill_palette(palette = c("Red", "Blue","Grey",  "Gold"))
p1 <- insert_xaxis_grob(ppool, xdens, grid::unit(.2, "null"), position = "top")
p2<- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(p2)
#dev.off()

#graphics.off()


pmeadow<- ggplot(merged, aes(x=Dist_to_meadow, y=degree, color=Adjusted_sex))+
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("Red", "Blue", "Grey", "Gold"))+ 
  xlim(0, 900)+
  ylab("")+ 
  xlab("Dist. to Nearest Meadow (m)")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        legend.position="")
#Marginal densities along x axis
xdens <- axis_canvas(pmeadow, axis = "x")+
  geom_density(data = merged, aes(x=Dist_to_meadow, fill = Adjusted_sex),
               alpha = 0.4, size = 0.2)+
  ggpubr::fill_palette(palette = c("Red", "Blue", "Grey", "Gold"))
# Marginal densities along y axis
## Need to set coord_flip = TRUE, if you plan to use coord_flip()
ydens <- axis_canvas(pmeadow, axis = "y", coord_flip = TRUE)+
  geom_density(data = merged, aes(x=degree, fill = Adjusted_sex),
               alpha = 0.4, size = 0.2)+
  coord_flip() +
  ggpubr::fill_palette(palette = c("Red", "Blue","Grey",  "Gold"))
p3 <- insert_xaxis_grob(pmeadow, xdens, grid::unit(.2, "null"), position = "top")
p4<- insert_yaxis_grob(p3, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(p4)
#dev.off()

pwet <- ggplot(merged, aes(x=Dist_to_wetland, y=degree, color=Adjusted_sex))+
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("Red", "Blue", "Grey", "Gold"))+ 
  xlim(0, 900)+
  ylab("")+ 
  xlab("Dist. to Nearest Wetland (m)")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        legend.position="")
#Marginal densities along x axis
xdens <- axis_canvas(pwet, axis = "x")+
  geom_density(data = merged, aes(x=Dist_to_wetland, fill = Adjusted_sex),
               alpha = 0.4, size = 0.2)+
  ggpubr::fill_palette(palette = c("Red", "Blue", "Grey", "Gold"))
# Marginal densities along y axis
## Need to set coord_flip = TRUE, if you plan to use coord_flip()
ydens <- axis_canvas(pwet, axis = "y", coord_flip = TRUE)+
  geom_density(data = merged, aes(x=degree, fill = Adjusted_sex),
               alpha = 0.4, size = 0.2)+
  coord_flip() +
  ggpubr::fill_palette(palette = c("Red", "Blue","Grey",  "Gold"))
p5 <- insert_xaxis_grob(pwet, xdens, grid::unit(.2, "null"), position = "top")
p6<- insert_yaxis_grob(p5, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(p6)
#dev.off()



legend <- ggplot(merged, aes(x=Dist_to_wetland, y=degree, color=Adjusted_sex))+
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("Red", "Blue", "Grey", "Gold"))+ 
  xlim(0, 900)+
  ylab("Degree")+ 
  xlab("Distance to Nearest Wetland (m)")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
#Marginal densities along x axis
xdens <- axis_canvas(legend, axis = "x")+
  geom_density(data = merged, aes(x=Dist_to_wetland, fill = Adjusted_sex),
               alpha = 0.4, size = 0.2)+
  ggpubr::fill_palette(palette = c("Red", "Blue", "Grey", "Gold"))
# Marginal densities along y axis
## Need to set coord_flip = TRUE, if you plan to use coord_flip()
ydens <- axis_canvas(legend, axis = "y", coord_flip = TRUE)+
  geom_density(data = merged, aes(x=degree, fill = Adjusted_sex),
               alpha = 0.4, size = 0.2)+
  coord_flip() +
  ggpubr::fill_palette(palette = c("Red", "Blue","Grey",  "Gold"))
p7 <- insert_xaxis_grob(legend, xdens, grid::unit(.2, "null"), position = "top")
p8<- insert_yaxis_grob(p7, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(p8)

multiplot(ggdraw(p2), ggdraw(p4), ggdraw(p6), cols=3)





