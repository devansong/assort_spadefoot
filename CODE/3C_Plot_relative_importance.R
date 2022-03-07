####Plot relative feature importance#####

graphics.off()
rm(list = ls()) # clear working directory
library(tidyverse)
library("dplyr")       

setwd("~/Dropbox/Publications_Work/Ecosphere_REV/DATA")
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
} #function for multiplot in ggplot2

sex_2017<- read.csv("importance_matrix_2017_SEX.csv")
sex_2017 <- sex_2017_full[order(sex_2017$Importance, decreasing = TRUE), ]
sex_2017 <- sex_2017[c(1:10), ]

svl_2017 <- read.csv("importance_matrix_2017_SVL.csv")
svl_2017 <- svl_2017[order(svl_2017$Importance, decreasing = TRUE), ]
svl_2017 <- svl_2017[c(1:10), ]

sex_2016 <- read.csv("importance_matrix_2016_SEX.csv")
sex_2016 <- sex_2016[order(sex_2016$Importance, decreasing = TRUE), ]
sex_2016 <- sex_2016[c(1:10), ]

svl_2016 <- read.csv("importance_matrix_2016_SVL.csv")
svl_2016 <- svl_2016[order(svl_2016$Importance, decreasing = TRUE), ]
svl_2016 <- svl_2016[c(1:10), ]




  
p1 <- ggplot(sex_2017, aes(reorder(Feature, Importance), Importance)) +
  geom_col(fill="#58508d") +
  coord_flip()+
  labs(x = "", y ="Relative Importance of Feature") +
  ylim(c(0, 0.4)) +
  ggtitle("Predicting Stage (2017)")+
  theme_classic()


p2 <- ggplot(svl_2017, aes(reorder(Feature, Importance), Importance)) + 
  geom_col(fill="#58508d") +
  coord_flip() + 
  labs(x = "", y ="Relative Importance of Feature") +
  ylim(c(0, 0.4)) +
  ggtitle("Predicting Snout-vent Length (2017)")+
  theme_classic()

multiplot(p1, p2, cols=2)


setwd("~/Dropbox/Publications_Work/Ecosphere_REV/FIGURES")
png("Feature_Importane_2017.png", units="in", width=14, height=4, res=300)
multiplot(p1, p2, cols=2)
dev.off()
       




p3 <- ggplot(sex_2016, aes(reorder(Feature, Importance), Importance)) +
  geom_col(fill="#58508d") +
  coord_flip()+
  labs(x = "", y ="Relative Importance of Feature") +
  ylim(c(0, 0.4)) +
  ggtitle("Predicting Stage (2016)")+
  theme_classic()


p4 <- ggplot(svl_2016, aes(reorder(Feature, Importance), Importance)) + 
  geom_col(fill="#58508d") +
  coord_flip() + 
  labs(x = "", y ="Relative Importance of Feature") +
  ylim(c(0, 0.4)) +
  ggtitle("Predicting Snout-vent Length (2016)")+
  theme_classic()

multiplot(p3, p4, cols=2)


setwd("~/Dropbox/Publications_Work/Ecosphere_REV/FIGURES")
png("Feature_Importane_2016.png", units="in", width=14, height=4, res=300)
multiplot(p3, p4, cols=2)
dev.off()
       

multiplot(p1, p2, p3, p4, cols=2)
