####Plot relative feature importance#####

graphics.off()
rm(list = ls()) # clear working directory
library(tidyverse)
library("dplyr")       

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
sex_2017 <- sex_2017[order(sex_2017$Importance, decreasing = TRUE), ]
sex_2017 <- sex_2017[c(1:10), ]
sex_2017$newfeature <- c("Distance to breeding pool", 
                         "Distance to other wetland",
                         "Distance to meadow",
                         "Soil: middle clay",
                         "Soil: middle sandy clay loam",
                         "Habitat: successional tuliptree",
                         "Soil: top fine sandy loam",
                         "Soil: alluvium parent material",
                         "Soil: middle clay loam",
                         "Habitat: mesic mixed hardwood")

svl_2017 <- read.csv("importance_matrix_2017_SVL.csv")
svl_2017 <- svl_2017[order(svl_2017$Importance, decreasing = TRUE), ]
svl_2017 <- svl_2017[c(1:10), ]
svl_2017$newfeature <-
  c("Distance to breeding pool", 
  "Distance to other wetland",
  "Distance to meadow",
  "Soil: middle clay loam",
  "Soil: alluvium parent material",
  "Soil: middle clay",
  "Soil: middle clay loam",
  "Habitat: successional tuliptree",
  "Soil: top fine sandy loam",
  "Habitat: mesic mixed hardwood")

sex_2016 <- read.csv("importance_matrix_2016_SEX.csv")
sex_2016 <- sex_2016[order(sex_2016$Importance, decreasing = TRUE), ]
sex_2016 <- sex_2016[c(1:10), ]
sex_2016$newfeature <- c("Distance to meadow",
                         "Distance to breeding pool", 
                         "Distance to other wetland",
                         "Habitat: successional tuliptree",
                         "Soil: middle clay loam",
                         "Soil: middle sandy clay loam",
                         "Soil: top fine sandy loam",
                         "Habitat: mesic mixed hardwood", 
                         "Soil: middle silty clay", 
                         "Soil: somewhat poorly drained")
  

svl_2016 <- read.csv("importance_matrix_2016_SVL.csv")
svl_2016 <- svl_2016[order(svl_2016$Importance, decreasing = TRUE), ]
svl_2016 <- svl_2016[c(1:10), ]
svl_2016$newfeature <- c("Distance to breeding pool", 
                         "Distance to other wetland",
                         "Distance to meadow",
                         "Soil: middle clay loam",
                         "Soil: alluvium parent material",
                         "Soil: middle clay",
                         "Soil: middle sandy clay loam",
                         "Habitat: successional tuliptree",
                         "Soil: top fine sandy loam",
                         "Habitat: mesic mixed hardwood")
                      

  
p1 <- ggplot(sex_2017, aes(reorder(newfeature, Importance), Importance)) +
  geom_col(fill="#58508d") +
  coord_flip()+
  labs(x = "", y =paste0("Relative", "\n", "Importance")) +
  ylim(c(0, 0.4)) +
  ggtitle("Stage (2017)")+
  theme_classic()

p1

p2 <- ggplot(svl_2017, aes(reorder(newfeature, Importance), Importance)) + 
  geom_col(fill="#58508d") +
  coord_flip() + 
  labs(x = "", y =paste0("Relative", "\n", "Importance")) +
  ylim(c(0, 0.4)) +
  ggtitle("SVL (2017)")+
  theme_classic()

multiplot(p1, p2, cols=2)




p3 <- ggplot(sex_2016, aes(reorder(newfeature, Importance), Importance)) +
  geom_col(fill="#58508d") +
  coord_flip()+
  labs(x = "", y =paste0("Relative", "\n", "Importance")) +
  ylim(c(0, 0.4)) +
  ggtitle("Stage (2016)")+
  theme_classic()


p4 <- ggplot(svl_2016, aes(reorder(newfeature, Importance), Importance)) + 
  geom_col(fill="#58508d") +
  coord_flip() + 
  labs(x = "", y =paste0("Relative", "\n", "Importance")) +
  ylim(c(0, 0.4)) +
  ggtitle("SVL (2016)")+
  theme_classic()+
  annotate("text", x = 0, y = 0, label = "D", col="blue")

p4

multiplot(p3, p4, cols=2)

multiplot(p1, p2, p3, p4, cols=2)
