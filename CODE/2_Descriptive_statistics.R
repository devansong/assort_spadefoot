##################################################
##############A. Devan-Song#######################
#######Code for S. holbrookii assortativity MS####
#######################Corvallis, Oregon, USA#####
##################March 2019###################### 
##################Exploraty plots#################
#######New edits December 2020, Bend, OR, USA#####
#######New edits Jan 2022, Bend, OR, USA##########
##################################################

##########SETUP#########
rm(list = ls()) # clear working directory
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

library(raster)
library(rgdal)
library(sf)
library(ggplot2)
library(dplyr)
library(spatstat)
library(raster)
library(abind)
library(reshape)
library(cowplot) 

#Function to turn data into degree coordinates 
reproject <- function(data){
  data.df <- as.data.frame(data)
  data.df.UTM <- SpatialPoints(data.df, proj4string=CRS("+proj=utm +zone=18 +datum=NAD83"))
  data.df.deg <- spTransform(data.df.UTM, CRS("+proj=longlat +datum=WGS84"))
  data.coords <- coordinates(data.df.deg)
  return(data.coords)
}

#function to calculate Euclidean distance of each frog to nearest pond 
calc_dist <- function(data, pond.coords){
  dst<- pointDistance(data, pond.coords, lonlat=TRUE)
  nnd <- apply(dst, 1, FUN=min) #pull out distance to nearest pond for each random point 
  return(nnd)
}


#Import spadefoot data points 
spade <- read.csv("2017_data_cleaned.csv") #load data with all the 2017 survey data 
spade$Y <- as.numeric(spade$Y) #make Y coordinate a number
spade$X <- as.numeric(spade$X) # make X coordinate a number 
spade_points <- spade[c("X", "Y")] # make a dataframe with all the xy coordinates  
spade_coords <- reproject(spade_points)#reproject all spadefoot UTMs to latlong 

#Import wetland locations, extract coordinates and convert to latlong 
ponds <- st_read("2017_PONDS.shp") #read shapefile with pond points
pond_points <- st_coordinates(ponds) #Extract pond coordinates from Shpfiles 
pond_coords <- reproject(pond_points) #reproject pond UTMs into WGS84 Lat-long 

spade_nnd<- calc_dist(spade_coords, pond_coords) #calc distance of each frog point to nearest pond in meters 
spade_nnd_df <- as.data.frame(spade_nnd)
spade$Distance <- spade_nnd_df$spade_nnd #add this to the data frame of survey data


##Plot Snout-vent length vs. distance to nearest wetland with marginal densities

spade$Category <- spade$Adjusted_sex

png("Figure_Exploratory.png", units="in", width=7.5, height=5.5, res=300)
pmain <- ggplot(spade, aes(x=Distance, y=SVL_mm, color=Category))+
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("Red", "Blue", "Grey", "Gold"))+ 
  ylab("SVL (mm)")+ 
  xlab("Distance to Nearest Breeding Pool (m)")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# Marginal densities along x axis
xdens <- axis_canvas(pmain, axis = "x")+
  geom_density(data = spade, aes(x =Distance, fill = Category),
               alpha = 0.7, size = 0.2)+
  ggpubr::fill_palette(palette = c("Red", "Blue", "Grey", "Gold"))
# Marginal densities along y axis
# Need to set coord_flip = TRUE, if you plan to use coord_flip()
ydens <- axis_canvas(pmain, axis = "y", coord_flip = TRUE)+
  geom_density(data = spade, aes(x = SVL_mm, fill = Category),
               alpha = 0.7, size = 0.2)+
  coord_flip() +
  ggpubr::fill_palette(palette = c("Red", "Blue", "Grey", "Gold"))
p1 <- insert_xaxis_grob(pmain, xdens, grid::unit(.2, "null"), position = "top")
p2<- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(p2)
graphics.off()

nrow(subset(spade, Category == "S"))
nrow(subset(spade, Category == "NBA"))

