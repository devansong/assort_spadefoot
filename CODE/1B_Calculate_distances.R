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
library(tidyverse)
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
calc_dist_to_pond <- function(data, pond.coords){
  dst<- pointDistance(data, pond.coords, lonlat=TRUE)
  nnd <- apply(dst, 1, FUN=min) #pull out distance to nearest pond for each random point 
  return(nnd)
}

calc_dist_to_wetland <- function(data, wetlands.sp){
  nnd <- apply(gDistance(data, wetlands.sp,byid=TRUE),2,min)
  return(nnd)
}

calc_dist_to_meadow <- function(data, meadows.sp){
  nnd <- apply(gDistance(data, meadows.sp,byid=TRUE),2,min)
  return(nnd)
}


#Import spadefoot data points 
spade <- read.csv("all_data_cleaned.csv") #load data with all the survey data 
spade$Y <- as.numeric(spade$Y) #make Y coordinate a number
spade$X <- as.numeric(spade$X) # make X coordinate a number 
spade_points <- spade[c("X", "Y")] # make a dataframe with all the xy coordinates  
spade_coords <- reproject(spade_points)#reproject all spadefoot UTMs to latlong 


library(rgeos)
#Import wetland locations, extract coordinates and convert to latlong 
ponds <- st_read("2017_PONDS.shp") #read shapefile with pond points
pond_points <- st_coordinates(ponds) #Extract pond coordinates from Shpfiles 
pond_coords <- reproject(pond_points) #reproject pond UTMs into WGS84 Lat-long 

spade_nnd<- calc_dist_to_pond(spade_coords, pond_coords) #calc distance of each frog point to nearest pond in meters 
spade_nnd_df <- as.data.frame(spade_nnd)
spade$Dist_to_breeding_pool <- spade_nnd_df$spade_nnd #add this to the data frame of survey data


#spade_points is not reprojected; use this from her on 
#Dist to nearest wetland
wetlands <- st_read("COLO_Wetlandsonly2022t.shp")
wetlands.sp <- as(wetlands, "Spatial")
spts_wetlands <- SpatialPoints(spade_points)

spade_nnd_wetland <- calc_dist_to_wetland(spts_wetlands, wetlands.sp)
spade_nnd_wetland_df <- as.data.frame(spade_nnd_wetland)
spade$Dist_to_wetland <- spade_nnd_wetland_df$spade_nnd_wetland #add this to the data frame of survey data


meadows <- st_read("Meadows_2022.shp")
meadows.sp <- as(meadows, "Spatial")
spts_meadows <- SpatialPoints(spade_points)

spade_nnd_meadows <- calc_dist_to_meadow(spts_meadows, meadows.sp)
spade_nnd_meadows_df <- as.data.frame(spade_nnd_meadows)
spade$Dist_to_meadow <- spade_nnd_meadows_df$spade_nnd_meadows #add this to the data frame of survey data

write.csv(spade, file="fulldata_withdist.csv", row.names=FALSE)


#fulldata_withdist.csv is the 

plot(spade$Dist_to_meadow, spade$Dist_to_wetland)


##Plot Snout-vent length vs. distance to nearest wetland with marginal densities
#save jpeg image 

spade$Category <- spade$Adjusted_sex

png("Figure_Exploratory.png", units="in", width=7.5, height=5.5, res=300)
pmain <- ggplot(spade, aes(x=Distance, y=SVL_mm, color=Adjusted_sex))+
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("Red", "Blue", "Grey", "Gold"))+ 
  ylab("SVL (mm)")+ 
  xlab("Distance to Nearest Breeding Pool (m)")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# Marginal densities along x axis
xdens <- axis_canvas(pmain, axis = "x")+
  geom_density(data = spade, aes(x =Distance, fill = Adjusted_sex),
               alpha = 0.7, size = 0.2)+
  ggpubr::fill_palette(palette = c("Red", "Blue", "Grey", "Gold"))
# Marginal densities along y axis
# Need to set coord_flip = TRUE, if you plan to use coord_flip()
ydens <- axis_canvas(pmain, axis = "y", coord_flip = TRUE)+
  geom_density(data = spade, aes(x = SVL_mm, fill = Adjusted_sex),
               alpha = 0.7, size = 0.2)+
  coord_flip() +
  ggpubr::fill_palette(palette = c("Red", "Blue", "Grey", "Gold"))
p1 <- insert_xaxis_grob(pmain, xdens, grid::unit(.2, "null"), position = "top")
p2<- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(p2)
graphics.off()

nrow(subset(spade, Category == "S"))
nrow(subset(spade, Category == "NBA"))


pmeadow <- ggplot(spade, aes(x=Distance_to_meadows, y=SVL_mm, color=Adjusted_sex))+
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("Red", "Blue", "Grey", "Gold"))+ 
  ylab("SVL (mm)")+ 
  xlab("Distance to Nearest Meadow (m)")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# Marginal densities along x axis
xdens <- axis_canvas(pmeadow, axis = "x")+
  geom_density(data = spade, aes(x =Distance_to_meadows, fill = Adjusted_sex),
               alpha = 0.7, size = 0.2)+
  ggpubr::fill_palette(palette = c("Red", "Blue", "Grey", "Gold"))
# Marginal densities along y axis
# Need to set coord_flip = TRUE, if you plan to use coord_flip()
ydens <- axis_canvas(pmeadow, axis = "y", coord_flip = TRUE)+
  geom_density(data = spade, aes(x = SVL_mm, fill = Adjusted_sex),
               alpha = 0.7, size = 0.2)+
  coord_flip() +
  ggpubr::fill_palette(palette = c("Red", "Blue", "Grey", "Gold"))
p3 <- insert_xaxis_grob(pmeadow, xdens, grid::unit(.2, "null"), position = "top")
p4<- insert_yaxis_grob(p3, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(p4)


pwetland <- ggplot(spade, aes(x=Distance_to_wetland, y=SVL_mm, color=Adjusted_sex))+
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("Red", "Blue", "Grey", "Gold"))+ 
  ylab("SVL (mm)")+ 
  xlab("Distance to Nearest wetland (m)")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# Marginal densities along x axis
xdens <- axis_canvas(pwetland, axis = "x")+
  geom_density(data = spade, aes(x =Distance_to_wetland, fill = Adjusted_sex),
               alpha = 0.7, size = 0.2)+
  ggpubr::fill_palette(palette = c("Red", "Blue", "Grey", "Gold"))
# Marginal densities along y axis
# Need to set coord_flip = TRUE, if you plan to use coord_flip()
ydens <- axis_canvas(pwetland, axis = "y", coord_flip = TRUE)+
  geom_density(data = spade, aes(x = SVL_mm, fill = Adjusted_sex),
               alpha = 0.7, size = 0.2)+
  coord_flip() +
  ggpubr::fill_palette(palette = c("Red", "Blue", "Grey", "Gold"))
p5 <- insert_xaxis_grob(pwetland, xdens, grid::unit(.2, "null"), position = "top")
p6<- insert_yaxis_grob(p5, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(p4)


multiplot(ggdraw(p2), ggdraw(p4), ggdraw(p6), cols=3)



####### try flipping axes

pmain <- ggplot(spade, aes(x=SVL_mm, y=Distance, color=Adjusted_sex))+
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("Red", "Blue", "Grey", "Gold"))+ 
  xlab("SVL (mm)")+ 
  ylab("Distance to Nearest Breeding Pool (m)")+ 
  ylim(0, 900)+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# Marginal densities along x axis
xdens <- axis_canvas(pmain, axis = "x")+
  geom_density(data = spade, aes(x =SVL_mm, fill = Adjusted_sex),
               alpha = 0.7, size = 0.2)+
  ggpubr::fill_palette(palette = c("Red", "Blue", "Grey", "Gold"))
# Marginal densities along y axis
# Need to set coord_flip = TRUE, if you plan to use coord_flip()
ydens <- axis_canvas(pmain, axis = "y", coord_flip = TRUE)+
  geom_density(data = spade, aes(x = Distance, fill = Adjusted_sex),
               alpha = 0.7, size = 0.2)+
  coord_flip() +
  ggpubr::fill_palette(palette = c("Red", "Blue", "Grey", "Gold"))
p1 <- insert_xaxis_grob(pmain, xdens, grid::unit(.2, "null"), position = "top")
p2<- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(p2)


pmeadows <- ggplot(spade, aes(x=SVL_mm, y=Distance_to_meadows, color=Adjusted_sex))+
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("Red", "Blue", "Grey", "Gold"))+ 
  xlab("SVL (mm)")+ 
  ylab("Distance to Nearest Meadow (m)")+ 
  ylim(0, 900)+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# Marginal densities along x axis
xdens <- axis_canvas(pmeadows, axis = "x")+
  geom_density(data = spade, aes(x =SVL_mm, fill = Adjusted_sex),
               alpha = 0.7, size = 0.2)+
  ggpubr::fill_palette(palette = c("Red", "Blue", "Grey", "Gold"))
# Marginal densities along y axis
# Need to set coord_flip = TRUE, if you plan to use coord_flip()
ydens <- axis_canvas(pmeadows, axis = "y", coord_flip = TRUE)+
  geom_density(data = spade, aes(x = Distance_to_meadows, fill = Adjusted_sex),
               alpha = 0.7, size = 0.2)+
  coord_flip() +
  ggpubr::fill_palette(palette = c("Red", "Blue", "Grey", "Gold"))
p3 <- insert_xaxis_grob(pmeadows, xdens, grid::unit(.2, "null"), position = "top")
p4<- insert_yaxis_grob(p3, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(p4)


pwetlands <- ggplot(spade, aes(x=SVL_mm, y=Distance_to_wetland, color=Adjusted_sex))+
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("Red", "Blue", "Grey", "Gold"))+ 
  xlab("SVL (mm)")+ 
  ylab("Distance to Nearest Wetland (m)")+ 
  ylim(0, 900)+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# Marginal densities along x axis
xdens <- axis_canvas(pwetlands, axis = "x")+
  geom_density(data = spade, aes(x =SVL_mm, fill = Adjusted_sex),
               alpha = 0.7, size = 0.2)+
  ggpubr::fill_palette(palette = c("Red", "Blue", "Grey", "Gold"))
# Marginal densities along y axis
# Need to set coord_flip = TRUE, if you plan to use coord_flip()
ydens <- axis_canvas(pwetlands, axis = "y", coord_flip = TRUE)+
  geom_density(data = spade, aes(x = Distance_to_wetland, fill = Adjusted_sex),
               alpha = 0.7, size = 0.2)+
  coord_flip() +
  ggpubr::fill_palette(palette = c("Red", "Blue", "Grey", "Gold"))
p5 <- insert_xaxis_grob(pwetlands, xdens, grid::unit(.2, "null"), position = "top")
p6<- insert_yaxis_grob(p5, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(p6)

multiplot(ggdraw(p2), ggdraw(p4), ggdraw(p6), cols=1)

