###############################################################
##R Code for Scaphiopus holbrookii network manuscript##########
##Simulations for influence of wetland om size#################
##Code written by Anne Devan-Song##############################
##Corvallis, Oregon, USA, June 2020############################
##Edited BEND Oregon, USA, Jan 2022############################
###############################################################

############SETUP##############################################
rm(list = ls()) # clear working directory
graphics.off()
setwd("~/Dropbox/Publications_Work/Ecosphere_REV/DATA") #set working directory
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
} #call multiplot function 

library(raster)
library(rgdal)
library(sf)
library(ggplot2)
library(dplyr)
library(spatstat)
library(abind)
library(reshape)
library(geosphere)
library(tidyverse)
library(rgeos)

calc_dist <- function(data, meadows.sp){
  nnd <- apply(gDistance(data, meadows.sp,byid=TRUE),2,min)
  return(nnd)
}

make_cdf <- function(data){
  breaks = seq(0, 1250, by=1) 
  nnd.cut = cut(data, breaks, right=FALSE) 
  nnd.freq = table(nnd.cut)
  cumfreq0 = c(0, cumsum(nnd.freq))
  out <- list("breaks" = breaks, "cumfreq0" = cumfreq0)
  return(out)
}

extract_prob <- function(df){
  prob_df = data.frame()
  for (i in 1:nrow(df)){  
    o_i = df[i,2]
    r_i = df[i, 3:length(df)]
    f_i = length(which(r_i>o_i))/length(r_i) 
    prob_df = rbind(prob_df, f_i)
  }
  return(prob_df)
}



#function to plot CDF of nnd (nearest neighbour distance)
plot_cdf <- function(data, ylimit){
  breaks = seq(0, 1250, by=1) 
  nnd.cut = cut(data, breaks, right=FALSE) 
  nnd.freq = table(nnd.cut)
  cumfreq0 = c(0, cumsum(nnd.freq))
  qplot(breaks, cumfreq0) + ylim(0, ylimit)
}



buffer <- st_read("~/Dropbox/Publications_Work/Ecosphere_REV/DATA/Shapefiles_rasters/Apr_2020_Polygon.shp")
#Import wetland locations, extract coordinates and convert to latlong 
meadows <- st_read("~/Dropbox/Publications_Work/Ecosphere_REV/DATA/Shapefiles_rasters/Meadows_2022.shp")
############################################################################

plot(buffer)
plot(meadows$geometry)
meadows.sp <- as(meadows, "Spatial")



spadefoot <- read.csv("~/Dropbox/Publications_Work/Ecosphere_REV/DATA/2017_data_cleaned.csv")
spadefoot <- spadefoot %>% drop_na(SVL_mm) #drop any indiv with no SVL 


##Set what we want to do here for subsetting 

####################################################

SEX <- "NBA"
####################################################

 
spade <-subset(spadefoot, Adjusted_sex == SEX) #subset females by extracting gravid 
spade_points <- spade[c("X", "Y")] #subset F points 
spts <- SpatialPoints(spade_points)

#Extract coordinates and reproject to latlong 
#spade_coords <- reproject(spade_points)

spade_nnd <- calc_dist(spts, meadows.sp) #calc dist of each frog to nearest pond 
plot_cdf(spade_nnd, 400)
spade_nnd_df <- as.data.frame(spade_nnd) #coerce into dataframe 


###CREATE ECDF FOR SIZE IN RELATION TO 
##Cumsize Juveniles#############

dist_size <- as.data.frame(cbind(spade_nnd_df$spade_nnd, spade$SVL_mm))
dist_size <- rename(dist_size, c("V1"="dist", "V2"="E_SVL_mm"))
dist_size <- na.omit(dist_size)

cumsize <- data.frame()
for (i in 1:1250){
  s <- subset(dist_size, dist <= i)
  sumsize <- sum(s$E_SVL_mm)
  cumsize <- rbind(cumsize, sumsize)
}

cumsize$id <- c(1:1250)
#cumsize$value <- cumsize$X0
colnames(cumsize) <- c("X0", "id")
plot(cumsize$id, cumsize$X0, type = "l")

#Simulate 1000 times, create vector of 303 size values that are drawn from the M size distribution
#Crate random sizes for 100 times
dist_size_random <- data.frame(dist_size$dist)
for (i in 1:1000){
  rand_size<- sample(dist_size$E_SVL_mm, replace = TRUE) 
  outdat = as.data.frame(rand_size)
  names(outdat) = c(paste("SVL",i, sep="_"))
  dist_size_random <- cbind(dist_size_random, outdat)
}
str(dist_size_random)


cumsize_rand <- data.frame(id=seq(1, 1250,1))
for (i in 2:1001){
  tmp_df <- data.frame()
  print(i)
  for (j in 1:1250){
    all_below <- dist_size_random['dist_size.dist'] <= j
    sumsize<- sum(dist_size_random[all_below, i]) # TODO understand
    tmp_df <- rbind(tmp_df, sumsize)
  }
  cname <- paste("cumSVL",i-1, sep="_")
  cumsize_rand[cname] <- tmp_df
}



cumsize_rand <-cbind(cumsize[, c(2,1)], cumsize_rand[, 2:1001])
prob_size <-extract_prob(cumsize_rand)
prob_size$dist <- c(1:1250)

colnames(prob_size) <- c("X0", "dist")

subset_prob<- subset(prob_size, X0<0.00416666666 | X0>0.9958333)


re_cumsize_rand <- melt(cumsize_rand, id="id")
sizeplot <- ggplot() + 
  geom_line(data=re_cumsize_rand, aes(x=id, y=(value/10000), color = variable, alpha = 0.75)) + 
  theme(legend.position = "none") + 
  scale_y_continuous(breaks = round(seq(min(0), max(0.4), by = 0.2),1), limits=c(0,0.4))+
  scale_x_continuous(breaks = round(seq(min(0), max(1000), by = 100),1), limits=c(0,1000))+
  ylab("Cumulative SVL/10^4 (mm)") +
  xlab("") +
  geom_line(data=cumsize, aes(x=id, y=(X0/10000)), color = "black") +
  ggtitle("NBA")+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        #axis.text.x = element_blank(), 
        legend.position = "none")
sizeplot


setwd("~/Dropbox/Publications_Work/Ecosphere_REV/FIGURES")
png("Meadow_NBA_SIZE.png", units="in", width=6, height=5, res=600)
sizeplot
dev.off()




#bind cumsize_m and cumsize_m_rand
cumsize_rand <-cbind(cumsize[, c(2, 1)], cumsize_rand[, 2:101])
prob_size <-extract_prob(cumsize_rand)
prob_size$dist <- c(1:1250)
#plot(prob_j_size$dist, prob_j_size$X0, type="l")
colnames(prob_size) <- c("X1", "dist")

cumprobplot <- ggplot(data=prob_size, aes(x=dist, y=X1)) +
  geom_line() + 
  theme(legend.position = "none") + 
  scale_y_continuous(breaks = round(seq(min(0), max(1), by = 0.1),1), limits=c(0,1))+
  scale_x_continuous(breaks = round(seq(min(0), max(1000), by = 100),1), limits=c(0,1000))+
  ggtitle("") +
  geom_hline(yintercept=0.025, linetype="dashed", 
             color = "red", size=0.9) + 
  geom_hline(yintercept=0.975, linetype="dashed", 
             color = "red", size=0.9) + 
  ylab("Cumulative SVL/10^4 (mm)") + 
  xlab("")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none")
cumprobplot
