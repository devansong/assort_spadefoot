###############################################################
##R Code for Scaphiopus holbrookii network manuscript##########
##Simulations for influence of wetland on location#############
##Code written by Anne Devan-Song##############################
##Corvallis, Oregon, USA, June 2020############################
##Edited Dec 2020 in Bend, OR, USA#############################
###############################################################

############SETUP##############################################
rm(list = ls()) # clear working directory
graphics.off()
library(raster)
library(rgdal)
library(sf)
library(ggplot2)
library(dplyr)
library(spatstat)
library(raster)
library(abind)
library(reshape)

####FUNCTIONS#################################################
#######Define multiplot function in ggplot2#############
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


#####Function to turn data into degree coordinates############
reproject <- function(data){
  data.df <- as.data.frame(data)
  data.df.UTM <- SpatialPoints(data.df, proj4string=CRS("+proj=utm +zone=18 +datum=NAD83"))
  data.df.deg <- spTransform(data.df.UTM, CRS("+proj=longlat +datum=WGS84"))
  data.coords <- coordinates(data.df.deg)
  return(data.coords)
}


#####function to create random points #########
create_random <- function(N){
  rand.points <- st_sample(buffer, N, type = "random", exact = TRUE) #generate N random points
  rnd.points  <- st_coordinates(rand.points) #Extract coordinates from Shpfiles 
  rand_coords <- reproject(rnd.points)
  return(rand_coords)
}

#function to calculate distance and pull out nearest pond to nearest pond 
calc_dist <- function(data, pond.coords){
  dst<- pointDistance(data, pond.coords, lonlat=TRUE)
  nnd <- apply(dst, 1, FUN=min) #pull out distance to nearest pond for each random point 
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



##########Import habitat features around road############################### 
buffer <- st_read("Apr_2020_Polygon.shp")
#Import wetland locations, extract coordinates and convert to latlong 
ponds <- st_read("2017_PONDS.shp")
pond_points <- st_coordinates(ponds) #Extract coordinates from Shpfiles 
pond_coords <- reproject(pond_points) #reproject points 
############################################################################


#####Import main dataset: spadefoot data points and subset by F, M, and SNBA
spadefoot <- read.csv("2017_data_cleaned.csv")
spadefoot <- spadefoot %>% drop_na(SVL_mm) #drop any indiv with no SVL 


#Change this for every sex 
nrow(subset(spadefoot, Adjusted_sex =="NBA")) #243, 299, 74, 535
SEX <- "S"
SAMPLESIZE <- 535


##########From here onwards, functions are for 3 DIFFERENT DEMOGRAPHY#######
##for Female (F), Male (M), and subadult/nob-breeding adult (J)#############
############################################################################

spade <-subset(spadefoot, Adjusted_sex == SEX) #subset females by extracting gravid 
spade_points <- spade[c("X", "Y")] #subset F points 
spade_coords <- reproject(spade_points) #reproject to latlong 
spade_nnd <- calc_dist(spade_coords, pond_coords) #calc dist of each frog to nearest pond 
spade_nnd_df <- as.data.frame(spade_nnd) #coerce into dataframe 


RND = data.frame(id=seq(1, SAMPLESIZE, 1))
for (i in c(1:1000)) {
  rand<- create_random(SAMPLESIZE)
  rand_nnd<- calc_dist(rand, pond_coords)
  outdat= as.data.frame(rand_nnd)
  names(outdat) = c(paste0(i))
  RND = cbind(RND, outdat)
  gc()
  RND[,i] = as.array(rand_nnd)
}

RND$id <- c(1:SAMPLESIZE)
mRND <- melt(RND, id="id")
write.csv(mRND, "mRND_S_breedingpools.csv")

# Create data frame to bind all columns 
ECDF = data.frame(id=seq(1, 1251, 1))
df <- make_cdf(spade_nnd)# Do this for females: empirical
CDF <- data.frame(matrix(unlist(df), nrow=2, byrow=T),stringsAsFactors=FALSE)
CDF <- as.data.frame(t(CDF))
ECDF <- cbind(ECDF, CDF$V2) #bind dataframe with female empirical 

# Do this for all columns in Random females
for (i in c(2:1001)) {
  df_R <- make_cdf(RND[,i])
  test_i <- data.frame(matrix(unlist(df_R), nrow=2, byrow=T),stringsAsFactors=FALSE)
  test_i <- as.data.frame(t(test_i))
  ECDF<- cbind(ECDF, test_i$V2)
  gc()
}

prob <-extract_prob(ECDF)
prob$dist <- c(1:1251)

write.csv(prob, "/prob_f.csv")
subset_prob<- subset(prob, X0<0.00416666666 | X0>0.9958333)

fx1 <- c(38, 647)
fx2 <- c(355, 829)
fy1 <- c(rep(0, 2))
fy2 <- c(rep(1, 2))

fshade = data.frame(fx1, fx2, fy1, fy2)
write.csv(fshade, file="significant_xvals_S_breedingpools.csv", row.names = FALSE)

#####
pS <- ggplot() + ####REMEMEBER TO CHANGE THIS!!!! 
  geom_rect(data=fshade, 
            mapping=aes(xmin=fx1, xmax=fx2, ymin=fy1, ymax=fy2), fill='blue', alpha=0.3)+
  stat_ecdf(data=mRND, aes(value,  colour = variable, alpha=0.95)) + 
  scale_y_continuous(breaks = round(seq(min(0), max(1), by = 0.1),1), limits=c(0,1))+
  scale_x_continuous(breaks = round(seq(min(0), max(1000), by = 100),1), limits=c(0,890))+
  scale_color_manual(values=c(rep("darkgrey", 1000)))+
  stat_ecdf(data=spade_nnd_df, aes(spade_nnd), color="black", size=1.3) + 
  ggtitle("")+
  ylab("") + 
  xlab("")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none")

pS

png("Breedingpool_S.png", units="in", width=3, height=2.5, res=600)
pS
dev.off()

multiplot(pF, pM, pNBA, pS, cols=4)


