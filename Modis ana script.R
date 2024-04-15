# 3. PRAKTIKA: NASA'S MODIS DATA 

# Libraries
library(RNetCDF)

# CREATE THE FULL NAME OF THE FILE
# 1.1. String for the path
setwd("C:/Users/DELL/Documents/MER/Semester 2/Satellite oceanography and meteology/Modis")
#inputpath<-"~/Desktop/SOM/12:04/"
inputpath <-"C:/Users/DELL/Documents/MER/Semester 2/Satellite oceanography and meteology/Modis/"

# 1.2. Concatenate PATH + NAME of the file
inputfile_1<-paste(inputpath,"AQUA_MODIS.20230123.L3m.DAY.CHL.chlor_a.9km.nc", sep="")
inputfile_2<-paste(inputpath,"AQUA_MODIS.20230123.L3m.DAY.SST.sst.9km.nc", sep="")
inputfile_1
inputfile_2

# 1. Chlorophyll
CHLOR_ini<-open.nc(inputfile_1)
# Summary
print.nc(CHLOR_ini)
# What we can see: 2160 positions in lat, 4320 positions in long, using 3 colors, 
# eightbitcolors (2ber8). Kolorena esplikau bar do: apuntetan. The first intesting variable for us
# is the concentra of chlor which is given as a matrix (lat and lon). Dakigu 4320 positions 
# diala en long eta 2160 en lat, apuntetan bisualki. 

# 1st variable: LATITUDE
lat_c<-var.get.nc(CHLOR_ini, "lat")

# 2nd variable: LONGITUDE
lon_c<-var.get.nc(CHLOR_ini, "lon")

# 3rd variable: most important one, CHLOROPHYLL CONCENTRATION
chlor<-var.get.nc(CHLOR_ini, "chlor_a")
dim(chlor)

# 2. SST
SST_ini<-open.nc(inputfile_2)
print.nc(SST_ini)
# What we can see: same w lat and lon (geographical grid is the same), colors too. 
# includes scale factor and an offset, so we need to include an additional command.

# 1st variable: LATITUDE
lat_s<-var.get.nc(SST_ini, "lat")

# 2nd variable: LONGITUDE
lon_s<-var.get.nc(SST_ini, "lon")

# 3rd variable: most important one, SST
sst<-var.get.nc(SST_ini, "sst", unpack=TRUE) #na.mode=1 #since we have scale factor we add 
# unpack true. The raw data should be multipled by 0.0049999 (summaryn agertzen dana), para obtener los datos reales.
# Unpact true nahibaeu beti jarri daikeu, ez do afektauko
# na.mode=1 is an additional protection against missing values we dont need it here, in class we cannot 
# know when we need it or not, w envisat we needed it. So, we can always put it. 

dim(sst)

# Is there a realtionship btw chlor and sst? yes, and we are going to check that. If tehre is a general 
# correlation worldwide btw these 2 variables.

# SO: we have sst matrix (4320,2160), same w chlor (4320, 2160). Now we're gonna rearrange the same info and 
# move from here to a column w the total ammount of grid points (4320x2160) of sst and another column w chlor 
# and n of rows corresponding to the total amount of grid points (4320x2160).
# Ordun bi kolumna eukikoitugu eta bien arteko korrelazioa ikertukoeu.
# we now have 2 empty matrixes:
# chlor_mat<-matrix(NA, 4320x2160,1)
# sst_mat<-matrix(NA, 4320x2160,1)
# for the first step we will need two loops: for (iii in seq (1,4320,1)){} and the second loop w 
# another variable: for (iii in seq (1.2160,1)){kounter<-kounter+1}. 
# probably the easiest way is to create a kounter and add it by one in each loop

# We need two matrixes with one column
# mat_1: (lon-lat-chlor)
# mat_2: (lon-lat-sst)

# We initialize two void matrixes
chlor_mat<-matrix(NA,4320*2160,1)
sst_mat<-matrix(NA,4320*2160,1)

#Long ranges from position 1 to 4320
#Lat ranges from position 1 to 2160

# We will nest two loops to fill the two matrixes

# Initialize a counter 
kounter<-0 # will give us the position of the rows, so I will increase it by one

# Eredua
for (ii in seq (1,4320,by=1)){
  for(jj in seq(1,2160,by=1)){
    kounter<-kounter+1
    # 1st column chlor
    chlor_mat[kounter,1]<-chlor[ii,jj]
    #2nd column sst
    sst_mat[kounter,1]<-sst[ii,jj]
  }
  # jj
}
# ii

############# Reala
#for (longitude in seq (1,4320,by=1)){
 # for(latitude in seq(1,2160,by=1)){
  #  kounter<-kounter+1
    # 1st column chlor
    #chlor_mat[kounter,1]<-chlor[longitude,latitude]
    #2nd column sst
    #sst_mat[kounter,1]<-sst[longitude,latitude]
  #}
  # latitude
#}
# longitude

################################################################## GAIZKI ATEATZEN BADA AURREKOA HAU ERABILI
chlor_mat <- matrix(NA, nrow = 4320 * 2160, ncol = 1)
sst_mat <- matrix(NA, nrow = 4320 * 2160, ncol = 1)

# Inicializar el contador
kounter <- 0

# Bucle para recorrer las coordenadas
#for (longitude in seq(1, 4320, by = 1)) {
  #for (latitude in seq(1, 2160, by = 1)) {
    #kounter <- kounter + 1
    # 1st column chlor
    #chlor_mat[kounter, 1] <- chlor[longitude, latitude]
    # 2nd column sst
    #sst_mat[kounter, 1] <- sst[longitude, latitude]
  #}
#}
for (longitude in seq(1, 4320, by=1)) {
  for (latitude in seq(1, 2160, by=1)) {
    if (longitude <= dim(chlor)[1] && latitude <= dim(chlor)[2]) {
      kounter <- kounter + 1
      chlor_mat[kounter, 1] <- chlor[longitude, latitude]
      sst_mat[kounter, 1] <- sst[longitude, latitude]
    }
  }
}
##################################################################

#By=1 means that the patter you are building for 1st variable (longitude), its 
# starts in 1 until 4320 at the steps of one.

# Bind two objects by column
cs<-cbind(chlor_mat[,1],sst_mat[,1])
# sometimes w have missing values, so we're going to choose only those rows where 
# we have actual values
# Select only cases w measurements for both variables, command: "complete.cases"
cs<-cs[complete.cases(cs),]

# Calculate the correlation btw both variables obtained from all the available 
# gridpoints this day: 2023-01-23
# Command: "cor.test"
cor_result<-cor.test(cs[,1],cs[,2]) #btw the 1st and 2nd column
# It is significant because of the p-value and the confidence interval is narrow 
# btw the cor value.

# OAIN MAPA INGOEU, BAT KLOROFILANTZAKO ETA BESTE BAT SST-AKO
# Make the map of chlor conc
chlor_map<-matrix(NA, 4320*32160,3)
# 3 variables: long-lat-conc

kounter<-0

for (ii in seq (1,4320,by=1)){
  for(jj in seq(1,2160,by=1)){
    kounter<-kounter+1
    #1st column longitude
    chlor_map[kounter,1]<-lon_c[ii]
    
    #2nd column latitude
    chlor_map[kounter,2]<-lat_c[jj]

    #3rd column chlor
    chlor_map[kounter,3]<-chlor[ii,jj]
  }
  # jj
}
# ii

# Just to check
chlor_map[33333:33335,]

# Adapt the script from topex or envisat
###### MAP OF SPAIN #############

library(sp)
library(maps)
library(mapdata)
library(shape)
library(reshape2)

# divide teh plotting area into two:matrix notation
layout(matrix(1:2,ncol = 2),width=c(1,4))

# create color palette for WEF ; 100 represents # of colors
coll<-colorRampPalette(c("red","orange","yellow", "green","blue"))(100)


# minimun observed chlor value
min(chlor_map[,3],na.rm = T) # 0.00193752
max(chlor_map[,3],na.rm = T) # 79.70502


# regression line borobiltzen deu 0tik 1ea porque balio ia danak tarte horretan dare
rescalecolor<-1+((100/1)*chlor_map[,3])

# colorscale goes to the LEFT SIDE
colorlegend(zlim=c(0,1),
            zval=seq(0,1,0.2),
            col=coll[1:100],main="Chlor. conc. \n (mg/m3) \n\n",
            main.cex=0.8,
            posx=c(0.2,0.35),posy=c(0.05,0.8),digit=1)


# to zoom, add x=c(0,20) and y=c(30,50)
# Define specific limits for Spain and the Canary Islands
#xlim_spain <- c(-18, 5)
#ylim_spain <- c(27, 45)
xlim_nigeria <- c(-10, 16)
ylim_nigeria <- c(2, 15)

# Trazar el mapa de España y las Islas Canarias
#map("world", regions = c("Spain", "Canary Islands"), col = "grey", fill = TRUE, xlim = xlim_spain, ylim = ylim_spain)
# worldhires is the world map object included in the package 
#pow_sat_aver2[,1]=lon
#pow_sat_aver2[,2]=lat
#points(chlor_map[,1],
#       chlor_map[,2],
 #      col=coll[rescalecolor], 
     #  pch=15) 

#map("world", regions = c("Spain", "Canary Islands", "Portugal"), col = "grey", fill = TRUE, xlim = xlim_spain, ylim = ylim_spain, add=T)
#map("world", regions = c("Europe"), col = "grey", fill = TRUE, xlim = xlim_spain, ylim = ylim_spain, add=T)
map("world", regions="Nigeria", col="grey", fill=TRUE, xlim=xlim_nigeria, ylim=ylim_nigeria)
points(chlor_map[, 1], chlor_map[, 2], col=coll[rescalecolor], pch=15)
map("world", regions="Nigeria", col="grey", fill=TRUE, xlim=xlim_nigeria, ylim=ylim_nigeria, add=TRUE)
axis(1);axis(2)
title(main="Chlorophyll concentration (mg/m3) \n JAN 23, 2023 in SPAIN and PORTUGAL",xlab="ºE")

## Arbelen idatzi do Olaiantzako: chlor_basque<-chlor_map[(chlor_map[,1]>-14)&(chlor_map[,1]<4)&(chlor_map[,2]>34)&(chlor_map[,2]<46),]
############ BESTE AURKERA BAT?
#min_chlor <- min(chlor_map[,3], na.rm = TRUE)  # 0.00193752
#max_chlor <- max(chlor_map[,3], na.rm = TRUE)  # 79.70502

#rescalecolor <- (chlor_map[,3] - min_chlor) / (max_chlor - min_chlor)
############ 

