
#########################################################
#                                                       #
# Spatial Analysis (Geog 2017; NTU Geography)           #
#                                                       #
# Lecture 1: Spatial Data Handling                      #
#                                                       #
# Instructor: Dr. Tzai-Hung Wen                         #
#                                                       #
# Source: Brunsdon and Comber(2018), Chapter 3          #
#                                                       #
#########################################################

rm(list = ls())

library(sf)
library(tmap)
library(pals) # collection of color palettes
library(cartography) # classification methods

setwd("D:/1092SA/Data")
load("Sample.RData")

########## 0. sf format and coordinates ###########

class(blocks_sf)
head(blocks_sf)

# extracing as a new layer
new_sf<-blocks_sf[,6]
new2_sf<-blocks_sf[1:3,]

# attribute table
blocks_df<- as.data.frame(blocks_sf)
class(blocks_df)

# coordinate system
st_crs(blocks_sf)
st_crs(roads_sf)
st_crs(roads_sf)<-st_crs(blocks_sf)

# export/import shapefiles
st_write(blocks_sf,"blocks.shp", delete_layer = TRUE)
blocks2_sf<- st_read("blocks.shp")


########## 1. Mapping Spatial Objects ###########

# Using plot()
plot(blocks_sf)
plot(blocks_sf["P_VACANT"], breaks = "jenks", nbreaks = 6)
#(blocks_sf$P_VACANT ?)

brewer.blues(6)
plot(blocks_sf["P_VACANT"], breaks = "jenks", nbreaks = 6, pal=brewer.blues(6))

display.brewer.all() # setting colors

# Using tmap package
?qtm # quick tmap
qtm(blocks_sf, fill="P_VACANT", style="natural")
colors()

# choropleth
lyr1<- qtm(blocks_sf, fill="P_VACANT", fill.title="Vacant %", title="My Map 1")

# bubble map
lyr2<- qtm(blocks_sf, symbols.size="P_VACANT", symbols.title.size="Vacant %", title="My Bubble Map")

# lines
lyr_road <- tm_shape(roads_sf)+tm_lines(col="orange")

# points
lyr_crimes <- tm_shape(breach_sf)+tm_dots(col="red", size= 0.1)

# overlay multiple plots
lyr1+lyr_crimes

st_crs(breach_sf)
st_crs(blocks_sf)

# showing multiple plots

library(grid)
# open a new plot page
grid.newpage()
# set up the layout
pushViewport(viewport(layout=grid.layout(2,2)))
# plot using the print command
print(lyr1, vp=viewport(layout.pos.col = 1,layout.pos.row=1))
print(lyr2, vp=viewport(layout.pos.col = 2,layout.pos.row=1))
dev.off() # reset

########## 2. Attribute Query ###########

index <- (blocks_sf$P_VACANT > 10)
newblocks_sf <- blocks_sf[index,]
lyr3<- qtm(newblocks_sf, fill="red", title="Vacant > 10%", style="natural")
lyr_bg<- qtm(blocks_sf, fill="grey")
lyr_bg+lyr3

########## 3. Calculating Fields ###########

# add a new AREA field
x<-st_area(blocks_sf) # unit: foot

library(units)
x2<-set_units(x, km^2)

blocks_sf$AREA1 <- x2

# remove a field
# blocks_sf <- subset(blocks_sf, select = -c(AREA1))

head(blocks_sf)

blocks_sf$POPDEN <- blocks_sf$POP1990 / blocks_sf$AREA1

plot(blocks_sf["POPDEN"], breaks = "jenks", nbreaks = 6, pal= brewer.blues(6))
qtm(blocks_sf, fill="POPDEN", fill.title="Population Density", title="Popn Map", style="natural")


########## 4. Detailed Settings for Mapping ###########

breakv<- getBreaks(v = blocks_sf$P_OWNEROCC, nclass = 6, method = "jenks")
# classification method:"fixed", "sd", "equal", "pretty", "quantile", "kmeans", 
# "hclust", "bclust", "fisher", "jenks", "dpih", "q6", "geom", "arith", "em", "msd" 

tm_shape(blocks_sf) +
  tm_polygons("P_OWNEROCC", title = "Owner Occ", palette = "-GnBu", 
              breaks = breakv, 
              legend.hist = T) +
  tm_scale_bar(width = 0.22) +
  tm_compass(position = c(0.8, 0.08)) +
  tm_layout(frame = F, title = "New Haven", 
            title.size = 2, title.position = c(0.55, "top"), 
            legend.hist.size = 0.5)


########## 5. Interactive mapping ###########

tmap_mode("view")
lyr5<- tm_shape(blocks_sf)+tm_polygons("POPDEN", alpha=0.5)
lyr5+lyr_crimes

ttm()

########## 6. Statistical plots ###########

hist(blocks_sf$P_VACANT, breaks = 40, col = "grey", 
     border = "red", 
     main = "The histogram of vacant property percentages", 
     xlab = "percentage vacant", xlim = c(0,40))

boxplot(blocks_sf$P_WHIT, blocks_sf$P_BLACK, names=c("White", "Black"), 
        xlab="Race", ylab="Percentage")

library(ggplot2)
library(reshape2)

blocks_df <- as.data.frame(blocks_sf)
head(blocks_df)

plot1<- ggplot(blocks_df) + aes(P_VACANT) +
          geom_histogram(col = "red", fill = "grey", bins = 40) +
          xlab("percentage vacant") +
          labs(title = "The histogram of vacant property percentages")

blocks2_df<- melt(blocks_df[, c("P_WHITE", "P_BLACK", "P_AMERI_ES")])
head(blocks2_df)

plot2<- ggplot(blocks2_df) + 
           aes(variable, value) +
           geom_boxplot()

# showing map+plot
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(lyr1+lyr_crimes, vp=viewport(layout.pos.col = 1))
print(plot2, vp=viewport(layout.pos.col = 2))

dev.off()

#==========================

# Data for Labs and Homework

TWN_sf<- st_read("Popn_TWN2.shp", options="ENCODING=BIG5",quiet = T)
st_crs(TWN_sf)
head(TWN_sf)

EPA_sf<- st_read("EPA_STN1.shp", options="ENCODING=BIG5")
st_crs(EPA_sf)
head(EPA_sf)

plot(TWN_sf["A0A14_CNT"], breaks = "jenks", nbreaks = 6, pal=brewer.blues(6))

tmap_mode("view")
lyr6<- tm_shape(TWN_sf)+tm_polygons("A0A14_CNT", alpha=0.5)
lyr6

ttm()

