#########################################################
#                                                       #
# Spatial Analysis (Geog 2017; NTU Geography)           #
#                                                       #
# Lecture 2: Using R as a GIS                           #
#                                                       #
# Instructor: Dr. Tzai-Hung Wen                         #
#                                                       #
# Source: Brunsdon and Comber(2018), Chapter 5          #
#                                                       #
#########################################################

rm(list = ls())

library(sf)
library(tmap)

setwd("D:/1092SA/Data")
load("Sample2.RData")

########## 1. Intersection ###########

## 1.1 Tornado Damage Assessment 

# Mapping
us_bg <- tm_shape(us_states_sf) + tm_polygons("grey90")
tornado <- tm_shape(torn_sf) + tm_dots(col = "#FB6A4A", size = 0.04, shape = 1, alpha = 0.5)
us_border <- tm_shape(us_states_sf) + tm_borders(col = "black") + tm_layout(frame = F) 
us_bg + tornado + us_border

st_crs(us_states_sf)

summary(torn_sf)
st_geometry(torn_sf)

# Attribute selection
index <- us_states_sf$STATE_NAME == "Texas" | us_states_sf$STATE_NAME == "New Mexico" | 
         us_states_sf$STATE_NAME == "Oklahoma" | us_states_sf$STATE_NAME == "Arkansas"  

AoI_sf <- us_states_sf[index,]
AoI_bg <- tm_shape(AoI_sf) + tm_polygons("grey90")

# Clip
torn_clip_sf <- torn_sf[AoI_sf,]
torn_clip <- tm_shape(torn_clip_sf) + tm_dots(col = "#FB6A4A", size = 0.04, shape = 1, alpha = 0.5)
AoI_bg + torn_clip

head(torn_clip_sf)

# Intersection
AoI_torn_sf <- st_intersection(AoI_sf,torn_clip_sf)
head(AoI_torn_sf)

AoI_torn_df <- as.data.frame(AoI_torn_sf)
head(AoI_torn_df)
summary(AoI_torn_df)

# Creating new data frame
AoI_torn_df$STATE_NAME <- droplevels(AoI_torn_df$STATE_NAME) 
newdf<- data.frame(Name = AoI_torn_df$STATE_NAME,Damage= AoI_torn_sf$DAMAGE)
head(newdf)
newdf$Damage <- as.integer(newdf$Damage)

# Crosstab analysis
count<- table(newdf$Damage, newdf$Name)
count

count2<- xtabs(~ newdf$Damage + newdf$Nam )
count2

## 1.2 Mapping Tornado Density
# (Counting Points in Polygons)

new1<- st_intersection(us_states_sf, torn_sf)

df2<- data.frame(new1$STATE_NAME,new1$TOR_NO)
df3<- table(new1$STATE_NAME)
class(x)

df3<-as.data.frame(df3)
colnames(df3) <- c("STATE_NAME","Counts")
head(df3)
head(us_states_sf)

library(dplyr)
us_states_sf<- left_join(us_states_sf, df3)
head(us_states_sf)

area1<-st_area(us_states_sf) # unit: foot

library(units)
area1<-set_units(area1, km^2)
us_states_sf$AREA1 <- area1
us_states_sf$Density <- us_states_sf$Counts / us_states_sf$AREA1

library(pals) # collection of color palettes
plot(us_states_sf["Density"], breaks = "jenks", nbreaks = 6, pal=brewer.reds(6))


########## 2. Buffer Zone ###########

st_crs(places_sf)
st_crs(places_sf)<- st_crs(blocks_sf)
head(places_sf)

block_bg <- tm_shape(blocks_sf) + tm_polygons("grey90")
places_pts <- tm_shape(places_sf) + tm_dots(col = "#FB6A4A", size = 0.5)

block_bg + places_pts

places_buf_sf <- st_buffer(places_sf, dist = 3000)
summary(places_buf_sf)

places_buf <- tm_shape(places_buf_sf) + tm_polygons("yellow")
block_bg + places_buf + places_pts

places_bufU_sf <- st_union(places_buf_sf)
head(places_bufU_sf)

places_bufU <- tm_shape(places_bufU_sf) + tm_polygons("yellow")
block_bg + places_bufU + places_pts


########## 3. Distance Analysis ###########

head(blocks_sf)
# using st_centroid()
blocks_center_sf <- st_centroid(blocks_sf)
blocks_center <- tm_shape(blocks_center_sf) + tm_dots(col = "blue", size = 0.1)
block_bg +blocks_center
blocks_center + places_pts

st_distance(blocks_center_sf[1,], places_sf[2,])

# using st_distance()
distance.matrix <- st_distance(blocks_center_sf, places_sf)
distance.matrix<-set_units(distance.matrix, km)

# using apply()
near_dist<- apply(distance.matrix, 1, mean)
near_dist[1]

xid<- which.min(near_dist)
near_dist[xid]
blocks_sf[xid,]
blocks_sf$POP1990[xid]

min_dist<- apply(distance.matrix, 1, min)
sel_blocks<- min_dist < 3000
sel_sf <- blocks_sf[sel_blocks,]
sel_map <- tm_shape(sel_sf) + tm_polygons("yellow")
block_bg + sel_map + places_pts


# using st_is_within_distance()
d1<- set_units(1, km)
sel_blocks = st_is_within_distance(blocks_center_sf, places_sf, dist = d1)
class(sel_blocks)
sel_blocks<- lengths(sel_blocks) > 0
sel_sf <- blocks_sf[sel_blocks,]
sel_map <- tm_shape(sel_sf) + tm_polygons("yellow")
block_bg + sel_map + places_pts

# choose specific one place
sel_blocks = st_is_within_distance(blocks_center_sf, places_sf[3,], dist = d1)
sel_blocks <- lengths(sel_blocks) > 0
sel_sf <- blocks_sf[sel_blocks,]
sel_map <- tm_shape(sel_sf) + tm_polygons("yellow")
block_bg + sel_map + places_pts

TOTAL_POP <- sum(sel_sf$POP1990)
TOTAL_WHITE <- sum(sel_sf$POP1990 * (sel_sf$P_WHITE/100) )

# accessibility analysis
# using apply()
eth<- as.data.frame(blocks_center_sf[,14:18])
eth<- as.matrix(eth[,1:5])
eth<- apply(eth, 2, function(x) (x*blocks_center_sf$POP1990))
colnames(eth)<- c("White","Black","Native","Asian","Other")
eth.access <- xtabs(eth~sel_blocks) 

eth.access <- as.data.frame(eth.access)
colnames(eth.access) <- c("Access","Eth5","Pop")

library(ggplot2)
ggplot(eth.access) + aes(fill=Access, y=Pop, x=Eth5) + 
  geom_bar(position="stack", stat="identity")
