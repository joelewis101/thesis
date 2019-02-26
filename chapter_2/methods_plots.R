# plot malawi map

data("World")
africa <- World %>% filter(continent == "Africa")


mal <- readShapePoly("/Users/joelewis/Downloads/MAA-level_1_SHP/MAA-level_1.shp")
mal2 <- fortify(mal)

ggplot(africa) + geom_sf(aes(fill = name == "Malawi"), colour = "black") +
  scale_fill_manual(values = c("white", "black")) + 
  geom_sf(data = subset(africa, name == "Malawi"), colour = "darkgreen", fill = "darkgreen") +
 theme(legend.position = "none")+ theme_inset() +
  ylim(-4200000, 1500000) -> p1 #+ xlim(1000000, 5000000) #-> p1
  # theme_bw() + 
  #  theme(#panel.border = element_blank(),
  #       axis.line.x = element_blank(), 
  #      axis.line.y =  element_blank(), 
  #     axis.ticks = element_blank(), 
  #    axis.text = element_blank(), 
  #   legend.position = "none") +


-> p1





afbox <- bbox <- c(left = -21.22, right = 55.02, top= 17.17, bottom = -37.75)

bbox <- c(left = 25.840, right = 42.737, top= -8.277, bottom = -18.167)
bbox2 <- c(left = 25.955, right = 38.717, top= -8.277, bottom = -18.167)
get_stamenmap(bbox2, zoom = 8) -> m
get_stamenmap(bbox2, zoom = 7, maptype = "terrain-background") -> m2

get_stamenmap(afbox, zoom = 4, maptype= "terrain-background") -> a

ggmap(m2) 

cities <- data.frame(city = c("Blantyre", "Lilongwe"), lat = c(-15.7667,-13.9626), lon = c(35.0168, 33.7741))


ggmap(a) + geom_polygon(data = mal2, aes(x= long, y = lat, group = group), fill = "black") +
  theme_inset() +
  geom_rect(xmin = bbox2["left"], xmax = bbox2["right"], ymin = bbox2["bottom"], ymax = bbox2["top"], fill = NA, colour = "black" )-> p1

ggmap(m2) +
  geom_path(data = mal2, aes(x= long, y = lat, group = group), alpha = 0.8) + 
  geom_point(data = cities, aes(x = lon, y = lat)) + 
  geom_text(data = cities, aes(x = lon, y = lat,label = city,  vjust = -0.5, hjust = 0.5, size = 7, fontface = "bold")) +
  theme_void() + theme(legend.position = "none") +  inset(grob = ggplotGrob(p1), xmin = 26.5, xmax = 31.5, ymin = -13, ymax = -9)
  
  #-> p2

ggmap(m) +
  geom_path(data = mal2, aes(x= long, y = lat, group = group), alpha = 0.8) + 
  geom_point(data = cities, aes(x = lon, y = lat)) + 
  geom_text_repel(data = cities, aes(x = lon, y = lat,label = city, size = 7, fontface = "bold")) +
  theme(legend.position = "none") #+ theme_void()  + 

ggmap(m2) + scalebar(mal2, dist = 1000, dist_unit = "km",
                     transform = FALSE, model = "WGS84")
