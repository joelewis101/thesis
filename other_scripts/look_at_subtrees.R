ggtree(tree2)


viewClade(t2, 1866) %<+% select(metadata, Lane.Name, `This study`) + 
  geom_tiplab(size = 2, aes(color = `This study`)) + 
  theme(legend.position = "none") + scale_color_manual(values = c("black", 'red', "grey"))

ggtree(tree2) + geom_text(aes(label=node), hjust=-.3, size = 2) -> t3

viewClade(t3, 1859) + 
  geom_cladelabel(node = 1866, label = "ST410", fontsize = 2.5)

gzoom(t3, 485:459) + 
  geom_cladelabel(node = 1866, label = "ST410", fontsize = 2.5)


viewClade(t2, 1864) %<+% mlst[c(2,1)] + geom_tiplab(size = 2, aes(color = ST == 410)) + geom_text(aes(label=node), hjust=-.3, size = 2)

viewClade(t2,111)

# ST 410 is node 1864

treeio::tree_subset(tree2, 1864, levels_back = 0) -> st410tree

select(metadata, Lane.Name, `This study`) -> this.study
this.study$`This study` <- as.character(this.study$`This study`)
this.study$`This study`[this.study$`This study` == 1] <- "Yes"
this.study$`This study`[this.study$`This study` == 0] <- "No"

#colz <- c( "grey11", "grey89", brewer.pal(name= "Set3", n = 7)[1:5] )

metadata$Country[metadata$Country == "thailand"] <- "Thailand"

colz <- c( "grey11", "grey89", hue_pal()(5))
names(colz) <- c( "1","0","Malawi", "Guatemala",  "Thailand" , "Mexico", "Bolivia")
ggtree(st410tree) %<+% this.study   %>%
  #gheatmap(select(esbl.cat, ESBL), width = 0.05, color = NA, font.size = 4, colnames_angle = 90,
   #        colnames_position = "top", offset =  0.0003) %>%
  gheatmap(select(metadata, Country), width = 0.05, color = NA, colnames = F,font.size = 4, colnames_angle = 90,
           colnames_position = "top", offset = 0.0003 , colnames_offset_y = 2) + 
  geom_tiplab(size = 1, align = T, aes(color = `This study`)) + 
  geom_treescale(x = 0.0005, y = 30, width = 0.0003) + scale_fill_manual(values = colz) +
  scale_color_manual(values = c("black", hue_pal()(1))) + theme(legend.position = "none") -> a


data.frame(country = c("Malawi", "Guatemala",  "Thailand" , "Mexico", "Bolivia"), n = c(1,2,3,4,5)) -> fudge
fudge$country <- factor(fudge$country, levels = c("Malawi", "Thailand", "Guatemala", "Mexico", "Bolivia"))
ggplot(fudge, aes(country, n, fill = country)) + geom_col()+ scale_fill_manual(values = colz) + 
  theme_bw() + theme(legend.title = element_blank(), legend.position = "bottom")-> fudgeplot

leg <- get_legend(fudgeplot)

## ST131 is node 1351

treeio::tree_subset(tree2, 1351, levels_back = 0) -> st131tree

ggtree(st131tree) %<+% select(metadata, Lane.Name, `This study`)   %>%
 # gheatmap(select(esbl.cat, ESBL), width = 0.05, color = NA, font.size = 4, colnames_angle = 90,
  #         colnames_position = "top", offset =  0.0002) %>%
  gheatmap(select(metadata, Country), width = 0.05, color = NA, colnames = F,font.size = 4, colnames_angle = 90,
           colnames_position = "top", offset = 0.0004, colnames_offset_y = 5) + 
  geom_tiplab(size = 1, align = T, aes(color = `This study`)) + geom_treescale(x = 0.0005, y = 57,offset = 1 )+ scale_fill_manual(values = colz) +
  scale_color_manual(values = c("black", hue_pal()(1)))  + theme(legend.position = "none") -> b

ggarrange(ggarrange(a,b, ncol = 2, nrow =1, labels = c("A: ST410", "B: ST131") ),
          ggarrange(NULL,leg,NULL, widths = c(1,0.5,1), ncol = 3),
          heights = c(1,0.1), ncol = 1, nrow = 2)

## 2020 is ST 167

treeio::tree_subset(tree2, 2020, levels_back = 0) -> st167tree

ggtree(st167tree) %<+% select(metadata, Lane.Name, `This study`)   %>%
  # gheatmap(select(esbl.cat, ESBL), width = 0.05, color = NA, font.size = 4, colnames_angle = 90,
  #         colnames_position = "top", offset =  0.0002) %>%
  gheatmap(select(metadata, Country), width = 0.05, color = NA, colnames = F,font.size = 4, colnames_angle = 90,
           colnames_position = "top", offset = 0.00005, colnames_offset_y = 5) + 
  geom_tiplab(size = 1, align = T, aes(color = `This study`)) + geom_treescale(x = 0.00005, y = 30,offset = 1, width = 0.00003 )+ scale_fill_manual(values = colz) +
  scale_color_manual(values = c( hue_pal()(1)))  + theme(legend.position = "none") -> c

