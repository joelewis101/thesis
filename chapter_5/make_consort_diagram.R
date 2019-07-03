
library(DiagrammeR)
library(DiagrammeRsvg) #Needed if you want to export the image
library(rsvg) #Needed if you want to export the image

library(plyr)
library(tidyverse)
library(reshape2)
source("final_cleaning_scripts/generate_visits_df.R")

source("/Users/joelewis/Documents/PhD/R/PhD/ESBL/load_and_clean_lims.R")

withd.odk <- read.csv("/Users/joelewis/Documents/PhD/Data/Current/portal_downloads/dassim_withdraw_raw.csv")
visits %>% group_by(arm) %>% summarise(w1 = sum(!is.na(w1)),
                                                w4 = sum(!is.na(w4)),
                                                w12 = sum(!is.na(w12)),
                                                w24 = sum(!is.na(w24)))
# visit 1



lims %>% group_by(arm, visit) %>% tally()

visits$tcut <- cut(visits$died_date, c(-1,7,28,90,180, 500))

as.data.frame(visits %>% filter(status != 0) %>% group_by(arm, tcut, status) %>%
  tally())

merge(followup, select(enroll, pid, arm)) %>% group_by(arm, d2visit, foutcome) %>% tally()

filter(withd, pid %in% filter(visits, tcut == "(-1,7]", arm == 3, status == 2)$pid)
filter(withd, pid %in% filter(visits, tcut == "(28,90]", arm == 2, status == 2)$pid)

filter(withd.odk, pid %in% filter(visits, tcut == "(-1,7]", arm == 3, status == 2)$pid)
filter(withd.odk, pid %in% filter(visits, tcut == "(28,90]", arm == 2, status == 2)$pid)
]


export_svg(DiagrammeR::grViz("digraph {
  graph[nodesep = 1.5]
  node[shape = 'box', width = 2, fontname = 'helvetica']
  
  subgraph a1{

{rank = same; b;b1};
 {rank = same; d;d1};
 {rank = same; f;f1};
 {rank = same; h;h1};
  {rank = same; j;j1};
a[label = 'Arm 1: Sepsis \nScreened n = 347']
b [syle = 'invis', shape = 'point', width = 0.001, height = 0.001]
c[label = 'Enrolled \n225 participants\n222 samples collected']
d [syle = 'invis', shape = 'point', width = 0.001]
e[label = 'Day 7 visit\n203 participants eligible\n162 samples collected']
f[syle = 'invis', shape = 'point', width = 0.001]
g[label = 'Day 28 \n182 participants eligible\n148 samples']
h[syle = 'invis', shape = 'point', width = 0.001]
i [label = 'Day 90 \n163 participants eligible\n126 samples collected']
j[syle = 'invis', shape = 'point', width = 0.001]
k[label = 'Day 180\n148 participants eligible\n127 samples collected']



b1[label = 'Excluded n = 122\n Physiology criteria not met 64\nOutside Blantyre 81\nConfused no guardian 4\nAge 3']
d1[label = 'Excluded n= 22 \n19 died\n3 LTFU']
f1[label = 'Excluded n=21 \n21 died']
h1[label = 'Excluded n=19\n12 died\n5 withdrew\n2 transfer out']
j1[label = 'Excluded n=15\n8 died\n6 transfer out\n1 LTFU']

a -> b[arrowhead = 'none']
b -> c 
c -> d [arrowhead = 'none']
d -> e 
e -> f[arrowhead = 'none']
f -> g
g -> h[arrowhead = 'none']
h -> i
h -> h1
i -> j[arrowhead = 'none']
j -> k


b -> b1
d -> d1
h -> h1

f -> f1
j -> j1
  }

  subgraph a2 {

{rank = same; b_2;b1_2};
 {rank = same; d_2;d1_2};
 {rank = same; f_2;f1_2};
 {rank = same; h_2;h1_2};
 {rank = same; j_2;j1_2};
a_2[label = 'Arm 2: Inpatients \nScreened n = 122']
b_2 [syle = 'invis', shape = 'point', width = 0.001, height = 0.001]
c_2[label = 'Enrolled \n100 participants\n99 samples collected']
d_2 [syle = 'invis', shape = 'point', width = 0.001]
e_2[label = 'Day 7 visit \n89 participants eligible\n63 samples collected']
f_2[syle = 'invis', shape = 'point', width = 0.001]
g_2[label = 'Day 28 visit \n83 participants eligible\n70 samples collected']
h_2[syle = 'invis', shape = 'point', width = 0.001]
i_2 [label = 'Day 90 visit \n76 participants eligible\n59 samples collected']
j_2[syle = 'invis', shape = 'point', width = 0.001]
k_2[label = 'Day 180 visit \n66 participants eligible\n 65 samples collected']

b1_2[label = 'Excluded n = 22 \nOutside Blantyre 23\nDeclined 7\nAge 3\nAntimicrobials 1\nConfused no guardian 1']
d1_2[label = 'Excluded n = 11\n6 died\n2 withdrew\n3 LTFU']
f1_2[label = 'Excluded n = 6 \n5 died\n1 LTFU']
h1_2[label = 'Excluded n = 7 \n6 died\n1 transfer out']
j1_2[label = 'Excluded n = 10\n6 died\n transfer out 3\n1 withdrew']


a_2 -> b_2[arrowhead = 'none']
b_2 -> c_2 
c_2 -> d_2 [arrowhead = 'none']
d_2 -> e_2 
e_2 -> f_2[arrowhead = 'none']
f_2 -> g_2
g_2 -> h_2[arrowhead = 'none']
h_2 -> i_2
h_2 -> h1_2
i_2 -> j_2[arrowhead = 'none']
j_2 -> j1_2
j_2 -> k_2

b_2 -> b1_2
d_2 -> d1_2
h_2 -> h1_2

f_2 -> f1_2
  }


  subgraph a3 {

{rank = same; b_3;b1_3};
 {rank = same; d_3;d1_3};
 {rank = same; f_3;f1_3};
 {rank = same; h_3;h1_3};
 {rank = same; j_3;j1_3};
a_3[label = 'Arm 3:Community \nScreened n = 102']
b_3 [syle = 'invis', shape = 'point', width = 0.001, height = 0.001]
c_3[label = 'Enrolled \n100 participants\n99 samples collected']
d_3 [syle = 'invis', shape = 'point', width = 0.001]
e_3[style = 'invis', shape = 'point', width = 0.001]
f_3[style = 'invis', shape = 'point', width = 0.001]
g_3[label = 'Day 28 visit \n99 participants\n93 samples collected']
h_3[syle = 'invis', shape = 'point', width = 0.001]
i_3 [syle = 'invis', shape = 'point', width = 0.001]
j_3[style = 'invis', shape = 'point', width = 0.001]
k_3[label = 'Day 180 visit \n97 participants eligible\n 83 samples collected']


b1_3[label = 'Excluded n = 2\nAntimicrobials 2']
d1_3[style = 'invis', shape = 'point', width = 0.001]
f1_3[label = 'Excluded n = 1\n1 transfer out']
h1_3[style = 'invis', shape = 'point', width = 0.001]
j1_3[label = 'Excluded n= 3\n2 withdrew\n1 transfer out']


a_3 -> b_3[arrowhead = 'none']
b_3 -> c_3 
c_3 -> d_3[arrowhead = 'none'] 
d_3 -> e_3 [arrowhead = 'none'] 
e_3 -> f_3[arrowhead = 'none']
f_3 -> g_3
g_3 -> h_3[arrowhead = 'none']
h_3 -> i_3[arrowhead = 'none']
h_3 -> h1_3[style = 'invis']
i_3 -> j_3[arrowhead = 'none']
j_3 -> k_3


b_3 -> b1_3 
d_3 -> d1_3[style = 'invis']
h_3 -> h1_3[style = 'invis']
f_3 -> f1_3
j_3 -> j1_3



  }

}")) -> sv

writeLines(sv, "chapter_5/figures/ESBLstudy_flow_diagram.svg")
rsvg_pdf("chapter_5/figures/ESBLstudy_flow_diagram.svg", "chapter_5/figures/ESBLstudy_flow_diagram.pdf", width = 1200, height = 1200)

