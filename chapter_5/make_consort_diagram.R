
library(DiagrammeR)
library(plyr)
library(tidyverse)
library(reshape2)
source("final_cleaning_scripts/generate_visits_df.R")

visits %>% group_by(arm) %>% summarise(w1 = sum(!is.na(w1)),
                                                w4 = sum(!is.na(w4)),
                                                w12 = sum(!is.na(w12)),
                                                w24 = sum(!is.na(w24)))

DiagrammeR::grViz("digraph {
  graph[]
  node[shape = 'box', width = 2, fontname = 'helvetica']
  
  subgraph a1{


{rank = same; b;b1};
 {rank = same; d;d1};
 {rank = same; f;f1};
 {rank = same; h;h1};
  {rank = same; j;j1};
a[label = 'Arm 1 \nScreened n = 347']
b [syle = 'invis', shape = 'point', width = 0.001, height = 0.001]
c[label = 'Enrolled \n225 participants']
d [syle = 'invis', shape = 'point', width = 0.001]
e[label = 'Week 1 \nxx participants']
f[syle = 'invis', shape = 'point', width = 0.001]
g[label = 'Week 4 \nxx participants']
h[syle = 'invis', shape = 'point', width = 0.001]
i [label = 'Week 12 \nxx participants']
j[syle = 'invis', shape = 'point', width = 0.001]
k[label = 'Week  \nxx participants']



b1[label = 'xx excluded \nxx died\nxx withdrew\nxx LTFU']
d1[label = 'xx excluded \nxx died\nxx withdrew\nxx LTFU']
f1[label = 'xx excluded \nxx died\nxx withdrew\nxx LTFU']
h1[label = 'xx excluded \nxx died\nxx withdrew\nxx LTFU']
j1[label = 'xx excluded \nxx died\nxx withdrew\nxx LTFU']

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
a_2[label = 'Arm 2 \nScreened']
b_2 [syle = 'invis', shape = 'point', width = 0.001, height = 0.001]
c_2[label = 'Enrolled \nxx participants']
d_2 [syle = 'invis', shape = 'point', width = 0.001]
e_2[label = 'Week 1 \nxx participants']
f_2[syle = 'invis', shape = 'point', width = 0.001]
g_2[label = 'Week 4 \nxx participants']
h_2[syle = 'invis', shape = 'point', width = 0.001]
i_2 [label = 'Week 12 \nxx participants']
j_2[syle = 'invis', shape = 'point', width = 0.001]
k_2[label = 'Week 24 \nxx participants']

b1_2[label = 'xx excluded \nxx died\nxx withdrew\nxx LTFU']
d1_2[label = 'xx excluded \nxx died\nxx withdrew\nxx LTFU']
f1_2[label = 'xx excluded \nxx died\nxx withdrew\nxx LTFU']
h1_2[label = 'xx excluded \nxx died\nxx withdrew\nxx LTFU']
j1_2[label = 'xx excluded \nxx died\nxx withdrew\nxx LTFU']


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
a_3[label = 'Arm 3 \nScreened']
b_3 [syle = 'invis', shape = 'point', width = 0.001, height = 0.001]
c_3[label = 'Enrolled \nxx participants']
d_3 [syle = 'invis', shape = 'point', width = 0.001]
e_3[label = 'Week 1 \nxx participants']
f_3[syle = 'invis', shape = 'point', width = 0.001]
g_3[label = 'Week 4 \nxx participants']
h_3[syle = 'invis', shape = 'point', width = 0.001]
i_3 [label = 'Week 12 \nxx participants']
j_3[syle = 'invis', shape = 'point', width = 0.001]
k_3[label = 'Week 24 \nxx participants']


b1_3[label = 'xx excluded \nxx died\nxx withdrew\nxx LTFU']
d1_3[label = 'xx excluded \nxx died\nxx withdrew\nxx LTFU']
f1_3[label = 'xx excluded \nxx died\nxx withdrew\nxx LTFU']
h1_3[label = 'xx excluded \nxx died\nxx withdrew\nxx LTFU']
j1_3[label = 'xx excluded \nxx died\nxx withdrew\nxx LTFU']


a_3 -> b_3[arrowhead = 'none']
b_3 -> c_3 
c_3 -> d_3[arrowhead = 'none'] 
d_3 -> e_3 
e_3 -> f_3[arrowhead = 'none']
f_3 -> g_3
g_3 -> h_3[arrowhead = 'none']
h_3 -> i_3
h_3 -> h1_3[arrowhead = 'none']
i_3 -> j_3[arrowhead = 'none']
j_3 -> k_3


b_3 -> b1_3 
d_3 -> d1_3
h_3 -> h1_3
f_3 -> f1_3
j_3 -> j1_3



  }

}")
