library(DiagrammeR)



DiagrammeR::grViz("digraph {
  
graph[compound = true, layout = dot, rankdir = LR, color = black, style = dashed, fontname= Arial, fontsize = 18]

node [shape = rectangle, style = filled, fillcolor = GhostWhite, fontname = Arial]

subgraph cluster_arm_3 {
label = 'Arm 3: Community'
k[label = 'Enrollmemt \n Stool']
l[style = 'invis']
m[label = 'Week 4 \n Stool']
n[style = 'invis']
o[label = 'Week 24 \n Stool']


k -> l [style = 'invis']
l -> m [style = 'invis']
m -> n [style = 'invis']
n -> o [style = 'invis'] 

k -> m
m -> o

}


subgraph cluster_arm_2 {
label = 'Arm 2: Hospital inpatients, antibiotic unexposed'
f[label = 'Enrollment \n Stool']
g[label = 'Week 1 \n Stool']
h[label = 'Week 4 \n Stool']
i[label = 'Week 12 \n Stool']
j[label = 'Week 24 \n Stool']


f -> g -> h -> i -> j
}



subgraph cluster_arm_1 {
label = 'Arm 1: Hospital inpatients, sepsis'
a[label = 'Enrollment \n Blood \n Stool \n Urine >']
b[label = 'Week 1 \n Stool']
c[label = 'Week 4 \n Blood \n Stool']
d[label = 'Week 12 \n Stool']
e[label = 'Week 24 \n Stool']

z[label = 'Baseline investigations \n FBC \n U&E \n Lactate \n mRDT \n HIV Ab \nCD4* \n Aerobic blood culture \n Mycobacterial blood culture \nAcute and convalescent serum save\n uLAM \n FASH USS \n Sputum Xpert** \n CXR**']

a -> b -> c -> d -> e



}



}") 


# export 620 x 519

