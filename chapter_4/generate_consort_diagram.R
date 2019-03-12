### generate consort diagram

library(DiagrammeR)

library(DiagrammeR)
library(DiagrammeRsvg) #Needed if you want to export the image
library(rsvg) #Needed if you want to export the image

grViz("digraph {
  graph [fontsize=10]
  node [shape=box] 
  Q21[shape=point]
  Q22

  Q12
  Q11

  
  Q21 -> Q22 [constraint=false]

  
  Q11 -> Q21

  Q21 -> Y1
  
  Q12 -> Q22

  Q22 -> Y2
  Y1 -> Y2[constraint=false]
  
Y1 -> Y3
Y3 -> Y4


  
 
      {rank = same; Q11; Q12;}
      {rank = same; Q21; Q22}
      {rank = same; Y1; Y2}

  Y2 [fillcolor=lightgray,style=filled]
  Y1 [fillcolor=lightgray,style=filled]

}")

grViz(diagram =" digraph {
  graph [fontsize=10]
      node [shape=box, width = 2, fontname = Arial] 
  Screened[pos='1,1',pin=true,label = 'Participants Screened \n n = xxx']
  Screened -> A[arrowhead='none']
  A -> Enrolled
  Enrolled -> B[arrowhead='none'] 
  B-> D28 
  D28 -> C[arrowhead='none'] 
  C-> D90
  D90-> D[arrowhead='none'] 
  D-> D180
  E[width = 3.5]
  E-> Excl[style='invis'] 
  Excl-> G[style = 'invis']
  G -> LTFU_B4_D28[style='invis']
  LTFU_B4_D28-> Died_by_D28[style = 'invis']
  Died_by_D28-> LTFU_B4_D90 [style='invis']
  LTFU_B4_D90 -> Died_D28_to_90[style = 'invis'] 
  Died_D28_to_90-> LTFU_B4_D180[style = 'invis']
  LTFU_B4_D180 -> Died_D90_to_180[style = 'invis']
A[shape='point', width = 0.001]
B[shape ='point', width = 0.001]
C[shape = 'point', width = 0.001]
D[shape = 'point', width = 0.001]
  E[style='invis']
  G[style = 'invis']

  edge [constraint=false]
  A -> Excl
  B ->  LTFU_B4_D28
  C -> LTFU_B4_D90

D -> LTFU_B4_D180
D28-> Died_by_D28
D90-> Died_D28_to_90
D180 -> Died_D90_to_180



 Excl[label = 'Excluded n = xxx \n reason 1 xxx \n reason 2 xxx']
  Enrolled[label = 'Enrolled \n n = xxx']
  LTFU_B4_D28[label = 'Excluded n = xxx \n Withdrawn n = xxx \n LTFU n = xxx']
  D28[label = 'Included in D28 analysis \n n = xxx \n']
  Died_by_D28[label = 'Died n = xxx' ]
  LTFU_B4_D90[label = 'Excluded n = xxx \n Withdrawn n = xxx \n LTFU n = xxx']
  D90[label = 'Included in D90 analysis \n n = xxx \n']
  Died_D28_to_90[label = 'Died n = xxx' ]
  LTFU_B4_D180[label = 'Excluded n = xxx \n Withdrawn n = xxx \n LTFU n = xxx']
  D180[label = 'Included in D180 analysis \n n = xxx \n']
  Died_D90_to_180[label = 'Died n = xxx' ]
      }")

