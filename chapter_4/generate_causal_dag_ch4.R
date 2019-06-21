
library(DiagrammeR)

export_svg(DiagrammeR::grViz("digraph {
  
graph[layout = dot, rankdir = TB]

node [shape = box, fontname = Helvetica, fontsize = 10]

I [label = 'Infection']
S [label = 'Severity']
O [label = 'Outcome']
H [label = 'Host']
T [label = 'Therapy']

I -> S -> O

H -> I
H -> S
H -> O [style = dashed]
H -> T
T -> O 
I -> T
S -> T
I -> O [style = dashed]
}")
) -> sv

writeLines(sv, "chapter_4/figures/causal_dag_mortality.svg")
rsvg_pdf("chapter_4/figures/causal_dag_mortality.svg", "chapter_4/figures/causal_dag_mortality.pdf", width = 300, height = 500)
