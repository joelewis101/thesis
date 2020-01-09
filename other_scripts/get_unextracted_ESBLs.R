
# generate sequencing manifests

# output - df of all DASSIM samples with lab ID, species, extracted Y/N sequenced Y/N passed QC Y/N

# data sources -> phD script
# shipped but not yet extracted samples

# and LIMS list of samples

# first - what has been sequenced 


wd <- "chapter_7/"

# and packages
library(plyr)
library(tidyverse)
library(reshape2)
library(knitr)
library(kableExtra)
library(grid)
library(ggpubr)
library(ggtree)
library(ape)
library(phytools)
library(RColorBrewer)
library(scales)
library(cowplot)
library(pheatmap)
library(viridis)
library(ggraph)
library(igraph)
library(gggenes)
library(IRanges)
library(glue)


parse_checkm_lines <- function(s, n) {
  strsplit(s, " ") -> stemp
  stemp <- stemp[[1]][stemp[[1]] != ""] 
  stemp <- c(stemp[1], paste0(stemp[2],"_", stemp[3]), stemp[4:15])
  names(stemp) <- n
  return(stemp)
}


output = "latex"

source("final_cleaning_scripts/load_and_clean_followup_and_enroll_labelled.R")
source("other_scripts/summary_table_functions.R")
source("final_cleaning_scripts/make_composite_hivstatus_variable.R")
source("final_cleaning_scripts/load_and_clean_hourly.R")
source("final_cleaning_scripts/load_and_clean_bloods.R")
#source("final_cleaning_scripts/load_and_clean_aetiol.R")
#source("final_cleaning_scripts/load_and_clean_hourly.R")
source("final_cleaning_scripts/load_and_clean_upto72.R")
source("final_cleaning_scripts/load_and_clean_post72.R")
source("final_cleaning_scripts/load_and_clean_hosp_oc.R")
#source("final_cleaning_scripts/load_and_clean_time_to_ab.R")
#source("final_cleaning_scripts/load_and_clean_fluid_hr1_to_6.R")


# lims




#panel data helper functions
source("other_scripts/panel_data_helpers/expand_covariates.R")
source("other_scripts/panel_data_helpers/sort_out_tb_rx_on_discharge.R")
source("other_scripts/panel_data_helpers/shuffle_a_in_b2.R")
source("other_scripts/panel_data_helpers/strip_post_dropout_rows.R")
source("other_scripts/panel_data_helpers/extract_covariate_exposure.R")
source("other_scripts/panel_data_helpers/collapse_covariates.R")
source("other_scripts/panel_data_helpers/ditch_everything_after_first_1.R")
source("other_scripts/panel_data_helpers/mstate_helper_functions.R")
source("other_scripts/panel_data_helpers/splice_ESBL2.R")

names(enroll)[names(enroll) == "data_date"] <- "enroll_date"
followup <- merge(followup, select(enroll, pid, arm, enroll_date), all.x = T)
followup$t <- followup$data_date - followup$enroll_date

# and load lims
source("final_cleaning_scripts/load_and_clean_lims.R")


source("other_scripts/load_metadata.R")


source("other_scripts/function_parse_cdhitest.R")
source("other_scripts/parse_blast_op_IRranges.R")

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

# sample_ids contains sample metadata of ALL SEQUENCED D1 and D2 isolates

# lims_orgs contains organisms


# tree

tree <- read.tree(paste0(wd, "core_genome_tree/core_alnD2ESCO_snp_sites.aln.treefile"))

# tree$tip.label contains the 473 

# Get all QC stuff

df1 <- read_tsv(paste0(wd, "D1_Braken_readscreen_species_composition.tsv"))
df2 <- read_tsv(paste0(wd, "D2_Bracken_species_composition.tsv"))
df3 <- read_tsv(paste0(wd, "D2_extra_krakenout_species_composition_16.tsv"))

df.brac <- bind_rows(df1, df2, df3)
df.brac[is.na(df.brac)] <- 0

melt(df.brac, id.vars = "name") -> dftemp

dftemp$variable <- factor(dftemp$variable, levels = unique(dftemp$variable[order(as.character(dftemp$variable))]))

dftemp$name <- factor(dftemp$name, levels = df.brac$name[order(df.brac$`Escherichia coli`, decreasing = T)])

labzzz <- names(df.brac)[!grepl("name", names(df.brac))]
labzzz[order(labzzz)] -> labzzz

paste0('"',labzzz,'"') -> labzzz

sub("other", "Other", labzzz) -> labzzz
sub("unclassified", "Unclassified", labzzz) -> labzzz
ggplot(dftemp, aes(name, value, fill = variable)) + 
  geom_col() + 
  coord_flip() + 
  theme_bw() + 
  theme(axis.text.y = element_blank(), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 8)) + 
  labs(x= "Sample", y = "% reads" ) +
  scale_fill_manual(
    values =
      brewer.pal(12,"Paired"),
    labels = 
      parse(text=c(
        paste('italic(',labzzz[1:7],')'),
        paste('plain(',labzzz[8],')'),
        paste('italic(',labzzz[9:11],')'),
        paste('plain(',labzzz[12],')')
      )
      )
  )

####

# load sanger QC

df1 <- read.csv(paste0(wd, "5341.pathfind_stats.csv"), stringsAsFactors = F)
df2 <- read.csv(paste0(wd, "5510.pathfind_stats.csv"), stringsAsFactors = F)

#df2$`No. Het SNPs`<- as.numeric(df2$`No. Het SNPs`)

df.qc <- rbind(df1, df2)


# laod checkM

con <- file(paste0(wd, "checkm_quast/D1/checkm.report"))
lines <- readLines(con)
close(con)

con <- file(paste0(wd, "checkm_quast/D220190318/checkm.report"))
lines2 <- readLines(con)
close(con)

con <- file(paste0(wd, "checkm_quast/D220190503/checkm.report"))
lines3 <- readLines(con)
close(con)

# line 2 has headings - get em
strsplit(lines[[2]]," ") -> heads
heads[[1]][heads[[1]] != "" & heads[[1]] != "#"] -> heads

# sigh
heads <- c(paste0(heads[1], "_", heads[2]), paste0(heads[3], "_", heads[4]), heads[5:6],
           paste0(heads[7],"_",heads[8]), heads[9:16], paste0(heads[17], "_", heads[18]))

lines <- lines[-c(1,2,3, length(lines))]
lines2 <- lines2[-c(1,2,3, length(lines2))]
lines3 <- lines3[-c(1,2,3, length(lines3))]



lines <- lapply(lines, parse_checkm_lines, heads)
lines2 <- lapply(lines2, parse_checkm_lines, heads)
lines3 <- lapply(lines3, parse_checkm_lines, heads)

lines <- data.frame(do.call(rbind, lines))
lines2 <- data.frame(do.call(rbind, lines2))
lines3 <- data.frame(do.call(rbind, lines3))

lines <- rbind(lines, lines2, lines3)
rm(lines2)
rm(lines3)
lines[3:ncol(lines)] <- sapply(lines[3:ncol(lines)], function(x) as.numeric(as.character(x)))

names(lines)[names(lines) == "Bin_Id"] <- "Assembly"

#sub( "_1_", "_1#", q$Assembly) -> q$Assembly
sub(".contigs_spades", "", lines$Assembly) -> lines$Assembly

checkm <- lines
rm(lines)

## and quast
rbind(
  read.delim(paste0(wd, "checkm_quast/D1/transposed_report.tsv"), stringsAsFactors = F ),
  read.delim(paste0(wd, "checkm_quast/D220190318/transposed_report.tsv"), stringsAsFactors = F ),
  read.delim(paste0(wd, "checkm_quast/D220190503/transposed_report.tsv"), stringsAsFactors = F )
) -> quast

for (i in 1:10) {
  sub("\\.", "", names(quast)) -> names(quast)
}

sub(".contigs_spades","", quast$Assembly) -> quast$Assembly
sub("_1_","_1#", quast$Assembly) -> quast$Assembly
sub("_2_","_2#", quast$Assembly) -> quast$Assembly

df.qc <- merge(df.qc, df.brac, by.x = "Lane.Name", by.y = "name", all.x = T)
df.qc <- merge(df.qc, checkm, by.x = "Lane.Name", by.y = "Assembly", all.x = T)
df.qc <- merge(df.qc, quast, by.x = "Lane.Name", by.y = "Assembly", all.x = T)


##
filter(dftemp, name %in% subset(df.qc, `Escherichia coli` < 20)$Lane.Name ) %>%
ggplot( aes(name, value, fill = variable)) + 
  geom_col() + 
  coord_flip() + 
  theme_bw() + 
  theme(axis.text.y = element_blank(), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 8)) + 
  labs(x= "Sample", y = "% reads" ) +
  scale_fill_manual(
    values =
      brewer.pal(12,"Paired"),
    labels = 
      parse(text=c(
        paste('italic(',labzzz[1:7],')'),
        paste('plain(',labzzz[8],')'),
        paste('italic(',labzzz[9:11],')'),
        paste('plain(',labzzz[12],')')
      )
      )
  )

## drop all of these - re-extract, there are only 12
## drop all the others that failed qc
## so we're left with those included in the original tree
## which are all e coli
# nice n easy



gsub(pattern = "_1_",replacement = "_1#",x = escos_in_tree) -> escos_in_tree
gsub(pattern = "_2_",replacement = "_2#",x = escos_in_tree) -> escos_in_tree

sample_ids <- subset(sample_ids, Lane %in% escos_in_tree) 

sample_ids <- select(sample_ids, Supplier.Name, Lane)
sample_ids$Extracted <- "Yes"
sample_ids$Shipped_to_sanger <- "Yes"
sample_ids$organism <- "Escherichia coli"

names(sample_ids)[1] <- "lab_id"

sample_ids %>% group_by(lab_id) %>% dplyr::slice(1) -> sample_ids

df <- merge(orgs, sample_ids, by = c("lab_id", "organism"), all.x= T)

# next shipped ones

klpn.1 <- read.csv("/Users/joelewis/Documents/Sanger/sequencing manifests/2019_04Feb_third_extraction_run/box_plan_1_klpn.csv", header = F, stringsAsFactors = F)

klpn.2 <- read.csv("/Users/joelewis/Documents/Sanger/sequencing manifests/2019_04Feb_third_extraction_run/box_plan_2_klpn.csv", header = F, stringsAsFactors = F)

klpn.3 <- read.csv("/Users/joelewis/Documents/Sanger/sequencing manifests/2019_04Feb_third_extraction_run/box_plan_3_klpn.csv", header = F, stringsAsFactors = F)

klpn <- rbind(klpn.1, klpn.2, klpn.3)

esco <- read.csv("/Users/joelewis/Documents/Sanger/sequencing manifests/2019_04Feb_third_extraction_run/box_plan_4_esco.csv", header = F, stringsAsFactors = F)


klpn %>% 
  pivot_longer(everything()) %>% 
  mutate(organism = "Klebsiella pneumoniae",
                                        Lane = NA,
                                        Extracted = "Yes",
                                        Shipped_to_sanger = "Yes") %>%
  select(-name) %>%
  rename(value = "lab_id") %>% 
  filter(lab_id != "") %>% 
  group_by(lab_id) %>%
  mutate(n = n()) %>%
  filter(n ==1) %>%
  select(-n) -> klpn
  
-> klpn

esco %>% 
  pivot_longer(everything()) %>% 
  mutate(organism = "Escherichia coli",
         Lane = NA,
         Extracted = "Yes",
         Shipped_to_sanger = "Yes") %>%
  select(-name) %>%
  rename(value = "lab_id") %>% 
  filter(lab_id != "") %>% 
  group_by(lab_id) %>%
  mutate(n = n()) %>%
  filter(n ==1) %>%
  select(-n) -> esco

esco <- subset(esco, !lab_id %in% sample_ids$lab_id)

df <- merge(orgs, 
            rbind(sample_ids,esco, klpn), 
            by = c("lab_id", "organism"), all.x= T)

ggplot(df, aes(organism, fill = Shipped_to_sanger)) + geom_bar() + theme_bw()

df <- subset(df, (organism == "Escherichia coli" | organism == "Klebsiella pneumoniae") & is.na(Shipped_to_sanger))

