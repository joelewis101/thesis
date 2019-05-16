### new function to choose best match fro a given interval
# using IR ranges


## these all need to be run with dplyr
# parse_blast_op_one_contig  - takes balst output and picks best bitscore match for a given interval
# trim_one_contig - trims around the AMR gene of interest (in $gene) to make the gggene plots look nice\
# flip one contig - flips the contig so the AMR gene of interest is always pointing left to right


parse_blast_op_one_contig <- function(dfsub) {
  require(IRanges)
  require(dplyr)
  # label all non-overlapping ranges
  
  
  ir <- IRanges(dfsub$qstart, dfsub$qend, names = dfsub$sseqid)
  #print("a")
  dfsub$range_group <- subjectHits(findOverlaps(ir, reduce(ir)))
  #print("b")
  ranges.temp <- unique(dfsub$range_group)
  dfout <- list()
  # iterate over and choose hit with best bitscore
  for (i in 1:length(ranges.temp)) {
  #  print(i)
    current_range <- ranges.temp[i]
    current.range.max.bitscore <- dfsub %>%
      dplyr::filter(range_group == current_range) %>%
      filter(bitscore == max(bitscore))
    # what if there are 2?
    #if there is a IS26 or ISEcp1 choose them othewise just pick one 
    
    if (nrow(current.range.max.bitscore) == 1) {
      current.range.max.bitscore$ambig_blast_match <- FALSE
    } else {
      current.range.max.bitscore$ambig_blast_match <- TRUE
      if (TRUE %in% (grepl("ISEcp1", current.range.max.bitscore$sseqid))) {
        current.range.max.bitscore  <- subset(current.range.max.bitscore, grepl("ISEcp1",sseqid)) 
        } else if (TRUE %in% (grepl("IS26", current.range.max.bitscore$sseqid))) {
        current.range.max.bitscore  <- subset(current.range.max.bitscore, grepl("IS26",sseqid )) 
        } else {
          current.range.max.bitscore[1,] -> current.range.max.bitscore 
        }
  }
    dfout[[i]] <- current.range.max.bitscore
  }
  dfout <- do.call(rbind, dfout)
  dfout <- dplyr::select(dfout, -range_group)
 return(dfout) 
}
  


trim_one_contig <- function(dfin, trim_dist) {
  #print(dfin$qseqid[1])
  if (length(unique(dfin$gene)) > 1) {
    stop(paste0("Error in trim_one_contig. More than one AMR gene in $gene for contig :", dfin$qseqid[1]))
  }
  
  if (!(dfin$gene[1] %in% dfin$sseqid)) {
    cat(paste0("WARNING: gene is not in sseqid in contig ", dfin$qseqid[1], ", no trimming done."))
    return(dfin)
  } else {
    amrgenestart <- min(dplyr::filter(dfin, sseqid == dfin$gene[1]) %>% select(qstart, qend))
    amrgeneend <- max(dplyr::filter(dfin, sseqid == dfin$gene[1]) %>% select(qstart, qend))
    #dfin$abs.start <- apply(dfin[7:8], 1, min)
    #dfin$abs.end <- apply(dfin[7:8], 1, max)
    #fin <- susbet(dfin, ((amrgeneend + trim_dist) >= abs.start) | ((abs.end + trim_dist) >= amrgenestart ))
    IRanges(dfin$qstart, dfin$qend, names = dfin$sseqid) -> ir
    ir.amrgene <- IRanges(amrgenestart, amrgeneend)
    dfin$shortest_distance_to_amr_gene <- distance(ir, ir.amrgene)
    dfin <- subset(dfin, shortest_distance_to_amr_gene < trim_dist)
    dfin <-select(dfin, -shortest_distance_to_amr_gene )
    return(dfin)
  }
  
  
}


flip_one_contig <- function(dfin, contig.clusts.df) {
  #print(dfin$qseqid[1])
  if (length(unique(dfin$gene)) > 1) {
    stop(paste0("Error in flip_one_contig. More than one AMR gene in $gene for contig :", dfin$qseqid[1]))
  }
  
  if (!(dfin$gene[1] %in% dfin$sseqid)) {
    cat(paste0("WARNING: gene is not in sseqid in contig ", dfin$qseqid[1], ", no flipping done."))
    return(dfin)
  } else {
    if (subset(dfin, sseqid == dfin$gene[1])$direc  == TRUE) {
      return(dfin)
    } else {
      current_contig_length <- as.numeric(contig.clusts.df$length[contig.clusts.df$contig == dfin$qseqid[1]])
      tempstart <- dfin$qstart 
      dfin$qstart <- current_contig_length - dfin$qend
      dfin$qend <- current_contig_length - tempstart
      
      dfin$direc <- !(dfin$direc)
      return(dfin)
    }
  }
}

