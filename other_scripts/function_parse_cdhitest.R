###

# does what it says on the tin


# split into clusters

parse_cd_hit_est_output_to_df <- function(filename) {
  file <- readLines(filename)
  

clusters_posn <- grep('>Cluster', file) 

out_list <- list()
for (i in 1:(length(clusters_posn))) {
  #print("i")
  #print(i)
  
  if (i < length(clusters_posn)) {
    temp_readlines <- file[(clusters_posn[i]+1):(clusters_posn[i+1]-1)]
  } else {
    temp_readlines <- file[(clusters_posn[i]+1):length(file)]
  }
  
  
  temp_list <- list()
  for (j in 1:length(temp_readlines)) {
    #print(j)
    id <- strsplit(temp_readlines[j],"[\\\\]|[^[:print:]]",fixed=FALSE)[[1]][[1]]
    remainder <- strsplit(temp_readlines[j],"[\\\\]|[^[:print:]]",fixed=FALSE)[[1]][[2]]
    length <- strsplit(remainder, ",")[[1]][[1]]
    remainder <- strsplit(remainder, ",")[[1]][[2]]
    sub("nt","", length) -> length
    remainder <- sub(" >.", "", remainder)
    contig  <- strsplit(remainder, "... ")[[1]][[1]]
    remainder  <- strsplit(remainder, "... ")[[1]][[2]]
    if (grepl('/', remainder)) {
      identity <-  strsplit(remainder, "/")[[1]][[2]]
      identity <- sub("%", "", identity)
    } else {
      identity <- "*"
    }
    
    
    temp_list[[j]] <- data.frame('cluster' = (i-1), 'id' = id, 'length' = length,
                                 'contig' = contig, 'identity' = identity, stringsAsFactors = F)
    
    
  }
  temp_list <- do.call(rbind, temp_list)
  out_list[[i]] <- temp_list
}

do.call(rbind, out_list) -> contigs

apply(contigs, 1, function(x) strsplit(x[4], "\\.")[[1]][[1]]) -> contigs$Lane
return(contigs)
}