# what fluids did people get

# only fluid 1-4 have owt


# check each in turn - fluid vol should not be able to go down

hourly$fluid_vol1[is.na(hourly$fluid_vol1)] <- 0



hourly %>% filter(pid %in% e1$pid) %>%  dplyr::select(pid, assess_type, fluid_vol1) %>% 
  pivot_wider(id_cols = pid, names_from = assess_type, values_from = fluid_vol1) -> f1

f1 %>% 
  pivot_longer(-pid, names_to = "assess_type") %>% group_by(pid) %>%
  arrange(assess_type) %>% fill(value) %>% ungroup() %>%
  pivot_wider(id_cols = pid, names_from = assess_type, values_from = value) -> f1

#na.locf(f1) -> f1

apply(select(f1, -pid), 1, does_cum_fl_decrease) -> f1$error

does_cum_fl_decrease <- function(inrow) {
  # give this a 
  errorflag <- FALSE
  for (j in length(inrow):2) {
     if (max(inrow[1:j]) == inrow[j]) {
       
     } else  if (which.max(inrow[1:j]) != j) {
      errorflag <- TRUE
    }
  }
  return(errorflag)
} 

f1$`6`[f1$pid == "DAS1014T"] <- 1000
f1$`3`[f1$pid == "DAS1023R"] <-  500
#f1$`5`[f1$pid == "DAS1032P"] <- 1250
#f1$`6`[f1$pid == "DAS1032P"] <- 1450
f1$`4`[f1$pid == "DAS1034L"] <- 500
f1$`5`[f1$pid == "DAS1034L"] <- 500
f1$`6`[f1$pid == "DAS1034L"] <- 500
f1$`4`[f1$pid == "DAS1035J"] <- 775
f1$`3`[f1$pid == "DAS1055B"] <- 100
f1$`4`[f1$pid == "DAS1055B"] <- 100
f1$`5`[f1$pid == "DAS1055B"] <- 100
f1$`6`[f1$pid == "DAS1055B"] <- 100
f1$`5`[f1$pid == "DAS1111R"] <- 700
f1$`6`[f1$pid == "DAS1111R"] <- 700

f1$`3`[f1$pid == "DAS1135B"] <- 1000
f1$`4`[f1$pid == "DAS1135B"] <- 1000
f1$`5`[f1$pid == "DAS1135B"] <- 1000
f1$`6`[f1$pid == "DAS1135B"] <- 1000

#f1$`3`[f1$pid == "DAS1135B"] <- 1250
f1$`5`[f1$pid == "DAS1149X"] <- 1000
f1$`6`[f1$pid == "DAS1149X"] <- 1000

f1$`6`[f1$pid == "DAS1165X"] <- 1500
f1$`6`[f1$pid == "DAS12257"] <- 1500
f1$`5`[f1$pid == "DAS12425"] <- 2500
f1$`6`[f1$pid == "DAS12425"] <- 2500

f1$`3`[f1$pid == "DAS1247W"] <- 500
f1$`5`[f1$pid == "DAS1249S"] <- 2500
f1$`6`[f1$pid == "DAS1249S"] <- 2500

f1$`6`[f1$pid == "DAS1272U"] <- 1850

f1$`5`[f1$pid == "DAS1275O"] <- 1750

f1$`4`[f1$pid == "DAS1296E"] <- 1000
f1$`5`[f1$pid == "DAS1296E"] <- 1000
f1$`6`[f1$pid == "DAS1296E"] <- 1000

f1$`4`[f1$pid == "DAS13225"] <- 575

f1$`3`[f1$pid == "DAS1336U"] <- 600
f1$`6`[f1$pid == "DAS1336U"] <- 1000

f1$`6`[f1$pid == "DAS14009"] <- 1000

f1$`5`[f1$pid == "DAS14017"] <- 2000
f1$`6`[f1$pid == "DAS14017"] <- 2000

f1$`6`[f1$pid == "DAS1474A"] <- 1000

f1$`6`[f1$pid == "DAS1480E"] <- 500

f1$`3`[f1$pid == "DAS1481C"] <- 200
f1$`4`[f1$pid == "DAS1481C"] <- 200
f1$`5`[f1$pid == "DAS1481C"] <- 200
f1$`6`[f1$pid == "DAS1481C"] <- 200

f1$`1`[f1$pid == "DAS14846"] <- 0
f1$`4`[f1$pid == "DAS14870"] <- 2600
f1$`4`[f1$pid == "DAS14950"] <- 100
f1$`5`[f1$pid == "DAS14950"] <- 100
f1$`6`[f1$pid == "DAS14950"] <- 100
f1$`3`[f1$pid == "DAS1519G"] <- 2000
f1$`4`[f1$pid == "DAS1519G"] <- 2000
f1$`3`[f1$pid == "DAS1519G"] <- 2000



apply(select(f1, -pid, -error), 1, does_cum_fl_decrease) -> f1$error
 # woop

hourly %>% filter(pid %in% e1$pid) %>%  dplyr::select(pid, assess_type, fluid_vol2) %>% 
  pivot_wider(id_cols = pid, names_from = assess_type, values_from = fluid_vol2) -> f2

f2$`1`[is.na(f2$`1`)] <- 0

f2 %>% 
  pivot_longer(-pid, names_to = "assess_type") %>% group_by(pid) %>%
  arrange(assess_type) %>% fill(value) %>% ungroup() %>%
  pivot_wider(id_cols = pid, names_from = assess_type, values_from = value) -> f2

apply(select(f2, -pid), 1, does_cum_fl_decrease) -> f2$error

subset(f2, error)

####

hourly %>% filter(pid %in% e1$pid) %>%  dplyr::select(pid, assess_type, fluid_vol3) %>% 
  pivot_wider(id_cols = pid, names_from = assess_type, values_from = fluid_vol3) -> f3

f3$`1`[is.na(f3$`1`)] <- 0

f3 %>% 
  pivot_longer(-pid, names_to = "assess_type") %>% group_by(pid) %>%
  arrange(assess_type) %>% fill(value) %>% ungroup() %>%
  pivot_wider(id_cols = pid, names_from = assess_type, values_from = value) -> f3

apply(select(f3, -pid), 1, does_cum_fl_decrease) -> f3$error

subset(f3,error)
# none

f3$`3`[f1$pid == "DAS1023R"] <- 200
f3$`6`[f1$pid == "DAS1192U"] <- 600
f3$`1`[f1$pid == "DAS13225"] <- 200

apply(select(f3, -pid, -error), 1, does_cum_fl_decrease) -> f3$error
subset(f3,error)

### blood


hourly %>% filter(pid %in% e1$pid) %>%  dplyr::select(pid, assess_type, fluid_vol4) %>% 
  pivot_wider(id_cols = pid, names_from = assess_type, values_from = fluid_vol4) -> f4

f4$`1`[is.na(f4$`1`)] <- 0

f4 %>% 
  pivot_longer(-pid, names_to = "assess_type") %>% group_by(pid) %>%
  arrange(assess_type) %>% fill(value) %>% ungroup() %>%
  pivot_wider(id_cols = pid, names_from = assess_type, values_from = value) -> f4

apply(select(f4, -pid), 1, does_cum_fl_decrease) -> f4$error

subset(f4,error)

## great

# combine

f1$fluid_type <- "Normal saline"
f2$fluid_type <- "Ringer's lactate"
f3$fluid_type <- "5% dextrose"
f4$fluid_type <- "Blood"

bind_rows(f1,f2,f3,f4) %>% select(-error, -fluid_type) %>% pivot_longer(-pid) %>%
  pivot_wider(id_cols = pid, names_from = name, values_from = value, values_fn = list(value = sum)) -> fluid

cat("Done! \n" )
cat("Total fluid of any kind over hour 1-6 now in fluid df  \n")
cat("Others are now in f1-4  \n")
cat("Share and enjoy! \n")

#fluid %>% pivot_longer(-pid) %>% group_by(name) %>% summarise(median = median(value),
  #                                                            lq = quantile(value, 0.25),
   #                                                           uq = quantile(value, 0.75)) -> fluid_sum

#ggplot(fluid %>% pivot_longer(-pid), aes(name, value)) + geom_jitter()

