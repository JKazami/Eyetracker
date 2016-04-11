rm(list = ls())
library(data.table)

eks_names <- list.files("H://prywatne//ALA//wyniki eytracker styczen 2016 AG//", pattern = "_eksp_")
for(i in eks_names){
  show(i)
  tmp <- data.table(read.table(paste0("H://prywatne//ALA//wyniki eytracker styczen 2016 AG//",i), header = T, sep = "\t", fill = T, stringsAsFactors = F, dec = ",", encoding = "utf8", comment.char = ""))
  #tmp <- tmp[,list(l_pupil = mean(PupilLeft, na.rm = T), r_pupil = mean(PupilRight, na.rm = T)), by = list(MediaName)]
  tmp$name <- i
  if(exists("eks")){
    eks <- rbind(eks,tmp)
  } else{
    eks <- tmp
  }
}


eks$gapi<-rowSums(data.frame(eks)[,47:82],na.rm=TRUE)

DASdbase::connect()



kon_names <- list.files("H://prywatne//ALA//wyniki eytracker styczen 2016 AG//", pattern = "kontrol_")
for(i in kon_names){
  show(i)
  tmp <- data.table(read.table(paste0("H://prywatne//ALA//wyniki eytracker styczen 2016 AG//",i), header = T, sep = "\t", fill = T, stringsAsFactors = F, dec = ",", encoding = "utf8", comment.char = ""))
  tmp <- tmp[,list(l_pupil = mean(PupilLeft, na.rm = T), r_pupil = mean(PupilRight, na.rm = T)), by = list(MediaName)]
  tmp$name <- i                  
  if(exists("kon")){
    kon <- rbind(kon,tmp)
  } else{
    kon <- tmp
  }
}



kon_j_names <- list.files("H://prywatne//ALA//wyniki eytracker styczen 2016 AG//", pattern = "kontrol-ja")
for(i in kon_j_names){
  show(i)
  tmp <- data.table(read.table(paste0("H://prywatne//ALA//wyniki eytracker styczen 2016 AG//",i), header = T, sep = "\t", fill = T, stringsAsFactors = F, dec = ",", encoding = "utf8", comment.char = ""))
  tmp <- tmp[,list(l_pupil = mean(PupilLeft, na.rm = T), r_pupil = mean(PupilRight, na.rm = T)), by = list(MediaName)]
  tmp$name <- i
  if(exists("kon_j")){
    kon_j <- rbind(kon_j,tmp)
  } else{
    kon_j <- tmp
  }
}

rm(tmp)
rm(eks_names)
rm(kon_names)
rm(kon_j_names)


eks$key <- paste0(eks$name,"|",eks$MediaName)
setkey(eks, key)
eks$l_norm <- eks[paste0(eks$name,"|")]$l_pupil
eks$r_norm <- eks[paste0(eks$name,"|")]$r_pupil
eks$l_norm_p <- eks[paste0(eks$name,"|http://127.0.0.1:8080/static/survey.html#90 (CRC)")]$l_pupil
eks$r_norm_p <- eks[paste0(eks$name,"|http://127.0.0.1:8080/static/survey.html#90 (CRC)")]$r_pupil
eks$l_norm_p <- ifelse(is.na(eks$l_norm_p), eks$l_norm, eks$l_norm_p)
eks$r_norm_p <- ifelse(is.na(eks$r_norm_p), eks$r_norm, eks$r_norm_p)
eks$l_norm_t <- eks[paste0(eks$name,"|http://127.0.0.1:8080/static/survey.html#new?ballot=eksp (CRC)")]$l_pupil
eks$r_norm_t <- eks[paste0(eks$name,"|http://127.0.0.1:8080/static/survey.html#new?ballot=eksp (CRC)")]$r_pupil
eks$l_norm_t <- ifelse(is.na(eks$l_norm_t), eks$l_norm, eks$l_norm_t)
eks$r_norm_t <- ifelse(is.na(eks$r_norm_t), eks$r_norm, eks$r_norm_t)

eks$l_zm <- eks$l_pupil / eks$l_norm
eks$r_zm <- eks$r_pupil / eks$r_norm
eks$l_zm_p <- eks$l_pupil / eks$l_norm_p
eks$r_zm_p <- eks$r_pupil / eks$r_norm_p
eks$l_zm_t <- eks$l_pupil / eks$l_norm_t
eks$r_zm_t <- eks$r_pupil / eks$r_norm_t

wyniki_eks <- eks[, list(l_zm = mean(l_zm), r_zm = mean(r_zm), l_zm_p = mean(l_zm_p), r_zm_p = mean(r_zm_p), l_zm_t = mean(l_zm_t), r_zm_t = mean(r_zm_t)), by = list(MediaName)]


kon$key <- paste0(kon$name,"|",kon$MediaName)
setkey(kon, key)
kon$l_norm <- kon[paste0(kon$name,"|")]$l_pupil
kon$r_norm <- kon[paste0(kon$name,"|")]$r_pupil
kon$l_norm_p <- kon[paste0(kon$name,"|http://127.0.0.1:8080/static/survey.html#90 (CRC)")]$l_pupil
kon$r_norm_p <- kon[paste0(kon$name,"|http://127.0.0.1:8080/static/survey.html#90 (CRC)")]$r_pupil
kon$l_norm_p <- ifelse(is.na(kon$l_norm_p), kon$l_norm, kon$l_norm_p)
kon$r_norm_p <- ifelse(is.na(kon$r_norm_p), kon$r_norm, kon$r_norm_p)
kon$l_norm_t <- kon[paste0(kon$name,"|http://127.0.0.1:8080/static/survey.html#new?ballot=kontrol (CRC)")]$l_pupil
kon$r_norm_t <- kon[paste0(kon$name,"|http://127.0.0.1:8080/static/survey.html#new?ballot=kontrol (CRC)")]$r_pupil
kon$l_norm_t <- ifelse(is.na(kon$l_norm_t), kon$l_norm, kon$l_norm_t)
kon$r_norm_t <- ifelse(is.na(kon$r_norm_t), kon$r_norm, kon$r_norm_t)

kon$l_zm <- kon$l_pupil / kon$l_norm
kon$r_zm <- kon$r_pupil / kon$r_norm
kon$l_zm_p <- kon$l_pupil / kon$l_norm_p
kon$r_zm_p <- kon$r_pupil / kon$r_norm_p
kon$l_zm_t <- kon$l_pupil / kon$l_norm_t
kon$r_zm_t <- kon$r_pupil / kon$r_norm_t


wyniki_kon <- kon[, list(l_zm = mean(l_zm), r_zm = mean(r_zm), l_zm_p = mean(l_zm_p), r_zm_p = mean(r_zm_p), l_zm_t = mean(l_zm_t), r_zm_t = mean(r_zm_t)), by = list(MediaName)]





kon_j$key <- paste0(kon_j$name,"|",kon_j$MediaName)
setkey(kon_j, key)
kon_j$l_norm <- kon_j[paste0(kon_j$name,"|")]$l_pupil
kon_j$r_norm <- kon_j[paste0(kon_j$name,"|")]$r_pupil
kon_j$l_norm_p <- kon_j[paste0(kon_j$name,"|http://127.0.0.1:8080/static/survey.html#90 (CRC)")]$l_pupil
kon_j$r_norm_p <- kon_j[paste0(kon_j$name,"|http://127.0.0.1:8080/static/survey.html#90 (CRC)")]$r_pupil
kon_j$l_norm_p <- ifelse(is.na(kon_j$l_norm_p), kon_j$l_norm, kon_j$l_norm_p)
kon_j$r_norm_p <- ifelse(is.na(kon_j$r_norm_p), kon_j$r_norm, kon_j$r_norm_p)
kon_j$l_norm_t <- kon_j[paste0(kon_j$name,"|http://127.0.0.1:8080/static/survey.html#new?ballot=kontrol-ja (CRC)")]$l_pupil
kon_j$r_norm_t <- kon_j[paste0(kon_j$name,"|http://127.0.0.1:8080/static/survey.html#new?ballot=kontrol-ja (CRC)")]$r_pupil
kon_j$l_norm_t <- ifelse(is.na(kon_j$l_norm_t), kon_j$l_norm, kon_j$l_norm_t)
kon_j$r_norm_t <- ifelse(is.na(kon_j$r_norm_t), kon_j$r_norm, kon_j$r_norm_t)

kon_j$l_zm <- kon_j$l_pupil / kon_j$l_norm
kon_j$r_zm <- kon_j$r_pupil / kon_j$r_norm
kon_j$l_zm_p <- kon_j$l_pupil / kon_j$l_norm_p
kon_j$r_zm_p <- kon_j$r_pupil / kon_j$r_norm_p
kon_j$l_zm_t <- kon_j$l_pupil / kon_j$l_norm_t
kon_j$r_zm_t <- kon_j$r_pupil / kon_j$r_norm_t

wyniki_kon_j <- kon_j[, list(l_zm = mean(l_zm), r_zm = mean(r_zm), l_zm_p = mean(l_zm_p), r_zm_p = mean(r_zm_p), l_zm_t = mean(l_zm_t), r_zm_t = mean(r_zm_t)), by = list(MediaName)]


wyniki_eks$grupa <- "eksperyment"
wyniki_kon$grupa <- "kontrolna"
wyniki_kon_j$grupa <- "kontrolna_j"

wyniki <- rbind(wyniki_eks, wyniki_kon, wyniki_kon_j)



df <- as.data.frame(eks)





