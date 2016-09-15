wczytajZrenice <- function(fold = "dane//", pattern = "_eksp_"){
  # wczytanie danych + przygotowanie ich przez wyliczenie srednich rozmiarow zrenicy w kazdym z etapow eksperymentu
  wczytajDane(fold, pattern, f_agr = function(x) tmp <- x[,list(l_pupil = mean(PupilLeft, na.rm = T), r_pupil = mean(PupilRight, na.rm = T)), by = list(MediaName)] )
}

wczytajDane <- function(fold = "dane//", pattern = "_eksp_", f_agr = function(x) x){
  # znalezienie odpowiednich plikow
  names <- list.files("dane//", pattern = pattern)
  for(i in names){
    show(i)
    # wczytanie z odpowiednimi ustawieniami
    tmp <- data.table(read.table(paste0("dane//",i), header = T, sep = "\t", fill = T, stringsAsFactors = F, dec = ",", encoding = "utf8", comment.char = ""))
    
    # wywolanie jakiejs funkcji na danych przed sklejeniem - np. czyszczenie albo odfiltrowanie potrzebnych - oszczedza miejsce
    tmp <- f_agr(tmp)
    
    # dodanie info o pliku z ktorego sa dane
    tmp$name <- i
    
    # stworzenie lub dodanie do istniejacego arkusza z wszystkimi danymi
    if(exists("result_dt")){
      result_dt <- rbind(result_dt,tmp)
    } else{
      result_dt <- tmp
    }
  }
  return(result_dt)
}

zrenicePolicz <- function(dt, grupa = "template"){
  
  # unikatowym wpisem jest para <ktory przebieg eksperymentu; ktora czesc eksperymentu>
  dt$key <- paste0(dt$name,"|",dt$MediaName)
  setkey(dt, key)
  
  # okreslamy rozmiar zrenic jako ten bez wyswietlania medium
  dt$l_norm <- dt[paste0(dt$name,"|")]$l_pupil
  dt$r_norm <- dt[paste0(dt$name,"|")]$r_pupil
  
  # okreslamy rozmiar zrenic jako ten podczas survey.html#90 (CRC)
  dt$l_norm_p <- dt[paste0(dt$name,"|http://127.0.0.1:8080/static/survey.html#90 (CRC)")]$l_pupil
  dt$r_norm_p <- dt[paste0(dt$name,"|http://127.0.0.1:8080/static/survey.html#90 (CRC)")]$r_pupil
  # w przypadku braku danych umieszczamy rozmiar przy braku medium
  dt$l_norm_p <- ifelse(is.na(dt$l_norm_p), dt$l_norm, dt$l_norm_p)
  dt$r_norm_p <- ifelse(is.na(dt$r_norm_p), dt$r_norm, dt$r_norm_p)
  
  # okreslamy rozmiar zrenic jako ten podczas survey.html#new?ballot=dtp (CRC)
  dt$l_norm_t <- dt[paste0(dt$name,"|http://127.0.0.1:8080/static/survey.html#new?ballot=dtp (CRC)")]$l_pupil
  dt$r_norm_t <- dt[paste0(dt$name,"|http://127.0.0.1:8080/static/survey.html#new?ballot=dtp (CRC)")]$r_pupil
  # w przypadku braku danych umieszczamy rozmiar przy braku medium
  dt$l_norm_t <- ifelse(is.na(dt$l_norm_t), dt$l_norm, dt$l_norm_t)
  dt$r_norm_t <- ifelse(is.na(dt$r_norm_t), dt$r_norm, dt$r_norm_t)
  
  # okreslamy zmiane w stosunku do rozmiaru w momencie braku medium
  dt$l_zm <- dt$l_pupil / dt$l_norm
  dt$r_zm <- dt$r_pupil / dt$r_norm
  
  # okreslamy zmiane w stosunku do rozmiaru przy pilce
  dt$l_zm_p <- dt$l_pupil / dt$l_norm_p
  dt$r_zm_p <- dt$r_pupil / dt$r_norm_p
  
  # okreslamy zmiane w stosunku do rozmiaru przy czymstam
  dt$l_zm_t <- dt$l_pupil / dt$l_norm_t
  dt$r_zm_t <- dt$r_pupil / dt$r_norm_t
  
  # wyliczamy odpowiednie srednie
  result_dt <- dt[, list(l_zm = mean(l_zm), r_zm = mean(r_zm), l_zm_p = mean(l_zm_p), r_zm_p = mean(r_zm_p), l_zm_t = mean(l_zm_t), r_zm_t = mean(r_zm_t)), by = list(MediaName)]
  
  # dodajemy info o analizowanej grupie
  result_dt$grupa <- grupa
  return(result_dt)
}

oczysc <- function(dt){
  dt <- dt[]
}


v_nast <- function(v){
  if(class(v) == "factor") stop("funkcja nie dziala dobrze na factorach")
  return(c(v[2:length(v)], v[1]))
}

v_pop <- function(v){
  if(class(v) == "factor") stop("funkcja nie dziala dobrze na factorach")
  return(c(v[length(v)], v[1:(length(v)-1)]))
}

okresl_AOI <- function(dt){
  
  # okreœlenie 1 czy jest jakikolwiek AOI
  dt$czy_AOI <- rowSums(data.frame(dt)[,grep("AOI", names(dt))], na.rm = T)
  
  # dodanie AOI - patrzy sie poza AOI
  dt$AOI_nope <- 1 - dt$czy_AOI
  
  
  dt[,czy_AOI := NULL]
  show(paste0("test spojnosci danych - powinno byc 1: ",unique(rowSums(data.frame(dt)[,grep("AOI", names(dt))], na.rm = T))))
  
  
  # zmiana struktury danych - jedna kolumna AOI z nazwa gdzie sie patrzyl
  aojce <- colnames(dt)[substr(colnames(dt),1,3) == "AOI"]
  dt <- melt.data.table(dt, id.vars = colnames(dt)[!colnames(dt)%in%aojce], variable.name = "AOI", variable.factor = F)[GazeEventType == "Fixation" & value == 1]
  dt <- dt[order(RecordingTimestamp)]
  
  # czy wpis jest poczatkiem patrzenia na AOI
  dt$start <- dt$AOI != v_pop(dt$AOI)
  
  # czy wpis jest ostatnim patrzenia na AOI
  dt$koniec <- dt$AOI != v_nast(dt$AOI)
  
  # czy wpis jest poczatkiem/koncem zmiany nazwy medium (czesci eksperymentu)
  dt$media <- dt$MediaName != v_pop(dt$MediaName) | dt$MediaName != v_nast(dt$MediaName) 
  
  # odfiltrowanie tych wierszy, które s¹ na granicach
  dt <- dt[start + koniec + media> 0]
  dt <- dt[order(RecordingTimestamp)]
  
  # poprawka na pierwszy i ostani wpis w tabeli
  dt[1, start := T]
  dt[.N, koniec := T]
  
  
  
  
#  dt$LocalTimeStamp2 <- as.POSIXct(strptime(kon$LocalTimeStamp, "%H:%M:%OS"))
  dt_se <- dt[start + koniec > 0]
  dt_se <- dt_se[order(MediaName, RecordingTimestamp)]
  dt_se$CzasPatrzenia_ms <- dt_se$RecordingTimestamp - v_pop(dt_se$RecordingTimestamp)
#  dt_se$czas_patrzenia2 <- dt_se$LocalTimeStamp2 - v_pop(dt_se$LocalTimeStamp2)
  setkey(dt_se, RecordingTimestamp)
  dt$CzasPatrzenia_ms <- dt_se[dt$RecordingTimestamp]$CzasPatrzenia_ms
  
  return(dt)
}


statystyki_patrzenia <- function(dt, grupa = "template"){
  result_dt <- dt[start == T, .(CzasPatrzenia_ms = sum(CzasPatrzenia_ms)), .(AOI, ParticipantName, MediaName)]
  czas_patrzenia <- result_dt[,.(CzasPatrzenia_ms = sum(CzasPatrzenia_ms)), .(ParticipantName, MediaName)]
  setkeyv(czas_patrzenia, c("ParticipantName", "MediaName"))
  result_dt$CzasPatrzeniaTotal_ms <- czas_patrzenia[J(result_dt$ParticipantName, result_dt$MediaName)]$CzasPatrzenia_ms
  result_dt$grupa <- grupa
  return(result_dt)
}




