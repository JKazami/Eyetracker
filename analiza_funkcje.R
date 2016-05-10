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

zrenicePolicz <- function(dt, grupa = "eksperyment"){
  
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
