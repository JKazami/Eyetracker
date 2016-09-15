#rm(list = ls())
require(data.table)
source("analiza_funkcje.R")

# wczytanie zbiorow danych
eks <- wczytajDane("dane//", pattern = "_eksp_", f_agr = okresl_AOI)
kon <- wczytajDane("dane//", pattern = "kontrol_", f_agr = okresl_AOI)
kon_j <- wczytajDane("dane//", pattern = "kontrol-ja", f_agr = okresl_AOI)



czasy_eks   <- statystyki_patrzenia(eks,  "eksperyment")
czasy_kon   <- statystyki_patrzenia(kon,  "kontrolna"  )
czasy_kon_j <- statystyki_patrzenia(kon_j,"kontrolna_j")

wyniki_czasy <- rbind(czasy_eks, czasy_kon, czasy_kon_j)

write.csv2(wyniki_czasy, "wyniki//analiza_czasow")
