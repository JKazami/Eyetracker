#rm(list = ls())
require(data.table)
source("analiza_funkcje.R")

# wczytanie zbiorow danych
eks <- wczytajZrenice("dane//", pattern = "_eksp_")
kon <- wczytajZrenice("dane//", pattern = "kontrol_")
kon_j <- wczytajZrenice("dane//", pattern = "kontrol-ja")


zrenice_eks   <- zrenicePolicz(eks,  "eksperyment")
zrenice_kon   <- zrenicePolicz(kon,  "kontrolna"  )
zrenice_kon_j <- zrenicePolicz(kon_j,"kontrolna_j")

wyniki_zrenice <- rbind(zrenice_eks, zrenice_kon, zrenice_kon_j)


