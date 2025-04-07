#Skript zum Bereinigen der Bevölkerungsdaten der Kreise (2023 auf Basis Zensus 2022)
library(needs)
needs(tidyverse, readxl)

# 1. Kreise ---------------------------------------------------------------
bevoelkerung_kreise <- read_excel("input/bevoelkerung_2023_kreise.xlsx", skip = 3)
bevoelkerung_kreise <- bevoelkerung_kreise[, c(2, 4, 5, 9)]

# Zeilen 1 und 2 löschen
bevoelkerung_kreise <- bevoelkerung_kreise[-c(1, 2), ]

# Spalten umbenennen
colnames(bevoelkerung_kreise) <- c("schluessel", "kreisart", "bevoelkerung", "bevoelkerungsdichte")

bevoelkerung_kreise$schluessel <- ifelse(
  startsWith(bevoelkerung_kreise$schluessel, "0"),
  substring(bevoelkerung_kreise$schluessel, 2),
  bevoelkerung_kreise$schluessel
)

bevoelkerung_kreise$kreisart <- dplyr::recode(
  bevoelkerung_kreise$kreisart,
  "K"   = "Landkreis",
  "LK"  = "Landkreis",
  "RV"  = "Landkreis",
  "KfS" = "Kreisfreie Stadt",
  "SK"  = "Kreisfreie Stadt"
)

bevoelkerung_kreise$schluessel <- as.numeric(bevoelkerung_kreise$schluessel)
