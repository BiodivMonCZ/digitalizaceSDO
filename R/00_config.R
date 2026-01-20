# SKRIPT PRO HROMADNOU EXTRAKCI DAT Z SDO (PDF)

library(pdftools)
library(stringr)
library(dplyr)
library(tidyr)
library(readr) # Pro ukládání CSV

# === NASTAVENÍ CEST ===
input_dir <- "Input/Data_test"
temp_dir <- "Input/Temp"
output_dir <- "Outputs/Data"

# Vytvoření výstupní složky, pokud neexistuje
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir, recursive = TRUE)
}
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


