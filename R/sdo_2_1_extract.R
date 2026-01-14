
# SKRIPT PRO HROMADNOU EXTRAKCI DAT Z SDO (PDF)


library(pdftools)
library(stringr)
library(dplyr)
library(tidyr)
library(readr) # Pro ukládání CSV

# === NASTAVENÍ CEST ===
input_dir <- "Input/Data_test"
output_dir <- "Outputs/Data"

# Vytvoření výstupní složky, pokud neexistuje
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# 1. POMOCNÉ FUNKCE (JÁDRO EXTRAKCE)


# --- Funkce pro extrakci Stanovišť ---
extract_stanoviste <- function(text, sitecode) {
  # Rozdělení na bloky. (?=...) je lookahead, aby zůstal oddělovač zachován
  blocks <- str_split(text, "(?=\\n\\s*Kód předmětu ochrany:)")[[1]]
  blocks <- blocks[grepl("Kód předmětu ochrany:", blocks)]
  
  stanoviste_list <- list()
  
  for (block in blocks) {
    # Extrakce pomocí str_match (bezpečnější než look-behind)
    kod <- str_match(block, "Kód předmětu ochrany:\\s*(\\d+\\*?)")[, 2]
    nazev <- str_match(block, "Název předmětu ochrany:\\s*([^\\n]+)")[, 2]
    
    # Rozlohy (nahrazení desetinné čárky tečkou)
    rozloha_str <- str_match(block, "Rozloha \\(ha\\):\\s*([0-9,\\.]+)")[, 2]
    rozloha <- as.numeric(gsub(",", ".", rozloha_str))
    
    rel_rozloha_str <- str_match(block, "Relativní rozloha \\(%\\):\\s*([0-9,\\.]+)")[, 2]
    rel_rozloha <- as.numeric(gsub(",", ".", rel_rozloha_str))
    
    # Cílový stav (dotall mode (?s))
    cilovy_stav <- str_match(block, "(?s)Cílový stav předmětu ochrany:\\s*(.+?)(?=\\n\\s*Kód předmětu|$)")[, 2]
    
    stanoviste_list[[length(stanoviste_list) + 1]] <- data.frame(
      sitecode = sitecode,
      nazev_predmetu = str_trim(nazev),
      feature_code = kod,
      rozloha_ha = rozloha,
      rozloha_procent = rel_rozloha,
      stav_text = str_trim(cilovy_stav),
      source_file = NA, # Doplní se později
      stringsAsFactors = FALSE
    )
  }
  
  if(length(stanoviste_list) > 0) bind_rows(stanoviste_list) else NULL
}

# --- Funkce pro extrakci Druhů (ROBUSTNÍ VERZE S KLÍČOVÝMI SLOVY) ---
extract_druhy_robust <- function(text, sitecode) {
  
  blocks <- str_split(text, "(?=\\n\\s*Název předmětu ochrany:)")[[1]]
  blocks <- blocks[grepl("Název předmětu ochrany:", blocks)]
  
  druhy_list <- list()
  
  for (block in blocks) {
    
    # 1. Základní identifikace
    nazev_full <- str_match(block, "Název předmětu ochrany:\\s*([^\\n]+)")[, 2]
    nazev_full <- str_trim(nazev_full)
    
    # Rozdělení latinský/český název
    parts <- unlist(str_split(nazev_full, "\\s+"))
    if (length(parts) >= 2) {
      nazev_lat <- paste(tail(parts, 2), collapse = " ")
      nazev_cz <- paste(head(parts, -2), collapse = " ")
    } else {
      nazev_lat <- ""
      nazev_cz <- nazev_full
    }
    
    kod <- str_match(block, "Kód předmětu ochrany:\\s*(\\d+)")[, 2]
    
    # 2. Populační data
    pop_line <- str_extract(block, "(stálá|rozmnožující se)[^\\n]+")
    pop_typ <- NA; pop_min <- NA; pop_max <- NA; pop_jednotka <- NA
    
    if (!is.na(pop_line)) {
      pop_typ <- ifelse(grepl("stálá", pop_line), "stálá", "rozmnožující se")
      cols <- unlist(str_split(pop_line, "\\s{2,}"))
      if(length(cols) >= 2) pop_min <- cols[2]
      if(length(cols) >= 3) pop_max <- cols[3]
      if(length(cols) >= 4) pop_jednotka <- cols[4]
    }
    
    # 3. ROBUSTNÍ EXTRAKCE ATRIBUTŮ (Zachovalost, Izolace, Hodnocení)
    # Převedeme na malá písmena a odstraníme nové řádky pro snazší hledání
    clean_txt <- str_to_lower(str_replace_all(block, "\\s+", " "))
    
    zach <- NA
    izol <- NA
    celk <- NA
    
    # A) IZOLACE
    if (str_detect(clean_txt, "není izolovaná")) {
      izol <- "populace není izolovaná"
    } else if (str_detect(clean_txt, "je izolovaná")) {
      izol <- "populace je izolovaná"
    } else if (str_detect(clean_txt, "okraj")) {
      izol <- "populace na okraji areálu"
    } else if (str_detect(clean_txt, "populace není izolovaná, leží uvnitř areálu rozšíření druhu")) {
      izol <- "populace není izolovaná, leží uvnitř areálu rozšíření druhu"
    }
    
    # B) ZACHOVALOST (hledáme "dobré" - střední rod)
    if (str_detect(clean_txt, "dobré")) {
      zach <- "dobré zachování"
    } else if (str_detect(clean_txt, "průměrné")) {
      zach <- "průměrné nebo omezené zachování"
    }
    
    # C) CELKOVÉ HODNOCENÍ (hledáme "dobrá" - ženský rod)
    if (str_detect(clean_txt, "dobrá")) {
      celk <- "dobrá hodnota"
    } else if (str_detect(clean_txt, "významná")) {
      celk <- "významná hodnota"
    } else if (str_detect(clean_txt, "omezená")) {
      celk <- "omezená hodnota"
    }
    
    # D) Řešení "Vynikající" (může být u obou)
    count_vynikajici <- str_count(clean_txt, "vynikající")
    
    if (count_vynikajici > 0) {
      if (is.na(zach)) {
        zach <- "vynikající zachování"
        count_vynikajici <- count_vynikajici - 1
      }
      if (is.na(celk) && count_vynikajici > 0) {
        celk <- "vynikající hodnota"
      }
    }
    
    # 4. Cílový stav
    cilovy_stav <- str_match(block, "(?s)Cílový stav předmětu ochrany:\\s*(.+?)(?=\\n\\s*Název předmětu|$)")[, 2]
    
    druhy_list[[length(druhy_list) + 1]] <- data.frame(
      sitecode = sitecode,
      nazev_cz = nazev_cz,
      nazev_lat = nazev_lat,
      feature_code = kod,
      pop_typ = pop_typ,
      pop_min = pop_min,
      pop_max = pop_max,
      pop_jednotka = pop_jednotka,
      zachovalost = zach,
      izolace = izol,
      celkove = celk,
      stav_text = str_trim(cilovy_stav),
      source_file = NA, # Doplní se později
      stringsAsFactors = FALSE
    )
  }
  
  if(length(druhy_list) > 0) bind_rows(druhy_list) else NULL
}

# --- Hlavní funkce pro zpracování jednoho souboru ---
process_pdf_file <- function(pdf_path) {
  
  tryCatch({
    # Načtení textu
    text <- pdf_text(pdf_path)
    full_text <- paste(text, collapse = "\n")
    
    # Sitecode
    sitecode <- str_extract(full_text, "Kód lokality:\\s*([A-Z0-9]+)")
    if(!is.na(sitecode)) sitecode <- str_trim(str_remove(sitecode, "Kód lokality:\\s*"))
    
    # Hledání kapitol 2.1 a 2.2
    chapter_2_1_start <- str_locate(full_text, "2\\.1\\s+Předměty ochrany")[1]
    chapter_2_2_start <- str_locate(full_text, "2\\.2\\s+Nároky")[1]
    
    if (is.na(chapter_2_1_start) || is.na(chapter_2_2_start)) {
      warning(paste("Kapitoly nenalezeny v souboru:", basename(pdf_path)))
      return(list(stanoviste = NULL, druhy = NULL))
    }
    
    chapter_text <- substr(full_text, chapter_2_1_start, chapter_2_2_start - 1)
    
    # Rozdělení na Stanoviště a Druhy
    stanoviste_pos <- str_locate(chapter_text, "(2\\.1\\.1\\s+)?Stanoviště")[1]
    druhy_pos <- str_locate(chapter_text, "\\n(2\\.1\\.2\\s+)?Druhy")[1]
    
    res_stanoviste <- NULL
    res_druhy <- NULL
    
    # Extrakce Stanovišť
    if (!is.na(stanoviste_pos) && !is.na(druhy_pos)) {
      stan_text <- substr(chapter_text, stanoviste_pos, druhy_pos - 1)
      res_stanoviste <- extract_stanoviste(stan_text, sitecode)
    }
    
    # Extrakce Druhů
    if (!is.na(druhy_pos)) {
      dr_text <- substr(chapter_text, druhy_pos, nchar(chapter_text))
      res_druhy <- extract_druhy_robust(dr_text, sitecode)
    }
    
    return(list(stanoviste = res_stanoviste, druhy = res_druhy))
    
  }, error = function(e) {
    warning(paste("Chyba při zpracování souboru", basename(pdf_path), ":", e$message))
    return(list(stanoviste = NULL, druhy = NULL))
  })
}


# 2. HLAVNÍ SMYČKA (BATCH PROCESSING)


# Získání seznamu PDF souborů
files <- list.files(input_dir, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)

if (length(files) == 0) {
  stop("Ve složce Input/Data_test nebyly nalezeny žádné PDF soubory.")
}

print(paste("Nalezeno", length(files), "souborů. Začínám zpracování..."))

all_stanoviste <- list()
all_druhy <- list()

# Iterace přes soubory
for (i in seq_along(files)) {
  f <- files[i]
  fname <- basename(f)
  print(paste0("[", i, "/", length(files), "] Zpracovávám: ", fname))
  
  data <- process_pdf_file(f)
  
  # Uložení výsledků, pokud nějaké jsou
  if (!is.null(data$stanoviste)) {
    data$stanoviste$source_file <- fname
    all_stanoviste[[length(all_stanoviste) + 1]] <- data$stanoviste
  }
  
  if (!is.null(data$druhy)) {
    data$druhy$source_file <- fname
    all_druhy[[length(all_druhy) + 1]] <- data$druhy
  }
}


# 3. EXPORT DAT (Windows-1250)

print("Spojování dat a export...")

# Spojení seznamů do jednoho dataframe
final_stanoviste <- bind_rows(all_stanoviste)
final_druhy <- bind_rows(all_druhy)

# Uložení Stanovišť
if (nrow(final_stanoviste) > 0) {
  out_path_s <- file.path(output_dir, "souhrn_stanoviste.csv")
  
  # Používáme write.csv z base R, protože umí fileEncoding.
  # row.names = FALSE, aby se nevytvořil sloupec s čísly řádků.
  write.csv(final_stanoviste, out_path_s, row.names = FALSE, fileEncoding = "Windows-1250")
  
  print(paste("Uloženo stanovišť:", nrow(final_stanoviste), "->", out_path_s))
} else {
  print("Nebyla nalezena žádná data pro stanoviště.")
}

# Uložení Druhů
if (nrow(final_druhy) > 0) {
  out_path_d <- file.path(output_dir, "souhrn_druhy.csv")
  
  write.csv(final_druhy, out_path_d, row.names = FALSE, fileEncoding = "Windows-1250")
  
  print(paste("Uloženo druhů:", nrow(final_druhy), "->", out_path_d))
} else {
  print("Nebyla nalezena žádná data pro druhy.")
}

print("=== HOTOVO ===")