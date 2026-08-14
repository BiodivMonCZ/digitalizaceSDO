library(stringr)
library(dplyr)
library(pdftools)

# --- 1. ROBUSTNÍ ČIŠTĚNÍ TEXTU (Zabrání rozpadu CSV) ---
clean_text <- function(x) {
  if (is.na(x)) return(NA)
  # Odstraní všechna řídící znaky (enter, tab, form feed) a nahradí je mezerou
  x <- str_replace_all(x, "[[:cntrl:]]+", " ")
  # Sjednotí vícenásobné mezery
  x <- str_replace_all(x, "\\s+", " ")
  return(str_trim(x))
}

# --- 2. EXTRAKCE ČÍSEL (Pro populace) ---
extract_numbers_from_text <- function(text) {
  # Najde všechna čísla (včetně desetinných)
  # Hledáme vzory jako "10", "10,5", "100-200"
  raw_nums <- str_extract_all(text, "[0-9]+([.,][0-9]+)?")[[1]]
  if (length(raw_nums) == 0) return(NULL)
  # Převedeme na čísla (čárky na tečky)
  nums <- as.numeric(gsub(",", ".", raw_nums))
  return(nums)
}

# --- 3. JÁDRO: EXTRAKCE DRUHŮ ---
extract_druhy_robust <- function(text, sitecode) {
  
  # Rozdělení na bloky (lookahead)
  blocks <- str_split(text, "(?=\\n\\s*Název předmětu ochrany:)")[[1]]
  blocks <- blocks[grepl("Název předmětu ochrany:", blocks)]
  
  druhy_list <- list()
  
  for (block in blocks) {
    
    # A) NÁZVY (Inteligentní dělení)
    nazev_full <- str_match(block, "Název předmětu ochrany:\\s*([^\\n]+)")[, 2]
    nazev_full <- clean_text(nazev_full)
    
    # Regex: Hledá latinu na konci (Slovo s velkým písmenem následované malými)
    match_lat <- str_match(nazev_full, "^(.*)\\s+([A-Z][a-z]+(?:\\s+[a-z]+)+)$")
    
    if (!is.na(match_lat[1, 1])) {
      nazev_cz <- str_trim(match_lat[1, 2])
      nazev_lat <- str_trim(match_lat[1, 3])
    } else {
      # Fallback: prosté rozdělení na poslední 2 slova
      parts <- unlist(str_split(nazev_full, "\\s+"))
      if (length(parts) >= 2) {
        nazev_lat <- paste(tail(parts, 2), collapse = " ")
        nazev_cz <- paste(head(parts, -2), collapse = " ")
      } else {
        nazev_lat <- ""
        nazev_cz <- nazev_full
      }
    }
    
    kod <- str_match(block, "Kód předmětu ochrany:\\s*(\\d+)")[, 2]
    
    # B) POPULACE (Hledáme v širším kontextu 250 znaků, abychom chytili tabulku
    # Min/Max/Jednotka, která se v PDF často zalamuje na další řádek)
    # (?s) zapíná "dotall" - tečka bere i nový řádek
    pop_chunk_match <- str_match(block, "(?s)(stálá|rozmnožující se|zimující)(.{0,250})")

    pop_typ <- NA; pop_min <- NA; pop_max <- NA; pop_jednotka <- NA

    if (!is.na(pop_chunk_match[1,1])) {
      pop_typ_raw <- pop_chunk_match[1,2]
      pop_context <- pop_chunk_match[1,3] # Text za typem populace

      # Typ
      if (grepl("stálá", pop_typ_raw)) pop_typ <- "stálá"
      else if (grepl("zimující", pop_typ_raw)) pop_typ <- "zimující"
      else pop_typ <- "rozmnožující se"

      # Min/Max/Jednotka: v tabulce SDO jde o trojici sloupců přímo za sebou
      # (např. "20 130 jedinci" nebo, pokud stav nebyl vyhodnocen, "-  -  -").
      # Nelze prostě vzít první dvě čísla v okolním textu - ta často patří do
      # sloupců "Podíl populace" nebo do prahu kategorie ("p > X %"), takže by
      # se např. z "-  -  -  běžný  2 %  p > 0 %" chybně vyčetlo min = 2, max = 0.
      minmax_match <- str_match(
        pop_context,
        "(-|[0-9]+(?:[.,][0-9]+)?)\\s+(-|[0-9]+(?:[.,][0-9]+)?)\\s+(-|[\\p{L}][\\p{L}.]*)"
      )

      if (!is.na(minmax_match[1, 1])) {
        min_raw <- minmax_match[1, 2]
        max_raw <- minmax_match[1, 3]
        jednotka_raw <- minmax_match[1, 4]

        pop_min <- if (min_raw == "-") NA_real_ else as.numeric(gsub(",", ".", min_raw))
        pop_max <- if (max_raw == "-") NA_real_ else as.numeric(gsub(",", ".", max_raw))
        if (jednotka_raw != "-") pop_jednotka <- jednotka_raw
      }

      # Fallback pro jednotku, pokud ji trojice Min/Max/Jednotka nezachytila
      # (např. víceslovná jednotka nebo neobvyklé zalomení tabulky)
      if (is.na(pop_jednotka)) {
        units <- str_extract(pop_context, "(jedinců|jedinci|párů|páry|ex\\.|trsy|trsů|kvadrat|mikropopulace)")
        if (!is.na(units)) pop_jednotka <- units
      }
    }
    
    # C) ATRIBUTY (Zachovalost, Izolace, Hodnocení)
    clean_txt_block <- str_to_lower(str_replace_all(block, "\\s+", " "))
    
    zach <- NA; izol <- NA; celk <- NA
    
    # Izolace
    if (str_detect(clean_txt_block, "není izolovaná")) izol <- "populace není izolovaná"
    else if (str_detect(clean_txt_block, "populace je")) izol <- "populace je izolovaná"
    else if (str_detect(clean_txt_block, "okraj")) izol <- "populace na okraji areálu"
    
    # Zachovalost
    if (str_detect(clean_txt_block, "dobré")) zach <- "dobré zachování"
    else if (str_detect(clean_txt_block, "průměrné")) zach <- "průměrné nebo omezené zachování"
    
    # Celkové
    if (str_detect(clean_txt_block, "dobrá")) celk <- "dobrá hodnota"
    else if (str_detect(clean_txt_block, "významná")) celk <- "významná hodnota"
    else if (str_detect(clean_txt_block, "omezená")) celk <- "omezená hodnota"
    
    # "Vynikající" override
    if (str_detect(clean_txt_block, "vynikající")) {
      if(is.na(zach)) zach <- "vynikající zachování"
      if(is.na(celk)) celk <- "vynikající hodnota"
    }
    
    # D) CÍLOVÝ STAV
    cilovy_stav <- str_match(block, "(?s)Cílový stav předmětu ochrany:\\s*(.+?)(?=\\n\\s*Název předmětu|$)")[, 2]
    
    druhy_list[[length(druhy_list) + 1]] <- data.frame(
      sitecode = clean_text(sitecode),
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
      stav_text = clean_text(cilovy_stav), # Kritické vyčištění!
      source_file = NA, 
      stringsAsFactors = FALSE
    )
  }
  
  if(length(druhy_list) > 0) bind_rows(druhy_list) else NULL
}

# --- 4. ZBYTEK SKRIPTU (Process file, Batch loop) ---
# ... (Zde použijte zbytek vašeho skriptu pro Stanoviště a Smyčku přes soubory beze změny,
# ... ale nezapomeňte aktualizovat volání clean_text i v extract_stanoviste pokud ji používáte)

# Pro jistotu verze extract_stanoviste s clean_text:
extract_stanoviste <- function(text, sitecode) {
  blocks <- str_split(text, "(?=\\n\\s*Kód předmětu ochrany:)")[[1]]
  blocks <- blocks[grepl("Kód předmětu ochrany:", blocks)]
  
  stanoviste_list <- list()
  
  for (block in blocks) {
    kod <- str_match(block, "Kód předmětu ochrany:\\s*(\\d+\\*?)")[, 2]
    nazev <- str_match(block, "Název předmětu ochrany:\\s*([^\\n]+)")[, 2]
    
    rozloha_str <- str_match(block, "Rozloha \\(ha\\):\\s*([0-9,\\.]+)")[, 2]
    rozloha <- as.numeric(gsub(",", ".", rozloha_str))
    
    cilovy_stav <- str_match(block, "(?s)Cílový stav předmětu ochrany:\\s*(.+?)(?=\\n\\s*Kód předmětu|$)")[, 2]
    
    stanoviste_list[[length(stanoviste_list) + 1]] <- data.frame(
      sitecode = clean_text(sitecode),
      nazev_predmetu = clean_text(nazev),
      feature_code = kod,
      rozloha_ha = rozloha,
      stav_text = clean_text(cilovy_stav),
      source_file = NA,
      stringsAsFactors = FALSE
    )
  }
  if(length(stanoviste_list) > 0) bind_rows(stanoviste_list) else NULL
}

# --- HLAVNÍ SMYČKA ---
# (Zde vložte váš původní kód smyčky files, process_pdf_file a exportu)
# (Funkce process_pdf_file se nezměnila, jen volá aktualizované extract funkce)

# --- Hlavni funkce pro zpracovani jednoho souboru ---
process_pdf_file <- function(pdf_path) {
  
  tryCatch({
    # Nacteni textu
    text <- pdf_text(pdf_path)
    full_text <- paste(text, collapse = "\n")
    
    # Sitecode
    sitecode <- str_extract(full_text, "Kód lokality:\\s*([A-Z0-9]+)")
    if(!is.na(sitecode)) sitecode <- str_trim(str_remove(sitecode, "Kód lokality:\\s*"))
    
    # Hledani kapitol 2.1 a 2.2
    chapter_2_1_start <- str_locate(full_text, "2\\.1\\s+Předměty ochrany")[1]
    chapter_2_2_start <- str_locate(full_text, "2\\.2\\s+Nároky")[1]
    
    if (is.na(chapter_2_1_start) || is.na(chapter_2_2_start)) {
      warning(paste("Kapitoly nenalezeny v souboru:", basename(pdf_path)))
      return(list(stanoviste = NULL, druhy = NULL))
    }
    
    chapter_text <- substr(full_text, chapter_2_1_start, chapter_2_2_start - 1)
    
    # Rozdeleni na Stanoviste a Druhy
    stanoviste_pos <- str_locate(chapter_text, "(2\\.1\\.1\\s+)?Stanoviště")[1]
    druhy_pos <- str_locate(chapter_text, "\\n(2\\.1\\.2\\s+)?Druhy")[1]
    
    res_stanoviste <- NULL
    res_druhy <- NULL
    
    # Extrakce Stanovist
    if (!is.na(stanoviste_pos) && !is.na(druhy_pos)) {
      stan_text <- substr(chapter_text, stanoviste_pos, druhy_pos - 1)
      res_stanoviste <- extract_stanoviste(stan_text, sitecode)
    }
    
    # Extrakce Druhu
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


# 2. HLAVNI SMYCKA (BATCH PROCESSING)


# Ziskani seznamu PDF souboru
files <- list.files(input_dir, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)

if (length(files) == 0) {
  stop("Ve složce Input/Data_test nebyly nalezeny žádné PDF soubory.")
}

print(paste("Nalezeno", length(files), "souborů. Začínám zpracování..."))

all_stanoviste <- list()
all_druhy <- list()

# Iterace pres soubory
for (i in seq_along(files)) {
  f <- files[i]
  fname <- basename(f)
  print(paste0("[", i, "/", length(files), "] Zpracovávám: ", fname))
  
  data <- process_pdf_file(f)
  
  # Ulozeni vysledku, pokud nejake jsou
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

# Spojeni seznamu do jednoho dataframe
final_stanoviste <- bind_rows(all_stanoviste)
final_druhy <- bind_rows(all_druhy)

# Ulozeni Stanovist
if (nrow(final_stanoviste) > 0) {
  out_path_s <- file.path(temp_dir, "souhrn_stanoviste.csv")
  
  # Pouzivame write.csv z base R, protoze umi fileEncoding.
  # row.names = FALSE, aby se nevytvoril sloupec s cisly radku.
  write.csv(final_stanoviste, out_path_s, row.names = FALSE, fileEncoding = "Windows-1250")
  
  print(paste("Uloženo stanovišť:", nrow(final_stanoviste), "->", out_path_s))
} else {
  print("Nebyla nalezena žádná data pro stanoviště.")
}

# Ulozeni Druhu
if (nrow(final_druhy) > 0) {
  out_path_d <- file.path(temp_dir, "souhrn_druhy.csv")
  
  write.csv(final_druhy, out_path_d, row.names = FALSE, fileEncoding = "Windows-1250")
  
  print(paste("Uloženo druhů:", nrow(final_druhy), "->", out_path_d))
} else {
  print("Nebyla nalezena žádná data pro druhy.")
}
