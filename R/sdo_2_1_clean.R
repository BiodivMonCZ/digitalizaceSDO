druhy_raw <- read.csv("Input/Temp/souhrn_druhy.csv", fileEncoding = "Windows-1250")
stanoviste_raw <- read.csv("Input/Temp/souhrn_stanoviste.csv", fileEncoding = "Windows-1250")

druhy_clean <- druhy_raw %>%
  dplyr::mutate(
    # Pattern explanation:
    # \\s* = matches any amount of whitespace (spaces, tabs) before the *
    # \\* = matches the literal asterisk character
    # \\s* = matches any amount of whitespace after the *
    # $    = anchors the match to the end of the string
    druh_cz = str_remove(nazev_cz, "\\s*\\*\\s*$"),
    druh = str_remove(nazev_lat, "\\s*\\*\\s*$"),
    poznamka = str_remove(stav_text, "\\* označuje prioritní druh") %>% 
      str_remove_all(., "\\n") %>%
      str_squish(),
    pop_min = as.numeric(pop_min),
    pop_max = as.numeric(pop_max)
    ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    pop_prum = mean(c(pop_min, pop_max), na.rm = TRUE),
    pop_jednotka = dplyr::case_when(
      pop_jednotka == "-" ~ NA_character_,
      TRUE ~ pop_jednotka
    )
  )

stanoviste_clean <- stanoviste_raw %>%
  dplyr::select(-nazev_predmetu) %>%
  dplyr::mutate(
    poznamka = str_remove(stav_text, "\\* označuje prioritní druh") %>% 
      str_remove_all(., "\\n") %>%
      str_squish()
  ) 

# Uložení Stanovišť
if (nrow(stanoviste_clean) > 0) {
  out_path_s <- file.path(output_dir, "sdo_cilove_stanoviste.csv")
  
  # Používáme write.csv z base R, protože umí fileEncoding.
  # row.names = FALSE, aby se nevytvořil sloupec s čísly řádků.
  write.csv(stanoviste_clean, out_path_s, row.names = FALSE, fileEncoding = "Windows-1250")
  
  print(paste("Uloženo stanovišť:", nrow(stanoviste_clean), "->", out_path_s))
} else {
  print("Nebyla nalezena žádná data pro stanoviště.")
}

# Uložení Druhů
if (nrow(druhy_clean) > 0) {
  out_path_d <- file.path(output_dir, "sdo_cilove_druhy.csv")
  
  write.csv(druhy_clean, out_path_d, row.names = FALSE, fileEncoding = "Windows-1250")
  
  print(paste("Uloženo druhů:", nrow(druhy_clean), "->", out_path_d))
} else {
  print("Nebyla nalezena žádná data pro druhy.")
}

print("=== HOTOVO ===")
