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
  ) %>%
  dplyr::rename(
    sdf_code = feature_code
  ) %>%
  dplyr::mutate(
    sdf_code = as.character(sdf_code)
  ) %>%
  dplyr::left_join(
    .,
    sites_subjects %>%
      dplyr::select(
        feature_code,
        sdf_code
      ),
    by = c("sdf_code" = "sdf_code")
  ) %>%
  dplyr::distinct()

# === NDOP: maximální abundance (od roku 2000 dosud) dle nálezové databáze AOPK ===
# Logika navazuje na https://github.com/BiodivMonCZ/host_naturecz (R/02_druhy/21_1_n2k_druhy_akce.R):
# nález je platný, pokud jednotka POCITANO odpovídá jednotce definované v limitech
# (zde: pop_jednotka z SDO). Přednost mají vždy nálezy s přesnou shodou jednotky -
# u druhu a lokality, kde takový nález existuje, se náhradní jednotky vůbec
# neuvažují. Teprve pokud pro danou dvojici druh x lokalita neexistuje žádný
# nález s přesnou shodou jednotky, použijí se u nálezů starších roku
# rok_hranice_jedinci (tj. z roku 2019 a dříve) jako náhrada i jiná dospělá
# vývojová stadia (POCITANO %in% pocitano_nahradni), protože starší data
# jednotku nerozlišovala tak důsledně.
rok_od <- 2000
rok_hranice_jedinci <- 2020
pocitano_nahradni <- c("jedinci", "adulti")

limity <- druhy_clean %>%
  dplyr::transmute(
    sitecode,
    druh,
    jednotka = str_squish(pop_jednotka)
  ) %>%
  dplyr::filter(!is.na(jednotka), jednotka != "") %>%
  dplyr::distinct()

nalezy_raw <- readr::read_delim(
  nalezy_path,
  delim = ";",
  locale = readr::locale(encoding = "Windows-1250"),
  col_types = readr::cols(.default = "c"),
  progress = FALSE
)

nalezy_clean <- nalezy_raw %>%
  dplyr::filter(is.na(NEGATIVNI) | NEGATIVNI == "0") %>%
  dplyr::transmute(
    druh = DRUH,
    sitecode = str_trim(str_extract(EVL, "^[^:]+")),
    rok = lubridate::year(lubridate::dmy(DATUM_OD)),
    pocitano = str_squish(POCITANO),
    pocet = suppressWarnings(as.numeric(POCET))
  ) %>%
  dplyr::filter(!is.na(rok), rok >= rok_od, !is.na(pocet))

# Ověření jednotky dle limitů (přesná shoda) + náhradní dospělá stadia pro nálezy
# starší rok_hranice_jedinci, použitá jen pokud přesná shoda pro danou dvojici
# druh x lokalita chybí
nalezy_klasifikace <- nalezy_clean %>%
  dplyr::mutate(zaznam_id = dplyr::row_number()) %>%
  dplyr::left_join(limity, by = c("druh", "sitecode"), relationship = "many-to-many") %>%
  dplyr::group_by(zaznam_id) %>%
  dplyr::summarise(
    druh = dplyr::first(druh),
    sitecode = dplyr::first(sitecode),
    rok = dplyr::first(rok),
    pocitano = dplyr::first(pocitano),
    pocet = dplyr::first(pocet),
    presna_shoda = any(pocitano == jednotka, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    nahradni_shoda = rok < rok_hranice_jedinci & pocitano %in% pocitano_nahradni
  ) %>%
  dplyr::filter(presna_shoda | nahradni_shoda)

# Přednost přesné shodě jednotky: pokud pro dvojici druh x lokalita existuje
# alespoň jeden nález s přesnou shodou, náhradní nálezy se zahodí
nalezy_valid <- nalezy_klasifikace %>%
  dplyr::group_by(druh, sitecode) %>%
  dplyr::filter(presna_shoda | !any(presna_shoda)) %>%
  dplyr::ungroup()

ndop_max_abundance <- nalezy_valid %>%
  dplyr::group_by(druh, sitecode) %>%
  dplyr::summarise(
    ndop_pop_max = max(pocet, na.rm = TRUE),
    ndop_pocitano = pocitano[which.max(pocet)],
    ndop_pocet_zaznamu = dplyr::n(),
    .groups = "drop"
  )

# Napojení maximální abundance a návrh hodnoty (vyšší z pop_prum a ndop_pop_max)
druhy_clean <- druhy_clean %>%
  dplyr::left_join(ndop_max_abundance, by = c("druh", "sitecode")) %>%
  dplyr::mutate(
    navrzena_hodnota = floor(pmax(pop_prum, ndop_pop_max, na.rm = TRUE))
  )

stanoviste_clean <- stanoviste_raw %>%
  dplyr::select(-nazev_predmetu) %>%
  dplyr::mutate(
    poznamka = str_remove(stav_text, "\\* označuje prioritní druh") %>% 
      str_remove_all(., "\\n") %>%
      str_squish(),
    stav_cis = NA
  ) %>%
  dplyr::distinct()

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
