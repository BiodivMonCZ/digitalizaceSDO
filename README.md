# digitalizaceSDO
Tento R skript slouží k hromadné extrakci dat o předmětech ochrany (druhy a stanoviště) z PDF souborů souhrnů doporučených opatření (SDO).
Skript zpracuje složku s PDF soubory, parsuje nestrukturovaný text a exportuje vyčištěná data do formátu CSV (kompatibilní s Excel).

## 🚀 Funkce
Robustní čištění textu: Odstraňuje neviditelné znaky (entry, tabulátory), které běžně "rozbíjejí" CSV soubory.
Chytrá extrakce jmen: Správně rozděluje české a latinské názvy (zvládá i poddruhy a složené názvy).
Parsování populací: Dokáže najít min/max počty jedinců i v případě, že jsou v PDF rozděleny na více řádků.
Podpora typů populací: Rozlišuje stálou, rozmnožující se a zimující populaci.
Kódování: Výstup je v kódování Windows-1250, připravený pro přímé otevření v českém Excelu.

## 📋 Požadavky
Pro spuštění je nutné mít nainstalované R a následující knihovny:

```{r}
install.packages("stringr")
install.packages("dplyr")
install.packages("pdftools")
```

## Použití
Umístěte PDF soubory do složky Input/Data_test (nebo upravte cestu ve skriptu).

Spusťte skript v R / RStudio.

Výsledné soubory se uloží do definované výstupní složky (defaultně temp_dir).

## 📂 Výstup
Skript generuje dva soubory:

```souhrn_stanoviste.csv``` – Data o biotopech (kód, název, rozloha, cílový stav).

```souhrn_druhy.csv``` – Data o druzích (název CZ/LAT, populace, zachovalost, izolace, cílový stav).

Poznámka k datům
Skript používá heuristické metody a regulární výrazy (Regex) pro zpracování nestrukturovaného textu z PDF. Přestože je velmi robustní, u dokumentů s nestandardním formátováním se doporučuje manuální kontrola výstupu.
