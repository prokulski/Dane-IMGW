library(tidyverse)
library(rvest)
library(stringr)
library(glue)
library(DBI)

# zrodlo danych
base_url <- "https://dane.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/dobowe/klimat/"


# wez liste folderow
get_folders_list <- function(folder) {
   folder_url <- paste0(base_url, folder)

   page <- read_html(folder_url)
   hrefs <- page %>% html_nodes("a") %>% html_attr("href")
   hrefs <- hrefs[str_sub(hrefs , start = -1, end = -1) == "/"]
   hrefs <- hrefs[hrefs != "/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/dobowe/"]

   return(hrefs)
}


# wez liste plikow
get_files_list <- function(folder) {
   folder_url <- paste0(base_url, folder)

   page <- read_html(folder_url)
   hrefs <- page %>% html_nodes("a") %>% html_attr("href")
   hrefs <- hrefs[str_sub(hrefs , start = -4, end = -1) == ".zip"]

   return(hrefs)
}

# obsluga pliku
process_file <- function(plik_url) {
   # sciagnij plik
   tmp_file <- tempfile()
   download.file(plik_url, tmp_file)

   # rozpakuj
   unzip(tmp_file, exdir = "tempdir")

   # usun sciagniety
   unlink(tmp_file)

   # poszukaj plikow CSV z danymi
   pliki <- list.files("tempdir", "k_d_") %>% .[str_sub(., 1, 6) != "k_d_t_"]

   df_all <- tibble()

   for(k in seq_along(pliki)) {
      # wczytaj plik k_d_*.csv
      df <- read_csv(paste0("tempdir/", pliki[k]),
                     col_names = c("Kod_stacji", "Nazwa_stacji",
                                   "Rok", "Miesiac", "Dzien",
                                   "MaxTemp", "StatusMaxTemp",
                                   "MinTemp", "StatusMinTemp",
                                   "MeanTemp", "StatusMeanTemp",
                                   "MinTempG", "StatusMinTempG",
                                   "SumaOpadow", "StatusSumaOpadow",
                                   "RodzajOpadu",
                                   "WysSniegu", "StatusWysSniegu"),
                     col_types = "iciiidcdcdcdcdicic")

      df_all <- bind_rows(df_all, df)
   }

   # kasujemy rozpakowane pliki
   unlink("tempdir/*")

   return(df_all)
}





# pobierz listę folderów
folders <- get_folders_list("")

# baza do zapisu danych
dbase <- dbConnect(RSQLite::SQLite(), "imgw.sqlite")


# dla każdego folderu
for(i in seq_along(folders)) {

   # weź listę plików
   files <- get_files_list(folders[i])
   file_path <- paste0(base_url, folders[i], files)

   # dla każdego pliku z danego folderu (online)
   for(j in seq_along(file_path)) {
      pliczek <- file_path[j]
      print(glue("pobieranie pliku {pliczek}"))

      # przetworzenie archiwum
      IMGW_data <- process_file(pliczek)

      # zapis do bazy
      dbWriteTable(dbase, "imgw", IMGW_data, append = TRUE)
   }
}

# rozlaczenie
dbDisconnect(dbase)

