library(tidyverse)
library(lubridate)
library(ggridges)
library(ggrepel)
library(forcats)
library(DBI)
library(jsonlite)
library(kknn)

dbase <- dbConnect(RSQLite::SQLite(), "imgw_hours.sqlite")


## Mapki 2017

# pomysł na bazie https://timogrossenbacher.ch/2018/03/categorical-spatial-interpolation-with-r/

# Wszystkie stacje jakie dokonały pomiarów w 2017 roku
stacje2017 <- dbGetQuery(dbase,
                         "SELECT Kod_stacji, COUNT(*) AS n
                         FROM imgw
                         WHERE Rok = 2017
                         GROUP BY Kod_stacji;") %>%
   # tylko te, z pełnym rokiem pomiarów (3 razy przez 365 dób)
   filter(n == 1095) %>%
   mutate(Kod_stacji = as.character(Kod_stacji))

# położenie stacji
stacje_lokalizacje <- fromJSON("http://monitor.pogodynka.pl/api/map/?category=meteo") %>%
   select(Kod_stacji = i, long = lo, lat = la, Nazwa_stacji = n) %>%
   mutate(Kod_stacji = as.character(Kod_stacji))


# łączymy z położeniem i zostawiamy te, które mają położenie
stacje2017 <- left_join(stacje2017, stacje_lokalizacje, by = "Kod_stacji") %>%
   filter(!is.na(long))

# temperatura w 2017 roku w wybranych stacjach pomiarowych
temperatura2017 <- dbGetQuery(dbase,
                              paste0("SELECT Kod_stacji, Miesiac, Dzien, Godzina, Temp
                                     FROM imgw
                                     WHERE Rok = 2017 AND Kod_stacji IN (", paste(stacje2017$Kod_stacji, collapse = ", "), ");")) %>%
   mutate(Kod_stacji = as.character(Kod_stacji))%>%
   left_join(stacje_lokalizacje, by = "Kod_stacji") %>%
   mutate(data = make_datetime(2017, Miesiac, Dzien, Godzina))


# kontury Polski
mapa <- map_data("world") %>% filter(region == "Poland")

# siatka punktów w Polsce
poland_grid <- expand.grid(long = seq(min(mapa$long), max(mapa$long), 0.05),
                           lat = seq(min(mapa$lat), max(mapa$lat), 0.05))

# w tym miejscu dobrze byłoby sprawdzic, ktore z punktow siatki leza w Polsce i odciac te, ktore sa poza granicami
# przygadna bedzie funckja gWithin() z pakietu rgeos - wiecej szczegolow we wpisie http://prokulski.net/index.php/2017/12/22/mapy-w-r-rgeos/


# funkcja dla wskazanego dnia interpoluje temperaturę i zapisuje gotowa mapę
save_temp_map <- function(f_data) {

   # dane ze wszystkich stacji tego dnia
   temp_dzien <- temperatura2017 %>%
      filter(data == f_data)

   # szukamy stacji z temperatura minimalna i maksymalna danego dnia
   # temp_dzien_minmax <- temp_dzien %>%
   #    filter(MeanTemp == min(MeanTemp) | MeanTemp == max(MeanTemp))


   # dane potrzebne do obliczenia wartości pozostałych punktów w siatce
   temp_dzien <- temp_dzien %>%
      select(long, lat, Temp)

   # obliczamy wartości temperatury dla wszystkich punktów siatki
   temp_dzien_kknn <- kknn(Temp~., temp_dzien, poland_grid, distance = 1, kernel = "gaussian")

   # dodajemy obliczone temperatury do siatki punktów
   temp_dzien_wynik <- poland_grid %>%
      mutate(Temp = fitted(temp_dzien_kknn))

   # rysujemy cala mapę
   plot <- ggplot(temp_dzien_wynik) +
      # warstwa z temperaturami
      geom_point(aes(long, lat, color = Temp)) +
      # warstwa z konturami Polski
      geom_polygon(data = mapa, aes(long, lat, group = group), color = "black", fill = NA) +
      # stala skala kolorow dla wszystkich dni
      scale_color_gradient2(low = "blue", mid = "#ffffaa", high = "red", midpoint = 0,
                         limits = c(min(temperatura2017$Temp), max(temperatura2017$Temp))) +
      coord_map() +
      theme_void() +
      theme(legend.position = "bottom") +
      labs(title = format(f_data, "%Y-%m-%d %H:%M"),
           subtitle = sprintf("Średnia temperatura w kraju:%*.1f°C", 5, mean(temp_dzien$Temp)),
           color = "Średnia temperatura dobowa [°C]: ")

   # zapisanie wykresu na dysk
   ggsave(sprintf("mapki_h/%03d%02d.png", yday(f_data), hour(f_data)), plot,
          # rozmiar obrazka - 1920x1080
          width = 19.20, height = 10.80, dpi = 100)
}

# co 6 godzin kazdego dnia 2017 roku
czasy <- tibble(dzien = seq(as_date("2017-01-01"), as_date("2017-12-31"), by = "day")) %>%
   crossing(godzina = c(6, 12, 18)) %>%
   mutate(czas = make_datetime(year(dzien), month(dzien), day(dzien), godzina)) %>%
   pull(czas)

# generujemy mape
lapply(czasy, save_temp_map)
