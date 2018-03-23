## Skad wziac dane o pogodzie w Polsce?

## Pory roku na przaestrzeni lat
# procentowy podział dni w roku na poszczególne pory roku
# temperatura w marcu

## Czy klimat sie ociepla?
# roznice w temperaturze na przestrzeni lat
# miesiace cieplejsze i zimniejsze od sredniej
# liczba dni cieplejszych i ziemniejszych niz srednia

## Czy jest roznica miedzy temperaturami w kraju?

## Mapki temperatury dla 2017 roku

## Kiedys to było...
# snieg i temperatura w Boże Narodzenie
# opady i temperatura w Lany Poniedziałek


library(tidyverse)
library(lubridate)
library(ggridges)
library(forcats)
library(DBI)
library(jsonlite)
library(kknn)


theme_set(theme_bw())

dbase <- dbConnect(RSQLite::SQLite(), "imgw.sqlite")


# srednia stednia temperatura dzienna ze wszystkich stacji, dzień po dniu
mTemps <- dbGetQuery(dbase,
                     "SELECT Rok, Miesiac, Dzien, AVG(MeanTemp) AS mTemp
                     FROM imgw
                     WHERE Rok <> 2018
                     GROUP BY Rok, Miesiac, Dzien;") %>%
   # robimy sobie pole z datą
   mutate(data = make_date(Rok, Miesiac, Dzien)) %>%
   # średnia temperatura dzienna ze wszystkich lat
   group_by(Miesiac, Dzien) %>%
   mutate(mTemp_all = mean(mTemp)) %>%
   ungroup() %>%
   # różnica pomiędzy temperaturą danego dnia a średnią ze wszystkich lat tego dnia
   # dodatnie = cieplej niz średnia
   mutate(dTemp = mTemp - mTemp_all) %>%
   # w zależności od średniej dziennej temperatury przypisujemy porę roku
   mutate(pora_roku = case_when(
      .$mTemp >= 15 ~ "lato",
      .$mTemp >= 5 ~ "wiosna/jesien",
      .$mTemp >= 0 ~ "przedwiosnie/przedzimie",
      .$mTemp < 0 ~ "zima",
      TRUE ~ "cos innego" # to nie ma prawa sie trafić :)
   ))



pory_roku_palette <- c("lato" = "#d7191c", "wiosna/jesien" = "#fdae61", "przedwiosnie/przedzimie" = "#abd9e9", "zima" = "#2c7bb6")

# pora roku na przestrzeni lat
mTemps %>%
   ggplot() +
   geom_tile(aes(make_date(2000, month(data), day(data)), Rok, fill = pora_roku)) +
   scale_fill_manual(values = pory_roku_palette) +
   scale_y_reverse() +
   scale_x_date(date_labels = "%d/%m", date_breaks = "1 month") +
   theme(legend.position = "bottom")


# to samo na poziomie średniej tygodniowej temperatury
mTemps %>%
   mutate(week = week(data)) %>%
   group_by(week, Rok) %>%
   summarise(mTemp = mean(mTemp)) %>%
   ungroup() %>%
   mutate(pora_roku = case_when(
      .$mTemp >= 15 ~ "lato",
      .$mTemp >= 5 ~ "wiosna/jesien",
      .$mTemp >= 0 ~ "przedwiosnie/przedzimie",
      .$mTemp < 0 ~ "zima",
      TRUE ~ "cos innego"
   )) %>%
   ggplot() +
   geom_tile(aes(week, Rok, fill = pora_roku)) +
   scale_fill_manual(values = pory_roku_palette) +
   scale_y_reverse() +
   theme(legend.position = "bottom")




# procentowy podział dni w roku na poszczególne pory roku
mTemps %>%
   group_by(Rok, pora_roku) %>%
   summarise(n = n()) %>%
   ungroup() %>%
   group_by(Rok) %>%
   mutate(p = 100*n/sum(n)) %>%
   ungroup() %>%
   filter(Rok != 2018) %>%
   ggplot() +
   geom_smooth(aes(Rok, p, color = pora_roku), se = FALSE) +
   labs(y = "% dni w danej porze roku", x = "")


# temperatura w marcu
mTemps %>%
   filter(Miesiac == 3) %>%
   # średnia temperatura dzienna ze wszystkich lat
   ggplot() +
   geom_point(aes(make_date(2000, month(data), day(data)), mTemp, color = pora_roku)) +
   geom_line(aes(make_date(2000, month(data), day(data)), mTemp_all), alpha = 0.8) +
   facet_wrap(~Rok) +
   scale_x_date(date_labels = "%d/%m") +
   theme(legend.position = "bottom")



## Czy klimat sie ociepla?

# roznice od sredniej
mTemps %>%
   ggplot() +
   geom_histogram(aes(dTemp, fill = as.factor(Rok)), binwidth = 0.5, show.legend = FALSE) +
   geom_vline(xintercept = 0, color = "red") +
   facet_wrap(~Rok, ncol = 10)




mTemps %>%
   # co 5 lat + ostatni rok extra
   filter(Rok %% 5 == 0 | Rok == 2017) %>%
   mutate(Rok = fct_rev(as.factor(Rok))) %>%
   ggplot() +
   geom_density_ridges(aes(dTemp, Rok, fill = as.factor(Rok)), show.legend = FALSE) +
   geom_vline(xintercept = 0, color = "red") +
   scale_x_continuous(limits = c(-10, 7.5))


# miesiace cieplejsze i zimniejsze od sredniej
mTemps %>%
   group_by(Rok, Miesiac) %>%
   summarise(m_dTemp = mean(dTemp)) %>%
   ungroup() %>%
   mutate(Miesiac = factor(Miesiac, levels = 12:1)) %>%
   ggplot() +
   geom_tile(aes(Rok, Miesiac, fill = m_dTemp), color = "gray10") +
   scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)


# liczba dni cieplejszych i ziemniejszych niz srednia
mTemps %>%
   mutate(cieplej = dTemp > 0) %>%
   group_by(Rok, cieplej) %>%
   summarise(n = n()) %>%
   ungroup() %>%
   group_by(Rok) %>%
   mutate(cieplej_p = 100*n/sum(n)) %>%
   ungroup() %>%
   ggplot() +
   geom_col(aes(Rok, cieplej_p, fill = cieplej)) +
   geom_hline(yintercept = 50)


mTemps %>%
   mutate(cieplej = dTemp > 0) %>%
   group_by(Rok, cieplej) %>%
   summarise(n = n()) %>%
   ungroup() %>%
   group_by(Rok) %>%
   mutate(cieplej_p = 100*n/sum(n)) %>%
   ungroup() %>%
   ggplot() +
   geom_smooth(aes(Rok, cieplej_p, color = cieplej), se = FALSE) +
   scale_y_continuous(limits = c(0, 100))




## czy jest roznica miedzy temperaturami w kraju?

# z jakich stacji mamy najwięcej pomiarów?
dbGetQuery(dbase,
           "SELECT Nazwa_stacji, Kod_stacji, COUNT(*) AS n
           FROM imgw
           GROUP BY Kod_stacji ORDER BY n DESC LIMIT 10;")

stacje <- tribble(
   ~Nazwa_stacji, ~Kod_stacji,
   "Kraków", 250190390,
   "Kórnik", 252170210,
   "Warszawa", 252200150,
   "Pułtusk", 252210050,
   "Lidzbark Warmiński", 254200080)

tab <- dbGetQuery(dbase,
                  "SELECT Rok, Miesiac, Dzien, Kod_stacji, MeanTemp
                  FROM imgw
                  WHERE Kod_stacji IN (250190390, 252170210, 252200150, 252210050, 254200080);") %>%
   mutate(data = make_date(Rok, Miesiac, Dzien)) %>%
   left_join(stacje, by = "Kod_stacji") %>%
   # kolejnosc stacji od północy
   mutate(Nazwa_stacji = fct_relevel(Nazwa_stacji, "Lidzbark Warmiński", "Pułtusk", "Warszawa", "Kórnik", "Kraków"))

# liczba najcieplejszych i najzimniejszych dni wg stacji
# n = 1 -> najzimniejsza stacja
tab %>%
   group_by(data) %>%
   # od najniższej temperatury
   arrange(MeanTemp) %>%
   mutate(poz = 1:n()) %>%
   ungroup() %>%
   # ile razy dana stacja była najcieplejsza itp
   count(Nazwa_stacji, poz) %>%
   group_by(Nazwa_stacji) %>%
   mutate(p = 100*n/sum(n)) %>%
   ungroup() %>%
   ggplot() +
   geom_tile(aes(Nazwa_stacji, poz, fill = p), show.legend = FALSE, color = "gray50") +
   geom_text(aes(Nazwa_stacji, poz, label = round(p, 1))) +
   scale_y_reverse() +
   scale_fill_distiller(palette = "RdYlBu") +
   labs(y = "Pozycja w kolejności od najzimniejszych (1) do najcieplejszych (5)", x = "")



## Mapki 2017

# Wszystkie stacje jakie dokonaly pomiarow w 2017 roku
stacje2017 <- dbGetQuery(dbase,
                        "SELECT Kod_stacji, COUNT(*) AS n
                        FROM imgw
                        WHERE Rok = 2017
                        GROUP BY Kod_stacji;") %>%
   # tylko te, z pelnym rokiem pomiarow
   filter(n == 365) %>%
   mutate(Kod_stacji = as.character(Kod_stacji))

# polozenie stacji
stacje_lokalizacje <- fromJSON("http://monitor.pogodynka.pl/api/map/?category=meteo") %>%
   select(Kod_stacji = i, long = lo, lat = la, Nazwa_stacji = n) %>%
   mutate(Kod_stacji = as.character(Kod_stacji))


# laczymy z polozeniem i zostawiamy te, ktore maja polozenie
stacje2017 <- left_join(stacje2017, stacje_lokalizacje, by = "Kod_stacji") %>%
   filter(!is.na(long))

# temperatura w 2017 roku w wybranych stacjach pomiarowych
temperatura2017 <- dbGetQuery(dbase,
           paste0("SELECT Kod_stacji, Miesiac, Dzien, MeanTemp
                  FROM imgw
                  WHERE Rok = 2017 AND Kod_stacji IN (", paste(stacje2017$Kod_stacji, collapse = ", "), ");")) %>%
   mutate(Kod_stacji = as.character(Kod_stacji))%>%
   left_join(stacje_lokalizacje, by = "Kod_stacji") %>%
   mutate(data = make_date(2017, Miesiac, Dzien))


# kontury Polski
mapa <- map_data("world") %>% filter(region == "Poland")

# siatka punktow w Polsce
poland_grid <- expand.grid(long = seq(min(mapa$long), max(mapa$long), 0.05),
                           lat = seq(min(mapa$lat), max(mapa$lat), 0.05))


# funkcja dla wskazanego dnia interpoluje temperature i zapisuje gotowa mape
save_temp_map <- function(f_data) {
   temp_dzien <- temperatura2017 %>%
      filter(data == f_data) %>%
      select(long, lat, MeanTemp)

   temp_dzien_kknn <- kknn(MeanTemp~., temp_dzien, poland_grid, distance = 1, kernel = "gaussian")

   temp_dzien_wynik <- poland_grid %>%
      mutate(MeanTemp = fitted(temp_dzien_kknn))

   plot <- ggplot(temp_dzien_wynik) +
      # warstwa z temperaturami
      geom_point(aes(long, lat, color = MeanTemp)) +
      # warstwa z konturami Polski
      geom_polygon(data = mapa, aes(long, lat, group = group), color = "black", fill = NA) +
      # stala skala kolorow dla wszystkich dni
      scale_color_gradient2(low = "blue", mid = "white", high = "red",
                            limits = c(min(temperatura2017$MeanTemp), max(temperatura2017$MeanTemp))) +
      coord_map() +
      theme_void() +
      theme(legend.position = "bottom") +
      labs(title = f_data,
           subtitle = sprintf("Srednia temperatura w kraju: %.1f", mean(temp_dzien$MeanTemp)),
           color = "Srednia temperatura dobowa: ")

   # zapisanie wykresu na dysk
   ggsave(sprintf("mapki/%03d.png", yday(f_data)), plot,
          # rozmiar obrazka - 800x600 px
          width = 8, height = 6, dpi = 100)
}

# dla wszystkich kolejnych dni z 2017 roku generujemy mape i zapisujemy jako plik PNG
seq(as_date("2017-01-01"), as_date("2017-12-31"), by = "day") %>% lapply(save_temp_map)

# potrzebny ImageMagick
# jak zainstalować na Ubuntu https://www.tutorialspoint.com/articles/how-to-install-imagemagick-on-ubuntu
# convert -delay 50 -loop 0 *.png animation.gif




# snieg w Wigilie
# srednia stednia temperatura dzienna ze wszystkich stacji, dzień po dniu
pogoda_bn <- dbGetQuery(dbase,
                     "SELECT Rok, Miesiac, Dzien, AVG(WysSniegu) AS WysSniegu, AVG(MeanTemp) AS MeanTemp
                     FROM imgw
                     WHERE Rok <> 2018 AND Miesiac = 12 AND Dzien IN (22, 23, 24, 25, 26, 27, 28)
                     GROUP BY Rok, Miesiac, Dzien;") %>%
   # robimy sobie pole z datą
   mutate(data = make_date(Rok, Miesiac, Dzien))

pogoda_bn %>%
   group_by(Rok) %>%
   summarise(snieg = mean(WysSniegu),
             temp = mean(MeanTemp)) %>%
   ungroup() %>%
   ggplot() +
   geom_col(aes(Rok, snieg), fill = "lightblue") +
   geom_smooth(aes(Rok, snieg), color = "blue", se = FALSE) +
   geom_point(aes(Rok, temp), color = "red", alpha = 0.7) +
   geom_smooth(aes(Rok, temp), color = "red", se = FALSE) +
   labs(title = "Snieg i temperatura w okresie Bożego Narodzenia (22-28 grudnia)",
        x = "Rok", y = "Wysokosc pokrywy snieznej w cm (slupki, niebieska linia)\nSrednia temperatura dobowa w C (punkty, czerowna linia)")




# czy Lany Poniedziałek zawsze był zimny?

## wyznaczenie daty Wielkanocy
# za http://www.algorytm.org/przetwarzanie-dat/wyznaczanie-daty-wielkanocy-metoda-meeusa-jonesa-butchera.html
# Metoda Metoda Meeusa/Jonesa/Butchera przedstawiona przez [Jeana Meeusa](https://pl.wikipedia.org/wiki/Jean_Meeus) w jego książce *Astronomical Algorithms* w 1991 roku.

data_Wielkanocy <- function(rok) {
   a <- rok %% 19
   b <- floor(rok/100)
   c <- rok %% 100
   d <- floor(b/4)
   e <- b %% 4
   f <- floor((b+8)/25)
   g <- floor((b-f+1)/3)
   h <- (19*a + b - d - g +15) %% 30
   i <- floor(c/4)
   k <- c %% 4
   l <- (32+2*e+2*i-h-k) %% 7
   m <- floor((a + 11*h + 22*l)/451)
   p <- (h + l - 7*m + 114) %% 31
   dzien <- p + 1
   miesiac <- floor((h + l - 7*m + 114)/31)

   return(make_date(rok, miesiac, dzien))
}


# wyznaczamy daty Lanych Poniedzialków
lane_poniedzialki <- tibble(data = data_Wielkanocy(1951:2017) + 1)

# pobieramy pogode
pogoda_wielkanoc <- dbGetQuery(dbase,
                    "SELECT Rok, Miesiac, Dzien, AVG(SumaOpadow) AS SumaOpadow, AVG(MeanTemp) AS MeanTemp
                     FROM imgw
                     WHERE Rok <> 2018
                     GROUP BY Rok, Miesiac, Dzien;") %>%
   # robimy sobie pole z datą
   mutate(data = make_date(Rok, Miesiac, Dzien))


pogoda_wielkanoc <- left_join(lane_poniedzialki, pogoda_wielkanoc, by = "data")


pogoda_wielkanoc %>%
   ggplot() +
   geom_col(aes(Rok, SumaOpadow ), fill = "lightblue") +
   geom_smooth(aes(Rok, SumaOpadow ), color = "blue", se = FALSE) +
   geom_point(aes(Rok, MeanTemp), color = "red", alpha = 0.7) +
   geom_smooth(aes(Rok, MeanTemp), color = "red", se = FALSE) +
   labs(title = "Opady i temperatura w Lany Poniedziałek",
        x = "Rok", y = "Suma opadow w mm (slupki, niebieska linia)\nSrednia temperatura dobowa w C (punkty, czerowna linia)")


