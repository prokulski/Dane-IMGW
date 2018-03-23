## Skąd wziąć dane o pogodzie w Polsce?

## Pory roku na przestrzeni lat
# procentowy podział dni w roku na poszczególne pory roku
# temperatura w marcu

## Czy klimat się ociepla?
# różnice w temperaturze na przestrzeni lat
# miesiące cieplejsze i zimniejsze od średniej
# liczba dni cieplejszych i zimniejszych niż średnia

## Czy jest różnica miedzy temperaturami w kraju?

## Mapki temperatury dla 2017 roku

## Kiedyś to było...
# śnieg i temperatura w Boże Narodzenie
# opady i temperatura w Lany Poniedziałek


library(tidyverse)
library(lubridate)
library(ggridges)
library(ggrepel)
library(forcats)
library(DBI)
library(jsonlite)
library(kknn)

theme_set(theme_minimal() +
             theme(plot.title = element_text(family = NULL, face = "bold", size = 18, color = "black"),
                   plot.subtitle = element_text(family = NULL, face = "plain", size = 12, color = "black"),
                   plot.caption = element_text(family = NULL, face = "italic", size = 9, color = "darkgray"),
                   plot.background = element_rect(fill="#efefef", color="#aaaaaa"),
                   panel.background = element_rect(fill = "white", color="black"),
                   strip.text.x = element_text(face = "bold")))


dbase <- dbConnect(RSQLite::SQLite(), "imgw.sqlite")


# średnia temperatura dzienna ze wszystkich stacji, dzień po dniu
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
   # dodatnie = cieplej niż średnia
   mutate(dTemp = mTemp - mTemp_all) %>%
   # w zależności od średniej dziennej temperatury przypisujemy porę roku
   mutate(pora_roku = case_when(
      .$mTemp >= 15 ~ "lato",
      .$mTemp >= 5 ~ "wiosna/jesień",
      .$mTemp >= 0 ~ "przedwiośnie/przedzimie",
      .$mTemp < 0 ~ "zima",
      TRUE ~ "cos innego" # to nie ma prawa sie trafić :)
   ))



pory_roku_palette <- c("lato" = "#d7191c", "wiosna/jesień" = "#fdae61", "przedwiośnie/przedzimie" = "#abd9e9", "zima" = "#2c7bb6")

# pora roku na przestrzeni lat
mTemps %>%
   ggplot() +
   geom_tile(aes(make_date(2000, month(data), day(data)), Rok, fill = pora_roku)) +
   scale_fill_manual(values = pory_roku_palette) +
   scale_y_reverse() +
   scale_x_date(date_labels = "%d/%m", date_breaks = "1 month") +
   theme(legend.position = "bottom") +
   labs(title = "Pora roku określona na podstawie temperatury dobowej",
        x = "Dzień w roku", y = "Rok",
        fill = "Pora roku")


# to samo na poziomie średniej tygodniowej temperatury
mTemps %>%
   mutate(week = week(data)) %>%
   group_by(week, Rok) %>%
   summarise(mTemp = mean(mTemp)) %>%
   ungroup() %>%
   mutate(pora_roku = case_when(
      .$mTemp >= 15 ~ "lato",
      .$mTemp >= 5 ~ "wiosna/jesień",
      .$mTemp >= 0 ~ "przedwiośnie/przedzimie",
      .$mTemp < 0 ~ "zima",
      TRUE ~ "cos innego"
   )) %>%
   ggplot() +
   geom_tile(aes(week, Rok, fill = pora_roku)) +
   scale_fill_manual(values = pory_roku_palette) +
   scale_y_reverse() +
   scale_x_continuous(breaks = seq(1, 55, 5)) +
   theme(legend.position = "bottom") +
   labs(title = "Pora roku określona na podstawie temperatury dobowej",
        x = "Tydzień w roku", y = "Rok",
        fill = "Pora roku")





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
   theme(legend.position = "bottom") +
   labs(title = "Udział procentowy dni z określoną (na podstawie średniej dobowej temperatury)\nporą roku na przestrzeni lat 1951-2017",
        y = "% dni w danej porze roku", x = "", color = "Pora roku")


# temperatura w marcu
mTemps %>%
   filter(Miesiac == 3) %>%
   # średnia temperatura dzienna ze wszystkich lat
   ggplot() +
   geom_point(aes(make_date(2000, month(data), day(data)), mTemp, color = pora_roku)) +
   geom_line(aes(make_date(2000, month(data), day(data)), mTemp_all), alpha = 0.8) +
   facet_wrap(~Rok, ncol = 10) +
   scale_x_date(date_labels = "%d", date_breaks = "7 days") +
   theme(legend.position = "bottom") +
   labs(title = "Średnie dobowe temperatury w marcu w porównaniu do średniej z lat 1951-2017",
        y = "Średnia dobowa temperatura °C", x = "", color = "Pora roku")



## Czy klimat się ociepla?

# różnice od średniej
mTemps %>%
   ggplot() +
   geom_histogram(aes(dTemp, fill = as.factor(Rok)), binwidth = 0.5, show.legend = FALSE) +
   geom_vline(xintercept = 0, color = "red") +
   facet_wrap(~Rok, ncol = 10) +
   labs(title = "Rozkład różnicy temperatur dziennych w stosunku do średniej dla danego dnia z lat 1951-2017",
        x = "Różnica temperatury °C", y = "Liczba dni")


mTemps %>%
   # co 5 lat + ostatni rok extra
   filter(Rok %% 5 == 0 | Rok == 2017) %>%
   mutate(Rok = fct_rev(as.factor(Rok))) %>%
   ggplot() +
   geom_density_ridges(aes(dTemp, Rok, fill = as.factor(Rok)), show.legend = FALSE) +
   geom_vline(xintercept = 0, color = "red") +
   scale_x_continuous(limits = c(-10, 7.5)) +
   labs(title = "Rozkład różnicy temperatur dziennych w stosunku do średniej dla danego dnia z lat 1951-2017",
        x = "Różnica temperatury °C", y = "Rok")


# miesiące cieplejsze i zimniejsze od średniej
mTemps %>%
   group_by(Rok, Miesiac) %>%
   summarise(m_dTemp = mean(dTemp)) %>%
   ungroup() %>%
   mutate(Miesiac = factor(Miesiac, levels = 12:1)) %>%
   ggplot() +
   geom_tile(aes(Rok, Miesiac, fill = m_dTemp), color = "gray10") +
   scale_fill_gradient2(low = "blue", mid = "#ffffaa", high = "red", midpoint = 0) +
   theme(legend.position = "bottom") +
   labs(title = "Miesiące cieplejsze i zimniejsze w stosunku do średniej z lat 1951-2017",
        x = "Rok", y = "Miesiąc", fill = "Średnia miesięczna różnica temperatury w °C")


# liczba dni cieplejszych i zimniejszych niż średnia
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
   geom_hline(yintercept = 50) +
   scale_fill_manual(values = c("blue", "red"), labels = c("Zimniej", "Cieplej")) +
   theme(legend.position = "bottom") +
   labs(title = "Podział dni cieplejszych i zimniejszych niż średnia z lat 1951-2017",
        x = "Rok", y = "Udział %", fill = "Cieplej?")


mTemps %>%
   mutate(cieplej = dTemp > 0) %>%
   group_by(Rok, cieplej) %>%
   summarise(n = n()) %>%
   ungroup() %>%
   group_by(Rok) %>%
   mutate(cieplej_p = 100*n/sum(n)) %>%
   ungroup() %>%
   ggplot() +
   geom_smooth(aes(Rok, cieplej_p, color = cieplej), method = "loess") +
   scale_y_continuous(limits = c(0, 100)) +
   scale_color_manual(values = c("blue", "red"), labels = c("Zimniej", "Cieplej")) +
   theme(legend.position = "bottom") +
   labs(title = "Podział dni cieplejszych i zimniejszych niż średnia z lat 1951-2017",
        x = "Rok", y = "Udział %", color = "Cieplej?")





## czy jest różnica miedzy temperaturami w kraju?

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
   # kolejność stacji od północy
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
   geom_text(aes(Nazwa_stacji, poz, label = sprintf("%.1f%%", p))) +
   scale_y_reverse() +
   scale_fill_distiller(palette = "RdYlBu") +
   labs(title = "Bieguny ciepła i zimna według stacji meteo",
        y = "Pozycja w kolejności od najzimniejszych (1) do najcieplejszych (5)", x = "")



## Mapki 2017

# pomysł na bazie https://timogrossenbacher.ch/2018/03/categorical-spatial-interpolation-with-r/

# Wszystkie stacje jakie dokonały pomiarów w 2017 roku
stacje2017 <- dbGetQuery(dbase,
                         "SELECT Kod_stacji, COUNT(*) AS n
                         FROM imgw
                         WHERE Rok = 2017
                         GROUP BY Kod_stacji;") %>%
   # tylko te, z pełnym rokiem pomiarów
   filter(n == 365) %>%
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
                              paste0("SELECT Kod_stacji, Miesiac, Dzien, MeanTemp
                                     FROM imgw
                                     WHERE Rok = 2017 AND Kod_stacji IN (", paste(stacje2017$Kod_stacji, collapse = ", "), ");")) %>%
   mutate(Kod_stacji = as.character(Kod_stacji))%>%
   left_join(stacje_lokalizacje, by = "Kod_stacji") %>%
   mutate(data = make_date(2017, Miesiac, Dzien))


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
      select(long, lat, MeanTemp)

   # obliczamy wartości temperatury dla wszystkich punktów siatki
   temp_dzien_kknn <- kknn(MeanTemp~., temp_dzien, poland_grid, distance = 1, kernel = "gaussian")

   # dodajemy obliczone temperatury do siatki punktów
   temp_dzien_wynik <- poland_grid %>%
      mutate(MeanTemp = fitted(temp_dzien_kknn))

   # rysujemy cala mapę
   plot <- ggplot(temp_dzien_wynik) +
      # warstwa z temperaturami
      geom_point(aes(long, lat, color = MeanTemp)) +
      # warstwa z konturami Polski
      geom_polygon(data = mapa, aes(long, lat, group = group), color = "black", fill = NA) +
      # warstwa z punktem min temperatury
      # geom_label_repel(data = filter(temp_dzien_minmax, MeanTemp == min(MeanTemp)),
      #                  aes(long, lat, label = sprintf("%s: %.1f°C", Nazwa_stacji, MeanTemp)), alpha = 0.7) +
      # geom_point(data = filter(temp_dzien_minmax, MeanTemp == min(MeanTemp)),
      #            aes(long, lat), color = "blue") +
      # warstwa z punktem max temperatury
      # geom_label_repel(data = filter(temp_dzien_minmax, MeanTemp == max(MeanTemp)),
      #                  aes(long, lat, label = sprintf("%s: %.1f°C", Nazwa_stacji, MeanTemp)), alpha = 0.7) +
      # geom_point(data = filter(temp_dzien_minmax, MeanTemp == max(MeanTemp)),
      #            aes(long, lat), color = "red") +
      # stala skala kolorow dla wszystkich dni
      scale_color_gradient2(low = "blue", mid = "#ffffaa", high = "red", midpoint = 0,
                            limits = c(min(temperatura2017$MeanTemp), max(temperatura2017$MeanTemp))) +
      coord_map() +
      theme_void() +
      theme(legend.position = "bottom") +
      labs(title = f_data,
           subtitle = sprintf("Średnia temperatura w kraju:%*.1f°C", 5, mean(temp_dzien$MeanTemp)),
           color = "Średnia temperatura dobowa [°C]: ")

   # zapisanie wykresu na dysk
   ggsave(sprintf("mapki/%03d.png", yday(f_data)), plot,
          # rozmiar obrazka - 1920x1080
          width = 19.20, height = 10.80, dpi = 100)
}

# dla wszystkich kolejnych dni z 2017 roku generujemy mapę i zapisujemy jako plik PNG
seq(as_date("2017-01-01"), as_date("2017-12-31"), by = "day") %>% lapply(save_temp_map)


# potrzebny ImageMagick - jak zainstalować na Ubuntu https://www.tutorialspoint.com/articles/how-to-install-imagemagick-on-ubuntu
# convert -delay 50 -loop 0 *.png animation.gif
# lub ffmpeg
# ffmpeg -framerate 5 -i %03d.png -c:v libx264 -r 30 -pix_fmt yuv420p out.mp4

# pogoda zmienia sie zbyt szybko - lepsze byłyby dane godzinowe
# mozna skorzystac z https://dane.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/terminowe/klimat/ - pomiar co 3 razy na dobe (6, 12, 18),
# ale wowczas ospowiednio nalezy dostosowac loadery



# śnieg w Wigilie
# średnia temperatura dzienna ze wszystkich stacji, dzień po dniu
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
   labs(title = "Śnieg i temperatura w okresie Bożego Narodzenia (22-28 grudnia)",
        x = "Rok", y = "Grubość pokrywy śnieżnej w cm (słupki, niebieska linia)\nŚrednia temperatura dobowa w °C (punkty, czerwona linia)")




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


# wyznaczamy daty Lanych Poniedziałków
lane_poniedzialki <- tibble(data = data_Wielkanocy(1951:2017) + 1)

# pobieramy pogodę
pogoda_wielkanoc <- dbGetQuery(dbase,
                     "SELECT Rok, Miesiac, Dzien, AVG(SumaOpadow) AS SumaOpadow, AVG(MeanTemp) AS MeanTemp
                     FROM imgw
                     WHERE Rok <> 2018
                     GROUP BY Rok, Miesiac, Dzien;") %>%
   # robimy sobie pole z datą
   mutate(data = make_date(Rok, Miesiac, Dzien))

# JOINem dat Lanych Poniedziałków z pogodą zostawiamy tylko pogodę Lanych Poniedziałków
pogoda_wielkanoc <- left_join(lane_poniedzialki, pogoda_wielkanoc, by = "data")


pogoda_wielkanoc %>%
   ggplot() +
   geom_col(aes(Rok, SumaOpadow ), fill = "lightblue") +
   geom_smooth(aes(Rok, SumaOpadow ), color = "blue", se = FALSE) +
   geom_point(aes(Rok, MeanTemp), color = "red", alpha = 0.7) +
   geom_smooth(aes(Rok, MeanTemp), color = "red", se = FALSE) +
   labs(title = "Opady i temperatura w Lany Poniedziałek",
        x = "Rok", y = "Suma opadów w mm (słupki, niebieska linia)\nŚrednia temperatura dobowa w °C (punkty, czerwona linia)")
