library(tidyverse)
library(lubridate)
library(ggridges)
library(forcats)
library(DBI)


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



## Czy klimat sie ociepla?

# roznice od sredniej
mTemps %>%
   ggplot() +
   geom_histogram(aes(dTemp, fill = as.factor(Rok)), binwidth = 0.5, show.legend = FALSE) +
   geom_vline(xintercept = 0, color = "red") +
   facet_wrap(~Rok)




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
   # od najzimniejszej temperatury
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

