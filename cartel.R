# Curso MSyST 2027-1
#Ejemplo 
#Cartel: Accidentes de tránsito terrestre en la CDMX


library(tidyverse)
library(readr)
library(showtext)
library(sysfonts)
library(ggtext)
library(patchwork)
library(scales)
library(magick) # para imagenes .webp
library(grid)
library(sf)
library(geodata)




#datos
df1 <- read_tsv(
  "datos_cartel.csv",
  locale = locale(encoding = "UTF-16LE"),
  show_col_types = FALSE
) %>%
  filter(str_detect(`Nivel inicial`, "^Accidentes de tránsito terrestre")) %>%
  transmute(year = `Periodo Corto`, value = Valor) %>%
  na.omit() %>%
  arrange(year)

# punbtos clave
pico   <- df1 %>% slice_max(value, n = 1)
covid  <- df1 %>% filter(year == 2020)
pre    <- df1 %>% filter(year == 2019)
inicio <- df1 %>% slice_min(year, n = 1)
final  <- df1 %>% slice_max(year, n = 1)
claves <- bind_rows(pico, covid, final) %>% distinct()

# cifras para los textos
cambio_total <- round((final$value / inicio$value - 1) * 100)   # 1997 vs ultimo año
caida_covid  <- round((covid$value / pre$value - 1) * 100)      # 2019 vs 2020

# estilo que seguira todo el cartel
font <- "Gudea"
font_add_google(family = font, font, db_cache = TRUE)
theme_set(theme_minimal(base_family = font, base_size = 14))
bg      <- "#F4F5F1"
txt_col <- "black"
col_ser <- "#9C3D2E"
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)


# pie del cartel
caption_text <- str_glue(
  "**Autor:** Rodrigo Basaldud<br>",
  "**Fuente:** INEGI, Accidentes de tránsito terrestre en zonas urbanas y suburbanas 1997–2025. "
)

# Funcion auxiliar para paneles de texto (mismo estilo que el titulo y subtitulo)
caja <- function(label, size, width = .85, halign = 0, lineheight = 1.1) {
  ggplot(tibble(x = 0, y = 0, label = label), aes(x = x, y = y)) +
    geom_textbox(
      aes(label = label),
      box.color = bg, fill = bg, width = unit(width, "npc"),
      family = font, size = size, lineheight = lineheight, halign = halign
    ) +
    coord_cartesian(expand = FALSE, clip = "off") +
    theme_void() +
    theme(plot.background = element_rect(color = bg, fill = bg))
}

# logo de la cdmx
# es mejor usar un archivo .webp para que el fondo sea transparente
logo_img  <- image_read("CDMX_Logo.webp")          
logo_grob <- rasterGrob(as.raster(logo_img), interpolate = TRUE)

logo <- wrap_elements(full = logo_grob) +
  theme(plot.background = element_rect(color = bg, fill = bg))

# titulo y sub
title <- caja("**Accidentes de tránsito en la Ciudad de México**<br>",
              size = 17, width = .9, halign = .5, lineheight = 1)

sub <- caja("Número de accidentes de tránsito terrestre registrados cada año en zonas urbanas y suburbanas de la Ciudad de México, de 1997 a 2025. Tras alcanzar su nivel más alto en 2012, la cifra bajó de forma sostenida hasta la caída abrupta de 2020 por la pandemia, y desde entonces no ha recuperado los niveles previos.<br>",
            size = 6, width = .85)


#mapa de la cdmx, usa paqueteria geodata funcion gadm
mex <- geodata::gadm(country = "MEX", level = 2, path = tempdir()) %>%
  st_as_sf()

cdmx <- mex %>%
  filter(str_detect(NAME_1, "Distrito Federal|Ciudad de M"))

mapa <- ggplot(cdmx) +
  geom_sf(fill = alpha(col_ser, .25), color = bg, linewidth = .6) +
  geom_sf_text(aes(label = NAME_2), family = font, size = 3.2,
               color = txt_col, check_overlap = TRUE) +
  coord_sf(expand = FALSE, clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(color = bg, fill = bg),
    plot.margin = margin(10, 10, 10, 10)
  )

# texto a la derecha del mapa
texto_mapa <- caja(
  str_glue(
    "**La Ciudad de México**<br>",
    "se divide en 16 alcaldías y concentra millones de desplazamientos diarios.<br><br>",
    "Durante {pico$year} se registraron **{comma(pico$value)}** accidentes de tránsito, el máximo del periodo observado.<br><br>",
    "Durante la pandemia (entre 2019 y 2020) hubo una caida de **{caida_covid}%**<br><br>",
    "Entre {inicio$year} y {final$year} se registró un descenso de **{cambio_total}%**.<br>"
  ),
  size = 5.5, width = .9, lineheight = 1.15
)

fila_mapa <- (mapa | texto_mapa) + plot_layout(widths = c(1.3, 1))


#serie de tiempo
p1 <- ggplot(df1, aes(x = year, y = value)) +
  geom_area(fill = col_ser, alpha = .12) +
  geom_line(color = col_ser, linewidth = 1.1) +
  geom_point(data = claves, color = col_ser, size = 3.5) +
  geom_text(
    data = claves,
    aes(label = comma(value)),
    vjust = -1.2, size = 5, family = font, fontface = "bold", color = col_ser
  ) +
  annotate("text", x = pico$year, y = pico$value + 2300,
           label = str_glue("Máximo histórico\n({pico$year})"),
           family = font, size = 4, lineheight = .9, color = txt_col) +
  annotate("text", x = covid$year, y = covid$value - 2300,
           label = "Pandemia\nCOVID-19", family = font, size = 4,
           lineheight = .9, color = txt_col) +
  scale_x_continuous(
    breaks = c(1997, seq(2000, 2020, by = 5), 2025),
    expand = expansion(mult = c(.02, .06))
  ) +
  scale_y_continuous(
    limits = c(0, 20000),
    breaks = seq(0, 20000, by = 5000),
    labels = comma
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(color = txt_col, size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(color = bg, fill = bg),
    plot.margin = margin(10, 30, 10, 10)
  )

# texto debajo de la serie
texto_serie <- caja(
  "Se puede apreciar una tendencia decreciente durante el periodo 1997 a 2025, no es plausible hablar de un periodo estacional sin embargo sí de ciclicidad. Hay un cambio abrupto en el comportamiento del fenómeno a partir de la pandemia COVID-19.<br>",
  size = 4.5, width = .85
)

# encabezado
# logo izquiera + titulo
# texto abajo
encabezado <- (logo | title) + plot_layout(widths = c(1, 4))


#junta todo
# para asegurar que la proporcion vertical
# encabezado .8 / subtitulo 1.2 / mapa+texto 1.8 / serie 1.8 / texto .4

finalPlot <- encabezado / sub / fila_mapa / p1 / texto_serie +
  plot_layout(heights = c(.8, 1.2, 1.8, 1.8, .4)) +
  plot_annotation(
    caption = caption_text,
    theme = theme(
      plot.caption = element_markdown(
        hjust = 0, margin = margin(20, 0, 0, 0),
        size = 10, color = txt_col, lineheight = 1.2
      ),
      plot.margin = margin(30, 30, 30, 30),
      plot.background = element_rect(color = bg, fill = bg)
    )
  )

finalPlot

ggsave("cartel3.png", finalPlot,
       width = 12, height = 18, dpi = 300, bg = bg)
