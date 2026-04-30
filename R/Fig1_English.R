# pak::pak("davidsjoberg/ggbump")

library(tidyverse)
library(ggbump)
library(patchwork)
library(data.table)

# Colors
color_palette <- viridisLite::mako(11)
description_color <- 'grey40'
cities_colors <- c(
  'São Paulo' = color_palette[2],
  'Fortaleza' = color_palette[7],
  'Recife' = color_palette[8],
  'Salvador' = color_palette[5],
  'Belo Horizonte' = color_palette[6],
  'Rio de Janeiro' = color_palette[3],
  'Curitiba' = color_palette[10],
  'Porto Alegre' = color_palette[9],
  'Brasília' = color_palette[4]
)

cities_rank_acessibilidade <- fread("./data/resultado_acessibilidade.csv", encoding = 'Latin-1') |>
  mutate(ordem=1)

cities_rank_censo <- fread("./data/resultado_censo2022.csv") |>
  mutate(cidade = case_when(code_muni == 2304400 ~ "Fortaleza",
                            code_muni == 2611606 ~ "Recife",
                            code_muni == 2927408 ~ "Salvador",
                            code_muni == 3106200 ~ "Belo Horizonte",
                            code_muni == 3304557 ~ "Rio de Janeiro",
                            code_muni == 3550308 ~ "São Paulo",
                            code_muni == 4106902 ~ "Curitiba",
                            code_muni == 4314902 ~ "Porto Alegre",
                            code_muni == 5300108 ~ "Brasília")) |>
  filter(!is.na(cidade)) |>
  mutate(rank = row_number(desc(avg_commute_time)),
         ordem = 2)


cities_rank_censo <- cities_rank_censo |>
  select("rank","cidade","ordem")

cities_rank_tomtom <- fread("./data/resultado_tomtom.csv", encoding = 'Latin-1') |>
  mutate(ordem=3)

cities_rank_geral <- rbind(cities_rank_acessibilidade,cities_rank_censo,cities_rank_tomtom) #|>



bump_chart_basic <- cities_rank_geral |>
  ggplot(aes(ordem, rank, col = cidade)) +
  geom_point(shape = '|', stroke = 6) +
  geom_bump(size = 1.5) +
  geom_text(
    data = cities_rank_geral |> filter(ordem == 1),
    aes(label = cidade),
    hjust = 1,
    nudge_x = -0.1,
    fontface = 'bold'
  ) +
  geom_text(
    data = cities_rank_geral |> filter(ordem==3),
    aes(label = rank),
    hjust = 0,
    nudge_x = 0.1,
    size = 5,
    fontface = 'bold'
  ) +
  annotate(
    'text',
    x = c(1, 2, 3),
    y = c(0.25, 0.25, 0.25),
    label = c("Accessibility", "Commuting time","Traffic Speed"),
    hjust = c(0, 0.5, 1),
    vjust = 1,
    size = 4,
    fontface = 'bold',
    color = description_color
  ) +
  scale_y_reverse(position = 'right', breaks = seq(16, 2, -2)) +
  scale_color_manual(values = cities_colors) +
  coord_cartesian(xlim = c(0.5, 3.5), ylim = c(10, 0), expand = F) +
  theme_void() +
  theme(
    legend.position = 'none',
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.subtitle = element_text(
      margin = margin(t = 3, b = 2, unit = 'mm')
    ),
    plot.title = element_text(
      face = 'bold',
      size = 20
    )
  )
  # + labs(
  #   title = 'Congestion Ranking',
  #   subtitle = 'Congestion measured through different methods'
  #)

ggsave(filename="./figures/Fig1_English.png", plot=ggplot2::last_plot(),
       dpi = 300, width = 25, height = 15, units = "cm")
