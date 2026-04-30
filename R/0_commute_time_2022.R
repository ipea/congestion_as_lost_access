library(sidrar)
library(dplyr)
library(ibger)
library(aopdata)

df_aop <- aopdata::read_population(city = "all", year = 2010)
munis_aop <- df_aop$code_muni |> unique()

# ibge_explorer(launch.browser = TRUE)


tabela <- 10330

df_localities <- ibger::ibge_localities(aggregate = tabela)
head(df_localities)

df_meta <- ibger::ibge_metadata(aggregate = tabela)

df_variables <- ibger::ibge_variables(aggregate = tabela)




# > df_meta[8]
# $variables
# # A tibble: 2 × 3
# id      name                                                                                             unit
# <chr>   <chr>                                                                                            <chr>
#   1 13376   Pessoas de 10 anos ou mais de idade, ocupadas na semana de referência, que, no trabalho princip… Pess…

# > df_meta[9]
# $classifications
# id    name                                                                        categories
# <chr> <chr>                                                                       <list>
#   1 537   Tempo habitual de deslocamento do domicílio para o trabalho principal       <tibble [8 × 4]>
#   2 2088  Meio de transporte em que passa mais tempo para chegar ao local de trabalho <tibble [15 × 4]>
#   3 86    Cor ou raça                                                                 <tibble [6 × 4]>
#   4 469   Local de exercício do trabalho principal                                    <tibble [4 × 4]>

# modos de transporte
df_meta[9]$classifications$categories


# commute time by transport mode
df_raw <- ibge_variables(
  aggregate      = tabela,
  variable       = 13376,
  periods        = 2022,
  localities     = list(N6 = munis_aop),
  classification = list(
    "537" = "all", # todos tempos de viagem
    "2088" = 79197 # somente viagens de automovel
  )
)


# drop total rows
df <- df_raw |>
  filter(classification_537 != "Total")

# select and rename columns
df <- df |>
  select(commute_time_d = classification_537,
         mode = classification_2088,
         code_muni = locality_id,
         name_muni = locality_name,
         pop = value
  )

# recode commute time to continuous variable
df <- df |>
  mutate( pop = as.numeric(pop)) |>
  mutate(commute_time_c = case_when(
    commute_time_d == "Até cinco minutos"                    ~ 2.5,
    commute_time_d == "De seis minutos até quinze minutos"   ~ 10.5,
    commute_time_d == "Mais de quinze minutos até meia hora" ~ 23,
    commute_time_d == "Mais de meia hora até uma hora"       ~ 45.5,
    commute_time_d == "Mais de uma hora até duas horas"      ~ 90.5,
    commute_time_d == "Mais de duas horas até quatro horas"  ~ 180.5,
    commute_time_d == "Mais de quatro horas"                 ~ 240
  )
  )

# replace Na with 0
summary(df$pop)
df <- df |>
  mutate(pop = ifelse(is.na(pop), 0, pop))

# average commuting time by car
output_avg <- df |>
  group_by(code_muni, name_muni, mode) |>
  summarise(
    avg_commute_time = weighted.mean(x = commute_time_c, w = pop) |> round(1)
  ) |>
  arrange(-avg_commute_time)

output_avg

data.table::fwrite(output_avg, "./data/resultado_censo2022.csv")
# a2010 <- data.table::fread("./data/resultado_censo2010.csv")



# # proportion of commuting trips between 15 and 45 minutes
# output_interval <- df |>
#   mutate(
#     in_15to45 = ifelse(dplyr::between(commute_time_c, 23, 45.5),1,0),
#     in_below15 = ifelse(commute_time_c < 15,1,0),
#   ) |>
#   group_by(code_muni, name_muni, mode) |>
#   summarise(
#     pop_total = sum(pop),
#     prop_15to45 = sum(pop[which(in_15to45==1)]) / pop_total,
#     prop_below15 = sum(pop[which(in_below15==1)]) / pop_total
#   ) |>
#   arrange(-prop_15to45)
#
# output_interval
# View(output_interval)


