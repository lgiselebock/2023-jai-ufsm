

options(scipen = 999)

# PACOTES NECESSARIOS -----------------------------------------------------

library(ggplot2)


# IMPORTACAO DAS BASES ----------------------------------------------------

populacao_censo_2000 <- readr::read_csv(
  here::here("dados", "censo", "2000-populacao-brasil.csv")
)

populacao_censo_2010 <- readr::read_csv(
  here::here("dados", "censo", "2010-populacao-brasil.csv")
)

populacao_projecao_2020_2060 <- readr::read_csv(
  here::here("dados", "projecao", "populacao_projecao_2020_2060.csv")
)


# TIDY DATA ---------------------------------------------------------------

razao_dependencia_2000 <- populacao_censo_2000 |>
  dplyr::filter(
    grupo_de_idade != "Total"
  ) |>
  dplyr::select(
    grupo_de_idade,
    total
  ) |>
  dplyr::mutate(
    id = dplyr::case_when(
      grupo_de_idade == "00 a 04 anos" ~ "jovem",
      grupo_de_idade == "05 a 09 anos" ~ "jovem",
      grupo_de_idade == "10 a 14 anos" ~ "jovem",
      grupo_de_idade == "15 a 19 anos" ~ "adulto",
      grupo_de_idade == "20 a 24 anos" ~ "adulto",
      grupo_de_idade == "25 a 29 anos" ~ "adulto",
      grupo_de_idade == "30 a 34 anos" ~ "adulto",
      grupo_de_idade == "35 a 39 anos" ~ "adulto",
      grupo_de_idade == "40 a 44 anos" ~ "adulto",
      grupo_de_idade == "45 a 49 anos" ~ "adulto",
      grupo_de_idade == "50 a 54 anos" ~ "adulto",
      grupo_de_idade == "55 a 59 anos" ~ "adulto",
      grupo_de_idade == "60 a 64 anos" ~ "adulto",
      grupo_de_idade == "65 a 69 anos" ~ "idoso",
      grupo_de_idade == "70 a 74 anos" ~ "idoso",
      grupo_de_idade == "75 a 79 anos" ~ "idoso",
      grupo_de_idade == "80 a 84 anos" ~ "idoso",
      grupo_de_idade == "85 a 89 anos" ~ "idoso",
      grupo_de_idade == "90 a 94 anos" ~ "idoso",
      grupo_de_idade == "95 a 99 anos" ~ "idoso",
      grupo_de_idade == "100 anos ou mais" ~ "idoso"
    )
  ) |>
  dplyr::group_by(id) |>
  dplyr::summarise(
    n_00 = sum(total)
  ) |>
  dplyr::ungroup()


razao_dependencia_2010 <- populacao_censo_2010 |>
  dplyr::filter(
    grupo_de_idade != "Total"
  ) |>
  dplyr::select(
    grupo_de_idade,
    total
  ) |>
  dplyr::mutate(
    id = dplyr::case_when(
      grupo_de_idade == "00 a 04 anos" ~ "jovem",
      grupo_de_idade == "05 a 09 anos" ~ "jovem",
      grupo_de_idade == "10 a 14 anos" ~ "jovem",
      grupo_de_idade == "15 a 19 anos" ~ "adulto",
      grupo_de_idade == "20 a 24 anos" ~ "adulto",
      grupo_de_idade == "25 a 29 anos" ~ "adulto",
      grupo_de_idade == "30 a 34 anos" ~ "adulto",
      grupo_de_idade == "35 a 39 anos" ~ "adulto",
      grupo_de_idade == "40 a 44 anos" ~ "adulto",
      grupo_de_idade == "45 a 49 anos" ~ "adulto",
      grupo_de_idade == "50 a 54 anos" ~ "adulto",
      grupo_de_idade == "55 a 59 anos" ~ "adulto",
      grupo_de_idade == "60 a 64 anos" ~ "adulto",
      grupo_de_idade == "65 a 69 anos" ~ "idoso",
      grupo_de_idade == "70 a 74 anos" ~ "idoso",
      grupo_de_idade == "75 a 79 anos" ~ "idoso",
      grupo_de_idade == "80 a 84 anos" ~ "idoso",
      grupo_de_idade == "85 a 89 anos" ~ "idoso",
      grupo_de_idade == "90 a 94 anos" ~ "idoso",
      grupo_de_idade == "95 a 99 anos" ~ "idoso",
      grupo_de_idade == "100 anos ou mais" ~ "idoso"
    )
  ) |>
  dplyr::group_by(id) |>
  dplyr::summarise(
    n_10 = sum(total)
  ) |>
  dplyr::ungroup()


razao_dependencia_2020_2060 <- populacao_projecao_2020_2060 |>
  dplyr::filter(
    grupo_de_idade != "Total"
  ) |>
  dplyr::select(
    grupo_de_idade,
    tidyr::starts_with("total")
  ) |>
  dplyr::mutate(
    id = dplyr::case_when(
      grupo_de_idade == "00 a 04 anos" ~ "jovem",
      grupo_de_idade == "05 a 09 anos" ~ "jovem",
      grupo_de_idade == "10 a 14 anos" ~ "jovem",
      grupo_de_idade == "15 a 19 anos" ~ "adulto",
      grupo_de_idade == "20 a 24 anos" ~ "adulto",
      grupo_de_idade == "25 a 29 anos" ~ "adulto",
      grupo_de_idade == "30 a 34 anos" ~ "adulto",
      grupo_de_idade == "35 a 39 anos" ~ "adulto",
      grupo_de_idade == "40 a 44 anos" ~ "adulto",
      grupo_de_idade == "45 a 49 anos" ~ "adulto",
      grupo_de_idade == "50 a 54 anos" ~ "adulto",
      grupo_de_idade == "55 a 59 anos" ~ "adulto",
      grupo_de_idade == "60 a 64 anos" ~ "adulto",
      grupo_de_idade == "65 a 69 anos" ~ "idoso",
      grupo_de_idade == "70 a 74 anos" ~ "idoso",
      grupo_de_idade == "75 a 79 anos" ~ "idoso",
      grupo_de_idade == "80 a 84 anos" ~ "idoso",
      grupo_de_idade == "85 a 89 anos" ~ "idoso",
      grupo_de_idade == "90 anos ou mais" ~ "idoso"
    )
  ) |>
  dplyr::group_by(id) |>
  dplyr::summarise(
    n_20 = sum(total_2020),
    n_30 = sum(total_2030),
    n_40 = sum(total_2040),
    n_50 = sum(total_2050),
    n_60 = sum(total_2060)
  ) |>
  dplyr::ungroup()


razao_dependencia <- dplyr::right_join(
  razao_dependencia_2000,
  razao_dependencia_2010,
  by = c("id")
) |>
  dplyr::right_join(
    razao_dependencia_2020_2060,
    by = c("id")
  )

readr::write_csv(
  razao_dependencia,
  here::here("dados", "razao-dependencia.csv")
)
