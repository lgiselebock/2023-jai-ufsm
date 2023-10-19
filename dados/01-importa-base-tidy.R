
options(scipen = 999)

# CENSO 2000

populacao_censo_2000 <- readxl::read_excel(
  here::here("dados", "censo", "raw", "2000-populacao-brasil-raw.xls"),
  skip = 5
) |>
  janitor::clean_names() |>
  dplyr::rename(
    "grupo_de_idade" = "x1",
    "total" = "x2",
    "homens" = "x3",
    "mulheres" = "x4",
    "urbana_total" = "total_5",
    "urbana_homens" = "homens_6",
    "urbana_mulheres" = "mulheres_7",
    "rural_total" = "total_8",
    "rural_homens" = "homens_9",
    "rural_mulheres" = "mulheres_10"
  ) |>
  dplyr::filter(
    grupo_de_idade == "Total" |
      grupo_de_idade == "0 a 4 anos" |
      grupo_de_idade == "5 a 9 anos" |
      grupo_de_idade == "10 a 14 anos" |
      grupo_de_idade == "15 a 19 anos" |
      grupo_de_idade == "20 a 24 anos" |
      grupo_de_idade == "25 a 29 anos" |
      grupo_de_idade == "30 a 34 anos" |
      grupo_de_idade == "35 a 39 anos" |
      grupo_de_idade == "40 a 44 anos" |
      grupo_de_idade == "45 a 49 anos" |
      grupo_de_idade == "50 a 54 anos" |
      grupo_de_idade == "55 a 59 anos" |
      grupo_de_idade == "60 a 64 anos" |
      grupo_de_idade == "65 a 69 anos" |
      grupo_de_idade == "70 a 74 anos" |
      grupo_de_idade == "75 a 79 anos" |
      grupo_de_idade == "80 a 84 anos" |
      grupo_de_idade == "85 a 89 anos" |
      grupo_de_idade == "90 a 94 anos" |
      grupo_de_idade == "95 a 99 anos" |
      grupo_de_idade == "100 anos ou mais"
  ) |>
  dplyr::mutate(
    grupo_de_idade = dplyr::case_when(
      grupo_de_idade == "0 a 4 anos" ~ "00 a 04 anos",
      grupo_de_idade == "5 a 9 anos" ~ "05 a 09 anos",
      TRUE ~ grupo_de_idade
    )
  )


# salva novo arquivo
readr::write_csv(
  populacao_censo_2000,
  here::here("dados", "censo", "2000-populacao-brasil.csv")
)


# CENSO 2010

populacao_censo_2010 <- readxl::read_excel(
  here::here("dados", "censo", "raw", "2010-populacao-brasil-raw.xls"),
  skip = 6
) |>
  janitor::clean_names() |>
  dplyr::rename(
    "grupo_de_idade" = "x1",
    "total" = "x2",
    "homens" = "x3",
    "mulheres" = "x4",
    "urbana_total" = "total_5",
    "urbana_homens" = "homens_6",
    "urbana_mulheres" = "mulheres_7",
    "rural_total" = "total_8",
    "rural_homens" = "homens_9",
    "rural_mulheres" = "mulheres_10"
  ) |>
  dplyr::filter(
    grupo_de_idade == "Total" |
      grupo_de_idade == "0 a  4 anos" |
      grupo_de_idade == "5 a  9 anos" |
      grupo_de_idade == "10 a 14 anos" |
      grupo_de_idade == "15 a 19 anos" |
      grupo_de_idade == "20 a 24 anos" |
      grupo_de_idade == "25 a 29 anos" |
      grupo_de_idade == "30 a 34 anos" |
      grupo_de_idade == "35 a 39 anos" |
      grupo_de_idade == "40 a 44 anos" |
      grupo_de_idade == "45 a 49 anos" |
      grupo_de_idade == "50 a 54 anos" |
      grupo_de_idade == "55 a 59 anos" |
      grupo_de_idade == "60 a 64 anos" |
      grupo_de_idade == "65 a 69 anos" |
      grupo_de_idade == "70 a 74 anos" |
      grupo_de_idade == "75 a 79 anos" |
      grupo_de_idade == "80 a 84 anos" |
      grupo_de_idade == "85 a 89 anos" |
      grupo_de_idade == "90 a 94 anos" |
      grupo_de_idade == "95 a 99 anos" |
      grupo_de_idade == "100 anos ou mais"
  ) |>
  dplyr::mutate(
    grupo_de_idade = dplyr::case_when(
      grupo_de_idade == "0 a  4 anos" ~ "00 a 04 anos",
      grupo_de_idade == "5 a  9 anos" ~ "05 a 09 anos",
      TRUE ~ grupo_de_idade
    )
  )


# salva novo arquivo
readr::write_csv(
  populacao_censo_2010,
  here::here("dados", "censo", "2010-populacao-brasil.csv")
)


# PROJEÇÕES

populacao_projecao_2020_2060 <- readxl::read_excel(
  here::here(
    "dados", "projecao", "raw", "2010-2016-populacao-brasil-raw-1.xls"
  ),
  skip = 1
) |>
  janitor::clean_names() |>
  dplyr::rename(
    "grupo_de_idade" = "grupo_etario",
    "total_2020" = "x2020_2",
    "total_2030" = "x2030_3",
    "total_2040" = "x2040_4",
    "total_2050" = "x2050_5",
    "total_2060" = "x2060_6",
    "homens_2020" = "x2020_7",
    "homens_2030" = "x2030_8",
    "homens_2040" = "x2040_9",
    "homens_2050" = "x2050_10",
    "homens_2060" = "x2060_11",
    "mulheres_2020" = "x2020_12",
    "mulheres_2030" = "x2030_13",
    "mulheres_2040" = "x2040_14",
    "mulheres_2050" = "x2050_15",
    "mulheres_2060" = "x2060_16"
  )


# salva novo arquivo

readr::write_csv(
  populacao_projecao_2020_2060,
  here::here(
    "dados", "projecao", "populacao_projecao_2020_2060.csv"
  )
)


