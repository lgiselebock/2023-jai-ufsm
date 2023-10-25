

library(ggplot2)

pop_regiao_00_10 <- readxl::read_excel(
  "dados/censo/raw/pop_regiao_2000_2010-raw.xlsx",
  skip = 4
) |>
  janitor::clean_names() |>
  dplyr::rename(
    "regiao_uf" = "grande_regiao_e_unidade_da_federacao",
    "valor" = "x4"
  ) |>
  # complete missing values with previous value
  tidyr::fill(
    regiao_uf,
    grupo_de_idade,
    .direction = "down"
  ) |>
  dplyr::mutate(
    ano = as.integer(ano),
    valor = as.numeric(valor)
  ) |>
  dplyr::filter(
    !regiao_uf %in% c(
      "Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
  )


map_br <- geobr::read_state(year = 2000) |>
  dplyr::mutate(
    name_state = dplyr::case_when(
      name_state == "Rio Grande Do Norte" ~ "Rio Grande do Norte",
      name_state == "Rio Grande Do Sul" ~ "Rio Grande do Sul",
      name_state == "Sao Paulo" ~ "São Paulo",
      name_state == "Mato Grosso Do Sul" ~ "Mato Grosso do Sul",
      name_state == "Parana" ~ "Paraná",
      name_state == "Maranhao" ~ "Maranhão",
      name_state == "Rio De Janeiro" ~ "Rio de Janeiro",
      TRUE ~ name_state)
  )

pop_regiao_00_10 |>
  dplyr::filter(
    ano == 2000,
    !regiao_uf %in% c(
      "Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
    ) |>
  dplyr::left_join(map_br, by = c("regiao_uf" = "name_state")) |>
  ggplot(aes(geometry = geom)) +
  geom_sf(aes(fill = valor))
