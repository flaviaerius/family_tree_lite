CSV_REQUIRED_COLS <- c("name", "sex", "nucleus", "birth_date")
# `generation` vazia (ou a coluna toda ausente) é calculada pela árvore: ver
# R/generations.R.
CSV_OPTIONAL_COLS <- c(
  "generation",
  "short_name",
  "death_date",
  "mother_name",
  "father_name",
  "partner_name",
  "image_file"
)

# `birth_date` e `death_date` aceitam a data completa (DD/MM/AAAA) ou só o ano
# (AAAA). Devolve o ano sempre que dá para tirá-lo e a data só quando ela veio
# completa; o que não casar com nenhum formato vira NA nos dois.
parse_partial_date <- function(x) {
  x <- trimws(as.character(x))
  date <- as.Date(rep(NA_character_, length(x)))
  pending <- !is.na(x) & nzchar(x)
  for (fmt in c("%d/%m/%Y", "%Y-%m-%d")) {
    if (!any(pending)) break
    date[pending] <- as.Date(x[pending], format = fmt)
    pending <- pending & is.na(date)
  }
  year <- as.integer(format(date, "%Y"))
  year_only <- pending & grepl("^[0-9]{4}$", x)
  year[year_only] <- as.integer(x[year_only])
  list(date = date, year = year)
}

load_family_csv <- function(path) {
  raw <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    encoding = "UTF-8",
    na.strings = c("", "NA")
  )
  for (col in names(raw)) {
    if (is.character(raw[[col]])) Encoding(raw[[col]]) <- "UTF-8"
  }

  missing_cols <- setdiff(CSV_REQUIRED_COLS, names(raw))
  if (length(missing_cols) > 0) {
    stop(
      "Colunas obrigatórias em falta: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  for (col in CSV_OPTIONAL_COLS) {
    if (!(col %in% names(raw))) raw[[col]] <- NA
  }

  empty_short <- is.na(raw$short_name) | raw$short_name == ""
  if (any(empty_short)) {
    raw$short_name[empty_short] <- vapply(
      strsplit(raw$name[empty_short], " "),
      `[`,
      character(1),
      1
    )
  }

  raw$generation <- suppressWarnings(as.integer(raw$generation))

  # O resto do app ordena e rotula pelo ano; a data completa fica para o
  # aniversário e para a idade exata.
  birth <- parse_partial_date(raw$birth_date)
  death <- parse_partial_date(raw$death_date)
  raw$birth_date <- birth$date
  raw$birth_year <- birth$year
  raw$death_date <- death$date
  raw$death_year <- death$year

  dup <- raw$name[duplicated(raw$name)]
  if (length(dup) > 0) {
    stop("Nomes completos duplicados: ", paste(unique(dup), collapse = ", "))
  }

  # Antes dos placeholders: eles herdam a geração de quem os cita.
  gens <- fill_generations(raw)

  # Cria as linhas dos co-genitores citados mas ausentes, para que nenhum ramo
  # fique solto no desenho (ver R/placeholders.R).
  family <- add_placeholder_parents(tibble::as_tibble(gens$family))
  attr(family, "generation_report") <- gens[c("filled", "divergent")]
  family
}
