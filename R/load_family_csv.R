CSV_REQUIRED_COLS <- c("name", "sex", "generation", "nucleus", "birth_year")
CSV_OPTIONAL_COLS <- c(
  "short_name",
  "birth_date",
  "death_year",
  "mother_name",
  "father_name",
  "partner_name",
  "image_file"
)

# `birth_date` é opcional e serve ao cartão da barra lateral, que mostra o dia do
# aniversário quando ele existe. Aceita AAAA-MM-DD e DD/MM/AAAA; o que não casar
# com nenhum formato vira NA e a pessoa continua sendo mostrada só pelo ano.
parse_birth_date <- function(x) {
  x <- trimws(as.character(x))
  out <- as.Date(rep(NA_character_, length(x)))
  pending <- !is.na(x) & nzchar(x)
  for (fmt in c("%Y-%m-%d", "%d/%m/%Y", "%Y/%m/%d", "%d-%m-%Y")) {
    if (!any(pending)) break
    out[pending] <- as.Date(x[pending], format = fmt)
    pending <- pending & is.na(out)
  }
  out
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
  raw$birth_year <- suppressWarnings(as.integer(raw$birth_year))
  raw$death_year <- suppressWarnings(as.integer(raw$death_year))

  raw$birth_date <- parse_birth_date(raw$birth_date)
  derive_year <- is.na(raw$birth_year) & !is.na(raw$birth_date)
  if (any(derive_year)) {
    raw$birth_year[derive_year] <- as.integer(
      format(raw$birth_date[derive_year], "%Y")
    )
  }

  dup <- raw$name[duplicated(raw$name)]
  if (length(dup) > 0) {
    stop("Nomes completos duplicados: ", paste(unique(dup), collapse = ", "))
  }

  # Cria as linhas dos co-genitores citados mas ausentes, para que nenhum ramo
  # fique solto no desenho (ver R/placeholders.R).
  add_placeholder_parents(tibble::as_tibble(raw))
}
