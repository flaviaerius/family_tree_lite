format_dates <- function(birth_year, death_year) {
  parts <- character(0)
  if (!is.na(birth_year)) {
    parts <- c(parts, paste0("* ", birth_year)) # * = nascimento
  }
  if (!is.na(death_year)) {
    parts <- c(parts, paste0("† ", death_year)) # † = falecimento
  }
  if (length(parts) == 0) {
    return("")
  }
  paste(parts, collapse = " — ")
}

format_dates_vec <- function(birth_year, death_year) {
  mapply(format_dates, birth_year, death_year, USE.NAMES = FALSE)
}

format_dates_short <- function(birth_year, death_year) {
  if (is.na(birth_year) && is.na(death_year)) {
    return("")
  }
  b <- if (is.na(birth_year)) "?" else as.character(birth_year)
  d <- if (is.na(death_year)) "Presente" else as.character(death_year)
  paste0(b, " - ", d)
}

format_dates_short_vec <- function(birth_year, death_year) {
  mapply(format_dates_short, birth_year, death_year, USE.NAMES = FALSE)
}

person_label <- function(person, with_dates = TRUE, html = TRUE) {
  name <- person$name
  if (html) {
    name <- paste0("<b>", name, "</b>")
  }
  if (!with_dates) {
    return(name)
  }
  dates <- format_dates(person$birth_year, person$death_year)
  if (nchar(dates) == 0) {
    return(name)
  }
  paste0(name, "<br>", dates)
}

role_label_pt <- function(role, sex = NA_character_) {
  child_label <- if (!is.na(sex) && sex == "M") {
    "Filho"
  } else if (!is.na(sex) && sex == "F") {
    "Filha"
  } else {
    "Filho/a"
  }
  switch(
    role,
    focus = "",
    partner = "Cônjuge",
    mother = "Mãe",
    father = "Pai",
    child = child_label,
    child_former = child_label,
    partner_former = "Cônjuge (anterior)"
  )
}

# Nomes dos meses fixos: format(x, "%B") depende do LC_TIME da máquina e sairia
# em inglês na maioria dos ambientes.
MESES_PT <- c(
  "janeiro", "fevereiro", "março", "abril", "maio", "junho",
  "julho", "agosto", "setembro", "outubro", "novembro", "dezembro"
)

# "14 de março de 1992" quando há data completa; cai para "1992" quando só o ano
# foi preenchido, e para "" quando não há nem um nem outro.
format_birthday_pt <- function(birth_date, birth_year) {
  if (length(birth_date) == 1L && !is.na(birth_date)) {
    d <- as.Date(birth_date)
    return(paste0(
      as.integer(format(d, "%d")),
      " de ",
      MESES_PT[as.integer(format(d, "%m"))],
      " de ",
      format(d, "%Y")
    ))
  }
  if (length(birth_year) == 1L && !is.na(birth_year)) {
    return(as.character(birth_year))
  }
  ""
}
