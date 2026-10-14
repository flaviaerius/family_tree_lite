# Números da aba "Análises". Contam só as pessoas DA FAMÍLIA: os fundadores
# (a geração mais antiga da planilha) e quem descende deles. Cônjuges que
# entraram na família ficam de fora, assim como os placeholders
# (R/placeholders.R), que existem só para o desenho não ficar com ramo solto.

# Rótulo de quem não tem `nucleus` preenchido.
NO_NUCLEUS_LABEL <- "Sem núcleo"

SEX_LABELS <- c(F = "Feminino", M = "Masculino")
SEX_UNKNOWN_LABEL <- "Não informado"

generation_label <- function(g) ifelse(is.na(g), "?", paste0(g, "ª"))

# "Núcleo A" e "Ramo A" aparecem só como "A": o título do gráfico já diz que
# aquilo é um núcleo.
nucleus_label <- function(nucleus) {
  short <- trimws(sub("^\\s*(n[uú]cleo|ramo)\\s+", "", nucleus, ignore.case = TRUE))
  ifelse(is.na(nucleus), NO_NUCLEUS_LABEL, ifelse(nzchar(short), short, nucleus))
}

# TRUE para fundadores e seus descendentes. Fundador é quem está na geração
# mais antiga; dali para baixo, alguém é da família quando a mãe ou o pai é.
# Quem casou com alguém da família e não tem pais na família é cônjuge.
is_family_member <- function(family) {
  real <- if ("is_placeholder" %in% names(family)) {
    !family$is_placeholder
  } else {
    rep(TRUE, nrow(family))
  }
  root_gen <- suppressWarnings(min(family$generation[real], na.rm = TRUE))
  member <- real & !is.na(family$generation) & family$generation == root_gen

  repeat {
    names_in <- family$name[member]
    child <- real & !member &
      (family$mother_name %in% names_in | family$father_name %in% names_in)
    if (!any(child)) break
    member <- member | child
  }
  member
}

# Idade de cada pessoa em `today`. Quem não tem `death_year` conta como vivo e
# recebe a idade atual; quem tem recebe a idade com que faleceu. Com
# `birth_date` a idade atual desconta o aniversário que ainda não chegou; só com
# o ano, é a diferença de anos.
person_ages <- function(family, today = Sys.Date()) {
  this_year <- as.integer(format(today, "%Y"))
  alive <- is.na(family$death_year)
  age <- ifelse(
    alive,
    this_year - family$birth_year,
    family$death_year - family$birth_year
  )

  has_date <- alive & !is.na(family$birth_date)
  if (any(has_date)) {
    before_bday <- format(family$birth_date[has_date], "%m-%d") > format(today, "%m-%d")
    age[has_date][before_bday] <- age[has_date][before_bday] - 1L
  }
  age[!is.na(age) & age < 0] <- NA

  data.frame(alive = alive, age = as.integer(age))
}

# Ordem dos núcleos nos gráficos: de onde a família começa para baixo (menor
# geração primeiro), e em ordem alfabética dentro da mesma geração.
nucleus_order <- function(people) {
  first_gen <- tapply(people$generation, people$nucleus_label, function(g) {
    if (all(is.na(g))) Inf else min(g, na.rm = TRUE)
  })
  nms <- names(first_gen)
  nms[order(first_gen, nms)]
}

safe_mean <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else mean(x)
}

family_stats <- function(family, today = Sys.Date()) {
  members <- family[is_family_member(family), , drop = FALSE]

  ages <- person_ages(members, today)
  people <- data.frame(
    name = members$name,
    sex = ifelse(
      !is.na(members$sex) & members$sex %in% names(SEX_LABELS),
      members$sex,
      NA_character_
    ),
    generation = members$generation,
    nucleus = members$nucleus,
    nucleus_label = nucleus_label(members$nucleus),
    alive = ages$alive,
    age = ages$age,
    stringsAsFactors = FALSE
  )

  living_age <- people$age[people$alive]
  death_age <- people$age[!people$alive]

  overview <- list(
    n = nrow(people),
    n_nuclei = length(unique(people$nucleus_label)),
    n_generations = length(unique(stats::na.omit(people$generation))),
    n_f = sum(people$sex %in% "F"),
    n_m = sum(people$sex %in% "M"),
    n_sex_unknown = sum(is.na(people$sex)),
    n_alive = sum(people$alive),
    n_deceased = sum(!people$alive),
    mean_age_alive = safe_mean(living_age),
    n_age_alive = sum(!is.na(living_age)),
    mean_age_death = safe_mean(death_age),
    n_age_death = sum(!is.na(death_age))
  )

  by_generation <- people |>
    dplyr::filter(!is.na(generation)) |>
    dplyr::group_by(generation) |>
    dplyr::summarise(
      n = dplyr::n(),
      n_f = sum(sex %in% "F"),
      n_m = sum(sex %in% "M"),
      n_sex_unknown = sum(is.na(sex)),
      .groups = "drop"
    ) |>
    dplyr::arrange(generation)

  by_nucleus <- people |>
    dplyr::group_by(nucleus_label) |>
    dplyr::summarise(
      nucleus = nucleus[1],
      n = dplyr::n(),
      n_f = sum(sex %in% "F"),
      n_m = sum(sex %in% "M"),
      n_sex_unknown = sum(is.na(sex)),
      n_generations = length(unique(stats::na.omit(generation))),
      gen_min = suppressWarnings(min(generation, na.rm = TRUE)),
      gen_max = suppressWarnings(max(generation, na.rm = TRUE)),
      # Idades só de quem está vivo: idade atual de quem já faleceu não existe.
      age_min = suppressWarnings(min(age[alive], na.rm = TRUE)),
      age_max = suppressWarnings(max(age[alive], na.rm = TRUE)),
      age_mean = safe_mean(age[alive]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      gen_min = ifelse(is.finite(gen_min), gen_min, NA_integer_),
      gen_max = ifelse(is.finite(gen_max), gen_max, NA_integer_),
      age_min = ifelse(is.finite(age_min), age_min, NA_integer_),
      age_max = ifelse(is.finite(age_max), age_max, NA_integer_),
      nucleus_label = factor(nucleus_label, levels = nucleus_order(people))
    ) |>
    dplyr::arrange(nucleus_label) |>
    dplyr::mutate(nucleus_label = as.character(nucleus_label))

  real <- if ("is_placeholder" %in% names(family)) {
    family[!family$is_placeholder, , drop = FALSE]
  } else {
    family
  }

  list(
    people = people,
    overview = overview,
    by_generation = by_generation,
    by_nucleus = by_nucleus,
    # Mesmas cores da árvore e do cartão (ver nucleus_color_map em R/colors.R),
    # por isso o mapa sai de todo mundo que a árvore desenha, cônjuges inclusive.
    nucleus_colors = nucleus_color_map(real$nucleus)
  )
}

nucleus_colors_for <- function(stats, nucleus) {
  vapply(
    nucleus,
    function(nuc) nucleus_color_for(stats$nucleus_colors, nuc),
    character(1),
    USE.NAMES = FALSE
  )
}

format_num <- function(x, digits = 1) {
  ifelse(
    is.na(x),
    "—",
    formatC(x, format = "f", digits = digits, decimal.mark = ",")
  )
}

format_pct <- function(part, total, digits = 0) {
  ifelse(total > 0, paste0(format_num(100 * part / total, digits), "%"), "—")
}
