# Normaliza os co-genitores ausentes ANTES do layout ser montado.
#
# Um ramo só consegue ser ligado ao pai/mãe porque o barramento dos filhos é
# ancorado no ponto médio do casal (ver build_overview_edges). Quando o outro
# genitor não tem linha na tabela, não há ponto médio e o ramo fica solto no
# desenho. A solução é criar a linha que falta: a partir daqui todo genitor
# citado existe de verdade, e todo o resto do pipeline (compute_width,
# groups_of, place_partner, build_overview_edges e o modal focado) funciona sem
# saber que essa pessoa é um placeholder.
#
# São três origens, todas marcadas com `is_placeholder = TRUE`:
#
#   1. Anônimo explícito — a convenção `placeholder_1`, `placeholder_2`... usada
#      na planilha para dizer que dois filhos têm a MESMA mãe desconhecida, e que
#      ela é diferente da mãe de um terceiro. Célula vazia não expressa isso.
#   2. Nomeado sem ficha — nome citado em mother/father/partner_name que ainda
#      não virou linha própria (Cristiane, Verter Calza, Ana, Fávaro).
#   3. Anônimo automático — o outro genitor ainda está vazio. Rede de segurança
#      para a planilha crescer sem voltar a produzir ramo solto; assim que o
#      nome real ou um `placeholder_N` for escrito, o tipo 1 ou 2 assume.

ANON_PLACEHOLDER_PREFIX <- "__sem_registro__"

# `placeholder`, `placeholder_2`, `Placeholder 3`... e os anônimos automáticos.
is_anonymous_placeholder <- function(name) {
  !is.na(name) &
    (grepl("^placeholder[_ ]?[0-9]*$", trimws(name), ignore.case = TRUE) |
      startsWith(name, ANON_PLACEHOLDER_PREFIX))
}

opposite_sex <- function(sex) {
  if (length(sex) != 1L || is.na(sex)) {
    return(NA_character_)
  }
  switch(sex, M = "F", F = "M", NA_character_)
}

add_placeholder_parents <- function(family) {
  if (!("is_placeholder" %in% names(family))) family$is_placeholder <- FALSE
  if (nrow(family) == 0) {
    return(family)
  }

  # O layout parte da geração 1 e só desce, então os pais de quem está na
  # geração-raiz nunca seriam posicionados: virariam linhas invisíveis.
  root_gen <- suppressWarnings(min(family$generation, na.rm = TRUE))
  if (!is.finite(root_gen)) root_gen <- 1L

  new_rows <- list()

  blank_row <- function() {
    r <- family[1, , drop = FALSE]
    for (col in names(r)) r[[col]][1] <- NA
    r
  }

  add_row <- function(name, sex, generation, partner_name = NA_character_) {
    if (name %in% family$name || !is.null(new_rows[[name]])) {
      return(invisible())
    }
    r <- blank_row()
    r$name <- name
    r$sex <- sex
    r$generation <- if (is.na(generation)) NA_integer_ else as.integer(generation)
    r$short_name <- if (is_anonymous_placeholder(name)) {
      "?"
    } else {
      strsplit(trimws(name), " ")[[1]][1]
    }
    r$partner_name <- partner_name
    r$is_placeholder <- TRUE
    new_rows[[name]] <<- r
    invisible()
  }

  has_ref <- function(x) !is.na(x) && nzchar(trimws(x))

  # --- Tipos 1 e 2: nomes citados que não existem na tabela ---------------
  # Cônjuges primeiro: a coluna partner_name diz o sexo do placeholder com mais
  # confiança do que mother/father_name, que na planilha aparecem trocados em
  # alguns registros.
  for (i in seq_len(nrow(family))) {
    ref <- family$partner_name[i]
    if (!has_ref(ref) || ref %in% family$name) next
    add_row(
      ref,
      opposite_sex(family$sex[i]),
      family$generation[i],
      partner_name = family$name[i]
    )
  }

  for (i in seq_len(nrow(family))) {
    gen <- family$generation[i]
    if (!is.na(gen) && gen <= root_gen) next # pais da raiz não são desenhados
    for (col in c("mother_name", "father_name")) {
      ref <- family[[col]][i]
      if (!has_ref(ref) || ref %in% family$name) next
      add_row(
        ref,
        if (col == "mother_name") "F" else "M",
        if (is.na(gen)) NA_integer_ else gen - 1L
      )
    }
  }

  # --- Tipo 3: o outro genitor ainda está vazio --------------------------
  # Um placeholder por pai/mãe, que é como build_overview_edges já agrupa hoje
  # esses filhos (todos sob a mesma chave "<NA>").
  for (i in seq_len(nrow(family))) {
    nm <- family$name[i]
    is_mother <- !is.na(family$mother_name) & family$mother_name == nm
    is_father <- !is.na(family$father_name) & family$father_name == nm
    kids <- which(is_mother | is_father)
    if (length(kids) == 0) next

    orphaned <- kids[vapply(
      kids,
      function(ci) {
        if (is_mother[ci]) is.na(family$father_name[ci]) else is.na(family$mother_name[ci])
      },
      logical(1)
    )]
    if (length(orphaned) == 0) next

    anon <- paste0(ANON_PLACEHOLDER_PREFIX, nm)
    add_row(anon, opposite_sex(family$sex[i]), family$generation[i])
    for (ci in orphaned) {
      if (is_mother[ci]) {
        family$father_name[ci] <- anon
      } else {
        family$mother_name[ci] <- anon
      }
    }
  }

  if (length(new_rows) > 0) {
    family <- dplyr::bind_rows(c(list(family), unname(new_rows)))
  }
  family
}
