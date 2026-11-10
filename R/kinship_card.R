# Cartão de resultado da aba "Parentesco": a frase nos dois sentidos, e embaixo
# o ancestral comum, o grau civil e o caminho de uma pessoa até a outra. O
# cálculo mora em R/kinship.R.

# Placeholder anônimo não tem nome para mostrar.
kin_display_name <- function(name) {
  if (is_anonymous_placeholder(name)) "sem registro" else name
}

join_names <- function(x) {
  if (length(x) <= 1) {
    return(paste(x, collapse = ""))
  }
  paste(paste(x[-length(x)], collapse = ", "), "e", x[length(x)])
}

# "Maria é tia-avó de João, por parte de pai." O termo vai na cor do sexo de A.
kinship_sentence <- function(family, a, b, rel, class = NULL) {
  color <- SEX_COLORS[[if (kin_sex(family, a) %in% c("F", "M")) kin_sex(family, a) else "unknown"]]
  # `.noWS`: sem ele o htmltools quebra a linha entre o nome e a pontuação, e
  # o navegador mostra "João ." com espaço.
  if (rel$kind == "none") {
    return(tags$p(
      class = class,
      "Não há parentesco registrado na planilha entre ", tags$b(a), " e ",
      tags$b(b, .noWS = "after"), "."
    ))
  }
  tags$p(
    class = class,
    tags$b(a), " é ",
    tags$span(style = paste0("color:", color, ";font-weight:700;"), rel$term),
    " de ", tags$b(b, .noWS = "after"),
    paste0(if (!is.na(rel$side)) paste0(", por parte de ", rel$side), ".")
  )
}

KIN_EDGE_ICONS <- list(
  up = list(icon = "arrow-up", title = "pai/mãe de quem vem antes"),
  down = list(icon = "arrow-down", title = "filho(a) de quem vem antes"),
  spouse = list(icon = "ring", title = "cônjuge de quem vem antes")
)

kinship_path_ui <- function(path) {
  if (is.null(path) || nrow(path) < 2) {
    return(NULL)
  }
  items <- list()
  for (i in seq_len(nrow(path))) {
    if (!is.na(path$edge[i])) {
      e <- KIN_EDGE_ICONS[[path$edge[i]]]
      items[[length(items) + 1]] <- tags$span(
        class = "text-muted", title = e$title, icon(e$icon)
      )
    }
    items[[length(items) + 1]] <- tags$span(
      class = "badge rounded-pill text-bg-light border fw-normal",
      style = "font-size:.9rem;",
      kin_display_name(path$name[i])
    )
  }
  tags$div(class = "d-flex flex-wrap align-items-center gap-2", items)
}

kinship_card_ui <- function(family, a, b) {
  if (identical(a, b)) {
    return(bslib::card(bslib::card_body(
      class = "text-center text-muted py-4",
      "As duas caixas estão com a mesma pessoa. Escolha outra para comparar."
    )))
  }
  ab <- relationship(family, a, b)
  ba <- relationship(family, b, a)

  details <- list()
  if (length(ab$common) > 0) {
    details[[length(details) + 1]] <- tags$div(
      tags$span(class = "text-muted", "Ancestrais em comum: "),
      join_names(unique(vapply(ab$common, kin_display_name, character(1))))
    )
  }
  if (!is.na(ab$degree)) {
    details[[length(details) + 1]] <- tags$div(
      tags$span(class = "text-muted", "Pelo Código Civil: "),
      "parentesco ", ab$degree
    )
  }
  if (ab$kind == "affinity") {
    details[[length(details) + 1]] <- tags$div(
      class = "text-muted",
      "Parentesco por casamento: não há ancestral em comum."
    )
  }

  bslib::card(
    bslib::card_body(
      kinship_sentence(family, a, b, ab, class = "fs-4 mb-1"),
      if (ba$kind != "none") kinship_sentence(family, b, a, ba, class = "fs-6 text-muted mb-2"),
      if (length(details) > 0) tags$div(class = "small d-grid gap-1 mb-2", details),
      if (!is.null(ab$path)) {
        tags$div(
          tags$h6(class = "mt-2 mb-2", style = "color:#566573;font-weight:600;", "Caminho na árvore"),
          kinship_path_ui(ab$path)
        )
      }
    )
  )
}
