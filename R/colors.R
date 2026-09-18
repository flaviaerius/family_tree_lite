NUCLEI_PALETTE <- list(
  Maca = list(primary = "#C0392B", light = "#F5B7B1", dark = "#7B241C"),
  Uva  = list(primary = "#7D3C98", light = "#D2B4DE", dark = "#4A235A"),
  Pera = list(primary = "#27AE60", light = "#A9DFBF", dark = "#145A32")
)

NEUTRAL <- list(primary = "#566573", light = "#D5DBDB", dark = "#212F3D")

nucleus_color <- function(nucleus, variant = "primary") {
  pal <- if (is.na(nucleus) || is.null(nucleus) || !(nucleus %in% names(NUCLEI_PALETTE))) {
    NEUTRAL
  } else {
    NUCLEI_PALETTE[[nucleus]]
  }
  pal[[variant]]
}

nucleus_color_vec <- function(nuclei, variant = "primary") {
  vapply(nuclei, nucleus_color, character(1), variant = variant, USE.NAMES = FALSE)
}

# Distinct colors for nuclei that are not in NUCLEI_PALETTE (e.g. came from a
# user CSV), so each branch still gets its own color instead of all-grey.
NUCLEI_FALLBACK_COLORS <- c("#2980B9", "#E67E22", "#16A085", "#D35400",
                            "#8E44AD", "#1ABC9C", "#34495E", "#CB4335")

# Border color for an ordered vector of nuclei (NA -> neutral).
nucleus_border_colors <- function(nuclei) {
  fallback_i <- 0L
  vapply(nuclei, function(nuc) {
    if (is.na(nuc)) {
      NEUTRAL$dark
    } else if (nuc %in% names(NUCLEI_PALETTE)) {
      NUCLEI_PALETTE[[nuc]]$dark
    } else {
      fallback_i <<- fallback_i + 1L
      NUCLEI_FALLBACK_COLORS[((fallback_i - 1L) %% length(NUCLEI_FALLBACK_COLORS)) + 1L]
    }
  }, character(1), USE.NAMES = FALSE)
}

# Mapa nome-do-núcleo -> cor, construído na MESMA ordem que render_overview
# usa ao montar os traces (não-NA em ordem alfabética, NA por último). Sem isso,
# as cores de fallback — atribuídas por posição — sairiam diferentes no gráfico
# e no cartão da barra lateral.
nucleus_color_map <- function(nuclei) {
  levels <- unique(nuclei)
  levels <- c(sort(levels[!is.na(levels)]), if (anyNA(levels)) NA)
  stats::setNames(
    nucleus_border_colors(levels),
    ifelse(is.na(levels), "NA", levels)
  )
}

nucleus_color_for <- function(map, nucleus) {
  key <- if (length(nucleus) != 1L || is.na(nucleus)) "NA" else nucleus
  if (key %in% names(map)) unname(map[[key]]) else NEUTRAL$dark
}
