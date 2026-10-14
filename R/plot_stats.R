# Gráficos da aba "Análises", a partir de family_stats() (R/family_stats.R).
#
# Núcleos usam as mesmas cores da árvore. Sexo tem cor fixa em todo o app:
# mulher é rosa, homem é azul (par validado para daltonismo). Textos ficam
# sempre na cor de tinta, nunca na cor da série.

STATS_INK <- "#2C3E50"
STATS_MUTED <- "#7F8C8D"
STATS_GRID <- "#E5E8E8"

SEX_COLORS <- c(F = "#D94E8F", M = "#2A78D6", unknown = "#B3B6B7")

stats_layout <- function(p, margin = list(l = 10, r = 20, t = 10, b = 10), ...) {
  p |>
    plotly::layout(
      font = list(color = STATS_INK, size = 12),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      margin = margin,
      hoverlabel = list(
        bgcolor = "white",
        bordercolor = STATS_GRID,
        font = list(color = STATS_INK)
      ),
      ...
    ) |>
    plotly::config(displayModeBar = FALSE)
}

stats_empty_plot <- function(message) {
  plotly::plot_ly(type = "scatter", mode = "markers") |>
    stats_layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(list(
        text = message,
        x = 0.5,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 13, color = STATS_MUTED)
      ))
    )
}

# Passo inteiro para eixos de contagem: sem ele o plotly marca "0,5 pessoa".
count_dtick <- function(max_value) max(1, ceiling(max_value / 5))

# Séries de sexo presentes nos dados, na ordem F, M, não informado.
sex_series <- function(n_f, n_m, n_unknown) {
  series <- list(
    list(key = "F", label = SEX_LABELS[["F"]], values = n_f),
    list(key = "M", label = SEX_LABELS[["M"]], values = n_m)
  )
  if (sum(n_unknown) > 0) {
    series[[3]] <- list(key = "unknown", label = SEX_UNKNOWN_LABEL, values = n_unknown)
  }
  series
}

plot_people_by_generation <- function(stats) {
  g <- stats$by_generation
  if (nrow(g) == 0) {
    return(stats_empty_plot("Nenhuma pessoa com geração preenchida."))
  }
  x <- generation_label(g$generation)

  p <- plotly::plot_ly()
  for (s in sex_series(g$n_f, g$n_m, g$n_sex_unknown)) {
    p <- plotly::add_trace(
      p,
      type = "bar",
      x = x,
      y = s$values,
      name = s$label,
      marker = list(color = SEX_COLORS[[s$key]], line = list(color = "white", width = 2)),
      hovertemplate = paste0("<b>%{x} geração</b><br>", s$label, ": %{y}<extra></extra>")
    )
  }

  # Total acima de cada coluna: é o número que a pergunta "quantos por geração"
  # quer, e a pilha sozinha obriga a somar de cabeça.
  totals <- lapply(seq_len(nrow(g)), function(i) {
    list(
      x = x[i],
      y = g$n[i],
      text = paste0("<b>", g$n[i], "</b>"),
      yanchor = "bottom",
      showarrow = FALSE,
      font = list(color = STATS_INK)
    )
  })

  p |>
    stats_layout(
      barmode = "stack",
      bargap = 0.4,
      xaxis = list(
        title = "Geração",
        type = "category",
        categoryorder = "array",
        categoryarray = x,
        fixedrange = TRUE
      ),
      yaxis = list(
        title = "Pessoas",
        gridcolor = STATS_GRID,
        zeroline = FALSE,
        tick0 = 0,
        dtick = count_dtick(max(g$n)),
        range = c(0, max(g$n) * 1.15),
        fixedrange = TRUE
      ),
      legend = list(orientation = "h", x = 0, y = 1.12, traceorder = "normal"),
      annotations = totals
    )
}

plot_people_by_nucleus <- function(stats) {
  b <- stats$by_nucleus
  plotly::plot_ly(
    type = "bar",
    orientation = "h",
    x = b$n,
    y = b$nucleus_label,
    marker = list(color = nucleus_colors_for(stats, b$nucleus)),
    text = b$n,
    textposition = "outside",
    textfont = list(color = STATS_INK),
    cliponaxis = FALSE,
    hovertemplate = "<b>%{y}</b><br>%{x} pessoas<extra></extra>"
  ) |>
    stats_layout(
      bargap = 0.35,
      showlegend = FALSE,
      xaxis = list(
        title = "Pessoas",
        gridcolor = STATS_GRID,
        zeroline = FALSE,
        tick0 = 0,
        dtick = count_dtick(max(b$n)),
        range = c(0, max(b$n) * 1.15),
        fixedrange = TRUE
      ),
      yaxis = list(
        title = "",
        type = "category",
        ticksuffix = "  ",
        categoryorder = "array",
        categoryarray = rev(b$nucleus_label),
        fixedrange = TRUE
      )
    )
}

# Barra flutuante da primeira à última geração de cada núcleo: mostra quantas
# gerações o núcleo tem e também onde ele se encaixa em relação aos outros.
plot_generations_by_nucleus <- function(stats) {
  b <- stats$by_nucleus[!is.na(stats$by_nucleus$gen_min), , drop = FALSE]
  if (nrow(b) == 0) {
    return(stats_empty_plot("Nenhuma pessoa com geração preenchida."))
  }
  gens <- seq(min(b$gen_min), max(b$gen_max))
  label <- paste(b$n_generations, ifelse(b$n_generations == 1, "geração", "gerações"))
  span <- ifelse(
    b$gen_min == b$gen_max,
    paste0("só na ", generation_label(b$gen_min)),
    paste0("da ", generation_label(b$gen_min), " à ", generation_label(b$gen_max))
  )

  plotly::plot_ly(
    type = "bar",
    orientation = "h",
    base = b$gen_min - 0.4,
    x = b$gen_max - b$gen_min + 0.8,
    y = b$nucleus_label,
    marker = list(color = nucleus_colors_for(stats, b$nucleus)),
    text = label,
    textposition = "outside",
    textfont = list(color = STATS_INK),
    cliponaxis = FALSE,
    customdata = span,
    hovertemplate = "<b>%{y}</b><br>%{text}, %{customdata}<extra></extra>"
  ) |>
    stats_layout(
      bargap = 0.35,
      showlegend = FALSE,
      xaxis = list(
        title = "Geração",
        tickvals = gens,
        ticktext = generation_label(gens),
        gridcolor = STATS_GRID,
        zeroline = FALSE,
        range = c(min(gens) - 0.6, max(gens) + 1.6),
        fixedrange = TRUE
      ),
      yaxis = list(
        title = "",
        type = "category",
        ticksuffix = "  ",
        categoryorder = "array",
        categoryarray = rev(b$nucleus_label),
        fixedrange = TRUE
      )
    )
}

# Proporção feminino/masculino na família toda (primeira linha) e em cada núcleo.
plot_sex_by_nucleus <- function(stats) {
  b <- stats$by_nucleus
  o <- stats$overview
  all_label <- "Família toda"
  rows <- c(all_label, b$nucleus_label)
  n_f <- c(o$n_f, b$n_f)
  n_m <- c(o$n_m, b$n_m)
  n_unknown <- c(o$n_sex_unknown, b$n_sex_unknown)
  total <- n_f + n_m + n_unknown

  p <- plotly::plot_ly()
  for (s in sex_series(n_f, n_m, n_unknown)) {
    pct <- ifelse(total > 0, 100 * s$values / total, 0)
    p <- plotly::add_trace(
      p,
      type = "bar",
      orientation = "h",
      x = pct,
      y = rows,
      name = s$label,
      marker = list(color = SEX_COLORS[[s$key]], line = list(color = "white", width = 2)),
      # Rótulo só onde cabe; o resto fica no hover.
      text = ifelse(pct >= 12, paste0(round(pct), "%"), ""),
      textposition = "inside",
      insidetextanchor = "middle",
      textfont = list(color = "white"),
      customdata = s$values,
      hovertemplate = paste0(
        "<b>%{y}</b><br>", s$label, ": %{customdata} (%{x:.0f}%)<extra></extra>"
      )
    )
  }

  p |>
    stats_layout(
      barmode = "stack",
      bargap = 0.35,
      legend = list(orientation = "h", x = 0, y = 1.12, traceorder = "normal"),
      xaxis = list(
        title = "",
        range = c(0, 100),
        ticksuffix = "%",
        tickvals = c(0, 25, 50, 75, 100),
        gridcolor = STATS_GRID,
        zeroline = FALSE,
        fixedrange = TRUE
      ),
      yaxis = list(
        title = "",
        type = "category",
        ticksuffix = "  ",
        categoryorder = "array",
        categoryarray = rev(rows),
        fixedrange = TRUE
      ),
      # Linha separando a família toda dos núcleos.
      shapes = list(list(
        type = "line",
        xref = "paper",
        x0 = 0,
        x1 = 1,
        yref = "y",
        y0 = length(rows) - 1.5,
        y1 = length(rows) - 1.5,
        line = list(color = STATS_MUTED, width = 1, dash = "dot")
      ))
    )
}

# "#C0392B" -> "rgba(192,57,43,0.1)", para o fundo claro dos retângulos.
hex_alpha <- function(hex, alpha) {
  rgb <- grDevices::col2rgb(hex)
  paste0("rgba(", rgb[1], ",", rgb[2], ",", rgb[3], ",", alpha, ")")
}

# Idade de quem está vivo, por núcleo, em linguagem de leigo: cada bolinha é uma
# pessoa (rosa ou azul), e um retângulo na cor do núcleo vai da mais nova à
# mais velha, com as duas idades escritas nas pontas.
plot_age_by_nucleus <- function(stats) {
  b <- stats$by_nucleus[!is.na(stats$by_nucleus$age_min), , drop = FALSE]
  if (nrow(b) == 0) {
    return(stats_empty_plot("Ninguém vivo com ano de nascimento para calcular idades."))
  }
  rows <- seq_len(nrow(b))
  row_of <- stats::setNames(rows, b$nucleus_label)
  colors <- nucleus_colors_for(stats, b$nucleus)

  pp <- stats$people
  pp <- pp[pp$alive & !is.na(pp$age) & pp$nucleus_label %in% b$nucleus_label, , drop = FALSE]
  # Espalha os pontos na vertical para não se sobreporem. É determinístico
  # (razão áurea) para o gráfico não "tremer" a cada redesenho.
  pp <- pp[order(pp$nucleus_label, pp$age), , drop = FALSE]
  k <- stats::ave(seq_len(nrow(pp)), pp$nucleus_label, FUN = seq_along)
  pp$y <- row_of[pp$nucleus_label] + ((k * 0.618) %% 1 - 0.5) * 0.4

  p <- plotly::plot_ly()
  sex_keys <- c("F", "M", "unknown")
  for (key in sex_keys) {
    sub <- if (key == "unknown") pp[is.na(pp$sex), , drop = FALSE] else pp[pp$sex %in% key, , drop = FALSE]
    if (nrow(sub) == 0) next
    p <- plotly::add_trace(
      p,
      type = "scatter",
      mode = "markers",
      x = sub$age,
      y = sub$y,
      name = if (key == "unknown") SEX_UNKNOWN_LABEL else SEX_LABELS[[key]],
      marker = list(
        color = SEX_COLORS[[key]],
        size = 10,
        line = list(color = "white", width = 1.5)
      ),
      text = sub$name,
      hovertemplate = "<b>%{text}</b><br>%{x} anos<extra></extra>"
    )
  }

  lo <- min(b$age_min)
  hi <- max(b$age_max)
  span <- max(hi - lo, 20)
  # Folga horizontal do retângulo em volta das bolinhas da ponta.
  pad <- span * 0.025

  boxes <- lapply(rows, function(i) {
    list(
      type = "rect",
      xref = "x",
      yref = "y",
      x0 = b$age_min[i] - pad,
      x1 = b$age_max[i] + pad,
      y0 = i - 0.34,
      y1 = i + 0.34,
      line = list(color = colors[i], width = 1.5),
      fillcolor = hex_alpha(colors[i], 0.1),
      layer = "below"
    )
  })

  edge_label <- function(x, y, text, side) {
    list(
      x = x,
      y = y,
      text = text,
      showarrow = FALSE,
      xanchor = if (side == "left") "right" else "left",
      xshift = if (side == "left") -6 else 6,
      font = list(color = STATS_MUTED, size = 11)
    )
  }
  # Núcleo com uma idade só (ou todas iguais) leva um rótulo só.
  spread <- rows[b$age_min < b$age_max]
  single <- setdiff(rows, spread)
  labels <- c(
    lapply(spread, function(i) edge_label(b$age_min[i] - pad, i, paste0("mín. ", b$age_min[i]), "left")),
    lapply(spread, function(i) edge_label(b$age_max[i] + pad, i, paste0("máx. ", b$age_max[i]), "right")),
    lapply(single, function(i) edge_label(b$age_max[i] + pad, i, paste0(b$age_max[i], " anos"), "right"))
  )

  # O eixo começa perto da pessoa mais nova, não no zero, com folga dos dois
  # lados para os rótulos "mín." e "máx." caberem.
  x_range <- c(max(lo - span * 0.32, -span * 0.05), hi + span * 0.28)
  ticks <- pretty(c(max(lo - span * 0.1, 0), hi))
  ticks <- ticks[ticks >= x_range[1] & ticks <= x_range[2]]

  p |>
    stats_layout(
      legend = list(orientation = "h", x = 0, y = 1.02, yanchor = "bottom", traceorder = "normal"),
      margin = list(l = 10, r = 20, t = 40, b = 10),
      annotations = labels,
      shapes = boxes,
      xaxis = list(
        title = "Idade (anos)",
        gridcolor = STATS_GRID,
        zeroline = FALSE,
        range = x_range,
        tickmode = "array",
        tickvals = ticks,
        fixedrange = TRUE
      ),
      yaxis = list(
        title = "",
        tickvals = rows,
        ticktext = paste0(b$nucleus_label, "  "),
        # Invertido: o primeiro núcleo fica em cima.
        range = c(nrow(b) + 0.6, 0.4),
        showgrid = FALSE,
        zeroline = FALSE,
        fixedrange = TRUE
      )
    )
}
