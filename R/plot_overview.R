# Uma geração vale este tanto de unidades horizontais. Prender o eixo y ao x com
# esta razão mantém a árvore na mesma proporção em qualquer tela, no lugar dos
# 130px verticais fixos de antes — que davam uma distorção de até 13:1 contra o
# espaçamento entre irmãos.
GEN_ASPECT <- 5

# Deslocamento em y do rótulo, abaixo da bola. O eixo y está invertido pelo
# `range`, então y crescente desce na tela.
LABEL_DY <- 0.11

LABEL_CHAR_WIDTH <- 0.52 # largura média de caractere, em ems
LABEL_MAX_FONT_PX <- 16 # espelha MAX_FONT_PX em www/tree_scale.js
LABEL_FONT_RATIO <- 0.75 # espelha FONT_RATIO em www/tree_scale.js
NODE_DIAMETER_UNITS <- 0.9 # espelha NODE_DIAMETER_UNITS em www/tree_scale.js

# Largura estimada do rótulo, em unidades do eixo x. Como a fonte é derivada do
# diâmetro da bola, que é medido em unidades, essa largura não muda com o zoom —
# o que permite o layout reservar espaço para o texto já na hora de posicionar.
label_width_units <- function(text, scale = 1) {
  nchar(gsub("<[^>]+>", "", text)) * LABEL_CHAR_WIDTH * LABEL_FONT_RATIO *
    NODE_DIAMETER_UNITS * scale
}
LABEL_HIGHLIGHT_SCALE <- 1.35 # letra maior para a geração mais antiga

# Quem leva o primeiro nome em letra maior: a geração mais antiga de cada núcleo
# e o tronco fundador (marcados em build_overview_layout), tirando os
# ex-cônjuges, que ficam em tamanho normal como todo mundo.
overview_label_highlight <- function(layout) {
  hl <- if ("is_highlight" %in% names(layout)) {
    !is.na(layout$is_highlight) & layout$is_highlight
  } else {
    rep(FALSE, nrow(layout))
  }
  hl & !layout$is_former_partner_node
}

# Todo mundo mostra o primeiro nome, em qualquer zoom; só a geração mais antiga
# de cada núcleo (e o tronco fundador) leva letra maior via `scale`.
#
# `default` e `zoomed` mandam o mesmo texto — o JS ainda escolhe entre os dois
# campos de `layout.meta` conforme o zoom, mas hoje sem diferença visível.
# Mandar do R, em vez de o JS guardar o original numa cache, evita rótulo
# obsoleto quando um CSV novo é carregado.
overview_label_texts <- function(layout, is_placeholder) {
  idx <- which(!is.na(layout$x))
  ph <- is_placeholder[idx]
  anon <- ph & is_anonymous_placeholder(layout$name[idx])
  highlight <- overview_label_highlight(layout)[idx]

  short <- layout$short_name[idx]
  short[anon] <- "?"

  italic <- function(x) ifelse(ph, paste0("<i>", x, "</i>"), x)
  list(
    default = italic(short),
    zoomed = italic(short),
    scale = ifelse(highlight, LABEL_HIGHLIGHT_SCALE, 1)
  )
}

# A partir de quantos pixels por unidade os primeiros nomes cabem sem colidir.
#
# Enquanto a fonte cresce junto com o espaçamento — os dois derivam dos mesmos
# pixels por unidade — a proporção entre eles não muda e dois nomes vizinhos
# colidem em QUALQUER zoom. Só depois que a fonte trava em LABEL_MAX_FONT_PX é
# que a largura do rótulo para de crescer e o espaçamento passa a ganhar
# terreno. Este é o zoom em que o pior par da árvore finalmente se separa.
#
# Calculado a partir dos nomes reais, e não fixado numa constante, para
# continuar certo conforme a planilha ganhar nomes mais longos.
first_name_zoom_threshold <- function(layout, texts) {
  idx <- which(!is.na(layout$x))
  plain <- gsub("<[^>]+>", "", texts$zoomed)
  width_px <- nchar(plain) * LABEL_CHAR_WIDTH * LABEL_MAX_FONT_PX * texts$scale
  needed <- 0
  for (g in unique(layout$generation[idx])) {
    at <- which(layout$generation[idx] == g)
    if (length(at) < 2) {
      next
    }
    o <- at[order(layout$x[idx][at])]
    for (k in seq_len(length(o) - 1)) {
      a <- o[k]
      b <- o[k + 1]
      gap <- layout$x[idx][b] - layout$x[idx][a]
      needed <- max(needed, (width_px[a] + width_px[b]) / 2 / gap)
    }
  }
  needed * 1.08 # margem para a estimativa de largura do caractere
}

# Os nomes são anotações, e não `text` dentro do trace, porque assim o zoom pode
# trocar o texto e o tamanho de cada um sem mexer nos dados das séries.
build_overview_labels <- function(layout, is_placeholder, texts) {
  idx <- which(!is.na(layout$x))
  lapply(seq_along(idx), function(k) {
    i <- idx[k]
    list(
      x = layout$x[i],
      y = layout$y[i] + LABEL_DY,
      text = texts$default[k],
      showarrow = FALSE,
      xanchor = "center",
      yanchor = "top",
      align = "center",
      font = list(
        size = 8 * texts$scale[k],
        color = if (is_placeholder[i]) "#95A5A6" else "#2C3E50"
      ),
      bgcolor = "rgba(250,250,250,0.85)",
      borderpad = 1
    )
  })
}

render_overview <- function(family) {
  layout <- build_overview_layout(family)
  edges <- build_overview_edges(layout)

  is_ph <- if ("is_placeholder" %in% names(layout)) {
    !is.na(layout$is_placeholder) & layout$is_placeholder
  } else {
    rep(FALSE, nrow(layout))
  }

  p <- plotly::plot_ly(source = "overview") |>
    plotly::config(displayModeBar = FALSE, scrollZoom = TRUE) |>
    plotly::event_register("plotly_click")

  # --- Edges (drawn first, behind nodes) ---
  if (nrow(edges) > 0) {
    for (k in unique(edges$kind)) {
      e <- edges[edges$kind == k, , drop = FALSE]
      xs <- as.numeric(rbind(e$x, e$xend, NA))
      ys <- as.numeric(rbind(e$y, e$yend, NA))
      color <- if (k == "partner") "#9CA3AF" else "#B0B7BD"
      width <- if (k %in% c("partner", "partner_former")) 1.5 else 1.2
      dash <- if (k == "partner_former") "dot" else "solid"
      p <- plotly::add_trace(
        p,
        x = xs,
        y = ys,
        type = "scatter",
        mode = "lines",
        line = list(color = color, width = width, dash = dash),
        hoverinfo = "skip",
        showlegend = FALSE
      )
    }
  }

  # --- Nodes: one trace per nucleus (derived from the data) ---
  # O tamanho aqui é só o valor inicial: www/tree_scale.js recalcula bolas,
  # fotos e fonte a cada redesenho, a partir dos pixels por unidade de dados.
  real <- layout[!is_ph, , drop = FALSE]
  nuclei <- unique(real$nucleus)
  nuclei <- c(sort(nuclei[!is.na(nuclei)]), if (anyNA(nuclei)) NA)
  border_colors <- nucleus_border_colors(nuclei)

  for (i in seq_along(nuclei)) {
    nuc <- nuclei[i]
    sel <- if (is.na(nuc)) {
      is.na(real$nucleus)
    } else {
      !is.na(real$nucleus) & real$nucleus == nuc
    }
    if (!any(sel)) {
      next
    }
    sub <- real[sel, , drop = FALSE]
    label <- if (is.na(nuc)) {
      "Família"
    } else {
      nuc
    }
    border_color <- border_colors[i]
    # Transparent fill where a photo will show through; white otherwise.
    fill_color <- ifelse(has_image(sub$image_file), "rgba(255,255,255,0)", "white")
    p <- plotly::add_trace(
      p,
      x = sub$x,
      y = sub$y,
      type = "scatter",
      mode = "markers",
      marker = list(
        size = 22,
        color = fill_color,
        line = list(color = border_color, width = 2.5)
      ),
      cliponaxis = FALSE,
      hoverinfo = "text",
      hovertext = paste0(
        "<b>",
        sub$name,
        "</b><br>",
        format_dates_vec(sub$birth_year, sub$death_year),
        "<br>Núcleo: ",
        label
      ),
      customdata = sub$name,
      name = label,
      legendgroup = label,
      showlegend = TRUE
    )
  }

  # --- Placeholders: co-genitores citados mas ainda sem ficha na planilha ---
  # Existem para que nenhum ramo fique solto (ver R/placeholders.R). Ficam com
  # traço mais leve para não competirem com as pessoas de verdade.
  if (any(is_ph)) {
    ph <- layout[is_ph, , drop = FALSE]
    anon <- is_anonymous_placeholder(ph$name)
    p <- plotly::add_trace(
      p,
      x = ph$x,
      y = ph$y,
      type = "scatter",
      mode = "markers",
      marker = list(
        size = 22,
        color = "white",
        line = list(color = "#C8CDD2", width = 1.5)
      ),
      cliponaxis = FALSE,
      hoverinfo = "text",
      hovertext = ifelse(
        anon,
        "<i>Pessoa ainda não registrada</i>",
        paste0("<b>", ph$name, "</b><br><i>ainda sem ficha na planilha</i>")
      ),
      customdata = ph$name,
      name = "Sem registro",
      legendgroup = "Sem registro",
      showlegend = TRUE
    )
  }

  # --- Axes / layout ---
  label_texts <- overview_label_texts(layout, is_ph)
  x_range <- range(layout$x, na.rm = TRUE)
  y_range <- range(layout$y, na.rm = TRUE)
  p <- plotly::layout(
    p,
    annotations = build_overview_labels(layout, is_ph, label_texts),
    # Lido por www/tree_scale.js para trocar inicial por primeiro nome no zoom.
    meta = list(
      first_name_px_per_unit = first_name_zoom_threshold(layout, label_texts),
      labels_default = label_texts$default,
      labels_zoomed = label_texts$zoomed,
      labels_scale = label_texts$scale
    ),
    images = build_node_images(layout, base_diameter = 0.9, ref_size = 22),
    xaxis = list(
      visible = FALSE,
      range = c(x_range[1] - 1.5, x_range[2] + 1.5)
    ),
    # Invertido pelo próprio range (geração 1 no topo): `autorange = "reversed"`
    # não combina com o scaleanchor abaixo.
    yaxis = list(
      visible = FALSE,
      range = c(y_range[2] + 0.8, y_range[1] - 0.8),
      scaleanchor = "x",
      scaleratio = GEN_ASPECT
    ),
    plot_bgcolor = "#FAFAFA",
    paper_bgcolor = "#FAFAFA",
    showlegend = TRUE,
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.02),
    margin = list(l = 10, r = 10, t = 20, b = 30)
  )
  p
}
