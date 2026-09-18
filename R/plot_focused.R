render_focused <- function(family, focus_name) {
  layout <- build_focused_layout(family, focus_name)
  if (is.null(layout)) {
    return(plotly::plot_ly(source = "focused") |>
      plotly::layout(title = "Pessoa não encontrada"))
  }
  big <- layout
  edges <- build_focused_connectors(layout)

  p <- plotly::plot_ly(source = "focused") |>
    plotly::config(displayModeBar = FALSE) |>
    plotly::event_register("plotly_click")

  # Connectors
  if (nrow(edges) > 0) {
    for (k in unique(edges$kind)) {
      e <- edges[edges$kind == k, , drop = FALSE]
      xs <- as.numeric(rbind(e$x, e$xend, NA))
      ys <- as.numeric(rbind(e$y, e$yend, NA))
      dash <- if (k == "dashed") "dash" else "solid"
      p <- plotly::add_trace(
        p, x = xs, y = ys, type = "scatter", mode = "lines",
        line = list(color = "#95A5A6", width = 1.5, dash = dash),
        hoverinfo = "skip", showlegend = FALSE
      )
    }
  }

  # Big balls (one trace per role for clean styling)
  big_roles <- c("focus", "partner", "partner_former", "mother", "father", "child", "child_former")
  for (rl in big_roles) {
    sub <- big[big$role == rl, , drop = FALSE]
    if (nrow(sub) == 0) next
    line_color <- nucleus_color_vec(sub$nucleus, "dark")
    # Transparent fill where a photo will show through; nucleus color otherwise.
    fill <- ifelse(
      has_image(sub$image_file),
      "rgba(0,0,0,0)",
      nucleus_color_vec(sub$nucleus, "primary")
    )
    p <- plotly::add_trace(
      p,
      x = sub$x, y = sub$y, type = "scatter", mode = "markers",
      marker = list(
        size = sub$size, color = fill,
        line = list(color = line_color, width = if (rl == "focus") 4 else 3),
        opacity = 1
      ),
      hoverinfo = "text",
      hovertext = paste0("<b>", sub$name, "</b><br>", format_dates_vec(sub$birth_year, sub$death_year)),
      customdata = sub$name,
      name = rl, showlegend = FALSE
    )
  }

  # Labels below each node, drawn as annotations so they can sit on an opaque
  # white background that hides the connection lines underneath. Only the
  # focused person shows the full name; everyone else shows just the first name.
  label_y_offset <- 0.9
  label_annotations <- lapply(seq_len(nrow(big)), function(i) {
    person_row   <- big[i, ]
    role_tag     <- role_label_pt(person_row$role, person_row$sex)
    display_name <- if (person_row$role == "focus") {
      person_row$name
    } else {
      person_row$short_name
    }
    label_text <- paste0(
      if (nzchar(role_tag)) paste0("<i>", role_tag, "</i><br>") else "",
      "<b>", display_name, "</b><br>",
      format_dates(person_row$birth_year, person_row$death_year)
    )
    list(
      x = person_row$x,
      y = person_row$y - label_y_offset,
      text = label_text,
      showarrow = FALSE,
      xanchor = "center",
      yanchor = "top",
      align = "center",
      font = list(size = 11, color = "#2C3E50"),
      bgcolor = "rgba(255, 255, 255, 0.85)",
      borderpad = 2
    )
  })

  # Fit the view to the actual nodes (extra room below for the labels).
  x_padding        <- 2.0
  y_padding_top    <- 2.0
  y_padding_bottom <- 3.0
  x_range <- c(min(big$x) - x_padding, max(big$x) + x_padding)
  y_range <- c(min(big$y) - y_padding_bottom, max(big$y) + y_padding_top)

  p <- plotly::layout(
    p,
    annotations = label_annotations,
    images = build_node_images(big, base_diameter = 1.65, ref_size = 70),
    xaxis = list(visible = FALSE, range = x_range, fixedrange = TRUE),
    yaxis = list(
      visible = FALSE, range = y_range,
      scaleanchor = "x", scaleratio = 1, fixedrange = TRUE
    ),
    plot_bgcolor = "#FAFAFA",
    paper_bgcolor = "#FAFAFA",
    showlegend = FALSE,
    margin = list(l = 10, r = 10, t = 30, b = 10)
  )
  p
}
