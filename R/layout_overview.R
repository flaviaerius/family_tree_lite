# --- Layout zoomed out for the whole family ---
build_overview_layout <- function(family) {
  env <- new.env()
  env$family <- family
  env$family$y <- as.numeric(env$family$generation)
  env$family$x <- NA_real_
  env$family$is_former_partner_node <- FALSE
  env$widths <- list()
  GAP <- 1.2 # horizontal gap between sibling subtrees
  COUPLE_DX <- 0.95 # half-distance between two partners of a couple
  COUPLE_DX_GEN1 <- 1.5 # wider gap for the root couple so names don't overlap
  COUPLE_DX_HIGHLIGHT <- 2.4 # casais em destaque mostram o primeiro nome grande

  # Quem aparece com o primeiro nome em letra maior: a geração mais antiga de
  # cada núcleo, mais o tronco fundador inteiro — o núcleo a que pertence a
  # geração 1, seja qual for o nome dele na planilha. Precisa ser decidido aqui,
  # e não só na hora de desenhar, porque esses rótulos são largos e o casal
  # precisa de espaço reservado para eles.
  real_idx <- if ("is_placeholder" %in% names(env$family)) {
    which(!env$family$is_placeholder)
  } else {
    seq_len(nrow(env$family))
  }
  founder_nucleus <- unique(env$family$nucleus[
    real_idx[env$family$generation[real_idx] == 1L]
  ])
  env$is_highlight <- vapply(seq_len(nrow(env$family)), function(i) {
    nuc <- env$family$nucleus[i]
    if (is.na(nuc) || !(i %in% real_idx)) {
      return(FALSE)
    }
    if (nuc %in% founder_nucleus) {
      return(TRUE)
    }
    same <- real_idx[!is.na(env$family$nucleus[real_idx]) &
      env$family$nucleus[real_idx] == nuc]
    isTRUE(env$family$generation[i] ==
      min(env$family$generation[same], na.rm = TRUE))
  }, logical(1))

  # Half-distance between a couple, from the primary's row.
  couple_dx_for <- function(i) {
    if (isTRUE(env$is_highlight[i])) {
      COUPLE_DX_HIGHLIGHT
    } else if (isTRUE(env$family$generation[i] == 1L)) {
      COUPLE_DX_GEN1
    } else {
      COUPLE_DX
    }
  }

  kids_of <- function(name) {
    idx <- which(
      (!is.na(env$family$mother_name) & env$family$mother_name == name) |
        (!is.na(env$family$father_name) & env$family$father_name == name)
    )
    if (length(idx) == 0) {
      return(character(0))
    }
    ord <- order(env$family$birth_year[idx], env$family$name[idx])
    env$family$name[idx[ord]]
  }

  # Largura do rótulo de cada pessoa, em unidades do eixo x (ver
  # label_width_units em R/plot_overview.R). É constante no zoom, porque a fonte
  # também é medida em unidades — então dá para reservar espaço para o texto já
  # aqui, na hora de posicionar.
  env$label_w <- vapply(seq_len(nrow(env$family)), function(i) {
    if (isTRUE(env$is_highlight[i])) {
      label_width_units(env$family$short_name[i], LABEL_HIGHLIGHT_SCALE)
    } else if (isTRUE(env$family$is_placeholder[i])) {
      label_width_units("?")
    } else {
      label_width_units(env$family$short_name[i])
    }
  }, numeric(1))

  # Largura que o casal (ou a pessoa sozinha) ocupa, rótulo incluído, antes de
  # considerar os filhos. compute_width e assign_pos PRECISAM usar a mesma
  # regra: enquanto discordavam, a largura reservada não era a mesma usada para
  # centrar o bloco e o vizinho encostava.
  header_width_for <- function(i) {
    # Quem tem cônjuge ao lado não é só quem preencheu partner_name: um
    # co-genitor vindo dos filhos também é desenhado ao lado (o Solteiro tem um,
    # e sem contá-lo ele acabava posicionado fora do próprio slot).
    partners <- vapply(groups_of(env$family$name[i]), function(g) {
      if (is.null(g$partner)) NA_character_ else g$partner
    }, character(1))
    partners <- unique(c(partners, env$family$partner_name[i]))
    j <- match(partners[!is.na(partners)], env$family$name)
    j <- j[!is.na(j)]
    if (length(j) == 0) {
      return(max(1, env$label_w[i]))
    }
    # da borda esquerda do rótulo de um até a borda direita do rótulo do outro
    max(2, 2 * couple_dx_for(i) + (env$label_w[i] + max(env$label_w[j])) / 2)
  }

  compute_width <- function(name) {
    if (!is.null(env$widths[[name]])) {
      return(env$widths[[name]])
    }
    i <- which(env$family$name == name)
    partner <- env$family$partner_name[i]
    header_w <- header_width_for(i)

    kids <- kids_of(name)
    if (length(kids) == 0) {
      env$widths[[name]] <- header_w
      return(header_w)
    }
    children_w <- sum(vapply(kids, compute_width, numeric(1))) +
      GAP * (length(kids) - 1)
    w <- max(header_w, children_w)
    # Multiple partnerships: mirrored partners stick out GAP/2 beyond the
    # children block on each side — reserve that space so siblings stay clear.
    if (length(groups_of(name)) > 1) w <- w + GAP
    env$widths[[name]] <- w
    w
  }

  # Other parent of `child_name` relative to `parent_name`
  other_parent_of <- function(child_name, parent_name) {
    ci <- which(env$family$name == child_name)
    op <- if (
      !is.na(env$family$mother_name[ci]) &&
        env$family$mother_name[ci] == parent_name
    ) {
      env$family$father_name[ci]
    } else {
      env$family$mother_name[ci]
    }
    if (is.na(op)) NA_character_ else op
  }

  # Partnership groups for a parent, ordered former (oldest first) → current (last)
  groups_of <- function(name) {
    i <- which(env$family$name == name)
    current_partner <- env$family$partner_name[i]
    kid_names <- kids_of(name)
    if (length(kid_names) == 0) {
      return(list())
    }

    ops <- vapply(kid_names, other_parent_of, character(1), parent_name = name)
    keys <- ifelse(is.na(ops), "<NA>", ops)

    grps <- lapply(unique(keys), function(k) {
      sel <- keys == k
      partner <- if (k == "<NA>") NA_character_ else k
      # Basta um dos dois declarar o outro. Na planilha há casais em que só um
      # lado preencheu partner_name (Xisto e Neuza), e sem isso o outro seria
      # tratado como ex-cônjuge: linha tracejada e rótulo rebaixado.
      is_current <- !is.na(partner) &&
        ((!is.na(current_partner) && partner == current_partner) ||
          identical(
            env$family$partner_name[match(partner, env$family$name)],
            name
          ))
      bys <- env$family$birth_year[match(kid_names[sel], env$family$name)]
      list(
        partner = partner,
        kids = kid_names[sel],
        is_current = is_current,
        oldest = suppressWarnings(min(bys, na.rm = TRUE))
      )
    })

    is_cur <- vapply(grps, `[[`, logical(1), "is_current")
    formers <- grps[!is_cur]
    currents <- grps[is_cur]
    if (length(formers) > 1) {
      formers <- formers[order(vapply(formers, `[[`, numeric(1), "oldest"))]
    }
    c(formers, currents)
  }

  place_partner <- function(partner_name, x, y, is_current) {
    if (is.na(partner_name)) {
      return(invisible())
    }
    j <- which(env$family$name == partner_name)
    if (length(j) != 1L || !is.na(env$family$x[j])) {
      return(invisible())
    }
    env$family$x[j] <- x
    env$family$y[j] <- y
    if (!is_current) {
      env$family$is_former_partner_node[j] <- TRUE
    }
    invisible()
  }

  assign_pos <- function(name, x_left) {
    i <- which(env$family$name == name)
    partner <- env$family$partner_name[i]
    grps <- groups_of(name)

    dx <- couple_dx_for(i)

    # Leaf: no children. Centrado na largura que compute_width reservou — que
    # não é 1 nem 2 quando a pessoa está em destaque e precisa caber o primeiro
    # nome em letra grande. Posicionar na borda esquerda do slot, como antes,
    # encostava o rótulo no vizinho da esquerda.
    if (length(grps) == 0) {
      own_w <- env$widths[[name]]
      center <- x_left + own_w / 2
      if (!is.na(partner)) {
        env$family$x[i] <- center - dx
        place_partner(partner, center + dx, env$family$y[i], TRUE)
      } else {
        env$family$x[i] <- center
      }
      return(invisible(own_w))
    }

    all_kids <- unlist(lapply(grps, `[[`, "kids"))
    kid_w <- vapply(all_kids, function(k) env$widths[[k]], numeric(1))
    total_children_w <- sum(kid_w) + GAP * (length(all_kids) - 1)
    total_w <- max(header_width_for(i), total_children_w)
    # Match compute_width: reserve GAP/2 each side for mirrored partners.
    if (length(grps) > 1) total_w <- total_w + GAP
    children_left <- x_left + (total_w - total_children_w) / 2

    # Lay out each group's children left → right (GAP between every sibling);
    # record each group's children-block start/end/centre.
    cx <- children_left
    centers <- numeric(length(grps))
    gstarts <- numeric(length(grps))
    gends <- numeric(length(grps))
    placed <- 0L
    for (gi in seq_along(grps)) {
      gs <- NA_real_
      ge <- NA_real_
      for (cn in grps[[gi]]$kids) {
        if (placed > 0L) cx <- cx + GAP
        if (is.na(gs)) gs <- cx
        cw <- env$widths[[cn]]
        assign_pos(cn, cx)
        cx <- cx + cw
        ge <- cx
        placed <- placed + 1L
      }
      gstarts[gi] <- gs
      gends[gi] <- ge
      centers[gi] <- (gs + ge) / 2
    }

    pri_y <- env$family$y[i]

    # Single partnership (normal couple, or a lone former partner):
    # primary + partner adjacent, centred over the one children block.
    if (length(grps) == 1) {
      g <- grps[[1]]
      ctr <- centers[1]
      if (is.na(g$partner)) {
        env$family$x[i] <- ctr
      } else {
        env$family$x[i] <- ctr - dx
        place_partner(g$partner, ctr + dx, pri_y, g$is_current)
      }
      return(invisible(total_w))
    }

    # Multiple partnerships: primary sits between the last former block and the
    # current block, and the partners são fixados ao lado dele — anteriores à
    # esquerda, atual à direita. O casal deixa de ficar centrado sobre o próprio
    # bloco de filhos, mas o barramento já se estende de lado até alcançá-lo
    # (ver build_overview_edges), e o par continua legível como um par. Com três
    # ou mais parcerias o espelhamento antigo não tinha nem onde se apoiar: só
    # existem dois lados.
    is_cur <- vapply(grps, `[[`, logical(1), "is_current")
    former_idx <- which(!is_cur)
    last_former <- former_idx[length(former_idx)]
    P <- if (any(is_cur)) {
      cur_i <- which(is_cur)[1]
      (gends[last_former] + gstarts[cur_i]) / 2
    } else {
      gends[last_former]
    }
    env$family$x[i] <- P
    # `dx` é a meia-distância do casal, então a separação é 2 * dx — a mesma das
    # parcerias simples. O cônjuge atual usa o `dx` da pessoa, que é largo quando
    # ela está em destaque e precisa caber o primeiro nome em letra grande; os
    # anteriores ficam no espaçamento normal, já que levam o nome em letra normal.
    current_dx <- 2 * dx
    former_dx <- 2 * COUPLE_DX
    formers_placed <- 0L
    for (gi in seq_along(grps)) {
      px <- if (is_cur[gi]) {
        P + current_dx
      } else {
        formers_placed <- formers_placed + 1L
        P - former_dx * formers_placed
      }
      place_partner(grps[[gi]]$partner, px, pri_y, is_cur[gi])
    }
    invisible(total_w)
  }

  env$family$is_highlight <- env$is_highlight

  root_idx <- which(env$family$generation == 1L)
  if (length(root_idx) >= 1L) {
    root_name <- env$family$name[root_idx[1]]
    compute_width(root_name)
    assign_pos(root_name, 0)
  }

  env$family
}

is_primary_blood <- function(layout, i) {
  layout$generation[i] == 1L ||
    !is.na(layout$mother_name[i]) ||
    !is.na(layout$father_name[i])
}

build_overview_edges <- function(layout) {
  edges <- list()
  add_edge <- function(x, y, xend, yend, kind = "child") {
    edges[[length(edges) + 1L]] <<- data.frame(
      x = x,
      y = y,
      xend = xend,
      yend = yend,
      kind = kind,
      stringsAsFactors = FALSE
    )
  }

  # Current partner edges — solid, drawn once per pair
  seen_pair <- character(0)
  for (i in seq_len(nrow(layout))) {
    pn <- layout$partner_name[i]
    if (is.na(pn) || is.na(layout$x[i])) {
      next
    }
    j <- which(layout$name == pn)
    if (length(j) != 1L || is.na(layout$x[j])) {
      next
    }
    key <- paste(sort(c(layout$name[i], pn)), collapse = "|")
    if (key %in% seen_pair) {
      next
    }
    seen_pair <- c(seen_pair, key)
    add_edge(layout$x[i], layout$y[i], layout$x[j], layout$y[j], "partner")
  }

  # Parent -> children buses
  first_gen1 <- which(layout$generation == 1L)[1]
  seen_fp_pair <- character(0)

  for (i in seq_len(nrow(layout))) {
    if (is.na(layout$x[i])) {
      next
    }
    if (!is_primary_blood(layout, i)) {
      next
    }
    if (isTRUE(layout$is_former_partner_node[i])) {
      next
    }
    if (layout$generation[i] == 1L && i != first_gen1) {
      next
    }

    primary_name <- layout$name[i]
    partner_name <- layout$partner_name[i]
    couple_y <- layout$y[i]

    kid_idx <- which(
      (!is.na(layout$mother_name) & layout$mother_name == primary_name) |
        (!is.na(layout$father_name) & layout$father_name == primary_name)
    )
    if (length(kid_idx) == 0) {
      next
    }

    other_parents <- vapply(
      kid_idx,
      function(child_i) {
        if (
          !is.na(layout$mother_name[child_i]) &&
            layout$mother_name[child_i] == primary_name
        ) {
          if (is.na(layout$father_name[child_i])) {
            NA_character_
          } else {
            layout$father_name[child_i]
          }
        } else {
          if (is.na(layout$mother_name[child_i])) {
            NA_character_
          } else {
            layout$mother_name[child_i]
          }
        }
      },
      character(1)
    )

    # One bus per co-parent group. The bus is always solid; only the link line
    # to a *former* co-parent (anyone who isn't the current partner) is dotted.
    op_keys <- ifelse(is.na(other_parents), "<NA>", other_parents)

    # Com os parceiros agora agrupados junto do primário, os barramentos das
    # várias parcerias sairiam quase do mesmo ponto e se sobreporiam na
    # horizontal. Cada grupo ganha a sua altura para os braços continuarem
    # rastreáveis.
    #
    # As alturas começam em 0,55 porque as duas linhas de rótulo ocupam de 0,13
    # a 0,455 abaixo da bola (ver LABEL_ROW_DY em R/plot_overview.R): mais acima
    # que isso, o barramento passaria por cima dos nomes. O passo de 0,08 deixa
    # três parcerias caberem antes de 0,89, onde começa a bola do filho.
    group_keys <- unique(op_keys)

    for (gi in seq_along(group_keys)) {
      k <- group_keys[gi]
      bus_y <- couple_y + 0.55 + (gi - 1L) * 0.08
      grp <- kid_idx[op_keys == k]
      xs <- layout$x[grp]
      ys <- layout$y[grp]
      op <- if (k == "<NA>") NA_character_ else k

      op_x <- if (!is.na(op)) {
        px <- layout$x[layout$name == op]
        if (length(px) == 1L) px else NA_real_
      } else {
        NA_real_
      }

      is_current <- !is.na(partner_name) && !is.na(op) && op == partner_name

      # Connecting line + bus anchor (= midpoint of the couple)
      if (!is.na(op_x)) {
        anchor_x <- (layout$x[i] + op_x) / 2
        # Solid current-partner line is drawn by the partner loop above;
        # here we only add the dotted line for former co-parents.
        if (!is_current) {
          fp_key <- paste(sort(c(primary_name, op)), collapse = "|")
          if (!fp_key %in% seen_fp_pair) {
            seen_fp_pair <- c(seen_fp_pair, fp_key)
            add_edge(layout$x[i], couple_y, op_x, couple_y, "partner_former")
          }
        }
      } else {
        valid_xs <- xs[!is.na(xs)]
        anchor_x <- if (length(valid_xs) > 0) mean(valid_xs) else layout$x[i]
      }

      add_edge(anchor_x, couple_y, anchor_x, bus_y, "child")
      valid_xs <- xs[!is.na(xs)]
      if (length(valid_xs) > 0) {
        bus_l <- min(c(valid_xs, anchor_x))
        bus_r <- max(c(valid_xs, anchor_x))
        if (bus_l < bus_r) add_edge(bus_l, bus_y, bus_r, bus_y, "child")
      }
      for (k2 in seq_along(grp)) {
        if (!is.na(xs[k2])) add_edge(xs[k2], bus_y, xs[k2], ys[k2], "child")
      }
    }
  }

  if (length(edges) == 0) {
    return(data.frame(
      x = numeric(0),
      y = numeric(0),
      xend = numeric(0),
      yend = numeric(0),
      kind = character(0)
    ))
  }
  do.call(rbind, edges)
}
