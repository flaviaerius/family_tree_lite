build_focused_layout <- function(family, focus_name) {
  focus <- person_by_name(family, focus_name)
  if (is.null(focus)) return(NULL)

  rows <- list()
  push <- function(person, role, x, y, size, group = NA_character_) {
    rows[[length(rows) + 1L]] <<- data.frame(
      name        = person$name,
      short_name  = person$short_name,
      sex         = person$sex,
      nucleus     = person$nucleus,
      birth_year  = person$birth_year,
      death_year  = person$death_year,
      image_file  = person$image_file,
      role        = role,
      x           = x,
      y           = y,
      size        = size,
      from_former = (role %in% c("child_former", "partner_former")),
      group       = group,
      stringsAsFactors = FALSE
    )
  }

  parent_offset_x  <- 3.0 # horizontal distance of mother/father from focus
  parent_y         <- 3.0 # height of the parents row
  partner_offset_x <- 4.0 # horizontal distance of the current partner
  child_y          <- -3.6 # height of the children row
  child_step       <- 2.2 # horizontal spacing between siblings
  former_start_x   <- -5.0 # x of the first former partner
  former_gap       <- 2.0 # horizontal gap between former-partnership blocks

  push(focus, "focus", 0, 0, 70)

  mother <- mother_of(family, focus_name)
  father <- father_of(family, focus_name)
  if (!is.null(mother)) push(mother, "mother", -parent_offset_x, parent_y, 55)
  if (!is.null(father)) push(father, "father", parent_offset_x, parent_y, 55)

  current_partner_name <- focus$partner_name
  current_partner <- if (!is.na(current_partner_name)) {
    person_by_name(family, current_partner_name)
  } else NULL

  if (!is.null(current_partner))
    push(current_partner, "partner", partner_offset_x, 0, 60, group = "current")

  # Partition children by other-parent
  all_kids <- children_of(family, focus_name)
  if (nrow(all_kids) > 0) {
    focus_is_mother <- !is.na(all_kids$mother_name) & all_kids$mother_name == focus_name
    all_kids$other_parent <- ifelse(focus_is_mother, all_kids$father_name, all_kids$mother_name)
    all_kids$is_former <- if (is.na(current_partner_name)) {
      rep(FALSE, nrow(all_kids))
    } else {
      is.na(all_kids$other_parent) | all_kids$other_parent != current_partner_name
    }
  } else {
    all_kids$other_parent <- character(0)
    all_kids$is_former    <- logical(0)
  }

  current_kids <- all_kids[!all_kids$is_former, , drop = FALSE]
  former_kids  <- all_kids[ all_kids$is_former, , drop = FALSE]

  if (nrow(current_kids) > 0)
    current_kids <- current_kids[order(current_kids$birth_year, current_kids$name, na.last = TRUE), ]

  # Current children centered at midpoint of focus (x=0) and current partner
  n_current_kids   <- nrow(current_kids)
  current_kids_mid <- if (!is.null(current_partner)) partner_offset_x / 2 else 0
  if (n_current_kids > 0) {
    xs_current <- if (n_current_kids == 1) current_kids_mid else
      seq(current_kids_mid - (n_current_kids - 1) * child_step / 2,
          current_kids_mid + (n_current_kids - 1) * child_step / 2,
          length.out = n_current_kids)
    for (i in seq_len(n_current_kids))
      push(current_kids[i, ], "child", xs_current[i], child_y, 50, group = "current")
  }

  # Former partnerships — ordered by oldest child's birth year
  fp_names_ordered <- if (nrow(former_kids) > 0) {
    fk <- former_kids[order(former_kids$birth_year, former_kids$name, na.last = TRUE), ]
    unique_fp <- unique(fk$other_parent)
    c(unique_fp[!is.na(unique_fp)], if (anyNA(unique_fp)) NA_character_ else NULL)
  } else character(0)

  former_partner_x <- former_start_x
  for (fp_name in fp_names_ordered) {
    fp_person <- if (!is.na(fp_name)) person_by_name(family, fp_name) else NULL
    group_id  <- if (!is.na(fp_name)) fp_name else paste0(".unknown.", abs(former_partner_x))

    fp_kids <- if (!is.na(fp_name)) {
      former_kids[!is.na(former_kids$other_parent) & former_kids$other_parent == fp_name, , drop = FALSE]
    } else {
      former_kids[is.na(former_kids$other_parent), , drop = FALSE]
    }
    if (nrow(fp_kids) > 0)
      fp_kids <- fp_kids[order(fp_kids$birth_year, fp_kids$name, na.last = TRUE), ]
    n_fp_kids <- nrow(fp_kids)

    if (!is.null(fp_person))
      push(fp_person, "partner_former", former_partner_x, 0, 55, group = group_id)

    fp_kids_mid <- if (!is.null(fp_person)) former_partner_x / 2 else former_partner_x + 0.5

    if (n_fp_kids > 0) {
      xs_fp <- if (n_fp_kids == 1) fp_kids_mid else
        seq(fp_kids_mid - (n_fp_kids - 1) * child_step / 2,
            fp_kids_mid + (n_fp_kids - 1) * child_step / 2,
            length.out = n_fp_kids)
      for (i in seq_len(n_fp_kids))
        push(fp_kids[i, ], "child_former", xs_fp[i], child_y, 50, group = group_id)
    }

    group_width      <- max(former_gap, n_fp_kids * child_step)
    former_partner_x <- former_partner_x - group_width - former_gap
  }

  do.call(rbind, rows)
}

build_focused_connectors <- function(layout) {
  edges <- list()
  add_edge <- function(x, y, xend, yend, kind = "solid") {
    edges[[length(edges) + 1L]] <<- data.frame(
      x = x, y = y, xend = xend, yend = yend, kind = kind,
      stringsAsFactors = FALSE
    )
  }

  focus_row    <- layout[layout$role == "focus",          , drop = FALSE]
  partner_row  <- layout[layout$role == "partner",        , drop = FALSE]
  fp_rows      <- layout[layout$role == "partner_former", , drop = FALSE]
  mother_row   <- layout[layout$role == "mother",         , drop = FALSE]
  father_row   <- layout[layout$role == "father",         , drop = FALSE]
  cur_children <- layout[layout$role == "child",          , drop = FALSE]
  fmr_children <- layout[layout$role == "child_former",   , drop = FALSE]

  # Parent connectors
  if (nrow(mother_row) == 1 || nrow(father_row) == 1) {
    add_edge(focus_row$x, focus_row$y, focus_row$x, 1.0)
    if (nrow(mother_row) == 1) {
      add_edge(focus_row$x, 1.0, mother_row$x, 1.0)
      add_edge(mother_row$x, 1.0, mother_row$x, mother_row$y)
    }
    if (nrow(father_row) == 1) {
      add_edge(focus_row$x, 1.0, father_row$x, 1.0)
      add_edge(father_row$x, 1.0, father_row$x, father_row$y)
    }
  }

  # Current partner line
  if (nrow(partner_row) == 1)
    add_edge(focus_row$x, focus_row$y, partner_row$x, partner_row$y)

  # Current children bus
  if (nrow(cur_children) > 0) {
    bus_y <- -2.0
    mid_x <- if (nrow(partner_row) == 1) mean(c(focus_row$x, partner_row$x)) else focus_row$x
    add_edge(mid_x, focus_row$y, mid_x, bus_y)
    xs    <- cur_children$x
    bus_l <- min(c(xs, mid_x)); bus_r <- max(c(xs, mid_x))
    if (bus_l < bus_r) add_edge(bus_l, bus_y, bus_r, bus_y)
    for (i in seq_len(nrow(cur_children)))
      add_edge(cur_children$x[i], bus_y, cur_children$x[i], cur_children$y[i])
  }

  # Former partnerships — one bus per group
  fmr_gids <- if (nrow(fmr_children) > 0) unique(fmr_children$group) else character(0)
  for (gid in fmr_gids) {
    fp_match <- fp_rows[!is.na(fp_rows$group) & fp_rows$group == gid, , drop = FALSE]
    fp_kids  <- fmr_children[!is.na(fmr_children$group) & fmr_children$group == gid, , drop = FALSE]

    mid_x <- if (nrow(fp_match) == 1) {
      add_edge(focus_row$x, focus_row$y, fp_match$x, fp_match$y, kind = "dashed")
      mean(c(focus_row$x, fp_match$x))
    } else {
      if (nrow(fp_kids) > 0) mean(fp_kids$x) else focus_row$x
    }

    if (nrow(fp_kids) > 0) {
      bus_y <- -2.0
      add_edge(mid_x, focus_row$y, mid_x, bus_y)
      xs    <- fp_kids$x
      bus_l <- min(c(xs, mid_x)); bus_r <- max(c(xs, mid_x))
      if (bus_l < bus_r) add_edge(bus_l, bus_y, bus_r, bus_y)
      for (j in seq_len(nrow(fp_kids)))
        add_edge(fp_kids$x[j], bus_y, fp_kids$x[j], fp_kids$y[j])
    }
  }

  if (length(edges) == 0)
    return(data.frame(x = numeric(0), y = numeric(0),
                      xend = numeric(0), yend = numeric(0), kind = character(0)))
  do.call(rbind, edges)
}
