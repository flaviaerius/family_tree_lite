person_by_name <- function(family, name) {
  if (is.null(name) || is.na(name)) {
    return(NULL)
  }
  hit <- family[family$name == name, , drop = FALSE]
  if (nrow(hit) == 0) {
    return(NULL)
  }
  hit[1, ]
}

partner_of <- function(family, name) {
  p <- person_by_name(family, name)
  if (is.null(p) || is.na(p$partner_name)) {
    return(NULL)
  }
  person_by_name(family, p$partner_name)
}

mother_of <- function(family, name) {
  p <- person_by_name(family, name)
  if (is.null(p) || is.na(p$mother_name)) {
    return(NULL)
  }
  person_by_name(family, p$mother_name)
}

father_of <- function(family, name) {
  p <- person_by_name(family, name)
  if (is.null(p) || is.na(p$father_name)) {
    return(NULL)
  }
  person_by_name(family, p$father_name)
}

children_of <- function(family, name) {
  family[
    !is.na(family$mother_name) &
      family$mother_name == name |
      !is.na(family$father_name) & family$father_name == name,
    ,
    drop = FALSE
  ]
}

siblings_of <- function(family, name) {
  p <- person_by_name(family, name)
  if (is.null(p)) {
    return(family[0, , drop = FALSE])
  }
  cond <- rep(FALSE, nrow(family))
  if (!is.na(p$mother_name)) {
    cond <- cond |
      (!is.na(family$mother_name) & family$mother_name == p$mother_name)
  }
  if (!is.na(p$father_name)) {
    cond <- cond |
      (!is.na(family$father_name) & family$father_name == p$father_name)
  }
  cond <- cond & family$name != name
  family[cond, , drop = FALSE]
}

children_with_origin <- function(family, focus_name) {
  focus <- person_by_name(family, focus_name)
  if (is.null(focus)) {
    return(family[0, , drop = FALSE])
  }
  kids <- children_of(family, focus_name)
  if (nrow(kids) == 0) {
    kids$from_former <- logical(0)
    return(kids)
  }
  current_partner <- focus$partner_name
  focus_is_mother <- !is.na(kids$mother_name) & kids$mother_name == focus_name
  other <- ifelse(focus_is_mother, kids$father_name, kids$mother_name)
  kids$from_former <- if (is.na(current_partner)) {
    rep(FALSE, nrow(kids))
  } else {
    is.na(other) | other != current_partner
  }
  kids
}

focus_circle_names <- function(family, focus_name) {
  focus <- person_by_name(family, focus_name)
  if (is.null(focus)) {
    return(character(0))
  }
  out <- focus_name
  if (!is.na(focus$partner_name)) {
    out <- c(out, focus$partner_name)
  }
  if (!is.na(focus$mother_name)) {
    out <- c(out, focus$mother_name)
  }
  if (!is.na(focus$father_name)) {
    out <- c(out, focus$father_name)
  }
  kids <- children_of(family, focus_name)
  if (nrow(kids) > 0) {
    out <- c(out, kids$name)
  }
  unique(out)
}
