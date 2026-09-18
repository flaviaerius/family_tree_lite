# Cartão da pessoa buscada, na barra lateral: foto, nome completo, aniversário e
# parentesco. Ocupa o espaço que antes era gasto documentando as colunas do CSV.

# Como citar alguém que aparece só como parente. Um placeholder não tem ficha,
# então não adianta mostrar o nome interno ("placeholder_1") como se fosse gente.
person_ref_label <- function(row) {
  if (is.null(row) || nrow(row) == 0) {
    return(character(0))
  }
  if (isTRUE(row$is_placeholder)) {
    if (is_anonymous_placeholder(row$name)) {
      if (identical(row$sex, "F")) {
        "não registrada"
      } else if (identical(row$sex, "M")) {
        "não registrado"
      } else {
        "não registrado(a)"
      }
    } else {
      paste0(row$name, " (sem ficha)")
    }
  } else {
    row$name
  }
}

person_card_ui <- function(family, name) {
  person <- person_by_name(family, name)
  if (is.null(person)) {
    return(NULL)
  }

  real <- if ("is_placeholder" %in% names(family)) {
    family[!family$is_placeholder, , drop = FALSE]
  } else {
    family
  }
  accent <- nucleus_color_for(nucleus_color_map(real$nucleus), person$nucleus)

  avatar <- if (has_image(person$image_file)) {
    tags$img(
      src = person$image_file,
      alt = person$name,
      style = paste0(
        "width:76px;height:76px;border-radius:50%;object-fit:cover;",
        "flex:0 0 auto;border:3px solid ",
        accent,
        ";"
      )
    )
  } else {
    tags$div(
      style = paste0(
        "width:76px;height:76px;border-radius:50%;flex:0 0 auto;",
        "border:3px solid ",
        accent,
        ";background:#fff;color:",
        accent,
        ";display:flex;align-items:center;justify-content:center;",
        "font-size:30px;font-weight:600;"
      ),
      toupper(substr(person$short_name, 1, 1))
    )
  }

  born <- format_birthday_pt(person$birth_date, person$birth_year)
  died <- if (!is.na(person$death_year)) {
    paste0("Falecimento: ", person$death_year)
  } else {
    NULL
  }

  rel_line <- function(label, value) {
    value <- value[!is.na(value) & nzchar(value)]
    if (length(value) == 0) {
      return(NULL)
    }
    tags$div(
      style = "font-size:0.8rem;color:#566573;margin-top:2px;",
      tags$strong(label),
      " ",
      paste(value, collapse = ", ")
    )
  }

  kids <- children_of(family, name)
  kid_names <- if (nrow(kids) > 0) {
    kids$short_name[order(kids$birth_year, kids$name, na.last = TRUE)]
  } else {
    character(0)
  }

  tags$div(
    style = paste0(
      "border:1px solid #E5E8EA;border-left:4px solid ",
      accent,
      ";border-radius:6px;padding:12px;background:#FFF;margin-bottom:8px;"
    ),
    tags$div(
      style = "display:flex;gap:12px;align-items:center;",
      avatar,
      tags$div(
        style = "min-width:0;",
        tags$div(
          style = "font-weight:600;font-size:0.95rem;line-height:1.2;color:#2C3E50;",
          person$name
        ),
        if (nzchar(born)) {
          tags$div(
            style = "font-size:0.82rem;color:#566573;margin-top:3px;",
            "Nascimento: ",
            born
          )
        },
        if (!is.null(died)) {
          tags$div(style = "font-size:0.82rem;color:#566573;", died)
        },
        tags$div(
          style = paste0(
            "display:inline-block;margin-top:5px;padding:1px 8px;",
            "border-radius:10px;font-size:0.72rem;color:#FFF;background:",
            accent,
            ";"
          ),
          if (is.na(person$nucleus)) "Família" else person$nucleus,
          " - geração ",
          person$generation
        )
      )
    ),
    tags$div(
      style = "margin-top:10px;",
      rel_line("Mãe:", person_ref_label(mother_of(family, name))),
      rel_line("Pai:", person_ref_label(father_of(family, name))),
      rel_line("Cônjuge:", person_ref_label(partner_of(family, name))),
      rel_line("Filhos:", kid_names)
    ),
    actionButton(
      "show_tree",
      "Ver árvore desta pessoa",
      class = "btn-sm btn-primary",
      style = "margin-top:10px;width:100%;"
    )
  )
}
