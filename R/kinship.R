# Parentesco entre duas pessoas (aba "Parentesco").
#
# O grafo sai das colunas mother_name/father_name lidas como texto, não das
# linhas da tabela: assim entram também os pais da geração-raiz citados sem
# ficha ("Bisa Ausente") e os placeholders de R/placeholders.R — dois filhos com
# `placeholder_1` como mãe são irmãos completos.
#
# Os nomes seguem o uso popular brasileiro: tataravó, tatataravó...; primo de
# primeiro grau para filhos de irmãos; e, entre primos de gerações diferentes,
# o mais velho é "tio de segundo grau" e o mais novo "sobrinho de segundo grau".

ORDINAL_WORDS <- c(
  "primeiro", "segundo", "terceiro", "quarto", "quinto",
  "sexto", "sétimo", "oitavo", "nono", "décimo"
)

# Termos formais a partir da quarta geração (tataravô = trisavô).
ANCESTOR_FORMAL <- c("trisavô", "tetravô", "pentavô", "hexavô", "heptavô", "octavô", "eneavô")
DESCENDANT_FORMAL <- c("trineto", "tetraneto", "pentaneto", "hexaneto", "heptaneto", "octaneto", "eneaneto")

ordinal_word <- function(n) {
  if (n >= 1 && n <= length(ORDINAL_WORDS)) ORDINAL_WORDS[n] else paste0(n, "º")
}

# Sexo de quem pode não ter linha própria: aí o papel de mãe ou pai decide.
kin_sex <- function(family, name) {
  s <- family$sex[match(name, family$name)]
  if (length(s) == 1 && !is.na(s) && s %in% c("F", "M")) {
    return(s)
  }
  if (name %in% family$mother_name) {
    return("F")
  }
  if (name %in% family$father_name) {
    return("M")
  }
  NA_character_
}

pick_sex <- function(sex, m, f) if (identical(sex, "F")) f else m

of_article <- function(sex) {
  if (identical(sex, "F")) "da" else if (identical(sex, "M")) "do" else "de"
}

spouse_word <- function(sex) {
  if (identical(sex, "F")) "esposa" else if (identical(sex, "M")) "marido" else "cônjuge"
}

# `n` gerações acima: pai, avô, bisavô, tataravô, tatataravô...
ancestor_word <- function(n, sex, formal = FALSE) {
  f <- identical(sex, "F")
  word <- switch(
    as.character(n),
    "1" = if (f) "mãe" else "pai",
    "2" = if (f) "avó" else "avô",
    "3" = if (f) "bisavó" else "bisavô",
    paste0(strrep("ta", n - 2), if (f) "ravó" else "ravô")
  )
  if (formal && n >= 4 && n - 3 <= length(ANCESTOR_FORMAL)) {
    tech <- ANCESTOR_FORMAL[n - 3]
    if (f) tech <- sub("ô$", "ó", tech)
    word <- paste0(word, " (", tech, ")")
  }
  word
}

# `n` gerações abaixo: filho, neto, bisneto, tataraneto, tatataraneto...
descendant_word <- function(n, sex, formal = FALSE) {
  f <- identical(sex, "F")
  word <- switch(
    as.character(n),
    "1" = if (f) "filha" else "filho",
    "2" = if (f) "neta" else "neto",
    "3" = if (f) "bisneta" else "bisneto",
    paste0(strrep("ta", n - 2), if (f) "raneta" else "raneto")
  )
  if (formal && n >= 4 && n - 3 <= length(DESCENDANT_FORMAL)) {
    tech <- DESCENDANT_FORMAL[n - 3]
    if (f) tech <- sub("o$", "a", tech)
    word <- paste0(word, " (", tech, ")")
  }
  word
}

# Irmão de um ancestral `n` gerações acima: tio, tio-avô, tio-bisavô...
uncle_word <- function(n, sex) {
  base <- pick_sex(sex, "tio", "tia")
  if (n == 1) base else paste0(base, "-", ancestor_word(n, sex))
}

# Descendente de um irmão, `n` gerações abaixo: sobrinho, sobrinho-neto...
nephew_word <- function(n, sex) {
  base <- pick_sex(sex, "sobrinho", "sobrinha")
  if (n == 1) base else paste0(base, "-", descendant_word(n, sex))
}

# O que A é de B, sabendo que A está `da` gerações abaixo do ancestral comum e
# B está `db`. Sem sexo conhecido, mostra as duas formas.
kinship_term <- function(da, db, sex) {
  if (!(identical(sex, "F") || identical(sex, "M"))) {
    return(paste(kinship_term(da, db, "M"), kinship_term(da, db, "F"), sep = " / "))
  }
  if (da == 0) {
    return(ancestor_word(db, sex, formal = TRUE))
  }
  if (db == 0) {
    return(descendant_word(da, sex, formal = TRUE))
  }
  if (da == 1 && db == 1) {
    return(pick_sex(sex, "irmão", "irmã"))
  }
  if (da == 1) {
    return(uncle_word(db - 1, sex))
  }
  if (db == 1) {
    return(nephew_word(da - 1, sex))
  }
  if (da == db) {
    return(paste(pick_sex(sex, "primo", "prima"), "de", ordinal_word(da - 1), "grau"))
  }
  gap <- abs(da - db)
  base <- if (da < db) uncle_word(gap, sex) else nephew_word(gap, sex)
  paste(base, "de", ordinal_word(min(da, db)), "grau")
}

kin_parents <- function(family, name) {
  i <- match(name, family$name)
  if (is.na(i)) {
    return(character(0))
  }
  p <- c(family$mother_name[i], family$father_name[i])
  unique(p[!is.na(p) & nzchar(p)])
}

# Todos os ancestrais de `name` (ele mesmo incluído, a distância 0), com a
# menor distância e o filho pelo qual se chegou a cada um (`via`).
ancestor_distances <- function(family, name) {
  nm <- name
  dist <- 0L
  via <- NA_character_
  frontier <- name
  d <- 0L
  repeat {
    d <- d + 1L
    nxt <- character(0)
    nxt_via <- character(0)
    for (child in frontier) {
      ps <- kin_parents(family, child)
      ps <- ps[!(ps %in% nm) & !(ps %in% nxt)]
      nxt <- c(nxt, ps)
      nxt_via <- c(nxt_via, rep(child, length(ps)))
    }
    if (length(nxt) == 0) break
    nm <- c(nm, nxt)
    dist <- c(dist, rep(d, length(nxt)))
    via <- c(via, nxt_via)
    frontier <- nxt
  }
  data.frame(name = nm, dist = dist, via = via, stringsAsFactors = FALSE)
}

# De um ancestral de volta até a pessoa de partida: c(ancestral, ..., pessoa).
lineage <- function(anc, target) {
  out <- target
  v <- anc$via[match(target, anc$name)]
  while (!is.na(v)) {
    out <- c(out, v)
    v <- anc$via[match(v, anc$name)]
  }
  out
}

# Parentesco de sangue pelo ancestral comum mais próximo, ou NULL. O caminho é
# um data.frame (name, edge), em que `edge` é a ligação com o nó anterior:
# "up" (sobe para pai/mãe), "down" (desce para filho) ou "spouse".
blood_relation <- function(family, a, b) {
  anc_a <- ancestor_distances(family, a)
  anc_b <- ancestor_distances(family, b)
  common <- merge(anc_a, anc_b, by = "name", suffixes = c("_a", "_b"))
  if (nrow(common) == 0) {
    return(NULL)
  }
  # Mais perto primeiro; num empate, a linha reta ganha da colateral.
  common <- common[order(
    common$dist_a + common$dist_b,
    pmin(common$dist_a, common$dist_b)
  ), , drop = FALSE]
  da <- common$dist_a[1]
  db <- common$dist_b[1]
  best <- common$name[common$dist_a == da & common$dist_b == db]

  up <- rev(lineage(anc_a, best[1]))
  down <- lineage(anc_b, best[1])[-1]
  path <- data.frame(
    name = c(up, down),
    edge = c(NA, rep("up", da), rep("down", db)),
    stringsAsFactors = FALSE
  )

  # Por parte de quem, olhando de B: o genitor de B que está no caminho.
  side <- NA_character_
  if (db >= 1) {
    b_row <- match(b, family$name)
    sides <- unique(vapply(best, function(anc) {
      lin <- lineage(anc_b, anc)
      parent <- lin[length(lin) - 1]
      if (identical(parent, family$mother_name[b_row])) {
        "mãe"
      } else if (identical(parent, family$father_name[b_row])) {
        "pai"
      } else {
        NA_character_
      }
    }, character(1)))
    if (length(sides) == 1) side <- sides
  }

  list(da = da, db = db, common = best, path = path, side = side)
}

# Cônjuges atuais: vale o `partner_name` dos dois lados, mas quem aponta para
# alguém que já aponta para outra pessoa é ex (mesma regra de
# current_spouse_of, em R/birthdays.R).
is_current_couple <- function(family, x, y) {
  px <- family$partner_name[match(x, family$name)]
  py <- family$partner_name[match(y, family$name)]
  (identical(px, y) && (is.na(py) || identical(py, x))) ||
    (identical(py, x) && (is.na(px) || identical(px, y)))
}

spouses_of <- function(family, name) {
  own <- family$partner_name[match(name, family$name)]
  cand <- unique(c(own, family$name[!is.na(family$partner_name) & family$partner_name == name]))
  cand <- cand[!is.na(cand) & cand != name]
  cand[vapply(cand, function(s) is_current_couple(family, name, s), logical(1))]
}

# Primeiro filho que os dois tiveram juntos, ou NA.
shared_child <- function(family, x, y) {
  hit <- (family$mother_name %in% x & family$father_name %in% y) |
    (family$mother_name %in% y & family$father_name %in% x)
  if (any(hit)) family$name[which(hit)[1]] else NA_character_
}

path_df <- function(name, edge) data.frame(name = name, edge = edge, stringsAsFactors = FALSE)

# Parentesco por afinidade: no máximo um salto de cônjuge em cada ponta (e o
# caso do filho do padrasto/da madrasta). Fica o candidato de caminho mais
# curto; nomes próprios (padrasto, sogra, cunhado...) quando existem, frase
# montada ("marido da prima de primeiro grau") quando não.
affinity_relation <- function(family, a, b) {
  sex_a <- kin_sex(family, a)
  sp_a <- spouses_of(family, a)
  sp_b <- spouses_of(family, b)
  cands <- list()
  add <- function(term, path, extra) {
    cands[[length(cands) + 1]] <<- list(term = term, path = path, cost = extra + nrow(path))
  }

  # A é cônjuge de um parente de B.
  for (s in sp_a) {
    r <- blood_relation(family, s, b)
    if (is.null(r)) next
    term <- if (r$da == 0 && r$db == 1) {
      pick_sex(sex_a, "padrasto", "madrasta")
    } else if (r$da == 1 && r$db == 0) {
      pick_sex(sex_a, "genro", "nora")
    } else if (r$da == 1 && r$db == 1) {
      pick_sex(sex_a, "cunhado", "cunhada")
    } else {
      sex_s <- kin_sex(family, s)
      paste(spouse_word(sex_a), of_article(sex_s), kinship_term(r$da, r$db, sex_s))
    }
    add(term, rbind(path_df(a, NA), transform(r$path, edge = replace(edge, 1, "spouse"))), 0)
  }

  # A é parente do cônjuge de B.
  for (t in sp_b) {
    r <- blood_relation(family, a, t)
    if (is.null(r)) next
    sex_t <- kin_sex(family, t)
    term <- if (r$da == 0 && r$db == 1) {
      pick_sex(sex_a, "sogro", "sogra")
    } else if (r$da == 1 && r$db == 0) {
      pick_sex(sex_a, "enteado", "enteada")
    } else if (r$da == 1 && r$db == 1) {
      pick_sex(sex_a, "cunhado", "cunhada")
    } else {
      paste(kinship_term(r$da, r$db, sex_a), of_article(sex_t), spouse_word(sex_t))
    }
    add(term, rbind(r$path, path_df(b, "spouse")), 0)
  }

  # Cônjuge de um parente do cônjuge.
  for (s in sp_a) {
    for (t in sp_b) {
      r <- blood_relation(family, s, t)
      if (is.null(r)) next
      sex_s <- kin_sex(family, s)
      sex_t <- kin_sex(family, t)
      term <- if (r$da == 1 && r$db == 1) {
        pick_sex(sex_a, "concunhado", "concunhada")
      } else {
        paste(
          spouse_word(sex_a), of_article(sex_s), kinship_term(r$da, r$db, sex_s),
          of_article(sex_t), spouse_word(sex_t)
        )
      }
      path <- rbind(
        path_df(a, NA),
        transform(r$path, edge = replace(edge, 1, "spouse")),
        path_df(b, "spouse")
      )
      add(term, path, 0)
    }
  }

  # Filho(a) do cônjuge de um genitor de B que não é genitor de B.
  for (p in kin_parents(family, b)) {
    for (q in spouses_of(family, p)) {
      if (q %in% kin_parents(family, b) || !(q %in% kin_parents(family, a))) next
      step <- if (identical(kin_sex(family, q), "F")) "da madrasta" else "do padrasto"
      term <- paste(pick_sex(sex_a, "filho", "filha"), step)
      add(term, path_df(c(a, q, p, b), c(NA, "up", "spouse", "down")), 0)
    }
  }

  if (length(cands) == 0) {
    return(NULL)
  }
  cands[[which.min(vapply(cands, `[[`, numeric(1), "cost"))]]
}

# O que A é de B. `kind` é "self", "blood", "spouse", "ex", "affinity" ou
# "none"; `common` e `degree` só existem para parentes de sangue.
relationship <- function(family, a, b) {
  out <- list(kind = "none", term = NA_character_, side = NA_character_,
              common = character(0), path = NULL, degree = NA_character_)
  if (identical(a, b)) {
    out$kind <- "self"
    return(out)
  }
  sex_a <- kin_sex(family, a)
  couple <- is_current_couple(family, a, b)

  r <- blood_relation(family, a, b)
  if (!is.null(r)) {
    term <- kinship_term(r$da, r$db, sex_a)
    if (r$da == 1 && r$db == 1 && length(r$common) == 1) {
      term <- pick_sex(sex_a, "meio-irmão", "meia-irmã")
    }
    if (couple) term <- paste0(spouse_word(sex_a), " · também ", term)
    out$kind <- "blood"
    out$term <- term
    # O lado só diz algo com B a duas gerações do ancestral, ou entre meios-irmãos.
    if (r$db >= 2 || (r$da == 1 && r$db == 1 && length(r$common) == 1)) {
      out$side <- r$side
    }
    out$common <- if (r$da == 0 || r$db == 0) character(0) else r$common
    out$path <- r$path
    n <- r$da + r$db
    out$degree <- paste0(
      if (r$da == 0 || r$db == 0) "em linha reta" else "colateral",
      " de ", n, "º grau"
    )
    return(out)
  }

  if (couple) {
    out$kind <- "spouse"
    out$term <- spouse_word(sex_a)
    out$path <- path_df(c(a, b), c(NA, "spouse"))
    return(out)
  }

  kid <- shared_child(family, a, b)
  if (!is.na(kid)) {
    out$kind <- "ex"
    out$term <- pick_sex(sex_a, "ex-companheiro", "ex-companheira")
    out$path <- path_df(c(a, kid, b), c(NA, "down", "up"))
    return(out)
  }

  aff <- affinity_relation(family, a, b)
  if (!is.null(aff)) {
    out$kind <- "affinity"
    out$term <- aff$term
    out$path <- aff$path
  }
  out
}
