library(testthat)
suppressWarnings(suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
}))

for (f in list.files(file.path("..", "R"), full.names = TRUE)) source(f)

family <- load_family_csv(file.path("fixtures", "family_sample.csv"))
layout <- build_overview_layout(family)
edges <- build_overview_edges(layout)

# Todo grupo de filhos e o co-genitor que o define, para um dado pai/mãe.
coparent_groups <- function(layout) {
  out <- list()
  for (i in seq_len(nrow(layout))) {
    if (is.na(layout$x[i])) next
    nm <- layout$name[i]
    is_mother <- !is.na(layout$mother_name) & layout$mother_name == nm
    is_father <- !is.na(layout$father_name) & layout$father_name == nm
    kids <- which(is_mother | is_father)
    if (length(kids) == 0) next
    other <- vapply(
      kids,
      function(ci) {
        if (is_mother[ci]) layout$father_name[ci] else layout$mother_name[ci]
      },
      character(1)
    )
    for (k in unique(other)) {
      out[[length(out) + 1L]] <- list(parent = nm, coparent = k)
    }
  }
  out
}

test_that("placeholder_1 e placeholder_2 viram duas pessoas distintas", {
  ph <- family[family$is_placeholder, ]
  expect_true(all(c("placeholder_1", "placeholder_2") %in% ph$name))
  expect_equal(family$short_name[family$name == "placeholder_1"], "?")
  # Os dois filhos de placeholder_1 continuam apontando para a MESMA pessoa,
  # e o de placeholder_2 para outra — é isso que a convenção existe para dizer.
  expect_equal(
    family$mother_name[family$name == "Filha Ph1"],
    family$mother_name[family$name == "Filho Ph1"]
  )
  expect_false(identical(
    family$mother_name[family$name == "Filha Ph1"],
    family$mother_name[family$name == "Filha Ph2"]
  ))
})

test_that("co-genitor citado mas sem ficha vira linha", {
  expect_true("Pai Sem Ficha" %in% family$name)
  expect_true(family$is_placeholder[family$name == "Pai Sem Ficha"])
  expect_false(is_anonymous_placeholder("Pai Sem Ficha"))
})

test_that("genitor vazio vira placeholder anônimo automático", {
  filha <- family[family$name == "Filha Solteiro", ]
  expect_false(is.na(filha$mother_name))
  expect_true(is_anonymous_placeholder(filha$mother_name))
  expect_true(family$is_placeholder[family$name == filha$mother_name])
})

test_that("pais da geração-raiz não viram linha", {
  # O layout parte da geração 1 e só desce: eles ficariam com x = NA, invisíveis.
  expect_false("Bisa Ausente" %in% family$name)
  expect_false("Bisavo Ausente" %in% family$name)
})

test_that("normalizar duas vezes não duplica nada", {
  expect_equal(nrow(add_placeholder_parents(family)), nrow(family))
})

test_that("toda pessoa recebe uma posição", {
  expect_false(any(is.na(layout$x)))
})

test_that("todo grupo de filhos sai do x de um pai/mãe posicionado", {
  # O bug: quando o co-genitor não tinha linha na tabela, o barramento era
  # ancorado na média dos filhos e o ramo ficava solto, sem ligação nenhuma.
  for (g in coparent_groups(layout)) {
    px <- layout$x[layout$name == g$coparent]
    expect_length(px, 1L)
    expect_false(
      is.na(px),
      info = paste0("ramo solto: ", g$parent, " -> ", g$coparent)
    )
  }
})

test_that("parceiros ficam lado a lado", {
  # O bug: o espelhamento das parcerias múltiplas afastava o parceiro na
  # proporção da largura do bloco de filhos (chegou a 47 unidades).
  # O casal mais aberto hoje é o em destaque, que precisa caber o primeiro nome
  # em letra grande: COUPLE_DX_HIGHLIGHT = 2.4 de cada lado, ou seja 4.8.
  seen <- character(0)
  for (i in seq_len(nrow(layout))) {
    pn <- layout$partner_name[i]
    if (is.na(pn)) next
    j <- which(layout$name == pn)
    if (length(j) != 1L || is.na(layout$x[i]) || is.na(layout$x[j])) next
    key <- paste(sort(c(layout$name[i], pn)), collapse = "|")
    if (key %in% seen) next
    seen <- c(seen, key)
    expect_lte(abs(layout$x[i] - layout$x[j]), 4.81)
  }
})

test_that("ninguém se sobrepõe dentro da mesma geração", {
  for (g in unique(layout$generation)) {
    xs <- sort(layout$x[layout$generation == g & !is.na(layout$x)])
    if (length(xs) < 2) next
    expect_gte(min(diff(xs)), 0.5)
  }
})

test_that("parcerias múltiplas ganham barramentos em alturas distintas", {
  # Com os parceiros agora agrupados junto do primário, barramentos na mesma
  # altura se sobreporiam na horizontal.
  px <- layout$x[layout$name == "Pai Multi"]
  py <- layout$y[layout$name == "Pai Multi"]
  mids <- vapply(
    c("Esposa Atual", "placeholder_1", "placeholder_2"),
    function(nm) (px + layout$x[layout$name == nm]) / 2,
    numeric(1)
  )
  drops <- edges[
    edges$kind == "child" &
      edges$y == py &
      edges$x == edges$xend &
      vapply(edges$x, function(v) any(abs(v - mids) < 1e-9), logical(1)),
    ,
    drop = FALSE
  ]
  expect_equal(nrow(drops), 3L) # três parcerias, três descidas
  expect_equal(length(unique(drops$yend)), 3L) # cada uma na sua altura
})

test_that("o tracejado só aparece para quem não é o cônjuge atual", {
  px <- layout$x[layout$name == "Pai Multi"]
  from_multi <- edges[
    edges$kind == "partner_former" & edges$x == px, ,
    drop = FALSE
  ]
  expect_equal(nrow(from_multi), 2L) # placeholder_1 e placeholder_2
  expect_setequal(
    from_multi$xend,
    layout$x[layout$name %in% c("placeholder_1", "placeholder_2")]
  )
  esposa_x <- layout$x[layout$name == "Esposa Atual"]
  expect_false(any(from_multi$xend == esposa_x))
})

test_that("format_birthday_pt cobre data completa, só ano e vazio", {
  expect_equal(format_birthday_pt(as.Date("1992-03-14"), 1992L), "14 de março de 1992")
  expect_equal(format_birthday_pt(NA, 1992L), "1992")
  expect_equal(format_birthday_pt(NA, NA), "")
})

test_that("birth_date é lido e o ano é derivado dele", {
  filho <- family[family$name == "Filho Nomeado", ]
  expect_equal(as.character(filho$birth_date), "1965-07-09")
  expect_equal(
    format_birthday_pt(filho$birth_date, filho$birth_year),
    "9 de julho de 1965"
  )
})

test_that("placeholders não entram no cartão como se fossem gente", {
  expect_equal(
    person_ref_label(mother_of(family, "Filha Solteiro")),
    "não registrada"
  )
  expect_equal(
    person_ref_label(father_of(family, "Filho Nomeado")),
    "Pai Sem Ficha (sem ficha)"
  )
  expect_equal(person_ref_label(father_of(family, "Filho Atual")), "Pai Multi")
})

test_that("os rótulos do estado padrão não colidem, em zoom nenhum", {
  # A fonte e o espaçamento derivam dos mesmos pixels por unidade, então a
  # largura do rótulo em unidades de dados é constante: se dois colidem,
  # colidem em QUALQUER zoom — aproximar não separa.
  texts <- overview_label_texts(layout, layout$is_placeholder)
  placed <- which(!is.na(layout$x))
  width <- nchar(gsub("<[^>]+>", "", texts$default)) *
    LABEL_CHAR_WIDTH * LABEL_FONT_RATIO * 0.9 * texts$scale
  xs <- layout$x[placed]
  gens <- layout$generation[placed]
  for (g in unique(gens)) {
    idx <- which(gens == g)
    if (length(idx) < 2) next
    o <- idx[order(xs[idx])]
    for (k in seq_len(length(o) - 1)) {
      a <- o[k]
      b <- o[k + 1]
      expect_gte(
        xs[b] - xs[a],
        (width[a] + width[b]) / 2,
        label = paste0(
          gsub("<[^>]+>", "", texts$default[a]), " / ",
          gsub("<[^>]+>", "", texts$default[b])
        )
      )
    }
  }
})

test_that("no limiar calculado, os primeiros nomes cabem", {
  # Acima de first_name_zoom_threshold a fonte já travou em LABEL_MAX_FONT_PX,
  # então a largura do nome em pixels para de crescer enquanto o espaçamento
  # continua — é só aí que os nomes se separam.
  texts <- overview_label_texts(layout, layout$is_placeholder)
  px_per_unit <- first_name_zoom_threshold(layout, texts)
  expect_gt(px_per_unit, 0)
  placed <- which(!is.na(layout$x))
  width_px <- nchar(gsub("<[^>]+>", "", texts$zoomed)) *
    LABEL_CHAR_WIDTH * LABEL_MAX_FONT_PX * texts$scale
  xs <- layout$x[placed]
  gens <- layout$generation[placed]
  for (g in unique(gens)) {
    idx <- which(gens == g)
    if (length(idx) < 2) next
    o <- idx[order(xs[idx])]
    for (k in seq_len(length(o) - 1)) {
      a <- o[k]
      b <- o[k + 1]
      expect_gte(
        (xs[b] - xs[a]) * px_per_unit,
        (width_px[a] + width_px[b]) / 2,
        label = paste0(
          gsub("<[^>]+>", "", texts$zoomed[a]), " / ",
          gsub("<[^>]+>", "", texts$zoomed[b])
        )
      )
    }
  }
})

test_that("as duas versões do rótulo batem com as anotações", {
  texts <- overview_label_texts(layout, layout$is_placeholder)
  anns <- build_overview_labels(layout, layout$is_placeholder, texts)
  expect_equal(length(anns), sum(!is.na(layout$x)))
  expect_equal(vapply(anns, function(a) a$text, character(1)), texts$default)
  # placeholders mantêm o itálico nas duas versões
  ph <- which(layout$is_placeholder[!is.na(layout$x)])
  expect_true(all(grepl("^<i>", texts$default[ph])))
  expect_true(all(grepl("^<i>", texts$zoomed[ph])))
  # e todos ficam numa linha só
  ys <- vapply(anns, function(a) a$y, numeric(1))
  expect_equal(ys, layout$y[!is.na(layout$x)] + LABEL_DY)
})

test_that("destaque mostra o primeiro nome em letra maior; o resto, em tamanho normal", {
  texts <- overview_label_texts(layout, layout$is_placeholder)
  placed <- which(!is.na(layout$x))
  hl <- overview_label_highlight(layout)[placed]

  # a geração mais antiga de cada núcleo e o tronco fundador entram em destaque
  expect_true(all(c("Raiz Um", "Raiz Dois") %in% layout$name[placed][hl]))
  expect_equal(texts$default[layout$name[placed] == "Raiz Um"], "Raiz")
  expect_equal(texts$scale[layout$name[placed] == "Raiz Um"], LABEL_HIGHLIGHT_SCALE)

  # os demais também mostram o primeiro nome, só que em tamanho normal
  fn <- which(layout$name[placed] == "Filho Nomeado")
  expect_equal(texts$default[fn], "FilhoN")
  expect_equal(texts$zoomed[fn], "FilhoN")
  expect_equal(texts$scale[fn], 1)
})

test_that("ex-cônjuge fica em tamanho normal mesmo estando na geração mais antiga", {
  texts <- overview_label_texts(layout, layout$is_placeholder)
  placed <- which(!is.na(layout$x))
  ex <- which(layout$is_former_partner_node[placed])
  expect_gt(length(ex), 0)
  expect_true(all(texts$scale[ex] == 1))
})

test_that("um lado só basta para reconhecer o casal", {
  # Na planilha há casais em que só um dos dois preencheu partner_name; sem a
  # checagem simétrica o outro virava ex-cônjuge, com linha tracejada.
  nora <- which(layout$name == "Nora")
  expect_false(layout$is_former_partner_node[nora])
})

test_that("barramentos ficam abaixo do rótulo e acima da bola do filho", {
  limite_inferior <- LABEL_DY + 0.55 * 0.9 * 1.25 / GEN_ASPECT
  limite_superior <- 1 - 0.45 / GEN_ASPECT
  buses <- edges[edges$kind == "child" & edges$y != edges$yend, , drop = FALSE]
  tops <- pmax(buses$y, buses$yend)
  offsets <- unique(tops - floor(tops))
  offsets <- offsets[offsets > 0]
  expect_true(all(offsets > limite_inferior))
  expect_true(all(offsets < limite_superior))
})

test_that("o teto de fonte do R e o do JS são o mesmo número", {
  # first_name_zoom_threshold() só está certo se LABEL_MAX_FONT_PX for de fato o
  # valor em que a fonte trava no navegador. Os dois vivem em arquivos
  # diferentes, então vale travar o par.
  js <- readLines(file.path("..", "www", "tree_scale.js"), warn = FALSE)
  js_const <- function(nome) {
    hit <- grep(paste0("^\\s*var ", nome, "\\s*="), js, value = TRUE)
    expect_length(hit, 1L)
    as.numeric(sub(paste0("^\\s*var ", nome, "\\s*=\\s*([0-9.]+).*"), "\\1", hit))
  }
  expect_equal(js_const("MAX_FONT_PX"), LABEL_MAX_FONT_PX)
  expect_equal(js_const("FONT_RATIO"), LABEL_FONT_RATIO)
})
