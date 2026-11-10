# Geração calculada pela árvore, a partir de mãe, pai e cônjuge.
#
# A coluna `generation` do CSV é opcional: o que vier vazio é calculado aqui.
# As regras são as da própria planilha:
#
#   - filho fica uma geração abaixo da mãe e do pai;
#   - cônjuges (partner_name, inclusive ex) e quem teve filho junto ficam na
#     mesma geração;
#   - a geração 1 é a mais antiga entre as pessoas com linha na tabela. Pais
#     citados sem ficha (os "Bisa Ausente" da raiz) só servem de ligação.
#
# Pessoa e cônjuge viram um nó só; os nós se ligam por pai/mãe → filho. A
# numeração corre em largura a partir do nó com a descendência mais longa (os
# fundadores), subindo e descendo pelas ligações: assim os antepassados de quem
# casou na família ficam acima do cônjuge, e os irmãos de cada um ficam na
# geração deles. Quando a planilha se contradiz (casamento entre gerações
# diferentes, por exemplo), vale o caminho mais curto até os fundadores.
#
# Grupos sem ligação entre si (duas famílias sem casamento em comum) são
# numerados cada um por conta própria.

# Geração calculada para cada linha de `family`, alinhada com family$name.
# Onde o CSV já traz a geração, a numeração do grupo é deslocada para concordar
# com ela (pelo deslocamento mais frequente), e não começa obrigatoriamente em 1.
infer_generations <- function(family) {
  n_rows <- nrow(family)
  if (n_rows == 0) {
    return(integer(0))
  }
  ref <- function(x) {
    x <- as.character(x)
    ifelse(!is.na(x) & nzchar(trimws(x)), x, NA_character_)
  }
  mother <- ref(family$mother_name)
  father <- ref(family$father_name)
  partner <- ref(family$partner_name)
  people <- unique(c(family$name, stats::na.omit(c(mother, father, partner))))
  id <- function(x) match(x, people)

  # Union-find: pessoa + cônjuge + co-genitor = um nó.
  root <- seq_along(people)
  find <- function(i) {
    while (root[i] != i) i <- root[i]
    i
  }
  join <- function(a, b) {
    if (is.na(a) || is.na(b)) {
      return(invisible())
    }
    ra <- find(a)
    rb <- find(b)
    if (ra != rb) root[rb] <<- ra
    invisible()
  }
  for (i in seq_len(n_rows)) {
    join(id(family$name[i]), id(partner[i]))
    join(id(mother[i]), id(father[i]))
  }
  node <- vapply(seq_along(people), find, integer(1))

  # Ligações pai/mãe → filho entre nós (sem repetição e sem laço no mesmo nó,
  # que só aparece quando a planilha se contradiz).
  from <- integer(0)
  to <- integer(0)
  for (i in seq_len(n_rows)) {
    child <- node[id(family$name[i])]
    for (p in c(mother[i], father[i])) {
      if (is.na(p)) next
      parent <- node[id(p)]
      if (parent != child) {
        from <- c(from, parent)
        to <- c(to, child)
      }
    }
  }
  keep <- !duplicated(paste(from, to))
  from <- from[keep]
  to <- to[keep]
  nodes <- unique(node)
  kids <- split(to, factor(from, levels = nodes))
  parents <- split(from, factor(to, levels = nodes))
  key <- as.character

  # Descendência mais longa de cada nó, para escolher por onde começar.
  depth <- stats::setNames(rep(NA_integer_, length(nodes)), nodes)
  visiting <- stats::setNames(rep(FALSE, length(nodes)), nodes)
  down_depth <- function(n) {
    k <- key(n)
    if (!is.na(depth[[k]])) {
      return(depth[[k]])
    }
    if (visiting[[k]]) {
      return(0L) # ciclo na planilha: não deixa a recursão girar
    }
    visiting[[k]] <<- TRUE
    ch <- kids[[k]]
    d <- if (length(ch) == 0) 0L else 1L + max(vapply(ch, down_depth, integer(1)))
    depth[[k]] <<- d
    d
  }
  for (n in nodes) down_depth(n)

  # Largura a partir do nó de descendência mais longa de cada grupo; no
  # empate, o que aparece primeiro na planilha.
  row_nodes <- node[id(family$name)]
  order_nodes <- unique(c(row_nodes, nodes))
  level <- stats::setNames(rep(NA_integer_, length(nodes)), nodes)
  group <- stats::setNames(rep(NA_integer_, length(nodes)), nodes)
  n_groups <- 0L
  repeat {
    pending <- order_nodes[is.na(level[key(order_nodes)])]
    if (length(pending) == 0) break
    start <- pending[which.max(depth[key(pending)])]
    n_groups <- n_groups + 1L
    level[[key(start)]] <- 0L
    group[[key(start)]] <- n_groups
    queue <- start
    while (length(queue) > 0) {
      cur <- queue[1]
      queue <- queue[-1]
      lv <- level[[key(cur)]]
      nb <- c(kids[[key(cur)]], parents[[key(cur)]])
      step <- c(rep(1L, length(kids[[key(cur)]])), rep(-1L, length(parents[[key(cur)]])))
      for (j in seq_along(nb)) {
        k <- key(nb[j])
        if (!is.na(level[[k]])) next
        level[[k]] <- lv + step[j]
        group[[k]] <- n_groups
        queue <- c(queue, nb[j])
      }
    }
  }

  gen <- unname(level[key(row_nodes)])
  grp <- unname(group[key(row_nodes)])
  given <- if ("generation" %in% names(family)) {
    suppressWarnings(as.integer(family$generation))
  } else {
    rep(NA_integer_, n_rows)
  }
  for (g in unique(grp)) {
    in_g <- grp == g
    diff <- given[in_g] - gen[in_g]
    diff <- diff[!is.na(diff)]
    offset <- if (length(diff) > 0) {
      as.integer(names(which.max(table(diff))))
    } else {
      1L - min(gen[in_g])
    }
    gen[in_g] <- gen[in_g] + offset
  }
  as.integer(gen)
}

# Preenche as gerações vazias com a calculada. Devolve a tabela e um resumo:
# quem foi preenchido e quem tem na planilha uma geração diferente da árvore.
fill_generations <- function(family) {
  if (!("generation" %in% names(family))) family$generation <- NA_integer_
  given <- suppressWarnings(as.integer(family$generation))
  computed <- infer_generations(family)
  filled <- is.na(given) & !is.na(computed)
  divergent <- !is.na(given) & !is.na(computed) & given != computed
  family$generation <- ifelse(is.na(given), computed, given)
  family$generation <- as.integer(family$generation)
  list(
    family = family,
    filled = family$name[filled],
    divergent = data.frame(
      name = family$name[divergent],
      given = given[divergent],
      computed = computed[divergent],
      stringsAsFactors = FALSE
    )
  )
}

# Linhas do modal de carregamento: quantas gerações vieram da árvore e quem
# tem na planilha uma geração que não bate com a de pai e mãe.
generation_status_ui <- function(report) {
  if (is.null(report)) {
    return(NULL)
  }
  out <- list()
  n_filled <- length(report$filled)
  if (n_filled > 0) {
    out[[length(out) + 1]] <- tags$div(
      icon("circle-check"), " ",
      if (n_filled == 1) "1 geração calculada" else paste(n_filled, "gerações calculadas"),
      " pela árvore (mãe, pai e cônjuge)"
    )
  }
  div <- report$divergent
  if (NROW(div) > 0) {
    shown <- utils::head(div, 5)
    items <- paste0(shown$name, " (", shown$given, "ª na planilha, ", shown$computed, "ª pela árvore)")
    if (nrow(div) > 5) items <- c(items, paste("e mais", nrow(div) - 5))
    out[[length(out) + 1]] <- tags$div(
      style = "color:#9A6B00;",
      icon("triangle-exclamation"), " ",
      "Geração diferente da calculada pela árvore (vale a da planilha): ",
      paste(items, collapse = "; ")
    )
  }
  if (length(out) == 0) NULL else out
}
