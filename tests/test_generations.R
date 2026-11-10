library(testthat)
suppressWarnings(suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
}))

for (f in list.files(file.path("..", "R"), full.names = TRUE)) source(f)

fam_df <- function(...) {
  rows <- list(...)
  data.frame(
    name = vapply(rows, `[[`, "", 1),
    mother_name = vapply(rows, function(r) r[2], ""),
    father_name = vapply(rows, function(r) r[3], ""),
    partner_name = vapply(rows, function(r) r[4], ""),
    stringsAsFactors = FALSE
  ) |>
    mutate(across(-name, ~ ifelse(.x == "", NA, .x)))
}

test_that("sem a coluna generation, a árvore reproduz a do exemplo", {
  raw <- read.csv(file.path("fixtures", "family_sample.csv"),
                  stringsAsFactors = FALSE, na.strings = c("", "NA"))
  expected <- raw$generation
  raw$generation <- NULL
  expect_equal(infer_generations(raw), expected)

  tmp <- tempfile(fileext = ".csv")
  write.csv(raw, tmp, row.names = FALSE, na = "")
  fam <- load_family_csv(tmp)
  real <- !fam$is_placeholder
  expect_equal(fam$generation[real], expected)
  expect_length(attr(fam, "generation_report")$filled, nrow(raw))
})

test_that("antepassados de quem casou na família ficam acima do cônjuge", {
  f <- fam_df(
    c("Avô", "", "", "Avó"), c("Avó", "", "", "Avô"),
    c("Filho", "Avó", "Avô", "Nora"), c("Nora", "Mãe da Nora", "", "Filho"),
    c("Mãe da Nora", "Bisa da Nora", "", ""), c("Bisa da Nora", "", "", ""),
    c("Irmã da Nora", "Mãe da Nora", "", ""),
    c("Neto", "Nora", "Filho", "")
  )
  g <- setNames(infer_generations(f), f$name)
  expect_equal(unname(g[c("Avô", "Avó", "Filho", "Nora", "Neto")]), c(2L, 2L, 3L, 3L, 4L))
  expect_equal(unname(g[c("Mãe da Nora", "Bisa da Nora", "Irmã da Nora")]), c(2L, 1L, 3L))
})

test_that("pai e mãe contam: ex que teve filho junto fica na mesma geração", {
  f <- fam_df(
    c("Raiz", "", "", ""), c("Filha", "", "Raiz", "Marido"),
    c("Marido", "", "", "Filha"), c("Ex", "", "", ""),
    c("Neto Ex", "Filha", "Ex", "")
  )
  g <- setNames(infer_generations(f), f$name)
  expect_equal(unname(g[c("Raiz", "Filha", "Marido", "Ex", "Neto Ex")]), c(1L, 2L, 2L, 2L, 3L))
})

test_that("grupos sem ligação começam cada um na geração 1", {
  f <- fam_df(c("A", "", "", ""), c("A filho", "", "A", ""), c("B", "", "", ""))
  expect_equal(infer_generations(f), c(1L, 2L, 1L))
})

test_that("geração preenchida em parte: o resto segue a numeração da planilha", {
  f <- fam_df(c("Raiz", "", "", ""), c("Filho", "", "Raiz", ""), c("Neto", "", "Filho", ""))
  f$generation <- c(NA, 5L, NA)
  out <- fill_generations(f)
  expect_equal(out$family$generation, c(4L, 5L, 6L))
  expect_equal(out$filled, c("Raiz", "Neto"))
  expect_equal(nrow(out$divergent), 0)
})

test_that("geração da planilha é mantida, e a divergência é apontada", {
  f <- fam_df(c("Raiz", "", "", ""), c("Filho", "", "Raiz", ""), c("Neto", "", "Filho", ""))
  f$generation <- c(1L, 2L, 2L)
  out <- fill_generations(f)
  expect_equal(out$family$generation, c(1L, 2L, 2L))
  expect_equal(out$divergent$name, "Neto")
  expect_equal(out$divergent$computed, 3L)
})

test_that("ciclo na planilha não trava o cálculo", {
  f <- fam_df(c("A", "", "B", ""), c("B", "", "A", ""))
  expect_length(infer_generations(f), 2)
})
