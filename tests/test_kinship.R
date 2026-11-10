library(testthat)
suppressWarnings(suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
}))

for (f in list.files(file.path("..", "R"), full.names = TRUE)) source(f)

family <- load_family_csv(file.path("fixtures", "family_sample.csv"))
term <- function(a, b) relationship(family, a, b)$term

test_that("ancestrais e descendentes seguem o uso popular", {
  expect_equal(kinship_term(0, 1, "F"), "mãe")
  expect_equal(kinship_term(0, 4, "F"), "tataravó (trisavó)")
  expect_equal(kinship_term(0, 5, "F"), "tatataravó (tetravó)")
  expect_equal(kinship_term(0, 6, "M"), "tatatataravô (pentavô)")
  expect_equal(kinship_term(4, 0, "M"), "tataraneto (trineto)")
  expect_equal(kinship_term(5, 0, "F"), "tatataraneta (tetraneta)")
  expect_equal(kinship_term(1, 1, NA), "irmão / irmã")
})

test_that("linha reta no fixture, inclusive pais da raiz sem ficha", {
  expect_equal(term("Raiz Um", "Neto Um"), "bisavô")
  expect_equal(term("Neto Um", "Raiz Dois"), "bisneto")
  expect_equal(term("Bisa Ausente", "Neto Um"), "tataravó (trisavó)")
  expect_equal(term("Pai Multi", "Filho Atual"), "pai")
  expect_equal(term("Filho Atual", "Pai Multi"), "filho")
  r <- relationship(family, "Raiz Um", "Neto Um")
  expect_equal(r$degree, "em linha reta de 3º grau")
  expect_equal(r$path$name, c("Raiz Um", "Pai Multi", "Filho Atual", "Neto Um"))
})

test_that("irmãos e meios-irmãos, contando placeholder_N como mãe comum", {
  expect_equal(term("Filha Ph1", "Filho Ph1"), "irmã")
  r <- relationship(family, "Filha Ph2", "Filha Ph1")
  expect_equal(r$term, "meia-irmã")
  expect_equal(r$side, "pai")
})

test_that("tios, primos e o grau popular entre gerações diferentes", {
  expect_equal(term("Mae Nomeada", "Neto Um"), "tia-avó")
  expect_equal(term("Neto Um", "Mae Nomeada"), "sobrinho-neto")
  r <- relationship(family, "Filho Nomeado", "Filha Solteiro")
  expect_equal(r$term, "primo de primeiro grau")
  expect_equal(sort(r$common), c("Raiz Dois", "Raiz Um"))
  expect_equal(r$degree, "colateral de 4º grau")
  expect_equal(term("Neto Um", "Filho Nomeado"), "sobrinho de segundo grau")
  expect_equal(term("Filho Nomeado", "Neto Um"), "tio de segundo grau")
  expect_equal(kinship_term(2, 4, "F"), "tia-avó de segundo grau")
  expect_equal(kinship_term(3, 3, "M"), "primo de segundo grau")
})

test_that("cônjuges e parentes por afinidade", {
  expect_equal(term("Esposa Atual", "Pai Multi"), "esposa")
  expect_equal(term("Pai Multi", "Esposa Atual"), "marido")
  expect_equal(term("Esposa Atual", "Filha Ph1"), "madrasta")
  expect_equal(term("Filha Ph1", "Esposa Atual"), "enteada")
  expect_equal(term("Esposa Atual", "Nora"), "sogra")
  expect_equal(term("Nora", "Esposa Atual"), "nora")
  expect_equal(term("Fulano da Silva", "Pai Multi"), "cunhado")
  expect_equal(term("Esposa Atual", "Fulano da Silva"), "concunhada")
  expect_equal(term("Nora", "Fulano da Silva"), "esposa do sobrinho da esposa")
  expect_equal(relationship(family, "Nora", "Esposa Atual")$kind, "affinity")
})

test_that("ex, filho do padrasto e gente sem ligação", {
  fam <- data.frame(
    name = c("Ana", "Bruno", "Caio", "Duda", "Edu", "Fabi", "Gil"),
    sex = c("F", "M", "M", "F", "M", "F", "M"),
    mother_name = c(NA, NA, "Ana", NA, NA, "Duda", "Duda"),
    father_name = c(NA, NA, "Bruno", NA, NA, "Edu", "Bruno"),
    partner_name = c("Edu", NA, NA, NA, "Ana", NA, NA),
    is_placeholder = FALSE,
    stringsAsFactors = FALSE
  )
  expect_equal(relationship(fam, "Ana", "Bruno")$term, "ex-companheira")
  # Caio é filho de Ana; Ana é casada com Edu, que é pai de Fabi.
  expect_equal(relationship(fam, "Caio", "Fabi")$term, "filho da madrasta")
  expect_equal(relationship(fam, "Edu", "Caio")$term, "padrasto")
  expect_equal(relationship(fam, "Ana", "Ana")$kind, "self")
  lonely <- rbind(fam, data.frame(
    name = "Zé", sex = "M", mother_name = NA, father_name = NA,
    partner_name = NA, is_placeholder = FALSE
  ))
  expect_equal(relationship(lonely, "Zé", "Ana")$kind, "none")
})
