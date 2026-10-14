library(testthat)
suppressWarnings(suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
}))

for (f in list.files(file.path("..", "R"), full.names = TRUE)) source(f)

family <- load_family_csv(file.path("fixtures", "family_sample.csv"))
stats <- family_stats(family, today = as.Date("2026-10-14"))

test_that("só fundadores e descendentes contam: cônjuges e placeholders não", {
  expect_false(any(stats$people$name %in% family$name[family$is_placeholder]))
  expect_false(any(c("Esposa Atual", "Nora", "Fulano da Silva") %in% stats$people$name))
  expect_true(all(c("Raiz Um", "Raiz Dois", "Maria Cavalcante", "Neto Cinco") %in% stats$people$name))
  expect_equal(stats$overview$n, 17L)
})

test_that("nome do núcleo aparece sem o prefixo", {
  expect_equal(
    nucleus_label(c("Núcleo A", "nucleo B", "Ramo C", "Fundador", NA)),
    c("A", "B", "C", "Fundador", NO_NUCLEUS_LABEL)
  )
})

test_that("contagem por sexo e por geração bate com a planilha", {
  o <- stats$overview
  expect_equal(o$n_f + o$n_m + o$n_sex_unknown, o$n)
  expect_equal(sum(stats$by_generation$n), o$n)
  expect_equal(o$n_generations, 4L)
})

test_that("gerações por núcleo contam gerações distintas", {
  b <- stats$by_nucleus
  nucleo_a <- b[b$nucleus_label == "A", ]
  expect_equal(nucleo_a$n_generations, 3L)
  expect_equal(c(nucleo_a$gen_min, nucleo_a$gen_max), c(2L, 4L))
  # Ordem de leitura: quem começa mais cedo vem primeiro.
  expect_equal(b$nucleus_label, c("Fundador", "A", "B", "C", "D"))
})

test_that("idade desconta o aniversário que ainda não chegou", {
  fam <- data.frame(
    birth_year = c(1965L, 1965L, 1900L),
    death_year = c(NA, NA, 1980L),
    birth_date = as.Date(c("1965-07-09", "1965-12-25", NA))
  )
  ages <- person_ages(fam, today = as.Date("2026-10-14"))
  expect_equal(ages$age, c(61L, 60L, 80L))
  expect_equal(ages$alive, c(TRUE, TRUE, FALSE))
})

test_that("idade ao falecer desconta o aniversário com as duas datas completas", {
  fam <- data.frame(
    birth_year = c(1900L, 1900L),
    death_year = c(1980L, 1980L),
    birth_date = as.Date(c("1900-06-10", "1900-06-10")),
    death_date = as.Date(c("1980-03-01", NA))
  )
  expect_equal(person_ages(fam)$age, c(79L, 80L))
})

test_that("idade mínima, máxima e média por núcleo usam só os vivos", {
  b <- stats$by_nucleus
  # Os dois fundadores faleceram: o núcleo não tem idade atual.
  expect_true(is.na(b$age_min[b$nucleus_label == "Fundador"]))
  a <- b[b$nucleus_label == "A", ]
  vivos_a <- stats$people$age[stats$people$nucleus_label == "A" & stats$people$alive]
  expect_equal(c(a$age_min, a$age_max), range(vivos_a))
  expect_equal(a$age_mean, mean(vivos_a))
})

test_that("aniversariantes do mês: família e cônjuges atuais, falecidos inclusive", {
  b <- month_birthdays(family, 10L, 2026L, today = as.Date("2026-10-16"))
  # Raiz Dois (03/10) faleceu e aparece marcada; Nora (30/10) é cônjuge atual.
  expect_equal(b$name, c("Raiz Dois", "Neto Tres", "Filha Solteiro", "Nora"))
  expect_equal(b$deceased, c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(b$status, c("past", "today", "upcoming", "upcoming"))
  expect_equal(b$turning[-1], c(37L, 56L, 64L))
  expect_equal(b$nucleus_label, c("Fundador", "A", "C", "A"))
  expect_equal(b$spouse_of, c(NA, NA, NA, "Filho Atual"))
})

test_that("outros meses: o status compara com hoje, inclusive em outro ano", {
  antes <- month_birthdays(family, 9L, 2026L, today = as.Date("2026-10-16"))
  expect_true(all(antes$status == "past"))
  depois <- month_birthdays(family, 1L, 2027L, today = as.Date("2026-10-16"))
  expect_true(all(depois$status == "upcoming"))
  expect_equal(depois$turning[depois$name == "Neto Quatro"], 36L)
})

test_that("ex-cônjuge não entra nos aniversários", {
  fam <- family
  # Filho Atual passa a ter outra cônjuge; Nora ainda o cita, mas agora é ex.
  fam$partner_name[fam$name == "Filho Atual"] <- "Esposa Nova"
  fam <- dplyr::bind_rows(fam, data.frame(
    name = "Esposa Nova", sex = "F", generation = 3L, nucleus = "Ramo A",
    birth_year = 1970L, birth_date = as.Date("1970-10-20"),
    partner_name = "Filho Atual", is_placeholder = FALSE
  ))
  b <- month_birthdays(fam, 10L, 2026L, today = as.Date("2026-10-16"))
  expect_false("Nora" %in% b$name)
  expect_true("Esposa Nova" %in% b$name)
})
