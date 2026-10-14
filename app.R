suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
  library(dplyr)
  library(htmltools)
  library(stringr)
})

# Ensure UTF-8 for Portuguese characters regardless of system locale
suppressWarnings(Sys.setlocale("LC_CTYPE", "UTF-8"))
suppressWarnings(Sys.setlocale("LC_COLLATE", "UTF-8"))

# Photos are uploaded per session and cropped to circles on the fly
# (see resolve_uploaded_images in R/uploaded_images.R) — nothing is pre-generated
# or served as a static file.

ui <- bslib::page_navbar(
  title = "Árvore Genealógica",
  bg = "#1E5A3A",
  # Só a árvore estica para caber na janela; a aba de análises rola como página.
  fillable = "Árvore",
  header = tags$head(tags$script(src = "tree_scale.js")),
  theme = bslib::bs_add_rules(
    bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#7D3C98"
    ),
    c(
      ".navbar .nav-link, .navbar .nav-link.active { color: #F5EFE0 !important; }",
      # O cartão da barra lateral é clicável por inteiro (ver R/person_card.R):
      # precisa parecer clicável.
      ".person-card { cursor: pointer; transition: box-shadow .15s ease; }",
      ".person-card:hover { box-shadow: 0 2px 10px rgba(0,0,0,.14); }",
      ".person-card:focus-visible { outline: 2px solid #7D3C98; outline-offset: 2px; }",
      ".person-card:hover .person-card-cta { filter: brightness(1.12); }"
    )
  ),


  bslib::nav_panel(
    title = "Árvore",
    # CSV e fotos entram só pelo modal (R/help_modal.R); a barra lateral fica
    # para os aniversariantes, a busca e o cartão da pessoa.
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 380,
        uiOutput("birthday_card"),
        selectizeInput(
          inputId = "name",
          label = "Buscar pessoa",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),
        uiOutput("person_card")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(textOutput("csv_status", inline = TRUE)),
        plotlyOutput("csv_plot", height = "780px")
      )
    )
  ),

  bslib::nav_panel(
    title = "Análises",
    uiOutput("stats_panel")
  ),

  # Depois do painel e do espaçador: a aba fica à esquerda e o atalho para o
  # modal de instruções (R/help_modal.R) encosta na direita da barra.
  bslib::nav_spacer(),
  bslib::nav_item(
    actionLink("open_start", "Carregar dados", icon = icon("upload"), class = "nav-link")
  ),
  bslib::nav_item(
    actionLink("show_help", "Como usar", class = "nav-link")
  )
)

server <- function(input, output, session) {
  focus_name <- reactiveVal(NULL)

  # Abre pedindo o CSV (ou o exemplo); as instruções ficam a um clique, no
  # próprio modal ou no link "Como usar".
  showModal(welcome_modal())
  observeEvent(input$show_help, showModal(help_modal()))
  observeEvent(input$show_help_start, showModal(help_modal()))
  observeEvent(input$back_to_start, showModal(welcome_modal()))
  observeEvent(input$open_start, showModal(welcome_modal()))

  # Per-session scratch folder for cropped photos; removed when the session ends.
  work_dir <- tempfile("photos_")
  dir.create(work_dir)
  session$onSessionEnded(function() unlink(work_dir, recursive = TRUE))

  # --- Aba da árvore ---
  csv_family <- reactiveVal(NULL)
  csv_error <- reactiveVal(NULL)
  # O input de fotos mora no modal e some com ele: guarda-se o upload aqui.
  photo_uploads <- reactiveVal(NULL)
  observeEvent(input$photos, photo_uploads(input$photos))

  # Family data with uploaded photos resolved to embedded (base64) images.
  family_resolved <- reactive({
    fam <- csv_family()
    req(fam)
    resolve_uploaded_images(fam, photo_uploads(), work_dir)
  })

  focused_modal <- function() {
    modalDialog(
      title = NULL,
      plotlyOutput("focused_plot", height = "520px"),
      footer = modalButton("Fechar"),
      size = "l",
      easyClose = TRUE
    )
  }

  # Mesmo caminho para o CSV enviado e para o exemplo embutido.
  load_csv_path <- function(path) {
    tryCatch(
      {
        fam <- load_family_csv(path)
        csv_family(fam)
        csv_error(NULL)
      },
      error = function(e) {
        csv_family(NULL)
        csv_error(conditionMessage(e))
        showNotification(
          paste0("Erro ao ler CSV: ", conditionMessage(e)),
          type = "error",
          duration = 10
        )
      }
    )
  }

  # O modal continua aberto depois do CSV, para dar tempo de escolher as fotos.
  observeEvent(input$csv_upload, load_csv_path(input$csv_upload$datapath))

  # Exemplo para quem quer ver o app funcionando sem preparar um CSV.
  observeEvent(input$use_example, {
    load_csv_path("tests/fixtures/family_sample.csv")
    removeModal()
  })

  output$load_status <- renderUI({
    err <- csv_error()
    fam <- csv_family()
    n_photos <- NROW(photo_uploads())
    if (!is.null(err)) {
      return(tags$div(class = "text-danger small", icon("circle-xmark"), " ", err))
    }
    if (is.null(fam) && n_photos == 0) {
      return(NULL)
    }
    tags$div(
      class = "small",
      style = "color:#1E5A3A;",
      if (!is.null(fam)) {
        tags$div(icon("circle-check"), " ", sum(!fam$is_placeholder), " pessoas carregadas")
      },
      if (n_photos > 0) {
        tags$div(icon("circle-check"), " ", n_photos, if (n_photos == 1) " foto" else " fotos")
      }
    )
  })

  # Placeholders existem só para nenhum ramo ficar solto no desenho (ver
  # R/placeholders.R): não são gente que se busque, se conte ou em que se clique.
  searchable_names <- function(fam) sort(fam$name[!fam$is_placeholder])
  is_placeholder_name <- function(fam, nm) {
    isTRUE(fam$is_placeholder[match(nm, fam$name)])
  }

  observeEvent(csv_family(), {
    fam <- csv_family()
    if (!is.null(fam)) {
      updateSelectizeInput(
        session,
        "name",
        choices = c("", searchable_names(fam)),
        server = TRUE
      )
    } else {
      updateSelectizeInput(session, "name", choices = NULL)
    }
  })

  # Buscar pelo nome e clicar numa bola da árvore fazem a mesma coisa: preencher
  # o cartão da barra lateral. A árvore focada só abre pelo clique no cartão —
  # abrindo-a junto, como antes, o modal tapava o próprio cartão que a pessoa
  # tinha acabado de pedir.
  observeEvent(input$name, {
    if (!is.null(input$name) && nzchar(input$name)) {
      focus_name(input$name)
    }
  })

  observeEvent(input$show_tree, {
    req(focus_name())
    showModal(focused_modal())
  })

  # Preenche o cartão e faz a caixa de busca acompanhar.
  select_person <- function(nm) {
    fam <- csv_family()
    if (is.null(nm) || is.null(fam) || is_placeholder_name(fam, nm)) {
      return()
    }
    focus_name(nm)
    # Com selectize servido pelo servidor, `selected` sozinho não pega: a opção
    # precisa vir junto para a caixa de busca acompanhar a bola clicada.
    updateSelectizeInput(
      session,
      "name",
      choices = c("", searchable_names(fam)),
      selected = nm,
      server = TRUE
    )
  }

  observeEvent(plotly::event_data("plotly_click", source = "overview"), {
    select_person(plotly::event_data("plotly_click", source = "overview")$customdata)
  })
  observeEvent(input$birthday_pick, select_person(input$birthday_pick))

  # Aniversariantes do mês: fixado no topo da barra lateral até a pessoa
  # remover; aí some pelo resto da sessão.
  birthdays_hidden <- reactiveVal(FALSE)
  observeEvent(input$hide_birthdays, birthdays_hidden(TRUE))
  # Quantos meses à frente (ou atrás, se negativo) do mês atual o cartão mostra.
  birthday_offset <- reactiveVal(0L)
  observeEvent(input$birthday_nav, {
    step <- as.integer(input$birthday_nav$step)
    birthday_offset(if (step == 0L) 0L else birthday_offset() + step)
  })
  output$birthday_card <- renderUI({
    fam <- csv_family()
    if (is.null(fam) || birthdays_hidden()) {
      return(NULL)
    }
    today <- Sys.Date()
    first_of_month <- as.Date(format(today, "%Y-%m-01"))
    view <- if (birthday_offset() == 0L) {
      first_of_month
    } else {
      seq(first_of_month, by = paste(birthday_offset(), "months"), length.out = 2)[2]
    }
    birthday_card_ui(fam, view = view, today = today)
  })

  observeEvent(plotly::event_data("plotly_click", source = "focused"), {
    click <- plotly::event_data("plotly_click", source = "focused")
    fam <- csv_family()
    nm <- click$customdata
    if (is.null(nm) || is.null(fam) || is_placeholder_name(fam, nm)) {
      return()
    }
    focus_name(nm)
  })

  output$person_card <- renderUI({
    fam <- csv_family()
    nm <- focus_name()
    if (is.null(fam) || is.null(nm)) {
      return(NULL)
    }
    person_card_ui(family_resolved(), nm)
  })

  output$csv_status <- renderText({
    err <- csv_error()
    if (!is.null(err)) {
      return(paste0("Erro: ", err))
    }
    fam <- csv_family()
    if (is.null(fam)) {
      return("Carregue um CSV para gerar a árvore.")
    }
    n_real <- sum(!fam$is_placeholder)
    n_pending <- sum(fam$is_placeholder)
    if (n_pending > 0) {
      paste0(
        n_real, " pessoas carregadas (", n_pending,
        " co-genitores citados ainda sem ficha na planilha)."
      )
    } else {
      paste0(n_real, " pessoas carregadas.")
    }
  })

  output$csv_plot <- renderPlotly({
    fam <- csv_family()
    if (is.null(fam)) {
      return(
        plotly::plot_ly() |>
          plotly::layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(list(
              text = "Clique em Carregar dados para começar.",
              x = 0.5,
              y = 0.5,
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              font = list(size = 14, color = "#7F8C8D")
            )),
            plot_bgcolor = "#FAFAFA",
            paper_bgcolor = "#FAFAFA"
          )
      )
    }
    render_overview(family_resolved())
  })

  output$focused_plot <- renderPlotly({
    fn <- focus_name()
    req(fn, csv_family())
    render_focused(family_resolved(), fn)
  })

  # --- Aba de análises ---
  stats <- reactive({
    fam <- csv_family()
    req(fam)
    family_stats(fam)
  })

  # Um gráfico dentro de uma seção: título pequeno, sem moldura própria — a
  # moldura é a da seção.
  stats_chart <- function(title, output_id, height = "340px") {
    tags$div(
      tags$h6(class = "mb-2", style = "color:#566573;font-weight:600;", title),
      plotlyOutput(output_id, height = height)
    )
  }

  # Seção que agrupa gráficos do mesmo tema num retângulo maior.
  stats_section <- function(title, icon_name, ...) {
    bslib::card(
      class = "mb-3",
      bslib::card_header(
        class = "fs-5 fw-semibold",
        style = "color:#1E5A3A;",
        icon(icon_name, class = "me-2"),
        title
      ),
      bslib::card_body(...)
    )
  }

  output$stats_panel <- renderUI({
    if (is.null(csv_family())) {
      return(bslib::card(
        bslib::card_body(
          class = "text-center text-muted py-5",
          "Clique em ",
          tags$b("Carregar dados"),
          ", na barra superior, para escolher o CSV (ou usar o exemplo)."
        )
      ))
    }
    st <- stats()
    o <- st$overview
    # Gráficos com uma barra por núcleo crescem com o número de núcleos.
    n_nuclei <- nrow(st$by_nucleus)
    nucleus_height <- paste0(max(260, 60 + 42 * n_nuclei), "px")
    top_height <- paste0(max(340, 90 + 42 * (n_nuclei + 1)), "px")

    age_detail <- paste0("entre ", o$n_age_alive, " pessoas vivas")
    if (o$n_age_death > 0) {
      age_detail <- paste0(
        age_detail, " · ao falecer: ", format_num(o$mean_age_death, 0),
        " anos (", o$n_age_death, ")"
      )
    }

    sex_icon <- function(name, sex) {
      icon(name, style = paste0("color: ", SEX_COLORS[[sex]], ";"))
    }

    tagList(
      bslib::layout_column_wrap(
        width = "200px",
        fill = FALSE,
        bslib::value_box(
          title = "Pessoas",
          value = o$n,
          showcase = icon("users"),
          paste(o$n_nuclei, ifelse(o$n_nuclei == 1, "núcleo", "núcleos"))
        ),
        bslib::value_box(
          title = "Gerações",
          value = o$n_generations,
          showcase = icon("layer-group"),
          paste0(o$n_alive, " vivos · ", o$n_deceased, " falecidos")
        ),
        bslib::value_box(
          title = "Idade média (vivos)",
          value = paste(format_num(o$mean_age_alive, 0), "anos"),
          showcase = icon("cake-candles"),
          age_detail
        ),
        bslib::value_box(
          title = "Mulheres",
          value = format_pct(o$n_f, o$n),
          showcase = sex_icon("venus", "F"),
          paste(o$n_f, ifelse(o$n_f == 1, "mulher", "mulheres"))
        ),
        bslib::value_box(
          title = "Homens",
          value = format_pct(o$n_m, o$n),
          showcase = sex_icon("mars", "M"),
          paste(o$n_m, ifelse(o$n_m == 1, "homem", "homens")),
          if (o$n_sex_unknown > 0) {
            tags$div(class = "small", o$n_sex_unknown, " sem sexo informado")
          }
        )
      ),
      stats_section(
        "Gênero",
        "venus-mars",
        bslib::layout_column_wrap(
          width = 1 / 2,
          fill = FALSE,
          gap = "2rem",
          stats_chart("Pessoas por geração", "stats_by_generation", top_height),
          stats_chart("Proporção feminino/masculino por núcleo", "stats_sex_by_nucleus", top_height)
        )
      ),
      stats_section(
        "Geração",
        "layer-group",
        bslib::layout_column_wrap(
          width = 1 / 2,
          fill = FALSE,
          gap = "2rem",
          stats_chart("Pessoas por núcleo", "stats_by_nucleus", nucleus_height),
          stats_chart("Gerações em cada núcleo", "stats_generations_by_nucleus", nucleus_height)
        ),
        stats_chart(
          "Idades por núcleo (pessoas vivas)",
          "stats_age_by_nucleus",
          paste0(max(280, 110 + 70 * n_nuclei), "px")
        )
      ),
      helpText(
        class = "mb-4",
        "Contam só as pessoas da família: os fundadores (a geração mais antiga) ",
        "e seus descendentes. Cônjuges e pais citados sem linha própria ficam ",
        "de fora. Quem não tem ", tags$code("death_date"), " é considerado vivo."
      )
    )
  })

  output$stats_by_generation <- renderPlotly(plot_people_by_generation(stats()))
  output$stats_sex_by_nucleus <- renderPlotly(plot_sex_by_nucleus(stats()))
  output$stats_by_nucleus <- renderPlotly(plot_people_by_nucleus(stats()))
  output$stats_generations_by_nucleus <- renderPlotly(plot_generations_by_nucleus(stats()))
  output$stats_age_by_nucleus <- renderPlotly(plot_age_by_nucleus(stats()))
}

shinyApp(ui, server)
