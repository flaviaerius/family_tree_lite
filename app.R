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

# Set the browser's directory-picker attributes on the file <input> so the user
# chooses a whole folder of photos at once. Walks the tag tree to find the single
# file input, which is robust across Shiny versions (tagQuery has no attribute
# selectors).
add_dir_attrs <- function(tag) {
  if (inherits(tag, "shiny.tag")) {
    if (identical(tag$name, "input") && identical(tag$attribs$type, "file")) {
      tag$attribs$webkitdirectory <- ""
      tag$attribs$directory <- ""
      tag$attribs$mozdirectory <- ""
    }
    tag$children <- lapply(tag$children, add_dir_attrs)
  } else if (is.list(tag)) {
    tag <- lapply(tag, add_dir_attrs)
  }
  tag
}

# A folder upload: a multi-file input that lets the user pick a whole folder.
folder_input <- function(id, label) {
  add_dir_attrs(fileInput(
    id,
    label,
    multiple = TRUE,
    buttonLabel = "Escolher pasta...",
    accept = c("image/png", "image/jpeg"),
    placeholder = "Nenhuma pasta selecionada"
  ))
}

ui <- bslib::page_navbar(
  title = "Árvore Genealógica",
  bg = "#1E5A3A",
  header = tags$head(tags$script(src = "tree_scale.js")),
  theme = bslib::bs_add_rules(
    bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#7D3C98"
    ),
    ".navbar .nav-link, .navbar .nav-link.active { color: #F5EFE0 !important; }"
  ),

  bslib::nav_panel(
    title = "Carregar CSV",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 380,
        tags$hr(),
        selectizeInput(
          inputId = "name",
          label = "Buscar pessoa",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),
        uiOutput("person_card"),
        tags$hr(),
        fileInput(
          "csv_upload",
          "Escolher CSV",
          accept = c(".csv", "text/csv"),
          buttonLabel = "Procurar...",
          placeholder = "Nenhum arquivo selecionado"
        ),
        folder_input("photos", "Insira aqui as suas fotos"),
        helpText(
          "Os nomes dos arquivos devem bater com a coluna ",
          tags$code("image_file"),
          " do CSV. As fotos ficam só nesta sessão."
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(textOutput("csv_status", inline = TRUE)),
        plotlyOutput("csv_plot", height = "780px")
      )
    )
  )
)

server <- function(input, output, session) {
  focus_name <- reactiveVal(NULL)

  # Per-session scratch folder for cropped photos; removed when the session ends.
  work_dir <- tempfile("photos_")
  dir.create(work_dir)
  session$onSessionEnded(function() unlink(work_dir, recursive = TRUE))

  # --- CSV tab ---
  csv_family <- reactiveVal(NULL)
  csv_error <- reactiveVal(NULL)

  # Family data with uploaded photos resolved to embedded (base64) images.
  family_resolved <- reactive({
    fam <- csv_family()
    req(fam)
    resolve_uploaded_images(fam, input$photos, work_dir)
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

  observeEvent(input$csv_upload, {
    f <- input$csv_upload
    if (is.null(f)) {
      return()
    }
    tryCatch(
      {
        fam <- load_family_csv(f$datapath)
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

  # A busca preenche o cartão da barra lateral. A árvore focada abre pelo botão
  # do cartão: abrindo-a na hora, como antes, o modal tapava o próprio cartão.
  observeEvent(input$name, {
    if (!is.null(input$name) && nzchar(input$name)) {
      focus_name(input$name)
    }
  })

  observeEvent(input$show_tree, {
    req(focus_name())
    showModal(focused_modal())
  })

  observeEvent(plotly::event_data("plotly_click", source = "overview"), {
    click <- plotly::event_data("plotly_click", source = "overview")
    fam <- csv_family()
    nm <- click$customdata
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
    showModal(focused_modal())
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
              text = "Carregue um CSV na barra lateral para ver a árvore.",
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
}

shinyApp(ui, server)
