# Modais de entrada: welcome_modal() é o que abre com o app e o ÚNICO lugar
# onde se carrega o CSV, as fotos ou o exemplo (reaberto pelo link "Carregar
# dados" da barra superior); help_modal() explica o formato do CSV e o das
# fotos, aberto pelo botão do welcome_modal ou pelo link "Como usar". As specs
# das colunas moravam na barra lateral; saíram de lá para o cartão de busca
# ocupar o espaço (todo/features.md, item 5) e é aqui que elas passaram a viver.

# Uma linha da tabela de colunas.
help_col_row <- function(col, required, desc) {
  tags$tr(
    tags$td(
      style = "white-space:nowrap;padding:5px 10px 5px 0;vertical-align:top;",
      tags$code(col)
    ),
    tags$td(
      style = "padding:5px 10px 5px 0;vertical-align:top;",
      if (required) {
        tags$span(
          style = paste0(
            "display:inline-block;padding:0 7px;border-radius:9px;",
            "font-size:0.68rem;color:#FFF;background:#7D3C98;"
          ),
          "obrigatória"
        )
      } else {
        tags$span(
          style = paste0(
            "display:inline-block;padding:0 7px;border-radius:9px;",
            "font-size:0.68rem;color:#566573;background:#ECEFF1;"
          ),
          "opcional"
        )
      }
    ),
    tags$td(
      style = "padding:5px 0;vertical-align:top;color:#566573;",
      desc
    )
  )
}

help_section_title <- function(...) {
  tags$div(
    style = paste0(
      "font-weight:600;color:#1E5A3A;font-size:0.95rem;",
      "margin:18px 0 8px;"
    ),
    ...
  )
}

HELP_CSV_EXAMPLE <- paste(
  "name,sex,generation,nucleus,birth_date,short_name,death_date,mother_name,father_name,partner_name,image_file",
  "Maria Cavalcante,F,1,Fundadores,12/03/1940,Maria,,,,Fulano da Silva,maria.jpg",
  "Fulano da Silva,M,1,Fundadores,1938,Fulano,21/08/2010,,,Maria Cavalcante,fulano.jpg",
  "Ana da Silva,F,2,Ramo Ana,09/07/1965,Ana,,Maria Cavalcante,Fulano da Silva,,ana.png",
  sep = "\n"
)

# Atributos do seletor de pasta do navegador no <input type="file">, para a
# pessoa escolher a pasta inteira de fotos de uma vez. Percorre a árvore de tags
# atrás do único input de arquivo, o que funciona em qualquer versão do Shiny
# (tagQuery não tem seletor por atributo).
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

folder_input <- function(id, label) {
  add_dir_attrs(fileInput(
    id,
    label,
    multiple = TRUE,
    buttonLabel = "Escolher pasta...",
    accept = c("image/png", "image/jpeg"),
    placeholder = "Nenhuma pasta selecionada",
    width = "100%"
  ))
}

# O CSV carrega assim que é escolhido, mas o modal só fecha no "Ver árvore":
# assim dá tempo de escolher as fotos também. `load_status` (no servidor) diz o
# que já foi carregado.
welcome_modal <- function() {
  modalDialog(
    title = "Árvore Genealógica",
    easyClose = TRUE,
    footer = tagList(
      actionLink(
        "show_help_start",
        "Formato do CSV e das fotos",
        icon = icon("circle-info"),
        class = "me-auto"
      ),
      tags$button(
        type = "button",
        class = "btn btn-primary",
        `data-bs-dismiss` = "modal",
        "Ver árvore"
      )
    ),
    fileInput(
      "csv_upload",
      "1. Escolha o CSV da família",
      accept = c(".csv", "text/csv"),
      buttonLabel = "Procurar...",
      placeholder = "Nenhum arquivo selecionado",
      width = "100%"
    ),
    folder_input("photos", "2. Fotos (opcional): escolha a pasta"),
    tags$div(
      class = "form-text",
      style = "margin-top:-8px;margin-bottom:10px;",
      "Os nomes dos arquivos devem bater com a coluna ",
      tags$code("image_file"),
      " do CSV. As fotos ficam só nesta sessão."
    ),
    uiOutput("load_status"),
    tags$div(
      style = "display:flex;align-items:center;gap:10px;margin:14px 0 4px;color:#95A5A6;font-size:0.8rem;",
      tags$hr(style = "flex:1;margin:0;"),
      "ou",
      tags$hr(style = "flex:1;margin:0;")
    ),
    actionButton(
      "use_example",
      "Usar CSV exemplo",
      icon = icon("wand-magic-sparkles"),
      class = "btn-outline-primary w-100"
    )
  )
}

help_modal <- function() {
  modalDialog(
    title = "Como montar a sua árvore",
    size = "l",
    easyClose = TRUE,
    footer = tagList(
      actionButton("back_to_start", "Voltar"),
      modalButton("Fechar")
    ),

    # HTML() em vez de tags$strong() solto: o htmltools separa filhos com quebra
    # de linha, e o espaço resultante antes do ":" aparece no texto renderizado.
    tags$p(
      style = "color:#566573;",
      HTML(paste0(
        "O app monta a árvore a partir de um arquivo <strong>CSV</strong> com ",
        "uma linha por pessoa. As fotos são <strong>opcionais</strong>: sem ",
        "elas cada pessoa aparece com a inicial do nome."
      ))
    ),

    help_section_title("1. O CSV"),
    tags$p(
      style = "color:#566573;font-size:0.88rem;margin-bottom:8px;",
      "Separado por vírgula, codificado em UTF-8 (para os acentos), com estes ",
      "nomes de coluna na primeira linha:"
    ),
    tags$table(
      style = "width:100%;font-size:0.85rem;border-collapse:collapse;",
      tags$tbody(
        help_col_row("name", TRUE, "Nome completo. É a chave: precisa ser único e é por ele que mãe, pai e cônjuge são ligados."),
        help_col_row("sex", TRUE, HTML("<code>M</code> ou <code>F</code>.")),
        help_col_row("generation", TRUE, "Número inteiro: 1 para a geração mais antiga, 2 para os filhos dela, e assim por diante."),
        help_col_row("nucleus", TRUE, "Nome do ramo da família. Define a cor da pessoa no desenho."),
        help_col_row("birth_date", TRUE, HTML("Data de nascimento, completa (<code>DD/MM/AAAA</code>) ou só o ano (<code>AAAA</code>). Com a data completa, a pessoa aparece nos aniversários do mês.")),
        help_col_row("short_name", FALSE, "Como o nome aparece dentro da bola. Vazio: usa o primeiro nome."),
        help_col_row("death_date", FALSE, HTML("Data de falecimento, também em <code>DD/MM/AAAA</code> ou <code>AAAA</code>. Vazio: a pessoa é considerada viva.")),
        help_col_row("mother_name", FALSE, "Nome completo da mãe, exatamente como escrito na coluna name dela."),
        help_col_row("father_name", FALSE, "Nome completo do pai, idem."),
        help_col_row("partner_name", FALSE, "Nome completo do cônjuge, idem."),
        help_col_row("image_file", FALSE, HTML("Nome do arquivo da foto, com extensão (<code>maria.jpg</code>). Veja o item 2."))
      )
    ),
    tags$p(
      style = "color:#566573;font-size:0.85rem;margin-top:12px;",
      "Células vazias podem ficar em branco ou como ",
      tags$code("NA"),
      ". Mãe, pai ou cônjuge citados que ainda não têm linha própria não são ",
      "problema: o app cria a ficha que falta para o ramo não ficar solto. ",
      "Se dois irmãos têm a mesma mãe desconhecida — diferente da mãe de um ",
      "terceiro — escreva ",
      tags$code("placeholder_1"),
      " nos dois e ",
      tags$code("placeholder_2"),
      " no outro."
    ),

    help_section_title("Exemplo"),
    tags$pre(
      style = paste0(
        "background:#F7F7F5;border:1px solid #E5E8EA;border-radius:6px;",
        "padding:10px;font-size:0.72rem;overflow-x:auto;margin:0;"
      ),
      HELP_CSV_EXAMPLE
    ),

    help_section_title("2. As fotos (se você quiser)"),
    tags$ul(
      style = "color:#566573;font-size:0.88rem;padding-left:18px;",
      tags$li(HTML("Formatos: <strong>JPG</strong> ou <strong>PNG</strong>.")),
      tags$li(
        "O nome de cada arquivo tem que bater com o que está na coluna ",
        tags$code("image_file"),
        " do CSV (maiúsculas e minúsculas não importam). Quem não tiver foto ",
        "correspondente aparece com a inicial do nome."
      ),
      tags$li(HTML(paste0(
        "Você escolhe a <strong>pasta inteira</strong> de uma vez, não ",
        "arquivo por arquivo."
      ))),
      tags$li(
        "Cada foto é recortada em círculo automaticamente. Imagens quadradas, ",
        "com o rosto no centro, ficam melhores."
      ),
      tags$li(HTML(paste0(
        "<strong>As fotos ficam só nesta sessão:</strong> são guardadas numa ",
        "pasta temporária, nunca publicadas, e apagadas quando você fecha o app."
      )))
    ),

    help_section_title("3. Navegando"),
    tags$ul(
      style = "color:#566573;font-size:0.88rem;padding-left:18px;margin-bottom:0;",
      tags$li(
        "Clique numa pessoa da árvore (ou busque pelo nome) para abrir o ",
        "cartão dela na barra lateral."
      ),
      tags$li(
        "Clique no cartão para ver a árvore só daquela pessoa, com pais, ",
        "cônjuge e filhos."
      )
    )
  )
}
