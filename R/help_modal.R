# Modal de boas-vindas: explica o formato do CSV e o das fotos.
#
# Aparece sozinho quando o app abre e pode ser reaberto pelo link "Como usar" da
# barra superior. As specs das colunas moravam na barra lateral; saíram de lá
# para o cartão de busca ocupar o espaço (todo/features.md, item 5) e é aqui que
# elas passaram a viver.

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
  "name,sex,generation,nucleus,birth_year,short_name,birth_date,death_year,mother_name,father_name,partner_name,image_file",
  "Maria Cavalcante,F,1,Fundadores,1940,Maria,12/03/1940,,,,Fulano da Silva,maria.jpg",
  "Fulano da Silva,M,1,Fundadores,1938,Fulano,,2010,,,Maria Cavalcante,fulano.jpg",
  "Ana da Silva,F,2,Ramo Ana,1965,Ana,1965-07-09,,Maria Cavalcante,Fulano da Silva,,ana.png",
  sep = "\n"
)

help_modal <- function() {
  modalDialog(
    title = "Como montar a sua árvore",
    size = "l",
    easyClose = TRUE,
    footer = modalButton("Entendi, vamos começar"),

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
        help_col_row("birth_year", TRUE, "Ano de nascimento (pode ficar vazio se você preencher birth_date)."),
        help_col_row("short_name", FALSE, "Como o nome aparece dentro da bola. Vazio: usa o primeiro nome."),
        help_col_row("birth_date", FALSE, HTML("Data completa, em <code>AAAA-MM-DD</code> ou <code>DD/MM/AAAA</code>. Aparece como aniversário no cartão.")),
        help_col_row("death_year", FALSE, "Ano de falecimento."),
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
