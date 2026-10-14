# Cartão "Aniversariantes do mês", fixado no topo da barra lateral da árvore.
# Entra quem é da família (fundadores e descendentes, ver is_family_member em
# R/family_stats.R) e os cônjuges ATUAIS deles; ex-cônjuges ficam de fora.
# Falecidos aparecem em cinza. Sem `birth_date` não há dia para mostrar.

# Para cada linha, o nome do familiar de quem a pessoa é cônjuge atual (NA se
# não for). Vale o `partner_name` do familiar: se ele aponta para outra pessoa,
# quem ainda o cita como cônjuge é ex. Só quando o familiar não tem
# `partner_name` preenchido é que o lado do cônjuge decide.
current_spouse_of <- function(family, member = is_family_member(family)) {
  real <- !family$is_placeholder
  fam_names <- family$name[member]
  fam_partner <- family$partner_name[member]

  out <- rep(NA_character_, nrow(family))
  for (i in which(real & !member)) {
    nm <- family$name[i]
    named_by <- fam_names[!is.na(fam_partner) & fam_partner == nm]
    if (length(named_by) > 0) {
      out[i] <- named_by[1]
      next
    }
    ref <- family$partner_name[i]
    if (!is.na(ref) && ref %in% fam_names && is.na(fam_partner[match(ref, fam_names)])) {
      out[i] <- ref
    }
  }
  out
}

# Aniversários de um mês qualquer (`month`/`year`), para o cartão poder andar
# para meses anteriores e posteriores. `status` compara a data do aniversário
# naquele ano com `today`: "past" (já foi), "today" ou "upcoming". Falecidos
# também aparecem — o cartão os mostra em cinza.
month_birthdays <- function(family, month, year, today = Sys.Date()) {
  member <- is_family_member(family)
  spouse_of <- current_spouse_of(family, member)
  keep <- (member | !is.na(spouse_of)) &
    !is.na(family$birth_date) &
    as.integer(format(family$birth_date, "%m")) == month
  people <- family[keep, , drop = FALSE]

  day <- as.integer(format(people$birth_date, "%d"))
  # Comparação como número AAAAMMDD: 29/02 em ano não bissexto não vira NA.
  when <- year * 10000L + month * 100L + day
  now <- as.integer(format(today, "%Y%m%d"))
  out <- data.frame(
    name = people$name,
    day = day,
    generation = people$generation,
    nucleus = people$nucleus,
    nucleus_label = nucleus_label(people$nucleus),
    spouse_of = spouse_of[keep],
    deceased = !is.na(people$death_year),
    death_year = people$death_year,
    turning = year - as.integer(format(people$birth_date, "%Y")),
    status = ifelse(when < now, "past", ifelse(when == now, "today", "upcoming")),
    stringsAsFactors = FALSE
  )
  out[order(out$day, out$name), , drop = FALSE]
}

BIRTHDAY_GREY <- "#95A5A6"

birthday_row_ui <- function(row, color) {
  past <- row$status == "past"
  today <- row$status == "today"
  dead <- row$deceased
  # Falecidos em cinza: no dia, no nome e na etiqueta do núcleo.
  if (dead) color <- BIRTHDAY_GREY
  pick <- paste0(
    "Shiny.setInputValue('birthday_pick', ",
    jsonlite::toJSON(row$name, auto_unbox = TRUE),
    ", {priority: 'event'});"
  )
  day_style <- if (dead) {
    if (today) "background:#7F8C8D;color:#FFF;" else "background:#ECEFF1;color:#7F8C8D;"
  } else {
    if (today) "background:#7D3C98;color:#FFF;" else "background:#F4F1F7;color:#4A235A;"
  }

  tags$div(
    style = paste0(
      "display:flex;gap:10px;align-items:center;padding:6px 0;",
      "border-top:1px solid #F0F2F3;",
      if (past) "opacity:.55;" else ""
    ),
    tags$div(
      style = paste0(
        "flex:0 0 38px;height:38px;border-radius:8px;display:flex;",
        "flex-direction:column;align-items:center;justify-content:center;",
        "line-height:1;font-weight:600;",
        day_style
      ),
      tags$span(style = "font-size:0.95rem;", row$day),
      if (today) tags$span(style = "font-size:0.6rem;font-weight:500;margin-top:2px;", "hoje")
    ),
    tags$div(
      style = "min-width:0;",
      tags$a(
        href = "#",
        onclick = paste0(pick, " return false;"),
        title = "Ver o cartão desta pessoa",
        style = paste0(
          "font-weight:600;font-size:0.85rem;text-decoration:none;color:",
          if (dead) "#7F8C8D" else "#2C3E50",
          ";"
        ),
        row$name
      ),
      tags$div(
        style = paste0("font-size:0.75rem;margin-top:2px;color:", if (dead) "#95A5A6" else "#566573", ";"),
        tags$span(
          style = paste0(
            "display:inline-block;padding:0 7px;border-radius:9px;color:#FFF;",
            "font-size:0.7rem;background:", color, ";"
          ),
          row$nucleus_label
        ),
        " ",
        # Cônjuge não é uma geração da família: diz de quem é cônjuge.
        if (is.na(row$spouse_of)) {
          paste0(generation_label(row$generation), " geração")
        } else {
          paste0("cônjuge de ", row$spouse_of)
        },
        if (dead) {
          paste0(" · † ", row$death_year)
        } else if (!is.na(row$turning)) {
          paste0(" · ", row$turning, " anos")
        }
      )
    )
  )
}

# `view` é qualquer dia do mês mostrado; as setas mandam `birthday_nav` (-1 ou
# +1) e clicar no nome do mês, quando ele não é o atual, volta para hoje.
birthday_card_ui <- function(family, view = Sys.Date(), today = Sys.Date()) {
  month <- as.integer(format(view, "%m"))
  year <- as.integer(format(view, "%Y"))
  bdays <- month_birthdays(family, month, year, today)
  real <- family[!family$is_placeholder, , drop = FALSE]
  colors <- nucleus_color_map(real$nucleus)
  is_current <- format(view, "%Y-%m") == format(today, "%Y-%m")
  month_name <- MESES_PT[month]
  month_title <- if (year == as.integer(format(today, "%Y"))) {
    month_name
  } else {
    paste(month_name, "de", year)
  }

  nav_button <- function(step, label, icon_name) {
    tags$button(
      type = "button",
      class = "btn btn-sm btn-link p-0",
      style = "color:#7D3C98;width:26px;",
      `aria-label` = label,
      title = label,
      onclick = paste0(
        "Shiny.setInputValue('birthday_nav', {step: ", step,
        ", at: Date.now()}, {priority: 'event'});"
      ),
      icon(icon_name)
    )
  }

  tags$div(
    style = paste0(
      "border:1px solid #E5E8EA;border-top:4px solid #7D3C98;border-radius:6px;",
      "padding:10px 12px 6px;background:#FFF;margin-bottom:12px;"
    ),
    tags$div(
      style = "display:flex;align-items:center;gap:8px;",
      icon("cake-candles", style = "color:#7D3C98;"),
      tags$div(
        style = "font-weight:600;font-size:0.9rem;color:#2C3E50;flex:1;",
        "Aniversariantes"
      ),
      tags$button(
        type = "button",
        class = "btn-close",
        style = "font-size:0.65rem;",
        `aria-label` = "Remover cartão de aniversariantes",
        title = "Remover",
        onclick = "Shiny.setInputValue('hide_birthdays', Date.now(), {priority: 'event'});"
      )
    ),
    tags$div(
      style = "display:flex;align-items:center;justify-content:space-between;margin:6px 0 4px;",
      nav_button(-1, "Mês anterior", "chevron-left"),
      if (is_current) {
        tags$span(style = "font-size:0.85rem;font-weight:600;color:#4A235A;", month_title)
      } else {
        tags$a(
          href = "#",
          title = "Voltar para o mês atual",
          style = "font-size:0.85rem;font-weight:600;color:#4A235A;text-decoration:none;",
          onclick = paste0(
            "Shiny.setInputValue('birthday_nav', {step: 0, at: Date.now()}, ",
            "{priority: 'event'}); return false;"
          ),
          month_title,
          tags$span(style = "font-weight:400;color:#95A5A6;margin-left:4px;", icon("rotate-left"))
        )
      },
      nav_button(1, "Próximo mês", "chevron-right")
    ),
    if (nrow(bdays) == 0) {
      tags$div(
        style = "font-size:0.8rem;color:#7F8C8D;padding:4px 0 6px;border-top:1px solid #F0F2F3;",
        "Ninguém da família faz aniversário em ", month_name, "."
      )
    } else {
      lapply(seq_len(nrow(bdays)), function(i) {
        birthday_row_ui(bdays[i, ], nucleus_color_for(colors, bdays$nucleus[i]))
      })
    }
  )
}
