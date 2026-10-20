# ============================================================
#  func_diagram.R
#  Diagramhjälpare. Interaktiva via ggiraph/girafe (hover + klick).
#  Färger och interaktiv css definieras centralt i def_farger.R.
#
#  .rd_tema() och .girafe_std() är samma delade mönster som i
#  utbildningsappens func_diagram.R (varje app definierar dem lokalt).
# ============================================================

# Källtext -> caption (med "Källa: "-prefix). NULL ger ingen caption.
.kalltext <- function(kalla) {
  if (is.null(kalla) || !nzchar(kalla)) NULL else paste0("K\u00e4lla: ", kalla)
}

# Liten platshållarplot.
.tom_plot <- function(msg = "") {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = msg, color = RD_TEXT_MUTED, size = 3) +
    ggplot2::theme_void()
}

# Gemensamt, avskalat tema.
.rd_tema <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.y    = ggplot2::element_blank(),
      panel.grid.minor      = ggplot2::element_blank(),
      axis.title.y          = ggplot2::element_blank(),
      axis.title.x          = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = ggplot2::element_text(face = "bold", size = 12.5, color = RD_TEXT,
                                            margin = ggplot2::margin(b = 1)),
      plot.subtitle = ggplot2::element_text(size = 9.5, color = RD_TEXT_MUTED,
                                            margin = ggplot2::margin(b = 6)),
      plot.caption  = ggplot2::element_text(size = 7.5, color = RD_TEXT_MUTED, hjust = 0,
                                            margin = ggplot2::margin(t = 8)),
      plot.margin   = ggplot2::margin(6, 12, 5, 5)
    )
}

# Standardiserad girafe. selection = TRUE ger klickbar korsfiltrering.
# selected: data_id (som text) för det element som ska visas som valt redan
# vid render -- ggiraph ritar då själv en kant (RD_SELECT_CSS) runt just
# det elementet, utan att diagrammet behöver rita en egen markering.
.girafe_std <- function(g, width_svg = 9, height_svg = 6, selection = FALSE,
                        selected = character(0)) {
  opts <- list(
    ggiraph::opts_hover(css = RD_HOVER_CSS),
    ggiraph::opts_tooltip(css = RD_TOOLTIP_CSS),
    ggiraph::opts_sizing(rescale = TRUE, width = 1),
    ggiraph::opts_toolbar(saveaspng = TRUE,
                          hidden = c("lasso_select", "lasso_deselect"))
  )
  if (selection) {
    opts <- c(opts, list(ggiraph::opts_selection(
      type = "single", only_shiny = TRUE, css = RD_SELECT_CSS, selected = selected)))
  }
  ggiraph::girafe(ggobj = g, width_svg = width_svg, height_svg = height_svg,
                  options = opts)
}

# ---- Lollipop: andel sysselsatta per bransch, tre geografinivåer --------
# (vald geografi / länet / riket -- samma grundidé som inspirationsbilderna
# i önskemålsdokumentet, utökad med tre jämförelsepunkter per bransch.)
skapa_diagram_bransch_jamforelse <- function(df, markerad_grupp = "", geo_namn = "Vald geografi",
                                             rubrik = NULL, underrubrik = NULL,
                                             kalla = NULL) {
  d <- df |>
    dplyr::group_by(geo_niva) |>
    dplyr::mutate(andel = antal / sum(antal, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      geo_niva_etikett = factor(geo_niva,
                                levels = c("riket", "lan", "vald"),
                                labels = c("Riket", "Dalarna", geo_namn)
      ),
      markerad = grupp_namn %in% markerad_grupp
    )

  # Sortera efter "vald" om den finns (en kommun); annars efter "lan"
  # (vald geografi ÄR länet, så det finns ingen separat "vald"-rad).
  sorteringsniva <- if ("vald" %in% d$geo_niva) "vald" else "lan"
  ordning <- d |>
    dplyr::filter(geo_niva == sorteringsniva) |>
    dplyr::arrange(dplyr::desc(andel)) |>
    dplyr::pull(grupp_namn)

  d <- d |> dplyr::mutate(grupp_namn = factor(grupp_namn, levels = rev(unique(ordning))))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = andel, y = grupp_namn)) +
    ggplot2::geom_line(ggplot2::aes(group = grupp_namn), color = "grey85", linewidth = 3) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        color   = geo_niva_etikett,
        size    = ifelse(markerad, 4.2, 2.8),
        tooltip = paste0("<b>", grupp_namn, "</b><br/>", geo_niva_etikett, ": ",
                         scales::percent(andel, accuracy = 0.1)),
        data_id = paste(grupp_namn, geo_niva_etikett)
      )
    ) +
    ggplot2::scale_color_manual(values = stats::setNames(
      c(RD_TEXT_MUTED, RD_ACCENT, RD_PRIMARY),
      c("Riket", "Dalarna", geo_namn)
    ), name = NULL) +
    ggplot2::scale_size_identity() +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                                expand = ggplot2::expansion(mult = c(0, 0.04))) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = rubrik, subtitle = underrubrik, caption = .kalltext(kalla)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top")

  .girafe_std(g,
              width_svg  = 9,
              height_svg = max(5, dplyr::n_distinct(d$grupp_namn) * 0.4 + 1.5),
              selection  = TRUE
  )
}

# ---- Stapeldiagram: antal sysselsatta per bransch ------------------------
# Klickbar (data_id = grupp_namn) -- fångas upp av modulen som
# input$<outputId>_selected och styr tidsseriediagrammet nedan. Rader med
# NA (sekretessbelagda smávärden, se prick_smavarden()) visas som en tom
# stapel med en egen förklarande tooltip i stället för att döljas, så att
# branschlistan inte ser ofullständig ut.
skapa_diagram_bransch_sysselsatta <- function(df, klickad = NULL, farg = RD_PRIMARY,
                                              rubrik = NULL, underrubrik = NULL,
                                              kalla = NULL) {
  if (nrow(df) == 0) {
    return(.girafe_std(.tom_plot("Inga värden att visa"), height_svg = 1.5))
  }
  ordning <- df |> dplyr::arrange(dplyr::desc(dplyr::coalesce(antal, 0))) |> dplyr::pull(grupp_namn)
  niv <- rev(ordning)

  d <- df |>
    dplyr::mutate(
      grupp_namn = factor(grupp_namn, levels = niv),
      dold       = is.na(antal),
      antal_vy   = dplyr::if_else(dold, 0, antal),
      tooltip    = dplyr::if_else(
        dold,
        paste0("<b>", grupp_namn, "</b><br/>Färre än 4 (redovisas inte av sekretesskäl)"),
        paste0("<b>", grupp_namn, "</b><br/>", scales::number(antal, big.mark = " "), " sysselsatta")
      )
    )

  rand <- intersect(as.character(klickad), as.character(d$grupp_namn))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = antal_vy, y = grupp_namn)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = grupp_namn),
      fill = farg, width = 0.72
    ) +
    ggplot2::scale_x_continuous(labels = scales::number_format(big.mark = " "),
                                expand = ggplot2::expansion(mult = c(0, 0.04))) +
    ggplot2::labs(x = NULL, y = NULL, title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla)) +
    .rd_tema()

  .girafe_std(g, width_svg = 9, height_svg = max(5, dplyr::n_distinct(d$grupp_namn) * 0.38 + 1.5),
              selection = TRUE, selected = rand)
}

# Könsfärg utifrån den faktiska textsträngen i källdatan (okänd exakt
# stavning/skiftläge, se func_data.R) -- case-okänslig delmatchning mot
# KON_FARGER, annars accentfärgen.
.kon_farg_match <- function(namn) {
  n <- tolower(namn %||% "")
  if (grepl("kvin", n)) return(unname(KON_FARGER["Kvinnor"]))
  if (grepl("män|man", n)) return(unname(KON_FARGER["Män"]))
  RD_ACCENT
}

# ---- Stapeldiagram: antal sysselsatta per bransch, könsuppdelat (staplad) --
# Samma klick-/sekretessmönster som skapa_diagram_bransch_sysselsatta(), men
# staplad per kön (fyllningsfärg) i stället för en enfärgad stapel. df ska
# bara innehålla kön-raderna (Totalt-raden filtreras bort av anroparen).
skapa_diagram_bransch_sysselsatta_kon <- function(df, klickad = NULL,
                                                  rubrik = NULL, underrubrik = NULL,
                                                  kalla = NULL) {
  if (nrow(df) == 0) {
    return(.girafe_std(.tom_plot("Inga värden att visa"), height_svg = 1.5))
  }
  ordning <- df |>
    dplyr::group_by(grupp_namn) |>
    dplyr::summarise(tot = sum(dplyr::coalesce(antal, 0), na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(tot)) |>
    dplyr::pull(grupp_namn)
  niv <- rev(ordning)

  d <- df |>
    dplyr::mutate(
      grupp_namn = factor(grupp_namn, levels = niv),
      dold       = is.na(antal),
      antal_vy   = dplyr::if_else(dold, 0, antal),
      tooltip    = dplyr::if_else(
        dold,
        paste0("<b>", grupp_namn, "</b><br/>", kon, ": Färre än 4 (redovisas inte av sekretesskäl)"),
        paste0("<b>", grupp_namn, "</b><br/>", kon, ": ", scales::number(antal, big.mark = " "))
      )
    )

  rand <- intersect(as.character(klickad), as.character(d$grupp_namn))
  farger <- stats::setNames(vapply(unique(d$kon), .kon_farg_match, character(1)), unique(d$kon))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = antal_vy, y = grupp_namn, fill = kon, group = kon)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = grupp_namn),
      width = 0.72
    ) +
    ggplot2::scale_fill_manual(values = farger, name = NULL) +
    ggplot2::scale_x_continuous(labels = scales::number_format(big.mark = " "),
                                expand = ggplot2::expansion(mult = c(0, 0.04))) +
    ggplot2::labs(x = NULL, y = NULL, title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top")

  .girafe_std(g, width_svg = 9, height_svg = max(5, dplyr::n_distinct(d$grupp_namn) * 0.38 + 1.5),
              selection = TRUE, selected = rand)
}

# ---- Linjediagram: utveckling av antal sysselsatta över tid --------------
skapa_diagram_sysselsatta_tid <- function(df, farg = RD_PRIMARY, rubrik = NULL,
                                          underrubrik = NULL, kalla = NULL) {
  if (nrow(df) == 0 || all(is.na(df$antal))) {
    return(.girafe_std(.tom_plot("Inga värden att visa"), height_svg = 1.5))
  }
  d <- df |>
    dplyr::mutate(tooltip = paste0("<b>", ar, "</b><br/>",
                                   dplyr::if_else(is.na(antal), "Färre än 4",
                                                  paste(scales::number(antal, big.mark = " "), "sysselsatta"))))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = ar, y = antal)) +
    ggplot2::geom_line(color = farg, linewidth = 1, na.rm = TRUE) +
    ggiraph::geom_point_interactive(ggplot2::aes(tooltip = tooltip), color = farg, size = 2.2,
                                    na.rm = TRUE) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty()) +
    ggplot2::scale_y_continuous(labels = scales::number_format(big.mark = " "),
                                expand = ggplot2::expansion(mult = c(0.08, 0.1))) +
    ggplot2::labs(x = NULL, y = NULL, title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla)) +
    .rd_tema()

  .girafe_std(g, width_svg = 9, height_svg = 4.2, selection = FALSE)
}

# ---- Linjediagram: utveckling av antal sysselsatta över tid, könsuppdelat --
# Samma uppbyggnad som skapa_diagram_sysselsatta_tid(), men en linje per
# kön (färg via .kon_farg_match(), legend högst upp) i stället för en enda
# totallinje. df ska innehålla en kon-kolumn (se hamta_sysselsatta_tid()).
skapa_diagram_sysselsatta_tid_kon <- function(df, rubrik = NULL, underrubrik = NULL, kalla = NULL) {
  if (nrow(df) == 0 || all(is.na(df$antal))) {
    return(.girafe_std(.tom_plot("Inga värden att visa"), height_svg = 1.5))
  }
  d <- df |>
    dplyr::mutate(tooltip = paste0("<b>", ar, "</b><br/>", kon, ": ",
                                   dplyr::if_else(is.na(antal), "Färre än 4",
                                                  paste(scales::number(antal, big.mark = " "), "sysselsatta"))))

  farger <- stats::setNames(vapply(unique(d$kon), .kon_farg_match, character(1)), unique(d$kon))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = ar, y = antal, color = kon, group = kon)) +
    ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
    ggiraph::geom_point_interactive(ggplot2::aes(tooltip = tooltip), size = 2.2, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = farger, name = NULL) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty()) +
    ggplot2::scale_y_continuous(labels = scales::number_format(big.mark = " "),
                                expand = ggplot2::expansion(mult = c(0.08, 0.1))) +
    ggplot2::labs(x = NULL, y = NULL, title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top")

  .girafe_std(g, width_svg = 9, height_svg = 4.2, selection = FALSE)
}
