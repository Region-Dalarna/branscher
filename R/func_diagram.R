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
.girafe_std <- function(g, width_svg = 9, height_svg = 6, selection = FALSE) {
  opts <- list(
    ggiraph::opts_hover(css = RD_HOVER_CSS),
    ggiraph::opts_tooltip(css = RD_TOOLTIP_CSS),
    ggiraph::opts_sizing(rescale = TRUE, width = 1),
    ggiraph::opts_toolbar(saveaspng = TRUE,
                          hidden = c("lasso_select", "lasso_deselect"))
  )
  if (selection) {
    opts <- c(opts, list(ggiraph::opts_selection(
      type = "single", only_shiny = TRUE, css = RD_SELECT_CSS)))
  }
  ggiraph::girafe(ggobj = g, width_svg = width_svg, height_svg = height_svg,
                  options = opts)
}

# ---- Lollipop: andel sysselsatta per bransch, tre geografinivåer --------
# (vald geografi / länet / riket -- samma grundidé som inspirationsbilderna
# i önskemålsdokumentet, utökad med tre jämförelsepunkter per bransch.)
skapa_diagram_bransch_jamforelse <- function(df, markerad_grupp = "",
                                             rubrik = NULL, underrubrik = NULL,
                                             kalla = NULL) {
  d <- df |>
    dplyr::group_by(geo_niva) |>
    dplyr::mutate(andel = antal / sum(antal, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      geo_niva_etikett = factor(geo_niva,
                                levels = c("riket", "lan", "vald"),
                                labels = c("Riket", "L\u00e4net", "Vald geografi")
      ),
      markerad = grupp_namn == markerad_grupp
    )

  ordning <- d |>
    dplyr::filter(geo_niva == "vald") |>
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
    ggplot2::scale_color_manual(values = c(
      "Riket"          = RD_TEXT_MUTED,
      "L\u00e4net"     = RD_ACCENT,
      "Vald geografi"  = RD_PRIMARY
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
