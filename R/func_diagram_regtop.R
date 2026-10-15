# ============================================================
#  func_diagram_regtop.R
#  Diagram för fliken Prognos (RegTop2025). Samma mönster som
#  func_diagram.R (.rd_tema(), .girafe_std(), .kalltext()).
# ============================================================

.tal <- function(x) format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE)
.pct <- function(x, plus = TRUE, accuracy = 0.1) {
  txt <- scales::percent(x, accuracy = accuracy, decimal.mark = ",")
  if (plus) ifelse(!is.na(x) & x > 0, paste0("+", txt), txt) else txt
}

# ---- Förändring per näringsgren: länet och riket ---------------------------
# df från naringsgren_forandring(). Hantel: en rad per näringsgren,
# punkt för länet och riket, sorterat efter länets förändring.
skapa_diagram_naringsgren_forandring <- function(df, basar, ar_val,
                                                 underrubrik = NULL, kalla = NULL) {
  d <- df |>
    dplyr::mutate(geo = dplyr::if_else(regionkod == REGTOP_LAN, "Dalarnas län", "Riket"))
  ordning <- d |>
    dplyr::filter(regionkod == REGTOP_LAN) |>
    dplyr::arrange(forandring_pct) |>
    dplyr::pull(naringsgren)
  d <- d |>
    dplyr::mutate(
      naringsgren = factor(naringsgren, levels = ordning),
      geo = factor(geo, levels = c("Riket", "Dalarnas län")),
      tooltip = paste0("<b>", naringsgren, "</b><br/>", geo, ": ", .pct(forandring_pct),
                       "<br/>", basar, ": ", .tal(antal_bas), " → ", ar_val, ": ",
                       .tal(antal_ar), " (", ifelse(forandring > 0, "+", ""), .tal(forandring), ")")
    )

  g <- ggplot2::ggplot(d, ggplot2::aes(x = forandring_pct, y = naringsgren)) +
    ggplot2::geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
    ggplot2::geom_line(ggplot2::aes(group = naringsgren), color = "grey85", linewidth = 3) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(color = geo, tooltip = tooltip, data_id = paste(naringsgren_kod, geo)),
      size = 3
    ) +
    ggplot2::scale_color_manual(values = c("Riket" = RD_TEXT_MUTED, "Dalarnas län" = RD_PRIMARY),
                                breaks = c("Dalarnas län", "Riket"), name = NULL) +
    ggplot2::scale_x_continuous(labels = function(x) .pct(x, plus = TRUE, accuracy = 1),
                                expand = ggplot2::expansion(mult = 0.06)) +
    ggplot2::labs(x = paste0("Förändring ", basar, "–", ar_val), y = NULL,
                  subtitle = underrubrik, caption = .kalltext(kalla)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top", legend.justification = "left")

  .girafe_std(g, width_svg = 10, height_svg = dplyr::n_distinct(d$naringsgren) * 0.32 + 1.8)
}

# ---- Utveckling år för år för valda näringsgrenar --------------------------
# d = hamta_naringsgren_prognos(). visa = "index" (basår = 100, länet
# heldraget och riket streckat) eller "antal" (bara länet). Färg följer
# näringsgrenen i den ordning de valts; prognosåren har grå bakgrund.
skapa_diagram_naringsgren_utveckling <- function(d, koder, basar, visa = c("index", "antal"),
                                                 underrubrik = NULL, kalla = NULL) {
  visa <- match.arg(visa)
  if (length(koder) == 0) {
    return(.girafe_std(.tom_plot("Välj en eller flera näringsgrenar"), height_svg = 1.5))
  }
  koder <- utils::head(koder, length(RD_KATEGORISK))

  dd <- d |>
    dplyr::filter(naringsgren_kod %in% koder,
                  visa == "index" | regionkod == REGTOP_LAN) |>
    dplyr::group_by(regionkod, naringsgren_kod) |>
    dplyr::mutate(bas = varde[ar == basar][1],
                  y = if (visa == "index") varde / bas * 100 else varde) |>
    dplyr::ungroup()

  namn <- dd |> dplyr::distinct(naringsgren_kod, naringsgren)
  namn <- namn[match(koder, namn$naringsgren_kod), ]
  farger <- stats::setNames(RD_KATEGORISK[seq_len(nrow(namn))], namn$naringsgren)

  dd <- dd |>
    dplyr::mutate(
      naringsgren = factor(naringsgren, levels = namn$naringsgren),
      geo = factor(dplyr::if_else(regionkod == REGTOP_LAN, "Dalarnas län", "Riket"),
                   levels = c("Dalarnas län", "Riket")),
      tooltip = paste0("<b>", naringsgren, "</b><br/>", geo, ", ", ar,
                       dplyr::if_else(typ == "prognos", " (prognos)", ""), "<br/>",
                       if (visa == "index") paste0("Index: ", formatC(y, format = "f", digits = 1,
                                                                       decimal.mark = ","), "<br/>") else "",
                       "Antal: ", .tal(varde))
    )

  slut <- max(dd$ar)
  g <- ggplot2::ggplot(dd, ggplot2::aes(x = ar, y = y, color = naringsgren)) +
    ggplot2::annotate("rect", xmin = basar + 0.5, xmax = slut + 0.5, ymin = -Inf, ymax = Inf,
                      fill = "grey96") +
    ggplot2::annotate("text", x = basar + 0.7, y = Inf, label = "Prognos", hjust = 0, vjust = 1.6,
                      size = 3, color = RD_TEXT_MUTED) +
    { if (visa == "index") ggplot2::geom_hline(yintercept = 100, color = "grey70", linewidth = 0.4) } +
    ggiraph::geom_line_interactive(
      ggplot2::aes(linetype = geo, group = interaction(naringsgren, geo),
                   tooltip = paste0("<b>", naringsgren, "</b><br/>", geo),
                   data_id = paste(naringsgren_kod, geo)),
      linewidth = 1
    ) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = paste(naringsgren_kod, geo, ar)),
      size = 1.6
    ) +
    ggplot2::scale_color_manual(values = farger, name = NULL) +
    ggplot2::scale_linetype_manual(values = c("Dalarnas län" = "solid", "Riket" = "22"),
                                   name = NULL, drop = TRUE) +
    ggplot2::scale_x_continuous(breaks = seq(basar, slut, by = 2),
                                expand = ggplot2::expansion(mult = 0.01)) +
    ggplot2::scale_y_continuous(labels = if (visa == "index") function(x) format(x, decimal.mark = ",")
                                         else function(x) .tal(x)) +
    ggplot2::labs(x = NULL, y = if (visa == "index") paste0("Index (", basar, " = 100)")
                               else "Förvärvsarbetande, Dalarnas län",
                  subtitle = underrubrik, caption = .kalltext(kalla)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top", legend.justification = "left",
                   legend.box = "vertical", legend.box.just = "left",
                   panel.grid.major.y = ggplot2::element_line(color = "grey92"),
                   axis.title.y = ggplot2::element_text(size = 9, color = RD_TEXT_MUTED)) +
    ggplot2::guides(color = ggplot2::guide_legend(order = 1, ncol = 2),
                    linetype = if (visa == "index") ggplot2::guide_legend(order = 2) else "none")

  .girafe_std(g, width_svg = 10, height_svg = 5.6)
}

# ---- Arbetskraften per ålder och kön (pyramid), länet ----------------------
# d = hamta_arbetskraft_prognos(). Fyllda staplar = valt år, ram =
# basåret. Kvinnor till vänster, män till höger.
skapa_diagram_arbetskraft_pyramid <- function(d, basar, ar_val, underrubrik = NULL, kalla = NULL) {
  dd <- d |>
    dplyr::filter(regionkod == REGTOP_LAN, ar %in% c(basar, ar_val)) |>
    dplyr::group_by(kon, alder) |>
    dplyr::summarise(bas = sum(varde[ar == basar]), varde = sum(varde[ar == ar_val]),
                     .groups = "drop") |>
    dplyr::mutate(
      tecken = dplyr::if_else(kon == "Kvinnor", -1, 1),
      alder = factor(alder, levels = sortera_aldersgrupper(alder)),
      tooltip = paste0("<b>", kon, " ", alder, " år</b><br/>",
                       basar, ": ", .tal(bas), "<br/>", ar_val, ": ", .tal(varde),
                       " (", .pct(dplyr::if_else(bas > 0, varde / bas - 1, NA_real_)), ")")
    )
  maxv <- max(c(dd$bas, dd$varde))

  g <- ggplot2::ggplot(dd, ggplot2::aes(y = alder)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(x = tecken * varde, fill = kon, tooltip = tooltip, data_id = paste(kon, alder)),
      width = 0.75
    ) +
    ggplot2::geom_col(ggplot2::aes(x = tecken * bas), fill = NA, color = RD_TEXT,
                      linewidth = 0.5, width = 0.75) +
    ggplot2::geom_vline(xintercept = 0, color = "white", linewidth = 1) +
    ggplot2::annotate("text", x = -maxv, y = Inf, label = "Kvinnor", hjust = 0, vjust = 1.4,
                      size = 3.3, color = RD_TEXT_MUTED) +
    ggplot2::annotate("text", x = maxv, y = Inf, label = "Män", hjust = 1, vjust = 1.4,
                      size = 3.3, color = RD_TEXT_MUTED) +
    ggplot2::scale_fill_manual(values = KON_FARGER, guide = "none") +
    ggplot2::scale_x_continuous(labels = function(x) .tal(abs(x)),
                                limits = c(-maxv, maxv) * 1.05) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = c(0.6, 1.2))) +
    ggplot2::labs(x = NULL, y = "Ålder", subtitle = underrubrik, caption = .kalltext(kalla)) +
    .rd_tema() +
    ggplot2::theme(axis.title.y = ggplot2::element_text(size = 9, color = RD_TEXT_MUTED),
                   panel.grid.major.x = ggplot2::element_line(color = "grey92"))

  .girafe_std(g, width_svg = 10, height_svg = 4.6)
}
