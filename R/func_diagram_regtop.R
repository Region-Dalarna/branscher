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

# ---- Behov & rekrytering ----------------------------------------------------

# Etiketter som markdown (ggtext) med fet stil för markerade, som i
# skapa_diagram_fordelning().
.etiketter_md <- function(markerade, bredd = 45) {
  function(x) {
    txt <- gsub("\n", "<br>", htmltools::htmlEscape(scales::label_wrap(bredd)(x)))
    ifelse(x %in% markerade, paste0("<b>", txt, "</b>"), txt)
  }
}

# Utbildningar som ett yrke rekryterar från: andel av yrkets
# förvärvsarbetande per utbildningsgrupp (de topp_n största), färgade
# efter SCB:s bedömning 2040 i länet. d = hamta_yrke_utbildning_bedomning()
# filtrerad på ett yrke.
skapa_diagram_yrke_utbildningar <- function(d, topp_n = 15, underrubrik = NULL, kalla = NULL) {
  if (nrow(d) == 0) {
    return(.girafe_std(.tom_plot("Ingen utbildningsstruktur för yrket"), height_svg = 1.5))
  }
  dd <- d |>
    dplyr::slice_max(andel, n = topp_n, with_ties = FALSE) |>
    dplyr::mutate(
      utbildningsgrupp = factor(utbildningsgrupp, levels = rev(utbildningsgrupp)),
      tooltip = paste0("<b>", utbildningsgrupp, "</b><br/>Andel av yrket: ", .pct(andel, plus = FALSE),
                       "<br/>Bedömning 2040: ", bedomning,
                       dplyr::if_else(is.na(balanstal), "",
                                      paste0("<br/>Balanstal: ",
                                             formatC(balanstal, format = "f", digits = 1, decimal.mark = ","),
                                             " %")))
    )
  ovriga <- 1 - sum(dd$andel)

  g <- ggplot2::ggplot(dd, ggplot2::aes(x = andel, y = utbildningsgrupp)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(fill = bedomning, tooltip = tooltip, data_id = utbildningsgrupp_kod),
      width = 0.72, color = "grey60", linewidth = 0.2
    ) +
    ggplot2::geom_text(ggplot2::aes(label = .pct(andel, plus = FALSE, accuracy = 1)),
                       hjust = -0.2, size = 3, color = RD_TEXT_MUTED) +
    ggplot2::scale_fill_manual(values = BEDOMNING_FARGER, drop = TRUE, name = "Bedömning 2040") +
    ggplot2::scale_y_discrete(labels = scales::label_wrap(45)) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                                expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::labs(x = NULL, y = NULL,
                  subtitle = paste0(underrubrik,
                                    if (ovriga > 0.005) paste0(" · övriga utbildningar: ",
                                                               .pct(ovriga, plus = FALSE)) else ""),
                  caption = .kalltext(kalla)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top", legend.justification = "left",
                   legend.title = ggplot2::element_text(size = 9, color = RD_TEXT_MUTED)) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE))

  .girafe_std(g, width_svg = 10, height_svg = nrow(dd) * 0.36 + 2.2)
}

# Yrken mest beroende av bristutbildningar: andel av yrkets
# förvärvsarbetande med utbildning som bedöms bli stor brist/brist/viss
# brist 2040, staplat per grad. d: yrke_kod, yrke, antal, bedomning,
# andel (bara bristgraderna). markerad = yrke_kod som framhävs (rand +
# fet etikett) och alltid tas med. Klick på ett yrke väljer det.
skapa_diagram_bristberoende <- function(d, markerad = NULL, topp_n = 20,
                                        underrubrik = NULL, kalla = NULL) {
  summa <- d |>
    dplyr::group_by(yrke_kod, yrke, antal) |>
    dplyr::summarise(brist = sum(andel), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(brist))
  valda <- unique(c(utils::head(summa$yrke_kod, topp_n), markerad))
  summa <- dplyr::filter(summa, yrke_kod %in% valda)
  if (nrow(summa) == 0) {
    return(.girafe_std(.tom_plot("Inga yrken att visa"), height_svg = 1.5))
  }

  niv <- rev(summa$yrke)  # störst andel överst
  dd <- d |>
    dplyr::filter(yrke_kod %in% valda) |>
    dplyr::left_join(dplyr::select(summa, yrke_kod, brist), by = "yrke_kod") |>
    dplyr::mutate(
      yrke = factor(yrke, levels = niv),
      bedomning = factor(as.character(bedomning), levels = rev(BEDOMNING_BRIST)),
      tooltip = paste0("<b>", yrke, "</b><br/>", .tal(antal), " förvärvsarbetande<br/>",
                       "Med bristutbildning: ", .pct(brist, plus = FALSE), "<br/>",
                       bedomning, ": ", .pct(andel, plus = FALSE))
    )
  rand <- summa |> dplyr::filter(yrke_kod %in% markerad) |> dplyr::pull(yrke)

  g <- ggplot2::ggplot(dd, ggplot2::aes(x = andel, y = yrke)) +
    ggplot2::geom_tile(data = data.frame(yrke = factor(rand, levels = niv)),
                       ggplot2::aes(x = max(summa$brist) * 0.52, y = yrke),
                       width = max(summa$brist) * 1.04, height = 0.96, fill = "#d6ebf2",
                       inherit.aes = FALSE) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(fill = bedomning, tooltip = tooltip, data_id = yrke_kod),
      width = 0.72, color = "white", linewidth = 0.4
    ) +
    ggplot2::scale_fill_manual(values = BEDOMNING_FARGER[BEDOMNING_BRIST], breaks = BEDOMNING_BRIST,
                               name = "Utbildningens bedömning 2040") +
    ggplot2::scale_y_discrete(limits = niv, labels = .etiketter_md(rand)) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                                expand = ggplot2::expansion(mult = c(0, 0.04))) +
    ggplot2::labs(x = "Andel av yrkets förvärvsarbetande med bristutbildning", y = NULL,
                  subtitle = underrubrik, caption = .kalltext(kalla)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top", legend.justification = "left",
                   legend.title = ggplot2::element_text(size = 9, color = RD_TEXT_MUTED),
                   axis.text.y.left = ggtext::element_markdown(hjust = 1, halign = 1, lineheight = 1.1,
                                                               margin = ggplot2::margin(r = 4)))

  .girafe_std(g, width_svg = 10, height_svg = nrow(summa) * 0.36 + 2.2, selection = TRUE)
}
