# ============================================================
#  func_diagram_yrke_utb.R
#  Diagram för fliken "Utbildning & yrken". Samma mönster som
#  func_diagram.R (.rd_tema(), .girafe_std(), .kalltext()).
# ============================================================

# ---- Färger ---------------------------------------------------------------

# Sekventiell ramp (ljus -> mörk, en kulör) för ordnade kategorier:
# utbildningsnivå och åldersgrupp.
rd_sekventiell <- function(n) {
  grDevices::colorRampPalette(c("#d3e9f1", RD_PRIMARY, "#0b3f52"), space = "Lab")(n)
}

# Matchning är ordnad (bra -> dålig): divergerande skala med två poler
# och neutral grå mitt. Validerad för färgblindhet.
MATCHNING_FARGER <- c("Helt matchade"   = "#2a78d6",
                      "Delvis matchade" = "#b4b3ad",
                      "Inte matchade"   = "#eb6834")

# Två kategorier utan inbördes ordning (t.ex. bakgrund): kategoriska
# plats 1 och 2 (blå, orange) -- validerade för färgblindhet.
RD_KATEGORISK_2 <- c("#2a78d6", "#eb6834")

# Fyllnadsfärger -> textfärg för etiketter inuti staplar.
.etikettfarg <- function(fyllning) {
  lum <- colSums(grDevices::col2rgb(fyllning) * c(0.299, 0.587, 0.114)) / 255
  ifelse(lum < 0.55, "white", RD_TEXT)
}

# Antal med sekretessprickning, för tooltips.
.antal_txt <- function(antal, troskel = 4) {
  ifelse(antal > 0 & antal < troskel, paste0("färre än ", troskel),
         format(antal, big.mark = " ", scientific = FALSE, trim = TRUE))
}

# ---- 100 %-staplar: fördelning per yrke eller bransch --------------------
# df från fordelning_per_enhet() (en rad per enhet = yrke/bransch och
# kategori). kat_ordning = kategoriernas ordning, farger = namngiven
# vektor (kategori -> färg). sortera_kategori: sortera enheterna efter
# den sammanlagda andelen i en eller flera kategorier (t.ex. "60-67 år"
# och "68+ år"); NULL eller tom sorterar efter storlek. markerad = enhet_kod som framhävs (övriga tonas ned).
skapa_diagram_fordelning <- function(df, kat_ordning, farger,
                                     markerad = NULL,
                                     sortera_kategori = NULL,
                                     rubrik = NULL, underrubrik = NULL,
                                     kalla = NULL) {
  d <- dplyr::filter(df, !is.na(andel))
  if (nrow(d) == 0) {
    return(.girafe_std(.tom_plot("För få sysselsatta i urvalet för att visa fördelning")))
  }

  ordning <- if (length(sortera_kategori) == 0) {
    d |> dplyr::distinct(enhet_namn, total) |> dplyr::arrange(total)
  } else {
    d |>
      dplyr::group_by(enhet_namn) |>
      dplyr::summarise(s = sum(andel[kategori %in% sortera_kategori])) |>
      dplyr::arrange(s)
  }

  markerat <- !is.null(markerad) && nzchar(markerad)
  d <- d |>
    dplyr::mutate(
      enhet_namn = factor(enhet_namn, levels = ordning$enhet_namn),
      kategori  = factor(kategori, levels = rev(kat_ordning)),
      alfa      = if (markerat) dplyr::if_else(enhet_kod == markerad, 1, 0.45) else 1,
      etikett   = dplyr::if_else(andel >= 0.1, scales::percent(andel, accuracy = 1), ""),
      # Nedtonade rader: mörk text (vit text blir oläslig på blek fyllning).
      etikettfarg = dplyr::if_else(alfa < 1, RD_TEXT, .etikettfarg(farger[as.character(kategori)]))
    )

  g <- ggplot2::ggplot(d, ggplot2::aes(x = andel, y = enhet_namn, fill = kategori, group = kategori)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(
        alpha   = alfa,
        tooltip = paste0("<b>", enhet_namn, "</b><br/>", kategori, ": ",
                         scales::percent(andel, accuracy = 0.1),
                         " (", .antal_txt(antal), " av ",
                         format(total, big.mark = " ", trim = TRUE), ")"),
        data_id = enhet_kod
      ),
      width = 0.72, color = "white", linewidth = 0.4
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = etikett, color = etikettfarg),
      position = ggplot2::position_stack(vjust = 0.5), size = 2.8
    ) +
    ggplot2::scale_fill_manual(values = farger, breaks = kat_ordning, name = NULL,
                               drop = TRUE) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_y_discrete(labels = scales::label_wrap(45)) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                                expand = ggplot2::expansion(mult = c(0, 0.01))) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = rubrik, subtitle = underrubrik, caption = .kalltext(kalla)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top",
                   legend.justification = "left",
                   panel.grid.major.x = ggplot2::element_blank()) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = ceiling(length(kat_ordning) / 4)))

  .girafe_std(g,
              width_svg  = 10,
              height_svg = max(4, dplyr::n_distinct(d$enhet_namn) * 0.38 + 2),
              selection  = TRUE)
}

# ---- Liggande staplar: andel inom ett yrke / en utbildning ----------------
# df från andel_inom(). Stapel = vald geografi, grå punkt = riket.
# "Övriga" läggs alltid sist och tonas ned. markerad = kod som ska
# framhävas (övriga staplar tonas ned), t.ex. vald bransch.
skapa_diagram_andel_inom <- function(df, geo_namn = "Vald geografi",
                                     markerad = NULL,
                                     rubrik = NULL, underrubrik = NULL,
                                     kalla = NULL) {
  vald  <- dplyr::filter(df, geo_niva == "vald")
  riket <- dplyr::filter(df, geo_niva == "riket", kod %in% vald$kod)

  ordning <- vald |>
    dplyr::arrange(kod == "övr", dplyr::desc(andel)) |>
    dplyr::pull(namn)
  niv <- rev(ordning)

  markerad <- markerad %||% ""
  vald  <- dplyr::mutate(vald, namn = factor(namn, levels = niv),
                         fyll = dplyr::case_when(
                           kod == "övr"                       ~ "grey75",
                           nzchar(markerad) & kod != markerad ~ "#b9dbe7",
                           TRUE                               ~ RD_PRIMARY),
                         etikettfarg = .etikettfarg(fyll))
  riket <- dplyr::mutate(riket, namn = factor(namn, levels = niv))

  g <- ggplot2::ggplot(vald, ggplot2::aes(x = andel, y = namn)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(
        fill    = fyll,
        tooltip = paste0("<b>", namn, "</b><br/>", geo_namn, ": ",
                         scales::percent(andel, accuracy = 0.1),
                         " (", .antal_txt(antal), ")"),
        data_id = paste0("v_", kod)
      ),
      width = 0.7
    ) +
    # Etikett inuti stapelns ände, så den inte krockar med rikspunkten.
    ggplot2::geom_text(ggplot2::aes(label = dplyr::if_else(andel >= 0.03,
                                                           scales::percent(andel, accuracy = 1), ""),
                                    color = etikettfarg),
                       hjust = 1.2, size = 3) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_discrete(labels = scales::label_wrap(45)) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                                expand = ggplot2::expansion(mult = c(0, 0.04))) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = rubrik, subtitle = underrubrik, caption = .kalltext(kalla)) +
    .rd_tema()

  if (nrow(riket) > 0) {
    g <- g +
      ggiraph::geom_point_interactive(
        data = riket,
        ggplot2::aes(
          tooltip = paste0("<b>", namn, "</b><br/>Riket: ",
                           scales::percent(andel, accuracy = 0.1)),
          data_id = paste0("r_", kod)
        ),
        shape = 21, size = 3, fill = RD_TEXT_MUTED, color = "white", stroke = 0.8
      )
  }

  .girafe_std(g,
              width_svg  = 9,
              height_svg = max(3.5, nrow(vald) * 0.36 + 1.8))
}

# ---- Mosaik (treemap): andel inom ett yrke / en utbildning -----------------
# Samma idé som mosaiken i Power BI-rapporten: rutornas yta = andel, de
# största uppe till vänster. Layouten räknas fram här (squarified
# treemap, Bruls m.fl. 2000) och ritas med geom_rect_interactive, så
# att hover, tooltip och klick fungerar som i övriga diagram.

# Rektanglar för ytor (sorterade fallande, summa = b * h) inom b x h.
.squarify <- function(ytor, b, h) {
  n <- length(ytor)
  ut <- matrix(NA_real_, n, 4, dimnames = list(NULL, c("xmin", "xmax", "ymin", "ymax")))
  x <- 0; y <- 0; i <- 1

  samst <- function(idx, kort) {
    s <- sum(ytor[idx])
    max(kort^2 * max(ytor[idx]) / s^2, s^2 / (kort^2 * min(ytor[idx])))
  }

  while (i <= n) {
    kort <- min(b, h)
    j <- i
    while (j < n && samst(i:(j + 1), kort) <= samst(i:j, kort)) j <- j + 1
    idx <- i:j
    s <- sum(ytor[idx])

    if (b >= h) {                 # kolumn längs vänsterkanten
      bredd <- s / h; yy <- y
      for (k in idx) { hh <- ytor[k] / bredd; ut[k, ] <- c(x, x + bredd, yy, yy + hh); yy <- yy + hh }
      x <- x + bredd; b <- b - bredd
    } else {                      # rad längs överkanten
      hojd <- s / b; xx <- x
      for (k in idx) { bb <- ytor[k] / hojd; ut[k, ] <- c(xx, xx + bb, y, y + hojd); xx <- xx + bb }
      y <- y + hojd; h <- h - hojd
    }
    i <- j + 1
  }
  tibble::as_tibble(ut)
}

# Etikett som får plats i en ruta: namnet radbrutet och avkortat, plus
# andelen på egen rad. Tecken per enhet/rad är kalibrerat mot
# width_svg = 10 tum, bredd 16 enheter och textstorlek 3.
.ruta_etikett <- function(namn, andel, bredd, hojd) {
  tecken <- floor(bredd * 6.8)
  rader  <- floor(hojd * 2.6)
  purrr::pmap_chr(list(namn, andel, tecken, rader), function(nm, a, t, r) {
    pct <- scales::percent(a, accuracy = 0.1, decimal.mark = ",")
    if (t < 5 || r < 1) return("")
    if (r < 2) return(if (t >= nchar(pct)) pct else "")
    txt <- strwrap(nm, width = t)
    # strwrap bryter inte långa ord -- korta av rader som ändå är för breda.
    for_lang <- nchar(txt) > t
    txt[for_lang] <- paste0(substr(txt[for_lang], 1, t - 1), "\u2026")
    if (length(txt) > r - 1) {
      txt <- txt[seq_len(r - 1)]
      txt[r - 1] <- paste0(substr(txt[r - 1], 1, max(1, t - 1)), "…")
    }
    paste(c(txt, pct), collapse = "\n")
  })
}

# df från andel_inom() (geo_niva, kod, namn, antal, total, andel).
# Rutorna gäller vald geografi; rikets andel visas i tooltip.
skapa_diagram_mosaik <- function(df, geo_namn = "Vald geografi",
                                 rubrik = NULL, underrubrik = NULL, kalla = NULL) {
  B <- 16; H <- 9

  riket <- df |>
    dplyr::filter(geo_niva == "riket") |>
    dplyr::select(kod, andel_riket = andel)

  d <- df |>
    dplyr::filter(geo_niva == "vald", antal > 0) |>
    dplyr::arrange(kod == "övr", dplyr::desc(andel)) |>
    dplyr::left_join(riket, by = "kod")

  d <- dplyr::bind_cols(d, .squarify(d$andel / sum(d$andel) * B * H, B, H)) |>
    dplyr::mutate(
      # y räknas uppifrån i layouten, ggplot räknar nedifrån
      ymin_g = H - ymax, ymax_g = H - ymin,
      fyll = dplyr::if_else(
        kod == "övr", "#d9d9d6",
        grDevices::colorRampPalette(c("#bcdde9", RD_PRIMARY, "#0b3f52"), space = "Lab")(100)[
          pmax(1, ceiling(andel / max(andel[kod != "övr"]) * 100))]
      ),
      textfarg = .etikettfarg(fyll),
      etikett  = .ruta_etikett(namn, andel, xmax - xmin, ymax - ymin),
      tooltip  = paste0(
        "<b>", namn, "</b><br/>", geo_namn, ": ",
        scales::percent(andel, accuracy = 0.1, decimal.mark = ","),
        " (", .antal_txt(antal), ")",
        dplyr::if_else(is.na(andel_riket), "",
                       paste0("<br/>Riket: ", scales::percent(andel_riket, accuracy = 0.1,
                                                               decimal.mark = ",")))
      )
    )

  g <- ggplot2::ggplot(d) +
    ggiraph::geom_rect_interactive(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin_g, ymax = ymax_g,
                   fill = fyll, tooltip = tooltip, data_id = kod),
      color = "white", linewidth = 0.8
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = xmin + 0.1, y = ymax_g - 0.1, label = etikett, color = textfarg),
      hjust = 0, vjust = 1, size = 3, lineheight = 0.95
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = NULL, y = NULL,
                  title = rubrik, subtitle = underrubrik, caption = .kalltext(kalla)) +
    .rd_tema() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank())

  .girafe_std(g, width_svg = 10, height_svg = 6.4, selection = TRUE)
}

# Tabell: de n vanligaste (exkl. "Övriga") med andel i vald geografi och
# riket, samt summarad. rd-table-styling från regiondalarna_ruf.css.
skapa_tabell_topp <- function(df, kolumnrubrik, geo_namn = "Vald geografi", n = 5) {
  pct <- function(x) ifelse(is.na(x), "–",
                            scales::percent(x, accuracy = 0.1, decimal.mark = ","))
  riket <- df |>
    dplyr::filter(geo_niva == "riket") |>
    dplyr::select(kod, andel_riket = andel)
  d <- df |>
    dplyr::filter(geo_niva == "vald", kod != "övr") |>
    dplyr::slice_max(andel, n = n, with_ties = FALSE) |>
    dplyr::left_join(riket, by = "kod")
  visa_riket <- nrow(riket) > 0

  rad <- function(namn, a, ar, tag = shiny::tags$td) {
    shiny::tags$tr(tag(namn), tag(class = "rd-num", pct(a)),
                   if (visa_riket) tag(class = "rd-num", pct(ar)))
  }

  shiny::tags$table(
    class = "rd-table rd-table--topp",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th(kolumnrubrik), shiny::tags$th(class = "rd-num", geo_namn),
      if (visa_riket) shiny::tags$th(class = "rd-num", "Riket"))),
    shiny::tags$tbody(purrr::pmap(list(d$namn, d$andel, d$andel_riket), rad)),
    shiny::tags$tfoot(rad("Totalt", sum(d$andel),
                          if (visa_riket) sum(d$andel_riket, na.rm = TRUE) else NA,
                          tag = shiny::tags$th))
  )
}
