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

# ---- 100 %-staplar: fördelning per yrke -----------------------------------
# df från fordelning_per_yrke(). kat_ordning = kategoriernas ordning,
# farger = namngiven vektor (kategori -> färg). sortera_kategori: sortera
# yrkena efter andelen i denna kategori (t.ex. "60-67 år"); NULL sorterar
# efter yrkets storlek. Varje yrke är klickbart (data_id = yrke_kod).
skapa_diagram_fordelning_per_yrke <- function(df, kat_ordning, farger,
                                              markerat_yrke = NULL,
                                              sortera_kategori = NULL,
                                              rubrik = NULL, underrubrik = NULL,
                                              kalla = NULL) {
  d <- dplyr::filter(df, !is.na(andel))
  if (nrow(d) == 0) {
    return(.girafe_std(.tom_plot("För få sysselsätta i urvalet för att visa fördelning")))
  }

  ordning <- if (is.null(sortera_kategori)) {
    d |> dplyr::distinct(yrke_namn, total) |> dplyr::arrange(total)
  } else {
    d |>
      dplyr::group_by(yrke_namn) |>
      dplyr::summarise(s = sum(andel[kategori == sortera_kategori])) |>
      dplyr::arrange(s)
  }

  markerat <- !is.null(markerat_yrke) && nzchar(markerat_yrke)
  d <- d |>
    dplyr::mutate(
      yrke_namn = factor(yrke_namn, levels = ordning$yrke_namn),
      kategori  = factor(kategori, levels = rev(kat_ordning)),
      alfa      = if (markerat) dplyr::if_else(yrke_kod == markerat_yrke, 1, 0.45) else 1,
      etikett   = dplyr::if_else(andel >= 0.1, scales::percent(andel, accuracy = 1), ""),
      etikettfarg = .etikettfarg(farger[as.character(kategori)])
    )

  g <- ggplot2::ggplot(d, ggplot2::aes(x = andel, y = yrke_namn, fill = kategori, group = kategori)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(
        alpha   = alfa,
        tooltip = paste0("<b>", yrke_namn, "</b><br/>", kategori, ": ",
                         scales::percent(andel, accuracy = 0.1),
                         " (", .antal_txt(antal), " av ",
                         format(total, big.mark = " ", trim = TRUE), ")"),
        data_id = yrke_kod
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
              height_svg = max(4, dplyr::n_distinct(d$yrke_namn) * 0.38 + 2),
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
