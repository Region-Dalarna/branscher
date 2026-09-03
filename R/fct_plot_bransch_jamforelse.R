# =====================================================================
# Interaktivt jämförelsediagram (ggiraph) för flik 1.
#
# Visar andel sysselsatta per bransch som en "lollipop"-rad, med tre
# punkter per bransch: vald geografi, länet (Dalarna) och riket --
# samma grundidé som inspirationsbilderna i önskemålsdokumentet, men
# utökad med tre jämförelsepunkter istället för en.
# =====================================================================

plot_bransch_jamforelse <- function(data, markerad_grupp = "") {

  d <- data |>
    filter(matt == "sysselsatta") |>
    group_by(geografiniva) |>
    mutate(andel = antal / sum(antal, na.rm = TRUE)) |>
    ungroup() |>
    mutate(
      geografiniva = factor(geografiniva,
        levels = c("riket", "lan", "vald"),
        labels = c("Riket", "L\u00e4net", "Vald geografi")
      ),
      markerad = as.character(grupp_id) == as.character(markerad_grupp)
    )

  # Sortera branscherna efter storlek i vald geografi
  ordning <- d |>
    filter(geografiniva == "Vald geografi") |>
    arrange(desc(andel)) |>
    pull(grupp_namn)

  d <- d |> mutate(grupp_namn = factor(grupp_namn, levels = rev(unique(ordning))))

  p <- ggplot(d, aes(x = andel, y = grupp_namn)) +
    geom_line(aes(group = grupp_namn), color = "grey85", linewidth = 3) +
    geom_point_interactive(
      aes(
        color   = geografiniva,
        size    = ifelse(markerad, 4.5, 3),
        tooltip = glue::glue(
          "{grupp_namn}\n{geografiniva}: {scales::percent(andel, accuracy = 0.1)}"
        ),
        data_id = paste(grupp_namn, geografiniva)
      )
    ) +
    scale_color_manual(values = c(
      "Riket"         = RD_GRA,
      "L\u00e4net"    = RD_GUL,
      "Vald geografi" = RD_PETROL
    )) +
    scale_size_identity() +
    scale_x_continuous(labels = scales::percent) +
    labs(x = NULL, y = NULL, color = NULL) +
    theme_minimal(base_family = "Poppins") +
    theme(
      legend.position = "top",
      panel.grid.major.y = element_blank()
    )

  girafe(
    ggobj = p,
    width_svg = 9, height_svg = 7,
    options = list(
      opts_hover(css = "stroke:black;stroke-width:1px;"),
      opts_tooltip(css = "font-family: Poppins; padding: 6px;")
    )
  )
}
