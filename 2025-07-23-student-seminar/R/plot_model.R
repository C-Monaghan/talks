plot_model <- function(model) {
  
  # Design matrix for faceting
  design <- "
  AABBCC 
  DDEEFF
  ##GG##
"
  # Plotting results
  model |>
    ggplot(aes(x = estimate, y = transition, colour = colour)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
    ggstance::geom_pointrangeh(
      aes(xmin = conf.low, xmax = conf.high),
      position = ggstance::position_dodgev(height = 0.5),
      linewidth = 2,
      fatten = 6) +
    # Estimates and CIS
    ggtext::geom_richtext(
      aes(x = label_pos, label = label_text),
      position = ggstance::position_dodge2v(height = 0.5),
      hjust = 0,
      size = 3,
      fill = NA, label.colour = NA,
      show.legend = FALSE) +
    # Customization
    scale_colour_manual(values = c(
      "Positive" = "#f2cdcd", 
      "Negative" = "#cba6f7", 
      "NS"       = "#B2BEB5")) +
    # scale_x_continuous(expand = expansion(mult = 0.025, add = 0.025)) +
    labs(
      # title = "Odds ratios",
      x = "Odds Ratio", y = NULL) +
    guides(colour = "none") +
    ggh4x::facet_manual(
      ~ term, scales = "free_x", 
      design = design, labeller = labeller(
        term = c("Depression" = "Apathy")
      )) +
    coord_cartesian(clip = "off")
}