#' Flowchart
#'
#' Create a flowchart with weights from PLS-PM (inner weights). 
#'
#' @param scores The scores/results for each latent.
#' @param weights A vector with inner weights from PLS-PM.
#' @param latents The names to used for the aspects in the EPSI model. (language)
#' @author Kristian D. Olsen
#' @note Alternately you can supply a lone survey object.
#' @import ggplot2
#' @export

flowchart <- function(...) {
  UseMethod("flowchart")
}

#' @export
flowchart.survey <- function(survey) {
  stop("Not ready.", call. = FALSE)
}

#' @export
flowchart.default <- function(scores, weights, latents = rt_defaults("latent_names")) {
  
  # Defaults
  width <- 0.080; height <- 0.050
  palette <- rt_defaults("ggcolors")
  coords <- flowchart_coords
  
  # Create plot
  p <- ggplot(xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  
  # Set limits and add theme
  p <- p + xlim(0, 1) + ylim(0, 1) + plot_theme() + theme(panel.grid = element_blank(),
                                                          axis.text = element_blank())
  
  # Draw lines
  for (i in seq_along(coords$lines$x)) {
    
    p <- p + geom_path(aes_q(x = coords$lines$x[[i]], y = coords$lines$y[[i]]), 
                       size = .45, colour = "#23373b")
    
  }
  
  # Draw arrows
  for (i in seq_along(coords$arrows$x)) {
    
    p <- p + geom_path(aes_q(x = coords$arrows$x[[i]], y = coords$arrows$y[[i]]), 
                       size = .45, colour = "#23373b", 
                       arrow = grid::arrow(length = grid::unit(0.2, "cm"), type = "closed"))
    
  }
  
  # Draw inner weights (numbers)
  p <- p + geom_text(aes_q(label = sprintf("%.2f", weights), x = coords$weights$x, 
                           y = coords$weights$y), size = 3, colour = "#23373b")
  
  
  
  # Plot rectangles and text
  p <- p + geom_rect(aes_q(xmin = coords$boxes$x - width, xmax = coords$boxes$x + width, 
                         ymin = coords$boxes$y - height, ymax = coords$boxes$y + height), fill = palette[2])
  
  # Label rectangles
  p <- p + geom_text(aes_q(label = latents, x = coords$boxes$x, y = coords$boxes$y), 
                     colour = "white", size = 3, vjust = -.7, fontface = "bold")
  
  # Add score
  p <- p + geom_text(aes_q(label = sprintf("%.1f", scores), x = coords$boxes$x, 
                           y = coords$boxes$y), colour = "white", size = 3, vjust = 1.1)
  
  # Return plot
  p
  
}

# Coordinates for the flowchart ------------------------------------------------

flowchart_coords <- list(
  
  "boxes" = list(
    
    "x" = c(0.150, 0.150, 0.150, 0.150, 0.400, 0.650, 0.900),
    "y" = c(0.900, 0.660, 0.410, 0.160, 0.610, 0.610, 0.610)),
  
  "lines" = list(
    
    "x" = list(c(0.070, 0.018), c(0.070, 0.000), c(0.018, 0.018), c(0.000, 0.000)),
    "y" = list(c(0.900, 0.900), c(0.660, 0.660), c(0.900, 0.150), c(0.660, 0.1750))),
  
  "weights" = list(
    
    "x" = c("image_expect"   = 0.175,
            "image_prodq"    = 0.050,
            "image_servq"    = 0.050,
            "expect_prodq"   = 0.175,
            "expect_servq"   = 0.050,
            "prodq_servq"    = 0.175,
            "prodq_value"    = 0.360,
            "servq_value"    = 0.265,
            "image_epsi"     = 0.490,
            "prodq_epsi"     = 0.490,
            "servq_epsi"     = 0.490,
            "value_epsi"     = 0.525,
            "epsi_loyal"     = 0.775
    ),
    
    "y" = c("image_expect"   = 0.780,
            "image_prodq"    = 0.435,
            "image_servq"    = 0.130,
            "expect_prodq"   = 0.535,
            "expect_servq"   = 0.191,
            "prodq_servq"    = 0.285,
            "prodq_value"    = 0.400,
            "servq_value"    = 0.540,
            "image_epsi"     = 0.780,
            "prodq_epsi"     = 0.520,
            "servq_epsi"     = 0.375,
            "value_epsi"     = 0.630,
            "epsi_loyal"     = 0.630
    )), 
  
  "arrows" = list(
    
    "x" = list("image_expect"   = c(0.150, 0.150),
               "image_prodq"    = c(0.018, 0.070),
               "image_servq"    = c(0.018, 0.070),
               "expect_prodq"   = c(0.150, 0.150),
               "expect_servq"   = c(0.000, 0.070),
               "prodq_servq"    = c(0.150, 0.150),
               "prodq_value"    = c(0.230, 0.320),
               "servq_value"    = c(0.230, 0.399),
               "image_epsi"     = c(0.230, 0.649),
               "prodq_epsi"     = c(0.230, 0.570),
               "servq_epsi"     = c(0.230, 0.649),
               "value_epsi"     = c(0.480, 0.570),
               "epsi_loyal"     = c(0.730, 0.820)
    ),
    
    "y" = list("image_expect"   = c(0.850, 0.710),
               "image_prodq"    = c(0.410, 0.410),
               "image_servq"    = c(0.150, 0.150),
               "expect_prodq"   = c(0.610, 0.460),
               "expect_servq"   = c(0.175, 0.175),
               "prodq_servq"    = c(0.360, 0.210),
               "prodq_value"    = c(0.410, 0.610),
               "servq_value"    = c(0.160, 0.559),
               "image_epsi"     = c(0.900, 0.661),
               "prodq_epsi"     = c(0.410, 0.610),
               "servq_epsi"     = c(0.160, 0.559),
               "value_epsi"     = c(0.610, 0.610),
               "epsi_loyal"     = c(0.610, 0.610)
    )) 
  
)
