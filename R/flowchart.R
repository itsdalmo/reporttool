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
  width <- 0.80; height <- 0.50
  palette <- rt_defaults("ggcolors")
  coords <- flowchart_coords
  
  # Create plot
  p <- ggplot(xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  
  # Set limits and add theme
  p <- p + xlim(0, 10) + ylim(0, 10) + plot_theme() + theme(panel.grid = element_blank(),
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
    
    "x" = c(1.50, 1.50, 1.50, 1.50, 4.00, 6.50, 9.00),
    "y" = c(9.00, 6.60, 4.10, 1.60, 6.10, 6.10, 6.10)),
  
  "lines" = list(
    
    "x" = list(c(0.70, 0.18), c(0.70, 0.00), c(0.18, 0.18), c(0.00, 0.00)),
    "y" = list(c(9.00, 9.00), c(6.60, 6.60), c(9.00, 1.50), c(6.60, 1.75))),
  
  "weights" = list(
    
    "x" = c("image_expect"   = 1.75,
            "image_prodq"    = 0.50,
            "image_servq"    = 0.50,
            "expect_prodq"   = 1.75,
            "expect_servq"   = 0.50,
            "prodq_servq"    = 1.75,
            "prodq_value"    = 3.60,
            "servq_value"    = 2.65,
            "image_epsi"     = 4.90,
            "prodq_epsi"     = 4.90,
            "servq_epsi"     = 4.90,
            "value_epsi"     = 5.25,
            "epsi_loyal"     = 7.75
    ),
    
    "y" = c("image_expect"   = 7.80,
            "image_prodq"    = 4.35,
            "image_servq"    = 1.30,
            "expect_prodq"   = 5.35,
            "expect_servq"   = 1.91,
            "prodq_servq"    = 2.85,
            "prodq_value"    = 4.00,
            "servq_value"    = 5.40,
            "image_epsi"     = 7.80,
            "prodq_epsi"     = 5.20,
            "servq_epsi"     = 3.75,
            "value_epsi"     = 6.30,
            "epsi_loyal"     = 6.30
    )), 
  
  "arrows" = list(
    
    "x" = list("image_expect"   = c(1.50, 1.50),
               "image_prodq"    = c(0.18, 0.70),
               "image_servq"    = c(0.18, 0.70),
               "expect_prodq"   = c(1.50, 1.50),
               "expect_servq"   = c(0.00, 0.70),
               "prodq_servq"    = c(1.50, 1.50),
               "prodq_value"    = c(2.30, 3.20),
               "servq_value"    = c(2.30, 3.99),
               "image_epsi"     = c(2.30, 6.49),
               "prodq_epsi"     = c(2.30, 5.70),
               "servq_epsi"     = c(2.30, 6.49),
               "value_epsi"     = c(4.80, 5.70),
               "epsi_loyal"     = c(7.30, 8.20)
    ),
    
    "y" = list("image_expect"   = c(8.50, 7.10),
               "image_prodq"    = c(4.10, 4.10),
               "image_servq"    = c(1.50, 1.50),
               "expect_prodq"   = c(6.10, 4.60),
               "expect_servq"   = c(1.75, 1.75),
               "prodq_servq"    = c(3.60, 2.10),
               "prodq_value"    = c(4.10, 6.10),
               "servq_value"    = c(1.60, 5.59),
               "image_epsi"     = c(9.00, 6.61),
               "prodq_epsi"     = c(4.10, 6.10),
               "servq_epsi"     = c(1.60, 5.59),
               "value_epsi"     = c(6.10, 6.10),
               "epsi_loyal"     = c(6.10, 6.10)
    )) 
  
)
