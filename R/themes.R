#' ggplot2 theme
#'
#' Wrapper to make styling plots for a report less verbose.
#'
#' @param legend Default is bottom. Passed directly to theme call for ggplot2.
#' @author Kristian D. Olsen
#' @return A ggplot2 theme object
#' @export
#' @examples 
#' p <- ggplot(df, aes(x=manifest, y=score, group=mainentity)) + geom_bar()
#' p + plot_theme()

plot_theme <- function(legend="bottom"){
  
  ggplot2::theme(
    
    # Make the background of the entire plot transparent
    panel.background = ggplot2::element_rect(fill = "transparent", colour = NA)
    , plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)
    , legend.background = ggplot2::element_rect(fill = "transparent", colour = NA)
    , legend.key = ggplot2::element_rect(fill = "transparent", colour = NA)
    
    # Margins should also be transparent
    , panel.border = ggplot2::element_rect(fill = "transparent", colour=NA)
    
    # Fix the border and axis lines/ticks
    , axis.ticks = ggplot2::element_blank()
    
    # Format axis text size
    , axis.text.x = ggplot2::element_text(size=9,colour="#23373b")
    , axis.text.y = ggplot2::element_text(size=9,colour="#23373b")
    , axis.title.y = ggplot2::element_blank()
    , axis.title.x = ggplot2::element_blank()
    
    # Format the grid (only horizontal lines)
    , panel.grid.major.x = ggplot2::element_blank()
    , panel.grid.major.y = ggplot2::element_line(colour="#D0D0D0",size=.5)
    
    # Facet-title formatting
    , strip.text = ggplot2::element_text(colour="white")
    , strip.background = ggplot2::element_rect(fill="#23373b")
    
    # Change plot margins
    , panel.margin = grid::unit(2, "lines") # facet margins
    , plot.margin = grid::unit(c(0, 0, 0, 0), "cm")
    
    # Legend position and format
    , legend.position = legend
    , legend.title = ggplot2::element_blank()
  )
  
}

#' @rdname plot_theme
#' @export
plot_shared_legend <- function(...) {
  
  plots <- list(...)
  
  grobs <- ggplot2::ggplotGrob(plots[[1]] + ggplot2::theme(legend.position = "bottom"))$grobs
  legends <- g[[which(vapply(g, function(x) x$name, character(1)) == "guide-box")]]
  heights <- sum(l$height)
  
  gridExtra::grid.arrange(
    do.call(gridExtra::arrangeGrob, lapply(plots, function(x) {
      x + ggplot2::theme(legend.position="none")})),
    legends,
    ncol = 1,
    heights = grid::unit.c(grid::unit(1, "npc") - heights, heights))
  
}

# Table style used for openxlsx wrappers ---------------------------------------

openxlsx_style <- openxlsx::createStyle(fontColour = "#000000", 
                                        fgFill = "#7DC6CC", 
                                        halign = "center", 
                                        valign = "center", 
                                        textDecoration = "Bold")