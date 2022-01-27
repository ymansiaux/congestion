#' Génération d'un thème macarons ggplot2
#'
#' @param my_font police google font à utiliser
#'
#' @return
#' @export
#' @importFrom showtext showtext_auto
#' @importFrom sysfonts font_add_google
#' @import ggplot2
#' @examples
theme_macarons <- function(my_font) {
  # inspiré du style macarons de echarts
  # https://echarts.apache.org/en/theme-builder.html
  
  bg_color <- "white"
  
  font_add_google(name = my_font, family = my_font)
  showtext_auto()
  
  title_color <- "#008acd"
  subtitle_color <- "#aaaaaa"
  axis_tick_color <- rgb(51, 51, 51, maxColorValue = 255)
  axis_text_color <- axis_tick_color
  
  theme(text = element_text(family = my_font, size = 22),
        axis.title = element_blank(),
        axis.text = element_text(color = axis_text_color),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.ticks = element_line(color = subtitle_color, size = .5),
        axis.ticks.length.x = unit(.7, "lines"),
        axis.ticks.length.y = unit(.7, "lines"),
        panel.grid = element_blank(),
        plot.margin = margin(20, 0, 20, 0),
        plot.background = element_rect(fill = bg_color, color =  bg_color),
        panel.background = element_rect(fill = bg_color, color =  bg_color),
        plot.title = element_text(color = title_color, size = 30, family = my_font),
        plot.subtitle = element_markdown(color = subtitle_color, size = 22),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        legend.position = "none")
  
  
}