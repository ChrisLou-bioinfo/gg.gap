#' @title Add Legend to gg.gap()
#' @description Add legend to gg.gap().
#' @param plot A 'ggplot2' plot.
#' @param margin Margins around the text.
#'
#' @return A legend-added picture
#' @export
#'
#' @examples
#' library(ggplot2)
#' mtcars$gear <- factor(mtcars$gear)
#' bp <- ggplot(data = mtcars, aes(x = gear, fill = gear)) +
#'     geom_bar() +
#'     ggtitle("Number of Cars by Gear") +
#'     xlab("Gears")
#' gg.gap(plot = bp,
#'        ylim = c(0,16),
#'        segments = c(6,8))
#' add.legend(plot = bp,
#'            margin = c(top=1,right=1,bottom=1,left=460))
add.legend <- function(plot,margin=c(top=200,right=200,bottom=200,left=200)){
    legend <- cowplot::get_legend(plot+theme(legend.box.margin =
                                     margin(margin)))
    grid::grid.draw(x = legend,recording=TRUE)
}

