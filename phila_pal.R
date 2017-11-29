# Adapted from Wes Anderson palettes


#' bf_blue_dark =     '#0f4d90', 
#' bf_blue =          '#2176d2', 
#' bf_blue_light =    '#96c9ff', 
#' electric_blue =    '#25cef7',
#' bell_yellow =      '#f3c613', 
#' flyers_orange =    '#f99300',
#' kellydrive_green = '#58c04d', 
#' light_bell =       '#ffefa2',
#' light_red =        '#fed0d0', 
#' light_kelly =      '#b9f2b1', 
#' light_blue =       '#DAEDFE',
#' phanatic_green =   '#3a833c', 
#' lovepark_red =     '#cc3000', 
#' pride_purple =     '#9400c6',
#' black =            '#000000',
#' dark_gray =        '#444444',
#' medium_gray =      '#a1a1a1', 
#' sidewalk =         '#cfcfcf'

phl_palettes <- list(
  normal = c('#2176d2', '#f3c613', '#58c04d', '#f99300', '#a1a1a1'), 
  light = c('#96c9ff', '#ffefa2', '#fed0d0', '#b9f2b1', '#cfcfcf'),
  dark = c('#0f4d90', '#cc3000', '#3a833c', '#9400c6', '#444444'),
  blues = c('#0f4d90', '#2176d2', '#96c9ff'),
  bgy = c('#2176d2', '#58c04d', '#f3c613'),
  bgy_light = c('#96c9ff', '#ffefa2','#f3c613')
)



phl_palette <- function(name = 'normal', n, type = c("discrete", "continuous")) {
  type <- match.arg(type)
  
  pal <- phl_palettes[[name]]
  if (is.null(pal))
    stop("Palette not found.")
  
  if (missing(n)) {
    n <- length(pal)
  }
  
  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }
  
  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}

#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))
  
  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  
  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}

