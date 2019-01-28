bb_theme <- function(bbobject) {
  p <- bbobject %>% 
    bb_x_axis(label = list(position = "center"), 
              tick = list(outer = F, centered = T)) %>% 
    bb_y_axis(label = list(position = "middle"), 
              padding = 0, min = 0, 
              tick = list(outer = F, fit = FALSE)) %>% 
    bb_legend(position = "right")
  return(p)
}