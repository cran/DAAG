DAAGtheme <-
function (fontsize = list(text = 10, points = 6), box = "gray40", color=TRUE,
          sides = list(tck = 0.6, pad1 = 0.75, pad2 = 0.75),
    ...)
{
    theme <- list(axis.line = list(alpha = 1, col = box,
        fill = "transparent", lty = 1, lwd = 0.5), strip.border = list(alpha = 1,
        col = rep(box, 6), lty = rep(1, 6), lwd = rep(0.5, 6)), 
        strip.shingle = list(alpha = 1, col = rep("gray80", 7)), 
        par.main.text = list(font = 1, fontface="plain", just = "left", 
                             x = grid::unit(5, "mm")), 
        par.sub.text = list(font = 1, just = "left", x = grid::unit(5, "mm")), 
        axis.text = list(cex = 0.8), 
        box.3d = list(col = box), 
        axis.components = list(left = sides, top = sides,
        right = sides, bottom = sides), fontsize = fontsize)
    modifyList(modifyList(lattice::standard.theme("pdf", color=color),
        theme), simpleTheme(...))
}
