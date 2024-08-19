# ============================================
# rv.utils
# ============================================

# Script that takes two equal-lenght, possibly multiline, 
# character vectors and return a vector of the two strings
# side by side and strwraped to specified length.
# Also split lines with no spaces and too long for the specified 'n' 
# (i use this to compare translations)
formatTextIn2Cols <- function(n = 40, x1, x2, cols = c("msgid", "msgstr")) {
  x <- list(x1, x2)
  
  y <- lapply(x, strwrap, width = n, simplify = F)
  y <- lapply(y, \(y) lapply(y, \(y) {
    while (any(nchar(y) > n)) {
      x <- match(T, nchar(y) > n)
      y <- c(y[seq_len(x - 1)], strwrap(paste0(
        substr(y[x], 1, n - 2),
        ">> >>",
        substr(y[x], n - 1, nchar(y[x])),
        paste(y[-seq_len(x)], collapse = " ")), n))
    }
    y
  }))
  y_l <- do.call(pmax, lapply(y, lengths))

  y <- Map(y, list(y_l), f = \(y, y_l) {
    Map(y, y_l, f = \(y, y_l) {
      replace(rep(strrep(" ", n), y_l), seq_along(y),
              paste0(y, strrep(" " , n - nchar(y))))
    })
  })

  y <- c(
    strrep("-", 2 * n) |> `substr<-`(1, nchar(cols[[1]], cols[[1]]) |>
      `substr<-`(n + 1, n + nchar(cols[[2]] , cols[[2]]),
    unlist(mapply(y[[1]], y[[2]], FUN = \(y1, y2) c(
      paste0(y1,  y2),  strrep("-", 2 * n) ))))
  y

}
