output <- function(psi, nuisance, call, class) {
  structure(
    list(
      psi = psi,
      nuisance = get_item(nuisance, "pred"),
      fits = get_item(nuisance, "fits"),
      call = call
    ),
    class = class
  )
}
