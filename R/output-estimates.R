output <- function(psi, nuisance, call) {
  structure(
    list(
      psi = psi,
      nuisance = get_item(nuisance, "pred"),
      fits = get_item(nuisance, "fits"),
      call = call
    ),
    class = "transported_ate"
  )
}
