as_transport_task <- function(data, A, Y, W, S, C, id, weights, folds = 1) {
  TransportTask$new(data, A, Y, W, S, C, id, weights, folds)
}
