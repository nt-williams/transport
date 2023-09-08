#' Specify Control Parameters for Estimating the Transport ATE
#'
#' @return
#' @export
#'
#' @examples
.transport_ate_control <- function() {
    list(folds = 10,
         learners_trt = c("mean", "glm"),
         learners_source = c("mean", "glm"),
         learners_outcome = c("mean", "glm"),
         folds_trt = 10,
         folds_source = 10,
         folds_outcome = 10)
}

.transport_ittate_control <- function() {
    list(folds = 10,
         learners_trt = c("mean", "glm"),
         learners_source = c("mean", "glm"),
         learners_outcome = c("mean", "glm"),
         folds_trt = 10,
         folds_source = 10,
         folds_outcome = 10)
}

.transport_cate_control <- function() {

}
