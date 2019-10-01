#' @title A Cat Function
#'
#' @description This function allows you to express your love for the superior furry animal.
#' @param agree Do you agree cats are the best pet? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples cats_over_dogs()

cats_over_dogs <- function(agree=TRUE){
  if(agree==TRUE){
    print("Meow meow!")
  }
  else {
    print("Try again.")
  }
}
