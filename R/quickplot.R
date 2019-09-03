#' Quick track plot
#'
#' df dataframe from get_coords_habit or get_coords
#' @export

quickplot <- function(df){
plot(df$rescaleX,df$rescaleY,type='l')
}
