#' Calculate immobility
#'
#' @param df output of get_coords_habit or get_coords
#' @param N seconds that animal has not moved to count as immobile
#' @param X distance animal cannot have travelled for
#' @param fr Frame Rate
#' @return a list including i) total immobile time, ii) periods of immobility
#' @examples
#' immobility(distcalc(get_coords_habit(habit)), X=1, N=3)
#' immobility(distcalc(get_coords_habit(habit)), X=2, N=1.5)
#' @export


immobility <- function(df, fr=30,N=2, X=2){
df$im <- ifelse(df$distance<X,T,F)
rl <- rle(df$im)
immobile <- data.frame(length = rl$lengths, immobile = rl$values)
immobile$line <- cumsum(immobile$length)
immobile$line1 <- immobile$line - immobile$length+1
immobile$time <- round(immobile$line1/fr,1)
immobile$duration <- round(immobile$length/fr,1)
immobile.out <- immobile[immobile$duration>N & immobile$immobile==T,]

immob <- immobile.out[,c('time','duration')]
total.immob <- sum(immob$duration)
return(list('total'=total.immob, 'periods'=immob))
}
