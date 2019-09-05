#' Example Habituation Dataset
#'
#' Habituation dataset is the raw output from python after tracking habituation behavior in an empty box.
#'
#' @format A data frame with 8938 rows and 14 variables:
#' \describe{
#'   \item{X1}{number of rows in data set}
#'   \item{File}{Season of match - refers to starting year}
#'   \item{FPS}{frame rate of camera}
#'   \item{Location_Thresh}{????}
#'   \item{Use_Window}{????}
#'   \item{Window_Weight}{????}
#'   \item{Window_Size}{????}
#'   \item{Start_Frame}{Frame tracking started on }
#'   \item{Frame}{Number of Frames}
#'   \item{X}{X coordinate from object being tracked}
#'   \item{Y}{Y coordinate from object being tracked}
#'   \item{Distance}{Distance object moved during tracking}
#'   \item{ROI}{T/F; T= object tracking in ROI, F= object tracking not in ROI }
#'   \item{ROI_coordinates}{Coordinates of the box}
#' }
"habit"
