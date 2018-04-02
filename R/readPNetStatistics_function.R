#' @title readPNetStatistics
#'
#' @description Extracts matrix from PNet simulated text file. Taken from MPNet manual.
#' @param filename Simulated .txt file
#' @export
#' @return matrix
#' @examples\donttest{
#' SimADJ <- readPNetStatistics('mytest_Network_A_1001000.txt')
#' }
readPNetStatistics <- function(filename)
{
  impordata <- scan(filename,what='character',quiet= TRUE)
  n <- as.numeric(impordata[(grep("*vertices",impordata)+1)])
  impordata <-
    impordata[(grep("*matrix",impordata)+1):(grep("*matrix",impordata)+n[1]
                                             *n[1])]
  AdjMatrix <- matrix(as.numeric(impordata),n,n,byrow=T)
  return(AdjMatrix)
}
