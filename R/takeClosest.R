#' find closest function
#' @name takeClosest
#' @keywords internal
NULL

takeClosest <- function(dat1, dat2, id1, id2, time1, time2, val2) {
  c1 <- names(dat1)
  nt <- time2
  nv <- val2
  if(time2 %in% c1) nt <- paste0(nt,'.y')
  if(val2 %in% c1) nv <- paste0(nv,'.y')
  # add new columns to first data set
  dat1[,nt] <- as.POSIXct(NA)
  dat1[,nv] <- NA
  
  dat2 <- dat2[,c(id2, time2, val2)]
  dat2 <- dat2[complete.cases(dat2),]
  ldat <- split(dat2[,c(time2, val2)], dat2[,id2])
  xidc <- as.character(dat1[,id1])
  xdts <- as.Date(dat1[,time1])
  lastid <- -1
  for(i in seq_along(xidc)) {
    curid <- xidc[i]
    if(curid != lastid) {
      i_dat <- ldat[[curid]]
    }
    if(!is.null(i_dat)) {
      mix <- which.min(abs(difftime(xdts[i], i_dat[[1]])))
      dat1[i,c(nt,nv)] <- i_dat[mix,]
    }
    lastid <- curid
  }
  dat1
}
