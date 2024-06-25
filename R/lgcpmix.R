lgcpmix <- function(lambda,...){
  if(is.im(lambda)){
    if(any(lambda<0)) stop("'lambda' must be a non-negative real-valued spatial intensity function")
  } else if(inherits(lambda,"bivden")){
    lambda <- lambda$z
  } else {
    stop("'lambda' must be an object of class 'im' (spatstat) or 'bivden' (sparr)")
  }
  
  ellip <- list(...)
  pr <- ellip$param
  md <- ellip$model
  if(is.null(pr)){
    stop("no 'param' values provided")
  } else {
    if(!is.list(pr)) stop("'param' must be a list")
    vr <- pr$var
    if(is.null(vr)) stop("'param$var' must be supplied")
    if(is.null(md)) stop("'model' must be supplied")
  }
  
  w <- as.mask(lambda)

  m <- -vr/2
  ags <- list(model=md,mu=m,param=pr,xy=w,win=w,saveLambda=TRUE,nsim=1,drop=TRUE)
  z <- do.call(spatstat.random::rLGCP,ags)
  zim <- attr(z,"Lambda")
  
  return(lambda*zim)
}
