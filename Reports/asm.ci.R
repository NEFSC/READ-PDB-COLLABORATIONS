#' @title asm.ci
#' @description Calculate approximate 95% confidence intervals based on log normal variable and its CV. Used in Dan's autoreporting as default for survey index CI calculations.
#'
#' @param x A lognormal variable (or vector of variables), no default.
#' @param cv.x The cv of variable x (or vector of CVs if multiple variables provided), no default.
#' @param bounds The confidence bound to calculate, default = 95.
#'
#' @return A data.frame containing lower (lci) and upper (uci) confidence bounds


asm.ci<-function(x,cv.x,bounds=95){
  #generate approximate 95% confidence intervals based on log normal variable x
  #and it's cv. cv.x
  s<-sqrt(log(1+cv.x^2))
  s<-ifelse(is.finite(s),s,0)
  p<-(1-(bounds/100))/2
  Z<-qnorm(p)
  lci<-x*exp(Z*(s))
  uci<-x*exp(-Z*(s))
  return(data.frame("lci"=lci,"uci"=uci))
}
