#' @title multi_selftestplot
#' @description
#' Automated creation of plots from a (multi)WHAM simulation self-test. Main plot shows mean, median, and 95%CI. 
#' Relative error = (Est-True)/True computed for F, R, and SSB.
#' The multi_selftestplot requires another function, selftesterrorfxn, which is included below. Users should not need to explicitly call the selftesterrorfxn. It gets called within multi_selftestplot.
#' @param mod result of fit_wham.
#' @param selftest result of self_test applied to mod.
#' @param pngprefix (default=NULL) prefix added to default names for all saved png files.
#' @param save.direct directory where you want png plots saved.
#' @return saves 3 png files, one each for F, recruitment, and SSB, to the specified directory.
#' @examples
#' mod=fit_wham(input,...)
#' selftest=self_test(mod,...)
#' multi_selftestplot(mod=mod,selftest = selftest,pngprefix = "pngname",save.direct = "C:/yourdirectory/here")
#' 

#calc relative error for SSB, F and Rec.
selftesterrorfxn=function(metric=NULL,mod=NULL,selftest=NULL){
  if(metric=="SSB"){
    true = mod$rep[metric]
    true=true$SSB
    est = sapply(selftest[[1]], function(x) return(x["SSB"]))
    est=matrix(unlist(est), ncol = length(est), byrow = FALSE)
    rel_resid = apply(est,2, function(x) x/true - 1)
    mean.metric=apply(rel_resid,1,mean)
    median.metric=apply(rel_resid,1,median)
    res=data.frame("mean"=mean.metric,"median"=median.metric, "Year"=mod$input$years)
    resid_cis = apply(rel_resid,1,mean) + apply(rel_resid,1,sd)*qnorm(0.975)*t(matrix(c(-1,1),2,length(true)))/sqrt(10)
    res=data.frame(res,"low"=resid_cis[,1],"hi"=resid_cis[,2])
  } 
  if(metric=="Fbar") { #end if metric
    true = mod$rep[metric]
    true=true$Fbar
    est = sapply(selftest[[1]], function(x) return(x["F"]))
    est=matrix(unlist(est), ncol = length(est), byrow = FALSE)
    rel_resid = apply(est,2, function(x) x/true - 1)
    mean.metric=apply(rel_resid,1,mean)
    median.metric=apply(rel_resid,1,median)
    res=data.frame("mean"=mean.metric,"median"=median.metric, "Year"=mod$input$years)
    resid_cis = apply(rel_resid,1,mean) + apply(rel_resid,1,sd)*qnorm(0.975)*t(matrix(c(-1,1),2,length(true)))/sqrt(10)
    res=data.frame(res,"low"=resid_cis[,1],"hi"=resid_cis[,2])  
  } 
  if(metric=="Rec") {
    true = mod$rep["NAA"][[1]][1,1,,1] 
    est = sapply(selftest[[1]], function(x) return(x$NAA[1,1,,1] ))
    est=matrix(unlist(est), ncol = ncol(est), byrow = FALSE)
    rel_resid = apply(est,2, function(x) x/true - 1)
    mean.metric=apply(rel_resid,1,mean)
    median.metric=apply(rel_resid,1,median)
    res=data.frame("mean"=mean.metric,"median"=median.metric, "Year"=mod$input$years)
    resid_cis = apply(rel_resid,1,mean) + apply(rel_resid,1,sd)*qnorm(0.975)*t(matrix(c(-1,1),2,length(true)))/sqrt(10)
    res=data.frame(res,"low"=resid_cis[,1],"hi"=resid_cis[,2])  
  } #end rec
  return(res)
}

#plot and save relative error of SSB, F, and rec.
multi_selftestplot=function(mod=NULL,selftest=NULL,pngprefix=NULL,save.direct=NULL){
  ssb=selftesterrorfxn(metric="SSB",mod=mod,selftest=selftest)
  FishMort=selftesterrorfxn(metric="Fbar",mod=mod,selftest=selftest)
  Rec=selftesterrorfxn(metric="Rec",mod=mod,selftest=selftest)
  
  figs=list()
  
  figs[[1]]=ggplot(ssb, aes(x=Year, y=mean)) +
    geom_line() +
    geom_line(aes(x = Year, y = median), color="blue") +
    geom_ribbon(aes(ymin=low,ymax=hi),alpha=0.3) + 
    geom_hline(yintercept = 0, col="red") +
    #facet_wrap(~VAR, ncol = 3) +
    ylab("Relative Error in SSB") +
    ggtitle("Self-test mean (black) and median (blue) bias with 95% CIs")+
    theme_bw()
  
  figs[[2]]=ggplot(FishMort, aes(x=Year, y=mean)) +
    geom_line() +
    geom_line(aes(x = Year, y = median), color="blue") +
    geom_ribbon(aes(ymin=low,ymax=hi),alpha=0.3) + 
    geom_hline(yintercept = 0, col="red") +
    #facet_wrap(~VAR, ncol = 3) +
    ylab("Relative Error in F") +
    ggtitle("Self-test mean (black) and median (blue) bias with 95% CIs")+
    theme_bw()
  
  figs[[3]]=ggplot(Rec, aes(x=Year, y=mean)) +
    geom_line() +
    geom_line(aes(x = Year, y = median), color="blue") +
    geom_ribbon(aes(ymin=low,ymax=hi),alpha=0.3) + 
    geom_hline(yintercept = 0, col="red") +
    #facet_wrap(~VAR, ncol = 3) +
    ylab("Relative Error in Recruitment") +
    ggtitle("Self-test mean (black) and median (blue) bias with 95% CIs")+
    theme_bw()
  
  ggsave(filename = paste0(pngprefix, "selftest_ssb.png"),path=save.direct, figs[[1]],width=6,height = 4,units="in")
  ggsave(filename = paste0(pngprefix, "selftest_F.png"),path=save.direct, figs[[2]],width=6,height = 4,units="in")
  ggsave(filename = paste0(pngprefix, "selftest_rec.png"),path=save.direct, figs[[3]],width=6,height = 4,units="in")
  
  return(figs)
}
