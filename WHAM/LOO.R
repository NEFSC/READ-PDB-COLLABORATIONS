#' @title Leave-one-out
#' 
#' @description refits a WHAM model sequentially dropping one index at a time 
#' 
#' @param mod An rds from a WHAM fit
#' @param dosdrep TRUE - whether or not to calculate standard deviations
#' 
#' @return A named list with each WHAM fit, where the name is of the index dropped
#'
#' @examples 
#' mod=readRDS("insert path to a WHAM.rds")
#' LOO.mods=LOO(mod=mod,dosdrep=TRUE)
#' You can add the base model to LOO.mods after the fact LOO.mods[["base"]]=mod
#' compare_wham_models can work to create comparison plots

LOO=function(mod=NULL,dosdrep=TRUE){
  #input$par$logit_selpars: rows = selblocks, columns are age-specific pars (1-n_ages), 
  #then logistic selpars, then double logistic selpars
  #input$data$selblock_pointer_fleets (n_years x n_fleets) 
  #and selblock_pointer_indices (n_years x n_indices)
  #input$data$selblock_models is which model for each selblock
  #1: age-specific, 2: logistic
  #index_paa_pars is parameters for age comp likelihoods; a matrix with rows = number of indices 
  #so you would just map that row to be all NA
  
  
  library(wham)
  mods=list()
  n_indices=mod$input$data$n_indices
  n_fleets=mod$input$data$n_fleets
  n_ages=mod$input$data$n_ages
  for(i in 1:n_indices)
  {
    input.temp=mod$input #duplicate input for modification
    input.temp$par=mod$parList #start parameters at values of base model
    
    input.temp$data$use_indices[,i]=0 #tell WHAM not to use the agg indices for index i
    input.temp$data$use_index_paa[,i]=0 #and don't use age comp either
    
    input.temp$map$logit_q[i]=NA #don't estimate catchability for index i
    levels(input.temp$map$logit_q)[i]=NA #change the levels as well as the number must match.
    
    if(length(input.temp$options$age_comp)>1) { #if all age comps are the same then there is only choice recorded here, but if they vary then each option is explicit
      agecomp=list(fleets=input.temp$options$age_comp$fleets,indices=input.temp$options$age_comp$indices)
    } else {
      agecomp=list(fleets=rep(input.temp$options$age_comp,input.temp$data$n_fleets),indices=rep(input.temp$options$age_comp,input.temp$data$n_indices))
    }
    agecomp$indices[i]="multinomial" #switch the missing index comp to multinomial because it
    #has no parameters and this was the easiest way to turn off estimation of parameters for alternate comp options
    input.temp=set_age_comp(input.temp,age_comp=agecomp)
    
    
    fix_sel=input.temp$options$selectivity$fix_pars #fix selectivity parameters based on option selected.
    if(input.temp$data$selblock_models[i+n_fleets]==1){
      fix_sel[[n_fleets+i]]=1:n_ages #fix age-specific parms for given index
    } 
    if(input.temp$data$selblock_models[i+n_fleets]==2) {
      fix_sel[[n_fleets+i]]=1:2 #fix logistic
    } 
    if(input.temp$data$selblock_models[i+n_fleets]==3) {
      fix_sel[[n_fleets+i]]=1:4 #fix dbl logistic
    }
    
    selnums=input.temp$data$selblock_models #replicate selectivity specifications for use in set_selectivity
    selmodels=ifelse(selnums==1,"age-specific",ifelse(selnums==2,"logistic","double-logistic"))
    sel=list(model=selmodels,fix_pars=fix_sel)
    input.temp=set_selectivity(input.temp,sel)
    input.temp$par$logit_selpars[i+n_fleets,]=Inf #set all pars to max for missing index just as a way to check function
    
    #turn on or off estimation of log sd of observations scalar for indices
    if(!is.na(input.temp$map$log_index_sig_scale[i])){
    input.temp$map$log_index_sig_scale[i]=NA
    input.temp$map$log_index_sig_scale=factor(input.temp$map$log_index_sig_scale)
    }
    
    mod.temp=fit_wham(input.temp,do.osa=F,do.retro = F,do.sdrep = dosdrep,do.check=FALSE)
    
    modname=input.temp$index_names[i]
    mods[[modname]]=mod.temp
  } #end i loop
  return(mods)
} #close LOO function