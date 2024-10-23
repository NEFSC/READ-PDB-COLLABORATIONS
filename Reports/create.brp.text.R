#' @title Function to create the brp text
#' 
#' @param brp.name A string for the type of BRP, options include: 'SSBproxy', 'MSYproxy', and 'Fproxy', no default.
#' @param round.digits The number of digits to round BRPs, default = 3. 
#' @param brp.table A table containing the following columns:
#' \itemize{
#'   \item{BRP - Name of the BRP}
#'   \item{est - BRP estimates}
#'   \item{lo_95 - Lower 95\% confidence bound}
#'   \item{hi_95 - Upper 95\% confidence bound}
#'   \item{source - Model source, either "prior" for prior assessment or "MT" for current assessment}
#' }
#' 
#' @return A string for the BRP text including 95% CI.

create.brp.text <- function(brp.name = NULL, 
                            round.digits = 3, 
                            brp.table = NULL){
  # Pull ref pt from model_prior
  old.brp <- brp.table %>% filter(source == "prior", BRP == brp.name) %>% select(est) %>% round(., round.digits) %>% unlist()
  
  # if(!brp.name == 'Fproxy'){ # Text for SSB and MSY proxies
    brp.ests <- brp.table %>% filter(source == "MT", BRP == brp.name) # Same formatting for all reference point text
    new.brp <- round(brp.ests$est, round.digits)
    lo.brp <- round(brp.ests$lo_95, round.digits)
    hi.brp <- round(brp.ests$hi_95, round.digits)
    output.text <- c(old.brp, paste(new.brp, " (", lo.brp," - ", hi.brp,")", sep=""))
  # } else {
  #   new.brp <- brp.table %>% filter(source == "MT", BRP == brp.name) %>% select(est) %>% round(., round.digits)
  #   output.text <- as.character(c(old.brp, new.brp))
  # }
  
  # Return
  return(output.text)
}
