# Simple simulation function for WHAM model

sim_fn <- function(om, self.fit = FALSE){
  input <- om$input
  input$data = om$simulate(complete=TRUE)
  if(self.fit) {
    fit <- fit_wham(input, do.osa = F, do.retro = F, MakeADFun.silent = T)
    return(fit)
  } else return(input) 
}