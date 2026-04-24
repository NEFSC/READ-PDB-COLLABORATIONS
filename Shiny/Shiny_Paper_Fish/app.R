# This tool allows user to explore how the amount or proportion of projected catch or SSB 
# depends on paper fish (fish in projected cohorts). It is uses Charles Perretti's projected 
# cohorts plotting function to highlight the paper fish.

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)

#--------------------------------------------------------------------------------
# Multiple plot function from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
#
# ggplot objects can be passed in ..., or to plotList (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiPlot <- function(..., plotList=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotList
  plots <- c(list(...), plotList)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#--------------------------------------------------------------------------------
############################################################################################
# defaults and constants

defaults <- list(
  mat50 = 3,
  s50 = 4,
  k = 0.3,
  rrat = 1,
  fe = 0.3,
  fp = 0.3,
  np = 10,
  met = "SSB",
  typ = "Proportion"
)

M <- 0.2
nages <- 20
firstyearlabel <- 2025 # for the equilibrium population before projections

#--------------------------------------------------------------------------------
# function to make maturity, selectivity, relative growth plot
make_age_plot <- function(mat50, s50, k){
  maturity <- 1 / (1 + exp(-((1:nages) - mat50)/0.5))
  selx <- 1 / (1 + exp(-((1:nages) - s50)/0.75))
  WAA <- 0.000001 * (100*(1 - exp(-k * seq(1,nages)))) ^ 3
  relWAA <- WAA / max(WAA)
  
  a50s <- tibble(Age = rep(1:nages, 3),
                 Source = rep(c("Maturity", "Selectivity", "Rel. Growth"), each=nages),
                 Value = c(maturity, selx, relWAA))
  
  age_plot <- ggplot(a50s, aes(x=Age, y=Value, color=Source)) +
    geom_point() +
    geom_line() +
    ylim(c(0,NA)) +
    theme_bw()
  
  return(age_plot)
}

# function to conduct the projections
run_proj <- function(mat50, s50, k, rrat, fe, fp, np, met, typ){
  # holds the results
  df <- tibble(Metric = character(),
               Type = character(),
               Year = numeric(),
               Age = numeric(),
               Value = double(),
               PaperProp = double())
  
  # some initialization stuff
  yearlabel <- firstyearlabel + 0:np
  nprojyears <- np + 1 # plus one to account for equilibrium population in first year
  
  maturity <- 1 / (1 + exp(-((1:nages) - mat50)/0.5))
  selx <- 1 / (1 + exp(-((1:nages) - s50)/0.75))
  WAA <- 0.000001 * (100*(1 - exp(-k * seq(1,nages)))) ^ 3
  
  NAA <- matrix(NA, nrow=nprojyears, ncol=nages)
  FAA <- matrix(NA, nrow=nprojyears, ncol=nages)
  ZAA <- matrix(NA, nrow=nprojyears, ncol=nages)
  
  # Equilibrium starting population calculations
  FAA[1,] <- fe * selx
  ZAA[1,] <- FAA[1,] + M
  NAA[1,1] <- 1 # assume recruitment of 1 in equilibrium (projection R is then relative to this value)
  for (i in 2:nages){
    NAA[1,i] <- NAA[1,i-1] * exp(-ZAA[1,i-1])
  }
  NAA[1,nages] <- NAA[1,nages] / (1 - exp(-ZAA[1,nages]))
  
  # projection years
  for(iyear in 2:nprojyears){
    FAA[iyear,] <- fp * selx
    ZAA[iyear,] <- FAA[iyear,] + M
    
    NAA[iyear,1] <- rrat # recruitment for projection years
    for (i in 2:nages){
      NAA[iyear,i] <- NAA[iyear-1,i-1] * exp(-ZAA[iyear-1,i-1])
    }
    NAA[iyear,nages] <- NAA[iyear,nages] + NAA[iyear-1,nages] * exp(-ZAA[iyear-1,nages])
  }
  
  # save results
  for(iyear in 1:nprojyears){
    if(met == "SSB"){
      valaa <- NAA[iyear,] * maturity * WAA
    }else{
      if(FAA[iyear,nages] == 0){ 
        valaa <- 0
      }else{
        valaa <- NAA[iyear,] * WAA * FAA[iyear,] * (1-exp(-ZAA[iyear,])) / ZAA[iyear,]
      }
    }
    if(typ == "Proportion"){
      if(sum(valaa) > 0){
        valaa <- valaa / sum(valaa)
      }
    }
    paper_idx <- if(iyear == 1) NULL else 1:(iyear-1)
    Paper_Prop <- if(is.null(paper_idx)) 0 else sum(valaa[paper_idx])
    
    thisdf <- tibble(Metric = met,
                     Type = typ,
                     Year = yearlabel[iyear],
                     Age = 1:nages,
                     Value = valaa,
                     Paper_Prop = Paper_Prop)
    df <- rbind(df, thisdf)
  }
  return(df)
}

make_proj_plot <- function(df, met, typ){
  # define cohorts
  df <- df |>
    mutate(Cohort = Year - Age,
           Cohort = ifelse(Age == nages, " Plus group", Cohort),
           Cohort = ifelse((Year - Age) >= firstyearlabel, "Projected recruits", Cohort))
  
  # Adjust colors
  cohort_levels <- unique(df$Cohort)
  
  manual_level <- "Projected recruits"
  
  other_levels <- setdiff(cohort_levels, manual_level)
  
  other_colors <- viridis::viridis(length(other_levels))
  names(other_colors) <- other_levels
  
  all_colors <- c(other_colors, "Projected recruits" = "gray") 
  
  proj_plot <- ggplot(df, aes(x = Year, y=Value)) +
    geom_bar(aes(fill = Cohort), position="stack", stat="identity") +
    geom_vline(xintercept =  firstyearlabel + 0.5, color = "black", linewidth = 1) +
    scale_fill_manual(values = all_colors) +
    ylab(typ) +
    ggtitle(paste(typ, "of", met, "by cohort")) +
    labs(caption = "The vertical black line separates the equilibrium starting population from the projection period.") +
    theme_bw() 
  
  return(proj_plot)
}
#--------------------------------------------------------------------------------
# Define UI for application 
ui <- fluidPage(
  
   # Application title
   titlePanel("Shiny Paper Fish"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
         div(style="display: flex; gap: 5px;",
             actionButton("resetAll", "Reset Defaults", class = "btn-danger", style="width: 50%"),
             actionButton("setBaseline", "Set Baseline", class = "btn-primary", style="width: 50%")
         ),
         sliderInput("Rratio",
                     label = "Projection R",
                     min = 0.05,
                     max = 5.0,
                     step = 0.05,
                     value = defaults$rrat),
         
         sliderInput("fequil",
                     label = "F in 2025",
                     min = 0,
                     max = 1.0,
                     value = defaults$fe),  
         
         sliderInput("fproj",
                     label = "Projection F",
                     min = 0,
                     max = 1.0,
                     value = defaults$fp),
         
         sliderInput("numPrjyrs",
                     label = "# Projection Years",
                     min = 3,
                     max = 24,
                     step = 1,
                     value = defaults$np),
         
         sliderInput("mature50",
                     label = "Age at 50% maturity",
                     min = 0,
                     max = 15,
                     value = defaults$mat50),
         
         sliderInput("selx50",
                     label = "Age at 50% selectivity",
                     min = 0,
                     max = 15,
                     value = defaults$s50),
         
         sliderInput("vonBk",
                     label = "Growth rate",
                     min = 0.01,
                     max = 0.99,
                     value = defaults$k),
         
         
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       wellPanel(
         fluidRow(
           column(6, 
                  selectInput("metric",
                              label = "Select Metric",
                              choices = c("Catch", "SSB"),
                              selected = defaults$met)),
           column(6,
                  selectInput("type",
                              label = "Select View Type",
                              choices = c("Amount", "Proportion"),
                              selected = defaults$typ)),        
                  
         )
       ),
       plotOutput("myPlots")
     )
   )
)

#--------------------------------------------------------------------------------
# Define server logic 
server <- function(input, output, session) {

  # Reactive values to store snapshots for both metrics and types
  baseline_ssb_amount <- reactiveVal(NULL)
  baseline_ssb_prop <- reactiveVal(NULL)
  baseline_catch_amount <- reactiveVal(NULL)
  baseline_catch_prop <- reactiveVal(NULL)
  
  observeEvent(input$resetAll, {
    updateSliderInput(session, "mature50", value = defaults$mat50)
    updateSliderInput(session, "selx50", value = defaults$s50)
    updateSliderInput(session, "vonBk", value = defaults$k)
    updateSliderInput(session, "Rratio", value = defaults$rrat)
    updateSliderInput(session, "fequil", value = defaults$fe)
    updateSliderInput(session, "fproj", value = defaults$fp)
    updateSliderInput(session, "numPrjyrs", value = defaults$np)
    updateSelectInput(session, "metric", selected = defaults$met)
    updateSelectInput(session, "type", selected = defaults$typ)
  })

  # Snapshot both metrics and types when button is clicked
  observeEvent(input$setBaseline, {
    baseline_ssb_amount(run_proj(input$mature50, input$selx50, input$vonBk, input$Rratio, input$fequil, input$fproj, input$numPrjyrs, "SSB", "Amount"))
    baseline_ssb_prop(run_proj(input$mature50, input$selx50, input$vonBk, input$Rratio, input$fequil, input$fproj, input$numPrjyrs, "SSB", "Proportion"))
    baseline_catch_amount(run_proj(input$mature50, input$selx50, input$vonBk, input$Rratio, input$fequil, input$fproj, input$numPrjyrs, "Catch", "Amount"))
    baseline_catch_prop(run_proj(input$mature50, input$selx50, input$vonBk, input$Rratio, input$fequil, input$fproj, input$numPrjyrs, "Catch", "Proportion"))
  })
  
  output$myPlots <- renderPlot({

     age_plot <- make_age_plot(input$mature50, input$selx50, input$vonBk)
     
     df <- run_proj(input$mature50, input$selx50, input$vonBk, input$Rratio, input$fequil, input$fproj, input$numPrjyrs, input$metric, input$type)

     proj_plot <- make_proj_plot(df, input$metric, input$type)

     # Pick the correct baseline based on metric
     active_baseline <- if(input$metric == "SSB" & input$type == "Amount"){ 
       baseline_ssb_amount()
     }else if(input$metric == "SSB" & input$type == "Proportion"){
       baseline_ssb_prop()
     }else if(input$metric == "Catch" & input$type == "Amount"){
       baseline_catch_amount()
     }else if(input$metric == "Catch" & input$type == "Proportion"){
       baseline_catch_prop()
     }
     
     if(!is.null(active_baseline)) {
       linedf <- active_baseline |>
         filter(Age == 1)
       proj_plot <- proj_plot + 
         geom_line(data = linedf, aes(x = Year, y = Paper_Prop, linetype = "Baseline Snapshot"), linewidth = 1) +
         scale_linetype_manual(name = "", values = c("Baseline Snapshot" = 2))
     }

     # make the final combined plot
     multiPlot(proj_plot, age_plot, cols=1)

  }, height = 675, width = 575)
}

# Run the application 
shinyApp(ui = ui, server = server)

