# Generic R code to pull all figures from a folder into a powerpoint
#' @description By default this code generates a PowerPoint presentation including all .png files in the specified plotStorage directory. Slides are labeled numerically
#' 
#' @param plotStorage A file path to the folder containing all figures to include in a powerpoint, this folder can contain sub-directories whose images will also be included
#' @param figureExtension A string specifying the file extension for the type of figures to include, default = ".png"
#' @param slideTitle A title for the PowerPoint slides, default = "Supplemental figures"
#' @param format A string indicating the type of format to use ("pptx" or "revealjs"), default = "pptx"
#' @param outdir A file path for the directory where the final presentation file should be saved, no default.
#' @param filename A string for the final presentation file name, no default.


makePresentation <- function(plotStorage = NULL,
         figureExtension = ".png",
         slideTitle = "Supplemental figures",
         format = "pptx",
         outdir = NULL,
         filename = NULL){

# List all files in here::here(), can replace this with whatever folder you have figures stored in
plotList <- list.files(path = plotStorage, pattern = figureExtension, recursive = TRUE)
nfigure = length(plotList)

slideOut = paste0(outdir, "/", filename, ".qmd")

# Title
write("---", slideOut, append = FALSE)
write(paste0("title: '", slideTitle, "'"), slideOut, append = TRUE)

# Set slide format
if(format == "revealjs"){
  write(paste0("format: "), slideOut, append = TRUE)
  write(paste0("  ", format,":"), slideOut, append = TRUE)
  write("    width: '150%'", slideOut, append = TRUE) # Increases size of plots, 
} else if(format == "pptx"){
  write(paste0("format: ", format), slideOut, append = TRUE)
}

for(ifigure in 1:nfigure){
  write(paste0("## Figure", ifigure), slideOut, append = TRUE)
  write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
  write(paste0("![](", paste(plotList[ifigure]),")"), slideOut, append = TRUE)
  write("", slideOut, append = TRUE)
}

quarto:::quarto_render(slideOut)

}
