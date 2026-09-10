# function to evaluate and summarize bottom trawl survey tows

#' Evaluate and Summarize Bottom Trawl Survey Tows
#'
#' This function processes bottom trawl survey (BTS) data to calculate stratum-specific
#' means, proportions of stock area sampled, and total tow counts. It generates a comprehensive 
#' suite of diagnostic plots (as PNGs or other formats) and CSV summaries to evaluate
#' survey design metrics across seasons (SPRING and FALL) and years.
#' Code developed by Liz Brooks (May 2026; upload to repo August 2026)
#' Contributions by Chris Legault (August 2026 (facet_wrap_paginate! and helpful checking for tripping hazards))
#'
#' @param filepath.sv.area Character. Filepath to the CSV file containing survey strata names and areas. Defaults to `NULL`.
#' @param filepath.spp.tow.map Character. Filepath to the CSV file containing individual tow data, including catches and coordinates. Defaults to `NULL`.
#' @param filepath.mean.index Character. Filepath to the CSV file containing ADIOS stratified mean index estimates. Defaults to `NULL`.
#' @param od Character. The output directory path where generated plots and CSV summaries will be saved. Defaults to `output.dir`.
#' @param sv.unit Character. The survey unit to evaluate. Options are `'N'` for abundance (numbers/tow) or `'Kg'` for biomass (kg/tow). Defaults to `'N'`.
#' @param tsa Numeric. Tow swept area in square nautical miles (nm^2). Currently not utilized in calculations. Defaults to `0.0112`.
#' @param plot.h Numeric. Height of the output plots in inches. Defaults to `6.5`.
#' @param plot.w Numeric. Width of the output plots in inches. Defaults to `8`.
#' @param plot.nrow.pp Numeric. Number of rows per page for multi-page faceted plots. Defaults to `4`.
#' @param plot.ncol.pp Numeric. Number of columns per page for multi-page faceted plots. Defaults to `5`.
#' @param yaxis.max Numeric. Maximum value for the y-axis on stratum mean plots. If `NULL`, the maximum observed value is used. Defaults to `NULL`.
#' @param plot.f Character. The file format/device extension for the exported plots (e.g., `"png"`, `"pdf"`). Defaults to `"png"`.
#' @param fall.color Character. Hex color code with transparency for Fall survey data visualizations. Defaults to `'#5588ddaa'`.
#' @param spring.color Character. Hex color code with transparency for Spring survey data visualizations. Defaults to `'#44cc77cc'`.
#' @param summary.color Character. Hex color code with transparency for summarized visual components. Defaults to `'#ffaa77bb'`.
#' @param strata.color.set Character vector. A recycled vector of hex colors used to uniquely identify individual strata in plots.
#'
#' @details 
#' The function filters input data for `PURPOSE_CODE == 10` and limits seasons strictly to `"SPRING"` and `"FALL"`. 
#' It automatically checks for the target output directory and creates it recursively if it does not exist.
#' If sampling is determined to be incomplete in a given year-season layer (sampled area < 100%), it generates 
#' an extra tracking plot mapping the divergence between the total area mean and the sampled area mean.
#'
#' @return The function does not explicitly return an object in the R environment. Instead, it writes 
#' diagnostic data tables (`.csv`) and visual trend plots (`.png` or specified format) directly to the target `od` path.
#'
#' @import tibble
#' @import ggplot2
#' @import tidyr
#' @import readr
#' @import dplyr
#' @import here
#' @import ggforce
#' @importFrom stats var
#' @importFrom utils read.csv write.csv
#' 
#' @export
survey_evaluation_plots <- function(filepath.sv.area=NULL,
                                    filepath.spp.tow.map=NULL,
                                    filepath.mean.index=NULL,
                                    od=output.dir,
                                    sv.unit = 'N',
                                    tsa = 0.0112,  # tow swept are (nm^2), not used
                                    plot.h=6.5,
                                    plot.w=8,
                                    plot.nrow.pp=4,
                                    plot.ncol.pp=5,
                                    yaxis.max=NULL,
                                    plot.f="png",
                                    fall.color= '#5588ddaa'    , 
                                    spring.color='#44cc77cc'   , 
                                    summary.color= '#ffaa77bb' ,
                                    strata.color.set = rep(c( "#4F2d03aa", "#FF9839", "#F01875", "#2C089799", "#000C7Dbc" , #inferno color map
                                                              "#98FF15", "#30CD13", "#1F890B", "#0F4B05", "#013f16aa" , #kgy color map
                                                              "#Fb906D", "#d6A85Daa", "#86B877", "#57BBB3","#37B7EC" ,  #i1 color map
                                                              "#c9c9c9", "#A4C503", "#2F896B", "#082FB5cc", "#11111177",  #l16 color map
                                                              "#df99aa" ,"#FE83FD" ,"#B91CFE" ,"#150FD3","#00024Bcc",   #l7 color map
                                                              "#D70500", "#FC6C9B", "#FfCD1C", "#65AF1E", "#085CF8"  #r3 color map
                                    ), 4)
) 
{   # begin function survey_evaluation_plots  ====
  
  
  if(!dir.exists(od))  dir.create(od, recursive=TRUE)
  
  
  # customize your own colors, as below, but make sure you have the same number of colors as unique number of strata
  # i used this for gb.haddock (only 15 strata, handpicked to align with spatial layout and depth)
  #            #stratum:     13         14         15         16          17        18         19          20
  # gb.strata.color.set <- c("#55DDDD", "#5599DD", "#1111DD", "#FF99AA" , "#FF1188", "#DD0055", "#CCCCEE", "#CCaaCC",
  #                         # 21         22          23         24        25          29         30
  #                         "#BB77DD", "#AA00BB", "#55BB00", "#117733", "#99DD00", "#551199", "#000088")
  
  
  #  =======================================
  
  # used to generate plot test.colored.bars.png to see colors side by side
  # test <- as_tibble(cbind(count=seq(1,30), n=rep(3,30), mycolor=strata.color.set[1:30]) )
  # test.colored.bars <- ggplot(test, aes(x=count, y=n, fill=count)) +
  #   geom_col(col='#F9F9F9') +
  #   scale_fill_manual(values=test$mycolor) +
  #   theme_light() +
  #   theme(legend.position = "none") +
  #   scale_x_discrete( labels=as.character(c(seq(1,30,by=1)))
  #                   ) +
  #   scale_y_discrete(labels="") +
  #   labs(subtitle = "Default colors (30) for individual strata colors, recycled if num(strata) exceeds 30") +
  #   labs(x="Color") +
  #   labs(y="")
  # ggsave(test.colored.bars, filename=file.path(od, 'test.colored.bars.png'), height=plot.h, width=plot.w, device=plot.f)
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # read in survey area
  sv.area <- as_tibble(read.csv(filepath.sv.area) ) %>%
    select(-STRATUM_NAME) #%>%
    # mutate(tsa=tsa,
    #        num.str.tows = STRATUM_AREA/tsa)
  
  
  # read in species file of tow by tow data
  spp.tow<-  as_tibble(read.csv(filepath.spp.tow.map, header=T)) |>
    filter(PURPOSE_CODE == 10) |>
    filter(SEASON %in% c("SPRING", "FALL"))

  # read in ADIOS mean index estimates
  spp.index <-  as_tibble(read.csv(filepath.mean.index, header=T)) %>%
    filter(PURPOSE_CODE == 10) |>
    filter(SEASON %in% c("SPRING", "FALL")) |>
    mutate(UNIT=case_when(
      INDEX_TYPE=="Abundance (numbers/tow)" ~ "CATCH_NO_CAL",
      INDEX_TYPE=="Biomass (kg/tow)" ~ "CATCH_WT_CAL"
    ) ) %>%
    filter(UNIT==ifelse(sv.unit=='N', "CATCH_NO_CAL", "CATCH_WT_CAL"))
  
  spp.name <- gsub(as.character(unique(spp.index$COMMON_NAME)), pattern=",", replacement=".")
  spp.stock <- unique(spp.index$STOCK_ABBREV)
  spp.itis <- unique(spp.index$SPECIES_ITIS)
  fall.strata <- unique(spp.tow$STRATUM[spp.tow$SEASON=="FALL"])
  spring.strata <- unique(spp.tow$STRATUM[spp.tow$SEASON=="SPRING"])
  
  # calc total area by season and year ====
  spp.str.area <- spp.tow%>%
    select(YEAR, SEASON, STRATUM) %>%
    distinct() %>%
    arrange(YEAR, SEASON, STRATUM) %>%
    left_join(sv.area) %>%
    group_by(YEAR, SEASON)  %>%
    dplyr::summarise(total.area.sampled.yr = sum(STRATUM_AREA))
  
  
  spp.strata <-as_tibble(cbind(STRATUM=c(fall.strata, spring.strata), 
                               SEASON=c(rep('FALL', length(fall.strata)), rep('SPRING', length(spring.strata) ) ) ) 
  )  %>%
    mutate(STRATUM=as.integer(STRATUM))
  
  spp.total.area <- as_tibble(cbind(FALL=sum(sv.area$STRATUM_AREA[sv.area$STRATUM %in% fall.strata]),
                                    SPRING=sum(sv.area$STRATUM_AREA[sv.area$STRATUM %in% spring.strata]) )
  ) %>%
    pivot_longer(cols=c(1,2), names_to = 'SEASON', values_to='Total')

  
  # calculate the proportion of total stock area that each stratum represents  ====
  spp.sv.area.prop <- spp.strata %>%
    left_join(sv.area) %>%
    left_join(spp.total.area) %>%
    mutate(Stratum.prop.area = STRATUM_AREA/Total) 
  
  write.csv(spp.sv.area.prop , file=file.path(od, 'spp.sv.area.prop.csv'), row.names = FALSE)  #plot this
  
  
  stratum.area.proportion.plot <- ggplot(spp.sv.area.prop, aes(x=as.character(STRATUM), y=Stratum.prop.area, fill=SEASON) ) +
    facet_wrap(~SEASON, nrow=2) +
    geom_col(col='black' ) +
    labs(x='Stratum') +
    labs(y='Proportion of Stock Area') +
    scale_fill_manual(values=c(fall.color, spring.color)) +
    theme_light()  +
    theme(panel.background = element_rect(fill='white')) +
    theme(strip.background =element_rect(fill="white", color="grey65"))+
    theme(strip.text = element_text(colour = 'black', size=10)) +
    theme(axis.text.x = element_text(size = 9, angle=90, vjust=0.5))   + 
    theme(axis.text.y = element_text(size = 10))   +
    theme(axis.title.y = element_text(size = 11))   +
    theme(legend.position="none")
  ggsave(stratum.area.proportion.plot, filename=file.path(od,paste( "stratum.area.proportion.plot", plot.f, sep=".")), 
         device=plot.f,  height=plot.h, width=plot.w) 
  
  
  
  
  surv.area.yr <- spp.tow%>%
    select(YEAR, SEASON, STRATUM) %>%
    arrange(YEAR, SEASON, STRATUM) %>%
    distinct() %>%
    left_join(spp.str.area) %>%
    left_join(sv.area) %>%
    left_join(spp.total.area  ) %>%
    mutate(spp.total.area=Total) %>%
    select(-Total)  %>%
    mutate(prop.spp.total.area = STRATUM_AREA/spp.total.area) %>%
    mutate(prop.spp.sampled.area = STRATUM_AREA/total.area.sampled.yr)  # this is probably what is used for SRS when there are missing strata
  
  
  
  # calculate how much of the total area was sampled each year/season ====
  surv.area.yr.season.sampled <- surv.area.yr %>%
    group_by(YEAR, SEASON, STRATUM) %>%
    dplyr::summarise(sum.sampled.area.stratum = sum(STRATUM_AREA, na.rm=TRUE),
                     prop.sampled.area.stratum = sum(STRATUM_AREA, na.rm=TRUE)/spp.total.area) %>%
    ungroup()  %>%
    group_by(YEAR, SEASON)  %>%
    dplyr::summarise(sum.sampled.area = sum(sum.sampled.area.stratum , na.rm=TRUE),
                     prop.sampled.area = sum(prop.sampled.area.stratum)  )
  
  stock.area.sampled.plot <- ggplot(surv.area.yr.season.sampled, aes(x=as.character(YEAR), y=prop.sampled.area, fill=SEASON ) ) +
    facet_wrap(~SEASON, nrow=2)  +
    geom_bar(position="dodge", stat="identity", col='white' ) +
    geom_hline(yintercept=c(0.95, 1.0), col='red', linetype=c('dashed', 'solid', 'dashed', 'solid' ))  +
    scale_fill_manual(values=c(fall.color, spring.color)) +
    labs(x='Year') +
    labs(y='Proportion of Stock Area sampled') +
    theme_light()  +
    theme(panel.background = element_rect(fill='white')) +
    theme(strip.background =element_rect(fill="white", color="grey65"))+
    theme(strip.text = element_text(colour = 'black', size=10)) +
    theme(axis.text.x = element_text(size = 9, angle=90, vjust=0.5))   + 
    theme(axis.text.y = element_text(size = 10))   +
    theme(axis.title.y = element_text(size = 11))   +
    theme(legend.position="none") +
    labs(subtitle='Dashed line is at 95%')
  ggsave(stock.area.sampled.plot , filename=file.path(od,paste( "stock.area.sampled.plot", plot.f, sep=".")), 
         device=plot.f,  height=plot.h, width=plot.w) 
  
  
  
  # calc stratum  means ====
  str.means <- spp.tow  %>%
    replace(is.na(.), 0) %>%
    mutate(Unit.to.use = sv.unit) %>%
    mutate(Value = ifelse(Unit.to.use=='N', CATCH_NO_CAL, CATCH_WT_CAL)) %>%
    select(YEAR, SEASON, STRATUM, TOW, Value) %>%
    group_by(YEAR, SEASON, STRATUM) %>%
    dplyr::summarise(mean.str = mean(Value, na.rm=TRUE ), ntows.str = n(),
                     std.str = sqrt(var(Value) ), 
                     CV.str = ifelse( (is.finite(std.str) & mean.str>0), std.str/mean.str, NA) ) %>%
    left_join(surv.area.yr) %>%
    mutate(prop.mean.sampled = mean.str*prop.spp.sampled.area,  # prop.spp.sampled.area is proportion of just sampled areas (sums to 1)
           prop.mean = mean.str*prop.spp.total.area  # prop.spp.total.area is proportion of total area (only sums to 1 in years where all strata are sampled)
             )  %>%  
    relocate(prop.mean , .after=mean.str) %>%
    relocate(prop.mean.sampled, .after=prop.mean)
  
  write.csv(str.means, file=file.path(od, paste('stratum.mean.contribution', sv.unit, 'csv', sep='.') )  )
  

  # NOTE: when sampling is incomplete in a year-season, 
  #  the stratified mean in stockeff only calculates based on proportion of area sampled

   
  srs.mean <- str.means %>%
    group_by(YEAR, SEASON) %>%
    dplyr::summarise(srs.mean.total.area = sum(prop.mean), 
                     srs.mean.sampled.area = sum(prop.mean.sampled) ) %>%
    ungroup()  %>%
    pivot_longer(cols=starts_with("srs.mean"), names_to = "Area_Used", values_to="Mean", names_prefix = "srs.mean.")
  
  
  # plot mean of each stratum over time   ====
  years.ts <- unique(str.means$YEAR)
  nstrata <- length(unique(str.means$STRATUM))
  if(is.null(yaxis.max))  yaxis.max=max(str.means$mean.str)
  
  stratum.means.plot <- ggplot(str.means, aes(x=YEAR, y=mean.str, col=SEASON, shape=SEASON , fill=SEASON)) +  
    facet_wrap_paginate(~STRATUM, nrow = ifelse(nstrata> 20  ,plot.nrow.pp, ceiling(max(length(fall.strata), length(spring.strata))/5 ) ),
                                                ncol = ifelse(nstrata>20, plot.ncol.pp, 5)   )  +
    geom_point()  +
    geom_line() +
    scale_color_manual(values=c(fall.color, spring.color) ) +
    scale_x_continuous(breaks=seq(years.ts[1], last(years.ts), by=5) )  +
    coord_cartesian(y=c(0,yaxis.max)) +
    labs(y=paste0('Stratum Mean (', sv.unit, '/tow)')  )+
    theme_light()  +
    theme(panel.background = element_rect(fill='white')) +
    theme(strip.background =element_rect(fill="white", color="grey65"))+
    theme(strip.text = element_text(colour = 'black', size=9)) +
    theme(axis.text.x = element_text(size = 9, angle=90, vjust=0.5))   + 
    theme(axis.text.y = element_text(size = 9))   +
    theme(axis.title.y = element_text(size = 11))   +
    theme(legend.position="bottom") 
  
  for (i in 1:n_pages(stratum.means.plot)){
    stratum.means.plot.current <- stratum.means.plot  +
      facet_wrap_paginate(~STRATUM, nrow = ifelse(nstrata> 20  ,plot.nrow.pp, ceiling(max(length(fall.strata), length(spring.strata))/5 ) ),
                          ncol = ifelse(nstrata>20, plot.ncol.pp, 5) , page = i  )
    print(stratum.means.plot.current)
    ggsave(stratum.means.plot.current,
           filename=file.path(od,paste( "stratum.means.plot.page", i, sv.unit, plot.f, sep=".")),
           device=plot.f,  height=plot.h, width=plot.w)
  }
  
  # ggsave(stratum.means.plot , filename=file.path(od,paste( "stratum.means.plot",sv.unit, plot.f, sep=".")), 
  #        device=plot.f,  height=plot.h, width=plot.w) 
  

  
  
  incomplete.sampling <- surv.area.yr.season.sampled[which(surv.area.yr.season.sampled$prop.sampled.area<1),]

  if (nrow(incomplete.sampling)>0) {
    
    srs.mean.incomplete <- incomplete.sampling %>%
      left_join(srs.mean)
    yrs.incomplete <- unique(srs.mean.incomplete$YEAR)
    xaxis.tick.width <- ifelse(length(yrs.incomplete) <10, 1, 3)
    
    mean.incomplete.diff.plot <- ggplot(srs.mean.incomplete, aes(x=YEAR, y=Mean,  col=Area_Used, shape=Area_Used)) +
      facet_wrap(~SEASON, nrow=2)  +
      geom_point(  cex=2.5) +
      labs(y=paste0('Mean (', sv.unit, '/tow)')  )+
      scale_color_manual(values=c('#ff442299', '#0055ff88'))  +
      scale_shape_manual(values=c(8,17))  +
      scale_x_continuous(breaks=seq(yrs.incomplete[1], last(yrs.incomplete), by=xaxis.tick.width))  +
      theme_light()  +
      theme(panel.background = element_rect(fill='white')) +
      theme(strip.background =element_rect(fill="white", color="grey65"))+
      theme(strip.text = element_text(colour = 'black', size=10)) +
      theme(axis.text.x = element_text(size = 9, angle=90, vjust=0.5))   + 
      theme(axis.text.y = element_text(size = 10))   +
      theme(axis.title.x = element_blank())   + 
      theme(axis.title.y = element_text(size = 11))  +
      coord_cartesian(ylim=c(0,yaxis.max)) +
      labs(subtitle="Difference in Mean due to incomplete sampling")
    ggsave(mean.incomplete.diff.plot , filename=file.path(od,paste( "mean.incomplete.diff.plot",sv.unit, plot.f, sep=".")), 
           device=plot.f,  height=plot.h, width=plot.w) 
    
  } # end plot for incomplete sampling srs.mean comparison
  
  
  # plots below for stratum contribution could be plotted relative to prop.mean (denominator is total stock area every year) 
  # or relative to prop.mean.sampled (denominator is total sampled area every year)
  # these will be similar when only a small stratum is missed but can diverge more when larger areas are missed
  
  str.mean.by.contr <- ggplot(str.means, 
                              aes(x=YEAR, y=prop.mean, fill=as.factor(STRATUM) )) +
    facet_wrap(~SEASON, nrow=2) +
    geom_col( color = "black") + 
    scale_fill_discrete(type = strata.color.set)+  
    scale_x_continuous(breaks=seq(years.ts[1], last(years.ts), by=2) ) +
    theme(axis.text.x = element_text(angle = 90))+
    labs(fill = "Stratum") +
    labs(y=paste0('Stratum Contribution to Mean (', sv.unit, '/tow)') )+
    theme_light()  +
    theme(panel.background = element_rect(fill='white')) +
    theme(strip.background =element_rect(fill="white", color="grey65"))+
    theme(strip.text = element_text(colour = 'black', size=10)) +
    theme(axis.text.x = element_text(size = 9, angle=90, vjust=0.5))   + 
    theme(axis.text.y = element_text(size = 10))   +
    theme(axis.title.x = element_blank())   + 
    theme(axis.title.y = element_text(size = 11))
  ggsave(str.mean.by.contr, filename=file.path(od,paste( "str.mean.by.contr", sv.unit, plot.f, sep=".")), 
         device=plot.f,  height=plot.h, width=plot.w) 
  
  
  str.mean.FALL.by.contr <- ggplot(str.means[str.means$SEASON=='FALL',], 
                                   aes(x=YEAR, y=prop.mean, fill=as.factor(STRATUM) )) +
    facet_wrap(~SEASON, nrow=2) +
    geom_col( color = "black") + 
    scale_fill_discrete(type = strata.color.set)+  
    scale_x_continuous(breaks=seq(years.ts[1], last(years.ts), by=2) ) +
    theme(axis.text.x = element_text(angle = 90))+
    labs(fill = "Stratum") +
    labs(y=paste0('Stratum Contribution to Mean (', sv.unit, '/tow)') ) +
    theme_light()  +
    theme(panel.background = element_rect(fill='white')) +
    theme(strip.background =element_rect(fill="white", color="grey65"))+
    theme(strip.text = element_text(colour = 'black', size=10)) +
    theme(axis.text.x = element_text(size = 9, angle=90, vjust=0.5))   + 
    theme(axis.text.y = element_text(size = 10))   +
    theme(axis.title.x = element_blank())   + 
    theme(axis.title.y = element_text(size = 11))
  
  ggsave(str.mean.FALL.by.contr, filename=file.path(od,paste( "str.mean.FALL.by.contr", sv.unit, plot.f, sep=".")), 
         device=plot.f,  height=plot.h, width=plot.w) 
  
  str.mean.SPRING.by.contr <- ggplot(str.means[str.means$SEASON=='SPRING',], 
                                     aes(x=YEAR, y=prop.mean, fill=as.factor(STRATUM) )) +
    facet_wrap(~SEASON, nrow=2) +
    geom_col( color = "black") + 
    scale_fill_discrete(type = strata.color.set)+  
    scale_x_continuous(breaks=seq(years.ts[1], last(years.ts), by=2) ) +
    theme(axis.text.x = element_text(angle = 90))+
    labs(fill = "Stratum") +
    labs(y=paste0('Stratum Contribution to Mean (', sv.unit, '/tow)') ) +
    theme_light()  +
    theme(panel.background = element_rect(fill='white')) +
    theme(strip.background =element_rect(fill="white", color="grey65"))+
    theme(strip.text = element_text(colour = 'black', size=10)) +
    theme(axis.text.x = element_text(size = 9, angle=90, vjust=0.5))   + 
    theme(axis.text.y = element_text(size = 10))   +
    theme(axis.title.x = element_blank())   + 
    theme(axis.title.y = element_text(size = 11))
  
  ggsave(str.mean.SPRING.by.contr, filename=file.path(od,paste( "str.mean.SPRING.by.contr", sv.unit, plot.f, sep=".")), 
         device=plot.f,  height=plot.h, width=plot.w) 
  
  
  str.mean.by.contr.prop <- ggplot(str.means, 
                                   aes(x=YEAR, y=prop.mean, fill=as.factor(STRATUM) ))  +
    facet_wrap(~SEASON, nrow=2) +
    geom_col(position="fill", color = "black") +
    scale_fill_discrete(type = strata.color.set)+  
    scale_x_continuous(breaks=seq(years.ts[1], last(years.ts), by=2) ) +
    theme(axis.text.x = element_text(angle = 90))+
    labs(fill = "Stratum") +
    labs(y=paste0('Stratum Contribution to Mean (', sv.unit, '/tow)') ) +
    theme_light()  +
    theme(panel.background = element_rect(fill='white')) +
    theme(strip.background =element_rect(fill="white", color="grey65"))+
    theme(strip.text = element_text(colour = 'black', size=10)) +
    theme(axis.text.x = element_text(size = 9, angle=90, vjust=0.5))   + 
    theme(axis.text.y = element_text(size = 10))   +
    theme(axis.title.x = element_blank())   + 
    theme(axis.title.y = element_text(size = 11))
  ggsave(str.mean.by.contr.prop, filename=file.path(od,paste( "str.mean.by.contr.prop", sv.unit, plot.f, sep=".")), 
         device=plot.f,  height=plot.h, width=plot.w) 
  
  str.mean.FALL.by.contr.prop <- ggplot(str.means[str.means$SEASON=='FALL',], 
                                        aes(x=YEAR, y=prop.mean, fill=as.factor(STRATUM) ))  +
    facet_wrap(~SEASON, nrow=2) +
    geom_col(position="fill", color = "black") +
    scale_fill_discrete(type = strata.color.set)+  
    # scale_x_continuous(name="Year",  breaks=seq(1962,2026, by=2), labels=seq(1962,2026, by=2)) +
    scale_x_continuous(breaks=seq(years.ts[1], last(years.ts), by=2) ) +
    theme(axis.text.x = element_text(angle = 90))+
    labs(fill = "Stratum") +
    labs(y=paste0('Stratum Contribution to Mean (', sv.unit, '/tow)') ) +
    theme_light()  +
    theme(panel.background = element_rect(fill='white')) +
    theme(strip.background =element_rect(fill="white", color="grey65"))+
    theme(strip.text = element_text(colour = 'black', size=10)) +
    theme(axis.text.x = element_text(size = 9, angle=90, vjust=0.5))   + 
    theme(axis.text.y = element_text(size = 10))   +
    theme(axis.title.x = element_blank())   + 
    theme(axis.title.y = element_text(size = 11)) +
    labs(subtitle='Contribution relative to Total stock area')
  ggsave(str.mean.FALL.by.contr.prop, filename=file.path(od,paste( "str.mean.FALL.by.contr.prop", sv.unit, plot.f, sep=".")), 
         device=plot.f,  height=plot.h, width=plot.w) 
  
  
  str.mean.SPRING.by.contr.prop <- ggplot(str.means[str.means$SEASON=='SPRING',], 
                                          aes(x=YEAR, y=prop.mean, fill=as.factor(STRATUM) ))  +
    facet_wrap(~SEASON, nrow=2) +
    geom_col(position="fill", color = "black") +
    scale_fill_discrete(type = strata.color.set)+  
    scale_x_continuous(breaks=seq(years.ts[1], last(years.ts), by=2) ) +
    theme(axis.text.x = element_text(angle = 90))+
    labs(fill = "Stratum") +
    labs(y=paste0('Stratum Contribution to Mean (', sv.unit, '/tow)') ) +
    theme_light()  +
    theme(panel.background = element_rect(fill='white')) +
    theme(strip.background =element_rect(fill="white", color="grey65"))+
    theme(strip.text = element_text(colour = 'black', size=10)) +
    theme(axis.text.x = element_text(size = 9, angle=90, vjust=0.5))   + 
    theme(axis.text.y = element_text(size = 10))   +
    theme(axis.title.x = element_blank())   + 
    theme(axis.title.y = element_text(size = 11)) +
    labs(subtitle='Contribution relative to Total stock area')
  ggsave(str.mean.SPRING.by.contr.prop, filename=file.path(od,paste( "str.mean.SPRING.by.contr.prop", sv.unit, plot.f, sep=".")), 
         device=plot.f,  height=plot.h, width=plot.w) 
  
  
 
  
  bigyears.ts <- unique(str.means$YEAR[str.means$YEAR>2008])  # bigelow years
  
  ntow.fall.by.stratum.plot <- ggplot(str.means[str.means$SEASON=='FALL'  ,], 
                                      aes(x=YEAR, y=ntows.str, color=as.factor(STRATUM))) +
    facet_wrap_paginate(~STRATUM, nrow = ifelse(length(fall.strata)> 20  ,plot.nrow.pp, ceiling(length(fall.strata)/5 ) ),
                        ncol = ifelse(nstrata>20, plot.ncol.pp, 5) , scales = "free_y"  ) +
    
    geom_line() +
    geom_point() +
    scale_color_discrete(type = strata.color.set)+  
    scale_x_continuous(breaks=seq(years.ts[1], last(years.ts), by=2) ) +
    labs(y='Fall BTS Number of tows')+
    theme_light()  +
    theme(panel.background = element_rect(fill='white')) +
    theme(strip.background =element_rect(fill="white", color="grey65"))+
    theme(strip.text = element_text(colour = 'black', size=10)) +
    theme(axis.text.x = element_text(size = 9, angle=90, vjust=0.5))   + 
    theme(axis.text.y = element_text(size = 10))   +
    theme(axis.title.x = element_blank())   + 
    theme(axis.title.y = element_text(size = 11))   +
    theme(legend.position="none") 
  
  for (i in 1:n_pages(ntow.fall.by.stratum.plot)){
    ntow.fall.by.stratum.plot.current <- ntow.fall.by.stratum.plot +
      facet_wrap_paginate(~STRATUM, nrow = ifelse(length(fall.strata)> 20  ,plot.nrow.pp, ceiling(length(fall.strata)/5 ) ),
                          ncol = ifelse(nstrata>20, plot.ncol.pp, 5), page = i, scales = "free_y"   )
    print(ntow.fall.by.stratum.plot.current)
    ggsave(ntow.fall.by.stratum.plot.current,
           filename=file.path(od,paste( "ntow.fall.by.stratum.plot.page", i, plot.f, sep=".")),
           device=plot.f,  height=plot.h, width=plot.w)
  }
  
  
  
  # ggsave(ntow.fall.by.stratum.plot, filename=file.path(od,paste( "ntow.fall.by.stratum.plot", plot.f, sep=".")), 
  #        device=plot.f,  height=plot.h, width=plot.w) 
  
  
  ntow.fall.by.stratum.bigyrs.plot <- ggplot(str.means[str.means$SEASON=='FALL' & str.means$YEAR>=2009,], aes(x=YEAR, y=ntows.str, color=as.factor(STRATUM))) +
    facet_wrap_paginate(~STRATUM, nrow = ifelse(length(fall.strata)> 20  ,plot.nrow.pp, ceiling(length(fall.strata)/5 ) ),
                        ncol = ifelse(nstrata>20, plot.ncol.pp, 5) , scales = "free_y"  ) +
    geom_line() +
    geom_point() +
    scale_color_discrete(type = strata.color.set)+  
    scale_x_continuous(breaks=seq(bigyears.ts[1], last(bigyears.ts), by=2) ) +
    labs(y='Fall BTS Number tows')+
    theme_light()  +
    theme(panel.background = element_rect(fill='white')) +
    theme(strip.background =element_rect(fill="white", color="grey65"))+
    theme(strip.text = element_text(colour = 'black', size=10)) +
    theme(axis.text.x = element_text(size = 9, angle=90, vjust=0.5))   + 
    theme(axis.text.y = element_text(size = 10))   +
    theme(axis.title.x = element_blank())   + 
    theme(axis.title.y = element_text(size = 11))   +
    theme(legend.position="none") 
  
  for (i in 1:n_pages(ntow.fall.by.stratum.bigyrs.plot)){
    ntow.fall.by.stratum.bigyrs.plot.current <- ntow.fall.by.stratum.bigyrs.plot +
      facet_wrap_paginate(~STRATUM, nrow = ifelse(length(fall.strata)> 20  ,plot.nrow.pp, ceiling(length(fall.strata)/5 ) ),
                          ncol = ifelse(nstrata>20, plot.ncol.pp, 5), page = i, scales = "free_y"   )
    print(ntow.fall.by.stratum.bigyrs.plot.current)
    ggsave(ntow.fall.by.stratum.bigyrs.plot.current,
           filename=file.path(od,paste( "ntow.fall.by.stratum.bigyrs.plot.page", i, plot.f, sep=".")),
           device=plot.f,  height=plot.h, width=plot.w)
  }
  
  
  # ggsave(ntow.fall.by.stratum.bigyrs.plot, filename=file.path(od,paste( "ntow.fall.by.stratum.bigyrs.plot", plot.f, sep=".")), 
  #        device=plot.f,  height=plot.h, width=plot.w) 
  
  
  
  
  ntow.spring.by.stratum.plot <- ggplot(str.means[str.means$SEASON=='SPRING',], 
                                        aes(x=YEAR, y=ntows.str, color=as.factor(STRATUM))) +
    facet_wrap_paginate(~STRATUM, nrow = ifelse(length(spring.strata)> 20  ,plot.nrow.pp, ceiling(length(spring.strata)/5 ) ),
                        ncol = ifelse(nstrata>20, plot.ncol.pp, 5) , scales = "free_y"  ) +
    geom_line() +
    geom_point() +
    scale_color_discrete(type = strata.color.set)+  
    scale_x_continuous(breaks=seq(years.ts[1], last(years.ts), by=2) ) +
    labs(y='Spring BTS Number of tows')+
    theme_light()  +
    theme(panel.background = element_rect(fill='white')) +
    theme(strip.background =element_rect(fill="white", color="grey65"))+
    theme(strip.text = element_text(colour = 'black', size=10)) +
    theme(axis.text.x = element_text(size = 9, angle=90, vjust=0.5))   + 
    theme(axis.text.y = element_text(size = 12))   +
    theme(axis.title.x = element_text(size = 14))   + 
    theme(axis.title.y = element_text(size = 14))   +
    theme(legend.position="none") 
  
  for (i in 1:n_pages(ntow.spring.by.stratum.plot)){
    ntow.spring.by.stratum.plot.current <- ntow.spring.by.stratum.plot +
      facet_wrap_paginate(~STRATUM, nrow = ifelse(length(spring.strata)> 20  ,plot.nrow.pp, ceiling(length(spring.strata)/5 ) ),
                          ncol = ifelse(nstrata>20, plot.ncol.pp, 5), page = i, scales = "free_y"   )
    print(ntow.spring.by.stratum.plot.current)
    ggsave(ntow.spring.by.stratum.plot.current,
           filename=file.path(od,paste( "ntow.spring.by.stratum.plot.page", i, plot.f, sep=".")),
           device=plot.f,  height=plot.h, width=plot.w)
  }
  # ggsave(ntow.spring.by.stratum.plot, filename=file.path(od,paste( "ntow.spring.by.stratum.plot", plot.f, sep=".")), 
  #        device=plot.f,  height=plot.h, width=plot.w) 
  
  
  ntow.spring.by.stratum.bigyrs.plot <- ggplot(str.means[str.means$SEASON=='SPRING' & str.means$YEAR>=2009,], aes(x=YEAR, y=ntows.str, color=as.factor(STRATUM))) +
    facet_wrap_paginate(~STRATUM, nrow = ifelse(length(spring.strata)> 20  ,plot.nrow.pp, ceiling(length(spring.strata)/5 ) ),
                        ncol = ifelse(nstrata>20, plot.ncol.pp, 5) , scales = "free_y"  ) +
    geom_line() +
    geom_point() +
    scale_color_discrete(type = strata.color.set)+  
    scale_x_continuous(breaks=seq(bigyears.ts[1], last(bigyears.ts), by=2) ) +
    labs(y='Spring BTS Number of tows')+
    theme_light()  +
    theme(panel.background = element_rect(fill='white')) +
    theme(strip.background =element_rect(fill="white", color="grey65"))+
    theme(strip.text = element_text(colour = 'black', size=10)) +
    theme(axis.text.x = element_text(size = 9, angle=90, vjust=0.5))   + 
    theme(axis.text.y = element_text(size = 12))   +
    theme(axis.title.x = element_text(size = 14))   + 
    theme(axis.title.y = element_text(size = 14))   +
    theme(legend.position="none") 
  
  for (i in 1:n_pages(ntow.spring.by.stratum.plot)){
    ntow.spring.by.stratum.bigyrs.plot.current <- ntow.spring.by.stratum.bigyrs.plot +
      facet_wrap_paginate(~STRATUM, nrow = ifelse(length(spring.strata)> 20  ,plot.nrow.pp, ceiling(length(spring.strata)/5 ) ),
                          ncol = ifelse(nstrata>20, plot.ncol.pp, 5), page = i, scales = "free_y"   )
    print(ntow.spring.by.stratum.bigyrs.plot.current)
    ggsave(ntow.spring.by.stratum.bigyrs.plot.current,
           filename=file.path(od,paste( "ntow.spring.by.stratum.bigyrs.plot.page", i, plot.f, sep=".")),
           device=plot.f,  height=plot.h, width=plot.w)
  }
  
  # ggsave(ntow.spring.by.stratum.bigyrs.plot, filename=file.path(od,paste( "ntow.spring.by.stratum.bigyrs.plot", plot.f, sep=".")), 
  #        device=plot.f,  height=plot.h, width=plot.w) 
  
  tows.yr <- str.means %>%
    group_by(YEAR, SEASON) %>%
    summarise(ntow.yr = sum(ntows.str))
  
  ntows.yr.season.plot <- ggplot(tows.yr, aes(x=YEAR, y=ntow.yr, color=SEASON)) +
    geom_line() +
    geom_point() +
    labs(y='Number of tows') +
    scale_x_continuous(breaks=seq(years.ts[1], last(years.ts), by=2) ) +
    scale_color_manual(values=c(fall.color, spring.color)) +
    theme_light()  +
    theme(panel.background = element_rect(fill='white')) +
    theme(strip.background =element_rect(fill="white", color="grey65"))+
    theme(strip.text = element_text(colour = 'black', size=10)) +
    theme(axis.text.x = element_text(size = 9, angle=90, vjust=0.5))   + 
    theme(axis.text.y = element_text(size = 12))   +
    theme(axis.title.x = element_text(size = 14))   + 
    theme(axis.title.y = element_text(size = 14))   +
    theme(legend.position="bottom") 
  ggsave(ntows.yr.season.plot, filename=file.path(od,paste( "ntows.yr.season.plot", plot.f, sep=".")), 
         device=plot.f,  height=plot.h, width=plot.w) 
  
  
  cv.yr.season.plot <- ggplot(spp.index, aes(x=YEAR, y=CV, color=SEASON)) +
    geom_line() +
    geom_point() +
    labs(y=paste0('CV (', sv.unit, '/tow)') ) +
    scale_x_continuous(breaks=seq(years.ts[1], last(years.ts), by=2) ) +
    scale_color_manual(values=c(fall.color, spring.color)) +
    theme_light()  +
    theme(panel.background = element_rect(fill='white')) +
    theme(strip.background =element_rect(fill="white", color="grey65"))+
    theme(strip.text = element_text(colour = 'black', size=10)) +
    theme(axis.text.x = element_text(size = 9, angle=90, vjust=0.5))   + 
    theme(axis.text.y = element_text(size = 12))   +
    theme(axis.title.x = element_text(size = 14))   + 
    theme(axis.title.y = element_text(size = 14))   +
    theme(legend.position="bottom") 
  ggsave(cv.yr.season.plot, filename=file.path(od,paste( "cv.yr.season.plot", sv.unit, plot.f, sep=".")), 
         device=plot.f,  height=plot.h, width=plot.w) 
  
} # end function survey_evaluation_plots