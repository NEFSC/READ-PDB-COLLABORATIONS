# visualize weekly/yearly workload allocation 
# liz brooks
# 18 february 2025

library(dplyr)
library(ggplot2)

# definitions: 
#     dpy = days per year
#     hpw = hours per week
#     hpd = hours per day



# read input ====
mypath <- "C:/Users/liz.brooks/R/popdy_workload"    # path where you have csv file

work <- read.csv(file=file.path(mypath, 'workload_example.csv'), header=TRUE)   # read in csv file

n.category <- length(work$Category) 


# get units ====
workload <- work %>%
  mutate(hpw.cf = case_when(
    unit=="dpy" ~ (8/52), #convert days per year to hours per week
    unit=="hpd" ~ 5,      #convert hours per day to hours per week
    unit=="hpw" ~ 1       #unit is already hours per week
  )      
  )  %>%
  mutate(dpy.cf = case_when(
    unit=="dpy" ~ 1,        #unit is already days per year
    unit=="hpd" ~ (5*52/8), #convert hours per day to days per year
    unit=="hpw" ~ (52/8)    #convert hours per week to days per year
  )
  )  %>%
  mutate(week=1, year=1) %>%
  mutate(dpy = number*dpy.cf, hpw=number*hpw.cf) %>%
  mutate(order  = as.factor(rev(seq(1,n.category))))  %>%
  mutate(prop.work = 100*(hpw/40) ) %>%
  mutate(cum.hpw = cumsum(hpw), cum.dpy=cumsum(dpy), cum.prop=cumsum(prop.work))  




# make plots ====
## reference line at 40 hrs/week
weekly <- ggplot(workload, aes(fill=order, y=hpw, x=week)) + 
  geom_bar(position="stack", stat="identity", col='black') +
  geom_hline( yintercept=40, col='black', size=2)  +
  scale_fill_discrete(name = "Work", labels = rev(workload$Category) ) +
  scale_x_discrete(name = 'Work week allocation', breaks=1, labels=1) +
  ylab("Hours per week") +
  ggtitle (paste0("Total hours per week: ", round(sum(workload$hpw),1)   ) ) +
  geom_text(aes(y = cum.hpw, label = as.character(round(hpw,1))), vjust = 1.1, colour = "white" )+
  guides( color = 'none', size = 'none')
ggsave(weekly, file=file.path(mypath, "weekly.png"), height=9, width=6)


# reference line for 365 days/yr - 52 weeks*2 for weekends
## federal holidays are already a category so don't need to subtract from 365
yearly <- ggplot(workload, aes(fill=order, y=dpy, x=week)) + 
  geom_bar(position="stack", stat="identity", col='black') +
  geom_hline( yintercept=(365-52*2), col='black', size=2)  +
  scale_fill_discrete(name = "Work", labels = rev(workload$Category) ) +
  scale_x_discrete(name = 'Work year allocation', breaks=1, labels=1) +
  ylab("Days per year") +
  ggtitle (paste0("Total days per year: ", round(sum(workload$dpy),1)   ) )+
  geom_text(aes(y = cum.dpy, label = as.character(round(dpy,1))), vjust = 1.1, colour = "white") +
  guides( color = 'none', size = 'none')
ggsave(yearly, file=file.path(mypath, "yearly.png"), height=9, width=6)


weekly.prop <- ggplot(workload, aes(fill=order, y=prop.work, x=week)) + 
  geom_bar(position="stack", stat="identity", col='black') +
  geom_hline( yintercept=100, col='black', size=2)  +
  scale_fill_discrete(name = "Work", labels = rev(workload$Category) ) +
  scale_x_discrete(name = 'Work week allocation', breaks=1, labels=1) +
  ylab("Proportion of Hours per week") +
  ggtitle (paste0("Total work week: ", round(sum(workload$prop.work),1) , ' %'  ) ) +
  geom_text(aes(y = cum.prop, label = as.character(round(prop.work,1)), vjust = 1), colour = "white" ) +
  guides( color = 'none', size = 'none')
ggsave(weekly.prop, file=file.path(mypath, "weekly.prop.png"), height=9, width=6)




# note : could use check_overlap with geom_text to suppress labels 
#        that would be overwritten by adjacent labels

