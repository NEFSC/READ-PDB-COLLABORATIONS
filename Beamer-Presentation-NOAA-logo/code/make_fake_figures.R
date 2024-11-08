library(dplyr)
library(tidyr)
library(ggplot2)
library(here)


fake_table <- data.frame(Year=seq(2019,2023), 
                    'Landings (mt)'=runif(n=5, min=100, max=185),
                    "Discards (mt)"= rnorm(n=5, mean=35, sd=7),
                    check.names=FALSE
                    )
fake_table
write.csv(fake_table, file=file.path(here(), 'tables', 'fake_table.csv'), row.names=FALSE)



fake.fig <- as_tibble(cbind(x=runif(n=1000, min=1, max=30), y=rnorm(n=1000, mean=50, sd=15), 
                  text.col=rep(seq(1,5), 200), fontsize=rep(seq(0.5,2.5, by=0.5)),
                  thelabel='word')  ) %>%
  mutate(x=as.numeric(x), y=as.numeric(y), fontsize=as.numeric(fontsize))

fake.fig.plot <- ggplot(fake.fig, aes(x=x, y=y, col=as.factor(text.col) )  ) +
  geom_point(col='white') +
  geom_text( aes(x=x, y=y, label=as.factor(thelabel), size=fontsize ) ,
            show.legend=FALSE, inherit.aes=TRUE) +
  guides(fill = 'none', color = 'none', linetype = 'none', shape = 'none') +
  theme_light()  +
  theme(strip.background =element_rect(fill="white", color="grey65"))+
  theme(strip.text = element_text(colour = 'black', size=12)) +
  theme(axis.text.x = element_text(size = 12))   + 
  theme(axis.text.y = element_text(size = 12))   +
  theme(axis.title.x = element_text(size = 14))   + 
  theme(axis.title.y = element_text(size = 14))   +
  theme(legend.position="bottom") +
  theme(legend.text = element_text(colour="black", size = 14, face = "bold"), 
        legend.title=element_text(size=10)) 

ggsave(fake.fig.plot, filename=file.path(here(), 'figures', 'fake.fig.plot.png'),  height=6, width=9)



years <- seq(2000,2020)
nyears <- length(years)
x1 <- 100
x2 <- 98
set.seed(1)

fake.model.results <- as_tibble(data.frame(Year=years, M1=(100+25*arima.sim(list(order=c(1,0,0), ar=.5), n=nyears)) ,
                                      M2=(rnorm(n=nyears, mean=100, sd=25)))  ) %>%
  pivot_longer(cols=M1:M2, names_to='Model', values_to='Estimate')

fake.model.plot <- ggplot(fake.model.results, aes(x=Year, y=Estimate, color=Model)) +
  geom_line(size=1.15) +
  geom_point(cex=2) +
  guides(fill = 'none', color = 'none', linetype = 'none', shape = 'none') +
  theme_light()  +
  theme(strip.background =element_rect(fill="white", color="grey65"))+
  theme(strip.text = element_text(colour = 'black', size=12)) +
  theme(axis.text.x = element_text(size = 12))   + 
  theme(axis.text.y = element_text(size = 12))   +
  theme(axis.title.x = element_text(size = 14))   + 
  theme(axis.title.y = element_text(size = 14))   +
  theme(legend.position="bottom") +
  theme(legend.text = element_text(colour="black", size = 14, face = "bold"), 
        legend.title=element_text(size=10)) 

ggsave(fake.model.plot, filename=file.path(here(), 'figures', 'fake.model.plot.png'),  height=6, width=9)



# make some fake output and save to rds folder
set.seed(1)
SSB1 <- rnorm(n=nyears, mean=90, sd=17)
set.seed(1)
SSB2 <- rnorm(n=nyears, mean=110, sd=27)
fake.output <- list(Update=2024, Years=years, 
                 M1=data.frame(SSB=SSB1, Catch.pred=SSB1*runif(n=nyears, min=0.45, max=0.6) ),
                 M2=data.frame(SSB=SSB2, Catch.pred=SSB2*runif(n=nyears, min=0.45, max=0.6) )
                                )

saveRDS(fake.output, file=file.path(here(), 'rds', 'fake.output.RDS'))


