library(WDI)
library(ggplot2)

WDIsearch("gdp")
WDIsearch("gdp.*capita.*constant")
dat = WDI(indicator='6.0.GDPpc_constant', country=c('UZ','CA','US'), start=1960, end=2012)

dat = WDI(indicator='NY.GDP.PCAP.KD', country=c('UZ','CA','US'), start=1960, end=2012)

ggplot(dat, aes(year, NY.GDP.PCAP.KD, color=country)) +
  geom_line() + 
  xlab('Year') + 
  ylab('GDP per capita')


