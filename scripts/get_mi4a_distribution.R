library(readxl)
mi4a_wpro <- read_excel('data/SEARO & WPRO data points.xlsx',sheet = 'WPRO Data',
                        skip=2)
mi4a_wpro <- mi4a_wpro[,!colnames(mi4a_wpro) %in% c('Presentation','Dosage Number',
                                                    'ContractLength')]
mi4a_wpro <- mi4a_wpro[!is.na(mi4a_wpro$`Income Group`),]
hist(mi4a_wpro[mi4a_wpro$`Income Group`=="HIC",]$`Price per Dose in USD`,
     main='HIC MI4A WPRO prices (2021)',xlab='Price per dose in USD')

plot(density(mi4a_wpro[mi4a_wpro$`Income Group`=="HIC",]$`Price per Dose in USD`),
             main='HIC MI4A WPRO prices (2021)',xlab='Price per dose in USD')

create_density_hist <- function(income_group){
  hist(mi4a_wpro[mi4a_wpro$`Income Group`==income_group,]$`Price per Dose in USD`,
       main=paste0(income_group,' MI4A WPRO prices (2021)'), xlab='Price per dose in USD',
       freq=FALSE, col=rgb(0,0,1,0.3), border=NA,xlim=c(0,55))
  lines(density(mi4a_wpro[mi4a_wpro$`Income Group`==income_group,]$`Price per Dose in USD`),
        col='red', lwd=2)
}
create_density_hist('HIC')
create_density_hist('UMIC')
create_density_hist('LMIC')


hist(mi4a_wpro[mi4a_wpro$`Income Group`=="UMIC",]$`Price per Dose in USD`)
hist(mi4a_wpro[mi4a_wpro$`Income Group`=="LMIC",]$`Price per Dose in USD`)
