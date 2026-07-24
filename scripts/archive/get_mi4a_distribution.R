library(readxl)
mi4a_wpro <- read_excel('data/SEARO & WPRO data points.xlsx',sheet = 'WPRO Data',
                        skip=2)
mi4a_wpro <- mi4a_wpro[,!colnames(mi4a_wpro) %in% c('Presentation','Dosage Number',
                                                    'ContractLength')]
mi4a_wpro <- mi4a_wpro[!is.na(mi4a_wpro$`Income Group`),]

create_density_hist <- function(income_group){
  if (income_group=='UMIC'){
    higher_grp <- 'HIC'
  }
  if (income_group =='LMIC'){
    higher_grp <- 'UMIC'
  }
  if (income_group %in% c('UMIC','LMIC')){
    median_higher_grp <- median(mi4a_wpro[mi4a_wpro$`Income Group`==higher_grp,]$`Price per Dose in USD`)
    to_plot <- median_higher_grp-mi4a_wpro[mi4a_wpro$`Income Group`==income_group,]$`Price per Dose in USD`
    xlab_text <- paste0('Difference in between median price per dose for ', higher_grp,'\n and ',
                   income_group,' price per dose')
  } else {
    to_plot <- mi4a_wpro[mi4a_wpro$`Income Group`==income_group,]$`Price per Dose in USD`
    xlab_text <- paste0('Price per dose in USD for ', income_group)
  }

  hist(to_plot,
       main=paste0('MI4A WPRO prices (2021)'), xlab=xlab_text,
       freq=FALSE, col=rgb(0,0,1,0.3), border=NA)
  lines(density(to_plot),
        col='red', lwd=2)
}
create_density_hist('HIC')
create_density_hist('UMIC')
create_density_hist('LMIC')


hist(mi4a_wpro[mi4a_wpro$`Income Group`=="UMIC",]$`Price per Dose in USD`)
hist(mi4a_wpro[mi4a_wpro$`Income Group`=="LMIC",]$`Price per Dose in USD`)
