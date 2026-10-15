library(readxl)

spreadsheet <- read_excel('data/Scenarios for regionalizing vaccine access in ASEAN_Jun24_updatedNov24.xlsx',
                          sheet = 'Vaccine Purchase Data_WHO', skip = 8)
spreadsheet <- spreadsheet[,!colnames(spreadsheet) %in% c('Presentation','Dosage Number',
                                                          'Annual Number of Doses','WHO PQ',
                                                          'ContractLength','INCOTerm',
                                                          'VATPercent','VATName')]
wpro <- spreadsheet[spreadsheet$Region=="WPRO",]
wpro_hic <- wpro[(wpro$`Income Group`=='HIC')&wpro$`Procurement Mechanism`=="Bilateral",]
wpro_lmic <- wpro[(wpro$`Income Group`=='LMIC')&wpro$`Procurement Mechanism`=="Bilateral",]
wpro_umic <- wpro[wpro$`Income Group`=="UMIC" & wpro$`Procurement Mechanism`=="Bilateral",]

euro <- spreadsheet[spreadsheet$Region=="EURO",]
euro_hic <- euro[euro$`Income Group`=='HIC',]
euro_hic_multilateral <- euro_hic[euro_hic$`Procurement Mechanism`=="Multilateral",]
median(euro_hic_multilateral$`Price per Dose in USD`)

median(spreadsheet[spreadsheet$`Income Group`=="HIC"&spreadsheet$`Procurement Mechanism`=="Bilateral",]$`Price per Dose in USD`)


hic_bilateral <- spreadsheet[spreadsheet$`Income Group`=="HIC"&spreadsheet$`Procurement Mechanism`=="Bilateral",]
umic_bilateral <- spreadsheet[spreadsheet$`Income Group`=="UMIC"&spreadsheet$`Procurement Mechanism`=="Bilateral",]
lmic_bilateral <- spreadsheet[spreadsheet$`Income Group`=="LMIC"&spreadsheet$`Procurement Mechanism`=="Bilateral",]


################################################################################

png(paste0("output/plots/mia4.png"),height=8,width=12,units='cm',res=700,pointsize=10)
if(1)
{
  pushViewport(plotViewport(c(3,4,1,1),xscale=c(0.5,3.5),yscale=c(0,40.5)))
  #grid.rect()
  #grid.yaxis(gp=gpar(fontsize=8))
  # grid.xaxis(at=1:3, 
  #            label=c("HIC","UMIC","LMIC"),gp=gpar(fontsize=8))
  # 
  grid.text('Source: WHO MI4A: Market Information for Access to Vaccines, 2021-2022',
            x = unit(0,'npc')+unit(9.5,'lines'),
            y = unit(0,'npc')+unit(-2.8,'lines'),gp = gpar(fontsize=8,col = 'black'))
  grid.text(paste0('Price per dose in USD'),x = unit(0,'npc')+unit(-4,'lines'),rot=90,gp = gpar(fontsize=9,fontface='bold',col = 'black'))
  
  # x axis
  country_groups <- c("HIC","UMIC","LMIC")
  for (i in 1:3){
    grid.text(country_groups[i],x=i,y=-2, default.units = 'native',gp=gpar(fontsize=8,fontface="bold"))
  }
  # y axis
  for (i in seq(0,40,10)){
    grid.text(i,x=0.35,y=i, default.units = 'native',gp=gpar(fontsize=8))
  }
  
  #HIC
  grid.lines(x=c(1,1), y=c(max(hic_bilateral$`Price per Dose in USD`), min(hic_bilateral$`Price per Dose in USD`)),default.units = 'native')
  grid.lines(x=c(0.95,1.05), y=c(max(hic_bilateral$`Price per Dose in USD`), max(hic_bilateral$`Price per Dose in USD`)),default.units = 'native',gp=gpar(lwd=1))
  grid.lines(x=c(0.95,1.05), y=c(min(hic_bilateral$`Price per Dose in USD`), min(hic_bilateral$`Price per Dose in USD`)),default.units = 'native',gp=gpar(lwd=1))
  
  hic_summary <- summary(hic_bilateral$`Price per Dose in USD`)
  grid.polygon(x = c(0.7, 1.3, 1.3, 0.7),
               y = c(hic_summary["1st Qu."],
                     hic_summary["1st Qu."],
                     hic_summary["3rd Qu."],
                     hic_summary["3rd Qu."]),
               default.units = 'native',
               gp = gpar(col = '#0047AB', fill = '#0047AB'))
  grid.lines(x=c(0.7,1.3), y=c(hic_summary["Median"],hic_summary["Median"]),
             default.units = 'native',gp=gpar(col="white"))
  grid.text("15.00",x=1.45,y=hic_summary["Median"], default.units = 'native',gp=gpar(fontsize=7))
  
  #UMIC
  grid.lines(x=c(2,2), y=c(max(umic_bilateral$`Price per Dose in USD`), min(umic_bilateral$`Price per Dose in USD`)),default.units = 'native')
  grid.lines(x=c(1.95,2.05), y=c(max(umic_bilateral$`Price per Dose in USD`), max(umic_bilateral$`Price per Dose in USD`)),default.units = 'native',gp=gpar(lwd=1))
  grid.lines(x=c(1.95,2.05), y=c(min(umic_bilateral$`Price per Dose in USD`), min(umic_bilateral$`Price per Dose in USD`)),default.units = 'native',gp=gpar(lwd=1))
  
  umic_summary <- summary(umic_bilateral$`Price per Dose in USD`)
  grid.polygon(x = c(1.7, 2.3, 2.3, 1.7),
               y = c(umic_summary["1st Qu."],
                     umic_summary["1st Qu."],
                     umic_summary["3rd Qu."],
                     umic_summary["3rd Qu."]),
               default.units = 'native',
               gp = gpar(col = '#0047AB', fill = '#0047AB'))
  grid.lines(x=c(1.7,2.3), y=c(umic_summary["Median"],umic_summary["Median"]),
             default.units = 'native',gp=gpar(col="white"))
  grid.text("10.00",x=2.45,y=umic_summary["Median"], default.units = 'native',gp=gpar(fontsize=7))
            
  
  #LMIC
  grid.lines(x=c(3,3), y=c(max(lmic_bilateral$`Price per Dose in USD`), min(lmic_bilateral$`Price per Dose in USD`)),default.units = 'native')
  grid.lines(x=c(2.95,3.05), y=c(max(lmic_bilateral$`Price per Dose in USD`), max(lmic_bilateral$`Price per Dose in USD`)),default.units = 'native',gp=gpar(lwd=1))
  grid.lines(x=c(2.95,3.05), y=c(min(lmic_bilateral$`Price per Dose in USD`), min(lmic_bilateral$`Price per Dose in USD`)),default.units = 'native',gp=gpar(lwd=1))
  
  lmic_summary <- summary(lmic_bilateral$`Price per Dose in USD`)
  grid.polygon(x = c(2.7, 3.3, 3.3, 2.7),
               y = c(lmic_summary["1st Qu."],
                     lmic_summary["1st Qu."],
                     lmic_summary["3rd Qu."],
                     lmic_summary["3rd Qu."]),
               default.units = 'native',
               gp = gpar(col = '#0047AB', fill = '#0047AB'))
  grid.lines(x=c(2.7,3.3), y=c(lmic_summary["Median"],lmic_summary["Median"]),
             default.units = 'native',gp=gpar(col="white"))
  grid.text("9.95",x=3.45,y=lmic_summary["Median"], default.units = 'native',gp=gpar(fontsize=7))
  
  
 popViewport()
}
dev.off()


