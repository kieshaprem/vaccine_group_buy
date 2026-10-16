#-------------------------------------------------------------------------------
# The following codes are for creating the plot for Supplementary Figure 1
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Reading in data for health out-of-pocket expenditure per capita (current US$)
# from WB indicator SH.XPD.OOPC.PC.CD
# ------------------------------------------------------------------------------
oop_raw = read.csv('data/oop_data.csv')
names(oop_raw)[5:9] = as.character(2019:2023)

oop_all_countries_2021 = oop_raw[,c("Country.Code","2021")]
oop_all_countries_2021 = oop_all_countries_2021[oop_all_countries_2021[,2]!=".." & 
                                                  oop_all_countries_2021[,2]!="",]
oop_all_countries_2021[,2] = as.numeric(oop_all_countries_2021[,2])

asean_iso =  c('MMR','TLS','LAO','KHM','PHL','VNM','IDN','THA','MYS','BRN','SGP')

# Reading in data for GDP per capita from WB indicator NY.GDP.PCAP.CD
gdp_raw = read.csv('data/gdp_data.csv')

gdp_all_countries_2022 = gdp_raw[,c(4,5)]
gdp_all_countries_2022 = gdp_all_countries_2022[gdp_all_countries_2022[,2]!=".." & 
                                                  gdp_all_countries_2022[,2]!="",]
gdp_all_countries_2022[,2] = as.numeric(gdp_all_countries_2022[,2])

# Reading in data for current health expenditure (CHE) per capita from WB 
# indicator SH.XPD.CHEX.PC.CD
exp_raw = read.csv('data/exp_2021.csv')
exp_all_countries_2021 = exp_raw[,c(2,7)]
exp_all_countries_2021 = exp_all_countries_2021[exp_all_countries_2021[,2]!=".." & 
                                                  exp_all_countries_2021[,2]!="",]
exp_all_countries_2021[,2] = as.numeric(exp_all_countries_2021[,2])

# Merge GDP per capita  with OOPS per capita 
gdp_oop <- merge(gdp_all_countries_2022, oop_all_countries_2021, 
                 by = "Country.Code")
names(gdp_oop)[2:3] <- c("GDP_2022","OOP_2021")

# Merge GDP per capita  and OOPS per capita with CHE per capita
gdp_oop_exp <- merge(gdp_oop, exp_all_countries_2021, by = "Country.Code")
names(gdp_oop_exp)[4] <- "EXP_2021"

#-------------------------------------------------------------------------------
# The following codes plot Supplementary Figure 1
#-------------------------------------------------------------------------------
library(grid)

model_oops <- lm(log(x=OOP_2021,base=10)~log(x=GDP_2022,base=10), 
                 data = gdp_oop_exp)
model_exp <- lm(log(x=EXP_2021,base=10)~log(x=GDP_2022,base=10), 
                data = gdp_oop_exp)


png("output/plots/supp1_gdp_oops_che_all.png",height=8,width=12,
    units='cm',res=700,pointsize=10)
if(1)
{
  pushViewport(plotViewport(c(4,5,1,2),xscale=c(2,6),yscale=c(0,5)))#)NEW
  grid.rect()
  
  ## Axes
  grid.yaxis(at=seq(0,5), # from the range of GDP per capita transformed into log10 scale
             label=format(c(1,10,100,1e3,1e4,1e5), big.mark = ",", 
                          scientific = FALSE),gp=gpar(fontsize=8))
  grid.xaxis(at=seq(2,6), # from the ranges of OOPS and CHE per capita transformed into log10 scale
             label=format(c(100,1000,1e4,1e5,1e6), big.mark=",", 
                          scientific=FALSE),gp=gpar(fontsize=8))
  
  # Regression line indicating trends between GDP per capita and OOPS/CHE per capita
  grid.function(function(input){return (list(x=input,
                                             y=model_oops$coefficients[1]+
                                               model_oops$coefficients[2]*input))},
                gp=gpar(lty=2, lwd=0.8))
  
  grid.function(function(input){return (list(x=input,
                                             y=model_exp$coefficients[1]+
                                               model_exp$coefficients[2]*input))},
                gp=gpar(lty=2, lwd=0.8))
  
  # X and Y-axis labels
  grid.text('GDP per capita in 2022 (current US$) in log10 scale',
            y = unit(0,'npc')+unit(-2.8,'lines'),gp = gpar(fontsize=9,
                                                           fontface='bold',
                                                           col = 'black'))
  grid.text('Healthcare expenditure in 2021\n(current US$) in log10scale',
            x = unit(0,'npc')+unit(-4.2,'lines'),rot=90,gp = gpar(fontsize=9,
                                                                  fontface='bold',
                                                                  col = 'black'))
  
  # Points for OOPS/CHE per capita plotted against GDP per capita
  ## Non-ASEAN points
  grid.points(x=log(x=gdp_oop[!gdp_oop$Country.Code %in% asean_iso,][,2],
                    base=10),
              y=log(x=gdp_oop[!gdp_oop$Country.Code %in% asean_iso,][,3],
                    base=10),
              default.units = 'native',pch=16,gp=gpar(alpha=0.5,cex = 0.5, 
                                                      lwd=0.8, 
                                                      col="#0047AB"))
  grid.points(x=log(x=gdp_oop_exp[!gdp_oop_exp$Country.Code %in% asean_iso,][,2],
                    base=10),
              y=log(x=gdp_oop_exp[!gdp_oop_exp$Country.Code %in% asean_iso,][,4],
                    base=10),
              default.units = 'native',pch=18,gp=gpar(alpha=0.5,cex = 0.6,
                                                      lwd=0.8, 
                                                      col="#40E0D0"))
  
  ## ASEAN points
  grid.points(x=log(x=gdp_oop[gdp_oop$Country.Code %in% asean_iso,][,2],base=10),
              y=log(x=gdp_oop[gdp_oop$Country.Code %in% asean_iso,][,3],base=10),
              default.units = 'native',pch=16,gp=gpar(alpha=0.8,cex = 0.5, 
                                                      col="#FD1813"))
  grid.points(x=log(x=gdp_oop[gdp_oop$Country.Code %in% asean_iso,][,2],base=10),
              y=log(x=gdp_oop[gdp_oop$Country.Code %in% asean_iso,][,3],base=10),
              default.units = 'native',pch=1,gp=gpar(cex = 0.5, lwd=1, 
                                                     col="#0047AB"))
  
  
  grid.points(x=log(x=gdp_oop_exp[gdp_oop_exp$Country.Code %in% asean_iso,][,2],
                    base=10),
              y=log(x=gdp_oop_exp[gdp_oop_exp$Country.Code %in% asean_iso,][,4],
                    base=10),
              default.units = 'native',pch=18,gp=gpar(alpha=0.8, cex = 0.6, 
                                                      col="#FD1813"))
  grid.points(x=log(x=gdp_oop_exp[gdp_oop_exp$Country.Code %in% asean_iso,][,2],
                    base=10),
              y=log(x=gdp_oop_exp[gdp_oop_exp$Country.Code %in% asean_iso,][,4],
                    base=10),
              default.units = 'native',pch=5,gp=gpar(cex = 0.6,lwd=1, 
                                                     col="#40E0D0"))
  
  
  # Legend boxes and text
  leg_1_box <- rectGrob(x = 0.06, y = 0.88, width = 0.025, height = 0.0125,
                        gp = gpar(col = "#0047AB", fill = "navy", lty = 1, 
                                  lwd=0.8))
  leg_2_box <- rectGrob(x = 0.06, y = 0.93, width = 0.025, height = 0.0125,
                        gp = gpar(col = "#40E0D0", fill = "#40E0D0", lty = 1, 
                                  lwd=0.8))
  leg_3_box <- rectGrob(x = 0.06, y = 0.83, width = 0.025, height = 0.0125,
                        gp = gpar(col = "#FD1813", fill = "#FD1813", lty = 1, 
                                  lwd=0.8))
  
  grid.draw(leg_1_box)
  grid.draw(leg_2_box)
  grid.draw(leg_3_box)
  
  grid.text("                    Health expenditure per capita", 
            x=unit(6.1,"lines"),y=unit(21.5,"lines"),gp=gpar(fontsize=6))
  grid.text("                   Household OOPS per capita", 
            x=unit(6.1,"lines"),y=unit(20.4, "lines"),gp=gpar(fontsize=6))
  grid.text("ASEAN countries", x=unit(6.1,"lines"),y=unit(19.3, "lines"),
            gp=gpar(fontsize=6))
  
  popViewport()
}
dev.off()

