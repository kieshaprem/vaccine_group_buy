bilateral_alphabetical <- bilateral[order(bilateral$Country),]
bilateral_sens_analysis$Brunei*dataset[dataset$Country=='Brunei',]$`Population (2022)`
bilateral_sens_analysis$Cambodia*dataset[dataset$Country=='Cambodia',]$`Population (2022)`
bilateral_sens_analysis$Indonesia*dataset[dataset$Country=='Indonesia',]$`Population (2022)`
bilateral_sens_analysis$Laos*dataset[dataset$Country=='Laos',]$`Population (2022)`
bilateral_sens_analysis$Malaysia*dataset[dataset$Country=='Malaysia',]$`Population (2022)`
bilateral_sens_analysis$Myanmar*dataset[dataset$Country=='Myanmar',]$`Population (2022)`
bilateral_sens_analysis$Philippines*dataset[dataset$Country=='Philippines',]$`Population (2022)`
bilateral_sens_analysis$Singapore*dataset[dataset$Country=='Singapore',]$`Population (2022)`
bilateral_sens_analysis$Thailand*dataset[dataset$Country=='Thailand',]$`Population (2022)`
bilateral_sens_analysis[['Viet Nam']]*dataset[dataset$Country=='Viet Nam',]$`Population (2022)`

uniform_analysis_expenditure$min_total_savings <- bilateral_analysis_expenditure$min_total_expenditure-uniform_analysis_expenditure$min_total_expenditure
uniform_analysis_expenditure$median_total_savings <- bilateral_analysis_expenditure$median_total_expenditure-uniform_analysis_expenditure$median_total_expenditure
uniform_analysis_expenditure$max_total_savings <- bilateral_analysis_expenditure$max_total_expenditure-uniform_analysis_expenditure$max_total_expenditure
