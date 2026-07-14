checking_bilateral <- data.frame(cbind(bilateral_full_updated,bilateral_min_full,
                                       bilateral_max_full))
library(writexl)
write_xlsx(checking_bilateral,path = 'output/check_bilateral_full.xlsx')
