# 
# outputFolder <- "output_5"
# bal <- readRDS(sprintf("%s/Bal_l1_s1_p1_t5441_c5442_s1_o5440.rds", outputFolder))
# bal$absAmSmd <- abs(bal$afterMatchingStdDiff)
# bal$absBmSmd <- abs(bal$beforeMatchingStdDiff)
# unbalAm <- bal[bal$absAmSmd > 0.1, ]
# unbalAm <- na.omit(unbalAm)
# unbalBm <- bal[bal$absBmSmd > 0.1, ]
# unbalBm <- na.omit(unbalBm)
# 
# unbal <- bal[bal$absAmSmd > 0.1 & bal$absBmSmd > 0.1, ]
# unbal <- na.omit(unbal)