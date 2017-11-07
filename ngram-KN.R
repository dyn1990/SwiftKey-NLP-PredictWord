###################################
##### discount for mkn method #####
###################################

mkn.discount <- data.frame(count = seq(0, 3, 1),
                       uni = rep(0, 4),
                       bi  = rep(0, 4),
                       tri = rep(0, 4))

### uni discount
Y <-  uni.freq$count[1] / (uni.freq$count[1] + 2 * uni.freq$count[2]) 
mkn.discount$uni[2] <-  1 - 2 * Y * uni.freq$count[2] / uni.freq$count[1]
mkn.discount$uni[3] <-  2 - 3 * Y * uni.freq$count[3] / uni.freq$count[2]
mkn.discount$uni[4] <-  3 - 4 * Y * uni.freq$count[4] / uni.freq$count[3]

### bi discount
Y <-  bi.freq$count[1] / (bi.freq$count[1] + 2 * bi.freq$count[2]) 
mkn.discount$bi[2] <-  1 - 2 * Y * bi.freq$count[2] / bi.freq$count[1]
mkn.discount$bi[3] <-  2 - 3 * Y * bi.freq$count[3] / bi.freq$count[2]
mkn.discount$bi[4] <-  3 - 4 * Y * bi.freq$count[4] / bi.freq$count[3]

### tri discount
Y <-  tri.freq$count[1] / (tri.freq$count[1] + 2 * tri.freq$count[2]) 
mkn.discount$tri[2] <-  1 - 2 * Y * tri.freq$count[2] / tri.freq$count[1]
mkn.discount$tri[3] <-  2 - 3 * Y * tri.freq$count[3] / tri.freq$count[2]
mkn.discount$tri[4] <-  3 - 4 * Y * tri.freq$count[4] / tri.freq$count[3]

saveRDS(mkn.discount, file = "mkn_discount.RDS")

