
tinh_tien_dien.function <- function(n){
    level1 <- 1678
    level2 <- 1734
    level3 <- 2014
    level4 <- 2536
    level5 <- 2834
    level6 <- 2927
    bac50 <- 50
    bac100 <- 100
    
    if (kwh <= 50){
        tongtien <- kwh*level1
    } else if (kwh <= 100 ){
        tongtien <-  (bac50*level1) + (kwh - bac50)*level2
    } else if (kwh <= 200){
        tongtien <-  (bac50*level1) + (bac50* level2) + (kwh - 2*bac50)*level3
    } else if (kwh <= 300){
        tongtien <-  (bac50*level1) + (bac50* level2) + (bac100*level3) + (kwh - 2*bac50 - bac100)*level4
    } else if (kwh <= 400){
        tongtien <-  (bac50*level1) + (bac50* level2) + (bac100*level3) + (bac100*level4) + (kwh - 2*bac50 - 2*bac100)*level5
    } else {
        tongtien <-  (bac50*level1) + (bac50* level2) + (bac100*level3) + (bac100*level4)+ (bac100*level5) + (kwh - 2*bac50 - 3*bac100)*level6
    }
    return(tongtien)
}

kiem_tra_SNT.function <- function(x){
    flag <- TRUE
    if(x < 2) {
        flag <- FALSE
    } else {
        i <- 2
        while (i <= x/2){
            if(x%%i==0){
            flag <- FALSE
            }
        i <- i+ 1
    }
    }
    return (flag)
}

#1: GOBIKE, 2: GOSEND
tinh_cuoc_xe.function <- function(dichvu,sokm) {
    dv1_2km <- 10000
    dv1_up2km <- 36000
    dv2_2km <- 15000
    dv2_up2km <- 4000
    if (dichvu == 1){
        if (sokm <= 2){
            tongtien <- dv1_2km
        } else {
            tongtien <-  dv1_2km + (sokm - 2)*dv1_up2km
        }
    } else {
        if (sokm <=2) {
            tongtien <- dv2_2km
        } else {
            tongtien <- dv2_2km + (sokm -2)*dv2_up2km
        }
    }
    return (tongtien)
}
