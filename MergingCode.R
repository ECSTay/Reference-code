library(flextable)

dat <- data.frame(v1 = c(rep("a", 3), rep("b", 3), rep("c", 3)),
                  v2 = c("G1", "G1", "G2", "G2", "G3", "G3", "G4", "G5", "G6"),
                  v3 = c(1:9), 
                  stringsAsFactors = FALSE)
dat$v_dummy <- paste0(dat$v1, dat$v2)
ft <- flextable(dat, col_keys = c("v1", "v2", "v3"))
ft <- theme_box(ft)
ft <- merge_v(ft, j = c("v1", "v_dummy"), target = c("v1", "v2"))
ft
