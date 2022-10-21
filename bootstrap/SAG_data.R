taf.library(icesFO)


out <- load_sag(2022, "Bay of Biscay and the Iberian Coast")

#I rescue ank as it has "Replaced" as Purpose (I need to modify the function to load Purpose = Replaced)
ank <- load_sag(2022, "Bay of Biscay and the Iberian Coast")
ank <- ank %>% filter(FishStock == "ank.27.78abd")

out <- out %>% filter(FishStock != "ank.27.78abd")
out <- rbind(out, ank)

sag_complete <- out

write.taf(out, file = "SAG_complete_BtS.csv", quote = TRUE)


status <- load_sag_status(2022)
#I rescue ank as it has "Replaced" as Purpose (I modify the function)
ank_status <- load_sag_status(2022)
status <- status %>% filter(StockKeyLabel != "ank.27.78abd")

ank_status <- ank_status %>% filter(StockKeyLabel == "ank.27.78abd")

status <- rbind(status, ank_status)
