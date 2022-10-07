
library(icesTAF)
library(icesFO)

sid <- load_sid(2022)

write.taf(sid, file = "sid.csv", quote = TRUE)
