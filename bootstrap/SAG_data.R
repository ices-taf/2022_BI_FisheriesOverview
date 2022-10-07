taf.library(icesFO)


out <- load_sag(2022, "Bay of Biscay and the Iberian Coast")

sag_complete <- out

status <- load_sag_status(2022)
write.taf(status, file = "bootstrap/data/SAG_data/SAG_status.csv", quote = TRUE)



