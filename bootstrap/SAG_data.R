taf.library(icesFO)


summary <- load_sag_summary(2021)
<<<<<<< HEAD
write.taf(summary, file = "bootstrap/data/SAG_data/SAG_summary.csv")

refpts <- load_sag_refpts(2021)
write.taf(refpts, file = "bootstrap/data/SAG_data/SAG_refpts.csv")

status <- load_sag_status(2021)
write.taf(status, file = "bootstrap/data/SAG_data/SAG_status.csv", quote = TRUE)
=======
write.taf(summary, file = "SAG_summary.csv")

refpts <- load_sag_refpts(2021)
write.taf(refpts, file = "SAG_refpts.csv")

status <- load_sag_status(2021)
write.taf(status, file = "SAG_status.csv", quote = TRUE)
>>>>>>> 126b60a802391bcf894b3d1a86a1ca3e3a43ffdf
