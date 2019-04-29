
devtools::load_all("~/Google Drive/McGill/Research/PEPFAR ANC/ANCRTAdjust/R")

data("ancrt")

ancrt <- name_var(ancrt, faciluid = "faciluid", time = "time", n_clients = "n_clients",
                  n_status = "n_status", knownpos = "knownpos", testpos = "testpos",
                  testneg = "testneg", age = "age") 

ancrt_cleaned <- data_clean(ancrt)


png(filename = "~/Google Drive/McGill/Research/PEPFAR ANC/ANCRTAdjust/man/figures/README-example-1.png",
    width = 5, height = 6, units = "in", pointsize = 24,
    bg = "white", res = 300)
descriptive_plot(ancrt_cleaned)
dev.off()

