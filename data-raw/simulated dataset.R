devtools::load_all("~/Google Drive/McGill/Research/PEPFAR ANC/ANCRTAdjust/R")
setwd("~/Google Drive/McGill/Research/PEPFAR ANC/ANCRTAdjust")

#'=================================  
# Simulated dataset
#'=================================
  # Assume that HIV prevalence is 10% and that there are 40% known positives.
  n_fac <- 1000  # Nb. Facilities
  time <- 5 * 4      # 5-year follow-up
  anc <- round(runif(n_fac, min = 1, max = 400), 0)   # Number of ANC attendees per clinic (average)
  
  par(mfrow = c(3, 1))
  # true hiv prevalence at each facility
  prevalence <-  rbeta(n_fac, shape1 = 2, shape2 = 10)
  print(paste('mean = ', round(2/(2 + 12) * 100, 1)))
  hist(prevalence)
  # art coverage at each facility
  art_cov <- rbeta(n_fac, shape1 = 5, shape2 = 8)
  print(paste('mean = ', round(5/(8 + 5) * 100, 1)))
  hist(art_cov)
  # hiv testing coverage at each facility (exluding knownpos)
  cov_fac <- rbeta(n_fac, shape1 = 9, shape2 = 1)
  print(paste('mean = ', round(9/(9 + 1) * 100, 1)))
  hist(cov_fac)
   
    id <- NULL
    time_id <- NULL
    n_clients <- NULL
    n_status <- NULL
    knownpos <- NULL
    testpos <- NULL
    testneg <- NULL
    true_prv <- NULL
    
    for (n in 1:n_fac) {
      id <- c(id, paste('F_', rep(n, time), sep = ''))
      time_id <- c(time_id, 1:time)
      n_clients.i <- round(rnorm(time, mean = anc[n], sd = 0.1 * anc[n]))
      pos.i <- rbinom(time, prob = prevalence[n], size = n_clients.i)  
      knownpos.i <- round(pos.i * art_cov[n])
      cov.i <- round(runif(time, min = cov_fac[n], max = 1) * n_clients.i) / n_clients.i
      
      tested.i <- round(cov.i * n_clients.i) - knownpos.i
      testpos.i <- round((pos.i - knownpos.i) * cov.i)
      testneg.i <- tested.i - testpos.i
      n_status.i <- knownpos.i + testpos.i + testneg.i
      
      n_clients <- c(n_clients, n_clients.i)
      n_status <- c(n_status, n_status.i)
      knownpos <- c(knownpos, knownpos.i)
      testpos <- c(testpos, testpos.i)
      testneg <- c(testneg, testneg.i)
      true_prv <- c(true_prv, pos.i / n_clients.i)
      
      if (any(n_status > n_clients)) { break }
    }

    ancrt <- data.frame(faciluid = id, time = time_id, 
                        n_clients = n_clients,  
                        n_status = n_status,
                        knownpos = knownpos,
                        testpos = testpos,
                        testneg = testneg,
                        true_prv = true_prv)
    summary(ancrt)
    
    weighted.mean(ancrt$true_prv, w = ancrt$n_clients) - 
      weighted.mean((ancrt$knownpos + ancrt$testpos) / ancrt$n_status, w = ancrt$n_status)
    
    ancrt$obs_prv <- obs_prv <- (ancrt$knownpos + ancrt$testpos) / ancrt$n_status
    ancrt$obs_cov <- obs_cov <- ancrt$n_status / ancrt$n_clients
    par(mfrow = c(1, 1))
    plot(obs_prv ~ obs_cov, 
         pch = 16, col = rgb(100, 100, 100, 100, max = 255))
    
    
    # library(splines)
    # LM1 <- glm(cbind(I(knownpos + testpos), testneg) ~ ns(obs_cov, df = 3) + as.factor(id), 
    #            data = ancrt, family = 'binomial')
    # NewData1 <- NewData2 <- ancrt
    # NewData1$obs_cov <- ancrt$obs_cov
    # NewData2$obs_cov <- 1
    # Predict1 <- predict(LM1, NewData1, type = 'response')
    # Predict2 <- predict(LM1, NewData2, type = 'response')
    # EffectSize <- weighted.mean(Predict2, w = ancrt$n_clients) - weighted.mean(Predict1, w = ancrt$n_status)
    # print(c(weighted.mean(ancrt$true_prv, w = ancrt$n_clients) - weighted.mean((ancrt$knownpos + ancrt$testpos) / ancrt$n_status, w = ancrt$n_status),
    #         EffectSize))
    
    
    ancrt_true <- ancrt
    
#' Here we add some noise to the data to mimick data entry errors.
    row_id <- seq(1:length(ancrt$faciluid))
    to_mod <- round(0.05 * length(ancrt$faciluid), 0)
    row_id_n_status <- sample(row_id, size = to_mod, replace = FALSE)
    
    ancrt$n_status[row_id_n_status] <- ancrt$n_status[row_id_n_status] + 
      abs(ceiling(rnorm(n = to_mod, mean = 0 , sd = 0.1 * ancrt$n_status[row_id_n_status])))
    ancrt$testneg[row_id_n_status] <- ancrt$testneg[row_id_n_status] + 
      abs(ceiling(rnorm(n = to_mod, mean = 0 , sd = 0.1 * ancrt$n_status[row_id_n_status])))

#' We introduce some clerical errors               
    row_id <- seq(1:length(ancrt$faciluid))
    to_mod <- round(0.02 * length(ancrt$faciluid), 0)
    row_id_clerical <- sample(row_id, size = to_mod, replace = FALSE)
    
    ancrt$knownpos[row_id_clerical] <- ancrt$knownpos[row_id_clerical] + round(0.2 * ancrt$knownpos[row_id_clerical])
    ancrt$testpos[row_id_clerical] <- ancrt$testpos[row_id_clerical] - round(0.2 * ancrt$knownpos[row_id_clerical])
    
    ancrt <- subset(ancrt, 
                     select = c('faciluid','time','n_clients','n_status','knownpos','testpos','testneg','true_prv'))
    dim(ancrt)
    head(ancrt)
    
    ancrt_true <- subset(ancrt_true, 
                     select = c('faciluid','time','n_clients','n_status','knownpos','testpos','testneg','true_prv'))
    dim(ancrt_true)
    head(ancrt_true)   
    

    usethis::use_data(ancrt, overwrite = TRUE)
    usethis::use_data(ancrt_true, overwrite = TRUE)
    