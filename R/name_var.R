

name_var <- function(data = NULL, faciluid = "faciluid", time = "time", n_clients = "n_clients",
                     n_status = "n_status", knownpos = "knownpos", testpos = "testpos",
                     testneg = "testneg", age = "age", ...) {
  
  # verifying inputs
  if (is.null(data)) { stop('Provide input data') }
  if (is.null(faciluid)) { stop('Provide name of faciluid variable') }
  if (is.null(time)) { stop('Provide name of time variable') }
  if (is.null(n_clients)) { stop('Provide name of n_clients variable') }
  if (is.null(n_status)) { stop('Provide name of n_status variable') }
  if (is.null(knownpos)) { stop('Provide name of knownpos variable') }
  if (is.null(testpos)) { stop('Provide name of testpos variable') }
  if (is.null(testneg)) { stop('Provide name of testneg variable') }
  if (is.null(age)) { stop('Provide name of age variable') }

  # second verification
  if (length(names(data)[names(data) == faciluid]) == 0) { stop("faciluid's name not recognized in data") }
  if (length(names(data)[names(data) == time]) == 0) { stop("time's name not recognized in data") }
  if (length(names(data)[names(data) == n_clients]) == 0) { stop("n_clients's name not recognized in data") }
  if (length(names(data)[names(data) == n_status]) == 0) { stop("n_status's name not recognized in data") }
  if (length(names(data)[names(data) == knownpos]) == 0) { stop("knownpos's name not recognized in data") }
  if (length(names(data)[names(data) == testpos]) == 0) { stop("tespos's name not recognized in data") }
  if (length(names(data)[names(data) == testneg]) == 0) { stop("testneg's name not recognized in data") }
  
  # chaging names
  names(data)[names(data) == faciluid] <- "faciluid"
  names(data)[names(data) == time] <- "time"
  names(data)[names(data) == n_clients] <- "n_clients"
  names(data)[names(data) == n_status] <- "n_status"
  names(data)[names(data) == knownpos] <- "knownpos"
  names(data)[names(data) == testpos] <- "testpos"
  names(data)[names(data) == testneg] <- "testneg"
  names(data)[names(data) == age] <- "age"
  
  return(data)
}
