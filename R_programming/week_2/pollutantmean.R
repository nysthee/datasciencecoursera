complete <- function(directory, pollutant, id = 1:332) {
    data <- data.frame()
    for (i in id) {
        formatid <- formatC(i, width = 3, format = "d", flag = "0")
        file <- paste(formatid, ".csv", sep = "")
        path <- file.path(directory, file)
        csv <- read.csv(path)
        data_without_na <- csv[!is.na(csv[pollutant]), pollutant]
        data <- c(data, data_without_na)
    }
    mean(data)
}
