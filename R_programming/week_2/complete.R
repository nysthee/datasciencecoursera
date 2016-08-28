complete <- function(directory, id = 1:332) {
    data <- data.frame()

    for (i in id) {
        formatid <- formatC(i, width = 3, format = "d", flag = "0")
        file <- paste(formatid, ".csv", sep = "")
        path <- file.path(directory, file)
        csv <- read.csv(path)
        complete <- complete.cases(csv)
        numberofcompleterows <- nrow(csv[complete, ])
        data <- rbind(data, data.frame(id = i, nobs = numberofcompleterows))
    }
    print(data)
}
