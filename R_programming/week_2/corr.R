corr <- function(directory, threshold = 0) {
    data <- c()

    for (file in list.files(directory)) {
        path <- file.path(directory, file)
        csv <- read.csv(path)
        complete <- csv[complete.cases(csv), ]
        if (nrow(complete) > threshold) {
            cor_data <- cor(complete["sulfate"], complete["nitrate"])
            data <- c(data, cor_data)
        }
    }
    data
}
