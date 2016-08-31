rankall <- function(outcome, num = "best") {
    data <- read.csv("outcome-of-care-measures.csv",
                     na.strings = "Not Available",
                     stringsAsFactors = FALSE)
    possible_outcomes <- c("heart attack" = 11, "heart failure" = 17,
                           "pneumonia" = 23)
    if (!any(names(possible_outcomes) == outcome)) {
        stop("invalid outcome")
    } else {
        outcome_data <- data[, c(2, 7, possible_outcomes[outcome])]
        names(outcome_data) <- c("Hospital", "State", "Outcome")
        data_no_na <- na.omit(outcome_data)
        ranked_data <- data_no_na[order(data_no_na$Outcome,
                                        data_no_na$Hospital), ]

        data_for_states <- split(ranked_data, ranked_data$State)
        hospital_list <- lapply(data_for_states,
                                function(x) gethospital(x, num))
        data.frame(Hospital = unlist(hospital_list),
                   State = names(hospital_list))
    }
}

gethospital <- function(data, num) {
    if (num == "best") data[1, 1]
    else if (num == "worst") data[nrow(data), 1]
    else data[num, 1]
}