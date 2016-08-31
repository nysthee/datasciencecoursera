rankhospital <- function(state, outcome, num = "best") {
    data <- read.csv("outcome-of-care-measures.csv",
                     na.strings = "Not Available",
                     stringsAsFactors = FALSE)
    possible_states <- unique(data$State)
    possible_outcomes <- c("heart attack" = 11, "heart failure" = 17,
                           "pneumonia" = 23)
    if (!any(possible_states == state)) {
        stop("invalid state")
    } else if (!any(names(possible_outcomes) == outcome)) {
        stop("invalid outcome")
    } else {
        outcome_data <- data[, c(2, 7, possible_outcomes[outcome])]
        names(outcome_data) <- c("Hospital", "State", "Outcome")
        data_for_state <- subset(outcome_data, State == state)
        data_no_na <- na.omit(data_for_state)
        ranked_data <- data_no_na[order(data_no_na$Outcome,
                                         data_no_na$Hospital), ]

        if (num == "best") ranked_data[1, 1]
        else if (num == "worst") ranked_data[nrow(ranked_data), 1]
        else ranked_data[num, 1]
    }
}