best <- function(state, outcome) {
      
      outcome_data <- read.csv("outcome-of-care-measures.csv")
      states <- unique(outcome_data$State)
      outcomes <- c("heart attack", "heart failure", "pneumonia")
      
      if (sum(state == states) == 0) {
            stop("invalid state")
      }
      
      else if (sum(outcome == outcomes) == 0) {
            stop("invalid outcome")
      }
      
      if (outcome == "heart attack") {
            outcome_data_subset <- data.frame(outcome_data$Hospital.Name, outcome_data$State,
                                              outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
            nona_data <- subset(outcome_data_subset, outcome_data_subset$outcome_data.Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available")
            mortality_vector <- as.vector(nona_data$outcome_data.Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
            mortality_vector <- as.numeric(mortality_vector)
            nona_data_mortality <- cbind(nona_data, mortality_vector)
            mortality_state <- subset(nona_data_mortality, nona_data_mortality$outcome_data.State == state)
            mortality_min <- min(mortality_state$mortality_vector)
            final <- subset(mortality_state, mortality_state$mortality_vector == mortality_min)
            final_name <- final$outcome_data.Hospital.Name
            final_name <- as.character(final_name)
            print(final_name)
      
            
      }
      
      else if (outcome == "heart failure") {
            outcome_data_subset <- data.frame(outcome_data$Hospital.Name, outcome_data$State,
                                              outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
            nona_data <- subset(outcome_data_subset, outcome_data_subset$outcome_data.Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available")
            mortality_vector <- as.vector(nona_data$outcome_data.Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
            mortality_vector <- as.numeric(mortality_vector)
            nona_data_mortality <- cbind(nona_data, mortality_vector)
            mortality_state <- subset(nona_data_mortality, nona_data_mortality$outcome_data.State == state)
            mortality_min <- min(mortality_state$mortality_vector)
            final <- subset(mortality_state, mortality_state$mortality_vector == mortality_min)
            final_name <- final$outcome_data.Hospital.Name
            final_name <- as.character(final_name)
            print(final_name)
      }
      
      else if (outcome == "pneumonia") {
            outcome_data_subset <- data.frame(outcome_data$Hospital.Name, outcome_data$State,
                                              outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
            nona_data <- subset(outcome_data_subset, outcome_data_subset$outcome_data.Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available")
            mortality_vector <- as.vector(nona_data$outcome_data.Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
            mortality_vector <- as.numeric(mortality_vector)
            nona_data_mortality <- cbind(nona_data, mortality_vector)
            mortality_state <- subset(nona_data_mortality, nona_data_mortality$outcome_data.State == state)
            mortality_min <- min(mortality_state$mortality_vector)
            final <- subset(mortality_state, mortality_state$mortality_vector == mortality_min)
            final_name <- final$outcome_data.Hospital.Name
            final_name <- as.character(final_name)
            print(final_name)
      }
      
}