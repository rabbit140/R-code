rankhospital <- function(state, outcome, num = "best") {
      
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
            order <- order(mortality_state$mortality_vector, partial = mortality_state$outcome_data.Hospital.Name)
            mortality_state <- mortality_state[order, ]
            rank <- c(1:length(mortality_state$mortality_vector))
            mortality_state <- cbind(mortality_state, rank)
            
            if (num > length(mortality_state$outcome_data.Hospital.Name) & !is.character(num)) {
                  return(NA)
            }
            
            if (num == "best") {
                  
                  print(as.character((subset(mortality_state, mortality_state$mortality_vector == (min(mortality_state$mortality_vector))))$outcome_data.Hospital.Name))
                  
            }
            
            else if (num == "worst") {
                  mortality_max <- max(mortality_state$mortality_vector)
                  final <- subset(mortality_state, mortality_state$mortality_vector == mortality_max)
                  final_name <- final$outcome_data.Hospital.Name
                  final_name <- as.character(final_name)
                  print(final_name)
            }
            
            if (num != "best" & num != "worst") {
                  
                  final <- subset(mortality_state, mortality_state$rank %in% num)
                  final_name <- final$outcome_data.Hospital.Name
                  final_name <- as.character(final_name)
                  print(final_name)
            }
            
            
            
            
      }
      
      else if (outcome == "heart failure") {
            outcome_data_subset <- data.frame(outcome_data$Hospital.Name, outcome_data$State,
                                              outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
            nona_data <- subset(outcome_data_subset, outcome_data_subset$outcome_data.Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available")
            mortality_vector <- as.vector(nona_data$outcome_data.Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
            mortality_vector <- as.numeric(mortality_vector)
            nona_data_mortality <- cbind(nona_data, mortality_vector)
            mortality_state <- subset(nona_data_mortality, nona_data_mortality$outcome_data.State == state)
            order <- order(mortality_state$mortality_vector, partial = mortality_state$outcome_data.Hospital.Name)
            mortality_state <- mortality_state[order, ]
            rank <- c(1:length(mortality_state$mortality_vector))
            mortality_state <- cbind(mortality_state, rank)
            
            if (num > length(mortality_state$outcome_data.Hospital.Name) & !is.character(num)) {
                  return(NA)
            }
            
            if (num == "best") {
                  mortality_min <- min(mortality_state$mortality_vector)
                  final <- subset(mortality_state, mortality_state$mortality_vector == mortality_min)
                  final_name <- final$outcome_data.Hospital.Name
                  final_name <- as.character(final_name)
                  print(final_name)
            }
            
            else if (num == "worst") {
                  mortality_max <- max(mortality_state$mortality_vector)
                  final <- subset(mortality_state, mortality_state$mortality_vector == mortality_max)
                  final_name <- final$outcome_data.Hospital.Name
                  final_name <- as.character(final_name)
                  print(final_name)
            }
            
            
            final <- subset(mortality_state, mortality_state$rank %in% num)
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
            order <- order(mortality_state$mortality_vector, partial = mortality_state$outcome_data.Hospital.Name)
            mortality_state <- mortality_state[order, ]
            rank <- c(1:length(mortality_state$mortality_vector))
            mortality_state <- cbind(mortality_state, rank)
            
            if (num > length(mortality_state$outcome_data.Hospital.Name) & !is.character(num)) {
                  return(NA)
            }
            
            if (num == "best") {
                  mortality_min <- min(mortality_state$mortality_vector)
                  final <- subset(mortality_state, mortality_state$mortality_vector == mortality_min)
                  final_name <- final$outcome_data.Hospital.Name
                  final_name <- as.character(final_name)
                  print(final_name)
            }
            
            else if (num == "worst") {
                  mortality_max <- max(mortality_state$mortality_vector)
                  final <- subset(mortality_state, mortality_state$mortality_vector == mortality_max)
                  final_name <- final$outcome_data.Hospital.Name
                  final_name <- as.character(final_name)
                  print(final_name)
            }
            
            
            final <- subset(mortality_state, mortality_state$rank %in% num)
            final_name <- final$outcome_data.Hospital.Name
            final_name <- as.character(final_name)
            print(final_name)
      }
}