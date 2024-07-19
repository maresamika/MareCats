# File: R/marecats_functions.R
# Load necessary library
#library(lubridate) thats in the dependencies now

#' Kitty Treats Calculation
#'
#' This function calculates how many treats to give a cat based on the time since the last feeding.
#' The calculation is based on one treat per hour.
#'
#' @param last_fed A character string representing the last feeding time in "HH:MM" format.
#' This should be the time when the cat was last fed on the current day.
#' @return A character string suggesting the number of treats to give.
#' @examples
#' kitty_treats("11:34")
#' @export
kitty_treats <- function(last_fed) {
  # Get the current time
  current_time <- Sys.time()

  # Parse the last feeding time as today's date with the specified time
  last_fed_time <- as.POSIXlt(paste(Sys.Date(), last_fed))

  # Calculate the difference in hours
  hours_since_fed <- difftime(current_time, last_fed_time, units = "hours")

  # Calculate the number of treats
  treats <- floor(as.numeric(hours_since_fed))

  # Generate the message
  if (is.na(treats) || treats < 0) {
    message <- "Invalid feeding time."
  } else if (treats > 6) {
    message <- paste("You should feed your cat", treats, "treats. The better option would be to provide a proper meal.")
  } else {
    message <- paste("You should feed your cat", treats, "treats.")
  }

  # Return the message
  return(message)
}

#' Kitty Mood Enrichment
#'
#' This function calculates the cat's mood based on weather conditions
#' and suggests enrichment activities to improve the mood.
#'
#' @param weather_condition A character string representing the current weather.
#' Valid values include "rainy", "hot", "snowy", "cold", "sunny", "warm", "mild", "nice".
#' @param current_mood An integer of the current mood of the cat, ranging from 1 (low mood) to 10 (high mood).
#' @return A message summarizing the cat's mood and suggesting enrichment activities to potentially improve the mood.
#' @examples
#' kitty_mood_enrichment("snowy", 1)
#' @export
kitty_mood_enrichment <- function(weather_condition, current_mood) {
  # Adjust mood based on weather conditions
  if (weather_condition %in% c("rainy", "hot", "snowy", "cold")) {
    current_mood <- max(current_mood - 1, 1)  # Decrease mood by 1 for bad weather, ensure it doesn't go below 1
  } else if (weather_condition %in% c("sunny", "warm", "mild", "nice")) {
    current_mood <- min(current_mood + 1, 10)  # Increase mood by 1 for good weather, ensure it doesn't go above 10
  }

  # Enrichment suggestions based on current mood
  enrichment_suggestions <- if (current_mood <= 3) {
    c(
      "Hide treats indoors for hunting games.",
      "Engage in play sessions with feather toys or laser pointers.",
      "Create an indoor obstacle course for exploration.",
      "Provide a scratch pad."
    )
  } else if (current_mood <= 6) {
    c(
      "Create new climbing structures or hiding spots.",
      "Use puzzle feeders or interactive toys.",
      "Set up a treat dispenser play with shallow basins or toys."
    )
  } else {
    c(
      "Encourage supervised outdoor play in safe areas.",
      "Provide cozy spots indoors for relaxation."
    )
  }

  # Update mood based on enrichment suggestions length
  if (length(enrichment_suggestions) > 0) {
    current_mood <- min(current_mood + length(enrichment_suggestions), 10)  # Ensure mood doesn't exceed 10
  }

  # Generate the output message with mood and enrichment suggestions
  message <- paste("Your cat's mood could be improved to", current_mood, "if you provide these options:")
  if (length(enrichment_suggestions) > 0) {
    message <- paste(message, paste(enrichment_suggestions, collapse = "; "))
  } else {
    message <- paste(message, "No specific enrichment suggestions.")
  }

  # Return the message
  return(message)
}


