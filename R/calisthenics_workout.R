#' Award a prize for every successful calisthenics workout
#' 
#' This function takes in the number of reps done per a given set of 
#' calisthenic related workouts and awards points
#' modelled in tiers and finally awards a prize to the user
#' based on the difficulty and the number of reps completed
#' 
#' @param reps Number of reps/rounds of combination of exercises completed
#' @param difficulty Named numeric vector giving the exercises and their 
#' counts, harder moves/combos earn more points per rep
#' @param announce if TRUE print a celebratory summary to the console
#' @return invisibly, a list with total `points`, the `tier`, and a `prize`
#' and `per_round` (points earned in one round of the combo)
#' @importFrom rlang abort
#' @importFrom cli cli_h1 cli_alert_success cli_text
#' @examples
#' workout_reward(reps = 4, difficulty = c(burpees = 30, pullups = 15))
#' @export

workout_reward <- function(
    reps, 
    difficulty = c(pushups = 20, squats = 30,
      pullups = 10, burpees = 20), 
    announce = TRUE) {
  
  if (!is.numeric(reps) || length(reps) != 1) {
    rlang::abort("`reps` must be a single number, e.g. 7")
  }

  if (reps < 0) {
    rlang::abort("`reps` cannot be negative, haha, nice try. Let's gooo")
  }
  
  if (!rlang::is_named(difficulty) || !is.numeric(difficulty)) {
    rlang::abort("`difficulty` must be a named numeric vector")
  }

  # points in 1 round = sum of each exercise's count, weighed by how hard
  # that exercise is, by default unknown exercises count as 1
  hardness <- c(pushups = 1,  squats = 1, burpees = 2, situps = 1,
    plank = 0.5, leg_raises = 1, pullups = 3, dips = 2)
  weight <- ifelse(names(difficulty) %in% names(hardness),
    hardness[names(difficulty)], 1)
  per_round <- sum(difficulty * weight)
  points <- reps * per_round

  tiers <- c("LazyCouch", "Bronze", "Silver", "Gold", "Platinum")
  tier <- tiers[findInterval(points, c(1, 200, 500, 900)) + 1]

  prizes <- c(
    LazyCouch = "Maybe Tomorrow?", 
    Bronze = "You, you get a high five buddy", 
    Silver = "You deserve a long, hot shower and bed rest",
    Gold = "A nice extra smooth protein shake, on the house",
    Platinum = "Nani huyo ameiva hivo? Dinner @Kempinski tonight"
  )
  
  prize <- unname(prizes[tier])

  if (announce) {
    cli::cli_h1("Workout Complete, hooray!")
    cli::cli_alert_success("You locked in today {reps} rounds = {points} points -- {tier} tier")
    cli::cli_text("Your prize, drumrolls please: {prize}")
  }
  invisible(list(points = points, tier = tier, prize = prize, per_round = per_round))
  
}