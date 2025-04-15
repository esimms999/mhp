#' Simulate the Monty Hall Problem
#'
#' Runs a simulation of the Monty Hall problem with a specified number of doors.
#'
#' @param n_doors Number of doors (>= 3).
#' @param n_simulations Number of simulations.
#' @return A list with sticking and switching win rates.
#' @examples
#' monty_hall_sim(3, 100)
#' @export
monty_hall_sim <- function(n_doors, n_simulations) {
  if (n_doors < 3) stop("n_doors must be >= 3")

  results <- replicate(n_simulations, {
    doors <- c(1, rep(0, n_doors - 1))
    doors <- sample(doors)
    player_choice <- sample(1:n_doors, 1)
    all_doors <- 1:n_doors
    goat_doors <- which(doors == 0)
    monty_options <- setdiff(goat_doors, player_choice)

    if (doors[player_choice] == 0) {
      monty_opens <- monty_options
    } else {
      n_to_open <- length(monty_options) - 1
      monty_opens <- if (n_to_open > 0) {
        monty_options[1:n_to_open]
      } else {
        integer(0)
      }
    }

    remaining_door <- setdiff(all_doors, c(player_choice, monty_opens))
    c(stick = doors[player_choice], switch = doors[remaining_door])
  })

  list(
    stick = mean(results["stick", ]),
    switch = mean(results["switch", ])
  )
}
