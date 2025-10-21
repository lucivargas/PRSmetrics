#' Title: Function to calculate odds ratios given quantiles of PGS distribution
#'
#' This function calculates odds ratios of a binary outcome across quantiles of a PGS distribution
#'
#' @param pgs Numeric vector. The total PGS value per individual.
#' @param outcome Factor vector. A binary vector encoding the outcome (e.g., case = 1 and control = 0).
#' @param pgs_quantiles List. A list specifying all intervals of the PGS quantiles to contrast. Values must span from 0 to 1. Defaults to deciles (0.1 increments).
#' @param create_plot Logical. A logical indicating whether to generate a plot of the OR point estimates and intervals. Defaults to false.
#' @return Results: A tibble with odds ratios, confidence intervals and p-values for each PGS quantile when compared to the baseline (lowest quantile).
#' @return Quantiles: A list of the PGS quantiles used.
#' @return Plot: A ggplot object showing odds ratios by PGS quantile
#' @examples
#' set.seed(123)
#' n <- 10000
#' pgs <- rnorm(n, mean = 0, sd = 4)
#' beta <- 0.3
#' mu <- qlogis(0.3)
#' prob_case <- plogis(mu + beta * pgs)
#' outcome <- rbinom(n, size = 1, prob = prob_case)
#' calc_or(pgs, outcome)
#' @importFrom stats quantile
#' @importFrom epitools oddsratio
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble mutate select as_tibble
#' @importFrom ggplot2 ggplot aes geom_linerange geom_pointrange labs theme_classic position_dodge
#' @export
calc_or <- function(pgs,
                    outcome,
                    pgs_quantiles=seq(0, 1, 0.1),
                    create_plot=T){

  # Ensure input variables are appropriately formatted
  all( pgs_quantiles >= 0 | pgs_quantiles <= 1  ) # PGS values must span from 0 to 1
  pgs <- as.numeric(pgs)
  outcome <- as.factor(outcome)

  # Create tibble
  df <- tibble(pgs=pgs,
               outcome=outcome)

  # Create table to store OR results
  quantile_lower <- pgs_quantiles[-length(pgs_quantiles)]
  quantile_upper <- pgs_quantiles[-1]
  or_results <- data.frame(quantile=1:length(pgs_quantiles[-1]),
                           lower_bound=quantile_lower,
                      upper_bound=quantile_upper,
                      cutoff1=quantile(unlist( pgs ), probs=quantile_lower),
                      cutoff2=quantile(unlist( pgs ), probs=quantile_upper),
                      OR=NA,
                      OR_ci1=NA,
                      OR_ci2=NA,
                      pval=NA,
                      stringsAsFactors=F)
  row.names(or_results) <- 1:nrow(or_results)

  # Add OR of 1 to the lower quantile (baseline)
  or_results$OR[1] <- 1

  for (i in 2:nrow(or_results)){

    # Define cutoff interval for the baseline
    ref_quantile_cutoff <- or_results$cutoff2[1]

    # Define cutoff interval to get OR from (test)
    test_quantile_cutoffs <- c(or_results$cutoff1[i],or_results$cutoff2[i])

    # Flag individuals in baseline and testing pgs cutoffs
    pgs_baseline <- pgs <= ref_quantile_cutoff
    pgs_test <- pgs > test_quantile_cutoffs[1] & pgs <= test_quantile_cutoffs[2]

    # Recode outcome variables from 0/1 to T/F
    logical_outcome <- ifelse(outcome == 1, T, F)

    # Create table with pgs_baseline/pgs_test vs. case/control
    or_matrix <- matrix(c(sum(pgs_baseline & !logical_outcome), sum(pgs_test & !logical_outcome), # no disease
                          sum(pgs_baseline & logical_outcome), sum(pgs_test & logical_outcome)),  # with disease
                        nrow=2, ncol=2, byrow=TRUE)
    dimnames(or_matrix) <- list('Predictor'=c("pgs_baseline", "pgs_test"),
                            'Outcome'=c("neg", "pos"))

    # If any cell count is 0, return warning
    if(any(or_matrix==0)){
      cat("Warning: Cannot compute OR with 0 observations")
      next
    }

    # Estimate OR for quantile
    or_quantile <- oddsratio(or_matrix)

    # Format data
    or_quantile <- cbind(or_quantile$measure, or_quantile$p.value)
    or_quantile <- as.data.frame(or_quantile)

    # Add to results
    or_results$OR[i] <- or_quantile["pgs_test", "estimate"]
    or_results$OR_ci1[i] <- or_quantile["pgs_test", "lower"]
    or_results$OR_ci2[i] <- or_quantile["pgs_test", "upper"]
    or_results$pval[i] <- or_quantile["pgs_test", "chi.square"]
  }

  # Plot results
  if(create_plot){
    or_plot <- ggplot2::ggplot(or_results,
                               aes(x = or_results$quantile, y = or_results$OR)) +
      ggplot2::geom_linerange(aes(ymin = or_results$OR_ci1, ymax = or_results$OR_ci2),
                              lwd = 1/2, position = position_dodge(width = 1/2)) +
      ggplot2::geom_pointrange(aes(ymin = or_results$OR_ci1, ymax = or_results$OR_ci2),
                               shape = 21, lwd = 1/2, position = position_dodge(width = 1/2), fill = "white") +
      labs(x="Quantiles", y="Odds Ratio") +
      theme_classic()
    print(or_plot)
  } else{
    or_plot <- NULL
  }

  # Format OR results
  or_results <- as_tibble(or_results) %>%
    dplyr::select(dplyr::all_of(c("quantile", "OR", "OR_ci1", "OR_ci2", "pval")))

  # Create object to return results
  res <- list(results = or_results,
              quantiles = pgs_quantiles,
              plot  = or_plot)

  # Return results
  return(res)
}

