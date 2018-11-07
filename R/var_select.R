#' Select the 'best' variables to include in a linear model
#'
#' This is an implementation of a simple algorithm that selects the 'best' variables
#' as the predictors in a linear regression model. The selection is based on
#' choosing the variables that give the highest adjusted R-squared value.
#'
#' @param data A data frame containing the predictor variables (X) and the dependent variable (Y).
#' @param y The name of the dependent variable.
#' @param stopat The total number of variables to include in the model.
#'  Default is `Inf` which means using all the variables in the data frame.
#'
#' @return A nested tibble with the lm models and stats.
#' @export
#'


var_select <- function(data, y, stopat = Inf) {
  # initial
  # catch error when all the predictors are NA
  cor.result <- tryCatch({

    cor(data[[y]], data[, -which(colnames(data) == y)],
        use = "complete.obs")

  }, error = function(e) {

    warning("No valid data. An empty dataframe is returned.", call. = FALSE)

    # return NULL
    return(NULL)

  })

  # return empty data frame if no valid data.
  if (is.null(cor.result) | all(is.na(cor.result))) return(tibble())

  # get stats for the starting point
  vars.model <- colnames(cor.result)[which.max(cor.result)]
  vars.remaining <- colnames(data)[!colnames(data) %in% c(y, vars.model)]
  start.form <- as.formula(paste(y, vars.model, sep = "~"))
  start.model <- lm(start.form, data)
  adj.rsq <- summary(start.model)$adj.r.squared
  mse <- mean(summary(start.model)$residuals^2)
  rsq <- summary(start.model)$r.squared

  # building the output of the data frame
  form.list <- c(start.form)
  model.list <- list()
  model.list[[1]] <- start.model

  # looping through remaing variables
  while (length(vars.remaining) > 0 & length(model.list) < stopat) {

    new.rsq <- purrr::map_dbl(vars.remaining,
                              function (x) {
                                vars.test <- paste(vars.model, x, sep="+")
                                fit <- lm(as.formula(paste(y, vars.test, sep="~")), data)
                                new.rsq <- summary(fit)$adj.r.squared
                              })
    vars.model <- paste(vars.model, vars.remaining[which.max(new.rsq)], sep = "+")
    new.form <- as.formula(paste(y, vars.model, sep = "~"))
    new.model <- lm(new.form, data)

    i <- stringr::str_split_fixed(vars.model, "\\+", n = Inf) %>% ncol()
    form.list <- c(form.list, new.form)
    model.list[[i]] <- new.model
    adj.rsq <- c(adj.rsq, summary(new.model)$adj.r.squared)
    mse <- c(mse, mean(summary(new.model)$residuals^2))
    rsq <- c(rsq, summary(new.model)$r.squared)

    vars.remaining <- vars.remaining[-which.max(new.rsq)]

  }

  # return
  tibble(form = form.list,
         model = model.list,
         adj.rsq = adj.rsq,
         mse = mse,
         rsq = rsq)

}
