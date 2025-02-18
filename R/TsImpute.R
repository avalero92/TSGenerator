
#' TsImpute
#'
#' @import imputeTS
#' @import dplyr
#'
#' @name TsImpute
#'
#' @param data
#' data frame where VI time series containing missing data are stored.
#' @param group_col
#' column by which the grouping is to be performed (example: ID column)
#' @param value_col
#' column where the time series of the VI data is stored (example: NDVI)
#'
#' @return
#' Un nuevo archivo similar al data frame original pero con una nueva columna donde se alacenan los datos imputados
#' @export
#'
#' @examples
#' #Ejemplo de aplicación de la función TsImpute
#' new_Data <- TsImpute(df, group_col = "ID", value_col = "NDVI")


TsImpute <- function(data, group_col, value_col) {

  library(dplyr)
  library(imputeTS)

  # Check if the data is a data.frame
  if (!is.data.frame(data)) {
    warning("The argument 'data' must be a data.frame.")
    return(NULL)
  }

  # Check if columns exist in the data.frame
  if (!group_col %in% names(data)) {
    warning(paste("The group column", group_col, "does not exist in the data.frame."))
    return(NULL)
  }

  if (!value_col %in% names(data)) {
    warning(paste("The value column", value_col, "does not exist in the data.frame."))
    return(NULL)
  }

  # Make the allocation
  data %>%
    group_by(!!sym(group_col)) %>%
    mutate(!!paste0(value_col, "_completed") := na_kalman(!!sym(value_col),
                                                          model = "StructTS",
                                                          smooth = TRUE,
                                                          type = c("level", "trend", "BSM"),
                                                          optim.control = "L-BFGS-B")) %>%
    ungroup()
}
