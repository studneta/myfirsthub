#' Function for Project 1
#' @description Function created for Project 1, making a scatterplot
#'
#' @param df data frame
#' @param SPECIES the species being filtered
#'
#' @return plot, csv file, data, and table
#' @importFrom dplyr '%>%' filter
#' @importFrom ggplot2 aes geom_smooth geom_point ggtitle
#'
#' @export
#'
#' @examples
#' \dontrun{obj <- myddt(df = ddt, SPECIES = "CCATFISH")}

myddt <- function(df, SPECIES)
{

  WEIGHT <- LENGTH <- RIVER <- NULL

  newdf <- df %>% filter(SPECIES == {{SPECIES}})

  g = ggplot(newdf, aes(x = WEIGHT, y = LENGTH)) + geom_point(aes(col=RIVER)) +
    geom_smooth(formula = y~x +I(x^2), method = "lm") + ggtitle("Audrey Hummell")

  print(g)

  write.csv(x = newdf,
            file = paste0("LvsWfor", SPECIES, ".csv" ),
            row.names = FALSE
            )

  list = list(df = df, newdf = newdf, tab = table(df$RIVER)/length(df$RIVER))

  return(list)

}
