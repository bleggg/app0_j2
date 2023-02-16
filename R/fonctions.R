#' decennie_a_partir_annee
#' @param num annee 
#' @return num decennie de l'annee
decennie_a_partir_annee <- function(annee) {
  return(annee - annee %% 10)
}

#' Calcul d'une statistique agregee
#' 
#' @inheritParams mean #pas besoin de specifier les parametres x et ... car dans la fct mean
#' @param stat chr statistique a calculer (default: "moyenne", "ecart-type" 
#' ou "variance")
#' 
#' @return num la statistique agregee
#' 
#' @examples
#' calcul_stats_desc(rnorm(10))
#' calcul_stats_desc(rnorm(10), "ecart-type")
#' calcul_stats_desc(rnorm(10), "variance")
calcul_stats_desc <- function(x, stat = "moyenne", ...) {
  if (stat == "moyenne") {
    res <- mean(x, na.rm = TRUE, ...)
  } else if (stat == "ecart-type" || stat == "sd") {
    res <- sd(x, na.rm = TRUE, ...)
  } else if (stat == "variance") {
    res <- var(x, na.rm = TRUE, ...)
  }
  return(res)
}
