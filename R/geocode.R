#' geocode
#'
#' To prevent errors and optimize accuracy, please remove non alphanumeric
#' characters from address strings prior to geocoding. In general, address
#' cleaning is outside the scope of this package and should be completed prior
#' to geocoding.
#'
#' By default, geocoding results will be cached in a local folder at the path
#' specified by `cache_name`.  See help for [mappp::mappp()] to change these
#' defaults.
#'
#' @param addresses a list or vector addresses
#' @param cache_name Name or path for a custom cache directory. Defaults to
#' `tempdir()`.
#' @param ... additional arguments passed to [mappp::mappp()]; set options for
#'   cache here
#' @return list of geocoding results with one address per element; some
#'   addresses may return more than one geocoding result if there is a tie among
#'   the best matches
#' @export
geocode <- function(
  addresses,
  ...,
  cache_name = tempdir()
) {
  start_geocoder_container()
  on.exit(stop_geocoder_container())
  message('now geocoding...')
  out <- mappp::mappp(addresses, gc_call, cache = TRUE, cache_name = cache_name, ...)

  do.call(rbind, out)
}
