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
#' @inheritParams start_geocoder_container
#' @inheritParams gc_call
#' @param cache passed to [mappp::mappp()]. If `TRUE`, cache results locally
#' using `{memoise}` in the folder specified by `cache_name`. Defaults to
#' `FALSE`.
#' @param cache_name Name or path for a custom cache directory. Defaults to
#' `tempdir()`.
#' @param as_data_frame If `TRUE`, return output as a data frame instead of a list.
#' @param ... additional arguments passed to [mappp::mappp()]; set options for
#'   cache here
#' @return list of geocoding results with one address per element; some
#'   addresses may return more than one geocoding result if there is a tie among
#'   the best matches
#' @export
geocode <- function(
  addresses,
  ...,

  as_data_frame = FALSE,
  image_name = getOption(
    "offlinegeocoder.image",
    "ghcr.io/degauss-org/geocoder:v3.4.0"
  ),
  version = getOption("offlinegeocoder.version", "3.4.0"),
  cache = FALSE,
  cache_name = tempdir()
) {
  start_geocoder_container(
    image_name = image_name
  )

  on.exit(stop_geocoder_container())
  message('now geocoding...')
  out <- mappp::mappp(
    addresses,
    function(x) {
      gc_call(x, version = version)
    },
    cache = cache,
    cache_name = cache_name,
    ...
  )

  if (!as_data_frame) {
    return(out)
  }

  do.call(rbind, out)
}
