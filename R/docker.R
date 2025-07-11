#' find the docker executable
#'
#' @export
find_docker_cmd <- function() {
  docker_cmd <- Sys.which('docker')
  if (length(docker_cmd) == 0) stop(paste('\n','Docker command not found. ','\n',
                                          'Please install docker: ','\n',
                                          'https://www.docker.com/products/overview#/install_the_platform'))
  docker_check <- suppressWarnings(system2(docker_cmd,'ps',stderr=TRUE,stdout=TRUE))
  if(!is.null(attr(docker_check,'status'))) stop(paste0('Cannot connect to the Docker daemon. ',
                                                        'Is the docker daemon running on this host?'))
  return(docker_cmd)
}

#' start geocoding container
#'
#' Beginning with v2.2, the latest version of the geocoder image is available
#' on the project GitHub repository:
#' <https://github.com/degauss-org/geocoder/pkgs/container/geocoder>
#'
#' @param image_name Name of geocoding image; can be used to specify the version. Defaults to `getOption( "offlinegeocoder.image", "ghcr.io/degauss-org/geocoder:v3.4.0")`.
#' @export
start_geocoder_container <- function(
  image_name = getOption(
    "offlinegeocoder.image",
    "ghcr.io/degauss-org/geocoder:v3.4.0"
  )
) {
  message('starting geocoding container...')
  docker_cmd <- find_docker_cmd()
  system2(
    docker_cmd,
    args = c(
      'run',
      '-it',
      '-d',
      '--name gs',
      '--entrypoint /bin/bash',
      image_name
    ),
    stdout = NULL
  )
  message('and loading address range database...')
}

#' stop geocoding container
#'
#' @export
stop_geocoder_container <- function() {
  docker_cmd <- find_docker_cmd()
  message('stopping geocoding container...')
  system2(docker_cmd,
          args = c('stop','gs'),
          stdout = NULL)
  system2(docker_cmd,
          args = c('rm','gs'),
          stdout = NULL)
}

#' call a running geocoding container to geocode an address
#'
#' This is used internally by `geocode()` and normally should not be called directly.
#'
#' @param address a string; see vignette for best practices and examples
#' @param version Version number. Defaults to `getOption("offlinegeocoder.version", "3.4.0")`. Version must match version specified by `image_name` parameter
#' @export
#' @importFrom jsonlite fromJSON
gc_call <- function(
  address,
  version = getOption("offlinegeocoder.version", "3.4.0")
) {
  docker_cmd <- find_docker_cmd()

  geocode_rb <- "/app/geocode.rb"
  if (version <= "3.1.0") {
    geocode_rb <- "/root/geocoder/geocode.rb"
  }

  docker_out <- system2(
    docker_cmd,
    args = c(
      "exec",
      "gs",
      "ruby",
      geocode_rb,
      shQuote(address)
    ),
    stderr = FALSE,
    stdout = TRUE
  )

  out <- jsonlite::fromJSON(docker_out)

  out$fips_county <- NULL

  if (length(out) == 0) {
    out <- data.frame(
      street = NA,
      zip = NA,
      city = NA,
      state = NA,
      lat = NA,
      lon = NA,
      score = NA,
      # TODO: Check if prenum and number were added in a later version
      prenum = NA,
      number = NA,
      precision = NA
    )
  }

  out
}
