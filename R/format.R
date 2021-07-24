#' @title
#' Format a URI
#' @description
#' Strict formatting guidelines are applied to any URI to prevent errors at the R-Protege interface.
#' Replaces any punctuation of length one or more with a dash and only keeps characters with alphanumeric characters or a dash.
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_replace}},\code{\link[stringr]{str_remove}}
#' @rdname uri_format
#' @export
#' @importFrom stringr str_replace_all str_remove_all
uri_format <-
  function(x) {

    x <-
      stringr::str_replace_all(x,
                               pattern = "[[:punct:]]{1,}",
                               replacement = "-")
    stringr::str_remove_all(x,
                            pattern = "[^A-Za-z0-9-]")

  }
