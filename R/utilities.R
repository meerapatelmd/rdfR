#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname is_id
#' @export
is_id <-
  function(x) {

    grepl(pattern = "^http://",
          x = x)
  }



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param rdf PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{select}}
#'  \code{\link[cli]{cli_alert}}
#' @rdname label_to_id
#' @export
#' @importFrom dplyr filter select
#' @importFrom cli cli_alert_danger
label_to_id <-
  function(x,
           rdf) {


    id <-
      query_label_map(rdf = rdf) %>%
      dplyr::filter(label == x) %>%
      dplyr::select(id) %>%
      unlist() %>%
      unname()


    if (length(id) == 0) {

      cli::cli_alert_danger(text = "'{x}' is not a valid label.")
      stop()

    }

    if (length(id)>1) {

      cli::cli_alert_danger(text = "{length(id)} ids found for label '{x}': {id}")
      stop()

    }

    id

  }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname id_if_label
#' @export
id_if_label <-
  function(x,
           rdf) {
    if (!is_id(x)) {

      label_to_id(x,
                  rdf = rdf)

    } else {
      x
    }
  }
