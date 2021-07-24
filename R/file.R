#' @title
#' Write RDF
#'
#' @description
#' Writes RDF exclusively as a Turtle file because it is the only format that
#' Protege properly loads when written via R.
#'
#' @seealso
#'  \code{\link[cli]{character(0)}}
#'  \code{\link[rdflib]{rdf_serialize}}
#' @rdname write_rdf
#' @export
#' @importFrom cli cli_abort
#' @importFrom rdflib rdf_serialize
write_rdf <-
  function(rdf,
           doc) {

    if (class(rdf_object) != "rdf") {
      cli::cli_abort("`rdf_object` is not rdf!")
    }

    rdflib::rdf_serialize(
      rdf = rdf_object,
      doc = doc,
      format = "turtle"
    )
  }

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param doc PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname open_in_protege
#' @export
open_in_protege <-
  function(doc) {
    open_in_app <-
      function(app,
               file) {

        file <- path.expand(file)
        x <- gsub(" ", "\\ ", file, fixed = TRUE)
        x <- gsub("(", "\\(", x, fixed = TRUE)
        x <- gsub(")", "\\)", x, fixed = TRUE)

        system(sprintf("open -a '%s' %s",
                       app,
                       x))
      }

    open_in_app(app = "Protégé",
                file = doc)
  }

#' @title
#' Is the RDF File Empty?
#' @description
#' Check if a given file has 1 or more triplets.
#' @param doc PARAM_DESCRIPTION
#' @param format PARAM_DESCRIPTION, Default: c("rdfxml", "nquads", "ntriples", "turtle", "jsonld")
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname is_empty_document
#' @export
is_empty_document <-
  function(doc, format = c("rdfxml", "nquads", "ntriples", "turtle", "jsonld")) {

    format <-
      match.arg(format,
                several.ok = FALSE)
    out <-
      capture.output(
        rdf_parse(
          doc = doc,
          format = format
        )
      )

    empty_document_msg <-
      c('Total of 0 triples, stored in hashes',
        '-------------------------------',
        '')

    if (length(out)==length(empty_document_msg)) {
      all(out == empty_document_msg)
    } else {
      FALSE
    }
  }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param doc PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rdflib]{rdf_parse}}
#' @rdname read_rdf_all_fmts
#' @export
#' @importFrom rdflib rdf_parse
read_rdf_all_fmts <-
  function(doc) {

    formats <-
      c("rdfxml", "turtle", "jsonld", "nquads", "ntriples")

    output <-
      vector(mode = "list",
             length = length(formats))
    names(output) <-
      formats

    for (format in formats) {

      output[[format]] <-
        tryCatch(
          rdflib::rdf_parse(doc = doc,
                            format = format),
          error = function(e) NULL
        )
    }

    output


  }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param doc PARAM_DESCRIPTION
#' @param formats PARAM_DESCRIPTION, Default: c("rdfxml", "turtle")
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rdflib]{rdf_parse}}
#'  \code{\link[cli]{cli_alert}}
#' @rdname read_rdf_fmts
#' @export
#' @importFrom rdflib rdf_parse
#' @importFrom cli cli_alert_warning
read_rdf_fmts <-
  function(doc,
           formats = c("rdfxml", "turtle")) {

    output <-
      vector(mode = "list",
             length = length(formats))
    names(output) <-
      formats

    for (format in formats) {

      output[[format]] <-
        tryCatch(
          rdflib::rdf_parse(doc = doc,
                            format = format),
          error = function(e) NULL
        )
    }

    invalid_formats <-
      formats[!(formats %in% names(output))]

    if (length(invalid_formats)>0) {

      cli::cli_alert_warning("The following formats did not parse and are not in the output: {invalid_formats}.")

    }

    output


  }



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param doc PARAM_DESCRIPTION
#' @param formats PARAM_DESCRIPTION, Default: c("rdfxml", "turtle")
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{safely}},\code{\link[purrr]{map}}
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[utils]{capture.output}}
#'  \code{\link[huxtable]{huxtable}},\code{\link[huxtable]{themes}},\code{\link[huxtable]{na_string}},\code{\link[huxtable]{number_format}},\code{\link[huxtable]{fmt_pretty}}
#'  \code{\link[cli]{character(0)}},\code{\link[cli]{cli_alert}}
#'  \code{\link[rdflib]{rdf_parse}}
#' @rdname read_rdf
#' @export
#' @importFrom purrr quietly map
#' @importFrom tibble tibble
#' @importFrom utils capture.output
#' @importFrom huxtable hux theme_article set_na_string set_number_format fmt_pretty
#' @importFrom cli cli_abort cli_alert_info cli_alert_success
#' @importFrom rdflib rdf_parse
read_rdf <-
  function(doc,
           formats = c("rdfxml", "turtle")) {

    quietly.rdf_as_df <-
      purrr::quietly(rdf_as_df)

    all_formats <-
      read_rdf_fmts(doc = doc,
                    formats = formats) %>%
      purrr::map(quietly.rdf_as_df) %>%
      purrr::map(pluck, "result") %>%
      purrr::map(nrow) %>%
      purrr::map(~ifelse(is.null(.), 0, .)) %>%
      unlist()

    all_formats_df <-
      tibble::tibble(
        format = names(all_formats),
        triplets = all_formats
      )

    all_formats_ht <-
      utils::capture.output(
        huxtable::hux(all_formats_df) %>%
          huxtable::theme_article() %>%
          huxtable::set_na_string(value = "(Invalid)") %>%
          huxtable::set_number_format(value = huxtable::fmt_pretty())
      )
    all_formats_ht <-
      all_formats_ht[-length(all_formats_ht)]

    cat("",
        all_formats_ht,
        sep = "\n")

    max_format <- max(all_formats,
                      na.rm = TRUE)

    # If the maximum format returns 0 rows, meaning nothing parsed
    if (max_format == 0) {
      cli::cli_abort("0 triplets were returned for formats: {formats}.")
    }


    format <- all_formats[all_formats %in% max_format]

    # In case of ties, the first format in the argument vector is used
    if (length(format)>1) {

      cli::cli_alert_info("Multiple formats parsed to {max_format} triplets: {format}. {format[1]} will be used for parsing...")
      format <- format[1]
    }



    format <- names(format)
    cli::cli_alert_info(text = "Parsing {format}...")
    on.exit(
      cli::cli_alert_success("Parsing {format}... complete!")
    )
    rdflib::rdf_parse(doc = doc,
                      format = format)

  }




#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param doc PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[xfun]{file_ext}}
#'  \code{\link[rdflib]{rdf_parse}}
#'  \code{\link[cli]{cli_alert}}
#' @rdname read_rdf2
#' @export
#' @importFrom xfun file_ext
#' @importFrom rdflib rdf_parse
#' @importFrom cli cli_alert_warning cli_alert_info cli_alert_success cli_alert_danger
read_rdf2 <-
  function(doc) {


    rdf_ext <- xfun::file_ext(doc)
    rdf_ext <-
      match.arg(arg = rdf_ext,
                choices =
                  c("rdf", "ttl"),
                several.ok = FALSE)

    ext_to_format <-
      list(rdf = "rdfxml",
           ttl = "turtle")

    format <- ext_to_format[[rdf_ext]]

    if (!is_empty_document(doc = doc,
                           format = format)) {

      return(
        rdflib::rdf_parse(doc = doc,
                          format = format))


    } else {


      cli::cli_alert_warning(
        "doc extension `{rdf_ext}` does not parse as `{format}` like expected..."
      )

      ext_to_format <- ext_to_format[!(names(ext_to_format) %in% rdf_ext)]
      # Add extension to other supported formats to cover all the bases
      ext_to_format <- c(ext_to_format, "jsonld", "nquads", "ntriples")

      for (i in seq_along(ext_to_format)) {

        format <-
          ext_to_format[[i]]

        cli::cli_alert_info("Trying format `{format}`...")

        if (!is_empty_document(doc = doc,
                               format = format)) {

          cli::cli_alert_success("Trying format `{format}`... success!\n")

          return(
            rdflib::rdf_parse(doc = doc,
                              format = format)
          )

        }


      }


      cli::cli_alert_danger("`doc` is invalid.")



    }
  }
