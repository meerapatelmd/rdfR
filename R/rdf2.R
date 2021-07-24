library(rdflib)

read_rdf_as_df <-
  function(doc) {
    input_rdf <- rdflib::rdf_parse(doc = doc)

    rdf_query(input_rdf, query = "SELECT DISTINCT ?subject ?predicate ?object WHERE { ?subject ?predicate ?object .}")

  }

rdf_as_df <-
  function(rdf) {

    rdf_query(rdf, query = "SELECT DISTINCT ?subject ?predicate ?object WHERE { ?subject ?predicate ?object .}")

  }

read_predicates <-
  function(doc) {

      input_rdf <- read_rdf(doc = doc)


    rdflib::rdf_query(input_rdf, query = "SELECT DISTINCT ?predicate WHERE { ?subject ?predicate ?object .}")

  }


query_predicates <-
  function(rdf) {

        rdflib::rdf_query(input_rdf, query = "SELECT DISTINCT ?predicate WHERE { ?subject ?predicate ?object .}")

  }



read_annotation_predicates <-
  function(doc) {

      input_rdf <- read_rdf(doc = doc)

    annotation_properties <-
    rdflib::rdf_query(input_rdf, query = "SELECT DISTINCT ?id WHERE { ?id <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#AnnotationProperty> .}")


    label_map <-
      get_label_map(doc = doc)


    annotation_properties %>%
      dplyr::left_join(label_map,
                by = "id") %>%
      split(.$label) %>%
      purrr::map(dplyr::select, -label)


  }


read_label_map <-
  function(doc) {

    input_rdf <- read_rdf(doc = doc)

    rdflib::rdf_query(
      input_rdf,
      query =
        "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      SELECT DISTINCT ?id ?label WHERE { ?id rdfs:label ?label .}")


  }


query_label_map <-
  function(rdf) {
    rdflib::rdf_query(
      rdf,
      query =
        "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      SELECT DISTINCT ?id ?label WHERE { ?id rdfs:label ?label .}")

  }

map_to_labels <-
  function(data,
           ...,
           rdf) {


    x <- data

    label_map <-
      query_label_map(rdf = rdf)


    if (missing(...)) {

      fields <- colnames(x)

    } else {

      selected_fields <-
        enquos(...)

      fields <-
        x %>%
        dplyr::select(!!!selected_fields) %>%
        colnames()



    }

    final_x <- x
    for (field in fields) {
      final_x <-
        eval(
          rlang::parse_expr(
            glue(
              'final_x %>%
            left_join(label_map,
                      by = c({field} = "id")) %>%
            rename({field}_label = label)'
            )
          )
        )


    }

    final_x

  }


search_by_predicate <-
  function(predicate,
           rdf_or_doc) {

    if (class(rdf_or_doc) != "rdf") {

     rdf <- read_rdf(doc = doc)

    }

    predicate_id <- id_if_label(predicate)

    rdflib::rdf_query(rdf,
              query = sprintf(
                "SELECT ?subject ?object
                WHERE { ?subject <%s> ?object .}",
                predicate_id))

  }




read_rdf_taxonomy <-
  function(doc) {

    input_rdf <- read_rdf(doc = doc)
    tax <- rdflib::rdf_query(input_rdf, query = "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> SELECT ?parent ?child WHERE { ?child rdfs:subClassOf ?parent .}")
    root <-
      tax %>%
      dplyr::filter(!(parent %in% tax$child)) %>%
      dplyr::distinct()

    output <- list()
    output[[1]] <- root

    for (i in 2:100) {

      x <-
        output[[i-1]] %>%
        dplyr::distinct(parent = child) %>%
        dplyr::inner_join(tax,
                   by = "parent")

      if (nrow(x)) {
        output[[length(output)+1]] <- x
      } else {
        break
      }


    }


    # output[[1]] <-
    #   output[[1]] %>%
    #   rename(root = parent)
    #
    #
    # output[[length(output)]] <-
    #   output[[length(output)]] %>%
    #   rename(leaf = child)

    output

  }


query_rdf_taxonomy <-
  function(rdf) {
    tax <- rdflib::rdf_query(rdf, query = "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> SELECT ?parent ?child WHERE { ?child rdfs:subClassOf ?parent .}")
    root <-
      tax %>%
      dplyr::filter(!(parent %in% tax$child)) %>%
      dplyr::distinct()

    output <- list()
    output[[1]] <- root

    for (i in 2:100) {

      x <-
        output[[i-1]] %>%
        dplyr::distinct(parent = child) %>%
        dplyr::inner_join(tax,
                          by = "parent")

      if (nrow(x)) {
        output[[length(output)+1]] <- x
      } else {
        break
      }


    }


    # output[[1]] <-
    #   output[[1]] %>%
    #   rename(root = parent)
    #
    #
    # output[[length(output)]] <-
    #   output[[length(output)]] %>%
    #   rename(leaf = child)

    output

  }


read_pivoted_rdf_taxonomy <-
  function(doc) {

    rdf_taxonomy_list <-
      read_rdf_taxonomy(doc = doc)


    out <-
      rdf_taxonomy_list[[1]] %>%
      dplyr::select(root = parent,
             child)

    for (i in 2:(length(rdf_taxonomy_list))) {

      out <-
        out %>%
        dplyr::left_join(rdf_taxonomy_list[[i]],
                  by = c("child" = "parent")) %>%
        dplyr::rename(!!dplyr::sym(glue::glue("level_{i-1}")) := child,
               child = child.y)


    }

    out <-
      out %>%
      dplyr::rename(!!dplyr::sym(glue::glue("level_{i}")) := child)
    #
    # out[[length(rdf_taxonomy_list)]] <-
    #   rdf_taxonomy_list[[length(rdf_taxonomy_list)]]

    out

  }


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


# The RDF class object *has* to be written in Turtle format or else
# protege does not recognize it.

write_rdf <-
  function(rdf_object,
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


# Assumed that any document must have at least 1 stored triplet
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
