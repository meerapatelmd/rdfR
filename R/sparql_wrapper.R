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
