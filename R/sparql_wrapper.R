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
#' @rdname read_rdf_as_df
#' @export
#' @importFrom rdflib rdf_parse rdf_query
read_rdf_as_df <-
  function(doc) {
    input_rdf <- rdflib::rdf_parse(doc = doc)

    rdflib::rdf_query(input_rdf, query = "SELECT DISTINCT ?subject ?predicate ?object WHERE { ?subject ?predicate ?object .}")

  }

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rdf PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rdf_as_df
#' @export
#' @importFrom rdflib rdf_query
rdf_as_df <-
  function(rdf) {

    rdflib::rdf_query(rdf, query = "SELECT DISTINCT ?subject ?predicate ?object WHERE { ?subject ?predicate ?object .}")

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
#'  \code{\link[rdflib]{rdf_query}}
#' @rdname read_predicates
#' @export
#' @importFrom rdflib rdf_query
read_predicates <-
  function(doc) {

      input_rdf <- read_rdf(doc = doc)


    rdflib::rdf_query(input_rdf, query = "SELECT DISTINCT ?predicate WHERE { ?subject ?predicate ?object .}")

  }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
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
#'  \code{\link[rdflib]{rdf_query}}
#' @rdname query_predicates
#' @export
#' @importFrom rdflib rdf_query
query_predicates <-
  function(rdf) {

        rdflib::rdf_query(input_rdf, query = "SELECT DISTINCT ?predicate WHERE { ?subject ?predicate ?object .}")

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
#'  \code{\link[rdflib]{rdf_query}}
#'  \code{\link[dplyr]{mutate-joins}},\code{\link[dplyr]{select}}
#'  \code{\link[purrr]{map}}
#' @rdname read_annotation_predicates
#' @export
#' @importFrom rdflib rdf_query
#' @importFrom dplyr left_join select
#' @importFrom purrr map
read_annotation_predicates <-
  function(doc) {

      input_rdf <- read_rdf(doc = doc)

    annotation_properties <-
    rdflib::rdf_query(input_rdf, query = "SELECT DISTINCT ?id WHERE { ?id <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#AnnotationProperty> .}")


    label_map <-
      read_label_map(doc = doc)


    annotation_properties %>%
      dplyr::left_join(label_map,
                by = "id") %>%
      split(.$label) %>%
      purrr::map(dplyr::select, -label)


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
#'  \code{\link[rdflib]{rdf_query}}
#' @rdname read_label_map
#' @export
#' @importFrom rdflib rdf_query
read_label_map <-
  function(doc) {

    input_rdf <- read_rdf(doc = doc)

    rdflib::rdf_query(
      input_rdf,
      query =
        "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      SELECT DISTINCT ?id ?label WHERE { ?id rdfs:label ?label .}")


  }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
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
#'  \code{\link[rdflib]{rdf_query}}
#' @rdname query_label_map
#' @export
#' @importFrom rdflib rdf_query
query_label_map <-
  function(rdf) {
    rdflib::rdf_query(
      rdf,
      query =
        "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      SELECT DISTINCT ?id ?label WHERE { ?id rdfs:label ?label .}")

  }

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
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
#'  \code{\link[dplyr]{select}}
#'  \code{\link[rlang]{parse_expr}}
#' @rdname map_to_labels
#' @export
#' @importFrom dplyr select
#' @importFrom rlang parse_expr
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


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param predicate PARAM_DESCRIPTION
#' @param rdf_or_doc PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rdflib]{rdf_query}}
#' @rdname search_by_predicate
#' @export
#' @importFrom rdflib rdf_query
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
#'  \code{\link[rdflib]{rdf_query}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{mutate-joins}}
#' @rdname read_rdf_taxonomy
#' @export
#' @importFrom rdflib rdf_query
#' @importFrom dplyr filter distinct inner_join
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


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
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
#'  \code{\link[rdflib]{rdf_query}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{mutate-joins}}
#' @rdname query_rdf_taxonomy
#' @export
#' @importFrom rdflib rdf_query
#' @importFrom dplyr filter distinct inner_join
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
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{mutate-joins}},\code{\link[dplyr]{rename}},\code{\link[dplyr]{tidyeval-compat}}
#'  \code{\link[glue]{glue}}
#' @rdname read_pivoted_rdf_taxonomy
#' @export
#' @importFrom dplyr select left_join rename sym
#' @importFrom glue glue
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
