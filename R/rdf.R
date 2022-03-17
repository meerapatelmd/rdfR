#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rdflib]{rdf_add}}
#' @rdname initialize_blank_rdf
#' @export
#' @importFrom rdflib rdf_add rdf
initialize_blank_rdf <-
  function() {


    root_ontology <- rdflib::rdf()

    rdflib::rdf_add(
      rdf = root_ontology,
      subject = "",
      predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      object = "http://www.w3.org/2002/07/owl#Ontology"
    )

    rdflib::rdf_add(
      rdf = root_ontology,
      subject = "",
      predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      object = "http://www.w3.org/2002/07/owl#Class"
    )

    rdflib::rdf_add(
      rdf = root_ontology,
      subject = "",
      predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      object = "http://www.w3.org/2002/07/owl#NamedIndividual"
    )

    rdflib::rdf_add(
      rdf = root_ontology,
      subject = "",
      predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      object = "http://www.w3.org/2002/07/owl#AnnotationProperty"
    )

    rdflib::rdf_add(
      rdf = root_ontology,
      subject = "",
      predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate",
      object = "http://www.w3.org/2000/01/rdf-schema#subClassOf"
    )



    root_ontology
  }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param name PARAM_DESCRIPTION
#' @param base_uri PARAM_DESCRIPTION
#' @param version PARAM_DESCRIPTION
#' @param comment PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rdflib]{rdf_add}}
#'  \code{\link[glue]{glue}}
#' @rdname initialize_rdf
#' @export
#' @importFrom rdflib rdf_add
#' @importFrom glue glue
initialize_rdf <-
  function(name,
           base_uri,
           version,
           comment) {


    rdf <-
      initialize_blank_rdf()



    rdflib::rdf_add(
      rdf = rdf,
      subject = base_uri,
      predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      object = "http://www.w3.org/2002/07/owl#Ontology"
    )

    rdf_version_id <-
      as.character(
        glue::glue("{base_uri}/{version}")
      )

    rdflib::rdf_add(
      rdf = rdf,
      subject = base_uri,
      predicate = "http://www.w3.org/2002/07/owl#versionIRI",
      object = rdf_version_id
    )


    rdflib::rdf_add(
      rdf = rdf,
      subject = base_uri,
      predicate = "http://www.w3.org/2000/01/rdf-schema#label",
      object = name
    )

    rdflib::rdf_add(
      rdf = rdf,
      subject = base_uri,
      predicate = "http://www.w3.org/2000/01/rdf-schema#comment",
      object = comment
    )


    rdf


  }



#' @title
#' Append AnnotationProperty
#' @description
#' `append_*` functions are to add an extension to the
#' RDF structure (i.e. addition of Object Properties, Annotation Properties, and
#' Data Properties) whereas `add_*` and `df_add_*` functions populate the RDF with
#' triplets.
#'
#' @param rdf PARAM_DESCRIPTION
#' @param base_uri PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rlang]{list2}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[rdflib]{rdf_add}}
#' @rdname append_annotation_properties
#' @export
#' @importFrom rlang list2
#' @importFrom glue glue
#' @importFrom rdflib rdf_add
append_annotation_properties <-
  function(rdf,
           base_uri,
           ...) {

    annotation_properties <- unlist(rlang::list2(...))
    for (annotation_property in annotation_properties) {
      id <-
        as.character(glue::glue("{base_uri}/AnnotationProperty#{annotation_property}"))

      rdflib::rdf_add(
        rdf = rdf,
        subject = id,
        predicate = "http://www.w3.org/2000/01/rdf-schema#label",
        object = annotation_property
      )

      rdflib::rdf_add(
        rdf = rdf,
        subject = id,
        predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
        object = "http://www.w3.org/2002/07/owl#AnnotationProperty"
      )


    }

    rdf
  }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rdf PARAM_DESCRIPTION
#' @param class_id PARAM_DESCRIPTION
#' @param class_label PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rdflib]{rdf_add}}
#' @rdname add_class
#' @export
#' @importFrom rdflib rdf_add
add_class <-
  function(rdf,
           class_id,
           class_label) {

    rdflib::rdf_add(
      rdf = rdf,
      subject =  class_id,
      predicate = "http://www.w3.org/2000/01/rdf-schema#label",
      object    = class_label
    )

    rdflib::rdf_add(
      rdf = rdf,
      subject =  class_id,
      predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      object    = "http://www.w3.org/2002/07/owl#Class"
    )

    rdf

  }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rdf PARAM_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param class_id_col PARAM_DESCRIPTION
#' @param class_label_col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{filter}}
#'  \code{\link[rdflib]{rdf_add}}
#' @rdname df_add_classes
#' @export
#' @importFrom dplyr filter
#' @importFrom rdflib rdf_add
#' @import cli
df_add_classes <-
  function(rdf,
           data,
           class_id_col,
           class_label_col) {


    data <-
      data %>%
      select({{ class_id_col }},
             {{ class_label_col }}) %>%
      distinct()

    n <- nrow(data)
    i <- 0
    class_label <- "class_label"
    cli::cli_alert_info("Adding {n} class{?es}...")
    cli::cli_progress_step("({i}/{n}) {class_label}",
                      spinner = TRUE)


    for (i in 1:nrow(data)) {

      class_id <-
        data %>%
        select({{ class_id_col }}) %>%
        dplyr::filter(row_number() == i) %>%
        unlist() %>%
        unname()


      class_label <-
        data %>%
        select({{ class_label_col }}) %>%
        dplyr::filter(row_number() == i) %>%
        unlist() %>%
        unname()

      rdflib::rdf_add(
        rdf = rdf,
        subject =  class_id,
        predicate = "http://www.w3.org/2000/01/rdf-schema#label",
        object    = class_label
      )

      rdflib::rdf_add(
        rdf = rdf,
        subject =  class_id,
        predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
        object    = "http://www.w3.org/2002/07/owl#Class"
      )

      Sys.sleep(0.05)
      cli::cli_progress_update()



    }

    rdf
  }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rdf PARAM_DESCRIPTION
#' @param individual_id PARAM_DESCRIPTION
#' @param individual_label PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rdflib]{rdf_add}}
#' @rdname add_individual
#' @export
#' @importFrom rdflib rdf_add
add_individual <-
  function(rdf,
           individual_id,
           individual_label) {


    rdflib::rdf_add(
      rdf = rdf,
      subject =  individual_id,
      predicate = "http://www.w3.org/2000/01/rdf-schema#label",
      object    = individual_label
    )

    rdflib::rdf_add(
      rdf = rdf,
      subject =  individual_id,
      predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      object    = "http://www.w3.org/2002/07/owl#NamedIndividual"
    )

    rdf

  }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rdf PARAM_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param individual_id_col PARAM_DESCRIPTION
#' @param individual_label_col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{filter}}
#'  \code{\link[rdflib]{rdf_add}}
#' @rdname df_add_individuals
#' @export
#' @importFrom dplyr filter
#' @importFrom rdflib rdf_add
#' @import cli

df_add_individuals <-
  function(rdf,
           data,
           individual_id_col,
           individual_label_col) {


    data <-
      data %>%
      select({{ individual_id_col }},
             {{ individual_label_col }}) %>%
      distinct()

    n <- nrow(data)
    i <- 0
    individual_label <- "individual_label"
    cli::cli_alert_info("Adding {n} individual{?s}...")
    cli::cli_progress_step("({i}/{n}) {individual_label}",
                      spinner = TRUE)


    for (i in 1:nrow(data)) {

      individual_id <-
        data %>%
        select({{ individual_id_col }}) %>%
        dplyr::filter(row_number() == i) %>%
        unlist() %>%
        unname()


      individual_label <-
        data %>%
        select({{ individual_label_col }}) %>%
        dplyr::filter(row_number() == i) %>%
        unlist() %>%
        unname()

      rdflib::rdf_add(
        rdf = rdf,
        subject =  individual_id,
        predicate = "http://www.w3.org/2000/01/rdf-schema#label",
        object    = individual_label
      )

      rdflib::rdf_add(
        rdf = rdf,
        subject =  individual_id,
        predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
        object    = "http://www.w3.org/2002/07/owl#NamedIndividual"
      )

      cli::cli_progress_update()


    }

    rdf
  }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rdf PARAM_DESCRIPTION
#' @param parent_class PARAM_DESCRIPTION
#' @param child_class PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}}
#'  \code{\link[rdflib]{rdf_add}}
#' @rdname add_subclassof
#' @export
#' @importFrom purrr map
#' @importFrom rdflib rdf_add
add_subclassof <-
  function(rdf,
           parent_class,
           child_class) {

    args <-
      list(parent_class = parent_class,
           child_class = child_class) %>%
      purrr::map(id_if_label)


    rdflib::rdf_add(
      rdf = rdf,
      subject =  args$child_class,
      predicate = "http://www.w3.org/2000/01/rdf-schema#subClassOf",
      object    = args$parent_class
    )

    rdf
  }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rdf PARAM_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param parent_class_id_col PARAM_DESCRIPTION
#' @param child_class_id_col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{filter}}
#'  \code{\link[rdflib]{rdf_add}}
#' @rdname df_add_subclassof
#' @export
#' @importFrom dplyr filter
#' @importFrom rdflib rdf_add
#' @import cli

df_add_subclassof <-
  function(rdf,
           data,
           parent_class_id_col,
           child_class_id_col) {

    data <-
      data %>%
      select({{ parent_class_id_col }},
             {{ child_class_id_col }}) %>%
      distinct()

    n <- nrow(data)
    i <- 0
    child <- "child"
    parent <- "parent"
    cli::cli_alert_info("Adding {n} parent-child relationship{?s}...")
    cli::cli_progress_step("({i}/{n}) parent:{parent} --> child:{child}",
                           spinner = TRUE)


    for (i in 1:nrow(data)) {

      child <-
        data %>%
        dplyr::filter(row_number() == i) %>%
        select({{ child_class_id_col }}) %>%
        unlist() %>%
        unname()

      parent <-
        data %>%
        dplyr::filter(row_number() == i) %>%
        select({{ parent_class_id_col }}) %>%
        unlist() %>%
        unname()


      rdflib::rdf_add(
        rdf = rdf,
        subject =  child,
        predicate = "http://www.w3.org/2000/01/rdf-schema#subClassOf",
        object    = parent
      )

      cli::cli_progress_update()

    }


    rdf

  }


# Technically type
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rdf PARAM_DESCRIPTION
#' @param individual PARAM_DESCRIPTION
#' @param class PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}}
#'  \code{\link[rdflib]{rdf_add}}
#' @rdname add_individual_class
#' @export
#' @importFrom purrr map
#' @importFrom rdflib rdf_add

add_individual_class <-
  function(rdf,
           individual,
           class) {


    args <-
      list(individual = indvidual,
           class = class) %>%
      purrr::map(id_if_label)



    rdflib::rdf_add(
      rdf = rdf,
      subject =  args$individual,
      predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      object    = args$class
    )

    rdf

  }

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rdf PARAM_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param class_id_col PARAM_DESCRIPTION
#' @param individual_id_col PARAM_DESCRIPTION
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
#'  \code{\link[rdflib]{rdf_add}}
#' @rdname df_add_individual_class
#' @export
#' @importFrom dplyr filter select
#' @importFrom rdflib rdf_add
#' @import cli

df_add_individual_class <-
  function(rdf,
           data,
           class_id_col,
           individual_id_col) {


    data <-
      data %>%
      select({{ class_id_col }},
             {{ individual_id_col }}) %>%
      distinct()

    n <- nrow(data)
    i <- 0
    individual <- "individual"
    class <- "class"
    cli::cli_alert_info("Adding {n} class-individual relationship{?s}...")
    cli::cli_progress_step("({i}/{n}) class:{class} --> individual:{individual}",
                           spinner = TRUE)


    for (i in 1:nrow(data)) {

      individual <-
        data %>%
        dplyr::filter(row_number() == i) %>%
        dplyr::select({{ individual_id_col }}) %>%
        unlist() %>%
        unname()

      class <-
        data %>%
        dplyr::filter(row_number() == i) %>%
        dplyr::select({{ class_id_col }}) %>%
        unlist() %>%
        unname()



      rdflib::rdf_add(
        rdf = rdf,
        subject =  individual,
        predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
        object    = class
      )

    cli::cli_progress_update()

    }

    rdf

  }





#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rdf PARAM_DESCRIPTION
#' @param entity PARAM_DESCRIPTION
#' @param annotation_property PARAM_DESCRIPTION
#' @param value PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}}
#'  \code{\link[rdflib]{rdf_add}}
#' @rdname add_annotation_property_value
#' @export
#' @importFrom purrr map
#' @importFrom rdflib rdf_add

add_annotation_property_value <-
  function(rdf,
           entity,
           annotation_property,
           value) {


    args <-
      list(entity = entity,
           annotation_property = annotation_property) %>%
      purrr::map(id_if_label,
                 rdf = rdf)


    rdflib::rdf_add(
      rdf = rdf,
      subject = args$entity,
      predicate = args$annotation_property,
      object = value
    )
  }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rdf PARAM_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param entity_col PARAM_DESCRIPTION
#' @param annotation_property_label PARAM_DESCRIPTION
#' @param value_col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[cli]{cli_alert}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{select}}
#' @rdname df_add_annotation_property_value
#' @export
#' @importFrom cli cli_alert_danger
#' @importFrom dplyr filter select
#' @import cli

df_add_annotation_property_value <-
  function(rdf,
           data,
           entity_col,
           annotation_property_label,
           value_col) {


    annotation_property_predicates <-
      query_annotation_predicates(rdf = rdf)
    annotation_property_predicate <-
      annotation_property_predicates[[annotation_property_label]]


    if (is.null(annotation_property_predicate)) {
      cli::cli_alert_danger(text = "{annotation_property_label} does not exist.")
      stop(call. = FALSE)
    }

    annotation_property_predicate <-
      annotation_property_predicate$id


    if (length(annotation_property_predicate)>1) {
      cli::cli_alert_danger("{annotation_property_label} is associated with {length(annotation_property_predicate)}: {annotation_property_predicate}.")
    }


    n <- nrow(data)
    i <- 0
    entity <- "entity"
    annotation_property_label <- "annotation_property_label"
    value <- "value"
    cli::cli_alert_info("Adding {n} annotation propert{?y/ies}...")
    cli::cli_progress_step("({i}/{n}) entity:{entity}-{annotation_property_label}-{value}",
                           spinner = TRUE)

    for (i in 1:nrow(data)) {

      entity <-
        data %>%
        dplyr::filter(row_number() == i) %>%
        dplyr::select({{ entity_col }}) %>%
        unlist() %>%
        unname()


      if (!is_id(entity)) {
        entity_id <- label_to_id(entity)
      } else {
        entity_id <- entity
      }


      value <-
        data %>%
        dplyr::filter(row_number() == i) %>%
        dplyr::select({{ value_col }}) %>%
        unlist() %>%
        unname()

      add_annotation_property_value(
        rdf = rdf,
        entity = entity_id,
        annotation_property = annotation_property_predicate,
        value = value
      )


      cli::cli_progress_update()


    }


    rdf



  }
