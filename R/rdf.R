initialize_blank_rdf <-
  function() {


    root_ontology <- rdf()

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



    }

    rdf
  }


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



    }

    rdf
  }


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


    }


    rdf

  }


# Technically type
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



    }

    rdf

  }





add_annotation_property_value <-
  function(rdf,
           entity,
           annotation_property,
           value) {


    args <-
      list(entity = entity,
           annotation_property = annotation_property) %>%
      purrr::map(id_if_label)


    rdflib::rdf_add(
      rdf = rdf,
      subject = args$entity,
      predicate = args$annotation_property,
      object = value
    )
  }


df_add_annotation_property_value <-
  function(rdf,
           data,
           entity_col,
           annotation_property_label,
           value_col) {


    annotation_property_predicates <-
      get_annotation_predicates(doc = rdf)
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
      stop(call. = FALSE)
    }


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

      rdf_add_annotation_property_value(
        rdf = rdf,
        entity = entity_id,
        annotation_property = annotation_property_predicate,
        value = value
      )


    }


    rdf



  }
