
rdf_to_df <-
  function(rdf) {

    if (file.exists("tmp.json")) {
      file.remove("tmp.json")
    }
    rdf_serialize(rdf = rdf,
                  doc = "tmp.json",
                  format = "jsonld")

    on.exit(
      expr = file.remove("tmp.json")
    )

    jsonlite::read_json(path = "tmp.json",
                        simplifyVector = TRUE)


  }



nodes_to_rdf <-
  function(data,
           id_col,
           label_col,
           type_col) {

root_ontology <- rdflib::rdf()
# Add Ontology, Annotation Property, and NamedIndividual Types
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

label_col <- dplyr::enquo(label_col)
id_col <- dplyr::enquo(id_col)
type_col <- dplyr::enquo(type_col)

attributes <-
  data %>%
  select(-!!label_col, -!!id_col) %>%
  colnames()

attribute_predicates <- vector()
for (attribute in attributes) {
  id <- as.character(glue("{baseurl}/AnnotationProperty#{attribute}"))

  rdf_add(
    rdf = root_ontology,
    subject = id,
    predicate = "http://www.w3.org/2000/01/rdf-schema#label",
    object = attribute
  )

  rdf_add(
    rdf = root_ontology,
    subject = id,
    predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
    object = "http://www.w3.org/2002/07/owl#AnnotationProperty"
  )

  attribute_predicates <-
    c(attribute_predicates,
      rdf_query(
        root_ontology,
        query = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o .}') %>%
        dplyr::filter(o == attribute) %>%
        select(s) %>%
        unlist() %>%
        unname()
    )

}
names(attribute_predicates) <- attributes
attribute_predicates <-
  as.list(attribute_predicates)


rdf_to_df(root_ontology)

}
