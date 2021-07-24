is_id <-
  function(x) {

    grepl(pattern = "^http://",
          x = x)
  }



label_to_id <-
  function(x,
           rdf) {


    id <-
      get_label_map(doc = rdf) %>%
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


id_if_label <-
  function(x) {
    if (!is_id(x)) {

      label_to_id(x)

    } else {
      x
    }
  }
