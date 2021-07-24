# Replaces any punctuation of length one or more with a dash
# Only keeps characters with alphanumeric characters or a dash

uri_format <-
  function(x) {

    x <-
      stringr::str_replace_all(x,
                               pattern = "[[:punct:]]{1,}",
                               replacement = "-")
    stringr::str_remove_all(x,
                            pattern = "[^A-Za-z0-9-]")

  }
