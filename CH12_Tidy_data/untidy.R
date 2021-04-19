tumors <- tibble::tribble(
    ~mrn, ~sequence, ~age, ~tumor_size, ~tumor_unit,
    289034L,        1L,  47L,         0.9,        "cm",
    289034L,        2L,  47L,        35.2,        "mm",
    290660L,        1L,  49L,          30,        "mm",
    290660L,        2L,  49L,        15.5,        "mm",
    341050L,        1L,  61L,        14.7,        "mm",
    341050L,        2L,  70L,         2.1,        "cm",
    385615L,        1L,  71L,          12,        "mm",
    385615L,        2L,  71L,           2,        "cm",
    550955L,        1L,  43L,         2.5,        "cm",
    550955L,        2L,  43L,         131,        "mm"
)

tumors_1 <-
    tumors %>%
    dplyr::mutate(tumor_size = paste(tumor_size, tumor_unit)) %>%
    dplyr::select(-tumor_unit) %>%
    dplyr::mutate(age = as.character(age)) %>%
    tidyr::pivot_longer(cols = c(age, tumor_size), names_to = "type") %>%
    dplyr::arrange(mrn, sequence)

tumors_2a <-
    tumors %>%
    dplyr::select(mrn, sequence, age) %>%
    tidyr::pivot_wider(names_from = sequence, values_from = age)

tumors_2b <-
    tumors %>%
    dplyr::mutate(tumor_size = paste(tumor_size, tumor_unit)) %>%
    dplyr::select(mrn, sequence, tumor_size) %>%
    tidyr::pivot_wider(names_from = sequence, values_from = tumor_size)

tumors_3a <-
    tumors %>%
    dplyr::mutate(age = paste0(sequence, " - ", age)) %>%
    dplyr::select(-sequence)

tumors_3b <-
    tumors %>%
    dplyr::mutate(age = paste0(age, " (", sequence, ")")) %>%
    dplyr::select(-sequence)

tumors_3c <-
    tumors %>%
    tidyr::unite(tumor_size, tumor_size, tumor_unit, sep = " ")

tumors_3d <-
    tumors_3c %>%
    dplyr::mutate(tumor_size = stringr::str_remove(tumor_size, " mm"))

set.seed(4242)
tumors_3e <-
    tumors_3c %>%
    dplyr::mutate(tumor_size = ifelse(
        sample(c(TRUE, FALSE), nrow(.), replace = TRUE),
        paste(tumor_size, "irregular"),
        tumor_size
    ))

tumors_4 <-
    t(tumors) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    tibble::rownames_to_column("var") %>%
    dplyr::as_tibble() %>%
    dplyr::mutate_all(trimws) %>%
    purrr::set_names(c("var", .[1, -1])) # %>%
    # dplyr::slice(-1)
