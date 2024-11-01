read_odk <- function(path) {
  data <- janitor::clean_names(readxl::read_xlsx(path))
  if ("data_usa_medicacao" %in% names(data)) {
    data <- data[, c(
      "data_email_participante",
      "data_usa_medicacao",
      "data_iniciou_trat"
    )]
  } else {
    data <- data[, c(
      "data_email_participante",
      "data_phq9_usa_medicacao",
      "data_phq9_iniciou_trat"
    )]
  }
  names(data) <- c("email", "med", "trat")
  return(data)
}

df <- readr::read_rds("./data/exported/pre_post.rds")

odk_1 <- read_odk("./data/odk_data/odk_v1.xlsx")
odk_2 <- read_odk("./data/odk_data/odk_v2.xlsx")
odk_3 <- read_odk("./data/odk_data/odk_v3.xlsx")
odk_4 <- read_odk("./data/odk_data/odk_v4.xlsx")

emails <- df |>
  dplyr::distinct(email)

treat_data <- dplyr::bind_rows(odk_1, odk_2, odk_3, odk_4) |>
  dplyr::slice_max(med, n = 1, by = email, with_ties = FALSE)

emails |>
  dplyr::left_join(treat_data, by = dplyr::join_by(email == email)) |>
  dplyr::count(med) |>
  dplyr::mutate(pct = n / sum(n))
