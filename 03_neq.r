df <- readr::read_rds("./data/exported/pre_post.rds")

# NOTE: Assessments of negative effects were done starting from the second
# assessment (W1 was the baseline)
w2 <- readxl::read_xlsx("./data/quest_data/w2.xlsx")
w3 <- readxl::read_xlsx("./data/quest_data/w3.xlsx")
w4 <- readxl::read_xlsx("./data/quest_data/w4.xlsx")

# NOTE: Get email (ID) of participants from treatment arms (excluding WL)
treated <- df |>
  dplyr::filter(group %in% c("GCBT", "App")) |>
  dplyr::pull(email) |>
  unique()

# NOTE: Get the treatment arm associated with each ID (email)
groups <- df |>
  dplyr::filter(group %in% c("GCBT", "App")) |>
  dplyr::select(email, group) |>
  dplyr::distinct(email, .keep_all = TRUE)

#' Get Negative Effects Questionnaire items from a data frame
#'
#' `get_neq_df` returns a data frame with email and NEQ items.
#'
#' @param data Original data frame
#' @param treated Emails of participants that were assigned to a treatment arm
get_neq_df <- function(data, treated) {
  data <- data[, c("email", grep("^neq.*$", names(data), value = TRUE))]
  data$email <- stringr::str_squish(stringr::str_to_lower(data$email))
  data <- data[data$email %in% treated, ]

  return(data)
}

# NOTE: Join all assessments and add the groups (treatment arms) for each
# study participant
neq_data <- dplyr::bind_rows(
  get_neq_df(w2, treated),
  get_neq_df(w3, treated),
  get_neq_df(w4, treated),
  .id = "assessment"
) |>
  dplyr::left_join(groups, by = dplyr::join_by(email == email)) |>
  dplyr::relocate(group, .after = email)


# NOTE: NEQ frequency table, in order to show how many subjects experienced
# a given negative effect during treatment. It is stratified by treatment arm
# assessment wave
neq_table_dic <- neq_data |>
  dplyr::select(
    group,
    assessment,
    dplyr::matches("^neq_[0-9]+$"), -neq_21
  ) |>
  dplyr::mutate(
    assessment = as.character(assessment),
    assessment = dplyr::case_match(
      assessment,
      "1" ~ "Week 4",
      "2" ~ "Week 8",
      "3" ~ "Week 12"
    ) |> factor(levels = c("Week 4", "Week 8", "Week 12")),
    dplyr::across(
      dplyr::starts_with("neq"),
      as.character
    ),
    dplyr::across(
      dplyr::starts_with("neq"),
      \(x) {
        dplyr::case_match(
          x,
          "1" ~ "No",
          "2" ~ "Yes"
        )
      }
    ),
  ) |>
  dplyr::rename_with(\(col) stringr::str_replace(col, "neq_", "Item ")) |>
  gtsummary::tbl_strata(
    strata = assessment,
    .tbl_fun =
      \(x) {
        x |>
          gtsummary::tbl_summary(
            by = group,
            missing = "ifany",
            missing_text = "Missing"
          )
      }
  )

neq_table_dic |>
  gtsummary::as_gt() |>
  gt::gtsave("./output/docx/neq_table_dic.docx")


# NOTE: NEQ intensity table, to check the reported intensity for each negative
# effect. Only subjects that answered "yes" for the binary item have an answer
# for these questions
neq_table_intensity <- neq_data |>
  dplyr::select(
    group,
    assessment,
    dplyr::matches("^neq_[0-9]+b$")
  ) |>
  dplyr::mutate(
    assessment = as.character(assessment),
    assessment = dplyr::case_match(
      assessment,
      "1" ~ "Week 4",
      "2" ~ "Week 8",
      "3" ~ "Week 12"
    ) |> factor(levels = c("Week 4", "Week 8", "Week 12")),
    dplyr::across(
      dplyr::starts_with("neq"),
      as.character
    ),
    # TODO: I have to check which levels do we have here in the intensity
    # scale
    dplyr::across(
      dplyr::starts_with("neq"),
      \(x) {
        dplyr::case_match(
          x,
          "1" ~ "I was not affected",
          "2" ~ "Slightly",
          "3" ~ "Moderately",
          "4" ~ "Very",
          "5" ~ "Extremely"
        ) |> factor(levels = c(
          "I was not affected", "Slightly", "Moderately",
          "Very", "Extremely"
        ))
      }
    ),
  ) |>
  dplyr::rename_with(
    \(col) stringr::str_remove(stringr::str_replace(col, "neq_", "Item "), "b")
  ) |>
  gtsummary::tbl_strata(
    strata = assessment,
    .tbl_fun =
      \(x) {
        x |>
          gtsummary::tbl_summary(
            by = group,
            missing = "ifany",
            missing_text = "N/A"
          )
      }
  )

neq_table_intensity |>
  gtsummary::as_gt() |>
  gt::gtsave("./output/docx/neq_table_intensity.docx")
