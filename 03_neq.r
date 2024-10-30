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
neq_yes_no <- neq_data |>
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
  )

neq_table_dic <- neq_yes_no |>
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

# NOTE: Plot the NEQ adverse effects in grouped bar plots

neq_freq <- neq_yes_no |>
  tidyr::pivot_longer(
    cols = neq_01:neq_20,
    names_to = "item",
    values_to = "response",
    names_transform = readr::parse_number
  ) |>
  dplyr::group_by(group, assessment, item) |>
  dplyr::count(response) |>
  dplyr::mutate(pct = n / sum(n)) |>
  dplyr::ungroup() |>
  dplyr::filter(response == "Yes") |>
  dplyr::mutate(
    item = as.factor(item) |> forcats::fct_rev(),
    assessment = forcats::fct_relevel(
      assessment, c("Week 12", "Week 8", "Week 4")
    )
  )

neq_plot <- neq_freq |>
  ggplot2::ggplot(ggplot2::aes(x = pct, y = item, fill = assessment)) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::scale_x_continuous(labels = scales::percent) +
  ggsci::scale_fill_nejm() +
  ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey20") +
  ggplot2::facet_wrap(~group) +
  ggplot2::labs(
    x = "Reported frequency (%)", y = "NEQ item", fill = "Assessment"
  ) +
  ggplot2::theme_bw(20, "Fira Sans") +
  ggplot2::theme(
    legend.position = "top",
    panel.grid.minor.y = ggplot2::element_blank()
  )

ggplot2::ggsave(
  plot = neq_plot,
  filename = "./output/plots/neq_plot.png",
  dpi = 500,
  height = 16,
  width = 9
)
