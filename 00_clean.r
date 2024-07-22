df <-
  # NOTE: Iterate over questionnaire data files and load in a list
  purrr::map(
    fs::dir_ls("./data/quest_data/") |>
      grep(pattern = "w[1-4].*\\.xlsx", value = TRUE),
    \(file) janitor::clean_names(readxl::read_xlsx(file))
  ) |>
  # NOTE: Rename each entry with more readable names
  purrr::set_names(
    nm = "T0", "T0_WL", "T1", "T2", "T3", "T3_WL"
  ) |>
  # NOTE: Join all entries in a single 'tibble'
  dplyr::bind_rows(
    .id = "time"
  ) |>
  # NOTE: Clean emails (IDs) and transform time to a numeric column
  dplyr::mutate(
    email = stringr::str_squish(stringr::str_to_lower(email)),
    time = readr::parse_number(time)
  ) |>
  # NOTE: Remove `group` column, add from the `groups` sheet
  dplyr::select(
    -group
  ) |>
  dplyr::left_join(
    y = readxl::read_xlsx("./data/aux_data/groups.xlsx"),
    by = dplyr::join_by(email == email)
  ) |>
  # NOTE: Recode and relocate it after the `time` column
  dplyr::mutate(
    group = dplyr::case_match(
      group,
      "gcbt" ~ "GCBT",
      "thrive" ~ "App",
      "waiting_list" ~ "WL"
    ),
    .after = time
  ) |>
  # NOTE: Calculate total scores for PHQ-9, GAD-7 and UCLA-3
  dplyr::rowwise() |>
  dplyr::mutate(
    dplyr::across(
      dplyr::matches("^(phq|gad)_0[1-9]$"),
      \(x) x - 1
    ),
    phq = phq_01 + phq_02 + phq_03 + phq_04 + phq_05 +
      phq_06 + phq_07 + phq_08 + phq_09,
    gad = gad_01 + gad_02 + gad_03 + gad_04 + gad_05 +
      gad_06 + gad_07,
    ucla = ucla_01 + ucla_02 + ucla_03,
    .after = phone
  ) |>
  dplyr::ungroup() |>
  # NOTE: Remove PHQ, GAD and UCLA items, NEQ items and phone numbers
  dplyr::select(
    -dplyr::matches("^(phq|gad|ucla)_0[1-9]$"),
    -dplyr::starts_with("neq"),
    -phone
  ) |>
  # NOTE: Remove entries with missing value on PHQ, GAD, UCLA or treatment arm
  dplyr::filter(
    !is.na(phq),
    !is.na(gad),
    !is.na(ucla),
    !is.na(group)
  ) |>
  # NOTE: In case there are multiple entries in the same timepoint for a single
  # individual, we keep the one with highest value on PHQ-9, followed by GAD-7
  dplyr::slice_max(
    tibble::tibble(phq, gad),
    n = 1,
    by = c(time, email)
  ) |>
  # NOTE: Keep only subjects that have at least two assessments
  dplyr::filter(
    dplyr::n() > 1,
    .by = email
  ) |>
  # NOTE: Order the rows based on the email (alphabetically)
  dplyr::arrange(
    email
  )


# NOTE: Create subset with only 'pre-post' data. For that, we only keep the
# rows with baseline data or last assessment (for each individual)
pre_post <- df |>
  dplyr::filter(
    time == 0 | time == max(time),
    .by = email
  ) |>
  # NOTE: Keep only subjects that have at least two assessments after filtering
  # for baseline data and last assessment. If there are participants that were
  # not assessed at baseline, and have entries in the 4-week and 8-week
  # assessments for example, they will be removed here
  dplyr::filter(
    dplyr::n() > 1,
    .by = email
  ) |>
  # NOTE: Transform time to dichotomous column (0: pre; 1: post)
  dplyr::mutate(
    time = dplyr::if_else(time > 0, 1, time)
  ) |>
  # NOTE: Change specific instrument columns to `outcome` and `score` columns
  tidyr::pivot_longer(
    cols = c(phq, gad, ucla),
    names_to = "outcome",
    values_to = "score",
    names_transform = \(x) {
      dplyr::case_match(
        x,
        "phq" ~ "PHQ-9",
        "gad" ~ "GAD-7",
        "ucla" ~ "UCLA-3"
      )
    }
  ) |>
  # NOTE: Transform `time` and `score` columns into `pre` and `post` columns
  tidyr::pivot_wider(
    names_from = "time",
    values_from = "score",
    names_glue = "{dplyr::if_else(time == 0, 'pre', 'post')}"
  )


# FIX: We have 18 subjects from the waiting list that have no sociodemographic
# data for the descriptive analysis.
pre_post |>
  dplyr::left_join(
    y = janitor::clean_names(
      readxl::read_xlsx("./data/aux_data/demog_table.xlsx")
    ),
    by = dplyr::join_by(email == email)
  ) |>
  dplyr::filter(is.na(ano_de_nascimento)) |>
  dplyr::pull(email) |>
  unique()


# TODO: Update exported clean data after inclusion of demographic info
pre_post |>
  readr::write_rds(
    file = "./data/exported/pre_post.rds"
  )
