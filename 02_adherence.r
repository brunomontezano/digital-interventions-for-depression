df <- readr::read_rds("./data/exported/pre_post.rds")

gcbt_absences <- readxl::read_xlsx("./data/aux_data/absences_gcbt.xlsx") |>
  dplyr::mutate(
    # NOTE: Clean emails to facilicate joins later on
    email = email |> stringr::str_squish() |> stringr::str_to_lower()
  )

app_long <- readr::read_csv("./data/app_data/thrive_long_data.csv")
app_wide <- readr::read_csv("./data/app_data/thrive_wide_data.csv")

# NOTE: In the next section, I calculate the total interactions in each
# month for each patient/study participant

interacoes <- app_long |>
  janitor::clean_names() |>
  dplyr::group_by(email, wave) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    interactions = sum(
      dplyr::c_across(dplyr::starts_with("n_of")),
      na.rm = TRUE
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::select(email, wave, interactions) |>
  dplyr::slice_max(
    interactions,
    n = 1, by = c(email, wave),
    with_ties = FALSE
  ) |>
  tidyr::pivot_wider(
    names_from = wave,
    values_from = interactions,
    names_prefix = "w"
  ) |>
  dplyr::mutate(adhered = w1 > 0 & w2 > 0 & w3 > 0)

# NOTE: Calculate how many participants from the App treatment arm actually
# adhered to treatment based on the criteria (at least one interactions per
# month)
df |>
  dplyr::filter(group == "App") |>
  dplyr::distinct(email) |>
  dplyr::left_join(interacoes, by = dplyr::join_by(email == email)) |>
  dplyr::count(adhered)

# NOTE: Get absences from participants of the GCBT group and check whether
# they meet adherence criteria or not (max of 4 absences)

gcbt_absences |>
  dplyr::mutate(faltas = dplyr::na_if(faltas, 99)) |>
  dplyr::right_join(
    df |>
      dplyr::filter(group == "GCBT") |>
      dplyr::distinct(email),
    by = dplyr::join_by(email == email)
  ) |>
  dplyr::count(met_criteria_gcbt = faltas < 5) |>
  dplyr::mutate(perc = n / sum(n))
