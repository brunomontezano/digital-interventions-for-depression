df <- readr::read_rds("./data/exported/pre_post.rds")

# NOTE: Tests for normality should be stratified by time, outcome and group to
# account for potential differences in distribution that could affect the
# validity of the statistical tests later on
df |>
  tidyr::pivot_longer(
    cols = c(pre, post),
    names_to = "time",
    values_to = "score"
  ) |>
  dplyr::group_by(group, outcome, time) |>
  rstatix::shapiro_test(score) |>
  dplyr::mutate(
    normal = p > 0.05
  )


# NOTE: Given the results above, the comparison of App vs. WL in the UCLA-3
# outcome should be performed using Wilcoxon signed-rank test. The remaining
# comparisons could be done by paired t-tests

wilcox_ucla_app <- wilcox.test(
  x = df$delta[df$group == "WL" & df$outcome == "UCLA-3"],
  y = df$delta[df$group == "App" & df$outcome == "UCLA-3"],
  alternative = "greater",
  exact = FALSE
)


df |>
  dplyr::group_by(outcome) |>
  rstatix::t_test(
    formula = delta ~ group,
    ref.group = "WL",
    alternative = "greater",
    p.adjust.method = "none",
    detailed = FALSE
  ) |>
  # NOTE: We should remove the UCLA-3 App comparison since it did not pass the
  # normality test
  dplyr::filter(!(outcome == "UCLA-3" & group2 == "App")) |>
  # NOTE: Not all columns from the output are relevant
  dplyr::select(
    outcome,
    arm = group2,
    statistic,
    df, p
  ) |>
  # NOTE: Manually add the row from UCLA-3 App comparison with WL
  tibble::add_row(
    outcome = "UCLA-3",
    arm = "App",
    statistic = wilcox_ucla_app$statistic,
    p = wilcox_ucla_app$p.value
  ) |>
  # NOTE: Finally, the p-values are adjusted based on Holm (1979) method
  # grouped by each outcome
  dplyr::group_by(outcome) |>
  rstatix::adjust_pvalue(
    p.col = "p",
    output.col = "p_adj",
    method = "fdr"
  )


# NOTE: Create descriptive table for the sample. There is a mutate()
# call to make some variables more readable and translated

gtsummary::theme_gtsummary_journal("jama")

df |>
  dplyr::mutate(
    age = 2022 - ano_de_nascimento,
    cor = dplyr::case_match(
      cor,
      "Branca" ~ "White",
      c("Parda", "Preta") ~ "Non-white"
    ),
    sexo = dplyr::case_match(
      sexo,
      c("Female", "Feminino") ~ "Female",
      c("Male", "Masculino") ~ "Male"
    ),
    orientacao_sexual = stringr::str_replace(orientacao_sexual, "ss", "s"),
    estado_civil = dplyr::case_match(
      estado_civil,
      "Casado (a)/ União Estável" ~ "Married or stable union",
      "Divorciado" ~ "Divorced",
      "Namorando" ~ "Dating",
      "Solteiro(a)" ~ "Single"
    ),
    filhos = dplyr::case_match(
      filhos,
      "Não" ~ "No",
      "Sim" ~ "Yes"
    ),
    # FIX: I should fix the ordering. From lowest to highest range
    renda_merged = stringr::str_replace(renda_merged, "De", "From") |>
      stringr::str_replace("até", "to") |>
      stringr::str_replace(
        "Mais de",
        "Over"
      ) |>
      stringr::str_replace("Menos do que", "Less than")
  ) |>
  dplyr::distinct(email, .keep_all = TRUE) |>
  gtsummary::tbl_summary(
    by = group,
    label = list(
      group = "Arm",
      age = "Age (in years)",
      cor = "Skin color",
      sexo = "Sex",
      orientacao_sexual = "Sexual orientation",
      estado_civil = "Marital status",
      filhos = "Has children",
      renda_merged = "Family income (in Brazilian reais)",
      grau_de_escolaridade = "Education level",
      trabalho = "Currently working"
    ),
    missing = "ifany",
    include = c(
      group, age, cor, sexo,
      orientacao_sexual, estado_civil, filhos, renda_merged,
      grau_de_escolaridade, trabalho
    )
  ) |>
  gtsummary::add_p()


# TODO: Pre- and post-treatment data using median and interquartile
# ranges
df |>
  tidyr::pivot_longer(
    cols = c(pre, post),
    names_to = "prepost",
    # NOTE: Make the assessment time variable prettier
    names_transform = function(x) {
      stringr::str_to_title(x) |>
        factor(
          levels = c("Pre", "Post"),
          labels = c("Pre-treatment", "Post-treatment")
        )
    },
    values_to = "score"
  ) |>
  tidyr::pivot_wider(
    names_from = outcome,
    values_from = score
  ) |>
  # NOTE: I need that to get rid of unnecessary rows with false NAs
  dplyr::group_by(email, group, prepost) |>
  dplyr::summarize(
    dplyr::across(
      c(`PHQ-9`, `GAD-7`, `UCLA-3`),
      \(x) max(x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  # NOTE: This tbl_strata function makes it possible to stratify a
  # summary table using more than one grouping variable
  gtsummary::tbl_strata(
    strata = prepost,
    .tbl_fun =
      ~ .x |> gtsummary::tbl_summary(
        by = group,
        missing = "ifany",
        type = list(
          `PHQ-9` = "continuous",
          `GAD-7` = "continuous",
          `UCLA-3` = "continuous"
        ),
        include = c(`PHQ-9`, `GAD-7`, `UCLA-3`)
      )
  )
