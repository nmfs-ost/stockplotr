# Contributing to `stockplotr`

Do you have an idea that would improve `stockplotr`? *Consider making a contribution!* 
We welcome ideas for improving not only our code, but also our documentation,
tutorial, and any other material associated with `stockplotr`. Here are some
options and tips for doing so.

Note: To make any contribution, you must agree to abide by the [Code of Conduct](https://github.com/nmfs-ost/stockplotr/blob/main/CODE_OF_CONDUCT.md).

## Contributing ideas: code, documentation, etc.

### Recommended workflow: fork & submit a pull request

The most efficient way to contribute an idea is to fork `asar`, make your 
suggested changes on a local branch, and then submit a pull request to the main branch. 
This will allow the developers to easily evaluate your suggested changes. 
Please see the [GitHub Docs' "Contributing to a project" page](https://docs.github.com/en/get-started/exploring-projects-on-github/contributing-to-a-project) 
for step-by-step guidance in using this workflow.

We also follow a few other practices that will help us expedite the review process.
After completing the bug fix or feature, please complete the following:

1. Run `devtools::test()` to verify the package checks are passing (this includes tests).

2. If including a feature, please add a test using the 
[`{testthat}` package structure](https://testthat.r-lib.org/reference/test_that.html).
If you are unfamiliar with this approach, simply let us know :smile

3. If including a feature that is a new function, please add documentation using
the [`{roxygen2}` package structure](https://roxygen2.r-lib.org/articles/roxygen2.html).

### Recommended practices

-   Write clear, succinct commit messages ([see some tips here](https://opensource.com/article/22/12/git-commit-message))
-   Limit a commit to a few, rather than many, changes. Smaller commits means more commit messages, which is often helpful for documentation.
-   Ensure your base branch is correct. We merge all branches into main.
-   Test that your contributed code will function as expected under different circumstances.
-   Add comments to the code if it's not immediately clear what the purpose of the code is, or how it works.
-   Name branches and title pull requests according to the release indicators in the table below.


#### Release Indicators:

| Indicator | Description |
|-----------|-------------|
| feat | A new feature |
| fix | A bug fix |
| docs | Documentation only changes |
| style | Changes that do not affect the meaning of the code (i.e. white-space, formatting, missing semi-colons) |
| refactor | A code change that neither fixes a bug nor adds a feature |
| perf | A code change that improves performance |
| test | Adding missing or correcting existing tests |
| chore | changes to the build process or auxiliary tools and libraries such as documentation generation |

#### Rules for Indicator Use

- If the body contains the text "BREAKING CHANGE" then MAJOR version is incremented.
- If the type contains feat, then MINOR version is incremented.
- If the type contains a fix, then PATCH version is incremented.
- If the type contains refactor/style/perf/doc/test/chore, then nothing is incremented and no release will be made.

**The above content is modeled after an article on ["Automating Versioning and Releases Using Semantic Release"](https://medium.com/agoda-engineering/automating-versioning-and-releases-using-semantic-release-6ed355ede742) from Agoda Engineering.**

## Contributing bugs

Found a bug? Tell us about it on our [Issues page](https://github.com/nmfs-ost/stockplotr/issues). Before you create an issue, please check that it has not already been resolved (i.e., is a "closed" issue) or documented in our [Frequently Asked Questions (FAQ) vignette](https://nmfs-ost.github.io/stockplotr/articles/faqs.html).

If possible, please submit a reproducible example ([reprex](https://reprex.tidyverse.org/articles/reprex-dos-and-donts.html)) to help us understand the problem better and, ideally, allow us to reproduce your issue.

## Contributing questions

Have a question? Ask it in our [Discussions page](https://github.com/nmfs-ost/stockplotr/discussions). You can categorize it under General, Ideas, Q&A, and more.

## Figure and Table Workflow Guide

This guide summarizes the workflow used by the `plot_x` and `table_x` functions in `R/`.
Use it as a template when building a new figure or table function from the existing package patterns.

### The common starting point

Most functions begin with standardized output from `convert_output()`.
That data is then narrowed to one label or related labels, reshaped if needed, and finally rendered as a plot or table.

The recurring helpers are:

- `filter_data()` to isolate the target label(s) and apply module, era, grouping, faceting, and scaling choices.
- `process_data()` to detect indexing variables, set `group_var`, and decide whether additional variables should become grouping or facetting variables.
- `process_table()` for table-specific label handling and row/column organization.
- `create_rda()` when `make_rda = TRUE`.

### How the figure functions are built

The figure functions in `R/` follow the same basic sequence:

1. **Filter the data.**  
   Pick the relevant label with `filter_data()`.  
   This is where each figure decides whether it needs `year`, `age`, `fleet`, `area`, `sex`, a specific module, or a specific era.

2. **Process the filtered data.**  
   Run `process_data()` to identify the index structure and to decide how the data should be grouped or faceted.

3. **Choose the plot builder.**  
   - `plot_timeseries()` for standard time-series figures
   - `plot_obsvpred()` for observed-vs-predicted index figures
   - `plot_aa()` for age-composition bubble plots
   - `plot_error()` for point/error summaries

4. **Add figure-specific layers.**  
   Examples from the existing functions include:
   - `reference_line()` and `calculate_reference_point()` for biomass and fishing mortality plots
   - `average_age_line()` for abundance/biomass-at-age plots
   - `cohort_line()` for catch-composition plots
   - extra overlays for expected recruitment or stock-recruit curves

5. **Apply the final theme.**  
   Most figures end with `theme_noaa()`.

6. **Optionally export the accessibility package.**  
   If `make_rda = TRUE`, the figure function usually:
   - calculates key quantities,
   - writes them with `export_kqs()`,
   - inserts them into captions and alt text with `insert_kqs()`,
   - and saves the final object with `create_rda()`.

### Figure families in this package

- **Time-series plots:** `plot_biomass()`, `plot_spawning_biomass()`, `plot_recruitment()`, `plot_landings()`, `plot_fishing_mortality()`, `plot_natural_mortality()`
- **Observation/comparison plots:** `plot_index()`, `plot_stock_recruitment()`, `plot_recruitment_deviations()`
- **Age-composition plots:** `plot_abundance_at_age()`, `plot_biomass_at_age()`, `plot_catch_comp()`

### How the table functions are built

The data-driven table functions use a shorter version of the same workflow:

1. **Filter the data.**  
   Use `filter_data()` to isolate the label or labels needed for the table.

2. **Clean and round values.**  
   The existing tables round `estimate` and `uncertainty` before formatting.

3. **Process the table structure.**  
   `process_table()` determines which variables are indexing the data, handles multiple labels, and prepares the data for table formatting.

4. **Merge estimates and uncertainty.**  
   `merge_error()` combines the value and error columns into a single presentation-ready column where needed.

5. **Render the table.**  
   - `table_index()` and `table_landings()` convert the prepared data to `gt` tables and apply `add_theme()`

6. **Optionally export the accessibility package.**  
   As with plots, `make_rda = TRUE` triggers `export_kqs()`, `insert_kqs()`, and `create_rda()`.

### Table families in this package

- **Data-driven tables:** `table_index()`, `table_landings()`
- **Legacy/incomplete table workflow:** `table_bnc()` is present but commented out

### A simple recipe for a new function

When adding a new `plot_x` or `table_x` function, follow this order:

1. Identify the source label(s) in the standardized output.
2. Filter with `filter_data()`.
3. Process with `process_data()` or `process_table()`.
4. Build the figure or table with the helper that matches the display type.
5. Add domain-specific layers or table formatting.
6. Apply the package theme.
7. Export key quantities and the `.rda` package if the function supports accessibility output.
8. Document the workflow in the function's roxygen file and add an example.

If a new figure or table does not fit an existing category, explain the new category and we can try to build the pipeline to incorporate it into the existing workflow.
