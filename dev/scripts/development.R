
## Agentic context
# Scaffold only when initializing a repository. Existing repository-owned
# customizations are intentionally not overwritten.
if (!file.exists("AGENTS.md")) {
  reproducibleai::use_agentic_context(
    path = ".",
    profiles = c("base", "r-package")
  )
}

# AGENTS.md routes Codex to the maintained repository context. Bootstrap prompts
# and transcript exports are no longer part of the development workflow.
reproducibleai::validate_agentic_context(path = ".", strict = TRUE)

# `renv`
install.packages("remotes")
remotes::install_github("MVR-GIS/reproducibleai")

## Configure `renv`
install.packages("renv")
update.packages()
renv::init()
file.exists("renv.lock")

## Workflow to update `renv`
update.packages()
renv::snapshot()
renv::status()

## Develop Code
devtools::document()
devtools::load_all()

## Build vignettes, when present
if (dir.exists("vignettes")) {
  devtools::build_vignettes()
}

## Ensure DESCRIPTION stays in sync with NAMESPACE
attachment::att_amend_desc()

## Test and check
devtools::test()
devtools::check()


