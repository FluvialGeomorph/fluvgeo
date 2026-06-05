
## Configure Package
library(reproducibleai)
recipes <- instructions_recipes()
use_instructions(recipes$r_package_governed)

## Chat instructions
Target repo: FluvialGeomorph/fluvgeo  
Read `dev/instructions/CHAT_INSTRUCTIONS.md` and follow the instruction 
modules listed under **Selected instruction modules (read in order)**.

## Update Chat History
reproducibleai::extract_copilot_chat(file.path(Sys.getenv("USERPROFILE"), "Downloads", "copilot_export.zip"))
  
## Develop Code
devtools::document()
devtools::load_all()

## Build vignettes
devtools::build_vignettes()

## Ensure DESCRIPTION stays in sync with NAMESPACE
attachment::att_amend_desc()

## Check
devtools::check(vignettes = FALSE, args = c("--no-tests", "--no-examples"))


