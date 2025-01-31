devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE, dependencies = FALSE)

Sys.setenv(R_RAP_INSTANCE="QAC")
Sys.setenv(R_RAP_CONFIG_PATH="/home/rstudio/norgast/data-raw/config")

norgast::norgastApp()

# rapbase::runAutoReport()
# Rscript -e "Sys.setenv(R_RAP_INSTANCE=\"QAC\")" -e "rapbase::runAutoReport(dato = Sys.Date()+1, dryRun = TRUE)"

RegData <- rapbase::loadRegData(
  registryName = "data",
  query="SELECT * FROM eq5dlformdatacontract",
  dbType="mysql")

tmp_yml <- yaml::read_yaml("./dev/test.yml")
tmp_json <- jsonlite::serializeJSON(tmp_yml)
query <- paste0("INSERT INTO `autoreport` VALUES ('", tmp_json, "');")

con <- rapbase::rapOpenDbConnection("autoreport")$con
DBI::dbExecute(con, query)
rapbase::rapCloseDbConnection(con)
