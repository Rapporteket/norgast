devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE, dependencies = FALSE)

Sys.setenv(FALK_EXTENDED_USER_RIGHTS="[{\"A\":80,\"R\":\"LU\",\"U\":601225},{\"A\":80,\"R\":\"SC\",\"U\":4211928},{\"A\":80,\"R\":\"SC\",\"U\":601225},{\"A\":80,\"R\":\"LC\",\"U\":601225}, {\"A\":80,\"R\":\"LU\",\"U\":4211928}]")
# Sys.setenv(FALK_EXTENDED_USER_RIGHTS="[{\"A\":80,\"R\":\"LC\",\"U\":106896},{\"A\":80,\"R\":\"SC\",\"U\":105593},{\"A\":80,\"R\":\"LC\",\"U\":105593},{\"A\":80,\"R\":\"LC\",\"U\":2}]")
Sys.setenv(FALK_EXTENDED_USER_RIGHTS="[{\"A\":80,\"R\":\"LC\",\"U\":601225},
           {\"A\":80,\"R\":\"SC\",\"U\":601225},
           {\"A\":81,\"R\":\"LC\",\"U\":4211928}]")
Sys.setenv(R_RAP_INSTANCE="QAC")
Sys.setenv(R_RAP_CONFIG_PATH="/home/rstudio/norgast/data-raw/config")
# Sys.setenv(MYSQL_DB_DATA="NoRGastReportDataStaging")

norgast::norgastApp()

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
