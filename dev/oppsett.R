devtools::install("../rapbase/.")

devtools::install(upgrade = FALSE, dependencies = FALSE)

Sys.setenv(FALK_EXTENDED_USER_RIGHTS="[{\"A\":80,\"R\":\"LC\",\"U\":106896},{\"A\":80,\"R\":\"SC\",\"U\":105593},{\"A\":81,\"R\":\"LC\",\"U\":2}]")
Sys.setenv(R_RAP_INSTANCE="QAC")
Sys.setenv(R_RAP_CONFIG_PATH="/home/rstudio/nordicscir/data-raw/config")
# Sys.unsetenv("MYSQL_PORT_LOG")
Sys.setenv(MYSQL_DB_DATA="NordicScirReportDataStaging")
# Sys.setenv(MYSQL_USER="root")
# Sys.setenv(MYSQL_PASSWORD="root")


nordicscir::kjor_NSapper(register='nordicscir')
RegData <- rapbase::loadRegData(
  registryName = "data",
  query="SELECT * FROM eq5dlformdatacontract",
  dbType="mysql")
