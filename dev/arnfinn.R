
sship::dec(
  "c://Users/ast046/Downloads/norgast169acafa3.sql.gz__20250508_152734.tar.gz",
  keyfile = "p://.ssh/id_rsa",
  target_dir = "c://Users/ast046/Downloads/."
)



Sys.setenv(FALK_EXTENDED_USER_RIGHTS = "[{\"A\":80,\"R\":\"SC\",\"U\":102966},{\"A\":80,\"R\":\"LU\",\"U\":4219765},{\"A\":80,\"R\":\"LC\",\"U\":4219765},{\"A\":80,\"R\":\"LC\",\"U\":700328}]")
Sys.setenv(MYSQL_DB_LOG = "db_log")
Sys.setenv(MYSQL_DB_AUTOREPORT = "db_autoreport")
Sys.setenv(MYSQL_DB_DATA = "norgast")
Sys.setenv(FALK_APP_ID = "80")
Sys.setenv(FALK_USER_EMAIL = "jesus@sky.no")
Sys.setenv(FALK_USER_FULLNAME = "Arnie")
Sys.setenv(R_RAP_INSTANCE = "QAC")
Sys.setenv(R_RAP_CONFIG_PATH = paste0(getwd(), "/dev/config"))

norgast::norgastApp(logAsJson = FALSE, browser = TRUE)
