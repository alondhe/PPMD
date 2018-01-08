source("C:/Users/admin_alondhe2/Desktop/setCredentials.R")

fftempdir <- "A:/fftemp/ppmd"
if (!dir.exists(fftempdir))
{
  dir.create(fftempdir, recursive = TRUE)
}
options(fftempdir = fftempdir)

cdmDatabaseSchemas <- list(
  #list(key = "OPTUMEXTSES_V654", value = "CDM_OPTUM_EXTENDED_DOD_V654.dbo"),
  list(key = "OPTUMEXTSES_V675", value = "CDM_OPTUM_EXTENDED_SES_V675.dbo")
)

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = Sys.getenv("cdmDbms"), 
                                                                server = Sys.getenv("cdmServer"), 
                                                                port = Sys.getenv("cdmServerPort"),
                                                                user = Sys.getenv("cdmUser"),
                                                                password = Sys.getenv("cdmPassword"))

for (cdmDb in cdmDatabaseSchemas)
{
  run(cdmDb = cdmDb, connectionDetails = connectionDetails)
}