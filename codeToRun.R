source("C:/Users/admin_alondhe2/Desktop/setCredentials.R")

fftempdir <- "A:/fftemp/ppmd"
if (!dir.exists(fftempdir))
{
  dir.create(fftempdir, recursive = TRUE)
}
options(fftempdir = fftempdir)

cdmDatabaseSchemas <- list(
  list(key = "OPTUMEXTSES_V675", 
       cdmDatabaseSchema = "CDM_OPTUM_EXTENDED_SES_V675.dbo",
       resultsDatabaseSchema = "CDM_OPTUM_EXTENDED_SES_V675.ohdsi_results")
  # list(key = "OPTUMEXTDOD_V654", 
  #      cdmDatabaseSchema = "CDM_OPTUM_EXTENDED_DOD_V654.dbo",
  #      resultsDatabaseSchema = "CDM_OPTUM_EXTENDED_DOD_V654.ohdsi_results")
)

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = Sys.getenv("cdmDbms"), 
                                                                server = Sys.getenv("cdmServer"), 
                                                                port = Sys.getenv("cdmServerPort"),
                                                                user = Sys.getenv("cdmUser"),
                                                                password = Sys.getenv("cdmPassword"))
scratchDatabaseSchema <- "scratch.dbo"

for (cdmDb in cdmDatabaseSchemas)
{
  # createCohorts(cdmDb = cdmDb,
  #               scratchDatabaseSchema = scratchDatabaseSchema,
  #               connectionDetails = connectionDetails)
  run(cdmDb = cdmDb,
      scratchDatabaseSchema = scratchDatabaseSchema,
      connectionDetails = connectionDetails)
}