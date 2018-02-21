#source("/home/alondhe2/Desktop/setCredentials.R")
#fftempdir <- "/ebs/fftemp/ppmd"

source("C:/Users/admin_alondhe2/Desktop/setCredentials.R")
fftempdir <- "/fftemp/ppmd"

if (!dir.exists(fftempdir))
{
  dir.create(fftempdir, recursive = TRUE)
}
options(fftempdir = fftempdir)

outputFolder <- "output_4"
if(!dir.exists(outputFolder)){
  dir.create(outputFolder, recursive = TRUE)
}

cdmDb <- list(
  key = "OPTUMEXTSES_V694", 
       cdmDatabaseSchema = "CDM_OPTUM_EXTENDED_SES_V694.dbo",
       resultsDatabaseSchema = "CDM_OPTUM_EXTENDED_SES_V694.ohdsi_results")
  
excludedConcepts <- unique(c(4175637,196168, 
                             OhdsiRTools::getConceptSetConceptIds(baseUrl = Sys.getenv("baseUrl"), setId = 5759),
                             OhdsiRTools::getConceptSetConceptIds(baseUrl = Sys.getenv("baseUrl"), setId = 2560),
                             OhdsiRTools::getConceptSetConceptIds(baseUrl = Sys.getenv("baseUrl"), setId = 3254),
                             OhdsiRTools::getConceptSetConceptIds(baseUrl = Sys.getenv("baseUrl"), setId = 4805),
                             OhdsiRTools::getConceptSetConceptIds(baseUrl = Sys.getenv("baseUrl"), setId = 5305)))

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = Sys.getenv("cdmDbms"), 
                                                                server = Sys.getenv("cdmServer"), 
                                                                port = Sys.getenv("cdmServerPort"),
                                                                user = Sys.getenv("cdmUser"),
                                                                password = Sys.getenv("cdmPassword"))
scratchDatabaseSchema <- "scratch.dbo"


# createCohorts(cdmDb = cdmDb,
#               scratchDatabaseSchema = scratchDatabaseSchema,
#               connectionDetails = connectionDetails)
run(cdmDb = cdmDb,
    scratchDatabaseSchema = scratchDatabaseSchema,
    connectionDetails = connectionDetails,
    excludedConcepts = excludedConcepts,
    outputFolder = outputFolder,
    negativeControlConcepts = c(5975, 5976, 5977, 5978, 5979, 5980, 5981))
      #c(375281,4040923,4123726,139099,81454,4209845,28060))



# bal <- readRDS("output_5/Bal_l1_s1_p1_t5441_c5442_s1_o5440.rds")
# bal[bal$covariateId == 1,]
