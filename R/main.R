
#' Create Target and Comparator Cohorts
#' @param cdmDb
#' @param scratchDatabaseSchema
#' @param connectionDetails
#' 
#' @export
createCohorts <- function(cdmDb, scratchDatabaseSchema, connectionDetails)
{
  cohorts <- list(
    list(cohortId = 5441, clause = "and A.episode_length >= 161 and A.episode_length < 259"), 
    list(cohortId = 5442, clause = "and A.episode_length >= 259 and A.episode_length <= 294")
  )
  
  connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  tablePrefix <- paste("al", "ppmd", strsplit(x = cdmDb$key, split = "_")[[1]][1], sep = "_")
  
  for (cohort in cohorts)
  {
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "createCohort.sql", 
                                             packageName = "PPMD", 
                                             dbms = connectionDetails$dbms,
                                             cdm_database_schema = cdmDb$cdmDatabaseSchema,
                                             target_database_schema = scratchDatabaseSchema,
                                             target_cohort_table = paste(tablePrefix, "cohort", sep = "_"),
                                             resultsDatabaseSchema = cdmDb$resultsDatabaseSchema,
                                             whereClause = cohort$clause,
                                             target_cohort_id = cohort$cohortId)
    
    DatabaseConnector::executeSql(connection = connection, sql = sql)
  }
  DatabaseConnector::disconnect(connection = connection)

}

#' @export
createDeliveryCovariateSettings <- function(useDelivery = TRUE) {
  covariateSettings <- list(useDelivery = useDelivery)
  attr(covariateSettings, "fun") <- "getDeliveryCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}

#' @export
getDeliveryCovariateData <- function(connection, 
                                     oracleTempSchema = NULL, 
                                     cdmDatabaseSchema, 
                                     cdmVersion = "5", 
                                     cohortTable = "#cohort_person",
                                     cohortId = -1,
                                     rowIdField = "subject_id", 
                                     covariateSettings,
                                     aggregated = FALSE) 
{
  if (covariateSettings$useDelivery == FALSE) {
    return(NULL)
  }
  if (aggregated)
  {
    stop("Aggregation not supported")
  }
  
  writeLines("***Creating custom delivery covariates***") 
  sql <- loadRenderTranslateSql(sqlFilename = "createDeliveryCovariates.sql", 
                                packageName = "PPMD", 
                                dbms = attr(connection, "dbms"),
                                cohortTable = cohortTable,
                                resultsDatabaseSchema = "cdm_optum_extended_ses_v675.ohdsi_results")
  sql <- SqlRender::translateSql(sql, targetDialect = attr(connection, "dbms"))$sql
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql.ffdf(connection, sql)
  # Convert colum names to camelCase:
  colnames(covariates) <- snakeCaseToCamelCase(colnames(covariates))
  
  # Construct covariate reference:
  covariateRef_1 <- data.frame(covariateId = 1,
                               covariateName = "SB",
                               analysisId = 1,
                               conceptId = 0)
  covariateRef_2 <- data.frame(covariateId = 2,
                               covariateName = "ECT",
                               analysisId = 1,
                               conceptId = 0)
  covariateRef_3 <- data.frame(covariateId = 3,
                               covariateName = "LB or DELIV",
                               analysisId = 1,
                               conceptId = 0)
  covariateRef <- rbind(covariateRef_1, covariateRef_2, covariateRef_3)
  covariateRef <- ff::as.ffdf(covariateRef)
  
  
  # Construct analysis reference:
  analysisRef <- data.frame(analysisId = 1,
                            analysisName = "Delivery Type",
                            domainId = "Procedure",
                            startDay = 0,
                            endDay = 0,
                            isBinary = "Y",
                            missingMeansZero = "Y")
  analysisRef <- ff::as.ffdf(analysisRef)
  
  #Construct analysis reference:
  metaData <- list(sql = sql, call = match.call())
  result <- list(covariates = covariates,
                 covariateRef = covariateRef,
                 analysisRef = analysisRef,
                 metaData = metaData)
  class(result) <- "covariateData"
  
  writeLines("***Custom Delivery covariates created***")
  return(result)
}



#' Run the analysis for a CDM
#' 
#' @param cdmDb
#' @param scratchDatabaseSchema
#' @param connectionDetails
#' 
#' @export
run <- function(cdmDb, scratchDatabaseSchema, connectionDetails)
{
  # Data extraction ----
  tablePrefix <- paste("al", "ppmd", strsplit(x = cdmDb$key, split = "_")[[1]][1], sep = "_")
  
  cdmDatabaseSchema <- cdmDb$cdmDatabaseSchema
  resultsDatabaseSchema <- scratchDatabaseSchema
  exposureTable <- paste(tablePrefix, "cohort", sep = "_")
  outcomeTable <- "cohort"
  cdmVersion <- "5" 
  outputFolder <- "output_5"
  maxCores <- parallel::detectCores()
  # if(!dir.exists(outputFolder)){
  #   dir.create(outputFolder, recursive = TRUE)
  # }
  #setwd(outputFolder)
  
  targetCohortId <- 5441
  comparatorCohortId <- 5442
  outcomeCohortId <- 5440
  outcomeList <- c(outcomeCohortId)
  
  # Default Prior & Control settings ----
  defaultPrior <- Cyclops::createPrior("laplace", 
                                       exclude = c(0),
                                       useCrossValidation = TRUE)
  
  defaultControl <- Cyclops::createControl(cvType = "auto",
                                           startingVariance = 0.01,
                                           noiseLevel = "quiet",
                                           tolerance  = 2e-07,
                                           cvRepetitions = 10,
                                           threads = 1)
  
  # PLEASE NOTE ----
  # If you want to use your code in a distributed network study
  # you will need to create a temporary cohort table with common cohort IDs.
  # The code below ASSUMES you are only running in your local network 
  # where common cohort IDs have already been assigned in the cohort table.
  
  # Get all  Concept IDs for exclusion ----
  
  excludedConcepts <- unique(c(4175637, 
                               OhdsiRTools::getConceptSetConceptIds(baseUrl = Sys.getenv("baseUrl"), setId = 4805),
                               OhdsiRTools::getConceptSetConceptIds(baseUrl = Sys.getenv("baseUrl"), setId = 5305)))
  
  # Get all  Concept IDs for inclusion ----
  
  includedConcepts <- c()
  
  
  # Get all  Concept IDs for exclusion in the outcome model ----
  
  omExcludedConcepts <- c()
  
  # Get all  Concept IDs for inclusion exclusion in the outcome model ----
  
  omIncludedConcepts <- c()
  
  
  # Get all  Concept IDs for empirical calibration ----
  
  negativeControlConcepts <- c()
  
  
  # Create drug comparator and outcome arguments by combining target + comparator + outcome + negative controls ----
  dcos <- CohortMethod::createDrugComparatorOutcomes(targetId = targetCohortId,
                                                     comparatorId = comparatorCohortId,
                                                     excludedCovariateConceptIds = excludedConcepts,
                                                     includedCovariateConceptIds = includedConcepts,
                                                     outcomeIds = c(outcomeList, negativeControlConcepts))
  
  drugComparatorOutcomesList <- list(dcos)
  
  
  
  # Define which types of covariates must be constructed ----
  covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = FALSE,
                                                                  useDemographicsAge = TRUE, 
                                                                  useDemographicsAgeGroup = TRUE,
                                                                  useDemographicsRace = TRUE, 
                                                                  useDemographicsEthnicity = TRUE,
                                                                  useDemographicsIndexYear = TRUE, 
                                                                  useDemographicsIndexMonth = FALSE,
                                                                  useDemographicsPriorObservationTime = TRUE,
                                                                  useDemographicsPostObservationTime = TRUE,
                                                                  useDemographicsTimeInCohort = FALSE,
                                                                  useDemographicsIndexYearMonth = FALSE,
                                                                  useConditionOccurrenceAnyTimePrior = TRUE,
                                                                  useConditionOccurrenceLongTerm = TRUE,
                                                                  useConditionOccurrenceMediumTerm = TRUE,
                                                                  useConditionOccurrenceShortTerm = TRUE,
                                                                  useConditionOccurrenceInpatientAnyTimePrior = TRUE,
                                                                  useConditionOccurrenceInpatientLongTerm = TRUE,
                                                                  useConditionOccurrenceInpatientMediumTerm = TRUE,
                                                                  useConditionOccurrenceInpatientShortTerm = TRUE,
                                                                  useConditionEraAnyTimePrior = TRUE, 
                                                                  useConditionEraLongTerm = TRUE,
                                                                  useConditionEraMediumTerm = TRUE, 
                                                                  useConditionEraShortTerm = TRUE,
                                                                  useConditionEraOverlapping = TRUE, 
                                                                  useConditionEraStartLongTerm = TRUE,
                                                                  useConditionEraStartMediumTerm = TRUE,
                                                                  useConditionEraStartShortTerm = TRUE,
                                                                  useConditionGroupEraAnyTimePrior = TRUE,
                                                                  useConditionGroupEraLongTerm = TRUE,
                                                                  useConditionGroupEraMediumTerm = TRUE,
                                                                  useConditionGroupEraShortTerm = TRUE,
                                                                  useConditionGroupEraOverlapping = TRUE,
                                                                  useConditionGroupEraStartLongTerm = TRUE,
                                                                  useConditionGroupEraStartMediumTerm = TRUE,
                                                                  useConditionGroupEraStartShortTerm = TRUE,
                                                                  useDrugExposureAnyTimePrior = TRUE, 
                                                                  useDrugExposureLongTerm = TRUE,
                                                                  useDrugExposureMediumTerm = TRUE, 
                                                                  useDrugExposureShortTerm = TRUE,
                                                                  useDrugEraAnyTimePrior = TRUE, 
                                                                  useDrugEraLongTerm = TRUE,
                                                                  useDrugEraMediumTerm = TRUE, 
                                                                  useDrugEraShortTerm = TRUE,
                                                                  useDrugEraOverlapping = TRUE, 
                                                                  useDrugEraStartLongTerm = TRUE,
                                                                  useDrugEraStartMediumTerm = TRUE, 
                                                                  useDrugEraStartShortTerm = TRUE,
                                                                  useDrugGroupEraAnyTimePrior = TRUE, 
                                                                  useDrugGroupEraLongTerm = TRUE,
                                                                  useDrugGroupEraMediumTerm = TRUE, 
                                                                  useDrugGroupEraShortTerm = TRUE,
                                                                  useDrugGroupEraOverlapping = TRUE, 
                                                                  useDrugGroupEraStartLongTerm = TRUE,
                                                                  useDrugGroupEraStartMediumTerm = TRUE,
                                                                  useDrugGroupEraStartShortTerm = TRUE,
                                                                  useProcedureOccurrenceAnyTimePrior = TRUE,
                                                                  useProcedureOccurrenceLongTerm = TRUE,
                                                                  useProcedureOccurrenceMediumTerm = TRUE,
                                                                  useProcedureOccurrenceShortTerm = TRUE,
                                                                  useDeviceExposureAnyTimePrior = TRUE, 
                                                                  useDeviceExposureLongTerm = TRUE,
                                                                  useDeviceExposureMediumTerm = TRUE, 
                                                                  useDeviceExposureShortTerm = TRUE,
                                                                  useMeasurementAnyTimePrior = TRUE, 
                                                                  useMeasurementLongTerm = TRUE,
                                                                  useMeasurementMediumTerm = TRUE, 
                                                                  useMeasurementShortTerm = TRUE,
                                                                  useMeasurementValueAnyTimePrior = TRUE,
                                                                  useMeasurementValueLongTerm = TRUE,
                                                                  useMeasurementValueMediumTerm = TRUE,
                                                                  useMeasurementValueShortTerm = TRUE,
                                                                  useMeasurementRangeGroupAnyTimePrior = TRUE,
                                                                  useMeasurementRangeGroupLongTerm = TRUE,
                                                                  useMeasurementRangeGroupMediumTerm = TRUE,
                                                                  useMeasurementRangeGroupShortTerm = TRUE,
                                                                  useObservationAnyTimePrior = TRUE, 
                                                                  useObservationLongTerm = TRUE,
                                                                  useObservationMediumTerm = TRUE, 
                                                                  useObservationShortTerm = TRUE,
                                                                  useCharlsonIndex = TRUE, 
                                                                  useDcsi = TRUE, 
                                                                  useChads2 = TRUE,
                                                                  useChads2Vasc = TRUE, 
                                                                  useDistinctConditionCountLongTerm = TRUE,
                                                                  useDistinctConditionCountMediumTerm = TRUE,
                                                                  useDistinctConditionCountShortTerm = TRUE,
                                                                  useDistinctIngredientCountLongTerm = TRUE,
                                                                  useDistinctIngredientCountMediumTerm = TRUE,
                                                                  useDistinctIngredientCountShortTerm = TRUE,
                                                                  useDistinctProcedureCountLongTerm = TRUE,
                                                                  useDistinctProcedureCountMediumTerm = TRUE,
                                                                  useDistinctProcedureCountShortTerm = TRUE,
                                                                  useDistinctMeasurementCountLongTerm = TRUE,
                                                                  useDistinctMeasurementCountMediumTerm = TRUE,
                                                                  useDistinctMeasurementCountShortTerm = TRUE,
                                                                  useVisitCountLongTerm = TRUE, 
                                                                  useVisitCountMediumTerm = TRUE,
                                                                  useVisitCountShortTerm = TRUE, 
                                                                  longTermStartDays = -365,
                                                                  mediumTermStartDays = -180, 
                                                                  shortTermStartDays = -30, 
                                                                  endDays = -1,
                                                                  includedCovariateConceptIds = c(), 
                                                                  addDescendantsToInclude = TRUE,
                                                                  excludedCovariateConceptIds = excludedConcepts, 
                                                                  addDescendantsToExclude = TRUE,
                                                                  includedCovariateIds = c())
  
  covariateSettingsList <- list(covariateSettings, createDeliveryCovariateSettings(useDelivery = TRUE))
  
  getDbCmDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(washoutPeriod = 365,
                                                                   firstExposureOnly = TRUE,
                                                                   removeDuplicateSubjects = TRUE,
                                                                   studyStartDate = "20120601",
                                                                   studyEndDate = "20170630",
                                                                   excludeDrugsFromCovariates = FALSE,
                                                                   covariateSettings = covariateSettingsList)
  
  createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                      firstExposureOnly = TRUE,
                                                                      washoutPeriod = 365,
                                                                      removeDuplicateSubjects = TRUE,
                                                                      minDaysAtRisk = 1,
                                                                      riskWindowStart = 0,
                                                                      addExposureDaysToStart = FALSE,
                                                                      riskWindowEnd = 180,
                                                                      addExposureDaysToEnd = TRUE)
  
  
  fitOutcomeModelArgs1 <- CohortMethod::createFitOutcomeModelArgs(useCovariates = FALSE,
                                                                  modelType = "logistic",
                                                                  stratified = TRUE,
                                                                  includeCovariateIds = omIncludedConcepts, 
                                                                  excludeCovariateIds = omExcludedConcepts,
                                                                  prior = defaultPrior, 
                                                                  control = defaultControl)
  
  createPsArgs1 <- CohortMethod::createCreatePsArgs(control = defaultControl) # Using only defaults
  trimByPsArgs1 <- CohortMethod::createTrimByPsArgs() # Using only defaults 
  trimByPsToEquipoiseArgs1 <- CohortMethod::createTrimByPsToEquipoiseArgs() # Using only defaults 
  matchOnPsArgs1 <- CohortMethod::createMatchOnPsArgs(caliper = 0.25, caliperScale = "standardized", maxRatio = 1) 
  stratifyByPsArgs1 <- CohortMethod::createStratifyByPsArgs() # Using only defaults 
  
  cmAnalysis1 <- CohortMethod::createCmAnalysis(analysisId = 1,
                                                description = "Early Pregnancy vs On-Time Pregnancy, PPMD",
                                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                createStudyPopArgs = createStudyPopArgs,
                                                createPs = TRUE,
                                                createPsArgs = createPsArgs1,
                                                trimByPs = FALSE,
                                                trimByPsArgs = trimByPsArgs1,
                                                trimByPsToEquipoise = FALSE,
                                                trimByPsToEquipoiseArgs = trimByPsToEquipoiseArgs1,
                                                matchOnPs = TRUE,
                                                matchOnPsArgs = matchOnPsArgs1,
                                                stratifyByPs = FALSE,
                                                stratifyByPsArgs = stratifyByPsArgs1,
                                                computeCovariateBalance = TRUE,
                                                fitOutcomeModel = TRUE,
                                                fitOutcomeModelArgs = fitOutcomeModelArgs1)
  
  
  cmAnalysisList <- list(cmAnalysis1)
  
  # Run the analysis ----
  result <- CohortMethod::runCmAnalyses(connectionDetails = connectionDetails,
                                        cdmDatabaseSchema = cdmDatabaseSchema,
                                        exposureDatabaseSchema = resultsDatabaseSchema,
                                        exposureTable = exposureTable,
                                        outcomeDatabaseSchema = cdmDatabaseSchema,
                                        outcomeTable = outcomeTable,
                                        cdmVersion = cdmVersion,
                                        outputFolder = outputFolder,
                                        cmAnalysisList = cmAnalysisList,
                                        drugComparatorOutcomesList = drugComparatorOutcomesList,
                                        getDbCohortMethodDataThreads = 1,
                                        createPsThreads = 2,
                                        psCvThreads = 20, #min(28, maxCores),
                                        computeCovarBalThreads = min(3, maxCores),
                                        createStudyPopThreads = min(3, maxCores),
                                        trimMatchStratifyThreads = min(10, maxCores),
                                        fitOutcomeModelThreads = max(1, round(maxCores/4)),
                                        outcomeCvThreads = min(4, maxCores),
                                        refitPsForEveryOutcome = FALSE)
  
  ## Summarize the results
  analysisSummary <- CohortMethod::summarizeAnalyses(result)
  head(analysisSummary)
  
  # Perform Empirical Calibration ----
  newSummary <- data.frame()
  
  # Calibrate p-values:
  drugComparatorOutcome <- drugComparatorOutcomesList[[1]]
  for (drugComparatorOutcome in drugComparatorOutcomesList) {
    for (analysisId in unique(analysisSummary$analysisId)) {
      subset <- analysisSummary[analysisSummary$analysisId == analysisId &
                                  analysisSummary$targetId == drugComparatorOutcome$targetId &
                                  analysisSummary$comparatorId == drugComparatorOutcome$comparatorId, ]
      
      negControlSubset <- subset[analysisSummary$outcomeId %in% negativeControlConcepts, ]
      negControlSubset <- negControlSubset[!is.na(negControlSubset$logRr) & negControlSubset$logRr != 0, ]
      
      hoiSubset <- subset[!(analysisSummary$outcomeId %in% negativeControlConcepts), ]
      hoiSubset <- hoiSubset[!is.na(hoiSubset$logRr) & hoiSubset$logRr != 0, ]
      
      if (nrow(negControlSubset) > 10) {
        null <- EmpiricalCalibration::fitMcmcNull(negControlSubset$logRr, negControlSubset$seLogRr)
        
        # View the empirical calibration plot with only negative controls
        EmpiricalCalibration::plotCalibrationEffect(negControlSubset$logRr,
                                                    negControlSubset$seLogRr)
        
        # Save the empirical calibration plot with only negative controls
        plotName <- paste("calEffectNoHois_a",analysisId, "_t", drugComparatorOutcome$targetId, "_c", drugComparatorOutcome$comparatorId, ".png", sep = "")
        EmpiricalCalibration::plotCalibrationEffect(negControlSubset$logRr,
                                                    negControlSubset$seLogRr,
                                                    fileName = file.path(outputFolder, plotName))
        
        # View the empirical calibration plot with  negative controls and HOIs plotted
        EmpiricalCalibration::plotCalibrationEffect(negControlSubset$logRr,
                                                    negControlSubset$seLogRr,
                                                    hoiSubset$logRr, 
                                                    hoiSubset$seLogRr)
        
        # Save the empirical calibration plot with  negative controls and HOIs plotted
        plotName <- paste("calEffect_a",analysisId, "_t", drugComparatorOutcome$targetId, "_c", drugComparatorOutcome$comparatorId, ".png", sep = "")
        EmpiricalCalibration::plotCalibrationEffect(negControlSubset$logRr,
                                                    negControlSubset$seLogRr,
                                                    hoiSubset$logRr, 
                                                    hoiSubset$seLogRr,
                                                    fileName = file.path(outputFolder, plotName))
        
        calibratedP <- calibrateP(null, subset$logRr, subset$seLogRr)
        subset$calibratedP <- calibratedP$p
        subset$calibratedP_lb95ci <- calibratedP$lb95ci
        subset$calibratedP_ub95ci <- calibratedP$ub95ci
        mcmc <- attr(null, "mcmc")
        subset$null_mean <- mean(mcmc$chain[, 1])
        subset$null_sd <- 1/sqrt(mean(mcmc$chain[, 2]))
      } else {
        subset$calibratedP <- NA
        subset$calibratedP_lb95ci <- NA
        subset$calibratedP_ub95ci <- NA
        subset$null_mean <- NA
        subset$null_sd <- NA
      }
      newSummary <- rbind(newSummary, subset)
    }
  }
  
  # Results ----
  drugComparatorOutcome <- drugComparatorOutcomesList[[1]]
  for (drugComparatorOutcome in drugComparatorOutcomesList) {
    for (analysisId in unique(analysisSummary$analysisId)) {
      currentAnalysisSubset <- analysisSummary[analysisSummary$analysisId == analysisId &
                                                 analysisSummary$targetId == drugComparatorOutcome$targetId &
                                                 analysisSummary$comparatorId == drugComparatorOutcome$comparatorId &
                                                 analysisSummary$outcomeId %in% outcomeList, ]
      
      for(currentOutcomeId in unique(currentAnalysisSubset$outcomeId)) {
        outputImageSuffix <- paste0("_a",analysisId, "_t", currentAnalysisSubset$targetId, "_c", currentAnalysisSubset$comparatorId, "_o", currentOutcomeId, ".png")
        
        cohortMethodFile <- result$cohortMethodDataFolder[result$target == currentAnalysisSubset$targetId &
                                                            result$comparatorId == currentAnalysisSubset$comparatorId &
                                                            result$outcomeId == currentOutcomeId &
                                                            result$analysisId == analysisId]
        
        cohortMethodData <- loadCohortMethodData(cohortMethodFile)
        
        studyPopFile <- result$studyPopFile[result$target == currentAnalysisSubset$targetId &
                                              result$comparatorId == currentAnalysisSubset$comparatorId &
                                              result$outcomeId == currentOutcomeId &
                                              result$analysisId == analysisId]
        
        # Return the attrition table for the study population ----
        studyPop <- readRDS(studyPopFile)
        getAttritionTable(studyPop)
        
        # View the attrition diagram
        drawAttritionDiagram(studyPop, 
                             treatmentLabel = "Target", 
                             comparatorLabel = "Comparator")
        
        # Save the attrition diagram ----
        plotName <- paste0("attritionDiagram", outputImageSuffix);
        drawAttritionDiagram(studyPop, 
                             treatmentLabel = "Target", 
                             comparatorLabel = "Comparator", 
                             fileName = file.path(outputFolder, plotName))
        
        
        psFile <- result$psFile[result$target == currentAnalysisSubset$targetId &
                                  result$comparatorId == currentAnalysisSubset$comparatorId &
                                  result$outcomeId == currentOutcomeId &
                                  result$analysisId == analysisId]
        
        ps <- readRDS(psFile)
        
        # Compute the area under the receiver-operator curve (AUC) for the propensity score model ----
        CohortMethod::computePsAuc(ps)
        
        # Plot the propensity score distribution ----
        CohortMethod::plotPs(ps, 
                             scale = "preference")
        
        # Save the propensity score distribution ----
        plotName <- paste0("propensityScorePlot", outputImageSuffix);
        CohortMethod::plotPs(ps, 
                             scale = "preference",
                             fileName = file.path(outputFolder, plotName))
        
        
        # Inspect the propensity model ----
        propensityModel <- CohortMethod::getPsModel(ps, cohortMethodData)
        head(propensityModel)
        
        
        strataFile <- result$strataFile[result$target == currentAnalysisSubset$targetId &
                                          result$comparatorId == currentAnalysisSubset$comparatorId &
                                          result$outcomeId == currentOutcomeId &
                                          result$analysisId == analysisId]
        strataPop <- readRDS(strataFile)
        
        # View PS With Population Trimmed By Percentile ----
        CohortMethod::plotPs(strataPop, 
                             ps, 
                             scale = "preference")
        
        # Save PS With Population Trimmed By Percentile ----
        plotName <- paste0("propensityScorePlotStrata", outputImageSuffix);
        CohortMethod::plotPs(strataPop, 
                             ps, 
                             scale = "preference",
                             fileName = file.path(outputFolder, plotName))
        
        
        # Get the attrition table and diagram for the strata pop ----
        CohortMethod::getAttritionTable(strataPop)
        
        # View the attrition diagram for the strata pop ----
        CohortMethod::drawAttritionDiagram(strataPop)
        
        # Save the attrition diagram for the strata pop ----
        plotName <- paste0("attritionDiagramStrata", outputImageSuffix);
        CohortMethod::drawAttritionDiagram(strataPop,
                                           fileName = file.path(outputFolder, plotName))
        
        
        # Plot the covariate balance ----
        balanceFile <- result$covariateBalanceFile[result$target == currentAnalysisSubset$targetId &
                                                     result$comparatorId == currentAnalysisSubset$comparatorId &
                                                     result$outcomeId == currentOutcomeId &
                                                     result$analysisId == analysisId]
        balance <- readRDS(balanceFile)
        
        # View the covariate balance scatter plot ----
        CohortMethod::plotCovariateBalanceScatterPlot(balance)
        
        # Save the covariate balance scatter plot ----
        plotName <- paste0("covBalScatter", outputImageSuffix);
        CohortMethod::plotCovariateBalanceScatterPlot(balance,
                                                      fileName = file.path(outputFolder, plotName))
        
        # View the plot of top variables ----
        CohortMethod::plotCovariateBalanceOfTopVariables(balance)
        
        # Save the plot of top variables ----
        plotName <- paste0("covBalTop", outputImageSuffix);
        CohortMethod::plotCovariateBalanceOfTopVariables(balance,
                                                         fileName = file.path(outputFolder, plotName))
        
        
        # Outcome Model ----

        outcomeFile <- result$outcomeModelFile[result$target == currentAnalysisSubset$targetId &
                                                 result$comparatorId == currentAnalysisSubset$comparatorId &
                                                 result$outcomeId == currentOutcomeId &
                                                 result$analysisId == analysisId]
        outcomeModel <- readRDS(outcomeFile)

        # Calibrated results -----
        outcomeSummary <- newSummary[newSummary$targetId == currentAnalysisSubset$targetId &
                                       newSummary$comparatorId == currentAnalysisSubset$comparatorId &
                                       newSummary$outcomeId == currentOutcomeId &
                                       newSummary$analysisId == analysisId, ]

        outcomeSummaryOutput <- data.frame(outcomeSummary$rr,
                                           outcomeSummary$ci95lb,
                                           outcomeSummary$ci95ub,
                                           outcomeSummary$logRr,
                                           outcomeSummary$seLogRr,
                                           outcomeSummary$p,
                                           outcomeSummary$calibratedP,
                                           outcomeSummary$calibratedP_lb95ci,
                                           outcomeSummary$calibratedP_ub95ci,
                                           outcomeSummary$null_mean,
                                           outcomeSummary$null_sd)

        colnames(outcomeSummaryOutput) <- c("Estimate",
                                            "lower .95",
                                            "upper .95",
                                            "logRr",
                                            "seLogRr",
                                            "p",
                                            "cal p",
                                            "cal p - lower .95",
                                            "cal p - upper .95",
                                            "null mean",
                                            "null sd")

        rownames(outcomeSummaryOutput) <- "treatment"

        # View the outcome model -----
        outcomeModelOutput <- capture.output(outcomeModel)
        outcomeModelOutput <- head(outcomeModelOutput,n=length(outcomeModelOutput)-2)
        outcomeSummaryOutput <- capture.output(printCoefmat(outcomeSummaryOutput))
        outcomeModelOutput <- c(outcomeModelOutput, outcomeSummaryOutput)
        writeLines(outcomeModelOutput)
      }
    }
  }
      
}

modelling <- function()
{

}