library(CDMConnector)
library(dplyr)
library(ggplot2)
library(checkmate)

validateCohortTableTypes <- function(cdm, cohortTableName) {
  dataTypes <- cdm[[cohortTableName]] %>%
    head() %>%
    dplyr::collect()
  
  checks <- checkmate::makeAssertCollection()
  checkmate::assertIntegerish(dataTypes$cohort_definition_id, null.ok = FALSE, add = checks)
  checkmate::assertIntegerish(dataTypes$subject_id, null.ok = FALSE, add = checks)
  checkmate::assertDate(dataTypes$cohort_start_date, null.ok = FALSE, add = checks)
  checkmate::assertDate(dataTypes$cohort_end_date, null.ok = FALSE, add = checks)
  checkmate::reportAssertions(collection = checks)
  
  message("cohort table data types: OK")
}

startDateCheck <- function(cdm, cohortTableName, targetCohortId) {
  subjects <- cdm[[cohortTableName]] %>%
    dplyr::group_by(subject_id, cohort_definition_id) %>%
    dplyr::mutate(startDateCheckTarget = dplyr::case_when(
      .data$cohort_definition_id != targetCohortId ~ .data$cohort_start_date,
      .default = as.Date("0001-01-01")
    )) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$cohort_definition_id != targetCohortId) %>%
    dplyr::mutate(startDateCheck = .data$startDateCheckTarget < cohort_start_date) %>%
    dplyr::filter(.data$startDateCheck) %>%
    dplyr::collect()
  
  
  n <- subjects %>%
    nrow()
  
  message(sprintf("%i event cohorts start before target cohort begins", n))
  return(subjects)
}

endDateCheck <- function(cdm, cohortTableName, targetCohortId) {
  subjects <- cdm[[cohortTableName]] %>%
    dplyr::group_by(subject_id, cohort_definition_id) %>%
    dplyr::mutate(endDateCheckTarget = dplyr::case_when(
      .data$cohort_definition_id != targetCohortId ~ .data$cohort_start_date,
      .default = as.Date("2999-12-31")
    )) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$cohort_definition_id != targetCohortId) %>%
    dplyr::mutate(endDateCheck = .data$cohort_start_date > .data$endDateCheckTarget) %>%
    dplyr::filter(.data$endDateCheck) %>%
    dplyr::collect()
  
  
  n <- subjects %>%
    nrow()
  
  message(sprintf("%i event cohorts start after target cohort ends", n))
  return(subjects)
}

durationCheck <- function(cdm, cohortTableName, targetCohortId) {
  subjects <- cdm[[cohortTableName]] %>%
    dplyr::mutate(eraDuration = .data$cohort_end_date - .data$cohort_start_date) %>%
    dplyr::filter(.data$eraDuration <= 0) %>%
    dplyr::collect()
  
  n <- subjects %>%
    nrow()
  
  message(sprintf("%i subjects have cohort with duration of 0 days", n))
  return(subjects)
}

duplicateCheck <- function(cdm, cohortTableName) {
  subjects <- cdm[[cohortTableName]] %>%
    dplyr::group_by(
      .data$subject_id,
      .data$cohort_definition_id,
      .data$cohort_start_date,
      .data$cohort_end_date) %>%
    dplyr::count() %>%
    dplyr::filter(n > 1) %>%
    dplyr::collect()
  
  n <- subjects %>%
    nrow()
  
  message(sprintf("%i duplicate rows in cohort table", n))
  return(subjects)
}

#' validateCohortTable
#' 
#' Function to validate the cohort_table generated.
#' 
#' @param cdm CDM-reference object
#' @param cohortTableName Name of the cohort table in the cdm-reference object.
#' @param targetCohortId ID of the target cohort
#' 
#' @return list of tibbles with erroneous records in the cohort table.
validateCohortTable <- function(cdm, cohortTableName, targetCohortId) {
  validateCohortTableTypes(cdm, cohortTableName)
  
  return(list(
    startDate = startDateCheck(cdm, cohortTableName, targetCohortId),
    endDate = endDateCheck(cdm, cohortTableName, targetCohortId),
    duration = durationCheck(cdm, cohortTableName, targetCohortId),
    duplicates = duplicateCheck(cdm, cohortTableName)
  ))
}
