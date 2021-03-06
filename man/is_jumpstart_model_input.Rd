% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jumpstart_utils.R
\name{is_jumpstart_model_input}
\alias{is_jumpstart_model_input}
\title{Determines if `model_id` and `version` input are for JumpStart.}
\usage{
is_jumpstart_model_input(model_id, version)
}
\arguments{
\item{model_id}{(str): Optional. Model ID of the JumpStart model.}

\item{version}{(str): Optional. Version of the JumpStart model.}
}
\description{
This method returns True if both arguments are not None, false if both arguments
             are None, and raises an exception if one argument is None but the other isn't.
}
