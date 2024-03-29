% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flag_adherent.R
\name{flag_adherent}
\alias{flag_adherent}
\title{flag_adherent}
\usage{
flag_adherent(time, adherent_method = c("adherent_hours_between"), ...)
}
\arguments{
\item{time}{a numeric vector containing sequences of minute values in a day.}

\item{adherent_method}{one of the following "adherent_hours_between" (other adherence rule could be integrated into the package).}

\item{...}{additional parameters required by the selected adherence criteria.}
}
\value{
boolean vector, adherent minutes encoded as T, and non-adherent encoded as F.
}
\description{
flag_adherent
}
\details{
Flag adherent hours in the day. So far only the "adherent_hours_between" rule is implemented.
}
