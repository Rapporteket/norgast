% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NorgastPreprosesser.R
\name{NorgastPreprosess}
\alias{NorgastPreprosess}
\title{Denne funksjonen definerer en del nye (sammensatte) variabler relevante for rapporter i NORGAST
og rensker opp i andre.}
\usage{
NorgastPreprosess(RegData, behold_kladd = FALSE)
}
\arguments{
\item{RegData}{En dataramme med alle nødvendige variabler fra registeret}
}
\value{
Data En list med det filtrerte datasettet og sykehusnavnet som tilsvarer reshID
}
\description{
Må ha tilgang til filen Helligdager2008-2022.csv
}
