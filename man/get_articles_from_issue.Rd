% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrapers.R
\name{get_articles_from_issue}
\alias{get_articles_from_issue}
\title{Scraping articles URLs from the ToC of OJS issues}
\usage{
get_articles_from_issue(input_url, verbose = FALSE)
}
\arguments{
\item{input_url}{Character vector.}

\item{verbose}{Logical.}
}
\value{
A long-format dataframe with the url you provided (input_url) and the articles url scrapped (output_url)
}
\description{
Takes a vector of OJS (issue) URLs and scrapes the links to articles from the issues table of content
}
\examples{
\donttest{
issues <- c(
   'https://revistas.ucn.cl/index.php/saludysociedad/issue/view/65',
   'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/issue/view/31'
)
articles <- ojsr::get_articles_from_issue(input_url = issues, verbose = TRUE)
}
}
