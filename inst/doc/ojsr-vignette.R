## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(knitr)

## -----------------------------------------------------------------------------

# first, load the library
library(ojsr)

# we'll use dplyr and ggplot later in this vignette
library(tidyverse)

# our collection of journals
journals <- data.frame ( cbind(
    name = c( 
      "Psicodebate",
      "Odisea"
      ),
    url = c(
      "https://dspace.palermo.edu/ojs/index.php/psicodebate/issue/archive",
      "https://publicaciones.sociales.uba.ar/index.php/odisea"
      )
  ), stringsAsFactors = FALSE
)


## ----funciones get------------------------------------------------------------

# we are using the journal url as input to retrieve issues
issues <- ojsr::get_issues_from_archive(input_url = journals$url) 

# we are using the issues url we just scraped as input to retrieve articles
articles <- ojsr::get_articles_from_issue(input_url = issues$output_url)

# we are using the articles url we just scraped as the input to retrieve metadata
metadata <- ojsr::get_html_meta_from_article(input_url = articles$output_url)


## -----------------------------------------------------------------------------

# we are including the base_url on each table to simplify joining
journals$base_url <- ojsr::parse_base_url(journals$url)
issues$base_url <- ojsr::parse_base_url(issues$input_url)
articles$base_url <- ojsr::parse_base_url(articles$input_url)
metadata$base_url <- ojsr::parse_base_url(metadata$input_url)

# a journal / issue / articles / metadata table
journals %>%
  left_join( 
    issues %>% count( base_url , name="n_issues") , 
    by="base_url") %>%
  left_join( 
    articles %>% count( base_url , name="n_articles") , 
    by="base_url") %>%
  left_join( 
    metadata %>% count( base_url , name="n_metadata") , 
    by="base_url") %>%
  select( name, n_issues, n_articles, n_metadata )


## ----fig.height=3, fig.width=7------------------------------------------------

metadata %>% 
  filter(
      meta_data_name=="citation_keywords", 
      meta_data_xmllang=="es",
      trimws(meta_data_content)!=""
    ) %>% # filtering keywords
  group_by(base_url, keyword = meta_data_content) %>% 
  tally(sort=TRUE) %>% top_n(wt = n, n = 3) %>% # 3 most frequent keywords by journal
  left_join( journals , by="base_url") %>% # let's include the journal names
  ggplot(aes(x=reorder(keyword,n),y=n)) + 
    facet_wrap(~name, scales = "free") + 
    geom_bar(stat = "identity") + 
    coord_flip()


## -----------------------------------------------------------------------------

journals <- c( 
  'https://dspace.palermo.edu/ojs/index.php/psicodebate/issue/archive', # issue archive
  'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2903' # article
)

issues <- ojsr::get_issues_from_archive(input_url = journals)


## -----------------------------------------------------------------------------

issues <- c( 
  'https://revistas.ucn.cl/index.php/saludysociedad/issue/view/65', # issue including ToC
  'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/issue/view/31' # no ToC nor links
)

articles <- ojsr::get_articles_from_issue(input_url = issues) 


## -----------------------------------------------------------------------------

journals <- c( 
  'https://revistapsicologia.uchile.cl/index.php/RDP/', 
  'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/issue/current' 
)

criteria <- "psicologia"
articles_search <- ojsr::get_articles_from_search(input_url = journals, search_criteria = criteria)


## -----------------------------------------------------------------------------

articles <- c( 
  'https://revistapsicologia.uchile.cl/index.php/RDP/article/view/55657', # galleys pdf and mp3
  'https://dspace.palermo.edu/ojs/index.php/psicodebate/article/view/516/311' # inline reader
)

galleys <- ojsr::get_galleys_from_article(input_url = articles) 


## -----------------------------------------------------------------------------

articles <- c( 
  'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2137', # article
  'https://dspace.palermo.edu/ojs/index.php/psicodebate/article/view/516/311' # xml galley
)

metadata <- ojsr::get_html_meta_from_article(input_url = articles) 


## -----------------------------------------------------------------------------

articles <- c(  
  'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2137', # article
  'https://dspace.palermo.edu/ojs/index.php/psicodebate/article/view/516/311' # xml galley
)

metadata_oai <- ojsr::get_oai_meta_from_article(input_url = articles)


## -----------------------------------------------------------------------------

mix_links <- c(
   'https://dspace.palermo.edu/ojs/index.php/psicodebate/issue/archive',
   'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2903'
)

base_url <- ojsr::parse_base_url(input_url = mix_links)


## -----------------------------------------------------------------------------

mix_links <- c(
   'https://dspace.palermo.edu/ojs/index.php/psicodebate/issue/archive',
   'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2903'
)

oai_url <- ojsr::parse_oai_url(input_url = mix_links)


