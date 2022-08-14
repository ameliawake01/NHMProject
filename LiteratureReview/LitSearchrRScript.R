#Setup
install.packages("remotes")
library(remotes)
install_github("elizagrames/litsearchr", ref="main")
packageVersion("litsearchr")
library(dplyr)
library(ggplot2)
install.packages("ggraph")
library(ggraph)
library(igraph)
library(readr)

#Import naive search results
library(litsearchr)

naive_results <- 
  import_results(
    directory = "/home/amelia-wake/Documents/NHMProject/PossibleDatasets/NaiveResults/",
    file = NULL,
    verbose = TRUE
  )
nrow(naive_results)
colnames(naive_results)
naive_results[1, "title"]

#Getting potential search terms from the keywords
naive_results[1, "keywords"]
sum(is.na(naive_results[, "keywords"]))
keywords <- extract_terms(keywords=naive_results[, "keywords"], method="tagged", min_n=1)

#Getting potential search terms from the titles
extract_terms(text=naive_results[, "title"], method="fakerake", min_freq=3, min_n=2)
stopwords <- get_stopwords("English")
title_terms <- extract_terms(
  text=naive_results[, "title"],
  method="fakerake",
  min_freq=3, min_n=2,
  stopwords=stopwords
)
terms <- unique(c(keywords, title_terms))

#Network analysis
docs <- paste(naive_results[, "title"], naive_results[, "abstract"])
dfm <- create_dfm(elements=docs, features=terms)
g <- create_network(dfm, min_studies=3)

#GGraph
ggraph(g, layout="stress") +
  coord_fixed() +
  expand_limits(x=c(-3, 3)) +
  geom_edge_link(aes(alpha=weight)) +
  geom_node_point(shape="circle filled", fill="white") +
  geom_node_text(aes(label=name), hjust="outward", check_overlap=TRUE) +
  guides(edge_alpha=FALSE)

#Pruning
strengths <- strength(g)

data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength) ->
  term_strengths

cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)

cutoff_cumulative <- find_cutoff(g, method="cumulative", percent=0.8)

cutoff_fig +
  geom_hline(yintercept=cutoff_cumulative, linetype="dashed")

get_keywords(reduce_graph(g, cutoff_cumulative))

cutoff_change <- find_cutoff(g, method="changepoint", knot_num=3)

cutoff_fig +
  geom_hline(yintercept=cutoff_change, linetype="dashed")

g_redux <- reduce_graph(g, cutoff_change[1])
selected_terms <- get_keywords(g_redux)
extra_terms <- c(
  "conventional",
  "europe",
  "diversity",
  "community composition",
  "bacteria",
  "microbes",
  "eukaryote"
)
selected_terms <- c(selected_terms, extra_terms)

#Grouping
grouped_terms <- list(
  soilfauna=selected_terms[c(62, 63, 64)],
  soilproperty=selected_terms[c(1, 6, 29, 39, 60, 61)],
  agriculture=selected_terms[c(3, 4, 5, 11, 13, 14, 20, 22, 31, 32, 36, 41, 51, 53, 56, 57, 58)]
)

#Writing new search words
write_search(
  grouped_terms,
  languages="English",
  exactphrase=TRUE,
  stemming=FALSE,
  closure="left",
  writesearch=FALSE
)





