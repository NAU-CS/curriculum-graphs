set.seed(123)
nodes1 <- paste(0:7)
nodes2 <- c("a", "b", "c")
nodes3 <- c("foo", "bar", "sars")
ft <- cbind(
  sample(nodes1, 24, replace=TRUE),
  sample(c(nodes2, nodes3), 24, replace=TRUE))
ft <- ft[!duplicated(apply(ft, 1, paste, collapse="")),]
g <- graph::ftM2graphNEL(ft, edgemode='directed')
twocolors <- c("#D9EF8B", "#E0F3F8")
nodeType <- 1 + (graph::nodes(g) %in% nodes1)
nA = Rgraphviz::makeNodeAttrs(g, fillcolor=twocolors[nodeType])
## graph=subGraph with last year nodes.
sgL = list(
  list(graph=graph::subGraph(nodes2, g), cluster = FALSE, attrs = c(rank="sink"))
)
att = list(graph = list(rankdir = "LR", rank = ""))
Rgraphviz::plot(g, attrs = att, nodeAttrs=nA, subGList = sgL)

data.list <- list()
for(data.name in dir("download.graph")){
  f <- file.path("download.graph", data.name)
  data.list[[data.name]] <- data.table::fread(f)
}

getDT <- function(x){
  dt <- nc::capture_first_vec(
    x, 
    subject=".*?", " ",
    number="[0-9]+", as.integer,
    suffix=".*")
  dt[, year := floor(number/100)]
  dt
}

all.reqs <- data.list$req[data.list$deg, on="course", nomatch=0L]
degree.list <- split(all.reqs, all.reqs$degree)
dir.create("figure-bipartite", showWarnings = FALSE)
for(degree in names(degree.list)){
  select.dt <- data.table(degree)
  node.names <- data.list$deg[select.dt, course, on=names(select.dt)]
  degree.reqs <- degree.list[[degree]]
  ft <- unique(degree.reqs[, cbind(requires, course)])
  g <- graph::ftM2graphNEL(ft, edgemode='directed', V=unique(c(node.names, ft)))
  node.dt <- getDT(node.names)
  is.max <- node.dt[, year==max(year)]
  sgL = list(
    list(graph=graph::subGraph(node.names[is.max], g),
         cluster = FALSE, attrs = c(rank="sink"))
  )
  att = list(graph = list(rankdir = "LR", rank = ""))
  att$node$shape <- "plaintext"
  nA$shape <- structure(rep("ellipse", length(node.names)), names=node.names)
  degree.png <- file.path("figure-bipartite", paste0(degree, ".png"))
  png(
    degree.png,
    width=10, height=15, units="in", res=100)
  Rgraphviz::plot(g, attrs = att, nodeAttrs=nA, subGList = sgL)
  dev.off()
  cat(paste0("[[file:", degree.png, "]]\n"))
}
