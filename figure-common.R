library(data.table)
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
i.vec <- seq_along(degree.list)
year.vec <- 1:4
pairs.dt <- data.table(expand.grid(i=i.vec, j=i.vec, year=year.vec))[i<j]
common.dt.list <- list()
make.pair <- function(x)paste(sort(x), collapse="-")
for(pair.i in 1:nrow(pairs.dt)){
  pair.row <- pairs.dt[pair.i]
  pair.unsort <- pair.row[, names(degree.list)[c(i, j)] ]
  pair.name <- make.pair(pair.unsort)
  f <- function(x)unique(degree.list[[x]]$course)
  common <- pair.row[, intersect(f(i), f(j)) ]
  some.colors <- c(#dput(RColorBrewer::brewer.pal(Inf, "Set3"))
    "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", 
    "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")
  common.colors <- structure(rep(some.colors, l=length(common)), names=common)
  pair.dir <- file.path(
    "figure-common", "pairs", gsub(" ", "_", pair.name))
  dir.create(pair.dir, showWarnings = FALSE, recursive = TRUE)
  out.png <- file.path(
    pair.dir,
    paste0(pair.row$year, ".png"))
  png(out.png, width=20, height=12, units="in", res=100)
  par(mfrow=c(1, 2))
  for(i.name in c("i", "j")){
    degree.i <- pair.row[[i.name]]
    degree <- names(degree.list)[[degree.i]]
    select.dt <- data.table(degree)
    deg.names <- data.list$deg[select.dt, course, on=names(select.dt)]
    deg.dt <- getDT(deg.names)
    node.dt <- deg.dt[year <= pair.row$year]
    node.names <- node.dt[, paste(subject, number)]
    n.common <- length(intersect(common, node.names))
    degree.reqs <- degree.list[[degree]][
      node.names, on="course"][requires %in% node.names]
    ft <- unique(degree.reqs[, cbind(requires, course)])
    g <- graph::ftM2graphNEL(
      ft, edgemode='directed', V=node.names)
    is.max <- node.dt[, year==max(year)]
    sgL = list(
      list(graph=graph::subGraph(node.names[is.max], g),
           cluster = FALSE, attrs = c(rank="sink"))
    )
    att = list(graph = list(rankdir = "LR", rank = ""))
    att$node$shape <- "plaintext"
    nA = Rgraphviz::makeNodeAttrs(
      g, fillcolor=common.colors[node.names])
    nA$shape <- structure(rep("ellipse", length(node.names)), names=node.names)
    degree.png <- file.path("figure-common", paste0(degree, ".png"))
    Rgraphviz::plot(
      g, attrs = att, nodeAttrs=nA, subGList = sgL,
      main=sprintf(
        "%s year %d, n.common=%d",
        degree, pair.row$year, n.common))
  }
  dev.off()
  common.dt.list[[pair.i]] <- data.table(
    year=pair.row$year,
    pair.name,
    i.name=pair.unsort[[1]],
    j.name=pair.unsort[[2]],
    n.common)
}
common.dt <- do.call(rbind, common.dt.list)

all.majors.dt.list <- list()
for(previous.major in names(degree.list)){
  previous.dt <- common.dt[
    i.name==previous.major | j.name==previous.major][order(year, -n.common)]
  previous.dt[, new.name := ifelse(i.name==previous.major, j.name, i.name)]
  major.dt <- data.table(previous.major)
  for(y in year.vec){
    year.dt <- previous.dt[J(y), on="year"]
    pre.xt <- year.dt[, .(
      new.major=sprintf(
        '<a href="%s">%s</a>',
        data.list$programs[new.name, href, on="degree"],
        new.name),
      classes=sprintf(
        '<a href="pairs/%s/%d.png">%d</a>',
        sapply(new.name, function(one.name){
          maybe.spaces <- make.pair(c(one.name, previous.major))
          gsub(" ", "_", maybe.spaces)
        }),
        y,
        n.common)
    )]
    xt <- xtable::xtable(pre.xt)
    major.dt[[paste("year", y)]] <- print(
      xt,
      include.rownames=FALSE,
      type="html", sanitize.text.function=identity, file="/dev/null")
  }
  all.majors.dt.list[[previous.major]] <- major.dt
}
all.majors.dt <- do.call(rbind, all.majors.dt.list)
all.majors.xt <- xtable::xtable(all.majors.dt)
all.majors.html <- print(
  all.majors.xt,
  include.rownames=FALSE,
  type="html",
  sanitize.text.function=identity,
  file="/dev/null")
cat('<title>NAU SICCS Programs</title>
<p>This web page shows how much overlap there is between
classes which are required by each SICCS undergraduate major.
It is meant to help students find out how much work it would
be to switch majors. Suggested use:</p>
<ul>
<li>Find the row that corresponds to your current/previous major
  by looking in the first column of the table below.</li>
<li>Look for the column that corresponds to how many years
  you have completed in your current/previous major.</li>
<li>Inside that box the other SICCS majors are ranked by
  the number of required classes that you have already completed.</li>
<li>Click the new major to see its requirements in the course catalog.</li>
<li>Click the number of classes in common to see a pre-requisite graph
  which compares the two majors.</li>
</ul>
<p>For example, say your current/previous major is Computer Science (row 3),
and you have completed two years of classes (year 2 column).
Then you can see that there are 13 classes that you have already completed
which are also requirements which could be used if you wanted to switch to
(1) Computer Engineering or (2) Informatics.
Applied Computer Science would also be a reasonable choice
(12 classes in common).</p>
<p><a href="https://github.com/NAU-CS/curriculum-graphs#30-oct-2020">Source
code</a> by <a href="http://tdhock.github.io/">Toby Dylan Hocking</a>.</p>
', all.majors.html, file=file.path("figure-common", "index.html"))
