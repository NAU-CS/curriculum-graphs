library(data.table)
today.dir <- file.path("bs", format(Sys.time(), "%Y-%m-%d"))
siccs.html <- file.path(today.dir, "siccs.html")
if(!file.exists(siccs.html)){
  dir.create(today.dir, showWarnings = FALSE, recursive = TRUE)
  download.file(
    "https://nau.edu/school-of-informatics-computing-and-cyber-systems/",
    siccs.html)
}

bs.dt <- nc::capture_all_str(
  siccs.html,
  nc::field("href", '="', '[^"]+'),
  '">',
  nc::field("B.S.", " ", "[^<]+"))

subject.catNbr.pattern <- list(
  subject="[A-Z]+",
  " ",
  catNbr="[0-9]+[A-Z]?")
Courses.prefix <- "https://catalog.nau.edu/Courses/"
degree.courses.list <- list()
for(bs.i in 1:nrow(bs.dt)){
  bs <- bs.dt[bs.i]
  degree <- bs[["B.S."]]
  bs.dir <- file.path(today.dir, degree)
  details.html <- file.path(bs.dir, "details.html")
  if(!file.exists(details.html)){
    dir.create(bs.dir, showWarnings = FALSE, recursive = TRUE)
    program.html <- file.path(bs.dir, "program.html")
    if(!file.exists(program.html)){
      download.file(bs[["href"]], program.html)
    }
    plan.dt <- nc::capture_all_str(
      program.html,
      nc::field("plan", "=", '[A-Z]+'))
    if(nrow(plan.dt)==0){
      plan.dt <- nc::capture_all_str(
        program.html,
        "nau-catalog ",
        plan='[^"]+')
    }
    u <- paste0(
      "https://catalog.nau.edu/Catalog/details?plan=",
      plan.dt[1, plan])
    download.file(u, details.html)
  }
  course.dt <- suppressWarnings(nc::capture_all_str(
    details.html,
    '<a',
    '.*?',
    nc::field(
      "href", '="', '.*?',
      nc::field("catalogYear", "=", "[0-9]+")
    ),
    ".*?",
    ">",
    subject="[A-Z]+",
    " ",
    catNbr="[0-9]+",
    "</a>"))
  degree.courses.list[[bs.i]] <- course.dt[, data.table(degree, subject, catNbr)]
}
degree.courses <- do.call(rbind, degree.courses.list)

all.courses <- unique(degree.courses[, .(subject, catNbr)])
req.courses.list <- list()
remove.courses.list <- list()
options(warn=2)
while(nrow({
  todo.courses <- all.courses[, data.table(
    subject, catNbr, name=paste(subject, catNbr)
  )[! name %in% names(req.courses.list)] ]
})){
  new.courses.list <- list(prev=all.courses)
  for(course.i in 1:nrow(todo.courses)){
    cat(sprintf("%4d / %4d courses\n", course.i, nrow(todo.courses)))
    course.row <- todo.courses[course.i]
    course.dir <- course.row[, file.path(
      "years", course.dt[1, catalogYear], subject, catNbr)]
    dir.create(course.dir, showWarnings = FALSE, recursive = TRUE)
    match.html <- file.path(course.dir, "match.html")
    if(!file.exists(match.html)){
      results.html <- file.path(course.dir, "results.html")
      if(!file.exists(results.html)){
        u <- course.row[, paste0(
          Courses.prefix, "results?subject=",
          subject, "&catNbr=", catNbr)]
        download.file(u, results.html)
      }
      f <- function(name){
        nc::field(name, '="', '.*?')
      }
      results.dt <- nc::capture_all_str(
        results.html,
        '<td><abbr ',
        f('title'),
        '" ><a ',
        f('href'),
        '"><strong>',
        subject.catNbr.pattern,
        '<')
      match.href <- results.dt[course.row, href, on=.(subject, catNbr)]
      if(is.na(match.href)){
        ## Write empty file to signify that this course was not found,
        ## e.g., MTHPLACE 65 is a pre-req but not a course.x
        cat("", file=match.html)
      }else{
        u <- paste0(Courses.prefix, match.href)
        download.file(u, match.html)
      }
    }
    course.lines <- suppressWarnings(readLines(match.html))
    requisite <- if(length(course.lines)==0){
      ## No course found, mark for later removal.
      remove.courses.list[[ course.row$name ]] <- NA
      character()
    }else{
      course.string <- paste(course.lines, collapse="\n")
      no.comments <- gsub("<!--(?:.*\n)*?.*?-->", "", course.string)
      course.info <- nc::capture_all_str(
        no.comments,
        "[^<]*?",
        "<strong>",
        field=".*?",
        ":</strong>\\s*",
        value="(?:.*\n)*?.*?",
        "\\s*<br /><br />")
      course.info[grepl("requisite", field), value]
    }
    if(length(requisite)==0)requisite <- ""
    req.dt <- nc::capture_all_str(
      requisite, subject.catNbr.pattern)
    req.courses.list[[ course.row$name ]] <- data.table(
      course=course.row$name,
      requires=req.dt[, if(.N==0)NA_character_ else paste(subject, catNbr)])
    new.courses.list[[paste(course.i)]] <- req.dt[, .(subject, catNbr)]
  }
  all.courses <- unique(do.call(rbind, new.courses.list))
}

req.courses <- do.call(rbind, req.courses.list)
names(remove.courses.list)

graph.list <- list(
  degree_courses=degree.courses[, .(
    degree, course=paste(subject, catNbr))],
  requirements=req.courses[
    ! is.na(requires) |
      course %in% names(remove.courses.list) |
        requires %in% names(remove.courses.list)])
dir.create("download.graph", showWarnings = FALSE, recursive = TRUE)
for(data.name in names(graph.list)){
  dt <- graph.list[[data.name]]
  f <- file.path("download.graph", paste0(data.name, ".csv"))
  data.table::fwrite(dt, f)
}