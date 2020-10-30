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
for(bs.i in 1:nrow(bs.dt)){
  bs <- bs.dt[bs.i]
  bs.dir <- file.path(today.dir, bs[["B.S."]])
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
    number="[0-9]+",
    "</a>"))
  print(course.dt[, .(plan=bs[["B.S."]], subject, number)])
  for(course.i in 1:nrow(course.dt)){
    course.row <- course.dt[course.i]
    course.dir <- course.row[, file.path(
      "years", catalogYear, subject, number)]
    dir.create(course.dir, showWarnings = FALSE, recursive = TRUE)
    course.html <- file.path(course.dir, "course.html")
    if(!file.exists(course.html)){
      download.file(course.row[["href"]], course.html)
    }
    course.lines <- suppressWarnings(readLines(course.html))
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
    ##print(course.info[["field"]])
    requisite <- course.info[grepl("requisite", field), value]
    if(length(requisite)==0)requisite <- ""
    req.dt <- nc::capture_all_str(
      requisite, subject.catNbr.pattern)
    for(req.i in 1:nrow(req.dt)){
      req.row <- req.dt[req.i]
      req.dir <- req.row[, file.path(
        "years", course.row$catalogYear, subject, catNbr)]
      results.html <- file.path(req.dir, "results.html")
      if(!file.exists(results.html)){
        u <- req.row[, paste0(
          Courses.prefix, "results?subject=",
          subject, "&catNbr=", catNbr)]
        dir.create(req.dir, showWarnings = FALSE, recursive = TRUE)
        download.file(u, results.html)
      }
      match.html <- file.path(req.dir, "match.html")
      if(!file.exists(match.html)){
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
        match.href <- results.dt[req.dt, href, on=.(subject, catNbr)]
        u <- paste0(Courses.prefix, match.href)
        download.file(u, match.html)
      }
      ## TODO parse match.html and save dependency graph.
    }
    cat(
      course.row[, paste(subject, number)],
      "requisite:",
      requisite,
      "\n\n")
  }
}
