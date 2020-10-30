course.subject <- "CS"
course.number <- "499"
##1211=spring 21, 1207=fall 20
course.term <- 1211
url.prefix <- "http://catalog.nau.edu/Courses/"

subject.dir <- file.path(
  "terms", course.term, course.subject, course.number)
search.html <- file.path(subject.dir, "search.html")
if(!file.exists(search.html)){
  dir.create(subject.dir, showWarnings = FALSE, recursive = TRUE)
  u <- paste0(
    url.prefix,
    "results?subject=",
    course.subject,
    "&catNbr=",
    course.number,
    "&term=",
    course.term)
  download.file(u, search.html)
}

match.html <- file.path(subject.dir, "match.html")
if(!file.exists(match.html)){
  f <- function(name){
    nc::field(name, '="', '.*?')
  }
  result.dt <- nc::capture_all_str(
    search.html,
    '<td><abbr ',
    f('title'),
    '" ><a ',
    f('href'),
    '"><strong>',
    subject.number='.*?',
    '<')
  match.href <- result.dt[
    subject.number==paste(course.subject, course.number),
    href]
  u <- paste0(url.prefix, match.href)
  download.file(u, match.html)
}

match.lines <- readLines(match.html)
match.string <- paste(match.lines, collapse="\n")
no.comments <- gsub("<!--(?:.*\n)*?.*?-->", "", match.string)
if(FALSE){
  s <- function(name)list(
    "[^<]*?",
    "<strong>",
    nc::field(name, ":</strong>\\s*", "(?:.*\n)*?.*?"),
    "\\s*<br /><br />")
  course.row <- nc::capture_all_str(
    no.comments,
    s("Description"),
    s("Units"),
    s("Sections offered"),
    ##s("Prerequisite or Corequisite"),
    rest="(?:.*\n)*")
  cat(course.row[["Sections offered"]])
  cat(course.row[["rest"]])
  cat(course.row[["Prerequisite or Corequisite"]])
}
course.info <- nc::capture_all_str(
  no.comments,
  "[^<]*?",
  "<strong>",
  field=".*?",
  ":</strong>\\s*",
  value="(?:.*\n)*?.*?",
  "\\s*<br /><br />")
print(course.info[["field"]])
cat(course.subject, course.number, "\nrequisite:",
    course.info[grepl("requisite", field), value], "\n\n")
