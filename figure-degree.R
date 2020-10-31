library(data.table)
library(ggplot2)
degree_courses <- data.table::fread("download.graph/degree_courses.csv")

dir.create('figure-degree', showWarnings = FALSE)
for(degree in unique(degree_courses[["degree"]])){
  select.dt <- data.table(degree)
  course.dt <- nc::capture_first_df(
    degree_courses[select.dt, on=names(select.dt)],
    course=list(
      subject=".*?", " ",
      number="[0-9]+", as.integer,
      suffix=".*"))[order(number)]
  course.dt[, year := floor(number/100)]
  course.dt[, rank := 1:.N, by=year]
  course.dt[, x := rank-mean(rank), by=year]
  gg <- ggplot()+
    ggtitle(degree)+
    geom_text(aes(
      x, year, label=paste(subject, number, sep="\n")),
      data=course.dt)+
    scale_y_reverse()+
    scale_x_continuous("", breaks=c())
  degree.png <- file.path("figure-degree", paste0(degree, ".png"))
  png(
    degree.png,
    width=10, height=5, units="in", res=100)
  print(gg)
  dev.off()
  cat(paste0("[[file:", degree.png, "]]\n"))
}
