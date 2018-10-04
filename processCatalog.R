# Process Emily Dickinson Project Gutenberg corpus
# Nicholas Horton, nhorton@amherst.edu   November 1, 2015
location = "catalog"
if (!file.exists(location)) {
  dir.create(location)
}

processCatalog = function(source="catalog2014.txt") {
  ds = readLines(source)
  prefix = source
  deptnum = 1
  sink(file=paste(location, "/", prefix, sprintf("%03d", deptnum), sep=""))
  for (i in 1:length(ds)) {
    if (ds[i] != "<H1>") {   # non blank text
      depnum=deptnum
      cat(ds[i], "\n")
    } else {  # must be a new dept!
      # new dept
      deptnum = deptnum + 1
      sink()
      sink(file=paste(location, "/", prefix, sprintf("%03d", deptnum), sep=""))
    }
  }
  sink()
}

processCatalog("catalog2014.txt")

