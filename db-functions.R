require(RODBC)

as.is.vector <- function (conn, table) {
  
  cols <- sqlColumns(conn, table)$COLUMN_NAME
  result <- rep(TRUE, length(cols))
  names(result) <- cols
  result
  
}


##RDBMS -> Rdata

RDBMS.to.Rdata <- function(conn, data.dir="data", Rdata.filename="schools.orig.Rdata", RDBMS.object.name=paste(model.schema,"R13_SCHOOLS",sep="."), R.object.name="schools", use.verbose=FALSE) {
  
  assign(R.object.name, sqlFetch(testing, RDBMS.object.name, as.is=as.is.vector(testing, RDBMS.object.name), nullstring=""))
  if (!file.exists(data.dir))
    dir.create(file.path(getwd(), data.dir))
  save(list=R.object.name, file=paste(data.dir, Rdata.filename, sep="/"))
}


object.name <- function (schema, simple.name) {
  paste(schema, simple.name, sep=".")  
}
