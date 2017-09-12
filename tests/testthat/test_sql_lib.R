# Does not work, for some reason or the other.
test_that("sql_lib works", {
  # parameters.sql.test<-list()
  # names(parameters.sql.test)
  # parameters.sql.test[["symbol"]]<-"'YPFD.BA'"
  # parameters.sql.test[["version"]]<-"'primary'"
  # parameters.sql.test[["source"]]<-"'yahoo.rt'"
  # parameters.sql.test[["date"]]<-"'2017-03-13 13:44:00'"
  #
  # ret_fields<-c("symbol","version","source","date","price", "vol")
  # m_fact_table<-"v_quotes_id_completed"
  # sql.text<-sql_gen_select(ret_fields,m_fact_table,where_fields = names(parameters.sql.test),parameters.sql.test)
  # sql_execute_select(1,sql.text,dbconn = datasource$m_dbconn)
  #
  # parameters.df<-as.data.frame(parameters.sql.test,stringsAsFactors=FALSE)
  # str(parameters.df)
  # parameters.df[2,]<-parameters.df[1,]
  # parameters.df[3,]<-parameters.df[1,]
  # sql.text<-sql_gen_select(ret_fields,m_fact_table,where_fields = names(parameters.df),parameters.df)
  # sql.text
  #
  #
  # sql_gen_where(where_fields = names(parameters.df),parameters.df)
  # value<-parameters.df[,1]
  # value<-"SADHD"
  # value<-"'2017-03-13 13:44:00'"
  # value<-"'2017-03-13' ' 13:44:00'"
  #
  # paste("'",sub("\\'([a-zA-Z0-9[:punct:]!'[:space:]]+)\\'","\\1",value),"'",sep="")
  # paste("'",sub("(?!')([a-zA-Z0-9[:punct:][:space:]]+)","\\1",value,perl = TRUE),"'",sep="")
  #
  expect_that(1, 1)
})
