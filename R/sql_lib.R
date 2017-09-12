# sql-lib.R
# Library for simple sql code generation from R
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

if (!exists("contadorDML"))
  contadorDML<<-0
if (!exists("sql_inserts"))
  sql_inserts<-data.frame(sql="",stringsAsFactors=FALSE)

#' The class that provides the SQL functionality
#'
#' @importFrom R6 R6Class
#' @export
RSQL.class <- R6::R6Class(
  'RSQL',
  public=list(

  ))


#' sql_execute_insert
#'
#' @import futile.logger
#' @param id_procesamiento TEST
#' @param sql_insert TEST
#' @param export TEST
#' @param debug_level TEST
#' @export
sql_execute_insert<-function(id_procesamiento,sql_insert,
                             dbconn=NULL,export=c("db","df"),debug_level=0){
  sql_insert<-gsub(",NA",",NULL",sql_insert)
  sql_insert<-gsub(", NA",",NULL",sql_insert)
  sql_insert<-paste(sql_insert,";",sep="")
  if ("db" %in% export){
    ret <- dbSendQuery(dbconn,sql_insert)
    dbSendQuery
    futile.logger::flog.info(sql_insert)

    #data<-fetch(rs,n=-1)
    #print(res)
    if (length(ret)>0){
      #TODO verificar OK

      #    if (nchar(ret[1])>5){
      #      print(paste("Para procesamiento", id_procesamiento, "en Insert No", contadorInserts, " se inserta error en la base"))
      #      print(paste(sql_insert))
      #      print(paste("Error:",res[1]))
      #      insertarErrorEnBD(id_procesamiento, contadorInserts, id_ciudad,anio, mes, sql_insert, res[1],context)
      #    }
    }
    }
    contadorDML<<-contadorDML+1
    if ("df" %in% export)
      sql_inserts<<-rbind(sql_inserts,sql_insert)
   ret
}

#' sql_execute_update
#'
#' @param id_procesamiento TEST
#' @param sql_update TEST
#' @param export TEST
#' @param debug_level TEST
#' @export
sql_execute_update<-function(id_procesamiento,sql_update,
                             dbconn=NULL,export=c("db","df"),debug_level=0){
  stop("Not implemented")
}


#' sql_execute_delete
#'
#' @param id_procesamiento TEST
#' @param sql_insert TEST
#' @param debug_level TEST
#' @export
sql_execute_delete<-function(id_procesamiento,sql_delete,dbconn=NULL,debug_level=0){
  sql_delete<-gsub(",NA",",NULL",sql_delete)
  sql_delete<-gsub(", NA",",NULL",sql_delete)
  ret<-dbGetQuery(dbconn,sql_delete)
  futile.logger::flog.info(sql_delete)
  ret

}

#' sql_execute_select
#'
#' @param id_procesamiento TEST
#' @param sql_select TEST
#' @param dbconn TEST
#' @param debug_level TEST
#' @export
sql_execute_select<-function(id_procesamiento,sql_select,dbconn=NULL,debug_level=0){
  #debug
  #sql_select<-"select price from v_quotes_id_completed where symbol="alua" order by date DESC LIMIT 1"
  #sql_select<-"select * from v_quotes"
  sql_select<-gsub(",NA",",NULL",sql_select)
  sql_select<-gsub(", NA",",NULL",sql_select)
  ret<-dbGetQuery(dbconn,sql_select)
  ret
}

#' execute_get_insert
#'
#' @param id_procesamiento TEST
#' @param sql_select TEST
#' @param sql_insert TEST
#' @export
execute_get_insert<-function(id_procesamiento,sql_select,sql_insert,...){
  ret<-execute_select(id_procesamiento,sql_select,dbconn)
  if (nrow(ret)==0){
    execute_insert(id_procesamiento,sql_insert)
    ret<-execute_select(id_procesamiento,sql_select,...)
  }
  ret[1,]
}

#' is.quoted
#'
#' @param text TEST
#' @param quotes_symbols TEST
#' @export
is.quoted<-function(text,quotes_symbols=c("'","'")){
  ret<-FALSE
  i<-1
  while (!ret & i<=length(quotes_symbols)){
    quotes<-quotes_symbols[i]
    ret<-substr(text,1,1)==quotes &substr(text,nchar(text),nchar(text))==quotes
    i<-i+1
  }
  ret
}

#' dequote
#'
#' @param text TEST
#' @export
dequote<-function(text){
  substr(text,2,nchar(text)-1)
}

#' re.quote
#'
#' @param text TEST
#' @param quotes TEST
#' @export
re.quote<-function(text,quotes="'"){
  quote<-FALSE
  if (!is.quoted(text,"'"))
    quote<-TRUE
  if (is.quoted(text,"'")){
    text<-dequote(text)
    quote<-TRUE
  }
  if (quote)
    text<-paste(quotes,text,quotes,sep="")
  text
}

#' add_quotes
#'
#' @param text TEST
#' @export
add_quotes<-function(text){
  ret<-sapply(text,
         FUN=re.quote)
  names(ret)<-NULL
  ret
}

#' rm.quotes
#'
#' @param text TEST
#' @param quotes TEST
#' @export
rm.quotes<-function(text,quotes="'"){
  if (quotes==substr(text,1,1) &
      quotes==substr(text,nchar(text),nchar(text))
      )
    text<-substr(text,2,nchar(text)-1)
  text
}

#' remove_quotes
#'
#' @param text TEST
#' @export
remove_quotes<-function(text){
  ret<-sapply(text,
              FUN=rm.quotes)
  names(ret)<-NULL
  ret
}


#' add_grep_exact_match
#'
#' @param text TEST
#' @export
add_grep_exact_match<-function(text){
  text<-gsub("(\\^|\\%)","\\\\\\1",text)
  paste("^",text,"$",sep="")
}

#' sql_gen_delete.bad
#'
#' @param table TEST
#' @param where_fields TEST
#' @param where_values TEST
#' @export
sql_gen_delete.bad<-function(table, where_fields,
                         where_values=c()){

  sql_where<-""
  if (length(where_fields)>0){
    sql_where<-"where "
    i<-1
    separator<-""
    for (f in where_fields){
      if (is.na(where_values[i]))
        value<-paste(":label_",f,":",sep="")
      else
        value<-where_values[i]
      if (is.character(value))
        value<-paste("'",value,"'",sep="")
      sql_where<-paste(sql_where,separator,f,"=", value,sep="")
      separator<-" and "
      i<-i+1
    }
  }
  ret<-paste("delete from",table, sql_where)
  ret

}

#' sql_gen_delete
#'
#' @param table TEST
#' @param where_fields TEST
#' @param where_values TEST
#' @export
sql_gen_delete<-function(table, where_fields="",
                             where_values=NULL){
  sql_where<-sql_gen_where(where_fields,where_values)
  ret<-paste("delete from",table, sql_where)
  ret

}

#' sql_gen_select
#'
#' @param select_fields TEST
#' @param table TEST
#' @param where_fields TEST
#' @param where_values TEST
#' @param group_by TEST
#' @param order_by TEST
#' @param top TEST
#' @param debug_level TEST
#' @export
sql_gen_select<-function(select_fields, table, where_fields=NULL,
                         where_values=NULL,group_by=c(),
                         order_by=c(),top=0,
                         debug_level=0){
  separator<-""
  sql_select_fields<-""
  for (f in select_fields){
    sql_select_fields<-paste(sql_select_fields,separator,f,sep="")
    separator<-", "
  }
  sql_where<-sql_gen_where(where_fields,where_values,debug_level=debug_level)
  sql_order_by<-paste(order_by,collapse=",")
  sql_group_by<-""
  if (length(group_by)>0){
    separator<-""
    for (f in group_by){
      sql_group_by<-paste(sql_group_by,separator,f,sep="")
      separator<-", "
    }
    order_by<-c(sql_group_by,order_by)
    sql_group_by<-paste("group by ",sql_group_by)
  }
  ret<-paste("select",sql_select_fields,"from",table, sql_where, sql_group_by)
  if (nchar(sql_order_by)>0)
    ret<-paste(ret,"order by",sql_order_by)
  if (top>0)
    ret<-paste(ret,"limit",top)
  trimws(ret)
}

#' sql_gen_where
#'
#' @param where_fields TEST
#' @param where_values TEST
#' @param debug_level TEST
#' @export
sql_gen_where<-function(where_fields,where_values,debug_level=0){
  ret<-""
  if (!is.null(where_fields)&!is.null(where_values)){
    #Asserts with values
    if (!is.vector(where_fields))
      stop(paste("where_fields must be a vector and was a ",str(where_fields)))
    if (!is.list(where_values))
      if (is.vector(where_values))
        where_values<-data.frame(matrix(where_values,byrow=TRUE,ncol=length(where_values)),stringsAsFactors = FALSE)
    if (length(where_fields)!=ncol(where_values))
      stop(paste("#where_fields!=#where_values:",
                 length(where_fields),"!=",length(where_values),
                 paste(where_fields,collapse=","),
                 paste(where_values,collapse=",")))
    #if strings values, add '
    for (col in names(where_values)){
      if (max(is.character(where_values[,col]))==1){
        #if there is at least one value character in column
        #remove ' for normalization and adding after
        new.values<-paste("'",sub("\\'([a-zA-Z0-9[:punct:]!'[:space:]]+)\\'","\\1",where_values[,col]),"'",sep="")
        futile.logger::flog.info(paste("col",col,"is character. Replacing values",
                          paste(where_values[,col],collapse=","),"with values",
                          paste(new.values,collapse=",")))
        where_values[,col]<-new.values
      }
    }


    ret<-""
    if (length(where_fields)>0 & !(length(where_fields)==1 & nchar(where_fields[1])==0)){
      where_values<-as.data.frame(where_values,nrow=nrow(where_values)/length(where_fields),stringsAsFactors = FALSE)
      if (nrow(where_values)>2)
        ret<-sql_gen_where_list(where_fields,where_values,debug_level=debug_level)
      else
        ret<-sql_gen_where_or(where_fields,where_values,debug_level=debug_level)
    }
  }
  else{
    if (!is.null(where_fields))
      stop(paste("no where_values specified"))
    if (!is.null(where_values))
      stop(paste("no where_fields specified"))
  }
  ret
}

#' sql_gen_where_list
#'
#' @param where_fields TEST
#' @param where_values TEST
#' @param debug_level TEST
#' @export
sql_gen_where_list<-function(where_fields,where_values,debug_level=0){
  sql_where<-""
  if (length(where_fields)>0){
    separator<-""
    sql_where<-"where ("
    for (f in where_fields){
      sql_where<-paste(sql_where,separator,f,sep="")
      separator=","
    }
    sql_where<-paste(sql_where,") in ",sep="")
    list_where<-""
    separator_list<-""
    for (v in c(1:nrow(where_values))){
      i<-1
      separator<-""
      sql_row_where<-""
      for (f in where_fields){
        if (is.na(where_values[v,i]))
          value<-paste(":label_",f,":",sep="")
        else
          value<-where_values[v,i]

        if (is.character(value)){
          value<-add_quotes(value)
        }
        sql_row_where<-paste(sql_row_where,separator,value,sep="")
        separator<-","
        i<-i+1
      }
      if(i>2)
        sql_row_where<-paste("(",sql_row_where,")",sep="")
      list_where<-paste(list_where,separator_list,sql_row_where,sep="")
      separator_list<-","
    }
    sql_where<-paste(sql_where,"(",list_where,")")
  }
  sql_where
}

#' sql_gen_where_or
#'
#' @param where_fields TEST
#' @param where_values TEST
#' @param debug_level TEST
#' @export
sql_gen_where_or<-function(where_fields,where_values,debug_level=0){
  sql_where<-""
  if (length(where_fields)>0){
    sql_where<-"where"
    separator_where<-""
    for (v in c(1:nrow(where_values))){
      i<-1
      separator<-""
      sql_row_where<-""
      for (f in where_fields){
        if (is.na(where_values[v,i]))
          value<-paste(":label_",f,":",sep="")
        else
          value<-where_values[v,i]
        if (is.character(value))
          value<-add_quotes(value)
        sql_row_where<-paste(sql_row_where,separator,f,"=", value,sep="")
        separator<-" and "
        i<-i+1
      }
      sql_where<-paste(sql_where,separator_where,"(",sql_row_where,")")
      separator_where<-" or "
    }
  }
  sql_where
}



#' sql_gen_insert
#'
#' @param table TEST
#' @param insert_fields TEST
#' @param values TEST
#' @param debug_level TEST
#' @export
sql_gen_insert<-function(table, insert_fields,values=c(),debug_level=0){
  values<-as.data.frame(values,stringsAsFactors = FALSE)
  if (length(insert_fields)!=ncol(values)){
    stop(paste("incompatible fields and data:", length(insert_fields),"!=",ncol(values), paste(insert_fields,collapse=";"),paste(values,collapse=";")))
  }
  separator<-""
  sql_insert_fields<-""
  for (f in insert_fields){
    sql_insert_fields<-paste(sql_insert_fields,separator,f,sep="")
    separator<-", "
  }
  sql_values<-""
  separator_rows<-""
  for (i in c(1:nrow(values))){
    sql_values_row<-""
    separator<-""
    for (j in c(1:length(insert_fields))){
      if (is.na(values[i,j]))
        #value<-paste(":label_",f,":",sep="")
        value<-"NA"
      else{
        value<-values[i,j]
        if (is.character(value))
          value<-paste("'",value,"'",sep="")
      }
      sql_values_row<-paste(sql_values_row,separator, value,sep="")
      separator<-", "
    }
    futile.logger::flog.info(sql_values_row)
    sql_values<-paste(sql_values,separator_rows, "(",sql_values_row,")")
    separator_rows<-", "
  }
  ret<-paste("insert into ",table, "(",sql_insert_fields,") values ",sql_values, sep="")
  ret
}

#' sql_execute_update
#'
#' @param id_procesamiento TEST
#' @param sql_insert TEST
#' @param dbconn TEST
#' @param export TEST
#' @param debug_level TEST
#' @export
sql_execute_update<-function(id_procesamiento,sql_insert,
                             dbconn=NULL,export=c("db","df"),debug_level=0){
  stop("Not implemented")
}

#' sql_execute_update
#'
#' @param table TEST
#' @param update_fields TEST
#' @param values TEST
#' @param fields_id TEST
#' @param values_id TEST
#' @param debug_level TEST
#' @export
sql_gen_update<-function(table,
                         update_fields,
                         values,
                         fields_id,
                         values_id,
                         debug_level=0){
  stop("Not implemented")
  ret<-paste("update ",table, " set (",sql_update_fields,")=(",sql_values, ") where ",sep="")
  ret
}


#' trim.leading
#'
#' Returns string w/o leading whitespace
#'
#' @param x TEST
#' @param update_fields TEST
#' @param values TEST
#' @param fields_id TEST
#' @param values_id TEST
#' @param debug_level TEST
#' @export
trim.leading <- function (x)  sub("^\\s+", "", x)

#' trim.leading
#'
#' Returns string w/o trailing whitespace
#'
#' @param x TEST
#' @param update_fields TEST
#' @param values TEST
#' @param fields_id TEST
#' @param values_id TEST
#' @param debug_level TEST
#' @export
trim.trailing <- function (x) sub("\\s+$", "", x)

#' trim.leading
#'
#' Returns string w/o leading or trailing whitespace
#'
#' @param x TEST
#' @export
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#' rename_col
#'
#' @param df TEST
#' @param name TEST
#' @param replace_name TEST
rename_col<-function (df,name, replace_name){
  i<-which(names(df)==name)
  names(df)[i]<-replace_name
  df
}

#' cbind_coerced
#'
#' @param ... TEST
#' @export
cbind_coerced<-function(...){
  ret<-cbind(...,stringsAsFactors=FALSE)
  if ("stringsAsFactors" %in%names(ret))
    ret<-ret[,-which(names(ret)=="stringsAsFactors")]
  ret
}


#' df_verify
#'
#' @param dataframe TEST
#' @param columns TEST
#' @export
df_verify<-function(dataframe,columns){
  ret<-NULL
  dataframe_names<-names(dataframe)
  for (column in columns){
    if (!column %in% dataframe_names)
      ret<-c(ret,column)
  }
  if (length(ret)>0)
    stop(paste("missing columns in dataframe:",paste(ret,collapse=","),"df",paste(dataframe_names,collapse=",")))
}

#' sql_retrieve_insert
#'
#' @param id_procesamiento TEST
#' @param table TEST
#' @param fields_id TEST
#' @param fields TEST
#' @param values TEST
#' @param dbconn TEST
#' @param debug_level TEST
#' @export
sql_retrieve_insert<-function(id_procesamiento,
                              table,fields_id,values_id,fields=NULL, values=NULL,
                              dbconn=NULL,debug_level=0){
  ret<-NULL
  values_id<-as.data.frame(values_id,stringsAsFactors = FALSE)
  values<-as.data.frame(values,stringsAsFactors = FALSE)
  if (nrow(values)>0 & nrow(values)<nrow(values_id)){
    stop(paste("error: nrow(values_id)!=nrow(values):",nrow(values_id),nrow(values)))
  }


  for (i in c(1:nrow(values_id))){
    #value_id<-as.character(values_id[i,])
    #value<-as.character(values[i,])
    value_id<-as.data.frame(values_id[i,],stringsAsFactors = FALSE)
    value<-values[i,]
    values_insert<-cbind_coerced(value_id,value)

    select_statement<-sql_gen_select("id",table,where_fields = fields_id,where_values = value_id)

    #debug
    futile.logger::flog.info(paste("verifying",select_statement,":"))
    insert_statement<-sql_gen_insert(table,insert_fields = c(fields_id,fields),values=values_insert)
    row<-sql_execute_select(id_procesamiento,select_statement,dbconn=dbconn,debug_level = debug_level)
    futile.logger::flog.info(paste(row,"rows"))
    if (nrow(row)==0){
      futile.logger::flog.info(paste("executing",insert_statement))
      res<-sql_execute_insert(id_procesamiento,insert_statement,dbconn=dbconn,debug_level = debug_level)
      row<-sql_execute_select(id_procesamiento,select_statement,dbconn=dbconn,debug_level = debug_level)
    }
    ret<-c(ret,as.numeric(row$id))
    i<-i+1
  }
  ret
}


#' sql_gen_joined_query
#'
#' @param dw_definition TEST
#' @param recipe TEST
#' @param indicator_fields TEST
#' @export
sql_gen_joined_query<-function(dw_definition,recipe,indicator_fields){
#  sql_gen_select<-function(select_fields, table, where_fields="",
#                           where_values=NULL,group_by=c(),debug_level=0){
    sql_select_fields<-rep("",length(indicator_fields))
    sql_from<-""
    sql_where<-""
    ind_i<-0
    where_sep<-""
    from_sep<-""
    for (i in c(1:nrow(recipe$m_recipe))){
      current_expression<-recipe$m_recipe[i,]
      #TODO correct in a dictionary
      alias<-gsub("\\.","_",current_expression$value)
      for (j in c(1:length(indicator_fields))){
        if (current_expression$op=="=")
          sql_select_fields[j]<-current_expression$value
        else{
          sql_select_fields[j]<-paste("(",sql_select_fields[j],current_expression$op,
                                      alias,".",indicator_fields[j], ")",sep="")
        }
      }
      if (current_expression$value_type=="indicator"){
        ind_i<-ind_i+1
        if (ind_i==1)
          first_alias<-alias
        sql_from<-paste(sql_from,from_sep,dw_definition$m_fact_table," ",alias,sep="")
        from_sep<-","
        first_field_def<-dw_definition$m_dimensions[1,]
        for (k in c(1:nrow(dw_definition$m_dimensions))){
          current_field_def<-dw_definition$m_dimensions[k,]
          explicit_value<-ind_i==1& nchar(current_field_def$default)==0
          #TODO define in dw_definition fields mapping
          explicit_value<-explicit_value|current_field_def$field=="symbol"
          filter<-ind_i>1|current_field_def$field=="symbol"

          #debug
          # explicit_value<<-explicit_value
          # dw_definition<<-dw_definition
          # k<<-k

          if (explicit_value){
            if (current_expression$value_id>0)
              right_value<-current_expression$value_id
            else
              right_value<paste("'",current_expression$value,"'",sep="")
          }
          else{
            if(filter)
              right_value<-paste(first_alias,".",current_field_def$field,sep="")
          }
          if (filter){
            sql_where<-paste(sql_where,where_sep, alias,".",current_field_def$field,"=",right_value,sep="")
            where_sep<-" and "
          }
        }
      }
      ret<-paste("select",paste(sql_select_fields,"as",indicator_fields, collapse=","),"from",sql_from, "where",sql_where)
    }
    ret
}
#' %IN%
#' Operator IN for multiple columns
#' @param x TEST
#' @param y TEST
#' @export
"%IN%" <- function(x, y) interaction(x) %in% interaction(y)
