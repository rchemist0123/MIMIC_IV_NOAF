#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

require(rapidoc)
library(plumber)
library(httr)
library(RSQLite)
require(DBI)


con <- dbConnect(SQLite(),
                 dbname = 'C:/Users/user/lab/shiny/workoutDashboard/db.sqlite')

#* @apiTitle Plumber Example API
#* @apiDescription Plumber example description.

#* Read all diary
#* @get /read
function() {
  sql <- "select * from diary"
  rs <- dbGetQuery(con, sql)
  print(rs)
  # dbClearResult(rs)
}
#* Read diary in specific date
#* @param dates:string 일자
#* @get /read/<dates>
function(dates){
  sql <- "select * from diary where dates=?"
  rs <- dbGetQuery(con, sql, bind.data = data.frame(dates=dates))
  print(rs)
}
#* Write diary
#* @param dates 일자
#* @param cat_big 대분류
#* @param cat_small 소분류
#* @param weight:numeric 무게
#* @param no_rep:numeric 반복수
#* @param no_set:numeric 세트수
#* @param memo:string 메모
#* @post /write
function(dates, cat_big, cat_small, weight, no_rep, no_set, memo=NA){
  sql <- "insert into diary 
                  (dates, cat_big, cat_small, weight, no_rep,
                  no_set, memo)
  values (?,?,?,?,?,?,?)"
  params <- list(dates, cat_big, cat_small, weight, no_rep, no_set, memo)
  res <- dbSendStatement(con, sql,
                         params = params)
  dbClearResult(res)
  
}

#* Update diary
#* @put /update/<id:int>
function(id){
  # sql <- "update diary set "
  # dbSendQuery()
}

#* Delete record
#* @delete /drop/<id:int>
function(id){
  sql <- "delete from diary where id=?"
  dbExecute(con, sql, params=id)
}

# Programmatically alter your API
#* @plumber
function(pr) {
  pr %>%
    # Overwrite the default serializer to return unboxed JSON
    pr_set_serializer(serializer_unboxed_json())
}
