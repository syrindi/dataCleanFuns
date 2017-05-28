# Data cleaning functions originally created for excel reports

#' @param
#' @export
#' @return value
#' @import XLConnect,lubridate,data.table,magrittr,stringr,dplyr,readr


#' @export
## determines the row and column of "Term" in data.frame(data)
searcherx = function(term, data)
{
  loc1 = list()
  loc2 = list()

  ## look for which row
  for(j in 1:ncol(data))
  {

    a = grep(term, unlist(data[,j]))
    #b = grep(term, unlist(data[,j]))

    if (length(a)>0)
    {
      loc = grep(term, unlist(data[,j]), ignore.case = T)
      loc1 = c(loc, loc1)
    }
  }



  ## look for which column
  for (j in 1:nrow(data))
  {
    b = grep(term, unlist(data[j,]))
    if (length(b)>0)
    {
      loc = grep(term, unlist(data[j,]), ignore.case = T)
      loc2 = c(loc, loc2)
    }
  }
  loc3= c(loc1,loc2)


  return(unlist(loc3))
}



#' @export
# clean file names
cleanFileNamex = function(x)
{
  a = gsub("[-~ ]", "", x)
  b = gsub("['$]", "", a)
  c = tolower(b)
  return(c) #
}


#' @export
# clean dataa
# deletes all rows and columns where all elements are NA

removeEmptyLines = function(x)
{
  a = data.table(x)
  b = a[,which(unlist(lapply(a, function(x)!all(is.na(x))))),with=F]  ## removes columns where  all is NA
  c = b[rowSums(is.na(b)) != ncol(b),]  ## removes rows whe all is NA
  c = data.frame(c)
  d = lapply(c, function(x) gsub("[$),]","",x))
  e = lapply(d, function(x) gsub("[(]","-",x))
  f = data.frame(e, stringsAsFactors = F)
  return(f)
}



## identify the monthly data which starts with the second date
dateExtractx = function(x)
{
  x = data.frame(x)
  named = names(x)
  date = named[nchar(named) > 5][2]
  date1 = unlist(str_split(date, "[ .]"))
  date2 = paste(date1[1], date1[length(date1)])

  return(date2)
}



