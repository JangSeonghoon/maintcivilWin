#' TQI_trend
#'
#' @param start, last ,km
#' @return table for hchart
devtools::use_package("RJDBC")
devtools::use_package("highcharter")
devtools::use_package("forecast")
devtools::use_package("dplyr")
devtools::use_package("stringr")
devtools::use_package("stats")
devtools::use_package("htmlwidgets")
#' @importFrom RJDBC JDBC
#' @importFrom DBI dbConnect
#' @importFrom DBI dbSendQuery
#' @importFrom DBI dbExecute
#' @importFrom DBI dbFetch
#' @importFrom DBI dbHasCompleted
#' @importFrom DBI dbWriteTable
#' @importFrom DBI dbDisconnect
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_sub
#' @importFrom stringr str_c
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom magrittr %>%
#' @importFrom highcharter hchart
#' @importFrom forecast forecast
#' @importFrom stats HoltWinters
#' @importFrom htmlwidgets saveWidget
#' @export
tqi_trend=function(distance){
  len=distance
  len1=len*10
  len2=floor(len1)

  if((len2)%%2==0){
    startD=(len2-1)*100
    lastD=startD+200
  }else{
    startD=startD*100
    lastD=startD+200
  }

  round((len*1000)/100,digit=0)

  drv=JDBC("oracle.jdbc.driver.OracleDriver","c:/Users/bigTeam/ojdbc6.jar")
  conn=dbConnect(drv,"jdbc:oracle:thin:@localhost:1521:xe","korail150773","0818")

  rs=dbSendQuery(conn,"select * from PRACTICE_CHART")
  inspect=dbFetch(rs)
  names(inspect)=str_replace_all(names(inspect),"\"","")
  names(inspect)[1]="distance"
  inspect_backup=inspect

  i=1;for(i in 1:length(inspect)){
    inspect[,i]=as.numeric(as.character(inspect[,i]))
  }
  n=((lastD-startD)/200)+1
  range_200=startD+200*(1:n-1)
  temp=inspect
  temp_backup=temp

  startD_50=startD-25
  startD_50=as.numeric(startD_50)
  startD_50<-startD_50

  lastD_50=lastD+24.75
  lastD_50=as.numeric(lastD_50)
  lastD_50<-lastD_50

  startD_100=startD-100
  startD_100=as.numeric(startD_100)
  startD_100<-startD_100

  lastD_100=lastD+99.75
  lastD_100=as.numeric(lastD_100)
  lastD_100<-lastD_100

  range_no=(startD-1000)+0.25*(c(1:(4*lastD-4*startD+8001))-1)
  range_no<-range_no

  range_TQI=(startD)+200*(c(1:((lastD-startD+200)/200))-1)
  range_TQI<-range_TQI

  distance_temp=data.frame("distance"=range_no)

  kind_v=c(2:11)

  count=1;for(count in 1:10){
    if(count!=1) temp_aggre=temp;

    temp=temp_backup
    kind=kind_v[count]
    longLevel=200
    longLevel<<-longLevel
    temp=left_join(distance_temp, temp[,c(1,kind)][!duplicated(temp[,c(1,kind)][,1]),],"distance")

    if(longLevel==50){
      temp=temp %>% filter(distance>=startD_50,distance<=lastD_50)
      if(is.na(sum(temp[,2]))) temp[is.na(temp[,2]),2]=0

      movingInclude=rep(0,100)
      i=101;for(i in 101:(length(temp[,1])-100)){
        movingInclude[i]=temp[i,2]-mean(temp[c((i-100):(i-1),i,(i+1):(i+100)),2])
        print(paste0("count=",count,"/",10," ",i,"/",length(temp[,1])-100))
      }#for(i)
      movingInclude=c(movingInclude,rep(0,100))
      temp=mutate(temp,movingInclude=movingInclude)

    }else if(longLevel==200){
      temp=temp %>% filter(distance>=startD_100,distance<=lastD_100)
      if(is.na(sum(temp[,2]))) temp[is.na(temp[,2]),2]=0
      movingInclude=rep(0,400)
      i=401;for(i in 401:(length(temp[,1])-400)){

        movingInclude[i]=temp[i,2]-mean(temp[c((i-400):(i-1),i,(i+1):(i+400)),2])
        print(paste0("count=",count,"/",10," ",i,"/",length(temp[,1])-400))
      }#for(i)
      movingInclude=c(movingInclude,rep(0,400))
      temp=mutate(temp,movingInclude=movingInclude)

    }else if(longLevel==0){c.eval("plot(A)")
      temp=mutate(temp,movingInclude=temp[,2])
    }#if

    TQI=integer(0)
    i=1;for(i in 1:(length(range_TQI)-1)){
      TQI[i]= sd((temp %>% select(1,3) %>% filter(distance>=range_TQI[i],distance<range_TQI[i+1]))[,2])
      print(paste0("count=",count,"/",9," ",i,"/",(length(range_TQI)-1)))
    }
    temp=data.frame(distance=range_200[-length(range_200)],TQI=TQI)
    if(count!=1) temp=left_join(temp_aggre,temp[!duplicated(temp[,1]),],"distance")
    print(paste0(count,"/",10))
  }#for(count)
  names(temp)=names(inspect)
  TQI_pita=temp
  TQI_pita=t(TQI_pita)
  demand<-ts(TQI_pita[-1,1],start=c(2015),frequency=4)
  (hw=HoltWinters(demand,seasonal="additive"))
  A<-forecast(hw,h=4)
  A<<-A
  saveWidget(hchart(forecast(hw,h=4)),"c:/Users/bigTeam/workspace/bigTeam/WebContent/html/tqi_trend.html")

}#function
