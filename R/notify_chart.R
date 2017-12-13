#' notify_ver2 in JAVA WEB
#'
#' return caution table for Showing in JAVA WEB
#' @param None
#' @return caution Tb(show spots to fix and working priorty)
devtools::use_package("magrittr")
devtools::use_package("dplyr")
devtools::use_package("csvread")
devtools::use_package("stringr")
devtools::use_package("rJava")
devtools::use_package("DBI")
devtools::use_package("RJDBC")
devtools::use_package("plotly")
devtools::use_package("htmlwidgets")
devtools::use_package("tidyr")
devtools::use_package("compiler")


#' @importFrom compiler cmpfun
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr full_join
#' @importFrom csvread map.coltypes
#' @importFrom csvread csvread
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_sub
#' @importFrom stringr str_c
#' @importFrom DBI dbConnect
#' @importFrom DBI dbSendQuery
#' @importFrom DBI dbExecute
#' @importFrom DBI dbFetch
#' @importFrom DBI dbHasCompleted
#' @importFrom DBI dbWriteTable
#' @importFrom DBI dbDisconnect
#' @importFrom RJDBC JDBC
#' @importFrom plotly plot_ly
#' @importFrom plotly add_trace
#' @importFrom plotly layout
#' @importFrom htmlwidgets saveWidget
#' @importFrom tidyr spread
#' @export
notify_chart=function(year,quater,workspace_no,carKind,order){
  A=cmpfun(
    function(){

      drv=JDBC("oracle.jdbc.driver.OracleDriver","c:/Users/bigTeam/ojdbc6.jar")
      conn=dbConnect(drv,"jdbc:oracle:thin:@localhost:1521:xe","korail150773","0818")

      quater=ifelse(length(quater)==1,paste0("0",quater),quater)
      year=as.integer(year)

      rs=dbSendQuery(conn,paste0("select * from inspectRS",year,quater, " where workspace='",workspace_no,"'"))
      temp=dbFetch(rs)

      j=1;for(j in 1:length(temp)){
        temp[,j]=str_replace_all(temp[,j],"(\")","")
      }
      car=ifelse(carKind==1,"궤도검측차","종합검측차")
      temp=temp[temp$CARKIND==car,]
      i=c(2,3,4,5,6,10,11)
      k=1;for(k in 1:length(i)){

        temp[,i[k]]=as.numeric(temp[,i[k]])
        print(k)
      }
      temp[,8]=str_replace_all(temp[,8],"[.]","/")
      temp[,8]=as.Date(temp[,8])

      seq=integer(0)
      deadline=temp[,8]

      i=1;for(i in 1:length(temp[,1])){
        if(str_detect(temp[i,1],"TWIST")){
          seq[i]=1
          deadline[i]=deadline[i]+30
        }else if(str_detect(temp[i,1],"ALIGNMENT")){
          seq[i]=2
          deadline[i]=deadline[i]+60
        }else{
          seq[i]=3
          deadline[i]=deadline[i]+90
        }
      }
      remnant=deadline-Sys.Date()
      remnant_alert=ifelse(remnant<0,paste0(remnant*(-1),"일 지남"),
                           ifelse(remnant==0,"D-DAY",paste0(remnant,"일 남음")))
      remnant=as.character(remnant)
      len=length(temp)
      temp=temp %>% mutate(deadline,remnant,remnant_alert,seq) %>% arrange(seq,STARTD) %>% select(1:(len+3))

      caution=temp %>% select(8,12,1,4,5,6,3,2,9,14,13)
      caution=cbind(seq=seq(caution[,3]),caution)

      test=caution[,c(9,4,8)]
      start=min(caution$STARTD)
      last=max(caution$LASTD)
      test=rbind(test,data.frame(EXCEPT=rep(start,7),PARAMETER=c("GAGE","PROFILE LEFT","PROFILE RIGHT","ALIGNMENT LEFT","ALIGNMENT RIGHT","SUP","TWIST 3M"),MAX=rep(0,7)))
      n=(last-start)/0.001+1
      LOCATION=start+0.001*((1:n)-1)
      names(test)[1]="LOCATION"
      test=test %>% group_by(PARAMETER, LOCATION) %>% mutate(ind=row_number()) %>% spread("PARAMETER","MAX")
      test=as.data.frame(test)
      i=1;for(i in 1:length(test)){
        test[is.na(test[,i]),i]=0
        print(i)
      }

      LOC=data.frame(LOCATION=LOCATION)
      LOC[,1]=round(LOC[,1],digit=3)
      test[,1]=round(test[,1],digit=3)
      test=full_join(LOC,test,by="LOCATION")
      test=test[,-2]
      i=1;for(i in 1:length(test)){
        test[is.na(test[,i]),i]=0
        print(i)
      }

      p <- plot_ly(x = ~ test$`LOCATION`, y = ~test$`GAGE`, type = 'scatter', mode = 'lines', name = 'GAGE', fill = 'tozeroy'
                   # , fillcolor = 'rgba(168, 216, 234, 0.5)'
                   ,
                   line = list(width = 0.5)) %>%
        add_trace(x = ~test$LOCATION, y = ~test$`ALIGNMENT LEFT`, name = 'ALIGNMENT LEFT', fill = 'tozeroy'
                  # ,fillcolor = 'rgba(255, 212, 96, 0.5)')
        ) %>%
        add_trace(x = ~test$LOCATION, y = ~test$`ALIGNMENT RIGHT`, name = 'ALIGNMENT RIGHT', fill = 'tozeroy'
                  # ,fillcolor = 'rgba(255, 212, 96, 0.5)')
        ) %>%
        add_trace(x = ~test$LOCATION, y = ~test$`PROFILE LEFT`, name = 'PROFILE LEFT', fill = 'tozeroy'
                  # ,fillcolor = 'rgba(255, 212, 96, 0.5)')
        ) %>%
        add_trace(x = ~test$LOCATION, y = ~test$`PROFILE RIGHT`, name = 'PROFILE RIGHT', fill = 'tozeroy'
                  # ,fillcolor = 'rgba(255, 212, 96, 0.5)')
        ) %>%
        add_trace(x = ~test$LOCATION, y = ~test$SUP, name = 'SUP', fill = 'tozeroy'
                  # ,fillcolor = 'rgba(255, 212, 96, 0.5)')
        ) %>%
        add_trace(x = ~test$LOCATION, y = ~test$`TWIST 3M`, name = 'TWIST 3M', fill = 'tozeroy'
                  # , fillcolor = 'rgba(255, 212, 96, 0.5)')
        ) %>%
        layout(
          title = "FIX",
          xaxis = list( rangeslider = list(type = "LOCATION"),title=""),
          yaxis = list(title = "INSPECT"))

      p1=p %>% layout(xaxis=list(range=c(caution[order,9]-0.004,caution[order,9]+0.004)))
      name=paste0("c:/Users/bigTeam/workspace/bigTeam/WebContent/html/segment.html")
      saveWidget(p1,name)

    }#function
  )#cmpfun
  A()##

}#function
