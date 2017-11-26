#'complete work
#'
#' @param year,quater,plant_no,workspace_no
#' @return None
devtools::use_package("rJava")
devtools::use_package("DBI")
devtools::use_package("RJDBC")
#' @importFrom compiler cmpfun
#' @importFrom RJDBC JDBC
#' @importFrom DBI dbConnect
#' @importFrom DBI dbSendQuery
#' @importFrom DBI dbExecute
#' @importFrom DBI dbFetch
#' @importFrom DBI dbHasCompleted
#' @importFrom DBI dbWriteTable
#' @importFrom DBI dbDisconnect

#' @export
execute_win=function(num,year,quater,plant_no,workspace_no){
  A=cmpfun(
    function(){

      drv=JDBC("oracle.jdbc.driver.OracleDriver","C:/Users/JSH/Desktop/DB/ojdbc6.jar")
      conn=dbConnect(drv,"jdbc:oracle:thin:@localhost:1521:xe","korail150773","0818")

      # rs=dbSendQuery(conn,paste0("select * from TQI",year,"_",quater,"_",workspace_no,"_BACKUP"))
      rs=dbSendQuery(conn,paste0("select * from TQI",year,"_",quater,"_",workspace_no))
      result=dbFetch(rs)

      rs1=dbSendQuery(conn,"select * from temporary")
      result2=dbFetch(rs1)


      i=1;for(i in 1:length(num)){
        num[i]=as.integer(num[i])
        startD=result2[num[i],2]
        lastD=result2[num[i],3]

        TV_1=-3
        TV_2=11
        WV_1=-3
        WV_2=17
        AV_1=-5
        AV_2=20
        SV_1=-10
        SV_2=35

        result[result$DISTANCE>=startD&result$DISTANCE<=lastD,3]=
          ifelse(
            result[result$DISTANCE>=startD&result$DISTANCE<=lastD,3]<=AV_1,TV_1,
            ifelse(
              result[result$DISTANCE>=startD&result$DISTANCE<=lastD,3]>=AV_2,TV_2,
              result[result$DISTANCE>=startD&result$DISTANCE<=lastD,3]
            )
          )

        TV_v=c(4,4,4,4,3,3)
        WV_v=c(8,8,7,7,10,8)
        AV_v=c(13,13,9,9,20,10)
        SV_v=c(20,20,17,17,0,21)
        column=c(6,9,12,15,18,21)

        j=1;for(j in 1:length(column)){
          result[result$DISTANCE>=startD&result$DISTANCE<=lastD,(column[j])]=
            ifelse(
              result[result$DISTANCE>=startD&result$DISTANCE<=lastD,(column[j])]<=AV_v[j]*(-1),TV_v[j]*(-1),
              ifelse(
                result[result$DISTANCE>=startD&result$DISTANCE<=lastD,(column[j])]>=AV_v[j],TV_v[j],
                result[result$DISTANCE>=startD&result$DISTANCE<=lastD,(column[j])]
              )
            )

        }
      }

      print("db")
      # try(rs<-dbExecute(conn,paste0("drop table TQI",year,"_",quater,"_",workspace_no)),
      #     silent=T)
      # dbHasCompleted(rs)

      # qry=sqlAppendTable(conn,paste0("TQI",year,"_",quater,"_",workspace_no),result,row.names=F)

      # dbWriteTable(conn,paste0("TQI",year,"_",quater,"_",workspace_no),result,row.names=F)



      # qry=sqlCreateTable(conn,paste0("TQI",year,"_",quater,"_",workspace_no),result)
      # dbExecute(conn,qry)
      # dbSendQuery(conn,qry)
      # dbDisconnect(conn)
      i=1;for(i in 1:length(result)){
        result[is.na(result[,i]),i]=""
        result[is.na(result[,i]),i]=NULL
        # result[,i]=as.numeric(result[,i])

      }
      write.csv(result,paste0("/home/jsh/DB/BigTeam/TQI",year,"_",quater,"_",workspace_no,".csv"),row.names=F)
      result<<-result
      # return(num[1])



    }#function
  )#cmpfun
  A()
}#function
