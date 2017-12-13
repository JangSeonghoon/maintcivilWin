#'
#'inspect_Measure_graph
#'
#' @param None
#' @return caution Tb(show spots to fix and working priorty)
devtools::use_package("magrittr")
devtools::use_package("stringr")
devtools::use_package("dplyr")
devtools::use_package("DMwR")
devtools::use_package("ggplot2")
devtools::use_package("rJava")
devtools::use_package("DBI")
devtools::use_package("RJDBC")

#' @importFrom compiler cmpfun
#' @importFrom magrittr %>%
#' @importFrom stringr str_extract
#' @importFrom stringr str_split
#' @importFrom stringr str_c
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom DMwR centralImputation
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_abline
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom RJDBC JDBC
#' @importFrom DBI dbConnect
#' @importFrom DBI dbSendQuery
#' @importFrom DBI dbExecute
#' @importFrom DBI dbFetch
#' @importFrom DBI dbHasCompleted
#' @importFrom DBI dbWriteTable
#' @importFrom DBI dbDisconnect
#' @export
inspect=function(order){
  A=cmpfun(
    function(){
      rm(list=ls())
      load("C:/Users/bigTeam/workspace/bigTeam/WebContent/RData/DB(utf8).RData")
      inspect_file=ls()[(length(ls())-3):length(ls())]

      drv=JDBC("oracle.jdbc.driver.OracleDriver","c:/Users/bigTeam/ojdbc6.jar")
      conn=dbConnect(drv,"jdbc:oracle:thin:@localhost:1521:xe","korail150773","0818")
      rs=dbSendQuery(conn,
                     paste0("select V4,V8,V9 FROM TEMPORARY WHERE V1=",order))
      d=dbFetch(rs)

      rs=dbSendQuery(conn,
                     paste0("select V4 FROM TEMPORARY"))

      kind=ifelse(d[1,1]=="ALIGNMENT LEFT","ALL10M",
                  ifelse(d[1,1]=="ALIGNMENT RIGHT","ALR10M",
                         ifelse(d[1,1]=="PROFILE LEFT","PRL10M",
                                ifelse(d[1,1]=="PRFILE RIGHT","PRR10M",
                                       ifelse(d[1,1]=="PRFILE LEFT","PRL10M",
                                              ifelse(d[1,1]=="TWIST 3M","TWIST3M","SUP"))
                                )
                         )
                  )
      )

      except=as.numeric(d[,3])

      max=as.numeric(d[,2])
      kind_no=ifelse(kind=="GAGE",3,
                     ifelse(kind=="PRL10M",4,
                            ifelse(kind=="PRR10M",5,
                                   ifelse(kind=="ALL10M",6,
                                          ifelse(kind=="ALR10M",7,
                                                 ifelse(kind=="SUP",8,
                                                        ifelse(kind=="TWIST3M",9,0)))))))

      startD=(except-0.2)*1000
      lastD=(except+0.2)*1000

      vector=1:((lastD-startD)*4+1)
      range=startD+0.25*(vector-1)
      range=round(range,digits=2)

      inspect=data.frame("LOCATION"=range)

      i=1;for(i in 1:4){
        if(i!=1){inspect1=inspect}
        inspect=left_join(inspect,eval(parse(text=inspect_file[i]))[,c(1,kind_no)],by="LOCATION")
        names(inspect)[length(inspect)]=paste0(names(inspect)[length(inspect)],"_",
                                               str_extract(inspect_file[i],"[0-9]{6}"))
        print(
          paste0(
            i,"/",4
          )
        )
      }
      inspect <- centralImputation(inspect)
      #####################################################################

      inspect_2=inspect %>% filter(LOCATION>=startD,LOCATION<=lastD)

      j=1;for(j in 1:3){
        k=5-j
        memory=1

        cor2=10000
        i=1;for(i in 1:100){
          if(i!=1) {cor2=ifelse(cor1<cor2,cor1,cor2)}
          range_original=101:(length(inspect_2[,5])-100)
          range_positive=i:(length(inspect_2[,k])-(201-i))
          # cor1=round(cor(inspect[range_original,5],inspect[range_positive,k])^2,digits=4)
          cor1=sum(abs(inspect_2[range_original,5]-inspect_2[range_positive,k]))

          range_negative=(100+i):(length(inspect_2[,k])-(101-i))
          # cor1_1=round(cor(inspect[range_original,5],inspect[range_negative,k])^2,digits=4)
          cor1_1=sum(abs(inspect_2[range_original,5]-inspect_2[range_negative,k]))
          cor1=ifelse(cor1<cor1_1,cor1,cor1_1)
          if(i!=1&cor1<cor2){
            memory=ifelse(cor1<cor1_1,i,i*(-1))

          }

          if(i==99){
            i=abs(memory)
            if(memory>0){
              range_positive=i:(length(inspect_2[,k])-(201-i))
              inspect_2[,k]=c(rep(0,100),inspect_2[range_positive,k],rep(0,100))
            }else if(memory<0){
              range_negative=(100+i):(length(inspect_2[,k])-(101-i))
              inspect_2[,k]=c(rep(0,100),inspect_2[range_negative,k],rep(0,100))
            }
          }#if

          print(paste0(
            "j=",j," i=",i,"/100"," cor=",cor1," ",cor2," memory=",memory
          ))
        }#for(i)

      }#for(j)

      #####################################################################

      inspect_3=inspect_2 %>% filter(LOCATION>=(except-0.02)*1000,LOCATION<=(except+0.02)*1000)
      a=which(inspect_3[,1]==except*1000)
      b=ifelse(max<0,which(inspect_3[,5]==min(inspect_3[,5])),which(inspect_3[,5]==max(inspect_3[,5])))
      c=a-b
      absc=abs(c)
      len=length(inspect_3[,1])
      if(c>0){
        inspect_3[,1]=c(inspect_3[-(1:absc),1],
                        rep(0,absc))
      }else{
        inspect_3[,1]=c(rep(0,absc),
                        inspect_3[-((len-absc+1):len),1])
      }

      inspect_3 %>%
        filter(LOCATION>=(except-0.007)*1000,LOCATION<=(except+0.007)*1000) %>%
        ggplot() +
        aes(x=LOCATION) +
        geom_line(aes(y=eval(parse(text=names(inspect[2])))),color= '#adc2eb') +
        geom_line(aes(y=eval(parse(text=names(inspect[3])))),color= '#7094db') +
        geom_line(aes(y=eval(parse(text=names(inspect[4])))),color= '#24478f') +
        geom_line(aes(y=eval(parse(text=names(inspect[5])))),color= '#e60000') +
        geom_abline(slope = 0,intercept = 0) +
        scale_x_continuous(breaks=seq((except-0.007)*1000,(except+0.007)*1000,2)) +
        theme_bw()+
        labs(x="km",y="검측치") +
        theme(axis.text.x=element_text(size=13, face="bold"),
              axis.title.x=element_text(size=15, face="bold"),
              axis.text.y=element_text(size=15, face="bold"),
              axis.title.y=element_text(size=15, face="bold"))
      ggsave("/home/jsh/eclipse-workspace/bigTeam/src/main/webapp/html/graph/inspect.jpg",
             width=20,height=14,units=c("cm"))
      print(except)

    }#fun
  )#cmpfun
  A()
}
