#' Rail Information
#'
#' @param
#' @return
devtools::use_package("RJDBC")
devtools::use_package("DBI")
devtools::use_package("stringr")

#' @importFrom compiler cmpfun
#' @importFrom RJDBC JDBC
#' @importFrom DBI dbConnect
#' @importFrom DBI dbSendQuery
#' @importFrom DBI dbExecute
#' @importFrom DBI dbFetch
#' @importFrom DBI dbHasCompleted
#' @importFrom DBI dbWriteTable
#' @importFrom DBI dbDisconnect
#' @importFrom stringr str_replace_all
#' @export

rail_info=function(distance,workspace_no){
  A=cmpfun(
    function(){
      workspace=floor(workspace_no/100)*100
      dista=distance*1000
      load("C:/Users/bigTeam/workspace/bigTeam/WebContent/RData/DB(utf8).RData")
      listDB=c(
        paste0("bridge_",workspace), paste0("sewage_",workspace), paste0("wall_",workspace),
        paste0("steep_",workspace),  paste0("curve_",workspace),  paste0("gugyo_",workspace),
        paste0("platform_",workspace), paste0("railInfo_L_",workspace_no), paste0("railInfo_R_",workspace_no)
      )
      bridge=integer(0);sewage=integer(0);wall=integer(0)
      steep=integer(0);curve=integer(0);gugyo=integer(0);
      platform=integer(0);railInfo_L=integer(0);railInfo_R=integer(0);

      name=c(
        "bridge","sewage","wall","steep","curve","gugyo","platform","railInfo_L","railInfo_R"
      )

      table=eval(parse(text=listDB[8]))
      len=length(table[,1])
      order=1;i=1;for(i in 1:(len-1)){

        if(
          (dista>=as.numeric(table[i,1]))&(dista<=as.numeric(table[i+1,1]))){
          railInfo_L[order]=i
          order=order+1
          print(i)
        }

      }

      table=eval(parse(text=listDB[9]))
      len=length(table[,1])
      order=1;i=1;for(i in 1:(len-1)){

        if(
          (dista>=as.numeric(table[i,1]))&(dista<=as.numeric(table[i+1,1]))
        ){railInfo_R[order]=i
        order=order+1
        print(i)
        }
      }

      db=1;for(db in 1:(length(listDB)-2)){
        table=eval(parse(text=listDB[db]))
        len=length(table[,1])
        order=1;i=1;for(i in 1:len){

          if(
            (dista>=as.numeric(table[i,1]))&(dista<=as.numeric(table[i,2]))
          ){
            x=eval(parse(text=paste0(name[db])))
            x[order]=i
            assign(name[db],x)
            order=order+1
          }
          print(
            paste0(name[db]," i=",i)
          )
        }
      }
      string=list(0);string_name=character(0);
      db=3;
      order=1;for(db in 1:length(listDB)){

        a=eval(parse(text=paste0(listDB[db])))
        no=eval(parse(text=paste0(name[db])))

        if(length(a[no,1])!=0){
          colna=colnames(a[no,])
          colnames(a[no,])=""
          string[[order]]=data.frame(column=colna,value=(t(a[no,]))[,1])
          string[[order]]=sapply(string[[order]],as.character)
          string[[order]]=as.data.frame(string[[order]])
          rownames(string[[order]])=NULL
          string_name[order]=name[db]
          order=order+1
        }
        print(db)
        print(a[no,])
        names(string)=string_name
        string_name<<-string_name
        string<<-string
      }
      names(string)=str_replace_all(names(string),"(railInfo_L)","레일(좌)")
      names(string)=str_replace_all(names(string),"(railInfo_R)","레일(우)")
      names(string)=str_replace_all(names(string),"(platform)","승강장")
      names(string)=str_replace_all(names(string),"(gugyo)","구교")
      names(string)=str_replace_all(names(string),"(curve)","곡선")
      names(string)=str_replace_all(names(string),"(steep)","구배")
      names(string)=str_replace_all(names(string),"(wall)","옹벽")
      names(string)=str_replace_all(names(string),"(sewage)","하수")
      names(string)=str_replace_all(names(string),"(bridge)","교량")
      string[[order]]=data.frame(name=names(string))
      string<<-string
    }#function
  )#cmpfun
  A()
}
