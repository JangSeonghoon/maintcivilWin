#' window AJVAnotifyDBAggre
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

#' @importFrom csvread map.coltypes
#' @importFrom csvread csvread
#' @importFrom compiler cmpfun
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_sub
#' @importFrom stringr str_c
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom RJDBC JDBC
#' @importFrom DBI dbConnect
#' @importFrom DBI dbSendQuery
#' @importFrom DBI dbExecute
#' @importFrom DBI dbFetch
#' @importFrom DBI dbHasCompleted
#' @importFrom DBI dbWriteTable
#' @importFrom DBI dbDisconnect
#' @importFrom magrittr %>%
#' @export

JAVAnotifyDBAggre_win=
function (year, quater, plant_no, workspace_no, criteria_distance)
{
  A = cmpfun(function() {
    drv = JDBC("oracle.jdbc.driver.OracleDriver", "C:/Users/JSH/Desktop/DB/ojdbc6.jar")
    conn = dbConnect(drv, "jdbc:oracle:thin:@localhost:1521:xe",
                     "korail150773", "0818")
    year = as.integer(year)
    rs = dbSendQuery(conn, paste0("select startd, lastd from PLANTLOCATION where plant='",
                                  plant_no, "' and workspace='", workspace_no, "'"))
    d = dbFetch(rs)
    startD = as.integer(d[1, 1])
    lastD = as.integer(d[1, 2])
    rs = dbSendQuery(conn, paste0("select * from TQI", year,
                                  "_", quater, "_", workspace_no, " where DISTANCE>=",
                                  startD - 1000, " and DISTANCE<=", lastD + 1000))
    temp = dbFetch(rs)
    names(temp)[1] = "distance"
    temp = temp %>% filter(distance >= startD, distance <=
                             lastD)
    i = 1
    for (i in 1:length(temp)) {
      temp[, i] = str_replace_all(temp[, i], "(\")", "")
      temp[, i] = as.numeric(temp[, i])
    }
    startD_50 = startD - 25
    startD_50 = as.numeric(startD_50)
    lastD_50 = lastD + 24.75
    lastD_50 = as.numeric(lastD_50)
    startD_100 = startD - 100
    startD_100 = as.numeric(startD_100)
    lastD_100 = lastD + 99.75
    lastD_100 = as.numeric(lastD_100)
    range_no = (startD - 1000) + 0.25 * (c(1:(4 * lastD -
                                                4 * startD + 8001)) - 1)
    range_TQI = (startD) + 200 * (c(1:((lastD - startD +
                                          200)/200)) - 1)
    tqi_no = c(4, 7, 10, 13, 16, 19, 22)
    fix_no = c(3, 6, 9, 12, 15, 18, 21)
    num = 1
    for (num in 1:length(tqi_no)) {
      if (sum(is.na(temp[, tqi_no[num]])) != 0)
        temp[is.na(temp[, tqi_no[num]]), tqi_no[num]] = 0
      if (sum(is.na(temp[, fix_no[num]])) != 0)
        temp[is.na(temp[, fix_no[num]]), fix_no[num]] = 0
    }
    original_TQI <<- (mean(temp[, 4]) + mean(temp[, 7]) +
                        mean(temp[, 10]) + mean(temp[, 13]) + mean(temp[,
                                                                        16]) + mean(temp[, 19]) + mean(temp[, 22]))/7
    temp_backup = temp
    temp_revise = temp[, c(1, 3, 6, 9, 12, 15, 18, 21)]
    TV_v = c(0, 4, 4, 4, 4, 3, 3)
    WV_v = c(0, 8, 8, 7, 7, 10, 8)
    AV_v = c(0, 13, 13, 9, 9, 20, 10)
    SV_v = c(0, 20, 20, 17, 17, 0, 21)
    alert_aggre = data.frame(distance = temp[, 1])
    n = 1
    for (n in 1:length(fix_no)) {
      if (n == 1) {
        TV_1 = -3
        TV_2 = 11
        WV_1 = -3
        WV_2 = 17
        AV_1 = -5
        AV_2 = 20
        SV_1 = -10
        SV_2 = 35
        temp = temp_backup[, c(1, fix_no[n])]
        name = str_sub(names(temp)[2], 1, str_locate(names(temp)[2],
                                                     "_")[1, 1] - 1)
        alert = temp %>% mutate(speedLimit = ifelse(temp[,
                                                         2] <= SV_1 | temp[, 2] >= SV_2, 1, 0))
        alert = alert %>% select(1:3) %>% mutate(name = ifelse(temp[,
                                                                    2] <= SV_1 | temp[, 2] >= SV_2, paste0(name,
                                                                                                           "_sv"), ifelse(temp[, 2] <= AV_1 | temp[, 2] >=
                                                                                                                            AV_2, name, 0))) %>% filter(temp[, 2] <= AV_1 |
                                                                                                                                                          temp[, 2] >= AV_2) %>% arrange(distance)
        names(alert)[c(3, 4)] = c(paste0(name, "_SV"),
                                  name)
        alert_aggre = left_join(alert_aggre, alert[!duplicated(alert[,
                                                                     c(1:4)]), c(1:4)], "distance")
      }
      else {
        TV = TV_v[n]
        WV = WV_v[n]
        AV = AV_v[n]
        SV = SV_v[n]
        temp = temp_backup[, c(1, fix_no[n])]
        name = str_sub(names(temp)[2], 1, str_locate(names(temp)[2],
                                                     "_")[1, 1] - 1)
        alert = temp %>% mutate(speedLimit = ifelse(abs(temp[,
                                                             2]) >= SV, 1, 0))
        alert = alert %>% select(1:3) %>% mutate(name = ifelse(abs(temp[,
                                                                        2]) >= SV, paste0(name, "_sv"), ifelse(abs(temp[,
                                                                                                                        2]) >= AV, name, 0))) %>% filter(abs(temp[,
                                                                                                                                                                  2]) >= AV) %>% arrange(distance)
        names(alert)[c(3, 4)] = c(paste0(name, "_SV"),
                                  name)
        alert_aggre = left_join(alert_aggre, alert[!duplicated(alert[,
                                                                     c(1:4)]), c(1:4)], "distance")
      }
    }
    notify = alert_aggre[which(ifelse(is.na(alert_aggre[,
                                                        4]) * is.na(alert_aggre[, 7]) * is.na(alert_aggre[,
                                                                                                          10]) * is.na(alert_aggre[, 13]) * is.na(alert_aggre[,
                                                                                                                                                              16]) * is.na(alert_aggre[, 19]) * is.na(alert_aggre[,
                                                                                                                                                                                                                  22]) == 1, FALSE, TRUE)), 1]
    notify = sort(notify)
    if (length(notify) != 1) {
      notify = data.frame(start = c(notify[1], notify[-length(notify)]),
                          last = notify)
      notify = notify %>% mutate(meter = last - start,
                                 range = ifelse(meter == 0.25, 0, 1))
      start = which(notify$range == 1) + 1
      last = c(which(notify$range == 1), length(notify[,
                                                       1]))[-1]
      distRange = numeric(0)
      cluster = integer(0)
      cul = 1
      clustNo = 1
      for (cul in 1:length(notify[, 1])) {
        if (cul == 1) {
          distRange[cul] = notify$meter[1]
          cluster[cul] = 1
        }
        else {
          distRange[cul] = ifelse(distRange[cul - 1] +
                                    notify$meter[cul] > criteria_distance * 1.2,
                                  0, distRange[cul - 1] + notify$meter[cul])
          if (distRange[cul - 1] + notify$meter[cul] >
              criteria_distance * 1.2) {
            clustNo = clustNo + 1
            cluster[cul] = clustNo
          }
          else {
            cluster[cul] = clustNo
          }
        }
        if (cul == length(notify[, 1])) {
          cluster = ifelse(notify$range == 1, 0, cluster)
          cluster[1] = 1
          notify = notify %>% mutate(cul = distRange,
                                     clust = cluster)
        }
        print(paste0(cul, "/", length(notify[, 1])))
      }
    }
    names(notify)[1] = names(temp_revise)[1]
    temp_revise = left_join(temp_revise, alert_aggre[, -c(2,
                                                          5, 8, 11, 14, 17, 20)], by = names(temp_revise)[1])
    temp_revise = left_join(temp_revise, notify[, c(1, 6)],
                            by = names(temp_revise)[1])
    backup = temp_revise
    temp_revise = backup
    i = 1
    startD = numeric(0)
    lastD = numeric(0)
    for (i in 1:unique(notify$clust)[length(unique(notify$clust))]) {
      startD[i] = ifelse(is.na(notify[which(notify$clust ==
                                              i)[1], 1]), 0, notify[which(notify$clust == i)[1],
                                                                    1])
      lastD[i] = ifelse(length(notify[which(notify$clust ==
                                              i)[length(which(notify$clust == i))], 2]) ==
                          0, 0, notify[which(notify$clust == i)[length(which(notify$clust ==
                                                                               i))], 2])
    }
    caution = data.frame(seq = seq(length(startD)), startD = startD,
                         lastD = lastD)
    if (!length(which(caution$startD == 0)) == 0)
      caution = caution[-which(caution$startD == 0), ]
    backup = temp_revise
    temp_revise = backup
    speedLimit = integer(0)
    count = integer(0)
    indexTQI = numeric(0)
    kind_fix = character(0)
    group = integer(0)
    i = 1
    for (i in 1:length(caution[, 1])) {
      temp_revise = backup
      loc = which(temp_revise$clust == caution$seq[i])
      count[i] = length(loc) * 0.25
      temp_revise[loc, 2] = ifelse(temp_revise[loc, 2] >
                                     20, 11, temp_revise[loc, 2])
      temp_revise[loc, 2] = ifelse(temp_revise[loc, 2] <- 5,
                                   -3, temp_revise[loc, 2])
      temp_revise[loc, 3] = ifelse(!abs(temp_revise[loc,
                                                    3]) > AV_v[2], temp_revise[loc, 3], ifelse(temp_revise[loc,
                                                                                                           3] > 0, AV_v[2], -AV_v[2]))
      temp_revise[loc, 4] = ifelse(!abs(temp_revise[loc,
                                                    4]) > AV_v[3], temp_revise[loc, 4], ifelse(temp_revise[loc,
                                                                                                           4] > 0, AV_v[3], -AV_v[3]))
      temp_revise[loc, 5] = ifelse(!abs(temp_revise[loc,
                                                    5]) > AV_v[4], temp_revise[loc, 5], ifelse(temp_revise[loc,
                                                                                                           5] > 0, AV_v[4], -AV_v[4]))
      temp_revise[loc, 6] = ifelse(!abs(temp_revise[loc,
                                                    6]) > AV_v[5], temp_revise[loc, 6], ifelse(temp_revise[loc,
                                                                                                           6] > 0, AV_v[5], -AV_v[5]))
      temp_revise[loc, 7] = ifelse(!abs(temp_revise[loc,
                                                    7]) > AV_v[6], temp_revise[loc, 7], ifelse(temp_revise[loc,
                                                                                                           7] > 0, AV_v[6], -AV_v[6]))
      temp_revise[loc, 8] = ifelse(!abs(temp_revise[loc,
                                                    8]) > AV_v[7], temp_revise[loc, 8], ifelse(temp_revise[loc,
                                                                                                           8] > 0, AV_v[7], -AV_v[7]))
      no = which((caution$startD[i] < range_TQI) == TRUE)[1] -
        1
      TQI = (temp_backup %>% filter(distance %in% temp_backup$distance[which((temp_backup$distance%%200 ==
                                                                                1) == TRUE)]))
      TQI_fix_GAGE = TQI[, 4]
      TQI_fix_PRL = TQI[, 7]
      TQI_fix_PRR = TQI[, 10]
      TQI_fix_ALL = TQI[, 13]
      TQI_fix_ALR = TQI[, 16]
      TQI_fix_SUP = TQI[, 19]
      TQI_fix_TWIST = TQI[, 22]
      TQI_fix_GAGE[no] = sd((temp_revise %>% select(1,
                                                    2) %>% filter(distance >= range_TQI[no], distance <
                                                                    range_TQI[no + 1]))[, 2])
      TQI_fix_PRL[no] = sd((temp_revise %>% select(1, 3) %>%
                              filter(distance >= range_TQI[i], distance < range_TQI[i +
                                                                                      1]))[, 2])
      TQI_fix_PRR[no] = sd((temp_revise %>% select(1, 4) %>%
                              filter(distance >= range_TQI[i], distance < range_TQI[i +
                                                                                      1]))[, 2])
      TQI_fix_ALL[no] = sd((temp_revise %>% select(1, 5) %>%
                              filter(distance >= range_TQI[i], distance < range_TQI[i +
                                                                                      1]))[, 2])
      TQI_fix_ALR[no] = sd((temp_revise %>% select(1, 6) %>%
                              filter(distance >= range_TQI[i], distance < range_TQI[i +
                                                                                      1]))[, 2])
      TQI_fix_SUP[no] = sd((temp_revise %>% select(1, 7) %>%
                              filter(distance >= range_TQI[i], distance < range_TQI[i +
                                                                                      1]))[, 2])
      TQI_fix_TWIST[no] = sd((temp_revise %>% select(1,
                                                     8) %>% filter(distance >= range_TQI[i], distance <
                                                                     range_TQI[i + 1]))[, 2])
      A = (mean(TQI_fix_GAGE) + mean(TQI_fix_PRL) + mean(TQI_fix_PRR) +
             mean(TQI_fix_ALL) + mean(TQI_fix_ALR) + mean(TQI_fix_SUP) +
             mean(TQI_fix_TWIST))/7
      indexTQI[i] = round(((original_TQI - A)/original_TQI) *
                            100, digits = 2)
      B = unique(c(ifelse(is.na(temp_revise[loc, 10]),
                          "", temp_revise[loc, 10]), ifelse(is.na(temp_revise[loc,
                                                                              12]), "", temp_revise[loc, 12]), ifelse(is.na(temp_revise[loc,
                                                                                                                                        14]), "", temp_revise[loc, 14]), ifelse(is.na(temp_revise[loc,
                                                                                                                                                                                                  16]), "", temp_revise[loc, 16]), ifelse(is.na(temp_revise[loc,
                                                                                                                                                                                                                                                            18]), "", temp_revise[loc, 18]), ifelse(is.na(temp_revise[loc,
                                                                                                                                                                                                                                                                                                                      20]), "", temp_revise[loc, 20]), ifelse(is.na(temp_revise[loc,
                                                                                                                                                                                                                                                                                                                                                                                22]), "", temp_revise[loc, 22])))
      B = B[which(nchar(B) != 0)]
      kind_fix[i] = str_c(B, collapse = " / ")
      D = sum(ifelse(is.na(temp_revise[loc, 9]), 0, temp_revise[loc,
                                                                9]), ifelse(is.na(temp_revise[loc, 11]), 0, temp_revise[loc,
                                                                                                                        11]), ifelse(is.na(temp_revise[loc, 13]), 0,
                                                                                                                                     temp_revise[loc, 13]), ifelse(is.na(temp_revise[loc,
                                                                                                                                                                                     15]), 0, temp_revise[loc, 15]), ifelse(is.na(temp_revise[loc,
                                                                                                                                                                                                                                              17]), 0, temp_revise[loc, 17]), ifelse(is.na(temp_revise[loc,
                                                                                                                                                                                                                                                                                                       19]), 0, temp_revise[loc, 19]), ifelse(is.na(temp_revise[loc,
                                                                                                                                                                                                                                                                                                                                                                21]), 0, temp_revise[loc, 21]))
      speedLimit[i] = D
      group[i] = 4
      if (str_detect(kind_fix[i], "AL"))
        group[i] = 3
      if (str_detect(kind_fix[i], "TWIST"))
        group[i] = 2
      if (D != 0)
        group[i] = 1
      print(paste0(i, "/", length(caution[, 1])))
    }
    caution = caution %>% mutate(count = count, speedLimit = speedLimit,
                                 indexTQI = indexTQI, kind_fix = kind_fix, group = group) %>%
      arrange(group, desc(indexTQI))
    if (length(caution[, 1]) != 0) {
      try(rs <- dbExecute(conn, "drop table temporary"),
          silent = T)
      dbHasCompleted(rs)
      execCaution = caution
      names(execCaution) = NULL
      dbWriteTable(conn, "temporary", execCaution)
      dbWriteTable
      dbDisconnect(conn)
    }
    else {
      caution = data.frame(start = c(0), last = c(0), count = c(0),
                           speedLimit = c(0), adjustTQI = c(0), original_TQI = c(0),
                           improveTQI = c(0), indexTQI = c(0), priority = c(0))
      try(rs <- dbExecute(conn, "drop table temporary"),
          silent = T)
      dbHasCompleted(rs)
      names(execCaution) = NULL
      execCaution = caution
      dbWriteTable(conn, "temporary", execCaution)
      dbDisconnect(conn)
    }
    return(caution)
  })
  A()
}
