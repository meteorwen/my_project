library(rJava)
library(DBI)
library(RJDBC)
library(ROracle)
drv <- dbDriver("Oracle")

drv <-JDBC("oracle.jdbc.driver.OracleDriver",
           "/usr/lib/oracle/11.2/client64/lib/ojdbc6.jar", identifier.quote="\"") 
conn <- dbConnect(drv, "jdbc:oracle:thin:@219.245.40.216:1521:orcl11g",
                  "SWCJK","smt_swcjk")
dbGetQuery(conn,"select count(1) cn from t")
# http://m.codes51.com/article/detail_112922.html
dyn.load("/usr/lib/oracle/11.2/client64/lib/libclntsh.so.11.1")
library(ROracle)
drv <- dbDriver("Oracle")
con <- dbConnect(drv,username="SWCJK",password="smt_swcjk",dbname = connect.string)


host <- "219.245.40.216"
port <- 1521
svc <- "SWCJK"
connect.string <- paste(
  "(DESCRIPTION=",
  "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
  "(CONNECT_DATA=(SERVICE_NAME=", svc, ")))", sep = "")
























