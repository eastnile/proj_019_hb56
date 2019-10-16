qwidb.connect()

class(qwidb$sql)

dbSendQuery(qwidb$sql,)

temp.data = mtcars

temp <- dbConnect(RSQLite::SQLite(), ":memory:")

dbWriteTable(temp, "mtcars", temp.data)

rs <- dbGetQuery(temp, "SELECT * FROM mtcars WHERE cyl = 4")


class(qwidb$dbplyr)

qwidb$dbplyr

z = select(qwidb$dbplyr, geography) %>% filter(geography==2)


z2 = collect(z)

z= as.tibble(temp.data)