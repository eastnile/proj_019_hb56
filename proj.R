libs = c('RSQLite', 'magrittr', 'tidyverse', 'data.table', 'lubridate') #'plyr'
installif(libs)
lib(libs)
rm(libs)
## Paths
p = list()
p$proj$local = paste0(gdrivepath(), 'research/proj_019_hb56/')
p$qwidb$cnty.naics3 = paste0('F:/data/qwi/qwi_cnty_naics3_latest_release.sqlite')
p$qwidb$cnty.naics2 = paste0('F:/data/qwi/qwi_cnty_naics2_latest_release.sqlite')

setwd(p$proj$local)


# Tools
tools = new.env()
#tools$j2jdb.connect = function(dbpath) {
    #j2jdb = list()
    #j2jdb$sql = dbConnect(RSQLite::SQLite(), dbname = dbpath)
    #j2jdb$metro$dbplyr = tbl(j2jdb$sql, 'metro')
    #j2jdb$state$dbplyr = tbl(j2jdb$sql, 'state')
    #assign('j2jdb', j2jdb, pos = globalenv())
#}
tools$qwidb.connect = function(dbpath) {
    qwidb = list()
    qwidb$sql = dbConnect(RSQLite::SQLite(), dbname = dbpath)
    qwidb$dbplyr = tbl(qwidb$sql, 'main')
    assign('qwidb', qwidb, pos = globalenv())
}

#Fetch schema
tools$fetchSchema = list()
tools$fetchSchema$qwi = function(schema.version) {
    url = list()
    url$schema = paste0('https://lehd.ces.census.gov/data/schema/', schema.version, '/')
    url$identifiers = paste0(url$schema, 'lehd_identifiers_qwi.csv')
    url$variables = paste0(url$schema, 'variables_qwi.csv')
    schema = list()
    schema$qwi = list()
    schema$qwi$identifiers = fread(url$identifiers)
    schema$qwi$variables = fread(url$variables)
    for (var in schema$qwi$identifiers$Variable) {
        z = paste0(url$schema, 'label_', var, '.csv')
        print(z)
        try({ schema$qwi[[var]] = fread(z) })
    }
    return(schema)
}
attach(tools)

# Set up urls
# saveRDS(fetchSchema$qwi('latest'), file = 'wrkdata/qwiSchema.rds')
schmea = readRDS('wrkdata/qwiSchema.rds')
