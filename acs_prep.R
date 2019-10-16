setproj(19)
library(censusapi)

Sys.setenv(CENSUS_KEY="f45bed1e85ca90e49e17730d7a282cec8e54a98d")

acsCounty = list()
for (i in 2010:2017) {
  z = data.table(listCensusMetadata(name = "acs/acs5/profile", vintage = i, type = "variables"))[(name %like% '^DP.') & !(name %like% '.PE$')][!(name %like% '^DP05.')]
  z = z[!(name %like% '^DP02PR.') & !(name %like% '^DP03PR.') & !(name %like% '^DP04PR.')]
  z = unique(z, by = 'label')
  z1 = try(getCensus(name = "acs/acs5/profile", vintage = i, vars = z[[1]], region = "county:*"))
  while(class(z1) == 'try-error') {
    print('Need to try one more time')
    z1 = try(getCensus(name = "acs/acs5/profile", vintage = i, vars = z[[1]], region = "county:*"))
  }
  acsCounty[[i-2009]] = z1
  names(acsCounty)[i-2009] = i
}

# --- Experient with 2010 ---
temp = data.table(acsCounty$`2011`)
temp1 = temp[state == '04'][, countyfips := as.numeric(paste0(state, county))][, c("countyfips", 'DP02_0108E', 'DP02_0114E','DP02_0115E')]
temp1 = temp1[,c(1,4)]
names(temp1) = c('region', 'value')


temp1[, sum(value)]
county_choropleth(temp1, state_zoom = 'arizona')


