setproj(19)

# Prep naics3 level, for relevant years
qwidb.connect(p$qwidb$cnty.naics3)
names = qwidb$dbplyr$ops$vars
names.id = names[1:16]
names.var = names[17:length(names)]
names.id.good = setdiff(names.id, c('periodicity', 'seasonadj', 'geo_level', 'ind_level', 'ownercode','firmage','firmsize'))

system.time({
for (year in 2005:2016) {
    test = as.integer(year)
    z = qwidb$dbplyr %>% select(c(names.id.good, names.var)) %>% filter(sex == 0 & agegrp == 'A00' & year == test) %>% collect()
    # print('jomama')
    #z = qwidb$dbplyr %>% filter(sex == 0 & agegrp == 'A00' & year == year) %>% collect()
    fwrite(z, paste0('wrkdata/qwi_naics3_eth_edu_', year, '.csv'))
    rm(z)
    gc()
}
})

# Prep naics2 level, for relevant years
qwidb.connect(p$qwidb$cnty.naics2)

names = qwidb$dbplyr$ops$vars
names.id = names[1:16]
names.id.good = setdiff(names.id, c('periodicity', 'seasonadj', 'geo_level', 'ind_level', 'ownercode', 'firmage', 'firmsize'))
names.var = names[17:length(names)]
names.var.good = c('Emp','EmpS','EmpTotal','HirA','HirAS','SepS','FrmJbC','FrmJbCS','EarnS','Payroll')

vars.get = c(names.id.good, names.var.good)
z = qwidb$dbplyr %>% select(vars.get) %>% filter(sex == 0 & agegrp == 'A00' & year %in% 2009L:2016L) %>% collect()
fwrite(z, paste0('wrkdata/qwi_naics2_eth_edu.csv'))

