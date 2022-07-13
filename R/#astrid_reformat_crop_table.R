
# reformat crop table (include for each measure the crop categories to which the measure applies)

#load crop table (under construction)
dt1<-  as.data.table(read_excel("C:/Astrid/BBWP-ecoregeling bestanden/b_lu_brp_GR220623_decast_for_client.xlsx"))

# change format from wide to long
dt2 <- melt(dt1, 
            id.vars = c("B_LU_BRP","B_LU_NAME"),
            measure.vars = patterns("^c"),
            variable.name = "BBWP_CAT",
            value.name = "boolean")

# keep only rows that indicate to which category the crop type belongs 
dt2 <- dt2[boolean == 1,]
dt2 <- dt2[,boolean := NULL]

# change format back from long to wide 
dt2 <- dcast(dt2,
             B_LU_BRP ~ rowid(B_LU_BRP),
             value.var = "BBWP_CAT", sep = ".")

# create extra columns when crop belongs in multiple categories in order that each row has unique crop type
dt1 <- setDT(dt2, key = "B_LU_BRP")[dt1, B_LU_NAME := i.B_LU_NAME]
setnames(dt1,c("1","2","3"),c("BBWP_CAT","B_LU_ECO1","B_LU_ECO2"))
setcolorder(dt1,c("B_LU_BRP","B_LU_NAME","BBWP_CAT"))

# load crop list and select relevant columns 
dt3 <- er_crops[,c("er_description","b_lu_name") := NULL]

# merge measures table with crop categories 
dt3 <- merge(dt3,dt1, by.x= "b_lu_brp", by.y = "B_LU_BRP")

# select relevant columns and keep only unique combinations
dt3 <- dt3[, c("eco_id","BBWP_CAT","B_LU_ECO1","B_LU_ECO2")]
dt3 <- (unique(dt3))

#rearrange order of eco_id in a way that the same eco_id's are among each other
dt1 <- dt3[order(eco_id)]

