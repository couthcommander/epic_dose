Install the "epic_dose" package to use the "epic_dose" function.

```
remotes::install_github('couthcommander/epic_dose')
```

# full example

## data generation
```
set.seed(250)
n <- 1000
isOP <- rbinom(n, 1, 0.8)
id <- sample(150, n, replace = TRUE)
rxopt <- c(
'amoxicillin 875 MG / clavulanate 125 MG Oral Tablet [Augmentin]',
'amoxicillin 80 MG/ML Oral Suspension [Amoxil]',
'amoxicillin 500 MG Oral Capsule',
'amoxicillin 120 MG/ML / clavulanate 8.58 MG/ML Oral Suspension'
)
pickrx <- sample(rxopt, n, replace = TRUE, prob = c(.4,.35,.15,.1))
rxid <- match(pickrx, rxopt)
dose_u <- character(n)
dose_u[rxid==1] <- 'tablet'
dose_u[rxid==3] <- 'mg'
dose_u[rxid==2] <- sample(c('mg/kg','mg/kg/day','mg'), sum(rxid==2), replace = TRUE)
dose_u[rxid==4] <- sample(c('mg/kg','mg/kg/day','mg','mL'), sum(rxid==4), replace = TRUE)
dose_n <- numeric(n)
dose_n[rxid==1] <- 1
dose_n[rxid==3] <- sample(c(500,1000,2000), sum(rxid==3), replace = TRUE)
du <- match(dose_u, c('mg/kg','mg/kg/day','mg','mL'))
dose_n[rxid==2 & du==1] <- sample(c(45, 50, 10, 25), sum(rxid==2 & du==1), replace = TRUE)
dose_n[rxid==2 & du==2] <- sample(c(90, 50, 80, 25), sum(rxid==2 & du==2), replace = TRUE)
dose_n[rxid==2 & du==3] <- sample(c(500, 1000, 800, 400), sum(rxid==2 & du==3), replace = TRUE)
dose_n[rxid==4 & du==1] <- 435
dose_n[rxid==4 & du==2] <- sample(c(90, 80, 40), sum(rxid==4 & du==2), replace = TRUE)
dose_n[rxid==4 & du==3] <- sample(c(875, 600), sum(rxid==4 & du==3), replace = TRUE)
dose_n[rxid==4 & du==4] <- 5
qnt <- numeric(n)
qnt[rxid %in% c(1,3)] <- sample(c(1:30, 60, 90, 180), sum(rxid %in% c(1,3)), replace = TRUE)
qnt[rxid %in% c(2,4)] <- sample(600, sum(rxid %in% c(2,4)), replace = TRUE)
time <- as.POSIXct(sample(100, n, replace = TRUE) * 86400, origin = '2020-01-01', tz = 'UTC')
time <- as.POSIXct(format(time, '%Y-%m-%d %H:%M'))
endtime <- rep(as.POSIXct(NA), n)
endtime[isOP==1] <- time[isOP==1] + sample(0:10, sum(isOP==1), replace = TRUE) * 86400
drug <- data.frame(
  MRN=id,
  DRUG_EXPOSURE_ID=NA,
  PERSON_ID=id,
  DRUG_CONCEPT_ID=rxid,
  CONCEPT_DRUG_NAME=pickrx,
  DRUG_TYPE_CONCEPT_ID=c(38000180,38000177)[isOP+1],
  DRUG_EXPOSURE_START_DATETIME=time,
  DRUG_EXPOSURE_END_DATETIME=endtime,
  STOP_REASON=NA,
  REFILLS=0,
  QUANTITY=qnt,
  DAYS_SUPPLY=NA,
  ROUTE_SOURCE_VALUE='oral',
  DOSE_UNIT_SOURCE_VALUE=dose_u,
  X_DOC_TYPE=c('EPIC IP ADMIN','EPIC OP ORDER')[isOP+1],
  X_DOC_STYPE=NA,
  X_DOSE=dose_n,
  X_DRUG_FORM='tablet',
  X_STRENGTH=c('875-125 mg','400 mg/5 mL','500 mg','600-42.9 mg/5 mL')[rxid],
  X_FREQUENCY=sample(c('daily','Every 12 hours scheduled'), n, replace = TRUE),
  X_QUANTITY_UNIT=c('tablet','mL','capsule','mL')[rxid],
  X_DURATION=NA
)

uid <- sort(unique(id))
demo <- data.frame(MRN=uid, birthDate=as.Date(sample(20*365.25, length(uid), replace = TRUE), origin = '2000-01-01'))

wgts <- data.frame(
  id=uid,
  dt=as.Date(sample(365, length(uid), replace = TRUE), origin = '2020-01-01'),
  val=sample(10:20, length(uid), replace = TRUE)
)
```

## create output
```
library(epicDose)
first_date <- as.POSIXct('2018-03-01')
dn <- 'amoxicillin'
drug0 <- drug[drug[,'X_DOC_TYPE'] %in% c('EPIC IP ADMIN','EPIC OP ORDER'),]
drug0[,'isOP'] <- grepl('OP', drug0[,'X_DOC_TYPE'])
colMap <- list(
  id = 'PERSON_ID', starttime = 'DRUG_EXPOSURE_START_DATETIME', endtime = 'DRUG_EXPOSURE_END_DATETIME',
  drugname = 'CONCEPT_DRUG_NAME', inout = 'isOP', strength = 'X_STRENGTH',
  frequency = 'X_FREQUENCY', dose = 'X_DOSE', doseunit = 'DOSE_UNIT_SOURCE_VALUE', quantity = 'QUANTITY',
  agelinkid = 'MRN', wgtlinkid = 'PERSON_ID'
)
res <- epicDose(drug0, colMap,
  age.data = demo, age.columns = list(id = 'MRN', dob = 'birthDate'), age.limits = 18,
  wgt.data = wgts, wgt.columns = list(id = 'id', datetime = 'dt', wgt = 'val'),
  earliest.date = first_date, drugname = dn
)
dim(res)
head(res)
```
