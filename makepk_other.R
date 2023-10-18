library(EHR)

drugname <- 'Amoxicillin'
dn <- tolower(drugname)
load('fakedata.RData')
# need drug/demo/wgts

nwn <- EHR:::nowarnnum
vc <- EHR:::validateColumns
lu <- function(x) length(unique(x))
takeClosest <- function(dat1, dat2, id1, id2, time1, time2, val2) {
  c1 <- names(dat1)
  nt <- time2
  nv <- val2
  if(time2 %in% c1) nt <- paste0(nt,'.y')
  if(val2 %in% c1) nv <- paste0(nv,'.y')
  # add new columns to first data set
  dat1[,nt] <- as.POSIXct(NA)
  dat1[,nv] <- NA

  dat2 <- dat2[,c(id2, time2, val2)]
  dat2 <- dat2[complete.cases(dat2),]
  ldat <- split(dat2[,c(time2, val2)], dat2[,id2])
  xidc <- as.character(dat1[,id1])
  xdts <- as.Date(dat1[,time1])
  lastid <- -1
  for(i in seq_along(xidc)) {
    curid <- xidc[i]
    if(curid != lastid) {
      i_dat <- ldat[[curid]]
    }
    mix <- which.min(abs(difftime(xdts[i], i_dat[[1]])))
    dat1[i,c(nt,nv)] <- i_dat[mix,]
    lastid <- curid
  }
  dat1
}
epic_date <- as.POSIXct('2018-03-01')

makepk <- function(drug, drug.columns = NULL,
  age.data = NULL, age.columns = NULL, age.limits = NULL,
  wgt.data = NULL, wgt.columns = NULL, min.obs = 3, max.obs = 10,
  earliest.date = NA, drugname = NULL, comments = TRUE
) {
  drug.req <- list(id = NA, starttime = NA, endtime = NULL, drugname = NA,
    inout = NULL, strength = NA, frequency = NULL,
    dose = NA, doseunit = NULL, duration = NULL, quantity = NULL,
    agelinkid = NULL, wgtlinkid = NULL
  )
  age.req <- list(id = NA, datetime = NULL, age = NULL, dob = NULL)
  wgt.req <- list(id = NA, datetime = NA, wgt = NA)
  # check "drug" data columns and prepare data for standardization
  drug.col <- vc(drug, drug.columns, drug.req)
  if(length(drug.col$starttime) == 2) {
    drugDT <- paste(drug[,drug.col$starttime[1]], drug[,drug.col$starttime[2]])
  } else {
    drugDT <- drug[,drug.col$starttime]
  }
  drug[,'dt'] <- pkdata::parse_dates(drugDT)
  drug.col$starttime <- 'dt'
  if(!is.null(drug.col$endtime)) {
    if(length(drug.col$endtime) == 2) {
      drugDT <- paste(drug[,drug.col$endtime[1]], drug[,drug.col$endtime[2]])
    } else {
      drugDT <- drug[,drug.col$endtime]
    }
    drug[,'enddt'] <- pkdata::parse_dates(drugDT)
    drug.col$endtime <- 'enddt'
  } else {
    drug[,'enddt'] <- as.POSIXct(NA)
  }
  # "inout" - 0:inpatient, 1:outpatient
  # if not provided, assume outpatient
  if(is.null(drug.col$inout)) {
    drug.col$inout <- 'inout'
    drug[,'inout'] <- 1
  }
  # "frequency"
  # if not provided, assume once per day
  if(is.null(drug.col$frequency)) {
    drug.col$frequency <- 'frequency'
    drug[,'frequency'] <- 1
  }
  # "dose unit"
  # if not provided, assume "mg"
  if(is.null(drug.col$doseunit)) {
    drug.col$doseunit <- 'doseunit'
    drug[,'doseunit'] <- 'mg'
  }
  # "duration", can be missing
  if(is.null(drug.col$duration)) {
    drug.col$duration <- 'duration'
    drug[,'duration'] <- NA
  }
  # "quantity", can be missing
  if(is.null(drug.col$quantity)) {
    drug.col$quantity <- 'quantity'
    drug[,'quantity'] <- NA
  }
  # if quantity and duration are all missing, duration cannot be calculated
  noDur <- all(is.na(drug[,drug.col$quantity]) & is.na(drug[,drug.col$duration]))
  # using "id" throughout is easier
  origID <- drug.col$id
  drug.col$id <- 'id'
  idCol <- match(origID, names(drug))
  names(drug)[idCol] <- 'id'
  # possibly link to age data
  if(is.null(drug.col$agelinkid) || drug.col$agelinkid == origID) {
    drug.col$agelinkid <- drug.col$id
  }
  # possibly link to wgt data
  if(is.null(drug.col$wgtlinkid) || drug.col$wgtlinkid == origID) {
    drug.col$wgtlinkid <- drug.col$id
  }

  voi <- unique(unname(unlist(drug.col)))
  dt2num <- unclass(drug[,'enddt'])
  dt2num[is.na(dt2num)] <- Inf
  # reorder data by id|dt|-enddate
  # -enddate forces duplicate start=end to last
  drug0 <- drug[order(drug[,'id'], drug[,'dt'], -dt2num), voi]

  # a valid user's first observation must be on/after "earliest.date"
  if(!is.na(earliest.date)) {
    earliest.date <- pkdata::parse_dates(earliest.date)
    dt1_p_usr <- as.POSIXct(tapply(drug0[,'dt'], drug0[,'id'], min), origin = '1970-01-01')
    valid_date_ids <- names(dt1_p_usr)[dt1_p_usr >= earliest.date]
    drug0 <- drug0[drug0[,'id'] %in% valid_date_ids,]
  }
  # a valid user must have between [A,B] observations
  if(!is.na(min.obs) || !is.na(max.obs)) {
    obs_p_usr <- tapply(drug0[,'dt'], drug0[,'id'], lu)
    min.obs <- max(min.obs, 0, na.rm = TRUE)
    max.obs <- min(max.obs, Inf, na.rm = TRUE)
    valid_obs_ids <- names(obs_p_usr)[obs_p_usr >= min.obs & obs_p_usr <= max.obs]
    drug0 <- drug0[drug0[,'id'] %in% valid_obs_ids,]
  }

  # check "age" data columns
  if(!is.null(age.data)) {
    # this will overwrite an "age" column in drug0
    if('age' %in% names(drug0)) {
      drug0[,'age'] <- NULL
    }
    age.col <- vc(age.data, age.columns, age.req)
    secsYear <- 86400 * 365.25
    if(!is.null(age.col$datetime) && !is.null(age.col$age)) {
      if(length(age.col$datetime) == 2) {
        ageDT <- paste(age.data[,age.col$datetime[1]], age.data[,age.col$datetime[2]])
      } else {
        ageDT <- age.data[,age.col$datetime]
      }
      age.data[,'agedt'] <- pkdata::parse_dates(ageDT)
      # merge with drug by closest time and calculate age
      drug0 <- takeClosest(drug0, age.data, drug.col$agelinkid, age.col$id, 'dt', 'agedt', age.col$age)
      dd <- as.numeric(difftime(drug0[,'dt'], drug0[,'agedt'], units = 'secs'))
      # convert date difference to years
      dd <- round(dd / secsYear, 2)
      # need to adjust age by date difference
      curage <- drug0[,age.col$age] + dd
      drug0[,age.col$age] <- NULL
      drug0[,'agedt'] <- NULL
      drug0[,'age'] <- curage
    } else if(!is.null(age.col$dob)) {
      ageDOB <- age.data[,age.col$dob]
      age.data[,'dob'] <- pkdata::parse_dates(ageDOB)
      age.col$dob <- 'dob'
      # merge with drug and calculate age
      drug0 <- merge(drug0, age.data[,c(age.col$id, 'dob')], all.x = TRUE, by.x = drug.col$agelinkid, by.y = age.col$id)
      # this should always be positive
      dd <- as.numeric(difftime(drug0[,'dt'], drug0[,'dob'], units = 'secs'))
      # convert date difference to years
      drug0[,'age'] <- round(dd / secsYear, 2)
      drug0[,'dob'] <- NULL
    } else if(!is.null(age.col$age)) {
      # merge with drug, has "age"
      drug0 <- merge(drug0, age.data[,c(age.col$id, 'dob')], all.x = TRUE, by.x = drug.col$agelinkid, by.y = age.col$id)
      curage <- drug0[,age.col$age]
      drug0[,age.col$age] <- NULL
      drug0[,'age'] <- curage
    } else {
      stop('age data is provided without age or date of birth')
    }
  }
  ## does DOB work with both "date" and "datetime"?

  # find IDs who fall within age limits [A,B)
  if(!is.null(age.limits) && 'age' %in% names(drug0)) {
    min_age <- tapply(drug0[,'age'], drug0[,'id'], min)
    if(length(age.limits) == 1) {
      age.limits <- c(0, age.limits)
    } else if(length(age.limits) > 2) {
      age.limits <- range(age.limits)
    }
    valid_age_ids <- names(min_age[min_age >= age.limits[1] & min_age < age.limits[2]])
    drug0 <- drug0[drug0[,'id'] %in% valid_age_ids,]
  }

  ### count redundant data
  dupcols <- c('id', drug.col$drugname, 'dt', drug.col$inout, drug.col$dose, drug.col$frequency, drug.col$duration, drug.col$quantity, drug.col$strength)
  key0 <- do.call(paste, c(drug0[,dupcols], sep = '|'))
  if(comments) {
    c1 <- sum(duplicated(key0))
    c2 <- length(key0)
    cat('####################\n')
    cat(sprintf('raw duplicate count: %.2f%% (%s / %s)\n', 100 * c1 / c2, c1, c2))
    cat('####################\n')
  }

  voi <- unique(c('id', drug.col$wgtlinkid, 'dt', 'enddt', drug.col$frequency, drug.col$duration, drug.col$quantity,
    drug.col$drugname, drug.col$dose, drug.col$doseunit, drug.col$inout, drug.col$strength
  ))
  drug1 <- drug0[!duplicated(key0), voi]

  # standardize DOSE
  dose_num <- nwn(sub('[^0-9.]*([0-9.]+)[^0-9.]?.*', '\\1', drug1[,drug.col$dose]))
  drug1[,'dose_num'] <- dose_num

  # UNIT
  unit_val <- tolower(drug1[,drug.col$doseunit])
  drug1[,'unit'] <- unit_val
  # need UNIT|DOSE|STRENGTH
  # possibly impute from same-day outpatient?
  drug2 <- drug1[!is.na(unit_val) & !is.na(dose_num) & !is.na(drug1[,drug.col$strength]),]

  # standardize frequency
  freq <- sub('[ ]?_.*', '', tolower(drug2[,drug.col$frequency]))
  freq <- sub('ha$', 'h', freq)
  # ufreq <- sort(unique(freq))
  # cbind(ufreq, EHR::stdzFreq(ufreq), EHR::freqNum(EHR::stdzFreq(ufreq)))
  freq <- EHR::freqNum(EHR::stdzFreq(freq))
  freq[is.na(freq)] <- 1
  drug2[,'freq_num'] <- freq

  # convert "X mg/Y mL" into "X/Y mg"
  str_low <- tolower(drug2[,drug.col$strength])
  str_u <- unique(str_low)
  ml_x <- grep('/[0-9]+ ml', str_u)
  if(length(ml_x)) {
    ml_den <- sub('.*/([0-9]+) ml', '\\1', str_u[ml_x])
    mg_num <- sub('[ ]*mg.*', '', gsub(',', '', str_u[ml_x]))
    ml2mg <- function(x, y) as.numeric(x) / as.numeric(y)
    mg_val <- mapply(ml2mg, strsplit(mg_num, '-'), ml_den)
    mg_str <- paste(vapply(mg_val, paste, character(1), collapse = '-'), 'mg')
    new_str <- mg_str[match(str_low, str_u[ml_x])]
    need_conv <- which(!is.na(new_str))
    drug2[need_conv,drug.col$strength] <- new_str[need_conv]
  }

  # is there a pattern that can establish combo drugs?
  combo_ix <- grep(' / ', drug2[,drug.col$drugname])
  if(length(combo_ix) > 0 && !is.null(drugname)) {
    combo_dn <- drug2[combo_ix,drug.col$drugname]
    combo_xs <- drug2[combo_ix,drug.col$strength]
    if(any(tapply(combo_xs, combo_dn, lu) > 1)) {
      stop('unique DRUG_NAME mapped to multiple STRENGTH')
    } else {
      combo_udn <- unique(combo_dn)
      nc <- length(combo_udn)
      # map drugname to strength - check above guarantees 1-to-1
      combo_uxs <- tolower(combo_xs[match(combo_udn, combo_dn)])
      combo_str <- strsplit(sub('[ ]*mg.*', '', gsub(',', '', combo_uxs)), '-')
    }
    l_cdn <- strsplit(combo_udn, ' / ')
    ldn <- tolower(drugname)
    cds <- do.call(rbind, lapply(seq_along(combo_udn), function(i) {
      cd_i <- l_cdn[[i]]
      cd_s <- nwn(combo_str[[i]])
      a <- sub('.*(^|[^a-zA-Z])([a-zA-Z]+) ([0-9.]+) MG.*', '\\2', cd_i)
      b <- as.numeric(sub('.*(^|[^a-zA-Z])([a-zA-Z]+) ([0-9.]+) MG.*', '\\3', cd_i))
      i_ix <- which.min(stringdist::stringdist(tolower(a), ldn))
      df_i <- data.frame(drugname = a, cstr = b)
      df_i[match(cd_s, b),'xstr'] <- cd_s
      df_i[i_ix,]
    }))
  } else {
    cds <- NULL
  }

  # standardize STRENGTH
  str_in_dn <- grep('[0-9]+[ ]*mg', drug2[,drug.col$drugname], ignore.case = TRUE)
  # this grabs first; need to associate strength with drugname when combo is given
  alt_str <- as.numeric(sub('.*[^0-9]([0-9]+)$', '\\1', sub('[ ]?mg.*', '', drug2[str_in_dn,drug.col$drugname], ignore.case = TRUE)))
  drug2[str_in_dn,'str_alt'] <- alt_str
  low_str <- tolower(drug2[,drug.col$strength])
  ustr <- unique(low_str)
  ## alter stdzStrength to remove commas(,)
  stdstr <- as.numeric(EHR::stdzStrength(gsub(',', '', ustr)))
  drug2[,'str_num'] <- stdstr[match(low_str, ustr)]
  alt_ix <- !is.na(drug2[,'str_alt']) & is.na(drug2[,'str_num'])
  drug2[alt_ix,'str_num'] <- drug2[alt_ix,'str_alt']
  # replace str_alt/str_num with combo
  if(!is.null(cds)) {
    alt_str1 <- cds[match(drug2[,drug.col$drugname], combo_udn),'cstr']
    cds_ix <- which(!is.na(alt_str1))
    drug2[cds_ix,'str_alt'] <- alt_str1[cds_ix]
    drug2[cds_ix,'str_num'] <- alt_str1[cds_ix]
  }

  # check "weight" data columns
  if(!is.null(wgt.data)) {
    # this will overwrite an "wgt" column
    if('wgt' %in% names(drug2)) {
      drug2[,'wgt'] <- NULL
    }
    wgt.col <- vc(wgt.data, wgt.columns, wgt.req)
    if(length(wgt.col$datetime) == 2) {
      wgtDT <- paste(wgt.data[,wgt.col$datetime[1]], wgt.data[,wgt.col$datetime[2]])
    } else {
      wgtDT <- wgt.data[,wgt.col$datetime]
    }
    wgt.data[,'wgtdt'] <- pkdata::parse_dates(wgtDT)
    # merge with drug by closest time
    drug2 <- takeClosest(drug2, wgt.data, drug.col$wgtlinkid, wgt.col$id, 'dt', 'wgtdt', wgt.col$wgt)
    drug2[,'wgt'] <- nwn(drug2[,wgt.col$wgt])
    drug2[,'wgt.dd'] <- abs(round(as.numeric(difftime(drug2[,'dt'], drug2[,'wgtdt'], units = 'days'))))
    drug2[,'wgtdt'] <- NULL
  } else {
    drug2[,'wgt'] <- NA
    drug2[,'wgt.dd'] <- NA
  }

  voi <- c('id', 'dt', 'enddt', 'str_num', 'wgt', 'wgt.dd', 'dose_num', 'unit', 'freq_num',
    drug.col$duration, drug.col$quantity, drug.col$inout
  )
  drug3 <- drug2[, voi]
  intake <- drug3[,'dose_num']
  ntabs <- intake / drug3[,'str_num']
  kgpd_ix <- grep('kg/day', drug3[,'unit'])
  kg_ix <- setdiff(grep('kg', drug3[,'unit']), kgpd_ix)
  ml_ix <- grep('ml', drug3[,'unit'])
  nn_ix <- grep('capsule|tablet', drug3[,'unit'])

  need_kg <- sort(c(kgpd_ix, kg_ix))
  n_nowgt <- sum(is.na(drug3[need_kg,'wgt']))
  if(comments && n_nowgt > 0) {
    cat('####################\n')
    cat(sprintf('records indicating weight and missing: %0.2f\n', n_nowgt))
    cat('####################\n')
  }

  # number of tablets/etc
  ntabs[kgpd_ix] <- ntabs[kgpd_ix] / drug3[kgpd_ix,'freq_num'] * drug3[kgpd_ix,'wgt']
  ntabs[kg_ix] <- ntabs[kg_ix] * drug3[kg_ix,'wgt']
  ntabs[ml_ix] <- intake[ml_ix]
  ntabs[nn_ix] <- intake[nn_ix]

  # convert kg/day to kg
  intake[kgpd_ix] <- intake[kgpd_ix] / drug3[kgpd_ix,'freq_num'] * drug3[kgpd_ix,'wgt']
  intake[kg_ix] <- intake[kg_ix] * drug3[kg_ix,'wgt']
  intake[ml_ix] <- intake[ml_ix] * drug3[ml_ix,'str_num']
  intake[nn_ix] <- intake[nn_ix] * drug3[nn_ix,'str_num']

  drug3[,'dose.intake'] <- intake
  drug3[,'daily.dose'] <- intake * drug3[,'freq_num']
  drug3[,'daily.qty'] <- ntabs * drug3[,'freq_num']

  # standardize DURATION
  # duration is in days
  if(noDur) {
    calc_dur <- 0
  } else {
    calc_dur <- nwn(drug3[,drug.col$duration])
    dur_quant_ix <- which(is.na(calc_dur))
    calc_dur[dur_quant_ix] <- round(drug3[dur_quant_ix,drug.col$quantity] / drug3[dur_quant_ix,'daily.qty'], 1)
  }
  drug3[,'calc_dur'] <- calc_dur

  ### remove duplicates
  voi <- c('id','dt','daily.dose')
  key1 <- do.call(paste, c(drug3[,voi], sep = '|'))
  if(comments) {
    c1 <- sum(duplicated(key1))
    c2 <- length(key1)
    cat('####################\n')
    cat(sprintf('daily-dose duplicate count: %.2f%% (%s / %s)\n', 100 * c1 / c2, c1, c2))
    cat('####################\n')
  }
  drug4 <- drug3[!duplicated(key1),]

  ### count conflicting doses
  key <- do.call(paste, c(drug4[,c('id','dt')], sep = '|'))
  if(comments) {
    c1 <- sum(duplicated(key))
    c2 <- length(key)
    cat('####################\n')
    cat(sprintf('conflicting doses: %.2f%% (%s / %s)\n', 100 * c1 / c2, c1, c2))
    cat('####################\n')
  }

  dupkeys <- unique(key[duplicated(key)])
  # d1 has no duplicates
  d1 <- drug4[!key %in% dupkeys,]
  # d2 has discrepancy
  d2 <- drug4[key %in% dupkeys,]
  d1[,'conflict'] <- 0
  # conflict=2 is same start/end time
  d1[!is.na(d1[,'enddt']) & d1[,'dt'] == d1[,'enddt'], 'conflict'] <- 2
  # conflict=3 has end time prior to start
  d1[!is.na(d1[,'enddt']) & d1[,'dt'] > d1[,'enddt'], 'conflict'] <- 3
  # conflict=1 is actual discrepant values
  d2[,'conflict'] <- 1
  cond <- numeric(nrow(d2))
  cond[d2[,'dt'] < d2[,'enddt']] <- 4
  cond[d2[,'dt'] == d2[,'enddt']] <- 2
  cond[d2[,'dt'] > d2[,'enddt']] <- 1
  cond[is.na(d2[,'enddt'])] <- 3
  d2[,'cond'] <- cond

  d2key <- do.call(paste, c(d2[,c('id','dt')], sep = '|'))
  ld2 <- split(d2, d2key)
  d3 <- do.call(rbind, lapply(ld2, function(i) {
    # want one enddate on different date
    k_i <- which(i$cond == max(i$cond))
    if(length(k_i) == 1) {
      i[,'conflict'] <- 0
    }
    i[k_i,]
  }))
  d3[,'cond'] <- NULL
  d4 <- rbind(d1, d3)

  voi <- c('id','dt','enddt','daily.dose','calc_dur','conflict',drug.col$inout,'wgt.dd','wgt')
  drug5 <- d4[,voi]
  grp <- paste(drug5[,'id'], drug5[,drug.col$inout], format(drug5[,'dt'], '%Y-%m-%d'), sep = '|')

  # for ID|DT, split by IP/OP - sum IP, move OP to next day
  dby_id_doc_dt <- split(drug5, grp)
  coi <- c('id',drug.col$inout,'calc_dur','conflict','wgt.dd','wgt')
  ddC <- do.call(rbind, lapply(dby_id_doc_dt, function(i) {
    # inpatient
    if(i[1,drug.col$inout] == 0) {
      ii <- cbind(i[1,coi], date=format(i[1,'dt'], '%Y-%m-%d'))
      ddd <- sum(i[,'daily.dose'])
    # outpatient
    } else {
      ii <- cbind(i[,coi], date=format(i[,'dt'], '%Y-%m-%d'))
      ddd <- i[,'daily.dose']
    }
    cbind(ii, ddd = ddd)
  }))

  #outpat should be shifted until after inpat
  #morning/afternoon/frequency *could* all come into play
  # this reorders data by ID|DATE|INOUT
  repeat {
    grp <- sprintf("%20s|%s", ddC[,'id'], ddC[,'date'])
    isOP <- ddC[,drug.col$inout]
    grpix <- order(grp, isOP)
    ddC <- ddC[grpix,]
    g_ix <- which(duplicated(grp[grpix]) & ddC[,'conflict'] == 0 & isOP[grpix])
    if(length(g_ix) == 0) break
    dp1 <- format(as.Date(ddC[g_ix,'date']) + 1, '%Y-%m-%d')
    ddC[g_ix,'date'] <- dp1
  }
  rownames(ddC) <- NULL
  ddC[,'outpatient'] <- +(ddC[,drug.col$inout] == 1)
  res <- ddC[,c('id','date','ddd','calc_dur','wgt','wgt.dd','outpatient','conflict')]
  names(res)[c(1,3)] <- c(origID, 'dailydose')
  noWgtData <- all(is.na(res[,'wgt']))
  if(noWgtData) {
    res[,'wgt'] <- NULL
    res[,'wgt.dd'] <- NULL
  }

  if(comments) {
    if(!noWgtData) {
      cat('####################\n')
      cat('counts of wgt-date-difference on same day\n')
      print(table(res[,'wgt.dd'] == 0, useNA = 'always'))
      cat('####################\n')
      cat('####################\n')
      cat('quantile of wgt-date-difference\n')
      print(quantile(res[,'wgt.dd'], na.rm = TRUE, probs = c(0, .25, .5, .7, 90:100/100)))
      cat('####################\n')
    }

    t1 <- c(N = nrow(res), table(res[,'conflict'], useNA = 'always'))
    t2 <- round(unname(unclass(t1) / t1[1]), 4)
    cat('####################\n')
    cat('counts of conflict variable\n')
    print(cbind(cnt = t1, freq = t2))
    cat('####################\n')
  }
  res
}

drug0 <- drug[drug[,'X_DOC_TYPE'] %in% c('EPIC IP ADMIN','EPIC OP ORDER'),]
drug0[,'isOP'] <- grepl('OP', drug0[,'X_DOC_TYPE'])
colMap <- list(
  id = 'PERSON_ID', starttime = 'DRUG_EXPOSURE_START_DATETIME', endtime = 'DRUG_EXPOSURE_END_DATETIME',
  drugname = 'CONCEPT_DRUG_NAME', inout = 'isOP', strength = 'X_STRENGTH',
  frequency = 'X_FREQUENCY', dose = 'X_DOSE', doseunit = 'DOSE_UNIT_SOURCE_VALUE', quantity = 'QUANTITY',
  agelinkid = 'MRN', wgtlinkid = 'PERSON_ID'
)
res <- makepk(drug0, colMap,
  age.data = demo, age.columns = list(id = 'MRN', dob = 'birthDate'), age.limits = 18,
  wgt.data = wgts, wgt.columns = list(id = 'id', datetime = 'dt', wgt = 'val'),
  earliest.date = epic_date, drugname = dn
)

write.csv(res, file = sprintf('Zoutput-%s-children18.csv', dn), row.names = FALSE)
