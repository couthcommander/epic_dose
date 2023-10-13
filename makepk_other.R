library(EHR)

hd <- file.path('C:','Users','beckca','OneDrive - VUMC')
studyDir <- file.path(hd, 'projects','choi','mprint')
setwd(studyDir)

only_children <- TRUE
only_adults <- FALSE
use_weight <- TRUE

drugname <- 'Amoxicillin'
dn <- tolower(drugname)
load('fakedata.RData')
# need drug/demo/wgts

nwn <- EHR:::nowarnnum
lu <- function(x) length(unique(x))

epic_date <- as.POSIXct('2018-03-01')

makepk <- function(drug, drug.columns = NULL,
  age.data = NULL, age.columns = NULL, age.req = NULL,
  wgts, dn
) {
  drug.req <- list(id = NA, starttime = NA, endtime = NULL, drugname = NA,
    inout = NULL, strength = NA, frequency = NULL,
    dose = NA, doseunit = NULL, duration = NULL, quantity = NULL
  )
  age.req <- list(id = NA, druglinkid = NA, age = NULL, dob = NULL)
  # check "drug" data columns and prepare data for standardization
  drug.col <- EHR:::validateColumns(drug, drug.columns, drug.req)
  if(length(drug.col$starttime) == 2) {
    drugDT <- paste(drug[,drug.col$starttime[1]], drug[,drug.col$starttime[2]])
  } else {
    drugDT <- drug[,drug.col$starttime]
  }
  drug[,'dt'] <- pkdata::parse_dates(drugDT)
  if(!is.null(drug.col$endtime)) {
    if(length(drug.col$endtime) == 2) {
      drugDT <- paste(drug[,drug.col$endtime[1]], drug[,drug.col$endtime[2]])
    } else {
      drugDT <- drug[,drug.col$endtime]
    }
    drug[,'enddt'] <- pkdata::parse_dates(drugDT)
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

  voi <- unname(unlist(drug.col))
  dt2num <- unclass(drug[,'enddt'])
  dt2num[is.na(dt2num)] <- Inf
  # reorder data by id|dt|-enddate
  # -enddate forces duplicate start=end to last
  drug0 <- drug[order(drug[,'id'], drug[,'dt'], -dt2num), voi]

  # check "age" data columns
  age.col <- EHR:::validateColumns(age.data, age.columns, age.req)

  print(head(drug0))
  stop('')
#  voi <- c(
#  'PERSON_ID','CONCEPT_DRUG_NAME','DRUG_EXPOSURE_START_DATETIME','DRUG_EXPOSURE_END_DATETIME',
#  'X_DOC_TYPE','X_STRENGTH','X_FREQUENCY','X_DOSE','X_DURATION','QUANTITY','DOSE_UNIT_SOURCE_VALUE','MRN'
#  )


  # add age
  if(!is.null(demo)) {
    dob <- as.Date(demo[,'birthDate'])
    dob0 <- dob[match(drug0[,'MRN'], demo[,'MRN'])]
    age_yrs <- (as.Date(drug0[,'dt']) - dob0) / 365.25
    drug0[,'age'] <- round(as.numeric(age_yrs), 2)
    min_age <- tapply(drug0[,'age'], drug0[,'MRN'], min)
    children_mrn <- names(min_age[min_age < 18])
    adult_mrn <- names(min_age[min_age >= 18])
  }

  dt1_p_usr <- as.POSIXct(tapply(drug0[,'dt'], drug0[,'MRN'], min), origin = '1970-01-01')
  obs_p_usr <- tapply(drug0[,'dt'], drug0[,'MRN'], lu)
  # is earliest date after EPIC switch?
  only_epic <- names(dt1_p_usr)[dt1_p_usr >= epic_date]
  # at least 3 observations?
  has_multi <- names(obs_p_usr)[obs_p_usr > 2 & obs_p_usr < 11]
  good_user <- intersect(only_epic, has_multi)

  if(only_children) good_user <- intersect(good_user, children_mrn)
  if(only_adults) good_user <- intersect(good_user, adult_mrn)

  # remove adults
  drugA <- drug0[drug0[,'MRN'] %in% good_user,]

  ### count redundant data
  key0 <- do.call(paste, c(drugA[,c('PERSON_ID', 'CONCEPT_DRUG_NAME', 'dt', 'X_DOC_TYPE', 'X_DOSE', 'X_FREQUENCY', 'X_DURATION', 'QUANTITY', 'X_STRENGTH')], sep = '|'))
  c1 <- sum(duplicated(key0))
  c2 <- length(key0)
  sprintf('%.2f%% (%s / %s)', 100 * c1 / c2, c1, c2)

  voi <- c('MRN','PERSON_ID','dt','enddt','X_FREQUENCY','X_DURATION','QUANTITY','CONCEPT_DRUG_NAME','X_DOSE','DOSE_UNIT_SOURCE_VALUE','X_DOC_TYPE','X_STRENGTH')
  drug1 <- drugA[!duplicated(key0), voi]

  # standardize DOSE
  dose_num <- nwn(sub('[^0-9.]*([0-9.]+)[^0-9.]?.*', '\\1', drug1[,'X_DOSE']))
  drug1[,'dose_num'] <- dose_num

  # UNIT
  unit_val <- tolower(drug1[,'DOSE_UNIT_SOURCE_VALUE'])
  drug1[,'unit'] <- unit_val
  table(unit_val)
  # need UNIT|DOSE|STRENGTH
  # possibly impute from same-day outpatient?
  drug2 <- drug1[!is.na(unit_val) & !is.na(dose_num) & !is.na(drug1[,'X_STRENGTH']),]

  # standardize frequency
  freq <- sub('[ ]?_.*', '', tolower(drug2[,'X_FREQUENCY']))
  freq <- sub('ha$', 'h', freq)
  # ufreq <- sort(unique(freq))
  # cbind(ufreq, EHR::stdzFreq(ufreq), EHR::freqNum(EHR::stdzFreq(ufreq)))
  freq <- EHR::freqNum(EHR::stdzFreq(freq))
  freq[is.na(freq)] <- 1
  drug2[,'freq_num'] <- freq

  # convert "X mg/Y mL" into "X/Y mg"
  str_low <- tolower(drug2[,'X_STRENGTH'])
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
    drug2[need_conv,'X_STRENGTH'] <- new_str[need_conv]
  }

  # is there a pattern that can establish combo drugs?
  combo_ix <- grep(' / ', drug2[,'CONCEPT_DRUG_NAME'])
  if(length(combo_ix) > 0) {
    combo_dn <- drug2[combo_ix,'CONCEPT_DRUG_NAME']
    combo_xs <- drug2[combo_ix,'X_STRENGTH']
    if(any(tapply(combo_xs, combo_dn, lu) > 1)) {
      stop('unique CONCEPT_DRUG_NAME mapped to multiple X_STRENGTH')
    } else {
      combo_udn <- unique(combo_dn)
      nc <- length(combo_udn)
      # map drugname to strength - check above guarantees 1-to-1
      combo_uxs <- tolower(combo_xs[match(combo_udn, combo_dn)])
      combo_str <- strsplit(sub('[ ]*mg.*', '', gsub(',', '', combo_uxs)), '-')
    }
    l_cdn <- strsplit(combo_udn, ' / ')
    cds <- do.call(rbind, lapply(seq_along(combo_udn), function(i) {
      cd_i <- l_cdn[[i]]
      cd_s <- nwn(combo_str[[i]])
      a <- sub('.*(^|[^a-zA-Z])([a-zA-Z]+) ([0-9.]+) MG.*', '\\2', cd_i)
      b <- as.numeric(sub('.*(^|[^a-zA-Z])([a-zA-Z]+) ([0-9.]+) MG.*', '\\3', cd_i))
      i_ix <- which.min(stringdist::stringdist(tolower(a), tolower(dn)))
      df_i <- data.frame(drugname = a, cstr = b)
      df_i[match(cd_s, b),'xstr'] <- cd_s
      df_i[i_ix,]
    }))
  } else {
    cds <- NULL
  }

  # standardize STRENGTH
  str_in_dn <- grep('[0-9]+[ ]*mg', drug2[,'CONCEPT_DRUG_NAME'], ignore.case = TRUE)
  # this grabs first; need to associate strength with drugname when combo is given
  alt_str <- as.numeric(sub('.*[^0-9]([0-9]+)$', '\\1', sub('[ ]?mg.*', '', drug2[str_in_dn,'CONCEPT_DRUG_NAME'], ignore.case = TRUE)))
  drug2[str_in_dn,'str_alt'] <- alt_str
  drug2[,'str'] <- tolower(drug2[,'X_STRENGTH'])
  ustr <- unique(drug2[,'str'])
  ## alter stdzStrength to remove commas(,)
  stdstr <- as.numeric(EHR::stdzStrength(gsub(',', '', ustr)))
  drug2[,'str_num'] <- stdstr[match(drug2[,'str'], ustr)]
  alt_ix <- !is.na(drug2[,'str_alt']) & is.na(drug2[,'str_num'])
  drug2[alt_ix,'str_num'] <- drug2[alt_ix,'str_alt']
  # replace str_alt/str_num with combo
  if(!is.null(cds)) {
    alt_str1 <- cds[match(drug2[,'CONCEPT_DRUG_NAME'], combo_udn),'cstr']
    cds_ix <- which(!is.na(alt_str1))
    drug2[cds_ix,'str_alt'] <- alt_str1[cds_ix]
    drug2[cds_ix,'str_num'] <- alt_str1[cds_ix]
  }

  # find "weight" by closest time
  drug2[,'wgt'] <- NA
  drug2[,'wgt.dd'] <- NA
  if(use_weight) {
    wgts <- wgts[wgts[,'id'] %in% drug2[,'PERSON_ID'],]
    wgt.ix <- tapply(seq(nrow(wgts)), wgts[,'id'], I)
    xidc <- as.character(drug2[,'PERSON_ID'])
    xdts <- as.Date(drug2[,'dt'])
    lastid <- -1
    for(i in seq_along(xidc)) {
      curid <- xidc[i]
      if(curid != lastid) {
        wgt_opt <- wgts[wgt.ix[[curid]], c('dt','val')]
        wgt_opt <- wgt_opt[complete.cases(wgt_opt), ]
      }
      if(nrow(wgt_opt) > 0) {
        # based on EHR::takeClosest
        dist <- abs(difftime(xdts[i], wgt_opt[[1]], units = 'hours'))
        drug2[i,'wgt'] <- wgt_opt[[2]][which.min(dist)]
        drug2[i,'wgt.dd'] <- round(as.numeric(min(dist) / 24))
      }
      lastid <- curid
    }
    drug2[,'wgt'] <- nwn(drug2[,'wgt'])
  }
  sum(is.na(drug2[,'wgt']))
  # users without weight
  sprintf('%0.2f%%', 100 * lu(drug2[is.na(drug2[,'wgt']),'MRN']) / lu(drug2[,'MRN']))

  # need MG/ML from DRUGNAME with unit=='ml'
  # ml_ix <- which(drug2[,'unit'] == 'ml' & grepl('MG/ML', drug2[,'CONCEPT_DRUG_NAME']))
  # drug2[ml_ix,'str_mgml'] <- cds_full[ml_ix,'cstr']
  # need KG from ??? with unit=='kg'

  voi <- c('MRN','PERSON_ID','dt','enddt','str_num','wgt','wgt.dd','dose_num','unit','freq_num','QUANTITY','X_DOC_TYPE')
  drug3 <- drug2[, voi]
  intake <- drug3[,'dose_num']
  ntabs <- intake / drug3[,'str_num']
  kgpd_ix <- grep('kg/day', drug3[,'unit'])
  kg_ix <- setdiff(grep('kg', drug3[,'unit']), kgpd_ix)
  ml_ix <- grep('ml', drug3[,'unit'])
  nn_ix <- grep('capsule|tablet', drug3[,'unit'])

  need_kg <- sort(c(kgpd_ix, kg_ix))
  sum(is.na(drug3[need_kg,'wgt']))
  # drug3[need_kg[is.na(drug3[need_kg,'wgt'])],]

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
  # DURATION applies to outpatient
  # duration is in days
  calc_dur <- round(drug3[,'QUANTITY'] / drug3[,'daily.qty'], 1)
  drug3[,'calc_dur'] <- calc_dur

  ### remove duplicates
  voi <- c('MRN','PERSON_ID','dt','daily.dose')
  key1 <- do.call(paste, c(drug3[,voi], sep = '|'))
  c1 <- sum(duplicated(key1))
  c2 <- length(key1)
  sprintf('%.2f%% (%s / %s)', 100 * c1 / c2, c1, c2)
  drug4 <- drug3[!duplicated(key1),]

  ### count conflicting doses
  key <- do.call(paste, c(drug4[,c('PERSON_ID','dt')], sep = '|'))
  c1 <- sum(duplicated(key))
  c2 <- length(key)
  sprintf('%.2f%% (%s / %s)', 100 * c1 / c2, c1, c2)

  dupkeys <- unique(key[duplicated(key)])
  # d1 has no duplicates
  d1 <- drug4[!key %in% dupkeys,]
  # d2 has discrepancy
  d2 <- drug4[key %in% dupkeys,]
  d1[,'conflict'] <- 0
  d1[!is.na(d1[,'enddt']) & d1[,'dt'] == d1[,'enddt'], 'conflict'] <- 2
  d1[!is.na(d1[,'enddt']) & d1[,'dt'] > d1[,'enddt'], 'conflict'] <- 3
  d2[,'conflict'] <- 1
  cond <- numeric(nrow(d2))
  cond[d2[,'dt'] < d2[,'enddt']] <- 4
  cond[d2[,'dt'] == d2[,'enddt']] <- 2
  cond[d2[,'dt'] > d2[,'enddt']] <- 1
  cond[is.na(d2[,'enddt'])] <- 3
  d2[,'cond'] <- cond

  d2key <- do.call(paste, c(d2[,c('PERSON_ID','dt')], sep = '|'))
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

  voi <- c('MRN','PERSON_ID','dt','enddt','daily.dose','calc_dur','conflict','X_DOC_TYPE','wgt.dd','wgt')
  drug5 <- d4[,voi]
  grp <- paste(drug5[,'MRN'], drug5[,'X_DOC_TYPE'], format(drug5[,'dt'], '%Y-%m-%d'), sep = '|')
  res0 <- drug5[order(grp),]
  rownames(res0) <- NULL

  # for MRN|DT, split by IP/OP - sum IP, move OP to next day
  dby_id_doc_dt <- split(drug5, grp)
  coi <- c('MRN','PERSON_ID','X_DOC_TYPE','calc_dur','conflict','wgt.dd','wgt')
  dd_coll <- do.call(rbind, lapply(dby_id_doc_dt, function(i) {
    if(grepl('IP', i[1,'X_DOC_TYPE'])) {
      ii <- cbind(i[1,coi], date=format(i[1,'dt'], '%Y-%m-%d'))
      ddd <- sum(i[,'daily.dose'])
    } else {
      ii <- cbind(i[,coi], date=format(i[,'dt'], '%Y-%m-%d'))
      ddd <- i[,'daily.dose']
    }
    cbind(ii, ddd = ddd)
  }))

  ###
  #outpat should be shifted until after inpat
  #morning/afternoon/frequency *could* all come into play

  #need to understand same day inpat/outpat transition
  ###
  ddC <- dd_coll
  repeat {
    grp <- sprintf("%20s|%s", ddC[,'MRN'], ddC[,'date'])
    isOP <- grepl('OP', ddC[,'X_DOC_TYPE'])
    grpix <- order(grp, isOP)
    ddC <- ddC[grpix,]
    g_ix <- which(duplicated(grp[grpix]) & ddC[,'conflict'] == 0 & isOP[grpix])
    if(length(g_ix) == 0) break
    dp1 <- format(as.Date(ddC[g_ix,'date']) + 1, '%Y-%m-%d')
    ddC[g_ix,'date'] <- dp1
  }
  res <- ddC
  rownames(res) <- NULL

  ###

  res
}

drug0 <- drug[drug[,'X_DOC_TYPE'] %in% c('EPIC IP ADMIN','EPIC OP ORDER'),]
drug0[,'isOP'] <- grepl('OP', drug0[,'X_DOC_TYPE'])
colMap <- list(
  id = 'PERSON_ID', starttime = 'DRUG_EXPOSURE_START_DATETIME', endtime = 'DRUG_EXPOSURE_END_DATETIME',
  drugname = 'CONCEPT_DRUG_NAME', inout = 'isOP', strength = 'X_STRENGTH',
  frequency = 'X_FREQUENCY', dose = 'X_DOSE', doseunit = 'DOSE_UNIT_SOURCE_VALUE', quantity = 'QUANTITY'
)
res <- makepk(drug0, colMap, demo, wgts, dn)

if(only_children & !only_adults) {
  name_post <- paste0(dn, '-children18')
} else if(!only_children & only_adults) {
  name_post <- paste0(dn, '-adults')
} else {
  name_post <- dn
}

write.csv(res, file = sprintf('output-%s.csv', name_post), row.names = FALSE)
