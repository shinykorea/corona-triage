backupPat = function(auth, Link, Link2){
  
  triage <- function(v) {
    PT <- PCO <- 0
    POT <- 0
    if (is.na(v$호흡곤란)) {
      PCF <- ""
      # NO POT ADD
    }
    else {
      PCF <- v$호흡곤란
      POT <- POT + PCF
    }
    
    if (is.na(v$가벼운불안)) {
      PM <- ""
      # NO POT ADD
    }
    else {
      PM <- v$가벼운불안
      POT <- POT + PM
    }
    
    ##### PT
    
    T <- v$체온
    if (!is.na(T)) {
      if (T <= 35) PT <- 3
      if (T <= 36 || T > 39) PT <- max(2, PT)
      if (T > 38) PT <- max(1, PT)
      POT <- POT + PT
    }
    else {
      PT <- ""
      # NO POT ADD
    }
    
    ##### PCO
    
    CO <- v$의식저하
    if (is.na(CO)) {
      PCO <- ""
      # NO POT ADD
    }
    else {
      if (CO) PCO <- 3
      POT <- POT + PCO
    }
    
    return(c(PT, PCF, PCO, PM, POT))
  }
  
  sheets_auth(auth) # hide.
  
  today <- Sys.Date()
  
  sheets <- sheets_sheets(Link)
  sheets <- sheets[which(lubridate::as_date(sheets) < today)] # 오늘 이전 탭만 읽음
  
  Pat <- c()
  
  for (i in 1:length(sheets)) {
    PatTemp <- read_sheet(Link, sheet = sheets[i]) # first sheets
    PatTemp$temperature <- as.numeric(unlist(PatTemp$temperature))
    PatTemp$mental <- as.numeric(unlist(PatTemp$mental))
    PatTemp$anxiety <- as.numeric(unlist(PatTemp$anxiety))
    PatTemp$dyspnea <- as.numeric(unlist(PatTemp$dyspnea))
    PatTemp$sao2 <- as.numeric(unlist(PatTemp$sao2))
    PatTemp$HR <- as.numeric(unlist(PatTemp$HR))
    PatTemp$PCR <- as.numeric(unlist(PatTemp$PCR))
    Pat <- rbind(Pat, PatTemp)
  }
  
  sheets <- sheets_sheets(Link2)
  sheets <- sheets[which(lubridate::as_date(sheets) < today)] # 오늘 이전 탭만 읽음
  
  for (i in 1:length(sheets)) {
    PatTemp <- read_sheet(Link2, sheet = sheets[i]) # first sheets
    PatTemp$temperature <- as.numeric(unlist(PatTemp$temperature))
    PatTemp$mental <- as.numeric(unlist(PatTemp$mental))
    PatTemp$anxiety <- as.numeric(unlist(PatTemp$anxiety))
    PatTemp$dyspnea <- as.numeric(unlist(PatTemp$dyspnea))
    PatTemp$sao2 <- as.numeric(unlist(PatTemp$sao2))
    PatTemp$HR <- as.numeric(unlist(PatTemp$HR))
    PatTemp$PCR <- as.numeric(unlist(PatTemp$PCR))
    Pat <- rbind(Pat, PatTemp)
  }
  
  colnames(Pat) <- c(
    "주민등록번호", "이름", "체온", "의식저하", "가벼운불안",
    "호흡곤란", "산소포화도", "호흡수", "맥박", "PCR",
    "퇴원여부", "센터", "입력날짜", "차수"
  )
  
  Age <- sapply(1:nrow(Pat), function(i) {
    # Year
    if (substr(Pat$주민등록번호[i], 7, 7) > 2) { ## after 2000
      day <- (today - as.Date(paste0("20", substr(Pat$주민등록번호[i], 1, 6)), "%Y%m%d"))[[1]]
    }
    else { # before 2000
      day <- (today - as.Date(paste0("19", substr(Pat$주민등록번호[i], 1, 6)), "%Y%m%d"))[[1]]
    }
    floor(day / 365)
  })
  
  ### 중증도 계산 --------------------------------
  
  TRIS <- sapply(1:nrow(Pat), function(i) {
    Pat[i, ] %>%
      select(체온, 호흡곤란, 의식저하, 가벼운불안) %>%
      triage()
  })
  rownames(TRIS) <- c("체온지수", "심폐지수", "의식지수", "심리지수", "중증도")
  TRIS <- data.frame(t(TRIS), stringsAsFactors = FALSE)
  
  Pat <- Pat %>% cbind(나이 = Age)
  Pat <- Pat %>% cbind(TRIS)
  Pat$중증도 <- as.numeric(Pat$중증도)
  
  ## 날짜 decompose ---------------
  
  DATE <- sapply(1:nrow(Pat), function(i) {
    if (Pat$차수[i] == 1) {
      return(paste0(Pat$입력날짜[i], " 1차"))
    }
    return(paste0(Pat$입력날짜[i], " 2차"))
  }, USE.NAMES = FALSE)
  
  Pat <- Pat %>%
    cbind(DATE) %>%
    rename(날짜 = DATE)
  
  Pat$날짜 <- as.character(Pat$날짜)
  
  # 생년월일
  
  Birth <- sapply(Pat$주민등록번호, function(i) {
    (i - i %% 10000000) / 10000000
  })
  
  Sex <- sapply(Pat$주민등록번호, function(i) {
    i <- i %% 10000000
    i <- (i - i %% 1000000) / 1000000
    ifelse(i %% 2 == 1, "남", "여")
  })
  
  Pat <- Pat %>%
    cbind(Birth) %>%
    rename(생년월일 = Birth) %>%
    cbind(Sex) %>%
    rename(성별 = Sex)
  PatB <- data.frame(Pat)
  save(PatB, file = 'PatBackup.RData')
}

backupPat(auth, Link, Link2)
