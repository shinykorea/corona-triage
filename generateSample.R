generate = function(day){
  # 성별
  S = rep(sample(c('M','F'),1),day)
    
  # 나이
  A = rep(sample(20:85, 1),day) 
  
  # 기저질병
  D = rep(sample(c(TRUE, FALSE), 1),day)
  
  ## 체온
  T = sample(c(0,1,2,3), day, replace = TRUE, prob = c(0.6,0.2,0.1,0.1)) 
  T = sapply(T, function(i){
    if(i==3){ # <= 35.0
      return(sample(seq(34,35,0.1), 1))
    }
    else if(i==2){
      high = sample(c(TRUE,FALSE),1)
      
      if(high){ # high, 39.1 >=
        return( sample(seq(39.1,39.5,0.1),1))
      }
      else{ # low, 35.1 ~ 36
        return( sample(seq(35.1,36,0.1), 1) )
      }
    } else if( i==1){ 
      return( sample(seq(38.1,39.1,0.1),1) )
    }
    return(sample(seq(36.1,38.1,0.1),1)) # 0
  })
  
  ## 심폐기능 
  CF = sample(c(0,1,2,3), day, replace = TRUE, prob = c(0.6,0.2,0.1,0.1)) 

  CF = sapply(CF, function(i){
    if(i==0){ # ALL Should 0
      return(c(0,0,0,0))
    }
    if(i==1){
      cnt = sample(1:4,1) # cnt
      return( sample(c(rep(1,cnt), rep(0,4-cnt))) )
    }
    if(i==2){
      cnt = sample(1:4,1) # 2 cnt
      return( sample( c(rep(2,cnt), rep(sample(c(0,1),size = 4-cnt, replace = TRUE))) ) )
    }
    if(i==3){
      cnt = sample(1:4,1) # 3 cnt
      return( sample( c(rep(3,cnt), rep(sample(c(0,1,2),size = 4-cnt, replace = TRUE))) ) )
    }
  })
    
  rownames(CF) = c('HB', 'O', 'BC', 'P')
  
  # 호흡곤란
  HB = sapply(CF['HB',], function(i){
    if(i <= 1 ){ # 0 or 1
      return(rep(FALSE,7))
    }
    if(i==2){
      return(c(TRUE, rep(FALSE,6)))
    }
    if(i==3){
      cnt = sample(1:6,1)
      return( c(sample(c(TRUE,FALSE),1), sample(c(rep(TRUE, cnt), rep(FALSE, 6-cnt))) ) )
    }
  })
  # 호흡곤란호소, 심한 호흡곤란, 가슴통증, 실신, 고열-몸살-기침, 파란입술, 호흡곤란 심화
  rownames(HB) = c('HB', 'DHB', 'CP', 'F', 'C', 'BL', 'HHB') 
  
  # 산소포화도
  O = sapply(CF['O',], function(i){
    if(i<=1){ # 95.1 ~ 100
      return(sample(seq(95.1,100,0.1),1))
    }
    if(i==2){ # 93.1 ~ 95
      return(sample(seq(93.1,95,0.1),1))
    }
    if(i==3){ # 92 ~ 93
      return(sample(seq(92,93,0.1),1))
    }
  })
    
  # 호흡수
  BC = sapply(CF['BC',], function(i){
    if(i<=1){
      return(sample(12:20,1))
    }
    if(i==2){
      high = sample(c(TRUE, FALSE), 1)
      if(high){
        return(sample(21:24,1))
      }
      return(sample(9:11,1))
    }
    if(i==3){ # >=25, <=8
      high = sample(c(TRUE, FALSE), 1)
      if(high){
        return(sample(25:30,1))
      }
      return(sample(5:8,1))
    }
  })
  
  # 맥박
  P = sapply(CF['P',], function(i){
    if(i <= 1){
      return(sample(51:100,1))
    }
    if(i==2){
      high = sample(c(TRUE, FALSE),1)
      if(high){
        return(sample(101:110,1))
      }
      return(sample(41:50,1))
    }
    if(i==3){
      high = sample(c(TRUE, FALSE),1)
      if(high){
        return(sample(111:130,1))
      }
      return(sample(30:40,1))
    }
    
  })
  
  P = sample(90:120, day, replace = TRUE) 
  
  ## 의식수준
  
  CO = sample(c(0,3), size = day, replace = TRUE, prob = c(0.95,0.05)) 
  CO = sapply(CO, function(i){
    if(i==3){
     return(FALSE) 
    }
    return(TRUE)
  })
  
  ## 심리상태
  M = sample(c(0,1,2,3), day, replace = TRUE, prob = c(0.6,0.2,0.1,0.1)) 
  
  # 두근거림, 발한, 몸떨림, 질식, 가슴불편, 복부불편, 어지러움, 감각이상, 두려움-공포
  
  M = sapply(M, function(i){
    if(i==0){ # 0 TRUE
      return(rep(FALSE, 9))
    }
    if(i==1){ # 1 ~ 3 TRUE
      cnt = sample(1:3,1)
    }
    else if(i>=2){
      cnt = sample(4:9,1)
    }
    return(sample( c(rep(TRUE, cnt), rep(FALSE, 9-cnt))) )
  })
  
  rownames(M) = c('PO', 'PE', 'TR', 'CH', 'CC', 'AC', 'W', 'SA', 'FE') 
  
  res = data.frame(S, A, D, T, t(HB), O, BC, P, CO, t(M))
  
  datatable(res, rownames = FALSE)
}
