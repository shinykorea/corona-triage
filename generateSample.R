generate = function(day){
  age = rep(sample(50:85, 1),day)
  disease = rep(sample(c(TRUE, FALSE), 1),day)
  temp = sample(seq(from = 35, to = 39.5, by = 0.1), day, replace = TRUE) 
  count = sample(7:13, day, replace = TRUE) 
  oxygen = sample(91:98, day, replace = TRUE)
  pressure = sample(90:120, day, replace = TRUE)
  breath = rep(sample(c(TRUE, FALSE), 1),day)
  datatable(data.frame(age, disease, temp, count, oxygen, pressure, breath), rownames = FALSE)
}

tab %>% filter(Date == max(Date)) %>% select(-City, -Town, -Occurrence, -Confirm)
