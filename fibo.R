while(T) {
  num = as.integer( readline(prompt = "Input the number: ") )
  
  if (num <= 0) {
    print("Thanks a lot!!")
    break
  } else if (num == 1) {
    print(0)
    next
  }
  
  p0 = 0
  p1 = 1
  ret = paste(p0, p1)   # if num == 2  ==> 0 1 출력!!

  while(num > 2) {
    p = p0 + p1
    p0 = p1
    p1 = p
    ret = paste(ret, p)
    
    num = num - 1
  }
  
  print(ret)
}