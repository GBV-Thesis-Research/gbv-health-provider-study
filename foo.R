data = data.table(data)
for (a in att.vars){
  c.val <- key[, get(a)]
  if (c.val==5){  #if c.val == 5, move scale down by 1 (0-4)
    data[, (a):=get(a)-1]
  } else if (c.val==1) { #otherwise, reverse code
    data[, (a):=get(a)+3]
    data[, (a):=abs(get(a)-8)]
  }
}