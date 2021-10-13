a = c(1,3,5)
print(a)
b = c(3:14)
print(b)
ab = c(a,b)
print(ab)
ab[c(6:10)] = c(0, -6, -3, -1, -5)
print(ab)
wek1 = rep(c(1,2), 3)
print(wek1)
wek2 = rep(1:2, each=3)
print(wek2)
wek3 = seq(1,10, 2)
print(wek3)
wek4 = seq(101,110, 2)
print(wek4)
wek34 = wek3 - wek4
print(wek34)
wek43 = wek4 - wek3
print(wek43)
wek5 = wek3 + wek4
print(wek5)
wek6 = wek4/wek3
print(wek6)
wek7 = 5 * wek4 + 6 * wek3
print(wek7)
wek8 = log(wek4) + cos(wek3)
print(wek8)