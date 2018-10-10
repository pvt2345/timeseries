attach(sleep)
extra.1 = extra[group==1]
extra.2 = extra[group==2]
plot(extra~group, data=sleep, main='Extra Sleep')
#10 same people testing on 2 drugs -> paired samples
t.test(extra.1, extra.2, paired=TRUE, alternative="two.sided") 