#Kruskal-wallis

crop.data = read.csv("C:\\Users\\jocel\\Downloads\\crop.data_.anova_\\crop.data.csv")

kruskal.wallis = kruskal.test(yield ~ fertilizer, data = crop.data)
summary(kruskal.wallis)