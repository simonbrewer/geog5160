## Script to crop and correct environmental data
myext = extent(c(-130,-100,30,50))

load("../Lab 8 SDMs/current.env.RData")
current.env = crop(current.env, myext)
current.env = stack(subset(current.env, seq(1,11)) / 10, subset(current.env, seq(12,19)))
current.env = readAll(current.env)
save(current.env, file = "current.env.RData")

load("../Lab 8 SDMs/future.env.RData")
future.env = crop(future.env, myext)
future.env = stack(subset(future.env, seq(1,11)) / 10, subset(future.env, seq(12,19)))
future.env = readAll(future.env)
save(future.env, file = "future.env.RData")
