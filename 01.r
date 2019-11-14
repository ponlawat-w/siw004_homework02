# Import data
data <- source('./01-foodstuffs.dat')$value

attach(data)

pairs(data)
