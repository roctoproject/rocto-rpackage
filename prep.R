# Specs contains:
# iterId
# outputPath
# data
specs <- getSpecs()

# Simple simulation

#params

grid <- expand.grid(beta = betas, r = r)

params <- grid[specs$iterId,]

#script

save(do.call(main, params), outputPath)


