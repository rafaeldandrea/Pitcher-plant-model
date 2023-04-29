## Model functions

pitcher = 
   function(
      t, 
      y, 
      parms
   ){
      nR = parms$nR
      nN = parms$nN
      
      alpha = parms$alpha
      mu = parms$mu
      rho = parms$rho
      
      beta = parms$beta
      h = parms$h
      lambda_max = parms$lambda
      q_intra = parms$q_intra
      q_inter = parms$q_inter
      gamma = parms$gamma
      p = parms$p
      
      qik = diag(nN) * q_intra + (1 - diag(nN)) * q_inter
      
      c = gamma
      
      time = y[1]
      R = y[3:(nR + 2)]
      N = y[(nR + 3):(nN + nR + 2)]
      
      R[R < 0] = 0
      N[N < 0] = 0
      N[alpha == 0] = 0
      
      lambda = lambda_max * as.numeric(exp(-qik %*% N))
      
      Upsilon = t(t(beta) * R / (1 + t(h * beta) * R))
      Gamma = Upsilon %*% (c * (1 - rowSums(p)))
      Omega = Upsilon %*% p
      
      dtimedt = 1
      dfdt = rowSums(lambda * Upsilon * N)
      dFdt = sum(dfdt)
      dNdt = alpha + Gamma * N - mu * N
      dRdt = rho * exp(-time) + colSums(Omega * N) - colSums(Upsilon * N)
   
   return(list(c(dtimedt, dFdt, dRdt, dNdt, dfdt)))
}

parameters = 
   function(
      n,
      sampling,
      resource_supply,
      ammonia,
      density_dependence,
      production,
      recalcitrance,
      handling_time,
      generalism,
      uptake_factor,
      efficiency,
      seed,
      maxtime
   ){
      
      nN = nR = chain_length
      
      if(production == 'serial'){
         p = matrix(0, nN, nR)
         for(i in seq(nN)) for(j in seq(nR)) if(j == (i + 1)) p[i, j] = p0
      } else if(production == 'parallel'){
         p = upper.tri(matrix(0, nN, nR))
         p = p0 * p / (1e-16 + rowSums(p))
      } else if(production == 'off'){
         p = matrix(0, nN, nR)
      }
      
      if(generalism == 'hierarchy'){
         beta = outer(seq(nN), seq(nR), function(i, j) exp(-abs(i - j)))
         beta = beta0 * (diag(nR) + beta * lower.tri(beta)) / rowSums(beta) * seq(nN) / nN
      } else if(generalism == 'random'){
         set.seed(seed)
         beta = beta0 * matrix(runif(nR * nN), nN, nR)
      } else if(generalism == 'gencomp'){
         v = exp(- .1 * seq(nN))
         v = v / mean(v) * beta0
         beta = v * outer(seq(nR), seq(nN), \(i, j) i >= j)
      } else if(generalism == 'generalists'){
         d = outer(seq(nN), seq(nR), \(i, j) i - j)
         v = exp(.1 * seq(nN))
         v = v / mean(v) * beta0 
         beta = v * exp(-(d / 2) ^ 2) / sum(exp(-(d / 2) ^ 2))
      } else if(generalism == 'specialists'){
         beta = nR * beta0 * diag(nR)
      } else if(generalism == 'tradeoff'){
         theta_ji = diag(nR) + upper.tri(matrix(NA, nR, nR))
         beta = beta0 * uptake_factor ^ seq(0, nR - 1) * nR / (nR - seq(0, nR - 1)) * theta_ji
      } else if(generalism == 'tradeoff_additive'){
         theta_ji = diag(nR) + upper.tri(matrix(NA, nR, nR))
         beta = (uptake_factor * seq(0, nR - 1) + nR * beta0) / (nR - seq(0, nR - 1)) * theta_ji
      }
      
      if(ammonia == 'flat'){
         lambda = matrix(mean(c(lambda_min, lambda_max)), nrow = nN, ncol = nR, byrow = recalcitrance)
      } else if(ammonia == 'increase'){
         lambda = matrix(seq(lambda_min, lambda_max, length = nN), nrow = nN, ncol = nR, byrow = recalcitrance)
      } else if(ammonia == 'decrease'){
         lambda = matrix(seq(lambda_max, lambda_min, length = nN), nrow = nN, ncol = nR, byrow = recalcitrance)
      } else if(ammonia == 'exponential'){
         lambda = matrix(exp(seq(log(lambda_max), log(lambda_min), length = nN)), nrow = nN, ncol = nR, byrow = recalcitrance)
      } 
      
      if(density_dependence == 'none'){
         q_intra = q_inter = 0
      } else if(density_dependence == 'neutral'){
         q_intra = q_inter = .1
      } else if(density_dependence == 'intra'){
         q_intra = .1
         q_inter = .05
      } else if(density_dependence == 'inter'){
         q_intra = .05
         q_inter = .1
      }
      
      if(efficiency == 'flat'){
         gamma = rep(gamma_min, length = nR)
      } else if(efficiency == 'increase'){
         gamma = exp(seq(0, 5, length = nR))
         gmax = gamma_max
         gmin = gamma_min
         slope = (gmax - gmin) / (max(gamma) - min(gamma))
         gamma = gmax - slope * max(gamma) + slope * gamma
      } 
      
      alpha = rep(0, chain_length)
      if(n == chain_length){
         consumer_index = seq(chain_length)
      }else if(n == (chain_length - 1)){
         consumer_index = seq(chain_length)[-seed]
      } else if(n > 1 & sampling == 'random'){
         set.seed(seed)
         consumer_index = sort(sample(chain_length, size = n))
      } else if(n > 1 & sampling == 'ordered'){
         consumer_index = seq(n)
      } else if(n == 1){
         consumer_index = seed
      }
      alpha[consumer_index] = alpha0
      
      if(resource_supply == 'ordered'){
         rho = c(rho0, rep(0, nR - 1))
      } else if(resource_supply == 'uniform'){
         rho = rep(rho0, nR)
      }
      
      if(handling_time == 'decrease'){
         h = exp(seq(hmax, hmin, length = totalspecies))
      } else if(handling_time == 'increase'){
         h = exp(seq(hmin, hmax, length = totalspecies))
      } else if(handling_time == 'flat'){
         h = mean(exp(c(hmax, hmin)))
      }
      
      params = 
         list(
            nN = nN,
            nR = nR,
            sampling = sampling,
            alpha = alpha,
            beta = beta,
            mu = rep(mu0, nN),
            rho = rho,
            lambda = lambda,
            q_intra = q_intra,
            q_inter = q_inter,
            gamma = gamma,
            h = h,
            p = p,
            maxtime = maxtime,
            seed = seed,
            consumer_index = consumer_index,
            resource_supply = resource_supply
         )
      
      return(params)
   }


simulation = 
   function(
      scenario,
      n,
      sampling,
      resource_supply,
      ammonia,
      density_dependence,
      production,
      recalcitrance,
      handling_time,
      generalism,
      uptake_factor,
      efficiency,
      seed,
      mintime = 1,
      maxtime,
      timeseq_length = 1e3,
      full.data = TRUE,
      save.file = FALSE,
      path = NULL,
      model = 'pitcher',
      keep.unsampled.species = FALSE
   ){
      
      params = 
         parameters(
            n,
            sampling,
            resource_supply,
            ammonia,
            density_dependence,
            production,
            recalcitrance,
            handling_time,
            generalism,
            uptake_factor,
            efficiency,
            seed,
            maxtime
         )
      
      nR = params$nR
      nN = params$nN
      R0 = rep(0, nR)
      N0 = rep(0, nN)
      abun = c(mintime, 0, R0, N0, rep(0, nN))
      
      time_sequence = round(seq(mintime, maxtime, length = timeseq_length))
      
      if(model == 'pitcher'){
         sim = 
            ode(
               y = abun, 
               times = time_sequence, 
               func = pitcher, 
               parms = params
            )
      }
      
      data_wide = 
         data.frame(sim) |>
         as_tibble() |>
         select(-X1) 
      
      names(data_wide) = 
         c(
            'time', 
            'function', 
            paste0('R', seq(nR)), 
            paste0('N', seq(nN)),
            paste0('F', seq(nN))
         )
      
      parms_tbl = 
         tibble(
            scenario,
            n,
            sampling,
            resource_supply,
            ammonia,
            density_dependence,
            production,
            recalcitrance,
            handling_time,
            generalism,
            uptake_factor,
            efficiency,
            seed,
            maxtime
         ) 
      
      data = 
         parms_tbl |>
         bind_cols(
            data_wide |>
               pivot_longer(-time, names_to = 'species')
         ) 
      
      if(!keep.unsampled.species){
         data =
            data |>
            filter(
               species %in% 
                  c(
                     'function', 
                     paste0('R', seq(nR)), 
                     paste0('N', params$consumer_index),
                     paste0('F', params$consumer_index)
                  )
            )
      }
      
      if(save.file){
         save_name = 
            paste0(
               path, 
               '/scenario-', 
               scenario, 
               '_seed-', 
               seed, 
               '.rds'
            )
         
         saveRDS(data, save_name)
      }
      
      if(full.data) return(data)
      
      if(!full.data){
         parms_tbl |>
            bind_cols(
               data_wide |>
                  slice_max(time) |>
                  select('time', 'function')
            )
      }
   }

