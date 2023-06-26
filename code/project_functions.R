## Dynamic model
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
      
      beta = parms$beta
      h = parms$h
      c = parms$c
      p = parms$p
      
      q_intra = parms$q_intra
      q_inter = parms$q_inter
      qik = diag(nN) * q_intra + (1 - diag(nN)) * q_inter
      
      time = y[1]
      R = y[3:(nR + 2)]
      N = y[(nR + 3):(nN + nR + 2)]
      
      R[R < 0] = 0
      N[N < 0] = 0
      N[alpha == 0] = 0
      
      rho0 = parms$rho0
      tau = parms$tau
      rho = rho0 * exp(-time/tau)
      
      lambda_0 = parms$lambda_0
      lambda = lambda_0 * as.numeric(exp(-qik %*% N))
      
      Upsilon = t(t(beta) * R / (1 + t(h * beta) * R))
      Gamma = Upsilon %*% (c * (1 - rowSums(p)))
      Omega = Upsilon %*% p
      
      dtimedt = 1
      dfdt = rowSums(lambda * Upsilon * N)
      dFdt = sum(dfdt)
      dNdt = alpha + Gamma * N - mu * N
      dRdt = rho + colSums(Omega * N) - colSums(Upsilon * N)
   
   return(list(c(dtimedt, dFdt, dRdt, dNdt, dfdt)))
}


## Define parameters based on simulation scenario
parameters = 
   function(
      n,
      niches,
      coregulation,
      byproduction,
      handling_time,
      quality,
      ammonia_capacity,
      seed
   ){
      
      nN = num_species
      nR = num_resources
      
      ## Immigration rates
      alpha = rep(0, num_species)
      if(n == num_species){
        consumer_index = seq(num_species)
      } else if(n == (num_species - 1)){
        consumer_index = seq(num_species)[-seed]
      } else if(n == 1){
        consumer_index = seed
      } else if(n > 1){
        set.seed(seed)
        consumer_index = sort(sample(num_species, size = n))
      } 
      alpha[consumer_index] = alpha0
      
      ## Niche scenario
      if(niches == 'Generalists'){
        set.seed(seed)
        beta = beta_max * matrix(runif(nR * nN), nN, nR)
      } else if(niches == 'Specialists'){
        beta = nR * beta_max * diag(nR)
      } else if(niches == 'Gradient'){
        theta_ji = diag(nR) + upper.tri(matrix(NA, nR, nR))
        beta = beta_max * nR / (nR - seq(0, nR - 1)) * theta_ji
      }
      
      ## Co-regulation scenario
      if(coregulation == 'Absent'){
        q_intra = q_inter = 0
      } else if(coregulation == 'Neutral'){
        q_intra = q_inter = qmax
      } else if(coregulation == 'Intraspecific'){
        q_intra = qmax
        q_inter = qmin
      } else if(coregulation == 'Interspecific'){
        q_intra = qmin
        q_inter = qmax
      }
      
      ## By-production architecture
      if(byproduction == 'Serial'){
         p = matrix(0, nN, nR)
         for(i in seq(nN)) 
           for(j in seq(nR)) 
             if(j == (i + 1)) p[i, j] = p0
      } else if(byproduction == 'Nested'){
         p = upper.tri(matrix(0, nN, nR))
         p = p0 * p / (1e-16 + rowSums(p))
      } else if(byproduction == 'None'){
         p = matrix(0, nN, nR)
      }
      
      ## Resource quality scenario
      if(quality == 'Flat'){
         c = rep(c_min, length = nR)
      } else if(quality == 'Increase'){
         c = exp(seq(0, 5, length = nR))
         slope = (c_max - c_min) / (max(c) - min(c))
         c = c_max - slope * max(c) + slope * c
      } 
      
      ## Handling time scenarios
      if(handling_time == 'Decrease'){
         h = exp(seq(logh_max, logh_min, length = nN))
      } else if(handling_time == 'Increase'){
         h = exp(seq(logh_min, logh_max, length = nN))
      } else if(handling_time == 'Flat'){
         h = mean(exp(c(logh_max, logh_min)))
      }
      
      ## Ammonia capacity down the species chain
      if(ammonia_capacity == 'Flat'){
        lambda_0 = rep(mean(c(lambda_min, lambda_max)), nN)
      } else if(ammonia_capacity == 'Increase'){
        lambda_0 = seq(lambda_min, lambda_max, length = nN)
      } else if(ammonia_capacity == 'Decrease'){
        lambda_0 = seq(lambda_max, lambda_min, length = nN)
      }
      
      params = 
         list(
            nN = nN,
            nR = nR,
            alpha = alpha,
            beta = beta,
            mu = mu,
            rho0 = rho0,
            tau = tau,
            lambda_0 = lambda_0,
            q_intra = q_intra,
            q_inter = q_inter,
            c = c,
            h = h,
            p = p,
            seed = seed,
            consumer_index = consumer_index
         )
      
      return(params)
   }


## Run simulation
simulation = 
   function(
      scenario,
      n,
      niches,
      coregulation,
      byproduction,
      handling_time,
      quality,
      ammonia_capacity,
      seed,
      t_min = 1,
      t_max = 1000,
      timeseq_length = 1e3,
      full.data = TRUE,
      save.file = FALSE,
      path = NULL
   ){
      
      params = 
         parameters(
            n = n,
            niches = niches,
            coregulation = coregulation,
            byproduction = byproduction,
            handling_time = handling_time,
            quality = quality,
            ammonia_capacity = ammonia_capacity,
            seed = seed
         )
      
      nR = params$nR
      nN = params$nN
      R0 = rep(0, nR)
      N0 = rep(0, nN)
      abun = c(t_min, 0, R0, N0, rep(0, nN))
      
      time_sequence = round(seq(t_min, t_max, length = timeseq_length))
      
      sim = 
        ode(
          y = abun, 
          times = time_sequence, 
          func = pitcher, 
          parms = params
        )
      
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
            scenario = scenario,
            n = n,
            niches = niches,
            coregulation = coregulation,
            byproduction = byproduction,
            handling_time = handling_time,
            quality = quality,
            ammonia_capacity = ammonia_capacity,
            seed = seed
         ) 
      
      data = 
         parms_tbl |>
         bind_cols(
            data_wide |>
               pivot_longer(-time, names_to = 'species')
         ) |>
        filter(
          species %in% 
            c(
              'function', 
              paste0('R', seq(nR)), 
              paste0('N', params$consumer_index),
              paste0('F', params$consumer_index)
            )
        )
      
      
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
