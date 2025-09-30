multivariateProcessStability <- function(
    data, 
    lambda = 0.2,        # EWMA smoothing parameter in (0,1]
    alpha  = 0.0027,     # false alarm rate (≈3-sigma rule by default)
    steady_state = FALSE,# if TRUE, use steady-state Sigma_z for all t
    ridge = 1e-8,        # small diagonal ridge if covariance ill-conditioned
    make_plot = TRUE
) {
  # Ensure matrix
  X <- as.matrix(data)
  n <- nrow(X); p <- ncol(X)
  if (n < 2) stop("Need at least 2 observations.")
  if (lambda <= 0 || lambda > 1) stop("lambda must be in (0,1].")
  
  # Phase I center and covariance
  mu_hat <- colMeans(X)
  S_hat  <- stats::cov(X)
  # Regularize if needed
  S_hat  <- S_hat + diag(ridge, p)
  
  # Centered observations
  Y <- sweep(X, 2, mu_hat, FUN = "-")
  
  # EWMA recursion
  Z <- matrix(0, nrow = n, ncol = p)
  z_prev <- rep(0, p)
  one_m_l <- (1 - lambda)
  
  for (t in 1:n) {
    z_t <- lambda * Y[t, ] + one_m_l * z_prev
    Z[t, ] <- z_t
    z_prev <- z_t
  }
  
  # Sigma_z,t and Q_t computation
  # Sigma_z,steady = (lambda / (2 - lambda)) * S
  # Sigma_z,t = Sigma_z,steady * (1 - (1 - lambda)^(2t))
  Sigma_z_steady <- (lambda / (2 - lambda)) * S_hat
  
  Qt <- numeric(n)
  for (t in 1:n) {
    if (steady_state) {
      Sigma_z_t <- Sigma_z_steady
    } else {
      scale_t <- 1 - (one_m_l)^(2 * t)
      Sigma_z_t <- Sigma_z_steady * scale_t
    }
    # Inverse via Cholesky for stability
    inv_Sig <- tryCatch(
      chol2inv(chol(Sigma_z_t)),
      error = function(e) {
        # Fallback: add a tad more ridge if not PD
        chol2inv(chol(Sigma_z_t + diag(ridge * 10, p)))
      }
    )
    z_t <- Z[t, ]
    Qt[t] <- as.numeric(t(z_t) %*% inv_Sig %*% z_t)
  }
  
  # Control limit (Chi-square with p df)
  UCL <- stats::qchisq(1 - alpha, df = p)
  
  # Out-of-control points
  ooc_idx <- which(Qt > UCL)
  
  # Plot
  if (make_plot) {
    y_max <- max(Qt, UCL) * 1.1
    plot(Qt, type = "b", pch = 19,
         xlab = "Observation", ylab = expression(Q[t]),
         ylim = c(0, y_max))
    abline(h = UCL, col = "red", lty = 2)
    legend("topright", legend = c("UCL"), col = "red", lty = 2, bty = "n")
    if (length(ooc_idx)) {
      points(ooc_idx, Qt[ooc_idx], col = "blue", pch = 4, cex = 1.5)
    }
  }
  
  # Return a small results bundle (matches your previous behavior + extras)
  list(
    Q = Qt,
    UCL = UCL,
    out_of_control_indices = ooc_idx,
    num_out_of_control = length(ooc_idx),
    lambda = lambda,
    alpha = alpha,
    steady_state = steady_state
  )
}

