double FOI_part(double I, double N, double beta, double R, double fc, double fP, double n, double eP, double ec) {
  
  return I * (1 -
              pow(1 -                       beta * R, n * (1 - fc) * (1 - fP)) *
              pow(1 - (1 - ec) *            beta * R, n *      fc  * (1 - fP)) *
              pow(1 -            (1 - eP) * beta * R, n * (1 - fc) *      fP ) *
              pow(1 - (1 - ec) * (1 - eP) * beta * R, n *      fc  *      fP ));
}

double all_FOI_together(double c, double p, double S0, double S1a, double S1b, double S1c, double I01, double I11, double I02, double I03, double I04, double I05, double I22, double I23, double I24, double I25, double I32, double I33, double I34, double I35, double I42, double I43, double I44, double I45, double N, double beta, double R, double fc, double fP, double n, double eP, double ec) {
  if (c == 0 || p == 0) {
    return 0.0;
  } else {
    return 1 / N * c * p * (
        FOI_part(I01, N, beta, R, fc, fP, n, eP, ec) +
          FOI_part(I02, N, beta, R, fc, fP, n, eP, ec) +
          FOI_part(I03, N, beta, R, fc, fP, n, eP, ec) +
          FOI_part(I04, N, beta, R, fc, fP, n, eP, ec) +
          FOI_part(I05, N, beta, R, fc, fP, n, eP, ec) +
          
          FOI_part(I11, N, beta, R, fc, fP, n, eP, ec) +
          
          FOI_part(I22, N, beta, R, fc, fP, n, eP, ec) +
          FOI_part(I23, N, beta, R, fc, fP, n, eP, ec) +
          FOI_part(I24, N, beta, R, fc, fP, n, eP, ec) +
          FOI_part(I25, N, beta, R, fc, fP, n, eP, ec) +
          
          FOI_part(I32, N, beta, R, fc, fP, n, eP, ec) +
          FOI_part(I33, N, beta, R, fc, fP, n, eP, ec) +
          FOI_part(I34, N, beta, R, fc, fP, n, eP, ec) +
          FOI_part(I35, N, beta, R, fc, fP, n, eP, ec) +
          
          FOI_part(I42, N, beta, R, fc, fP, n, eP, ec) +
          FOI_part(I43, N, beta, R, fc, fP, n, eP, ec) +
          FOI_part(I44, N, beta, R, fc, fP, n, eP, ec) +
          FOI_part(I45, N, beta, R, fc, fP, n, eP, ec));
  }
}

double compute_lambda(double c_comm, double p_comm, double S0, double S1a, double S1b, double S1c, double I01, double I11, double I02, double I03, double I04, double I05, double I22, double I23, double I24, double I25, double I32, double I33, double I34, double I35, double I42, double I43, double I44, double I45, double N, double beta, double R, double fc_comm, double fP_comm, double n_comm, double eP, double ec, double fc_noncomm, double fP_noncomm, double n_noncomm, double c_noncomm, double p_noncomm) {
  return all_FOI_together(c_comm, p_comm, S0, S1a, S1b, S1c, I01, I11, I02, I03, I04, I05, I22, I23, I24, I25, I32, I33, I34, I35, I42, I43, I44, I45, N, beta, R, fc_comm, fP_comm, n_comm, eP, ec) + all_FOI_together(c_noncomm, p_noncomm, S0, S1a, S1b, S1c, I01, I11, I02, I03, I04, I05, I22, I23, I24, I25, I32, I33, I34, I35, I42, I43, I44, I45, N, beta, R, fc_noncomm, fP_noncomm, n_noncomm, eP, ec);
}