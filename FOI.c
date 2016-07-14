double FOI_part(double I, double N, double beta, double R, double fc, double fP, double n, double eP, double ec) {
  
  return I/N * (1 - 
                pow(1 -                       beta * R, n * (1 - fc) * (1 - fP)) *
                pow(1 - (1 - ec) *            beta * R, n *      fc  * (1 - fP)) *
                pow(1 -            (1 - eP) * beta * R, n * (1 - fc) *      fP ) *
                pow(1 - (1 - ec) * (1 - eP) * beta * R, n *      fc  *      fP ));
}

double compute_lambda(double c, double S0, double S1a, double S1b, double S1c, double I01, double I11, double I02, double I03, double I04, double I05, double I22, double I23, double I24, double I25, double I32, double I33, double I34, double I35, double I42, double I43, double I44, double I45, double N, double beta, double R, double fc, double fP, double n, double eP, double ec) {
  return c * (FOI_part(I01, N, beta, R, fc, fP, n, eP, ec) +
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
