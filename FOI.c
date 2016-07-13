double FOI_part(double I, double N, double beta, double R, double fc, double fP, double n, double eP, double ec) {
  
  return I/N * (1 - 
                pow(1 -                       beta * R, n * (1 - fc) * (1 - fP)) *
                pow(1 - (1 - ec) *            beta * R, n *      fc  * (1 - fP)) *
                pow(1 -            (1 - eP) * beta * R, n * (1 - fc) *      fP ) *
                pow(1 - (1 - ec) * (1 - eP) * beta * R, n *      fc  *      fP ));
}