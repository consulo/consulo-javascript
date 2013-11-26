function f(a) {
  <spot>if</spot> (a == 0) {
    // First case
    return 1;
  } else if (a == 9) {
    // Second case with variable declaration
    var n = 8;
    return n;
  } else
    // Third case without braces
    return 2;
}