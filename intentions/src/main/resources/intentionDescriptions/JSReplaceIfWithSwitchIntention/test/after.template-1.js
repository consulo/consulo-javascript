function f(a) {
  switch (a) {
    case 0:
      // First case
      return 1;
    case 9:
      // Second case with variable declaration
      var n = 8;
      return n;
    default:
      return 2;
  }
}