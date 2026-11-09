function declarations(p, { q }) {
    var x = p;
    let y;
    const { e1, e3 } = q;
    x = y = e1;
    x += e3;
    for (var k in q) {
        y = k;
    }
    return x;
}
