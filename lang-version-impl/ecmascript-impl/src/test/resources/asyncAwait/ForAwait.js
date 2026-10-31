async function f(stream) {
    for await (const chunk of stream) {
        console.log(chunk);
    }
}
