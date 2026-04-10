type Result<T, E> = { ok: true; value: T } | { ok: false; error: E };
function identity<T>(arg: T): T {
    return arg;
}
let list: Array<string> = [];
