async function load(url) {
    const response = await fetch(url);
    return await response.json();
}
const f = async function () {
    await load('x');
};
export async function exported() {
}
export default async function () {
}
