const g = async () => 1;
const h = async x => await x;
const i = async (a, b) => {
    await a;
};
