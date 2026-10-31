class C {
    async m() {
        await this.n();
    }
    async *gen() {
    }
    static async s() {
    }
}
const o = {
    async m() {
        await 1;
    }
};
