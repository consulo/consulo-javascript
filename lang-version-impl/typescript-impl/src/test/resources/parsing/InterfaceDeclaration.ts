interface User {
    name: string;
    age: number;
    email?: string;
}

interface Repository<T> extends Iterable<T> {
    findById(id: string): T;
}
