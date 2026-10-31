@Component({ selector: 'app-root', templateUrl: './app.component.html' })
export class AppComponent {
    title = 'x';
}

@Injectable()
export default class Service {
}

export @Pipe({ name: 'p' }) class P {
}
