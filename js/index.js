import * as Main from '../output/Main/index';
import * as Files from './files.js';

// console.log("Before");
// console.log(Files.default);
// Main.main();
// console.log("After");

export let step = Main.initialize(x => () => Files.default[x])();
for (var i = 0; i < 30000; ++i)
    step();
