import * as Main from '../output/bundle.js';
import * as Files from './files.js';

let before = +new Date();
Main.initialize(x => () => Files.default[x])();
let after = +new Date();
console.log("done in " + (after - before) + "ms");
