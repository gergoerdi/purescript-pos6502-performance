import * as PureScript from '../output/bundle.js';
import * as JavaScript from './jos6502.js';
import * as Files from './files.js';

{
    let before = +new Date();
    let cnt = PureScript.initialize(x => () => Files.default[x].slice(0,0x10000))();
    let after = +new Date();
    console.log("PureScript: " + cnt + " cycles done in " + (after - before) + "ms");
}

{
    let before = +new Date();
    let cnt = JavaScript.initialize(x => () => Files.default[x].slice(0,0x10000))();
    let after = +new Date();
    console.log("JavaScript: " + cnt + " cycles done in " + (after - before) + "ms");
}
