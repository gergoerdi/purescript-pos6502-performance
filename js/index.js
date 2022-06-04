import * as Main from '../output/bundle.js';
import * as Files from './files.js';

let divLong = document.getElementById("longMessage");
function longMessage(lines)
{
    while (divLong.firstChild) {
        divLong.removeChild(divLong.lastChild);
    }

    
    lines.forEach(line => {
        let div1 = document.createElement("div");
        divLong.appendChild(div1);
        div1.appendChild(document.createTextNode(line))});
}

let divShort = document.getElementById("shortMessage");
function shortMessage(msg)
{
    divShort.textContent = msg;
}

let cmds = {};
for (var i = 7, cmd = document.getElementById("commands").firstChild; cmd; cmd = cmd.nextElementSibling)
{
    cmds[i] = cmd;
}

function run(step)
{
    while (true)
    {
        let io = step();
    
        console.log(io);

        if (io instanceof Main.LongMessage) {
            console.log(io.value0);
            longMessage(io.value0);
            step = io.value1;
        } else if (io instanceof Main.Menu) {
            for (const [cmd, btn] of Object.entries(cmds)) {
                btn.onclick = () => {
                    for (const [cmd, btn] of Object.entries(cmds))
                        btn.onclick = () => {};
                    run(io.value0(cmd));
                };
            }
            break;
        } else if (io instanceof Main.ShortMessage) {
            console.log(io.value0);
            shortMessage(io.value0);
            step = io.value1;
        } else {
            break;
        }
    }
}

var step = Main.initialize(x => () => Files.default[x]);
run(step);

