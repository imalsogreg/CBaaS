var w = undefined;

function reverse(x){
    var r = '';
    var l = x.length;
    for (var i = 0; i < l; i++ ){
        r = x[i] + r;
    }
    return r;
};

function wswork() {
    w = new WebSocket('ws://localhost:9160/worker?name=test&function=reverse&tags=testing&tags=not+much');
    w.onmessage = function(m) {
        console.log('Got message: ' + m.data);
        var req = JSON.parse(m.data);
        console.log(req);
        var jobid     = req.contents[0];
        var browserid = req.contents[1];
        var cbaas_arg = req.contents[2].arg;
        if (!(cbaas_arg.tag == 'VText')){
            throw new Error('testworker called with not-text argument');
        }
        var res = {'tag':'VText', 'contents': reverse(cbaas_arg.contents)};
        var resMsg = {tag:'WorkerFinished', contents:[jobid,browserid,{job:jobid, value:res}]};
        w.send(JSON.stringify(resMsg));
        console.log('Sent response: ');
        console.log(resMsg);
    };
};
