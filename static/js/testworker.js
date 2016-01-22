function reverse(x){
    var r = '';
    var l = x.length;
    for (var i = 0; i < l; i++ ){
        r = x[i] + r;
    }
    return r;
};

function wswork() {
    var w = new WebSocket('ws://localhost:9160/worker?name=test&function=reverse&tags=testing&tags=not+much');
    w.onmessage = function(m) {
        console.log('Got message: ' + m.data);
        w.send(reverse(m.data));
        console.log('Sent response');
    };
};
