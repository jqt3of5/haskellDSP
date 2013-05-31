var net = require('net');
var spawn = require('child_process').spawn;
var readline = require('readline');


var capture = spawn('../hwi/capture');
var rlInput = readline.createInterface({
    input: capture.stdout, 
    output: capture.stdin
});

var server = net.createServer(function(con) {
    console.log("A client has connected");

    rlInput.on('line', function(line) {
	con.write(line);
    });
    // capture.stdout.on('data', function (data) {
    // 	con.write(data);
    // });

    // process.stdin.resume();
    // process.stdin.on('data', function(chunk) {
    // 	con.write(chunk);

    // });

    // process.stdin.on('end', function(chunk) {
    // 	process.stdout.write("end");
    // 	con.write("end");
    // });
});

server.listen(1234, function() {});

