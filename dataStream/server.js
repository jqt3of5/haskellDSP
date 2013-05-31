var net = require('net');



var server = net.createServer(function(con) {


    console.log("A client has connected");
    process.stdin.resume();
    process.stdin.on('data', function(chunk) {
	con.write(chunk);

    });

    process.stdin.on('end', function(chunk) {
	process.stdout.write("end");
	con.write("end");
    });
});

server.listen(1234, function() {});

