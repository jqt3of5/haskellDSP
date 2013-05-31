var net = require('net');

var client = net.connect({port: 1234, host: 'localhost'}, 
			 function() {
			     console.log("The client has connected");

			 });

client.on('data', function(data) {
    console.log(data.toString());
});


