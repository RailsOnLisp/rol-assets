var path = require('path');
var rw = require('rw');
var less = require('less');

var print_error = function (e) {
  less.writeError(e);
  process.exit(2);
};

try {
  var opt = JSON.parse(rw.readFileSync('/dev/stdin', 'utf8'));

  var parse_data = function (e, data) {
    if (e)
      print_error(e);
    else {
      less.render(data, opt.parser, function (e, output) {
	console.log(output);    
	rw.writeFileSync('/dev/stdout', output.css, 'utf8');
      });
    }
  };

  rw.readFile(path.resolve(process.cwd(), opt.src), 'utf8', parse_data);
} catch (e) {
  print_error(e);
}
