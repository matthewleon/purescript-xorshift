const child_process = require('child_process');

exports.scipyChisquareImpl = function (input) {
  return function () {
    const script = 'test/chisquare.py';
    const encoding = 'ascii';
    const opts = {encoding: encoding, input: input};
    const res = child_process.execFileSync('python3', [script], opts);
    console.log(res);
  };
};
