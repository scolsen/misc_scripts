function maybe(x){
    if([...arguments].length >= 1) return x;
  return null;
}

function _(x){
	return maybe(x);
}

function bottom(arguments){
	return [...arguments].includes(_);
}

function test(x, y, z){
	if(bottom(arguments)) return arguments;
	console.log(x, y, z);
}

test('a', 'y', 'b');
test('a', _, 'b');

function array_from_arguments(arguments){
	let args = [...arguments];
  args.reverse();
  args.pop();
  return args;
}

function curry(fn, args){
	let argus = array_from_arguments(arguments);
  let narg = Array.from(fn).map((x, index)=>{
  	if(x === _ && argus.length >= 1) {
    	let r = x(argus[argus.length - 1]);
      argus.pop();
      return r;
    }
    return x;
  });
  return fn.callee(...narg);
}

function rl_curry(fn, args){
	let argus = array_from_arguments(arguments);
  argus.reverse();
  let narg = Array.from(fn).map((x, index)=>{
  	if(x === _ && argus.length >= 1) {
    	let r = x(argus[argus.length - 1]);
      argus.pop();
      return r;
    }
    return x;
  });
  return fn.callee(...narg);
}

curry(test('a', _, _), 'q', 'f');
rl_curry(test('a', _, _), 'q', 'f');
curry(curry(test('a', _, _), 'l'), 'k');
let partial = test('a', _, _);
curry(curry(partial, 'q'), ',');

