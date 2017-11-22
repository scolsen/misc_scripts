/**
 * Created by scolsen on 6/2/2017.
 * Fiat.js - a collection of functions for building Functional CLIs
 */

function unit(){
    return ()=>{return null};
}

function singleton(item){
    return ()=>{return item};
}

function arg_minimum(scope, minimum){
    return scope.length >= minimum;
}

function pattern_match(){
    [...arguments].forEach(x=>{
        if(x) return x;
    });
}

function tuple(a, b){
    if(!arg_minimum([...arguments], 2)) return tuple(a, unit());
    return singleton({
            1: singleton(a),
            2: singleton(b)
           });
}

function is_function(fn){
    return typeof fn === 'function';
}

/**
 * 'Unwraps' the values contained in nested lambda/anonymous functions.
 * Returns the first non-function value it hits.
 * if the result of executing the function is another function nest another unlambda.
 * @param fn
 * @returns {*}
 */
function unlambda(fn){
    if(is_function(fn()))return unlambda(fn());
    return fn();
}

function curry(f, g){
    if(is_function(g)){return singleton(f(unlambda(g)))}
    return singleton(f(g));
}

/**
 * Should create a functional pattern like the following:
 * fn(fn(fn(x)y)z)
 * use: expand(tuple, 1, 2);
 * @param fn
 * @returns {*}
 */
function expand(fn){
    let args = [...arguments];
    let func = unlambda(args[0]);
    for(k=1; k<args.length; k++){
        func = unlambda(func[args[k]]);
    }
    return func;
}

function linearCurry(fn){
    let func;
    let args = [...arguments];
    if (typeof(args[0])=== 'object'){ //Account for multiple arguments
       func = args[1](...args[0]);
    } else {
       func = curry(args[1], args[0]);
    }
    for(k=2; k<args.length; k++){
        func = curry(args[k], func);
    }
    return func;
}

//Auto populate number of 2s needed to uncast the target value.
//e.g. a triple of (1,2,3) should return 1 when we run
//uncase(triple, 3);
function uncase(fn, n){
    let args = [fn];
    for (k= n - 1; k > 0; k--){
        args.push(2);
    }
    args.push(1);
    return expand(...args);
}

function xle(){
    let args  = [...arguments];
    let func = unit(); //we define a temporary function to recursively build up our chain.
    for (k=0; k<args.length; k++){
        func = tuple(args[k], func);
    }
    return func;
}


exports.unit = unit;
exports.singleton = singleton;
exports.tuple = tuple;
exports.curry = curry;
exports.unlambda = unlambda;
exports.xle = xle;
exports.expand = expand;
exports.uncase = uncase;
exports.linearCurry = linearCurry;
