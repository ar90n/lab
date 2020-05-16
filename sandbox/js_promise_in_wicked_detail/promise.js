// my work for http://www.mattgreer.org/articles/promises-in-wicked-detail/

function Promise(fn) {
  var state = 'pending';
  var value;
  var deferred = null;

  function resolve(newValue) {
    if(newValue && typeof newValue.then === 'function') {
      newValue.then(resolve);
      return;
    }

    value = newValue;
    state = 'resolved';

    if (deferred) {
      handle(deferred);
    }
  }

  function reject(reason) {
    state = 'rejected';
    value = reason;

    if(deferred) {
      handle(deferred);
    }
  }

  function handle(handler) {
    if (state === 'pending') {
      deferred = handler;
      return;
    }

    var handlerCallback;

    if(state === 'resolved') {
      handlerCallback = handler.onResolved;
    } else {
      handlerCallback = handler.onRejected;
    }

    if(!handlerCallback) {
      if(state === 'resolved') {
        handler.resolve(value);
      } else {
        handler.reject(value);
      }

      return;
    }

    var ret = handlerCallback(value);
    handler.resolve(ret);
  }

  this.then = function (onResolved, onRejected) {
    return new Promise(function (resolve, reject) {
      handle({
        onResolved: onResolved,
        onRejected: onRejected,
        resolve: resolve,
        reject: reject
      });
    });
  }

  fn(resolve, reject);
}

function somehowGetTheValue() {
  return {
    error: "error",
    value: 100
  };
}

function doSomething() {
  return new Promise(function (resolve, reject) {
    var result = somehowGetTheValue();
    if(result.error) {
      reject(result.error);
    } else {
      resolve(result.value);
    }
  })
}

doSomething().then(function (value) {console.log("ok:", value);},function(value) { console.log("error:", value);});
