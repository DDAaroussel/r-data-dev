var contents = [];
for (...) {
  for (...) {
    contents.push(arr[i][j]);
  }
}
contents.reduce(...); 


// OR


var result = arr[0][0];
for (...) {
  for(...) {
    if (arr[i][j] > result) {
      result = arr[i][j];
    }
  }
}
return result;


/* You would need to do something like Math.max(arr[i][0], arr[i][1], ..., arr[i][n]);
but that's a problem if you don't know the length of the subarray beforehand
which is why you want to just call reduce on the subarray
you can't put an array into Math.max */


//For ES6 onwards
return Math.max(...arr.map(a => Math.max(...a)));

