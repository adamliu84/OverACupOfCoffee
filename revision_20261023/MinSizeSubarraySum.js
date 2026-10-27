// https://leetcode.com/problems/minimum-size-subarray-sum/description/

var minSubArrayLen = function (target, nums) {
    let leftptr = 0, rightptr = 0;
    let curSum = 0;
    let minSize = 0;

    for (rightptr; rightptr < nums.length; rightptr++) {
        curSum += nums[rightptr];
        while (curSum >= target) {
            minSize = minSize === 0 ? (rightptr - leftptr + 1) : Math.min(minSize, (rightptr - leftptr + 1));
            curSum -= nums[leftptr];
            leftptr++
        }
    }

    return minSize
};


console.log(minSubArrayLen(7, [2, 3, 1, 2, 4, 3]));
console.log(minSubArrayLen(4, [1, 4, 4]));
console.log(minSubArrayLen(11, [1, 1, 1, 1, 1, 1, 1, 1]));
console.log(minSubArrayLen(51, [1, 4, 45, 6, 0, 19]));
console.log(minSubArrayLen(100, [1, 10, 5, 2, 7]));