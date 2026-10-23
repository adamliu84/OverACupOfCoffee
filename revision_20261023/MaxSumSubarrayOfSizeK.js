// https://www.geeksforgeeks.org/problems/max-sum-subarray-of-size-k5313/1

class Solution {
    maxSubarraySum(arr, k) {
        let maxSum = 0, currentSum = 0;
        let rightPtr = k - 1; //1 ≤ k ≤ arr.size()

        for (let i = 0; i <= rightPtr; i++) {
            currentSum += arr[i];
        }
        maxSum = currentSum;

        rightPtr++;
        for (rightPtr; rightPtr <= arr.length - 1; rightPtr++) {
            currentSum = currentSum + arr[rightPtr] - arr[rightPtr - k];
            maxSum = Math.max(maxSum, currentSum);
        }

        return maxSum;
    }
}

(async () => {
    let solution = new Solution;
    console.log(solution.maxSubarraySum([100, 200, 300, 400], 2));
    console.log(solution.maxSubarraySum([1, 4, 2, 10, 23, 3, 1, 0, 20], 4));
    console.log(solution.maxSubarraySum([100, 200, 300, 400], 1));
})();