class Solution:
    def __init__(slef):
        pass

    def flattenList(self, x, result):
        while x:
            curr_elem = x.pop(0)
            if type(curr_elem) == type([]):
                self.flattenList(curr_elem, result)
            else:
                result.append(curr_elem)

        

if __name__ == '__main__':

    test = [1, 2, [3, 4], [5, 6, [7, 8, [9, 10, 11]], [12]]]
    result = []
    s = Solution()
    s.flattenList(test, result)

    print(result)

    test2 = []
    result2 = []
    s.flattenList(test2, result2)
    print(result2)
