import math

def excludedNimbers(n):
	M = []
	for i in xrange(int(math.floor((n-1)/2))+1):
		M.append(i ^ (n-1-i))
		M.append(i ^ (n-2-i))
	print n
	print list(set(M))

def printNExcludedNimbers(n):
	for i in xrange(3,n+1):
		excludedNimbers(i)

def exNim(n):
	N = [0,1,1]
	for i in xrange(3, n+1):
		# compute ith element of N
		# remember: N[k] = nimber for k sticks
		M = []
		for s in xrange(int(math.floor((i-1)/2))+1):
			M.append(N[s] ^ N[i-1-s])
			M.append(N[s] ^ N[i-2-s])
		M = sorted(list(set(M)))
		# finding mex of elements of m
		mex = -1
		if (M[0] > 0):
			mex = 0
		else :
			for j in xrange(len(M) - 1):
				if (M[j+1] - M[j] > 1):
					mex = M[j] + 1
					break
			if (mex == -1):
				mex = M[-1] + 1
		N.append(mex)
	return N

print sorted(exNim(10000))
