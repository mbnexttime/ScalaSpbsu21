//todo: What is Scala?

//define method isPrime

//print all primes from 1 to 100
def isPrime(n : Int) : Boolean = n > 1 && (2 until n).forall(n % _ != 0)

for {
  i <- 1 until 100
  if isPrime(i)
} print(s" ${i}")