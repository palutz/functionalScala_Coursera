// currying...
// separated the parameters and now returns a function..
def sum(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int =
    if(a > b) 0
    else f(a) + sumF(a+1, b)
  sumF
}

val idSum = sum(x => x)
idSum(3, 10)

// same as before... just shorter ...
def sum2(f: Int => Int)(a:Int, b: Int) : Int =
  if(a>b) 0
  else f(a) + sum(f)(a+1, b)

val idSum2 = sum2((x: Int) => x + 1)
idSum2(3, 10)


// HOF - High Order Function
def sumOfFactorials(f: Int => Int, a: Int, b: Int) : Int =
  if(a > b) a
  else f(a) + sumOfFactorials(f, a+1, b)

def sumOfCube (a: Int, b: Int) = sumOfFactorials((x:Int) => x * x * x, a, b)

sumOfCube(1, 5) // 231

// the same with anonymous
sumOfFactorials((x:Int) => x * x * x, 1, 5) // 231
// sum of Int (with identity function)
sumOfFactorials((x:Int) => x, 1, 5) // 21


def sum(f:Int => Int, a: Int, b: Int) = {
  def loop(a: Int, acc: Int): Int =
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  loop(a, 0)
}

sum(x => x * x, 2, 5)  // 54

// product function that calculates the product of the values of a function for the points in
// a given inteval
def product(f: Int => Int)(a: Int, b: Int) : Int = {
  if(a > b) 1
  else f(a) * product(f)(a+1, b)
}
product(x => x * x)(3, 5)   // 144

// write the factorial in terms of product
def factWithProduct(a: Int) : Int =
  product(x => x)(1, a)

factWithProduct(6)

// write a more general function that generalizes both sum and product
def genericProduct(f: (Int, Int) => Int)(identity: Int)(g: Int => Int)(a: Int, b: Int) : Int =
  if (a > b) identity
  else f(g(a), genericProduct(f)(identity)(g)(a +1, b))

genericProduct((x,y) => x *y)(1)(x => x * x)(3, 5)