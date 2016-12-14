// define imperative loops in Scala

// we need to pass the parameters by name so they are reevaluated in each iteration
def WHILE(condition: => Boolean)(command: => Unit): Unit =
  if(condition) {
    command
    WHILE(condition)(command)
  } else ()

var i = 0
WHILE(i < 10)({ i = i + 1; print(i) })

// repeat until condition is true
def REPEAT(condition: => Boolean)(command: => Unit): Unit = {
  command
  if(condition)
    REPEAT(condition)(command)
  else ()
}

var j=0
REPEAT(j < 10)({ j=j+1; println("J"+ j) })

// using foreach to translate for loops
for {
  h <- 1 until 3
  k <- "abc"
} println(h + " " + k)

(1 until 3) foreach(i => "abc" foreach(j => println(i + " " + j)))
