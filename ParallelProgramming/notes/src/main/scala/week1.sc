object tryThread {

  private var uidCount = 0
  val x = new AnyRef {}
  def getUniqueId() = {
    x.synchronized{
      uidCount += 1
      uidCount
    }
  }



  class Account(private var amount: Int = 0) {
    val uid = getUniqueId()
    private def lockAndTransfer(target: Account, n: Int) = {
      this.synchronized {
        target.synchronized {
          this.amount -= n
          target.amount += n
        }
      }
    }
    def transfer(target: Account, n: Int) = {
      if(this.uid < target.uid)
        this.lockAndTransfer(target, n)
      else
        this.lockAndTransfer(this, -n)
    }
  }

  def startThread(a: Account, b: Account, n: Int) = {
    val t = new Thread {
      override def run(): Unit = {
        for(i <- 0 until n) {
          a.transfer(b, 1)
        }
      }
    }
    t.start()
    t
  }

  val a1 = new Account(50000)
  val a2 = new Account(75000)

  val t = startThread(a1, a2, 15000)
  val t2 = startThread(a2, a1, 15000)
  t.join()
  t2.join()
}
