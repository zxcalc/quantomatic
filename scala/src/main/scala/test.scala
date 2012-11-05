import quanto.core._


object Test {
  def main(args: Array[String]) = {
    val coreProc = new CoreProcess
    println("starting core...")
    coreProc.startCore("/Users/aleks/git/quanto/quantomatic/core/bin/quanto-core")
    println("done")
    val json = """
    {"request_id":0,
     "controller":"red_green",
     "module":"Main",
     "function":"echo",
     "input":1337}"""
    
    println("writing to core...")
    coreProc.stdin.write(json, 0, json.length) 
    coreProc.stdin.flush()
    println("done. reading from core...")
    
    //val out = Json.stream[Map[String,Any]](coreProc.stdout)
    coreProc.killCore(false)
  }
}