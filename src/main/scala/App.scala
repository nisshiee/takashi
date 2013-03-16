package org.nisshiee.takashi

object App {
  def main(args: Array[String]) = args.toList match {
    case "-s" :: Nil => prob1.AkkaTakashi.startSearchNode; ()
    case "-a" :: file :: Nil => {
      val field = prob1.Field.read(file)
      prob1.AkkaTakashi.routeCount(field, System.currentTimeMillis)
    }
    case _ => ()
  }
}
