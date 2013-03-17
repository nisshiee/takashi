package org.nisshiee.takashi

object App {
  def main(args: Array[String]) = args.toList match {

    case "1" :: t => {
      import prob1._
      t match {
        case "search" :: _ => SearchActor.startSearchNode()
        case "aggregater" :: _ => Aggregater.startAggregaterNode()
        case "akka" :: file :: _ => {
          val field = Field.read(file)
          AkkaTakashi.routeCount(field)
        }
        case file :: _ => {
          val field = Field.read(file)
          println(Takashi.routeCount(field))
        }
        case _ => println("args: search | aggregater | akka <file> | <file>")
      }
    }

    case "2" :: t => {
      import prob2._
      t match {
        case file :: _ => {
          val field = Field.read(file)
          println(Takashi.maxCount(field))
        }
        case _ => println("args: <file>")
      }
    }

    case "3" :: t => {
      import prob3._
      t match {
        case "distance" :: file :: _ => {
          val graph = Graph.read(file)
          println(Takashi.minDistanceRoute(graph))
        }
        case "fee" :: file :: _ => {
          val graph = Graph.read(file)
          println(Takashi.minFeeRoute(graph))
        }
        case _ => println("args: ( distance | fee ) <file>")
      }
    }

    case _ => println("args: ( 1 | 2 | 3 ) [<problem specific arg> ...]")
  }
}
